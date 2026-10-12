# Visium HD cell-segmentation loader (Space Ranger >= 4.0, outs/segmented_outputs)
#
# Space Ranger writes one count column per segmented cell to
#   segmented_outputs/filtered_feature_cell_matrix(.h5)
# and the cell outlines to
#   segmented_outputs/cell_segmentations.geojson
# Unlike binned_outputs there is no tissue_positions file: the only place a cell's
# location is recorded is the GeoJSON (polygons in full-resolution image pixels).
#
# This loader reduces every cell to its centroid and writes a temporary
# Space Ranger style "spatial" folder (tissue_positions.csv + scalefactors +
# tissue image), then reads it with Seurat::Read10X_Image() exactly like the
# spot/bin loaders do. The resulting Seurat object therefore has the same shape
# as every other VST-DAVis object (Spatial + RNA assays, one image per sample),
# so all downstream modules work without changes.
#
# Sample ZIP layout (one ZIP per sample, same idea as the other input formats)
#   Sample_1/
#     segmented_outputs/
#       filtered_feature_cell_matrix.h5          required (or the filtered_feature_cell_matrix/
#                                                folder with matrix.mtx.gz, features.tsv.gz,
#                                                barcodes.tsv.gz)
#       cell_segmentations.geojson               required (cell positions and outlines)
#       graphclust_annotated_cell_segmentations.geojson
#                                                optional, adds the Space Ranger cluster as metadata
#       nucleus_segmentations.geojson            optional, adds the nucleus area as metadata
#     spatial/
#       scalefactors_json.json                   required, copied from
#                                                outs/segmented_outputs/spatial/
#       tissue_lowres_image.png                  required (drawn behind the cells)
#       tissue_hires_image.png, aligned_fiducials.jpg, ...   the other Space Ranger images
#
# Space Ranger itself keeps scalefactors_json.json in segmented_outputs/spatial/ and the
# images in the top-level spatial/. That untouched two-folder layout is accepted as well.

visium_hd_seg_source_mode <- "visium_hd_segmentation"

visium_hd_seg_or <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(y)
  }
  x
}

visium_hd_seg_matrix_stems <- function() {
  "filtered_feature_cell_matrix"
}

visium_hd_seg_cell_geojson_names <- function() {
  c(
    "graphclust_annotated_cell_segmentations.geojson",
    "cell_segmentations.geojson"
  )
}

visium_hd_seg_nucleus_geojson_names <- function() {
  c(
    "nucleus_segmentations.geojson",
    "graphclust_annotated_nucleus_segmentations.geojson"
  )
}

visium_hd_seg_path_depth <- function(paths) {
  vapply(
    strsplit(gsub("\\\\", "/", paths), "/", fixed = TRUE),
    length,
    integer(1)
  )
}

visium_hd_seg_first_existing <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return(NULL)
  }
  paths[[1]]
}

# TRUE when the folder (or anything below it) holds a Space Ranger segmentation matrix.
visium_hd_seg_has_outputs <- function(root_dir) {
  if (is.null(root_dir) || !length(root_dir) || !dir.exists(root_dir)) {
    return(FALSE)
  }
  stems <- visium_hd_seg_matrix_stems()
  hits <- list.files(
    root_dir,
    pattern = paste0("^(", paste(stems, collapse = "|"), ")(\\.h5)?$"),
    recursive = TRUE,
    include.dirs = TRUE,
    full.names = FALSE
  )
  length(hits) > 0
}

# Locate every file the loader needs below `root_dir`.
visium_hd_seg_locate <- function(root_dir, image_name = "tissue_lowres_image.png") {
  root_dir <- normalizePath(root_dir, winslash = "/", mustWork = TRUE)
  all_dirs <- unique(c(root_dir, list.dirs(root_dir, recursive = TRUE, full.names = TRUE)))
  all_dirs <- all_dirs[!grepl("__MACOSX", all_dirs, fixed = TRUE)]

  stem <- visium_hd_seg_matrix_stems()[[1]]
  has_matrix <- vapply(all_dirs, function(d) {
    file.exists(file.path(d, paste0(stem, ".h5"))) || dir.exists(file.path(d, stem))
  }, logical(1))

  seg_dirs <- all_dirs[has_matrix]
  if (!length(seg_dirs)) {
    stop(
      "No Visium HD cell segmentation matrix was found. The sample must contain ",
      "segmented_outputs/", stem, ".h5 (or the ", stem, " folder)."
    )
  }

  # Prefer a folder literally called segmented_outputs, then the shallowest hit.
  seg_rank <- order(
    basename(seg_dirs) != "segmented_outputs",
    visium_hd_seg_path_depth(seg_dirs)
  )
  seg_dir <- seg_dirs[seg_rank][[1]]
  sample_root <- if (identical(basename(seg_dir), "segmented_outputs")) dirname(seg_dir) else seg_dir

  cell_geojson <- visium_hd_seg_first_existing(
    file.path(seg_dir, visium_hd_seg_cell_geojson_names())
  )
  if (is.null(cell_geojson)) {
    stop(
      "The Visium HD segmentation folder is missing cell_segmentations.geojson: ", seg_dir,
      ". This file holds the cell positions and is required."
    )
  }

  nucleus_geojson <- visium_hd_seg_first_existing(
    file.path(seg_dir, visium_hd_seg_nucleus_geojson_names())
  )

  find_below <- function(file_name, preferred) {
    hit <- visium_hd_seg_first_existing(preferred)
    if (!is.null(hit)) {
      return(hit)
    }
    # Look in the sample first, then in the whole upload (the user may have zipped
    # segmented_outputs and spatial side by side without a sample folder).
    for (search_root in unique(c(sample_root, root_dir))) {
      hits <- list.files(
        search_root,
        pattern = paste0("^", gsub(".", "\\.", file_name, fixed = TRUE), "$"),
        recursive = TRUE,
        full.names = TRUE
      )
      hits <- hits[!grepl("__MACOSX", hits, fixed = TRUE)]
      if (length(hits)) {
        return(hits[order(visium_hd_seg_path_depth(hits))][[1]])
      }
    }
    NULL
  }

  scalefactors <- find_below(
    "scalefactors_json.json",
    c(
      file.path(seg_dir, "spatial", "scalefactors_json.json"),
      file.path(sample_root, "spatial", "scalefactors_json.json"),
      file.path(seg_dir, "scalefactors_json.json")
    )
  )
  if (is.null(scalefactors)) {
    stop(
      "The Visium HD segmentation sample is missing scalefactors_json.json. ",
      "Copy outs/segmented_outputs/spatial/scalefactors_json.json into the spatial folder of the ZIP."
    )
  }

  image_path <- find_below(
    image_name,
    c(
      file.path(sample_root, "spatial", image_name),
      file.path(seg_dir, "spatial", image_name)
    )
  )
  if (is.null(image_path)) {
    stop(
      "The Visium HD segmentation sample is missing the tissue image ", image_name,
      ". Include the Space Ranger spatial folder with its image files in the ZIP."
    )
  }

  list(
    root_dir = root_dir,
    sample_root = sample_root,
    seg_dir = seg_dir,
    matrix_dir = file.path(seg_dir, stem),
    h5_path = file.path(seg_dir, paste0(stem, ".h5")),
    cell_geojson = cell_geojson,
    nucleus_geojson = nucleus_geojson,
    scalefactors = scalefactors,
    image_path = image_path
  )
}

# Shoelace centroid/area of one GeoJSON geometry (outer rings only).
# Returns c(x, y, area) in the coordinate units of the file (full-res pixels).
visium_hd_seg_geometry_stats <- function(geometry) {
  empty <- c(NA_real_, NA_real_, NA_real_)
  if (is.null(geometry) || is.null(geometry$coordinates)) {
    return(empty)
  }

  geometry_type <- visium_hd_seg_or(geometry$type, "Polygon")
  polygons <- if (identical(geometry_type, "MultiPolygon")) {
    geometry$coordinates
  } else if (identical(geometry_type, "Polygon")) {
    list(geometry$coordinates)
  } else {
    return(empty)
  }

  sum_area <- 0
  sum_cx <- 0
  sum_cy <- 0
  mean_x <- NA_real_
  mean_y <- NA_real_

  for (polygon in polygons) {
    if (!length(polygon)) {
      next
    }
    ring <- polygon[[1]]
    n <- length(ring)
    if (n < 1) {
      next
    }
    values <- suppressWarnings(as.numeric(unlist(ring, use.names = FALSE)))
    width <- length(values) %/% n
    if (width < 2 || length(values) != width * n) {
      next
    }
    x <- values[seq.int(1L, by = width, length.out = n)]
    y <- values[seq.int(2L, by = width, length.out = n)]
    if (anyNA(x) || anyNA(y)) {
      next
    }
    if (is.na(mean_x)) {
      mean_x <- mean(x)
      mean_y <- mean(y)
    }
    if (n < 3) {
      next
    }

    # Shift to the first vertex so the cross products stay numerically small.
    x0 <- x[[1]]
    y0 <- y[[1]]
    xs <- x - x0
    ys <- y - y0
    xn <- c(xs[-1L], xs[[1]])
    yn <- c(ys[-1L], ys[[1]])
    cross <- xs * yn - xn * ys
    signed_area <- sum(cross) / 2
    area <- abs(signed_area)
    if (!is.finite(area) || area <= 0) {
      next
    }
    cx <- sum((xs + xn) * cross) / (6 * signed_area) + x0
    cy <- sum((ys + yn) * cross) / (6 * signed_area) + y0

    sum_area <- sum_area + area
    sum_cx <- sum_cx + cx * area
    sum_cy <- sum_cy + cy * area
  }

  if (sum_area > 0) {
    return(c(sum_cx / sum_area, sum_cy / sum_area, sum_area))
  }
  c(mean_x, mean_y, NA_real_)
}

# Turn a list of parsed GeoJSON features into a data.frame (one row per feature).
visium_hd_seg_features_to_df <- function(features, centroid_key = "cell_centroid") {
  n <- length(features)
  cell_id <- rep(NA_real_, n)
  x <- rep(NA_real_, n)
  y <- rep(NA_real_, n)
  area <- rep(NA_real_, n)
  label <- rep(NA_character_, n)

  for (i in seq_len(n)) {
    feature <- features[[i]]
    props <- feature$properties

    id_value <- visium_hd_seg_or(props$cell_id, visium_hd_seg_or(props$id, feature$id))
    if (!is.null(id_value) && length(id_value) == 1) {
      cell_id[[i]] <- suppressWarnings(as.numeric(id_value))
    }

    stats <- visium_hd_seg_geometry_stats(feature$geometry)
    area[[i]] <- stats[[3]]

    # Space Ranger stores the centroid it computed; fall back to the polygon centroid.
    centroid <- props[[centroid_key]]
    if (!is.null(centroid) && length(centroid) >= 2) {
      centroid <- suppressWarnings(as.numeric(unlist(centroid, use.names = FALSE)))
    } else {
      centroid <- NULL
    }
    if (!is.null(centroid) && length(centroid) >= 2 && all(is.finite(centroid[1:2]))) {
      x[[i]] <- centroid[[1]]
      y[[i]] <- centroid[[2]]
    } else {
      x[[i]] <- stats[[1]]
      y[[i]] <- stats[[2]]
    }

    classification <- props$classification
    if (is.list(classification)) {
      classification <- classification$name
    }
    if (!is.null(classification) && length(classification) == 1 && !is.na(classification)) {
      label[[i]] <- as.character(classification)
    }
  }

  data.frame(
    cell_id = cell_id,
    x = x,
    y = y,
    area_px = area,
    classification = label,
    stringsAsFactors = FALSE
  )
}

# Streaming reader. Space Ranger writes the whole FeatureCollection on a single line
# and large samples reach hundreds of MB, so the file is read in chunks, cut at
# feature boundaries, and each block of complete features is parsed with jsonlite.
visium_hd_seg_read_geojson_stream <- function(path,
                                              centroid_key = "cell_centroid",
                                              chunk_bytes = 4 * 1024^2) {
  feature_start <- "\\{\\s*\"type\"\\s*:\\s*\"Feature\"\\s*,"
  open_brace <- as.raw(0x7b)
  close_brace <- as.raw(0x7d)

  parse_block <- function(block_raw) {
    text <- rawToChar(block_raw)
    Encoding(text) <- "UTF-8"
    text <- sub("[,\\s]+$", "", text, perl = TRUE)
    features <- jsonlite::parse_json(paste0("[", text, "]"), simplifyVector = FALSE)
    visium_hd_seg_features_to_df(features, centroid_key = centroid_key)
  }

  find_starts <- function(buffer_raw) {
    if (!length(buffer_raw)) {
      return(integer(0))
    }
    hits <- gregexpr(feature_start, rawToChar(buffer_raw), perl = TRUE, useBytes = TRUE)[[1]]
    as.integer(hits[hits > 0])
  }

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)

  carry <- raw(0)
  pieces <- list()

  repeat {
    chunk <- readBin(con, what = "raw", n = chunk_bytes)
    if (!length(chunk)) {
      break
    }
    buffer_raw <- c(carry, chunk)
    starts <- find_starts(buffer_raw)

    if (length(starts) < 2) {
      # No complete feature in the buffer yet; keep reading.
      carry <- if (length(starts) == 1) buffer_raw[starts[[1]]:length(buffer_raw)] else buffer_raw
      next
    }

    last_start <- starts[[length(starts)]]
    pieces[[length(pieces) + 1L]] <- parse_block(buffer_raw[starts[[1]]:(last_start - 1L)])
    carry <- buffer_raw[last_start:length(buffer_raw)]
  }

  # The leftover holds the final feature(s) followed by the closing "]}" of the collection.
  starts <- find_starts(carry)
  if (length(starts)) {
    tail_raw <- carry[starts[[1]]:length(carry)]
    last_start <- starts[[length(starts)]] - starts[[1]] + 1L
    last_raw <- tail_raw[last_start:length(tail_raw)]
    depth <- cumsum((last_raw == open_brace) - (last_raw == close_brace))
    feature_end <- which(depth == 0L)
    if (!length(feature_end)) {
      stop("The GeoJSON file ends inside a feature and looks truncated: ", path)
    }
    pieces[[length(pieces) + 1L]] <- parse_block(tail_raw[seq_len(last_start - 1L + feature_end[[1]])])
  }

  if (!length(pieces)) {
    return(NULL)
  }
  do.call(rbind, pieces)
}

# Whole-file fallback for GeoJSON written in a layout the streaming reader does not recognise.
visium_hd_seg_read_geojson_whole <- function(path, centroid_key = "cell_centroid") {
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  features <- parsed$features
  if (is.null(features) || !length(features)) {
    return(NULL)
  }
  visium_hd_seg_features_to_df(features, centroid_key = centroid_key)
}

visium_hd_seg_read_geojson <- function(path, centroid_key = "cell_centroid") {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite package is required to read Visium HD segmentation GeoJSON files.")
  }

  result <- tryCatch(
    visium_hd_seg_read_geojson_stream(path, centroid_key = centroid_key),
    error = function(e) NULL
  )
  if (is.null(result) || !nrow(result) || all(is.na(result$cell_id))) {
    result <- visium_hd_seg_read_geojson_whole(path, centroid_key = centroid_key)
  }
  if (is.null(result) || !nrow(result)) {
    stop("No cell outlines could be read from: ", path)
  }

  result <- result[!is.na(result$cell_id) & is.finite(result$x) & is.finite(result$y), , drop = FALSE]
  result <- result[!duplicated(result$cell_id), , drop = FALSE]
  if (!nrow(result)) {
    stop("The GeoJSON file does not contain usable cell_id / centroid values: ", path)
  }
  rownames(result) <- NULL
  result
}

# Space Ranger names segmented barcodes cellid_<9-digit cell_id>-1.
visium_hd_seg_barcode_to_cell_id <- function(barcodes) {
  core <- sub("-[0-9]+$", "", as.character(barcodes))
  digits <- regmatches(core, regexpr("[0-9]+$", core))
  out <- rep(NA_real_, length(core))
  out[grepl("[0-9]+$", core)] <- suppressWarnings(as.numeric(digits))
  out
}

visium_hd_seg_resolve_dir <- function(input_path) {
  if (dir.exists(input_path)) {
    return(normalizePath(input_path, winslash = "/", mustWork = TRUE))
  }

  if (!file.exists(input_path) || !grepl("\\.zip$", input_path, ignore.case = TRUE)) {
    stop("Input must be a Visium HD sample directory or ZIP file.")
  }

  exdir <- file.path(
    tempdir(),
    paste0("visium_hd_seg_", tools::file_path_sans_ext(basename(input_path)))
  )
  if (dir.exists(exdir)) {
    unlink(exdir, recursive = TRUE, force = TRUE)
  }
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(input_path, exdir = exdir)
  normalizePath(exdir, winslash = "/", mustWork = TRUE)
}

# Build a Space Ranger style spatial folder (one centroid per cell) that
# Seurat::Read10X_Image() can read in every supported Seurat version.
visium_hd_seg_write_spatial_dir <- function(positions,
                                            scalefactors_path,
                                            image_path,
                                            sample_name) {
  spatial_dir <- tempfile(
    pattern = paste0("visium_hd_seg_spatial_", gsub("[^A-Za-z0-9]+", "_", sample_name), "_"),
    tmpdir = tempdir()
  )
  dir.create(spatial_dir, recursive = TRUE, showWarnings = FALSE)

  scale_factors <- jsonlite::fromJSON(scalefactors_path)
  if (is.null(scale_factors$tissue_lowres_scalef)) {
    stop("scalefactors_json.json does not contain tissue_lowres_scalef: ", scalefactors_path)
  }

  # Segmentation scale factors carry no spot size. Use the diameter of a circle with
  # the median cell area so that plotted points are roughly the size of a cell.
  cell_diameter <- 2 * sqrt(stats::median(positions$area_px, na.rm = TRUE) / pi)
  if (!is.finite(cell_diameter) || cell_diameter <= 0) {
    microns_per_pixel <- suppressWarnings(as.numeric(visium_hd_seg_or(scale_factors$microns_per_pixel, NA)))
    cell_diameter <- if (is.finite(microns_per_pixel) && microns_per_pixel > 0) 10 / microns_per_pixel else 20
  }
  scale_factors$spot_diameter_fullres <- cell_diameter
  scale_factors$fiducial_diameter_fullres <- visium_hd_seg_or(
    scale_factors$fiducial_diameter_fullres,
    cell_diameter
  )
  scale_factors$tissue_hires_scalef <- visium_hd_seg_or(
    scale_factors$tissue_hires_scalef,
    scale_factors$tissue_lowres_scalef
  )

  writeLines(
    jsonlite::toJSON(scale_factors, auto_unbox = TRUE, digits = NA, pretty = TRUE),
    file.path(spatial_dir, "scalefactors_json.json")
  )

  # GeoJSON points are [x, y] = [image column, image row] in full-resolution pixels.
  tissue_positions <- data.frame(
    barcode = positions$barcode,
    in_tissue = 1L,
    array_row = as.integer(round(positions$y / cell_diameter)),
    array_col = as.integer(round(positions$x / cell_diameter)),
    pxl_row_in_fullres = positions$y,
    pxl_col_in_fullres = positions$x,
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    tissue_positions,
    file.path(spatial_dir, "tissue_positions.csv"),
    row.names = FALSE,
    quote = FALSE
  )

  # Same convention as the spot/bin loaders: the spatial folder carries every tissue
  # image of the sample (low-res, high-res, aligned, CytAssist), not only the plotted one.
  image_files <- list.files(
    dirname(image_path),
    pattern = "\\.(png|jpg|jpeg|tif|tiff)$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  for (src in unique(c(image_path, image_files))) {
    file.copy(src, file.path(spatial_dir, basename(src)), overwrite = TRUE)
  }

  list(
    dir = spatial_dir,
    cell_diameter_px = cell_diameter,
    microns_per_pixel = suppressWarnings(as.numeric(visium_hd_seg_or(scale_factors$microns_per_pixel, NA)))
  )
}

load_visium_hd_segmentation_sample_compat <- function(input_path,
                                                      sample_name = NULL,
                                                      assay = "Spatial",
                                                      default_assay = "RNA",
                                                      image_name = "tissue_lowres_image.png",
                                                      create_rna_assay = TRUE) {
  root_dir <- visium_hd_seg_resolve_dir(input_path)
  sample_name <- visium_hd_seg_or(sample_name, basename(root_dir))
  files <- visium_hd_seg_locate(root_dir, image_name = image_name)

  counts <- visium_hd_compat_read_counts(
    matrix_dir = files$matrix_dir,
    h5_path = files$h5_path
  )

  cells <- visium_hd_seg_read_geojson(files$cell_geojson, centroid_key = "cell_centroid")

  barcodes <- colnames(counts)
  matched <- match(visium_hd_seg_barcode_to_cell_id(barcodes), cells$cell_id)
  keep <- !is.na(matched)
  if (!any(keep)) {
    stop(
      "No overlapping cells were found between the segmentation matrix and ",
      basename(files$cell_geojson), ". Both files must come from the same Space Ranger run."
    )
  }
  n_without_outline <- sum(!keep)

  positions <- cells[matched[keep], , drop = FALSE]
  positions$barcode <- barcodes[keep]
  counts <- counts[, keep, drop = FALSE]

  spatial <- visium_hd_seg_write_spatial_dir(
    positions = positions,
    scalefactors_path = files$scalefactors,
    image_path = files$image_path,
    sample_name = sample_name
  )
  on.exit(unlink(spatial$dir, recursive = TRUE, force = TRUE), add = TRUE)

  image_obj <- Seurat::Read10X_Image(
    image.dir = spatial$dir,
    image.name = image_name,
    assay = assay,
    slice = sample_name,
    filter.matrix = TRUE
  )

  image_cells <- SeuratObject::Cells(x = image_obj)
  if (!length(image_cells) || !all(image_cells %in% colnames(counts))) {
    stop("The cell positions could not be matched to the segmentation matrix.")
  }
  counts <- counts[, image_cells, drop = FALSE]
  positions <- positions[match(image_cells, positions$barcode), , drop = FALSE]

  obj <- Seurat::CreateSeuratObject(
    counts = counts,
    assay = assay,
    project = sample_name
  )
  obj[[sample_name]] <- image_obj

  if (isTRUE(create_rna_assay) && !("RNA" %in% names(obj@assays))) {
    obj[["RNA"]] <- Seurat::CreateAssayObject(counts = counts)
  }

  obj@project.name <- sample_name
  obj$orig.ident <- sample_name
  obj$condition <- sample_name
  obj$spatial_source_mode <- visium_hd_seg_source_mode
  obj$bin_size_um <- NA_integer_
  obj$visium_hd_bin_label <- "cell_segmentation"

  microns_per_pixel <- spatial$microns_per_pixel
  area_scale <- if (is.finite(microns_per_pixel) && microns_per_pixel > 0) microns_per_pixel^2 else NA_real_
  obj$cell_area_um2 <- positions$area_px * area_scale

  if (!is.null(files$nucleus_geojson)) {
    nuclei <- tryCatch(
      visium_hd_seg_read_geojson(files$nucleus_geojson, centroid_key = "nucleus_centroid"),
      error = function(e) NULL
    )
    if (!is.null(nuclei)) {
      obj$nucleus_area_um2 <- nuclei$area_px[match(positions$cell_id, nuclei$cell_id)] * area_scale
    }
  }

  if (any(!is.na(positions$classification))) {
    obj$spaceranger_cluster <- positions$classification
  }

  if (!is.null(default_assay) && default_assay %in% names(obj@assays)) {
    Seurat::DefaultAssay(obj) <- default_assay
  }

  Seurat::Idents(obj) <- "orig.ident"
  obj@misc$visium_hd_segmentation <- list(
    cells_loaded = ncol(obj),
    cells_without_outline = n_without_outline,
    cell_geojson = basename(files$cell_geojson),
    microns_per_pixel = microns_per_pixel,
    median_cell_area_um2 = stats::median(obj$cell_area_um2, na.rm = TRUE)
  )
  obj
}
