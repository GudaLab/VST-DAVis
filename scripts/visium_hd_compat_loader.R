visium_hd_compat_normalize_counts <- function(counts) {
  if (inherits(counts, "dgCMatrix")) {
    return(counts)
  }

  if (inherits(counts, "matrix")) {
    return(methods::as(counts, "dgCMatrix"))
  }

  if (is.list(counts)) {
    preferred_names <- c("Gene Expression", "Gene Expression Matrix", "Spatial", "RNA")
    for (nm in preferred_names) {
      if (nm %in% names(counts)) {
        return(visium_hd_compat_normalize_counts(counts[[nm]]))
      }
    }
    return(visium_hd_compat_normalize_counts(counts[[1]]))
  }

  stop("Unable to normalize the counts matrix returned by Read10X/Read10X_h5.")
}

visium_hd_compat_read_counts <- function(matrix_dir, h5_path = NULL) {
  counts <- NULL

  if (!is.null(matrix_dir) && dir.exists(matrix_dir)) {
    counts <- tryCatch(Seurat::Read10X(matrix_dir), error = function(e) NULL)
  }

  if (is.null(counts) && !is.null(h5_path) && file.exists(h5_path)) {
    counts <- tryCatch(Seurat::Read10X_h5(h5_path), error = function(e) NULL)
  }

  if (is.null(counts)) {
    stop("No readable filtered matrix was found in the selected Visium HD bin directory.")
  }

  visium_hd_compat_normalize_counts(counts)
}

visium_hd_compat_available_bins <- function(data_dir) {
  bins_root <- file.path(data_dir, "binned_outputs")
  if (!dir.exists(bins_root)) {
    return(integer(0))
  }

  bin_dirs <- list.dirs(bins_root, recursive = FALSE, full.names = FALSE)
  parsed <- sub("^square_([0-9]+)um$", "\\1", bin_dirs)
  suppressWarnings(as.integer(parsed[grepl("^square_[0-9]+um$", bin_dirs)]))
}

visium_hd_compat_pick_bin <- function(data_dir, requested_bin = "auto") {
  bins <- sort(unique(visium_hd_compat_available_bins(data_dir)))
  if (!length(bins)) {
    return(NULL)
  }

  if (is.null(requested_bin) || length(requested_bin) == 0 || (length(requested_bin) == 1 && is.na(requested_bin))) {
    requested_bin <- "auto"
  }
  requested_bin <- as.character(requested_bin)
  if (!identical(requested_bin, "auto")) {
    requested_bin_num <- suppressWarnings(as.integer(requested_bin))
    if (!is.na(requested_bin_num) && requested_bin_num %in% bins) {
      return(requested_bin_num)
    }
  }

  preferred_bins <- c(8L, 16L, 2L)
  preferred_match <- preferred_bins[preferred_bins %in% bins]
  if (length(preferred_match)) {
    return(preferred_match[[1]])
  }

  bins[[1]]
}

visium_hd_compat_copy_spatial_images <- function(data_dir, bin_dir) {
  root_spatial_dir <- file.path(data_dir, "spatial")
  bin_spatial_dir <- file.path(bin_dir, "spatial")

  if (!dir.exists(root_spatial_dir)) {
    stop("The Visium HD sample is missing the top-level spatial folder: ", data_dir)
  }
  if (!dir.exists(bin_spatial_dir)) {
    stop("The selected Visium HD bin is missing its spatial folder: ", bin_dir)
  }

  image_files <- list.files(
    root_spatial_dir,
    pattern = "\\.(png|jpg|jpeg|tif|tiff)$",
    ignore.case = TRUE,
    full.names = TRUE
  )

  for (src in image_files) {
    dest <- file.path(bin_spatial_dir, basename(src))
    if (!file.exists(dest)) {
      file.copy(src, dest, overwrite = TRUE)
    }
  }

  bin_spatial_dir
}

visium_hd_compat_resolve_dir <- function(input_path) {
  if (dir.exists(input_path)) {
    return(normalizePath(input_path, winslash = "/", mustWork = TRUE))
  }

  if (!file.exists(input_path) || !grepl("\\.zip$", input_path, ignore.case = TRUE)) {
    stop("Input must be a Visium HD sample directory or ZIP file.")
  }

  exdir <- file.path(
    tempdir(),
    paste0("visium_hd_", tools::file_path_sans_ext(basename(input_path)))
  )

  if (dir.exists(exdir)) {
    unlink(exdir, recursive = TRUE, force = TRUE)
  }
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(input_path, exdir = exdir)

  if (dir.exists(file.path(exdir, "binned_outputs"))) {
    return(normalizePath(exdir, winslash = "/", mustWork = TRUE))
  }

  top_dirs <- list.dirs(exdir, recursive = FALSE, full.names = TRUE)
  top_dirs <- top_dirs[dir.exists(file.path(top_dirs, "binned_outputs"))]
  if (length(top_dirs) == 1) {
    return(normalizePath(top_dirs[[1]], winslash = "/", mustWork = TRUE))
  }

  stop("Could not resolve a Visium HD sample directory from: ", input_path)
}

load_visium_hd_sample_compat <- function(input_path,
                                         sample_name = NULL,
                                         requested_bin = "8",
                                         assay = "Spatial",
                                         default_assay = "RNA",
                                         image_name = "tissue_lowres_image.png",
                                         create_rna_assay = TRUE,
                                         filter_matrix = TRUE) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
      return(y)
    }
    x
  }

  data_dir <- visium_hd_compat_resolve_dir(input_path)
  sample_name <- sample_name %||% basename(data_dir)
  selected_bin <- visium_hd_compat_pick_bin(data_dir, requested_bin = requested_bin)
  if (is.null(selected_bin)) {
    stop("No Visium HD bins were found under: ", data_dir)
  }

  bin_label <- sprintf("square_%03dum", selected_bin)
  bin_dir <- file.path(data_dir, "binned_outputs", bin_label)
  bin_spatial_dir <- visium_hd_compat_copy_spatial_images(data_dir, bin_dir)

  counts <- visium_hd_compat_read_counts(
    matrix_dir = file.path(bin_dir, "filtered_feature_bc_matrix"),
    h5_path = file.path(bin_dir, "filtered_feature_bc_matrix.h5")
  )

  image_obj <- Seurat::Read10X_Image(
    image.dir = bin_spatial_dir,
    image.name = image_name,
    assay = assay,
    slice = sample_name,
    filter.matrix = filter_matrix
  )

  image_cells <- SeuratObject::Cells(x = image_obj)
  if (!length(image_cells)) {
    stop("No cells were found in the Visium HD image metadata.")
  }
  if (!all(image_cells %in% colnames(counts))) {
    stop("No overlapping cells were found between the selected Visium HD matrix and spatial image metadata.")
  }
  counts <- counts[, image_cells, drop = FALSE]

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
  obj$spatial_source_mode <- "visium_hd"
  obj$bin_size_um <- selected_bin
  obj$visium_hd_bin_label <- bin_label

  if (!is.null(default_assay) && default_assay %in% names(obj@assays)) {
    Seurat::DefaultAssay(obj) <- default_assay
  }

  Seurat::Idents(obj) <- "orig.ident"
  obj
}

load_visium_hd_samples_compat <- function(input_paths,
                                          requested_bin = "8",
                                          merge_objects = FALSE,
                                          default_assay = "RNA",
                                          project = "merged") {
  objs <- lapply(input_paths, function(path) {
    load_visium_hd_sample_compat(
      input_path = path,
      sample_name = tools::file_path_sans_ext(basename(path)),
      requested_bin = requested_bin,
      default_assay = default_assay
    )
  })

  names(objs) <- tools::file_path_sans_ext(basename(input_paths))

  if (!isTRUE(merge_objects) || length(objs) <= 1) {
    return(objs)
  }

  Seurat::merge(
    x = objs[[1]],
    y = objs[-1],
    add.cell.ids = names(objs),
    project = project
  )
}
