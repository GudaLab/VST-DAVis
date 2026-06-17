datainput_multiple_sample <- function(index_multiple_sample_file,
                                      index_multiple_sample_file1,
                                      index_multiple_sample_file_names,
                                      index_multiple_sample_format,
                                      index_multiple_sample_name,
                                      index_multiple_sample_spatial_mode = "auto",
                                      index_multiple_sample_hd_bin_size = "8") {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
      return(y)
    }
    x
  }

  safe_plot <- function(expr) {
    tryCatch(expr, error = function(e) NULL)
  }

  source_app_script("scripts/assay_utils.R")
  source_app_script("scripts/visium_hd_compat_loader.R")

  normalize_counts_input <- function(counts) {
    if (inherits(counts, "dgCMatrix")) {
      return(counts)
    }

    if (inherits(counts, "matrix")) {
      return(as(counts, "dgCMatrix"))
    }

    if (is.list(counts)) {
      preferred_names <- c(
        "Gene Expression",
        "Gene Expression Matrix",
        "Spatial",
        "RNA"
      )

      for (nm in preferred_names) {
        if (nm %in% names(counts)) {
          return(normalize_counts_input(counts[[nm]]))
        }
      }

      return(normalize_counts_input(counts[[1]]))
    }

    stop("Unable to normalize the counts matrix returned by Read10X/Read10X_h5.")
  }

  get_counts_matrix <- function(obj, assay_name) {
    get_assay_layer_matrix(
      object = obj,
      assay = assay_name,
      layer = "counts",
      slot_fallback = "counts",
      as_sparse = TRUE
    )
  }

  guess_mt_pattern <- function(feature_names) {
    if (any(grepl("^MT-", feature_names))) {
      return("^MT-")
    }

    if (any(grepl("^mt-", feature_names))) {
      return("^mt-")
    }

    NA_character_
  }

  sample_cells_for_display <- function(obj, max_cells = Inf, seed = 123L) {
    all_cells <- colnames(obj)
    if (length(all_cells) <= max_cells) {
      return(all_cells)
    }

    sample_ids <- obj$orig.ident %||% rep("sample", length(all_cells))
    sample_ids <- as.character(sample_ids)
    split_cells <- split(all_cells, sample_ids)
    split_sizes <- as.numeric(lengths(split_cells))
    total_cells <- sum(split_sizes)
    if (!is.finite(total_cells) || total_cells <= 0) {
      return(all_cells)
    }

    max_cells_num <- as.numeric(max_cells)
    target_counts <- pmax(1L, floor(max_cells_num * split_sizes / total_cells))
    target_counts <- as.integer(target_counts)

    current_total <- sum(target_counts)
    if (current_total < max_cells) {
      room <- split_sizes - target_counts
      room[room < 0] <- 0
      while (current_total < max_cells && any(room > 0)) {
        idx <- which.max(room)
        target_counts[idx] <- target_counts[idx] + 1L
        room[idx] <- room[idx] - 1L
        current_total <- current_total + 1L
      }
    }

    set.seed(seed)
    sampled_cells <- unlist(
      lapply(seq_along(split_cells), function(i) {
        cells_i <- split_cells[[i]]
        take_n <- min(length(cells_i), target_counts[[i]])
        if (take_n >= length(cells_i)) {
          cells_i
        } else {
          sample(cells_i, take_n)
        }
      }),
      use.names = FALSE
    )

    unique(sampled_cells)
  }

  create_display_object <- function(obj, max_cells = Inf) {
    display_cells <- sample_cells_for_display(obj, max_cells = max_cells)
    if (length(display_cells) == ncol(obj)) {
      return(list(
        object = obj,
        message = paste0("QC plots were generated on all ", format(ncol(obj), big.mark = ","), " spatial bins/spots.")
      ))
    }

    display_obj <- subset(obj, cells = display_cells)
    list(
      object = display_obj,
      message = paste0(
        "QC plots were generated on a display subset of ",
        format(length(display_cells), big.mark = ","),
        " of ",
        format(ncol(obj), big.mark = ","),
        " spatial bins/spots to keep the app stable; the full Seurat object is retained for analysis."
      )
    )
  }

  empty_plot <- function(message_text) {
    ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::annotate("text", x = 0, y = 0, label = message_text, size = 5)
  }

  detect_qc_feature_column <- function(meta_df) {
    if ("nFeature_Spatial" %in% colnames(meta_df)) {
      return("nFeature_Spatial")
    }

    feature_cols <- grep("^nFeature_", colnames(meta_df), value = TRUE)
    if (!length(feature_cols)) {
      return(NA_character_)
    }
    feature_cols[[1]]
  }

  detect_qc_count_column <- function(meta_df) {
    if ("nCount_Spatial" %in% colnames(meta_df)) {
      return("nCount_Spatial")
    }

    count_cols <- grep("^nCount_", colnames(meta_df), value = TRUE)
    if (!length(count_cols)) {
      return(NA_character_)
    }
    count_cols[[1]]
  }

  build_qc_metadata <- function(obj) {
    meta_df <- obj@meta.data
    feature_col <- detect_qc_feature_column(meta_df)
    count_col <- detect_qc_count_column(meta_df)

    if (is.na(feature_col) || is.na(count_col)) {
      stop("The uploaded object does not contain the expected QC metadata columns.")
    }

    data.frame(
      cell = rownames(meta_df),
      orig.ident = as.character(meta_df$orig.ident %||% obj@project.name),
      nFeature = as.numeric(meta_df[[feature_col]]),
      nCount = as.numeric(meta_df[[count_col]]),
      percent.mt = as.numeric(meta_df$percent.mt %||% 0),
      stringsAsFactors = FALSE
    )
  }

  build_spatial_qc_metadata <- function(sample_objects) {
    spatial_frames <- lapply(seq_along(sample_objects), function(i) {
      obj <- sample_objects[[i]]
      sample_name <- names(sample_objects)[[i]] %||% obj@project.name %||% paste0("sample_", i)
      image_names <- tryCatch(Seurat::Images(obj), error = function(e) character(0))
      if (!length(image_names)) {
        return(NULL)
      }

      coords <- tryCatch(
        Seurat::GetTissueCoordinates(obj, image = image_names[[1]]),
        error = function(e) NULL
      )
      if (is.null(coords) || !nrow(coords)) {
        return(NULL)
      }

      coords <- as.data.frame(coords)
      x_col <- if ("x" %in% colnames(coords)) {
        "x"
      } else if ("imagecol" %in% colnames(coords)) {
        "imagecol"
      } else if ("col" %in% colnames(coords)) {
        "col"
      } else {
        NULL
      }

      y_col <- if ("y" %in% colnames(coords)) {
        "y"
      } else if ("imagerow" %in% colnames(coords)) {
        "imagerow"
      } else if ("row" %in% colnames(coords)) {
        "row"
      } else {
        NULL
      }

      if (is.null(x_col) || is.null(y_col)) {
        return(NULL)
      }

      meta_df <- obj@meta.data
      shared_cells <- intersect(rownames(coords), rownames(meta_df))
      if (!length(shared_cells)) {
        return(NULL)
      }

      coords <- coords[shared_cells, , drop = FALSE]
      meta_df <- meta_df[shared_cells, , drop = FALSE]
      feature_col <- detect_qc_feature_column(meta_df)
      count_col <- detect_qc_count_column(meta_df)
      if (is.na(feature_col) || is.na(count_col)) {
        return(NULL)
      }

      data.frame(
        cell = shared_cells,
        orig.ident = sample_name,
        x = as.numeric(coords[[x_col]]),
        y = as.numeric(coords[[y_col]]),
        nFeature = as.numeric(meta_df[[feature_col]]),
        nCount = as.numeric(meta_df[[count_col]]),
        percent.mt = as.numeric(meta_df$percent.mt %||% 0),
        stringsAsFactors = FALSE
      )
    })

    spatial_frames <- Filter(Negate(is.null), spatial_frames)
    if (!length(spatial_frames)) {
      return(NULL)
    }

    do.call(rbind, spatial_frames)
  }

  build_qc_violin_panel <- function(qc_df) {
    make_one <- function(value_col, y_label, fill_color) {
      plot_df <- data.frame(
        orig.ident = qc_df$orig.ident,
        value = as.numeric(qc_df[[value_col]]),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(plot_df, ggplot2::aes(x = orig.ident, y = value)) +
        ggplot2::geom_violin(fill = fill_color, color = "grey30", scale = "width", trim = TRUE) +
        ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white", color = "grey20") +
        ggplot2::labs(x = "Sample", y = y_label) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
    }

    patchwork::wrap_plots(
      make_one("nFeature", "nFeature_Spatial", "#9ecae1"),
      make_one("nCount", "nCount_Spatial", "#fdae6b"),
      make_one("percent.mt", "percent.mt", "#a1d99b"),
      ncol = 1
    )
  }

  build_qc_scatter_panel <- function(qc_df) {
    make_one <- function(x_col, y_col, x_label, y_label) {
      plot_df <- data.frame(
        orig.ident = qc_df$orig.ident,
        x = as.numeric(qc_df[[x_col]]),
        y = as.numeric(qc_df[[y_col]]),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_bin2d(bins = 120) +
        ggplot2::facet_wrap(stats::as.formula("~ orig.ident"), scales = "free") +
        ggplot2::scale_fill_gradient(low = "grey92", high = "#08519c", name = "Bin count") +
        ggplot2::labs(x = x_label, y = y_label) +
        ggplot2::theme_bw()
    }

    patchwork::wrap_plots(
      make_one("nFeature", "percent.mt", "nFeature_Spatial", "percent.mt"),
      make_one("nFeature", "nCount", "nFeature_Spatial", "nCount_Spatial"),
      ncol = 2
    )
  }

  build_spatial_qc_panel <- function(spatial_qc_df) {
    if (is.null(spatial_qc_df) || !nrow(spatial_qc_df)) {
      return(empty_plot("Spatial QC coordinates are unavailable for this upload."))
    }

    make_one <- function(value_col, title_text) {
      plot_df <- spatial_qc_df
      plot_df$value <- as.numeric(plot_df[[value_col]])

      ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = y)) +
        ggplot2::stat_summary_2d(
          ggplot2::aes(z = value, fill = ggplot2::after_stat(value)),
          fun = function(z) mean(z, na.rm = TRUE),
          bins = 120
        ) +
        ggplot2::facet_wrap(stats::as.formula("~ orig.ident"), scales = "free") +
        ggplot2::scale_y_reverse() +
        ggplot2::coord_equal() +
        ggplot2::scale_fill_gradient(low = "grey95", high = "#cb181d", name = title_text) +
        ggplot2::labs(x = "x", y = "y", title = title_text) +
        ggplot2::theme_bw()
    }

    patchwork::wrap_plots(
      make_one("nFeature", "nFeature_Spatial"),
      make_one("nCount", "nCount_Spatial"),
      make_one("percent.mt", "percent.mt"),
      ncol = 1
    )
  }

  read_counts_source <- function(matrix_dir, h5_path) {
    counts <- NULL

    if (!is.null(matrix_dir) && dir.exists(matrix_dir)) {
      counts <- tryCatch(Read10X(matrix_dir), error = function(e) NULL)
    }

    if (is.null(counts) && !is.null(h5_path) && file.exists(h5_path)) {
      counts <- tryCatch(Read10X_h5(h5_path), error = function(e) NULL)
    }

    if (is.null(counts)) {
      stop("No readable filtered matrix was found.")
    }

    normalize_counts_input(counts)
  }

  resolve_sample_root <- function(input_path, preferred_type = c("standard", "matrix", "visium_hd", "auto")) {
    preferred_type <- match.arg(preferred_type)

    if (dir.exists(input_path)) {
      root_dir <- normalizePath(input_path, winslash = "/", mustWork = TRUE)
    } else if (file.exists(input_path) && grepl("\\.zip$", input_path, ignore.case = TRUE)) {
      exdir <- file.path(
        tempdir(),
        paste0("upload_", tools::file_path_sans_ext(basename(input_path)))
      )

      if (dir.exists(exdir)) {
        unlink(exdir, recursive = TRUE, force = TRUE)
      }

      dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
      utils::unzip(input_path, exdir = exdir)
      root_dir <- normalizePath(exdir, winslash = "/", mustWork = TRUE)
    } else {
      stop("Input must be a sample directory or ZIP file.")
    }

    candidate_dirs <- unique(c(root_dir, list.dirs(root_dir, recursive = TRUE, full.names = TRUE)))

    classify_candidate <- function(dir_path) {
      has_spatial <- dir.exists(file.path(dir_path, "spatial"))
      has_bins <- dir.exists(file.path(dir_path, "binned_outputs"))
      has_h5 <- file.exists(file.path(dir_path, "filtered_feature_bc_matrix.h5"))
      has_matrix_dir <- dir.exists(file.path(dir_path, "filtered_feature_bc_matrix"))
      has_flat_matrix <- all(file.exists(file.path(
        dir_path,
        c("matrix.mtx.gz", "features.tsv.gz", "barcodes.tsv.gz")
      )))

      data.frame(
        dir = dir_path,
        has_spatial = has_spatial,
        has_bins = has_bins,
        has_h5 = has_h5,
        has_matrix_dir = has_matrix_dir,
        has_flat_matrix = has_flat_matrix,
        stringsAsFactors = FALSE
      )
    }

    candidates <- do.call(rbind, lapply(candidate_dirs, classify_candidate))
    candidates$depth <- vapply(
      strsplit(gsub("\\\\", "/", candidates$dir), "/", fixed = TRUE),
      length,
      integer(1)
    )

    pick_first <- function(mask) {
      hits <- candidates[mask, , drop = FALSE]
      if (!nrow(hits)) {
        return(NULL)
      }
      hits <- hits[order(hits$depth), , drop = FALSE]
      hits[1, , drop = FALSE]
    }

    visium_hd_hit <- pick_first(candidates$has_bins)
    standard_hit <- pick_first(candidates$has_spatial & (candidates$has_h5 | candidates$has_matrix_dir))
    matrix_hit <- pick_first(candidates$has_spatial & (candidates$has_matrix_dir | candidates$has_flat_matrix))

    if (preferred_type == "visium_hd" && !is.null(visium_hd_hit)) {
      return(list(dir = visium_hd_hit$dir[[1]], type = "visium_hd"))
    }

    if (preferred_type == "standard" && !is.null(standard_hit)) {
      return(list(dir = standard_hit$dir[[1]], type = "standard"))
    }

    if (preferred_type == "matrix" && !is.null(matrix_hit)) {
      return(list(dir = matrix_hit$dir[[1]], type = "matrix"))
    }

    if (preferred_type == "auto") {
      auto_hit <- standard_hit %||% visium_hd_hit %||% matrix_hit
      if (!is.null(auto_hit)) {
        auto_type <- if (identical(auto_hit$has_bins[[1]], TRUE)) {
          "visium_hd"
        } else if (identical(auto_hit$has_h5[[1]], TRUE)) {
          "standard"
        } else {
          "matrix"
        }
        return(list(dir = auto_hit$dir[[1]], type = auto_type))
      }
    }

    if (preferred_type %in% c("standard", "matrix") && !is.null(visium_hd_hit)) {
      return(list(dir = visium_hd_hit$dir[[1]], type = "visium_hd"))
    }

    stop("Could not locate a supported sample layout inside the uploaded directory or ZIP.")
  }

  pick_matrix_dir <- function(data_dir) {
    nested_matrix_dir <- file.path(data_dir, "filtered_feature_bc_matrix")
    if (dir.exists(nested_matrix_dir)) {
      return(nested_matrix_dir)
    }

    flat_files <- c("matrix.mtx.gz", "features.tsv.gz", "barcodes.tsv.gz")
    if (all(file.exists(file.path(data_dir, flat_files)))) {
      return(data_dir)
    }

    nested_hits <- list.dirs(data_dir, recursive = TRUE, full.names = TRUE)
    nested_hits <- nested_hits[basename(nested_hits) == "filtered_feature_bc_matrix"]
    if (!length(nested_hits)) {
      return(NULL)
    }
    nested_hits[[1]]
  }

  available_hd_bins <- function(data_dir) {
    bins_root <- file.path(data_dir, "binned_outputs")
    if (!dir.exists(bins_root)) {
      return(integer(0))
    }

    bin_dirs <- list.dirs(bins_root, recursive = FALSE, full.names = FALSE)
    parsed <- sub("^square_([0-9]+)um$", "\\1", bin_dirs)
    suppressWarnings(as.integer(parsed[grepl("^square_[0-9]+um$", bin_dirs)]))
  }

  pick_hd_bin <- function(data_dir, requested_bin = "auto") {
    bins <- sort(unique(available_hd_bins(data_dir)))
    if (length(bins) == 0) {
      return(NULL)
    }

    requested_bin <- as.character(requested_bin %||% "auto")
    if (!identical(requested_bin, "auto")) {
      requested_bin_num <- suppressWarnings(as.integer(requested_bin))
      if (!is.na(requested_bin_num) && requested_bin_num %in% bins) {
        return(requested_bin_num)
      }
    }

    preferred_bins <- c(8L, 16L, 2L)
    preferred_match <- preferred_bins[preferred_bins %in% bins]
    if (length(preferred_match) > 0) {
      return(preferred_match[[1]])
    }

    bins[[1]]
  }

  ensure_spatial_dir_ready <- function(spatial_meta_dir, image_source_dir) {
    if (!dir.exists(spatial_meta_dir)) {
      return(NULL)
    }

    if (dir.exists(image_source_dir)) {
      image_files <- list.files(
        image_source_dir,
        pattern = "\\.(png|jpg|jpeg|tif|tiff)$",
        ignore.case = TRUE,
        full.names = TRUE
      )

      for (src in image_files) {
        dest <- file.path(spatial_meta_dir, basename(src))
        if (!file.exists(dest)) {
          file.copy(src, dest, overwrite = TRUE)
        }
      }
    }

    spatial_meta_dir
  }

  add_spatial_image <- function(obj, spatial_dir, sample_name) {
    if (is.null(spatial_dir) || !dir.exists(spatial_dir)) {
      return(obj)
    }

    image_obj <- tryCatch(
      Read10X_Image(
        image.dir = spatial_dir,
        assay = "Spatial",
        filter.matrix = TRUE,
        slice = sample_name
      ),
      error = function(e) {
        warning("Spatial image could not be read for sample ", sample_name, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(image_obj)) {
      obj[[sample_name]] <- image_obj
    }

    obj
  }

  create_spatial_object <- function(counts,
                                    spatial_dir,
                                    sample_name,
                                    source_mode,
                                    bin_size_um = NA_integer_) {
    obj <- CreateSeuratObject(
      counts = counts,
      assay = "Spatial",
      project = sample_name
    )

    obj <- add_spatial_image(obj, spatial_dir = spatial_dir, sample_name = sample_name)
    obj[["RNA"]] <- CreateAssayObject(counts = get_counts_matrix(obj, "Spatial"))
    DefaultAssay(obj) <- "RNA"

    obj@project.name <- sample_name
    obj$orig.ident <- sample_name
    obj$condition <- sample_name
    obj$spatial_source_mode <- source_mode
    obj$bin_size_um <- bin_size_um
    Idents(obj) <- "orig.ident"

    obj
  }

  load_standard_h5_sample <- function(data_dir, sample_name) {
    resolved <- resolve_sample_root(data_dir, preferred_type = "standard")

    if (identical(resolved$type, "visium_hd")) {
      return(load_visium_hd_sample_compat(
        input_path = resolved$dir,
        sample_name = sample_name,
        requested_bin = index_multiple_sample_hd_bin_size,
        assay = "Spatial",
        default_assay = "RNA",
        image_name = "tissue_lowres_image.png",
        create_rna_assay = TRUE,
        filter_matrix = TRUE
      ))
    }

    resolved_dir <- resolved$dir
    counts <- read_counts_source(
      matrix_dir = pick_matrix_dir(resolved_dir),
      h5_path = file.path(resolved_dir, "filtered_feature_bc_matrix.h5")
    )

    spatial_dir <- ensure_spatial_dir_ready(
      spatial_meta_dir = file.path(resolved_dir, "spatial"),
      image_source_dir = file.path(resolved_dir, "spatial")
    )

    create_spatial_object(
      counts = counts,
      spatial_dir = spatial_dir,
      sample_name = sample_name,
      source_mode = "standard",
      bin_size_um = NA_integer_
    )
  }

  load_visium_hd_sample <- function(data_dir, sample_name, requested_bin = "auto") {
    load_visium_hd_sample_compat(
      input_path = data_dir,
      sample_name = sample_name,
      requested_bin = requested_bin,
      assay = "Spatial",
      default_assay = "RNA",
      image_name = "tissue_lowres_image.png",
      create_rna_assay = TRUE,
      filter_matrix = TRUE
    )
  }

  load_mfb_sample <- function(data_dir, sample_name) {
    resolved <- resolve_sample_root(data_dir, preferred_type = "matrix")
    resolved_dir <- resolved$dir

    if (identical(resolved$type, "visium_hd")) {
      return(load_visium_hd_sample_compat(
        input_path = resolved_dir,
        sample_name = sample_name,
        requested_bin = index_multiple_sample_hd_bin_size,
        assay = "Spatial",
        default_assay = "RNA",
        image_name = "tissue_lowres_image.png",
        create_rna_assay = TRUE,
        filter_matrix = TRUE
      ))
    }

    counts <- read_counts_source(
      matrix_dir = pick_matrix_dir(resolved_dir),
      h5_path = NULL
    )

    spatial_dir <- ensure_spatial_dir_ready(
      spatial_meta_dir = file.path(resolved_dir, "spatial"),
      image_source_dir = file.path(resolved_dir, "spatial")
    )

    create_spatial_object(
      counts = counts,
      spatial_dir = spatial_dir,
      sample_name = sample_name,
      source_mode = "matrix",
      bin_size_um = NA_integer_
    )
  }

  load_h5_or_hd_sample <- function(data_dir, sample_name, spatial_mode, requested_bin) {
    attempt_order <- switch(
      as.character(spatial_mode %||% "auto"),
      visium_hd = c("visium_hd", "standard"),
      standard = c("standard", "visium_hd"),
      c("visium_hd", "standard")
    )

    errors <- character(0)

    for (mode in attempt_order) {
      loaded <- tryCatch(
        {
          if (mode == "visium_hd") {
            load_visium_hd_sample(data_dir, sample_name, requested_bin = requested_bin)
          } else {
            load_standard_h5_sample(data_dir, sample_name)
          }
        },
        error = function(e) {
          errors <<- c(errors, paste(mode, "-", conditionMessage(e)))
          NULL
        }
      )

      if (!is.null(loaded)) {
        return(loaded)
      }
    }

    stop(paste(errors, collapse = " | "))
  }

  index_multiple_sample_format <- as.character(index_multiple_sample_format)
  index_multiple_sample_spatial_mode <- as.character(index_multiple_sample_spatial_mode %||% "auto")
  index_multiple_sample_hd_bin_size <- as.character(index_multiple_sample_hd_bin_size %||% "8")

  original_path <- vstdavis_safe_getwd()
  on.exit(vstdavis_restore_wd(original_path), add = TRUE)

  library(Seurat)
  library(Matrix)
  library(dplyr)
  library(patchwork)

  spatial_objects <- list()
  sample_summaries <- character(0)

  if (index_multiple_sample_format == "h5") {
    zip_paths <- index_multiple_sample_file
    zip_names <- index_multiple_sample_file_names
    if (!is.null(index_multiple_sample_file1) && !is.null(zip_names)) {
      zip_paths <- file.path(index_multiple_sample_file1, basename(zip_names))
    }

    spatial_objects <- lapply(seq_along(zip_paths), function(i) {
      sample_name <- sub("\\.zip$", "", basename(zip_names[[i]]))
      obj <- load_standard_h5_sample(data_dir = zip_paths[[i]], sample_name = sample_name)

      sample_summaries <<- c(
        sample_summaries,
        paste0(
          sample_name,
          " : ",
          obj$spatial_source_mode[[1]],
          if (!is.na(obj$bin_size_um[[1]])) paste0(" (", obj$bin_size_um[[1]], " um)") else ""
        )
      )

      obj
    })

    names(spatial_objects) <- sub("\\.zip$", "", basename(zip_names))
  } else if (index_multiple_sample_format == "visium_bin") {
    zip_paths <- index_multiple_sample_file
    zip_names <- index_multiple_sample_file_names
    if (!is.null(index_multiple_sample_file1) && !is.null(zip_names)) {
      zip_paths <- file.path(index_multiple_sample_file1, basename(zip_names))
    }

    spatial_objects <- lapply(seq_along(zip_paths), function(i) {
      sample_name <- sub("\\.zip$", "", basename(zip_names[[i]]))
      obj <- load_visium_hd_sample_compat(
        input_path = zip_paths[[i]],
        sample_name = sample_name,
        requested_bin = index_multiple_sample_hd_bin_size,
        assay = "Spatial",
        default_assay = "RNA",
        image_name = "tissue_lowres_image.png",
        create_rna_assay = TRUE,
        filter_matrix = TRUE
      )

      sample_summaries <<- c(
        sample_summaries,
        paste0(
          sample_name,
          " : ",
          obj$spatial_source_mode[[1]],
          if (!is.na(obj$bin_size_um[[1]])) paste0(" (", obj$bin_size_um[[1]], " um)") else ""
        )
      )

      obj
    })

    names(spatial_objects) <- sub("\\.zip$", "", basename(zip_names))
  } else if (index_multiple_sample_format == "MFB") {
    zip_paths <- index_multiple_sample_file
    zip_names <- index_multiple_sample_file_names
    if (!is.null(index_multiple_sample_file1) && !is.null(zip_names)) {
      zip_paths <- file.path(index_multiple_sample_file1, basename(zip_names))
    }

    spatial_objects <- lapply(seq_along(zip_paths), function(i) {
      sample_name <- sub("\\.zip$", "", basename(zip_names[[i]]))
      obj <- load_mfb_sample(zip_paths[[i]], sample_name = sample_name)
      sample_summaries <<- c(sample_summaries, paste0(sample_name, " : matrix"))
      obj
    })

    names(spatial_objects) <- sub("\\.zip$", "", basename(zip_names))
  } else if (index_multiple_sample_format == "exampledata") {
    example_dir <- vstdavis_app_file("www/example_data/GSE230207")
    if (!dir.exists(example_dir)) {
      stop("Example data folder not found: ", example_dir)
    }
    setwd(example_dir)

    zip_files <- list.files(pattern = "\\.zip$", full.names = FALSE)
    data_dirs <- sub("\\.zip$", "", basename(zip_files))
    invisible(lapply(zip_files, unzip, overwrite = TRUE))

    spatial_objects <- lapply(data_dirs, function(data_dir) {
      sample_name <- basename(data_dir)
      obj <- load_h5_or_hd_sample(
        data_dir = data_dir,
        sample_name = sample_name,
        spatial_mode = "auto",
        requested_bin = "8"
      )

      sample_summaries <<- c(
        sample_summaries,
        paste0(
          sample_name,
          " : ",
          obj$spatial_source_mode[[1]],
          if (!is.na(obj$bin_size_um[[1]])) paste0(" (", obj$bin_size_um[[1]], " um)") else ""
        )
      )

      obj
    })

    names(spatial_objects) <- basename(data_dirs)
  }

  valid_objects <- Filter(function(x) inherits(x, "Seurat"), spatial_objects)
  if (length(valid_objects) == 0) {
    return(list(
      is_valid = FALSE,
      text_summary = "Please check your sample ZIP structure. Supported layouts are standard Space Ranger, Visium HD with binned_outputs, or matrix/barcodes/features plus spatial image files."
    ))
  }

  merged_spatial <- if (length(valid_objects) == 1) {
    valid_objects[[1]]
  } else {
    merge(
      valid_objects[[1]],
      y = valid_objects[-1],
      add.cell.ids = names(valid_objects),
      project = "merged"
    )
  }

  spatial_counts <- get_counts_matrix(merged_spatial, "Spatial")
  mt_pat <- guess_mt_pattern(rownames(spatial_counts))
  if (is.na(mt_pat)) {
    merged_spatial$percent.mt <- 0
  } else {
    merged_spatial[["percent.mt"]] <- PercentageFeatureSet(
      merged_spatial,
      pattern = mt_pat,
      assay = "Spatial"
    )
    merged_spatial$percent.mt[is.na(merged_spatial$percent.mt)] <- 0
  }

  Idents(merged_spatial) <- merged_spatial@meta.data$orig.ident

  table1 <- table(merged_spatial$orig.ident) %>% as.data.frame()
  colnames(table1) <- c("Sample names", "Cell counts")
  multiple_list <- valid_objects

  display_info <- create_display_object(
    merged_spatial,
    max_cells = Inf
  )
  display_spatial <- display_info$object
  sample_summaries <- c(sample_summaries, display_info$message)

  plots1 <- safe_plot(VlnPlot(display_spatial, features = "nFeature_Spatial", ncol = 1, raster = FALSE))
  plots2 <- safe_plot(VlnPlot(display_spatial, features = "nCount_Spatial", ncol = 1, raster = FALSE))
  plots3 <- safe_plot(VlnPlot(display_spatial, features = "percent.mt", ncol = 1, raster = FALSE))
  plots4 <- safe_plot(
    SpatialFeaturePlot(
      display_spatial,
      features = c("nFeature_Spatial", "nCount_Spatial", "percent.mt"),
      raster = TRUE
    )
  )
  if (is.null(plots4)) {
    plots4 <- safe_plot(
      SpatialFeaturePlot(
        display_spatial,
        features = c("nFeature_Spatial", "nCount_Spatial", "percent.mt")
      )
    )
  }
  plots5 <- safe_plot(
    FeatureScatter(
      display_spatial,
      feature1 = "nFeature_Spatial",
      feature2 = "percent.mt",
      raster = TRUE
    )
  )
  if (is.null(plots5)) {
    plots5 <- safe_plot(FeatureScatter(display_spatial, feature1 = "nFeature_Spatial", feature2 = "percent.mt"))
  }
  plots6 <- safe_plot(
    FeatureScatter(
      display_spatial,
      feature1 = "nFeature_Spatial",
      feature2 = "nCount_Spatial",
      raster = TRUE
    )
  )
  if (is.null(plots6)) {
    plots6 <- safe_plot(FeatureScatter(display_spatial, feature1 = "nFeature_Spatial", feature2 = "nCount_Spatial"))
  }

  qc_plot <- if (!is.null(plots1) && !is.null(plots2) && !is.null(plots3)) {
    plots1 + plots2 + plots3
  } else {
    plots1 %||% plots2 %||% plots3
  }

  ff_plot <- if (!is.null(plots5) && !is.null(plots6)) {
    plots5 + plots6
  } else {
    plots5 %||% plots6
  }

  list(
    is_valid = TRUE,
    text_summary = unique(sample_summaries),
    plot1 = qc_plot,
    Plot3 = ff_plot,
    data2 = merged_spatial,
    data3 = table1[, 1],
    data4 = multiple_list,
    Plot2 = plots4,
    data1 = table1
  )
}
