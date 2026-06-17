datainput_single_multiple_sample_hdwgcna<- function(index_multiple_sample_hdwgcna_input, index_subclustering_multiple_sample_hdwgcna_input, index_multiple_sample_hdwgcna_input2, index_subclustering_multiple_sample_hdwgcna_input2, index_multiple_sample_normalization_method_hdwgcna, index_subclustering_multiple_sample_normalization_method_hdwgcna, index_s_hdwgcna1, index_s_hdwgcna2, index_s_hdwgcna3, index_s_hdwgcna4, index_s_hdwgcna5, index_s_hdwgcna6, index_s_hdwgcna7, index_s_hdwgcna8, index_s_hdwgcna9, index_s_hdwgcna10, index_s_hdwgcna11, index_s_hdwgcna12, index_s_hdwgcna13, index_s_hdwgcna14){
  source_app_script("scripts/assay_utils.R")
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  find_hdwgcna_assay_layer <- function(obj, assay_candidates, layer_candidates) {
    assays <- tryCatch(Seurat::Assays(obj), error = function(e) character(0))

    ordered_assays <- unique(c(
      assay_candidates,
      Seurat::DefaultAssay(obj),
      assays
    ))

    ordered_assays <- ordered_assays[!is.na(ordered_assays) & nzchar(ordered_assays)]
    ordered_assays <- ordered_assays[ordered_assays %in% assays]

    for (assay_name in ordered_assays) {
      for (layer_name in layer_candidates) {
        mat <- tryCatch(
          SeuratObject::LayerData(object = obj, assay = assay_name, layer = layer_name),
          error = function(e) NULL
        )

        if (!is.null(mat) && !is.null(dim(mat)) && length(dim(mat)) == 2 && nrow(mat) > 0 && ncol(mat) > 0) {
          return(list(assay = assay_name, layer = layer_name))
        }
      }
    }

    NULL
  }

  pick_hdwgcna_assays <- function(obj, normalization_method) {
    assays <- tryCatch(Seurat::Assays(obj), error = function(e) character(0))

    preferred_assays <- c(
      "RNA",
      if (identical(normalization_method, "LogNormalize") && "integrated" %in% assays) "integrated",
      "Spatial"
    )

    if (!"RNA" %in% assays && identical(normalization_method, "SCTransform") && "SCT" %in% assays) {
      preferred_assays <- c(preferred_assays, "SCT")
    }

    data_source <- find_hdwgcna_assay_layer(
      obj,
      assay_candidates = preferred_assays,
      layer_candidates = c("data", "counts")
    )

    count_source <- find_hdwgcna_assay_layer(
      obj,
      assay_candidates = c("RNA", preferred_assays),
      layer_candidates = c("counts", "data")
    )

    if (is.null(data_source)) {
      stop("Could not find a usable assay/layer combination for hdWGCNA expression data.")
    }

    if (is.null(count_source)) {
      count_source <- data_source
    }

    list(
      data_assay = data_source$assay,
      data_layer = data_source$layer,
      count_assay = count_source$assay,
      count_layer = count_source$layer
    )
  }

  make_placeholder_plot <- function(title_text) {
    ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle(title_text)
  }

  as_tabular_object <- function(x, fallback_prefix = "value") {
    if (is.null(x)) {
      return(data.frame())
    }

    if (is.atomic(x) && is.null(dim(x))) {
      x <- data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
    } else if (!is.null(dim(x)) && length(dim(x)) < 2) {
      x <- data.frame(as.vector(x), stringsAsFactors = FALSE, check.names = FALSE)
    } else if (is.matrix(x)) {
      x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
    } else if (!is.data.frame(x)) {
      x <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    }

    n_cols <- tryCatch(ncol(x), error = function(e) 0L)
    if (is.null(n_cols) || !length(n_cols) || !is.finite(n_cols)) {
      n_cols <- 0L
    }

    if (n_cols == 0 && length(x)) {
      x <- data.frame(value = x, stringsAsFactors = FALSE, check.names = FALSE)
      n_cols <- tryCatch(ncol(x), error = function(e) 0L)
    }

    if (!is.data.frame(x)) {
      x <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      n_cols <- tryCatch(ncol(x), error = function(e) 0L)
    }

    if (n_cols > 0) {
      current_colnames <- tryCatch(colnames(x), error = function(e) NULL)
      if (is.null(current_colnames) || length(current_colnames) != n_cols) {
        current_colnames <- rep("", n_cols)
      }

      empty_cols <- is.na(current_colnames) | !nzchar(current_colnames)
      if (any(empty_cols)) {
        current_colnames[empty_cols] <- paste0(fallback_prefix, seq_len(sum(empty_cols)))
        colnames(x) <- current_colnames
      }
    }

    x
  }

  wrap_plot_collection <- function(plot_object, ncol = 3, empty_title = "Plot unavailable for this dataset.") {
    if (inherits(plot_object, "ggplot") || inherits(plot_object, "patchwork") || inherits(plot_object, "gtable")) {
      return(plot_object)
    }

    if (is.list(plot_object)) {
      valid_plots <- Filter(function(x) {
        inherits(x, "ggplot") || inherits(x, "patchwork") || inherits(x, "gtable")
      }, plot_object)

      if (length(valid_plots)) {
        return(patchwork::wrap_plots(valid_plots, ncol = ncol))
      }
    }

    make_placeholder_plot(empty_title)
  }

  is_hdwgcna_plot_object <- function(plot_object) {
    inherits(plot_object, c("gg", "ggplot", "patchwork", "trellis", "grob", "gtable", "recordedplot", "Heatmap", "HeatmapList", "pheatmap"))
  }

  draw_hdwgcna_plot_object <- function(plot_object) {
    if (is.null(plot_object)) {
      return(invisible(FALSE))
    }

    if (inherits(plot_object, "recordedplot")) {
      grDevices::replayPlot(plot_object)
      return(invisible(TRUE))
    }

    if (inherits(plot_object, c("gg", "ggplot", "patchwork", "trellis"))) {
      print(plot_object)
      return(invisible(TRUE))
    }

    if (inherits(plot_object, "pheatmap") && !is.null(plot_object$gtable)) {
      grid::grid.newpage()
      grid::grid.draw(plot_object$gtable)
      return(invisible(TRUE))
    }

    if (inherits(plot_object, c("grob", "gtable"))) {
      grid::grid.newpage()
      grid::grid.draw(plot_object)
      return(invisible(TRUE))
    }

    if (inherits(plot_object, c("Heatmap", "HeatmapList")) && requireNamespace("ComplexHeatmap", quietly = TRUE)) {
      ComplexHeatmap::draw(plot_object)
      return(invisible(TRUE))
    }

    if (is.list(plot_object) && !is.data.frame(plot_object)) {
      plot_candidates <- Filter(is_hdwgcna_plot_object, plot_object)

      if (length(plot_candidates)) {
        if (requireNamespace("patchwork", quietly = TRUE) && all(vapply(plot_candidates, inherits, logical(1), what = c("gg", "ggplot", "patchwork")))) {
          print(patchwork::wrap_plots(plot_candidates))
        } else {
          for (plot_candidate in plot_candidates) {
            draw_hdwgcna_plot_object(plot_candidate)
          }
        }
        return(invisible(TRUE))
      }
    }

    invisible(FALSE)
  }

  open_hdwgcna_pdf_device <- function(file_path, width, height) {
    if (isTRUE(capabilities("cairo"))) {
      grDevices::cairo_pdf(
        filename = file_path,
        width = width,
        height = height,
        bg = "white",
        fallback_resolution = 300
      )
    } else {
      grDevices::pdf(file_path, width = width, height = height, bg = "white", useDingbats = FALSE)
    }
  }

  safe_pdf_plot <- function(file_path, plot_fun, width = 8, height = 6, warning_prefix = "PDF plot unavailable: ") {
    dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
    device_id <- NULL
    old_par <- NULL

    tryCatch(
      {
        if (file.exists(file_path)) {
          unlink(file_path, force = TRUE)
        }
        open_hdwgcna_pdf_device(file_path, width, height)
        device_id <- grDevices::dev.cur()
        old_par <- tryCatch(graphics::par(no.readonly = TRUE), error = function(e) NULL)
        try(graphics::par(bg = "white"), silent = TRUE)
        plot_result <- plot_fun()
        draw_hdwgcna_plot_object(plot_result)
      },
      error = function(e) {
        warning(paste0(warning_prefix, conditionMessage(e)))
      },
      finally = {
        if (!is.null(old_par)) {
          try(graphics::par(old_par), silent = TRUE)
        }
        open_devices <- grDevices::dev.list()
        if (!is.null(device_id) && length(open_devices) && device_id %in% open_devices) {
          try(grDevices::dev.off(device_id), silent = TRUE)
        }
      }
    )

    if (!file.exists(file_path) || isTRUE(file.info(file_path)$size == 0)) {
      warning(paste0(warning_prefix, "no PDF content was written to ", basename(file_path)))
    }
  }

  safe_png_plot <- function(file_path, plot_fun, width = 8, height = 6, dpi = 150, warning_prefix = "PNG preview unavailable: ") {
    dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
    device_id <- NULL
    old_par <- NULL

    tryCatch(
      {
        if (file.exists(file_path)) {
          unlink(file_path, force = TRUE)
        }
        png_args <- list(filename = file_path, width = width, height = height, units = "in", res = dpi, bg = "white")
        if (isTRUE(capabilities("cairo"))) {
          png_args$type <- "cairo"
        }
        do.call(grDevices::png, png_args)
        device_id <- grDevices::dev.cur()
        old_par <- tryCatch(graphics::par(no.readonly = TRUE), error = function(e) NULL)
        try(graphics::par(bg = "white"), silent = TRUE)
        plot_result <- plot_fun()
        draw_hdwgcna_plot_object(plot_result)
      },
      error = function(e) {
        warning(paste0(warning_prefix, conditionMessage(e)))
      },
      finally = {
        if (!is.null(old_par)) {
          try(graphics::par(old_par), silent = TRUE)
        }
        open_devices <- grDevices::dev.list()
        if (!is.null(device_id) && length(open_devices) && device_id %in% open_devices) {
          try(grDevices::dev.off(device_id), silent = TRUE)
        }
      }
    )

    file.exists(file_path) && isTRUE(file.info(file_path)$size > 0)
  }

  render_pdf_preview_png <- function(pdf_path, png_path, dpi = 150) {
    if (!requireNamespace("pdftools", quietly = TRUE) || !file.exists(pdf_path) || isTRUE(file.info(pdf_path)$size == 0)) {
      return(FALSE)
    }

    tryCatch(
      {
        if (file.exists(png_path)) {
          unlink(png_path, force = TRUE)
        }
        rendered <- pdftools::pdf_convert(pdf_path, format = "png", pages = 1, filenames = png_path, dpi = dpi, verbose = FALSE)
        length(rendered) && file.exists(png_path) && isTRUE(file.info(png_path)$size > 0)
      },
      error = function(e) FALSE
    )
  }

  create_hdwgcna_work_dir <- function() {
    base_candidates <- c(
      tryCatch(file.path(tempdir(), "VST-DAVis_hdwgcna"), error = function(e) character()),
      tryCatch(vstdavis_app_file("www/.runtime/hdwgcna"), error = function(e) character())
    )
    base_candidates <- base_candidates[!is.na(base_candidates) & nzchar(base_candidates)]
    run_suffix <- paste0("run_", Sys.getpid(), "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample.int(1000000, 1))

    for (base_dir in unique(base_candidates)) {
      work_dir <- file.path(base_dir, run_suffix)
      if (dir.create(work_dir, recursive = TRUE, showWarnings = FALSE) && dir.exists(work_dir)) {
        return(normalizePath(work_dir, winslash = "/", mustWork = TRUE))
      }
    }

    stop("Could not create a writable hdWGCNA working directory for TOM files.")
  }

  create_hdwgcna_pdf_output_dir <- function() {
    base_dir <- file.path(tempdir(), "VST-DAVis_hdwgcna_pdfs")
    run_suffix <- paste0("run_", Sys.getpid(), "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample.int(1000000, 1))
    output_dir <- file.path(base_dir, run_suffix)

    if (!dir.create(output_dir, recursive = TRUE, showWarnings = FALSE) || !dir.exists(output_dir)) {
      stop("Could not create a writable temporary hdWGCNA PDF output directory.")
    }

    normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  }

  sanitize_hdwgcna_file_name <- function(x, fallback = "hdwgcna_tom") {
    x <- as.character(x %||% fallback)
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x)) {
      x <- fallback
    }

    x <- gsub("[/\\\\:*?\"<>|]+", "_", x[[1]])
    x <- gsub("[^A-Za-z0-9._-]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    if (!nzchar(x)) {
      x <- fallback
    }

    substr(x, 1, 80)
  }

  pick_hdwgcna_soft_power <- function(power_table, fallback = 6L) {
    fallback <- suppressWarnings(as.integer(fallback))
    if (!is.finite(fallback) || fallback < 1L || fallback > 50L) {
      fallback <- 6L
    }

    if (!is.data.frame(power_table) || !nrow(power_table)) {
      return(fallback)
    }

    power_cols <- grep("power", colnames(power_table), ignore.case = TRUE, value = TRUE)
    if (!length(power_cols)) {
      return(fallback)
    }

    powers <- suppressWarnings(as.numeric(power_table[[power_cols[[1]]]]))
    valid <- is.finite(powers) & powers >= 1 & powers <= 50
    if (!any(valid)) {
      return(fallback)
    }

    fit_cols <- grep("SFT|scale|fit|Rsq|R.sq|R2", colnames(power_table), ignore.case = TRUE, value = TRUE)
    for (fit_col in fit_cols) {
      fit_values <- suppressWarnings(as.numeric(power_table[[fit_col]]))
      good_fit <- valid & is.finite(fit_values) & fit_values >= 0.8
      if (any(good_fit)) {
        return(as.integer(round(powers[which(good_fit)[[1]]])))
      }
    }

    as.integer(round(powers[which(valid)[[1]]]))
  }

  construct_hdwgcna_network_safely <- function(obj, tom_name, power_table) {
    tryCatch(
      ConstructNetwork(
        obj,
        tom_name = tom_name,
        overwrite_tom = TRUE
      ),
      error = function(e) {
        if (!grepl("power must be between 1 and 50", conditionMessage(e), ignore.case = TRUE)) {
          stop(e)
        }

        fallback_power <- pick_hdwgcna_soft_power(power_table)
        warning("hdWGCNA soft power selection was invalid for this dataset; retrying with soft_power = ", fallback_power, ".")

        retry_soft_power <- tryCatch(
          ConstructNetwork(
            obj,
            tom_name = tom_name,
            overwrite_tom = TRUE,
            soft_power = fallback_power
          ),
          error = function(e2) e2
        )
        if (!inherits(retry_soft_power, "error")) {
          return(retry_soft_power)
        }

        retry_power <- tryCatch(
          ConstructNetwork(
            obj,
            tom_name = tom_name,
            overwrite_tom = TRUE,
            power = fallback_power
          ),
          error = function(e3) e3
        )
        if (!inherits(retry_power, "error")) {
          return(retry_power)
        }

        stop(retry_soft_power)
      }
    )
  }

  is_visium_hd_binned_object <- function(obj) {
    source_mode <- tryCatch(as.character(obj$spatial_source_mode %||% ""), error = function(e) character(0))
    bin_size <- tryCatch(suppressWarnings(as.numeric(obj$bin_size_um %||% NA)), error = function(e) NA_real_)

    any(source_mode == "visium_hd", na.rm = TRUE) || any(is.finite(bin_size) & bin_size > 0, na.rm = TRUE)
  }

  return_hdwgcna_partial_results <- function(reason_text,
                                            obj,
                                            plots801,
                                            plots802,
                                            power_table,
                                            pdf_output_dir,
                                            dendrogram_file,
                                            run_stamp) {
    reason_text <- as.character(reason_text %||% "The module-level hdWGCNA results are unavailable for this dataset.")
    reason_text <- reason_text[!is.na(reason_text) & nzchar(reason_text)]
    if (!length(reason_text)) {
      reason_text <- "The module-level hdWGCNA results are unavailable for this dataset."
    }
    reason_text <- reason_text[[1]]

    modules <- as_tabular_object(
      tryCatch(GetModules(obj), error = function(e) data.frame()),
      fallback_prefix = "module"
    )
    if ("module" %in% colnames(modules)) {
      modules <- subset(modules, module != "grey")
    }

    hub_df <- data.frame()
    placeholder_message <- paste("Module-level hdWGCNA plots unavailable.", reason_text, sep = "\n")
    plots804 <- make_placeholder_plot(placeholder_message)
    plots805 <- make_placeholder_plot(placeholder_message)
    plots807 <- make_placeholder_plot(placeholder_message)
    plots810 <- make_placeholder_plot(placeholder_message)

    correlogram_file <- paste0("PlotModuleCorrelogram_", run_stamp, ".pdf")
    module_networks_file <- paste0("combined_output_", run_stamp, ".pdf")
    module_umap_file <- paste0("ModuleUMAPPlot_", run_stamp, ".pdf")

    safe_pdf_plot(
      file.path(pdf_output_dir, correlogram_file),
      function() make_placeholder_plot(placeholder_message),
      warning_prefix = "Module correlogram placeholder PDF unavailable: "
    )
    safe_pdf_plot(
      file.path(pdf_output_dir, module_networks_file),
      function() make_placeholder_plot(placeholder_message),
      width = 10,
      height = 8,
      warning_prefix = "Individual module network placeholder PDF unavailable: "
    )
    safe_pdf_plot(
      file.path(pdf_output_dir, module_umap_file),
      function() make_placeholder_plot(placeholder_message),
      width = 8,
      height = 6,
      warning_prefix = "Module UMAP placeholder PDF unavailable: "
    )

    list(
      plot801 = plots801,
      plot802 = plots802,
      plot804 = plots804,
      plot805 = plots805,
      plot807 = plots807,
      text_summary = pdf_output_dir,
      data1 = power_table,
      data2 = modules,
      data3 = hub_df,
      data4 = obj,
      plot810 = plots810,
      dendrogram_file = dendrogram_file,
      correlogram_file = correlogram_file,
      module_networks_file = module_networks_file,
      module_umap_file = module_umap_file
    )
  }

  index_s_hdwgcna11 <-as.logical(index_s_hdwgcna11)
  index_s_hdwgcna14 <-as.logical(index_s_hdwgcna14)
  run_stamp <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample.int(1000000, 1))
  
  if (index_s_hdwgcna1 == "multiple_sample" & index_s_hdwgcna2 == "seurat_clusters"){
    single_multiple_sample_clustering <- index_multiple_sample_hdwgcna_input
    cluster_types <- "seurat_clusters"
    cluster_number <- index_s_hdwgcna3
    }
  else if (index_s_hdwgcna1 == "multiple_sample_subclustering" & index_s_hdwgcna2 == "seurat_clusters"){
    single_multiple_sample_clustering <- index_subclustering_multiple_sample_hdwgcna_input 
    cluster_types <- "seurat_clusters"
    cluster_number <- index_s_hdwgcna3
  }
 else if (index_s_hdwgcna1 == "multiple_sample" & index_s_hdwgcna2 == "predicted"){
    if (index_multiple_sample_hdwgcna_input2 == "sctype_classification"){
      single_multiple_sample_clustering <- index_multiple_sample_hdwgcna_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_hdwgcna_input2
      cluster_types <- "sctype_classification"
      cluster_number <- index_s_hdwgcna3
    }
    else if (index_multiple_sample_hdwgcna_input2 == "singleR_labels"){
      single_multiple_sample_clustering <- index_multiple_sample_hdwgcna_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_hdwgcna_input2
      cluster_types <- "singleR_labels"
      cluster_number <- index_s_hdwgcna3
    }
    else if (index_multiple_sample_hdwgcna_input2 == "GPTCelltype"){
      single_multiple_sample_clustering <- index_multiple_sample_hdwgcna_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_hdwgcna_input2
      cluster_types <- "GPTCelltype"
      cluster_number <- index_s_hdwgcna3
    }
    else if (index_multiple_sample_hdwgcna_input2 == "cell_type"){
      single_multiple_sample_clustering <- index_multiple_sample_hdwgcna_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_hdwgcna_input2
      cluster_types <- "cell_type"
      cluster_number <- index_s_hdwgcna3
     }
  }
  else if (index_s_hdwgcna1 == "multiple_sample_subclustering" & index_s_hdwgcna2 == "predicted"){
    if (index_subclustering_multiple_sample_hdwgcna_input2 == "sctype_classification"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_hdwgcna_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_hdwgcna_input2
      cluster_types <- "sctype_classification"
      cluster_number <- index_s_hdwgcna3
      }
    else if (index_subclustering_multiple_sample_hdwgcna_input2 == "singleR_labels"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_hdwgcna_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_hdwgcna_input2
      cluster_types <- "singleR_labels"
      cluster_number <- index_s_hdwgcna3
      }
    else if (index_subclustering_multiple_sample_hdwgcna_input2 == "GPTCelltype"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_hdwgcna_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_hdwgcna_input2
      cluster_types <- "GPTCelltype"
      cluster_number <- index_s_hdwgcna3
      }
    else if (index_subclustering_multiple_sample_hdwgcna_input2 == "cell_type"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_hdwgcna_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_hdwgcna_input2
      cluster_types <- "cell_type"
      cluster_number <- index_s_hdwgcna3
      }
  }
  
  enableWGCNAThreads(nThreads = 8)
  
  plots801 <- DimPlot(single_multiple_sample_clustering, group.by=cluster_types, label=TRUE) + NoLegend()

  normalization_method_hdwgcna <- if (identical(index_s_hdwgcna1, "multiple_sample")) {
    index_multiple_sample_normalization_method_hdwgcna
  } else {
    index_subclustering_multiple_sample_normalization_method_hdwgcna
  }

  assay_choices <- pick_hdwgcna_assays(single_multiple_sample_clustering, normalization_method_hdwgcna)
  DefaultAssay(single_multiple_sample_clustering) <- assay_choices$data_assay

  wgcna_gene_fraction <- if (is_visium_hd_binned_object(single_multiple_sample_clustering)) {
    0.01
  } else {
    0.05
  }

  single_multiple_sample_clustering <- SetupForWGCNA(
    single_multiple_sample_clustering,
    gene_select = "fraction",
    fraction = wgcna_gene_fraction,
    wgcna_name = "scrdavis"
  )

  single_multiple_sample_clustering  <- MetacellsByGroups(
    seurat_obj = single_multiple_sample_clustering,
    group.by = cluster_types,
    assay = assay_choices$count_assay,
    layer = assay_choices$count_layer,
    reduction = index_s_hdwgcna4,
    min_cells = index_s_hdwgcna6,
    k = index_s_hdwgcna5,
    max_shared = index_s_hdwgcna7,
    ident.group = cluster_types,
    target_metacells = index_s_hdwgcna8
  )

  single_multiple_sample_clustering <- NormalizeMetacells(single_multiple_sample_clustering)

  assay_choices <- pick_hdwgcna_assays(single_multiple_sample_clustering, normalization_method_hdwgcna)
  DefaultAssay(single_multiple_sample_clustering) <- assay_choices$data_assay

  single_multiple_sample_clustering <- SetDatExpr(
    single_multiple_sample_clustering,
    group_name = cluster_number,
    group.by = cluster_types,
    assay = assay_choices$data_assay,
    layer = assay_choices$data_layer
  )
  
  
  #Select soft-power threshold
  # Test different soft powers:
  single_multiple_sample_clustering <- TestSoftPowers(
    single_multiple_sample_clustering,
    networkType = index_s_hdwgcna9 # you can also use "unsigned" or "signed hybrid"
  )
  # plot the results:
  plot_list <- PlotSoftPowers(single_multiple_sample_clustering)
  # assemble with patchwork
  plots802 <- wrap_plot_collection(plot_list, ncol = 2, empty_title = "Soft power plots unavailable for this dataset.")
  power_table <- as_tabular_object(GetPowerTable(single_multiple_sample_clustering), fallback_prefix = "power")
  #head(power_table)

  hdwgcna_work_dir <- create_hdwgcna_work_dir()
  previous_wd <- vstdavis_safe_getwd()
  setwd(hdwgcna_work_dir)
  tom_name <- sanitize_hdwgcna_file_name(cluster_number)
  on.exit({
    vstdavis_restore_wd(previous_wd)
    unlink(hdwgcna_work_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  # construct co-expression network:
  single_multiple_sample_clustering <- construct_hdwgcna_network_safely(
    single_multiple_sample_clustering,
    tom_name = tom_name,
    power_table = power_table
  )
  
  pdf_output_dir <- create_hdwgcna_pdf_output_dir()
  output_file <- paste0("PlotDendrogram_", run_stamp, ".pdf")          # File name for the PDF
  pdf_path <- file.path(pdf_output_dir, output_file)
  safe_pdf_plot(
    pdf_path,
    function() PlotDendrogram(single_multiple_sample_clustering, main = "Spatial hdWGCNA Dendrogram"),
    warning_prefix = "Dendrogram PDF unavailable: "
  )
  #Optional: inspect the topoligcal overlap matrix (TOM)
  TOM <- GetTOM(single_multiple_sample_clustering)
  
  
  #Module Eigengenes and Connectivity
  # compute all MEs in the full single-cell dataset
  module_eigengene_result <- tryCatch(
    ModuleEigengenes(
      single_multiple_sample_clustering,
      scale.model.use= index_s_hdwgcna10, #"poisson", or "negbinom"
      #merge.cut.height = 0.25
      #group.by.vars="condition"
    ),
    error = function(e) e
  )

  if (inherits(module_eigengene_result, "error")) {
    warning("Module eigengene calculation unavailable: ", conditionMessage(module_eigengene_result))
    return(return_hdwgcna_partial_results(
      reason_text = paste("Module eigengene calculation failed:", conditionMessage(module_eigengene_result)),
      obj = single_multiple_sample_clustering,
      plots801 = plots801,
      plots802 = plots802,
      power_table = power_table,
      pdf_output_dir = pdf_output_dir,
      dendrogram_file = output_file,
      run_stamp = run_stamp
    ))
  }

  single_multiple_sample_clustering <- module_eigengene_result
  
  
  # compute eigengene-based connectivity (kME):
  module_connectivity_result <- tryCatch(
    ModuleConnectivity(
      single_multiple_sample_clustering,
      group.by = cluster_types, group_name = cluster_number
    ),
    error = function(e) e
  )
  if (inherits(module_connectivity_result, "error")) {
    warning("Module connectivity calculation unavailable: ", conditionMessage(module_connectivity_result))
    return(return_hdwgcna_partial_results(
      reason_text = paste("Module connectivity calculation failed:", conditionMessage(module_connectivity_result)),
      obj = single_multiple_sample_clustering,
      plots801 = plots801,
      plots802 = plots802,
      power_table = power_table,
      pdf_output_dir = pdf_output_dir,
      dendrogram_file = output_file,
      run_stamp = run_stamp
    ))
  }
  single_multiple_sample_clustering <- module_connectivity_result

  # rename the modules
  single_multiple_sample_clustering <- tryCatch(
    ResetModuleNames(
      single_multiple_sample_clustering,
      new_name = paste0("SM", cluster_number)
    ),
    error = function(e) {
      warning("Module renaming unavailable: ", conditionMessage(e))
      single_multiple_sample_clustering
    }
  )
  
  # harmonized module eigengenes:
  hMEs <- tryCatch(GetMEs(single_multiple_sample_clustering, harmonized=index_s_hdwgcna11), error = function(e) NULL)
  # module eigengenes:
  MEs <- as_tabular_object(
    tryCatch(GetMEs(single_multiple_sample_clustering, harmonized=TRUE), error = function(e) NULL),
    fallback_prefix = "ME"
  )
  
  # plot genes ranked by kME for each module
  plots804 <- wrap_plot_collection(
    tryCatch(PlotKMEs(single_multiple_sample_clustering, ncol = 3, n_hubs = index_s_hdwgcna13), error = function(e) NULL),
    ncol = 3,
    empty_title = "Module connectivity plots unavailable for this dataset."
  )
  # get the module assignment table:
  modules <- as_tabular_object(
    tryCatch(GetModules(single_multiple_sample_clustering), error = function(e) NULL),
    fallback_prefix = "module"
  )
  if ("module" %in% colnames(modules)) {
    modules <- subset(modules, module != "grey")
    mods <- unique(as.character(modules$module))
    mods <- mods[!is.na(mods) & nzchar(mods) & mods != "grey"]
  } else {
    mods <- character(0)
  }
  # get hub genes
  hub_df <- as_tabular_object(
    tryCatch(GetHubGenes(single_multiple_sample_clustering, n_hubs = index_s_hdwgcna12), error = function(e) NULL),
    fallback_prefix = "hub"
  )
    
 # add the MEs to the seurat metadata so we can plot it with Seurat functions
  if (ncol(MEs)) {
    single_multiple_sample_clustering@meta.data <- cbind(single_multiple_sample_clustering@meta.data, MEs)
  }
  
  # make a featureplot of hMEs for each module
  plot_list <- tryCatch(
    ModuleFeaturePlot(
      single_multiple_sample_clustering,
      features = "hMEs",
      order = TRUE
    ),
    error = function(e) NULL
  )
  
  # stitch together with patchwork
  plots805 <- wrap_plot_collection(plot_list, ncol = 3, empty_title = "Module feature plots unavailable for this dataset.")
  
  plots810 <- if (length(mods)) {
    tryCatch(
      SpatialFeaturePlot(
        single_multiple_sample_clustering,
        features = mods,
        alpha = c(0.1, 1),
        ncol = length(unique(single_multiple_sample_clustering$orig.ident))
      ),
      error = function(e) make_placeholder_plot("Module spatial plots unavailable for this dataset.")
    )
  } else {
    make_placeholder_plot("No non-grey modules were detected for spatial plotting.")
  }
  
  #correlation plot
  output_file1 <- paste0("PlotModuleCorrelogram_", run_stamp, ".pdf")          # File name for the PDF
  pdf_path1 <- file.path(pdf_output_dir, output_file1)
  safe_pdf_plot(
    pdf_path1,
    function() ModuleCorrelogram(single_multiple_sample_clustering),
    warning_prefix = "Module correlogram PDF unavailable: "
  )
  
  
  # add hMEs to Seurat meta-data:# get hMEs from seurat object
  #MEs <- GetMEs(single_multiple_sample_clustering, harmonized=TRUE)
  #modules1 <- GetModules(single_multiple_sample_clustering)
  #mods <- levels(modules1$module); mods <- mods[mods != 'grey']
  
  # add hMEs to Seurat meta-data:
  #single_multiple_sample_clustering@meta.data <- cbind(single_multiple_sample_clustering@meta.data, MEs)
  # plot with Seurat's DotPlot function
  plots807 <- if (length(mods)) {
    tryCatch(
      DotPlot(single_multiple_sample_clustering, features = mods, group.by = cluster_types) +
        RotatedAxis() +
        scale_color_gradient2(high = "red", mid = "grey95", low = "blue"),
      error = function(e) make_placeholder_plot("Module dot plot unavailable for this dataset.")
    )
  } else {
    make_placeholder_plot("No non-grey modules were detected for dot plotting.")
  }
  
  old_pdf_options <- grDevices::pdf.options()
  grDevices::pdf.options(bg = "white", useDingbats = FALSE)
  network_plot_object <- tryCatch(
    ModuleNetworkPlot(single_multiple_sample_clustering),
    error = function(e) {
      warning(paste0("Module network plot unavailable: ", conditionMessage(e)))
      NULL
    },
    finally = {
      try(do.call(grDevices::pdf.options, old_pdf_options), silent = TRUE)
    }
  )
  
  analysis_wd <- hdwgcna_work_dir
  pdf_dir <- file.path(analysis_wd, "ModuleNetworks")
  
  # List all PDF files in the directory
  pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)
  pdf_files <- pdf_files[file.exists(pdf_files) & file.info(pdf_files)$size > 0]
  output_pdf_name <- paste0("combined_output_", run_stamp, ".pdf")
  output_pdf <- file.path(pdf_output_dir, output_pdf_name)
  if (file.exists(output_pdf)) {
    unlink(output_pdf, force = TRUE)
  }
  # Combine the PDF files
  if (length(pdf_files) > 0) {
    pdftools::pdf_combine(pdf_files, output = output_pdf)
  } else if (is_hdwgcna_plot_object(network_plot_object) || (is.list(network_plot_object) && any(vapply(network_plot_object, is_hdwgcna_plot_object, logical(1))))) {
    safe_pdf_plot(
      output_pdf,
      function() network_plot_object,
      width = 10,
      height = 8,
      warning_prefix = "Individual module network PDF unavailable: "
    )
  } else {
    safe_pdf_plot(
      output_pdf,
      function() make_placeholder_plot("Individual module network plots unavailable for this dataset."),
      width = 10,
      height = 8,
      warning_prefix = "Individual module network PDF unavailable: "
    )
  }
  # Remove the directory
  unlink(pdf_dir, recursive = TRUE)
  
  #Applying UMAP to co-expression networks
  module_umap_ready <- TRUE
  single_multiple_sample_clustering <- tryCatch(
    RunModuleUMAP(
      single_multiple_sample_clustering,
      n_hubs = 10, # number of hub genes to include for the UMAP embedding
      n_neighbors=15, # neighbors parameter for UMAP
      min_dist=0.1 # min distance between points in UMAP space
    ),
    error = function(e) {
      module_umap_ready <<- FALSE
      warning(paste0("Module UMAP embedding unavailable: ", conditionMessage(e)))
      single_multiple_sample_clustering
    }
  )
  
  output_file2 <- paste0("ModuleUMAPPlot_", run_stamp, ".pdf")          # File name for the PDF
  pdf_path2 <- file.path(pdf_output_dir, output_file2)
  if (module_umap_ready) {
    safe_pdf_plot(
      pdf_path2,
      function() ModuleUMAPPlot(
        single_multiple_sample_clustering,
        edge.alpha=0.25,
        sample_edges=TRUE,
        edge_prop=0.1, # proportion of edges to sample (20% here)
        label_hubs=index_s_hdwgcna13,# how many hub genes to plot per module?
        keep_grey_edges=index_s_hdwgcna14
      ),
      warning_prefix = "Module UMAP PDF unavailable: "
    )
  } else {
    safe_pdf_plot(
      pdf_path2,
      function() make_placeholder_plot("Module UMAP plot unavailable for this dataset."),
      width = 8,
      height = 6,
      warning_prefix = "Module UMAP PDF unavailable: "
    )
  }
  
  tom_dir <- file.path(analysis_wd, "TOM")
  unlink(tom_dir, recursive = TRUE)
  
  return(list(plot801 = plots801, plot802 = plots802, plot804 = plots804, plot805 = plots805, plot807 = plots807, text_summary = pdf_output_dir, data1 = power_table, data2 = modules, data3 = hub_df, data4 = single_multiple_sample_clustering, plot810 = plots810, dendrogram_file = output_file, correlogram_file = output_file1, module_networks_file = output_pdf_name, module_umap_file = output_file2))
}
