datainput_multiple_celltype <- function(index_multiple_celltype_input, index_cell_markers, index_m_celltype1, index_m_celltype2, index_m_celltype3, index_m_celltype4, index_m_celltype5, index_m_celltype6, index_m_celltype7, index_m_celltype8, index_m_celltype9, index_m_clustering6, index_multiple_sample_normalization_method){
  source_app_script("scripts/assay_utils.R")
  multiple_sample_clustering <- index_multiple_celltype_input
  multiple_sample_clustering_markers <- index_cell_markers
  index_m_celltype8 <- isTRUE(as.logical(index_m_celltype8))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  pick_annotation_assay <- function(obj, normalization_method) {
    assays <- tryCatch(Seurat::Assays(obj), error = function(e) character(0))
    if (identical(normalization_method, "SCTransform") && "SCT" %in% assays) {
      return("SCT")
    }
    if (identical(normalization_method, "LogNormalize") && "integrated" %in% assays) {
      return("integrated")
    }
    if ("RNA" %in% assays) {
      return("RNA")
    }
    if ("Spatial" %in% assays) {
      return("Spatial")
    }
    assays[[1]] %||% Seurat::DefaultAssay(obj)
  }

  get_assay_matrix_with_fallback <- function(obj, assay_name, preferred_layers = c("data", "counts"), as_sparse = FALSE) {
    for (layer_name in preferred_layers) {
      mat <- tryCatch(
        get_assay_layer_matrix(
          object = obj,
          assay = assay_name,
          layer = layer_name,
          slot_fallback = layer_name,
          as_sparse = as_sparse
        ),
        error = function(e) NULL
      )
      if (!is.null(mat) && nrow(mat) > 0 && ncol(mat) > 0) {
        return(mat)
      }
    }
    stop("Could not retrieve a usable matrix for assay '", assay_name, "'.")
  }

  get_sctype_input <- function(obj, assay_name) {
    scaled_mat <- tryCatch(
      get_assay_layer_matrix(
        object = obj,
        assay = assay_name,
        layer = "scale.data",
        slot_fallback = "scale.data",
        as_sparse = FALSE
      ),
      error = function(e) NULL
    )

    if (!is.null(scaled_mat) && nrow(scaled_mat) > 0 && ncol(scaled_mat) > 0) {
      return(list(mat = as.matrix(scaled_mat), scaled = TRUE))
    }

    fallback_mat <- get_assay_matrix_with_fallback(
      obj,
      assay_name = assay_name,
      preferred_layers = c("data", "counts"),
      as_sparse = FALSE
    )
    list(mat = as.matrix(fallback_mat), scaled = FALSE)
  }

  sanitize_annotation_values <- function(values, unknown_label = "Unknown") {
    values <- as.character(values)
    values[is.na(values) | !nzchar(values)] <- unknown_label
    factor(values)
  }

  resolve_split_by <- function(obj, requested_split) {
    if (is.null(requested_split) || !nzchar(requested_split) || identical(requested_split, "none")) {
      return(NULL)
    }
    if (!(requested_split %in% colnames(obj@meta.data))) {
      return(NULL)
    }
    split_values <- obj@meta.data[[requested_split]]
    if (all(is.na(split_values)) || length(unique(stats::na.omit(split_values))) == 0) {
      return(NULL)
    }
    requested_split
  }

  make_message_plot <- function(message_text) {
    ggplot2::ggplot(data.frame(x = 0, y = 0, label = message_text), ggplot2::aes(x = x, y = y, label = label)) +
      ggplot2::geom_text(size = 4.5) +
      ggplot2::theme_void() +
      ggplot2::xlim(-1, 1) +
      ggplot2::ylim(-1, 1)
  }

  build_annotation_dimplot <- function(obj, annotation_field, reduction_name, label_points, requested_split) {
    plot_args <- list(
      object = obj,
      reduction = reduction_name,
      group.by = annotation_field,
      label = label_points,
      repel = TRUE
    )
    split_field <- resolve_split_by(obj, requested_split)
    if (!is.null(split_field)) {
      plot_args$split.by <- split_field
    }
    tryCatch(
      do.call(Seurat::DimPlot, plot_args),
      error = function(e) {
        plot_args$split.by <- NULL
        do.call(Seurat::DimPlot, plot_args)
      }
    )
  }

  build_annotation_spatialplot <- function(obj, annotation_field, label_points, requested_split) {
    plot_args <- list(
      object = obj,
      group.by = annotation_field,
      label = label_points
    )
    split_field <- resolve_split_by(obj, requested_split)
    if (!is.null(split_field)) {
      plot_args$split.by <- split_field
    }
    tryCatch(
      do.call(Seurat::SpatialDimPlot, plot_args),
      error = function(e) {
        plot_args$split.by <- NULL
        do.call(Seurat::SpatialDimPlot, plot_args)
      }
    )
  }

  build_annotation_result <- function(obj, annotation_field, table_data, extra_plot1 = NULL, extra_plot2 = NULL) {
    annotation_values <- unique(stats::na.omit(as.character(obj@meta.data[[annotation_field]])))
    annotation_values <- annotation_values[nzchar(annotation_values)]
    dim_plot <- build_annotation_dimplot(
      obj = obj,
      annotation_field = annotation_field,
      reduction_name = index_m_clustering6,
      label_points = index_m_celltype8,
      requested_split = index_m_celltype9
    )
    spatial_plot <- build_annotation_spatialplot(
      obj = obj,
      annotation_field = annotation_field,
      label_points = index_m_celltype8,
      requested_split = index_m_celltype9
    )
    placeholder_text <- "This annotation mode does not generate an additional diagnostic plot."
    if (is.null(extra_plot1)) {
      extra_plot1 <- make_message_plot(placeholder_text)
    }
    if (is.null(extra_plot2)) {
      extra_plot2 <- make_message_plot(placeholder_text)
    }
    list(
      data1 = obj,
      data2 = unique(obj@meta.data$seurat_clusters),
      data3 = annotation_values,
      text_summary = annotation_field,
      plot1 = dim_plot,
      plot2 = spatial_plot,
      table1 = as.data.frame(table_data),
      plot3 = extra_plot1,
      plot4 = extra_plot2
    )
  }

  if (as.character(index_m_celltype1) == "1") {
    checkGeneSymbols <- HGNChelper::checkGeneSymbols
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/gene_sets_prepare.R", local = TRUE)
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/sctype_score_.R", local = TRUE)
    db_ <- "https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/ScTypeDB_full.xlsx"
    tissue <- index_m_celltype2
    gs_list <- gene_sets_prepare(db_, tissue)
    sctype_assay <- pick_annotation_assay(multiple_sample_clustering, index_multiple_sample_normalization_method)
    sctype_input <- get_sctype_input(multiple_sample_clustering, assay_name = sctype_assay)
    scRNAseqData_scaled <- sctype_input$mat
    es.max <- sctype_score(
      scRNAseqData = scRNAseqData_scaled,
      scaled = sctype_input$scaled,
      gs = gs_list$gs_positive,
      gs2 = gs_list$gs_negative
    )
    cL_resutls <- do.call("rbind", lapply(unique(multiple_sample_clustering@meta.data$seurat_clusters), function(cl) {
      es.max.cl <- sort(
        rowSums(es.max[, rownames(multiple_sample_clustering@meta.data[multiple_sample_clustering@meta.data$seurat_clusters == cl, ])]),
        decreasing = TRUE
      )
      head(
        data.frame(
          cluster = cl,
          type = names(es.max.cl),
          scores = es.max.cl,
          ncells = sum(multiple_sample_clustering@meta.data$seurat_clusters == cl)
        ),
        10
      )
    }))
    sctype_scores <- cL_resutls %>% dplyr::group_by(cluster) %>% dplyr::top_n(n = 1, wt = scores)
    sctype_scores$type[as.numeric(as.character(sctype_scores$scores)) < sctype_scores$ncells / 4] <- "Unknown"
    multiple_sample_clustering@meta.data$sctype_classification <- ""
    for (j in unique(sctype_scores$cluster)) {
      cl_type <- sctype_scores[sctype_scores$cluster == j, ]
      multiple_sample_clustering@meta.data$sctype_classification[multiple_sample_clustering@meta.data$seurat_clusters == j] <- as.character(cl_type$type[1])
    }
    multiple_sample_clustering@meta.data$sctype_classification <- sanitize_annotation_values(
      multiple_sample_clustering@meta.data$sctype_classification
    )

    return(build_annotation_result(
      obj = multiple_sample_clustering,
      annotation_field = "sctype_classification",
      table_data = sctype_scores
    ))
  }

  if (as.character(index_m_celltype1) == "2") {
    ref <- celldex::fetchReference(index_m_celltype3, "2024-02-26")
    singleR_assay <- pick_annotation_assay(multiple_sample_clustering, index_multiple_sample_normalization_method)
    multiple_sample_clustering.counts <- get_assay_matrix_with_fallback(
      multiple_sample_clustering,
      assay_name = singleR_assay,
      preferred_layers = c("data", "counts"),
      as_sparse = TRUE
    )
    Pred <- SingleR::SingleR(
      test = multiple_sample_clustering.counts,
      ref = ref,
      labels = ref$label.main,
      assay.type.test = 1,
      de.method = index_m_celltype4
    )
    Pred1 <- Pred %>% data.table::as.data.table()
    Pred1$cells <- rownames(Pred)
    multiple_sample_clustering$singleR_labels <- sanitize_annotation_values(
      Pred$labels[match(rownames(multiple_sample_clustering@meta.data), rownames(Pred))]
    )

    return(build_annotation_result(
      obj = multiple_sample_clustering,
      annotation_field = "singleR_labels",
      table_data = Pred1,
      extra_plot1 = SingleR::plotScoreHeatmap(Pred),
      extra_plot2 = SingleR::plotDeltaDistribution(Pred)
    ))
  }

  if (as.character(index_m_celltype1) == "3") {
    res <- gptcelltype(multiple_sample_clustering_markers, model = index_m_celltype5, topgenenumber = index_m_celltype6)
    multiple_sample_clustering@meta.data$GPTCelltype <- sanitize_annotation_values(
      res[as.character(Seurat::Idents(multiple_sample_clustering))]
    )
    gpt_table <- data.frame(
      cluster = names(res),
      predicted_celltype = unname(res),
      stringsAsFactors = FALSE
    )

    return(build_annotation_result(
      obj = multiple_sample_clustering,
      annotation_field = "GPTCelltype",
      table_data = gpt_table
    ))
  }

  if (as.character(index_m_celltype1) == "4") {
    cell_type <- index_m_celltype7
    names(cell_type) <- levels(multiple_sample_clustering)
    multiple_sample_clustering@meta.data$cell_type <- sanitize_annotation_values(
      cell_type[as.character(Seurat::Idents(multiple_sample_clustering))]
    )
    manual_table <- data.frame(
      cluster = levels(multiple_sample_clustering),
      predicted_celltype = unname(cell_type),
      stringsAsFactors = FALSE
    )

    return(build_annotation_result(
      obj = multiple_sample_clustering,
      annotation_field = "cell_type",
      table_data = manual_table
    ))
  }

  stop("Unsupported cell type annotation mode: ", index_m_celltype1)
}
