# Helpers for restoring Seurat objects downloaded from VST-DAVis.
# This file intentionally contains no Shiny state or UI side effects.

saved_seurat_object_specs <- data.frame(
  file_name = c(
    "seurat_object_before_qc.RDS",
    "multiple_sample_seurat_object_after_qc.RDS",
    "multiple_sample_seurat_object_after_normalization.RDS",
    "multiple_sample_seurat_object_after_clustering.RDS",
    "multiple_sample_seurat_object_after_marker_identification.RDS",
    "multiple_sample_seurat_object_after_celltypes.RDS",
    "multiple_sample_seurat_object_after_plots.RDS",
    "multiple_sample_subclustering_seurat_object.RDS",
    "subclustering_multiple_sample_seurat_object_after_normalization.RDS",
    "subclustering_multiple_sample_seurat_object_after_clustering.RDS",
    "subclustering_multiple_sample_seurat_object_after_marker_identification.RDS",
    "subclustering_multiple_sample_seurat_object_after_celltypes.RDS",
    "subclustering_multiple_sample_seurat_object_after_plots.RDS"
  ),
  workflow = c(rep("multiple", 7), rep("subclustering", 6)),
  stage = c(
    "stats",
    "qc",
    "normalization",
    "clustering",
    "marker",
    "celltype",
    "plots",
    "cell_stats",
    "normalization",
    "clustering",
    "marker",
    "celltype",
    "plots"
  ),
  tab = c(
    "Stats",
    "Sample Groups and QC Filtering",
    "Normalization and PCA Analysis",
    "Clustering",
    "Markers Identification",
    "Cell Type Prediction",
    "Cluster-Based Plots",
    "Cell Stats",
    "Normalization and PCA Analysis",
    "Clustering",
    "Markers Identification",
    "Cell Type Prediction",
    "Cluster-Based Plots"
  ),
  stage_rank = c(seq_len(7), seq_len(6)),
  stringsAsFactors = FALSE
)

accepted_saved_seurat_object_names <- saved_seurat_object_specs$file_name
canonical_saved_seurat_object_names <- accepted_saved_seurat_object_names

get_saved_seurat_spec <- function(file_name) {
  if (length(file_name) != 1L || is.na(file_name) || !nzchar(file_name)) {
    return(NULL)
  }
  match_index <- match(as.character(file_name), saved_seurat_object_specs$file_name)
  if (is.na(match_index)) {
    return(NULL)
  }
  saved_seurat_object_specs[match_index, , drop = FALSE]
}

match_saved_seurat_filename <- get_saved_seurat_spec

is_accepted_saved_seurat_filename <- function(file_name) {
  !is.null(get_saved_seurat_spec(file_name))
}

empty_shortcut_plot <- function(message) {
  message <- paste(as.character(message), collapse = " ")
  if (!nzchar(message)) {
    message <- "This plot is not available in the uploaded Seurat object."
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(structure(
      list(message = message),
      class = c("vstdavis_placeholder_plot", "ggplot")
    ))
  }

  ggplot2::ggplot(data.frame(x = 0, y = 0)) +
    ggplot2::annotate("text", x = 0, y = 0, label = message, size = 4.5) +
    ggplot2::theme_void() +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1)
}

is_seurat_object <- function(object) {
  inherits(object, "Seurat")
}

shortcut_metadata <- function(object) {
  tryCatch(object@meta.data, error = function(e) data.frame())
}

shortcut_assays <- function(object) {
  tryCatch(names(object@assays), error = function(e) character(0))
}

shortcut_reductions <- function(object) {
  tryCatch(names(object@reductions), error = function(e) character(0))
}

shortcut_images <- function(object) {
  tryCatch(names(object@images), error = function(e) character(0))
}

shortcut_default_assay <- function(object) {
  default_assay <- tryCatch(
    SeuratObject::DefaultAssay(object),
    error = function(e) NA_character_
  )
  if (length(default_assay) == 1L && !is.na(default_assay) && nzchar(default_assay)) {
    return(default_assay)
  }
  assays <- shortcut_assays(object)
  if (length(assays)) assays[[1]] else NA_character_
}

shortcut_feature_assay <- function(object) {
  assays <- shortcut_assays(object)
  if (!length(assays)) {
    return(NA_character_)
  }
  preferred <- unique(c("Spatial", shortcut_default_assay(object), "RNA", "SCT", assays))
  preferred <- preferred[!is.na(preferred) & preferred %in% assays]
  if (length(preferred)) preferred[[1]] else assays[[1]]
}

shortcut_assay_features <- function(object, assay = shortcut_feature_assay(object)) {
  if (length(assay) != 1L || is.na(assay) || !nzchar(assay)) {
    return(character(0))
  }
  tryCatch(rownames(object[[assay]]), error = function(e) character(0))
}

ensure_shortcut_seurat_object <- function(object) {
  if (!is_seurat_object(object)) {
    stop("The supplied object is not a Seurat object.", call. = FALSE)
  }

  cell_count <- ncol(object)
  metadata <- shortcut_metadata(object)

  orig_ident <- if ("orig.ident" %in% colnames(metadata)) {
    as.character(metadata[["orig.ident"]])
  } else {
    rep("Sample1", cell_count)
  }
  if (length(orig_ident) != cell_count) {
    orig_ident <- rep("Sample1", cell_count)
  }
  orig_ident[is.na(orig_ident) | !nzchar(orig_ident)] <- "Sample1"
  object[["orig.ident"]] <- orig_ident

  metadata <- shortcut_metadata(object)
  condition <- if ("condition" %in% colnames(metadata)) {
    as.character(metadata[["condition"]])
  } else {
    orig_ident
  }
  if (length(condition) != cell_count) {
    condition <- orig_ident
  }
  missing_condition <- is.na(condition) | !nzchar(condition)
  condition[missing_condition] <- orig_ident[missing_condition]
  object[["condition"]] <- condition

  metadata <- shortcut_metadata(object)
  clusters <- if ("seurat_clusters" %in% colnames(metadata)) {
    as.character(metadata[["seurat_clusters"]])
  } else {
    tryCatch(as.character(SeuratObject::Idents(object)), error = function(e) character(0))
  }
  if (length(clusters) != cell_count) {
    clusters <- rep("0", cell_count)
  }
  clusters[is.na(clusters) | !nzchar(clusters)] <- "0"
  object[["seurat_clusters"]] <- clusters

  metadata <- shortcut_metadata(object)
  if ("percent.mt" %in% colnames(metadata)) {
    percent_mt <- suppressWarnings(as.numeric(metadata[["percent.mt"]]))
    percent_mt[!is.finite(percent_mt)] <- 0
  } else {
    assay <- shortcut_feature_assay(object)
    features <- shortcut_assay_features(object, assay)
    mt_pattern <- if (any(grepl("^MT-", features))) {
      "^MT-"
    } else if (any(grepl("^mt-", features))) {
      "^mt-"
    } else {
      NULL
    }
    percent_mt <- if (is.null(mt_pattern)) {
      rep(0, cell_count)
    } else {
      tryCatch(
        Seurat::PercentageFeatureSet(object, pattern = mt_pattern, assay = assay),
        error = function(e) rep(0, cell_count)
      )
    }
    percent_mt <- suppressWarnings(as.numeric(percent_mt))
    if (length(percent_mt) != cell_count) {
      percent_mt <- rep(0, cell_count)
    }
    percent_mt[!is.finite(percent_mt)] <- 0
  }
  object[["percent.mt"]] <- percent_mt

  tryCatch({
    SeuratObject::Idents(object) <- "seurat_clusters"
    object
  }, error = function(e) object)
}

prepare_seurat_object_for_download <- function(object) {
  if (is.list(object) && length(object) == 1L && is_seurat_object(object[[1]])) {
    object <- object[[1]]
  }
  if (is_seurat_object(object)) {
    object <- ensure_shortcut_seurat_object(object)
  }
  object
}

shortcut_qc_feature_names <- function(object) {
  metadata_names <- colnames(shortcut_metadata(object))
  assays <- shortcut_assays(object)
  assay_order <- unique(c("Spatial", shortcut_default_assay(object), "RNA", "SCT", assays))
  assay_order <- assay_order[!is.na(assay_order) & assay_order %in% assays]

  for (assay in assay_order) {
    feature_column <- paste0("nFeature_", assay)
    count_column <- paste0("nCount_", assay)
    if (all(c(feature_column, count_column) %in% metadata_names)) {
      return(list(feature = feature_column, count = count_column, percent = "percent.mt"))
    }
  }

  feature_columns <- grep("^nFeature_", metadata_names, value = TRUE)
  for (feature_column in feature_columns) {
    count_column <- sub("^nFeature_", "nCount_", feature_column)
    if (count_column %in% metadata_names) {
      return(list(feature = feature_column, count = count_column, percent = "percent.mt"))
    }
  }

  list(feature = NA_character_, count = NA_character_, percent = "percent.mt")
}

shortcut_qc_features <- function(object, include_percent = TRUE) {
  columns <- shortcut_qc_feature_names(object)
  features <- c(columns$feature, columns$count)
  if (isTRUE(include_percent)) {
    features <- c(features, columns$percent)
  }
  unique(features[!is.na(features) & features %in% colnames(shortcut_metadata(object))])
}

choose_shortcut_reduction <- function(object, preferred = NULL) {
  reductions <- shortcut_reductions(object)
  if (length(preferred) == 1L && !is.na(preferred) && preferred %in% reductions) {
    return(preferred)
  }
  candidates <- c("umap", "tsne", "pca", "harmony")
  available <- candidates[candidates %in% reductions]
  if (length(available)) available[[1]] else NULL
}

safe_shortcut_dim_plot <- function(
    object,
    group_by = NULL,
    split_by = NULL,
    preferred_reduction = NULL,
    label = FALSE,
    ncol = 6) {
  reduction <- choose_shortcut_reduction(object, preferred_reduction)
  if (is.null(reduction)) {
    return(empty_shortcut_plot("No dimensional reduction was found in the uploaded Seurat object."))
  }

  metadata_names <- colnames(shortcut_metadata(object))
  plot_args <- list(
    object = object,
    reduction = reduction,
    raster = FALSE,
    label = isTRUE(as.logical(label))
  )
  if (length(group_by) == 1L && group_by %in% metadata_names) {
    plot_args$group.by <- group_by
  }
  if (length(split_by) == 1L && split_by %in% metadata_names) {
    plot_args$split.by <- split_by
    plot_args$ncol <- ncol
  }

  tryCatch(
    do.call(Seurat::DimPlot, plot_args),
    error = function(e) empty_shortcut_plot(paste("Dimensional plot unavailable:", conditionMessage(e)))
  )
}

safe_shortcut_vln_plot <- function(object, features = NULL, group_by = NULL, show_dots = TRUE) {
  if (is.null(features)) {
    features <- shortcut_qc_features(object)
  }
  available_features <- unique(c(colnames(shortcut_metadata(object)), rownames(object)))
  features <- as.character(features)
  features <- features[features %in% available_features]
  if (!length(features)) {
    return(empty_shortcut_plot("QC features were not found in the uploaded Seurat object."))
  }

  plot_args <- list(
    object = object,
    features = features,
    ncol = 1,
    pt.size = if (isTRUE(as.logical(show_dots))) 0.1 else 0,
    raster = FALSE
  )
  if (length(group_by) == 1L && group_by %in% colnames(shortcut_metadata(object))) {
    plot_args$group.by <- group_by
  }
  tryCatch(
    do.call(Seurat::VlnPlot, plot_args),
    error = function(e) empty_shortcut_plot(paste("QC plot unavailable:", conditionMessage(e)))
  )
}

shortcut_combine_plots <- function(plots, message) {
  plots <- Filter(Negate(is.null), plots)
  if (!length(plots)) {
    return(empty_shortcut_plot(message))
  }
  if (length(plots) == 1L || !requireNamespace("patchwork", quietly = TRUE)) {
    return(plots[[1]])
  }
  tryCatch(
    patchwork::wrap_plots(plots),
    error = function(e) plots[[1]]
  )
}

safe_shortcut_feature_scatter <- function(object) {
  columns <- shortcut_qc_feature_names(object)
  metadata_names <- colnames(shortcut_metadata(object))
  pairs <- list(
    c(columns$feature, columns$percent),
    c(columns$feature, columns$count)
  )
  pairs <- Filter(function(pair) all(!is.na(pair) & pair %in% metadata_names), pairs)
  plots <- lapply(pairs, function(pair) {
    tryCatch(
      Seurat::FeatureScatter(object, feature1 = pair[[1]], feature2 = pair[[2]]),
      error = function(e) NULL
    )
  })
  shortcut_combine_plots(plots, "Feature relationship data were not found in the uploaded Seurat object.")
}

safe_shortcut_spatial_feature_plot <- function(object, features = NULL) {
  if (!length(shortcut_images(object))) {
    return(empty_shortcut_plot("No spatial image was found in the uploaded Seurat object."))
  }
  if (is.null(features)) {
    features <- shortcut_qc_features(object)
  }
  available_features <- unique(c(colnames(shortcut_metadata(object)), rownames(object)))
  features <- as.character(features)
  features <- features[features %in% available_features]
  if (!length(features)) {
    return(empty_shortcut_plot("Spatial plot features were not found in the uploaded Seurat object."))
  }
  tryCatch(
    Seurat::SpatialFeaturePlot(object, features = features),
    error = function(e) empty_shortcut_plot(paste("Spatial feature plot unavailable:", conditionMessage(e)))
  )
}

safe_shortcut_spatial_dim_plot <- function(object, group_by = NULL, label = FALSE) {
  if (!length(shortcut_images(object))) {
    return(empty_shortcut_plot("No spatial image was found in the uploaded Seurat object."))
  }
  plot_args <- list(object = object, label = isTRUE(as.logical(label)))
  if (length(group_by) == 1L && group_by %in% colnames(shortcut_metadata(object))) {
    plot_args$group.by <- group_by
  }
  tryCatch(
    do.call(Seurat::SpatialDimPlot, plot_args),
    error = function(e) empty_shortcut_plot(paste("Spatial cluster plot unavailable:", conditionMessage(e)))
  )
}

shortcut_count_table <- function(object, column, label, count_label = "Counts") {
  metadata <- shortcut_metadata(object)
  if (!column %in% colnames(metadata)) {
    return(data.frame(
      Message = paste("Column not found:", column),
      stringsAsFactors = FALSE
    ))
  }

  values <- metadata[[column]]
  character_values <- as.character(values)
  character_values[is.na(character_values) | !nzchar(character_values)] <- "NA"
  value_order <- if (is.factor(values)) {
    c(levels(values)[levels(values) %in% character_values], setdiff(unique(character_values), levels(values)))
  } else {
    unique(character_values)
  }
  counts <- as.integer(table(factor(character_values, levels = value_order)))
  result <- data.frame(value_order, counts, stringsAsFactors = FALSE, check.names = FALSE)
  colnames(result) <- c(label, count_label)
  result
}

shortcut_wide_count_table <- function(
    object,
    row_column,
    cluster_column = "seurat_clusters",
    row_label = row_column) {
  metadata <- shortcut_metadata(object)
  if (!all(c(row_column, cluster_column) %in% colnames(metadata))) {
    return(data.frame(
      Message = paste("Required columns not found:", row_column, "and", cluster_column),
      stringsAsFactors = FALSE
    ))
  }
  row_values <- as.character(metadata[[row_column]])
  cluster_values <- as.character(metadata[[cluster_column]])
  row_values[is.na(row_values) | !nzchar(row_values)] <- "NA"
  cluster_values[is.na(cluster_values) | !nzchar(cluster_values)] <- "NA"
  count_matrix <- as.data.frame.matrix(table(row_values, cluster_values))
  data.frame(
    setNames(list(rownames(count_matrix)), row_label),
    count_matrix,
    check.names = FALSE,
    row.names = NULL
  )
}

normalize_shortcut_count_table <- function(
    table_df,
    label,
    count_label,
    label_candidates,
    count_candidates) {
  if (!is.data.frame(table_df) || !nrow(table_df)) {
    return(NULL)
  }
  label_column <- intersect(label_candidates, colnames(table_df))
  count_column <- intersect(count_candidates, colnames(table_df))
  if (!length(label_column) || !length(count_column)) {
    return(NULL)
  }
  result <- data.frame(
    as.character(table_df[[label_column[[1]]]]),
    suppressWarnings(as.numeric(table_df[[count_column[[1]]]])),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  colnames(result) <- c(label, count_label)
  result[[count_label]][!is.finite(result[[count_label]])] <- 0
  result
}

shortcut_first_non_null <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value)) {
      return(value)
    }
  }
  NULL
}

shortcut_misc <- function(object) {
  tryCatch(object@misc, error = function(e) list())
}

shortcut_stored_stage_result <- function(object, stage) {
  misc <- shortcut_misc(object)
  stage_keys <- switch(
    stage,
    marker = c("marker", "markers"),
    celltype = c("celltype", "cell_type", "annotation"),
    clusterbased = c("clusterbased", "cluster_based", "plots"),
    conditionbased = c("conditionbased", "condition_based"),
    qc = c("qc", "qc_counts"),
    stage
  )
  containers <- list(
    misc$vstdavis_results,
    misc$VSTDAVis$results,
    misc$VSTDAVis,
    misc$vstdavis,
    misc$VST_DAVis,
    misc$ScRDAVis,
    misc
  )
  for (container in containers) {
    if (!is.list(container)) {
      next
    }
    for (stage_key in stage_keys) {
      value <- container[[stage_key]]
      if (!is.null(value)) {
        return(value)
      }
    }
  }
  NULL
}

get_shortcut_stored_qc_counts <- function(object) {
  misc <- shortcut_misc(object)
  stored_qc <- shortcut_stored_stage_result(object, "qc")
  qc_sources <- list(
    if (is.list(stored_qc)) stored_qc$qc_counts else NULL,
    stored_qc,
    misc$VSTDAVis$qc_counts,
    misc$vstdavis$qc_counts,
    misc$VST_DAVis$qc_counts,
    misc$ScRDAVis$qc_counts,
    misc$qc_counts
  )
  qc_sources <- Filter(is.list, qc_sources)

  for (qc_counts in qc_sources) {
    sample_before <- normalize_shortcut_count_table(
      shortcut_first_non_null(qc_counts$sample_before, qc_counts$samples_before, qc_counts$sample_counts_before),
      "Samples",
      "Cell counts before QC",
      c("Samples", "Sample names", "orig.ident"),
      c("Cell counts before QC", "Cell counts", "Counts")
    )
    sample_after <- normalize_shortcut_count_table(
      shortcut_first_non_null(qc_counts$sample_after, qc_counts$samples_after, qc_counts$sample_counts_after),
      "Samples",
      "Cell counts after QC",
      c("Samples", "Sample names", "orig.ident"),
      c("Cell counts after QC", "Cell counts", "Counts", "No of cells used for further analysis")
    )
    group_before <- normalize_shortcut_count_table(
      shortcut_first_non_null(qc_counts$group_before, qc_counts$groups_before, qc_counts$group_counts_before),
      "Groups",
      "Cell counts before QC",
      c("Groups", "condition"),
      c("Cell counts before QC", "Cell counts", "Counts")
    )
    group_after <- normalize_shortcut_count_table(
      shortcut_first_non_null(qc_counts$group_after, qc_counts$groups_after, qc_counts$group_counts_after),
      "Groups",
      "Cell counts after QC",
      c("Groups", "condition"),
      c("Cell counts after QC", "Cell counts", "Counts", "No of cells used for further analysis")
    )
    if (all(vapply(
      list(sample_before, sample_after, group_before, group_after),
      Negate(is.null),
      logical(1)
    ))) {
      return(list(
        sample_before = sample_before,
        sample_after = sample_after,
        group_before = group_before,
        group_after = group_after
      ))
    }
  }
  NULL
}

shortcut_qc_count_tables <- function(object) {
  stored_counts <- get_shortcut_stored_qc_counts(object)
  if (!is.null(stored_counts)) {
    return(stored_counts)
  }

  sample_after <- shortcut_count_table(object, "orig.ident", "Samples", "Cell counts after QC")
  group_after <- shortcut_count_table(object, "condition", "Groups", "Cell counts after QC")
  sample_before <- sample_after
  group_before <- group_after
  if ("Cell counts after QC" %in% colnames(sample_before)) {
    colnames(sample_before)[colnames(sample_before) == "Cell counts after QC"] <- "Cell counts before QC"
  }
  if ("Cell counts after QC" %in% colnames(group_before)) {
    colnames(group_before)[colnames(group_before) == "Cell counts after QC"] <- "Cell counts before QC"
  }
  list(
    sample_before = sample_before,
    sample_after = sample_after,
    group_before = group_before,
    group_after = group_after
  )
}

shortcut_qc_bar_plot <- function(before_table, after_table, entity_column) {
  before_count <- "Cell counts before QC"
  after_count <- "Cell counts after QC"
  required_before <- c(entity_column, before_count)
  required_after <- c(entity_column, after_count)
  if (!is.data.frame(before_table) || !is.data.frame(after_table) ||
      !all(required_before %in% colnames(before_table)) ||
      !all(required_after %in% colnames(after_table))) {
    return(empty_shortcut_plot(paste("QC counts are not available for", entity_column)))
  }

  merged_counts <- merge(before_table, after_table, by = entity_column, all = TRUE, sort = FALSE)
  merged_counts[[before_count]][is.na(merged_counts[[before_count]])] <- 0
  merged_counts[[after_count]][is.na(merged_counts[[after_count]])] <- 0
  entities <- as.character(merged_counts[[entity_column]])
  plot_data <- data.frame(
    entity = factor(rep(entities, times = 2), levels = entities),
    variable = factor(
      rep(c(before_count, after_count), each = length(entities)),
      levels = c(before_count, after_count)
    ),
    Cell_counts = c(merged_counts[[before_count]], merged_counts[[after_count]]),
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = entity, y = Cell_counts, fill = variable)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge()) +
    ggplot2::geom_text(
      ggplot2::aes(label = Cell_counts),
      vjust = 1.6,
      position = ggplot2::position_dodge(0.9),
      color = "white",
      size = 3.5
    ) +
    ggplot2::labs(x = entity_column, fill = "Cell count") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1))
}

shortcut_bar_plot <- function(object, x_column, fill_column = x_column, label = FALSE) {
  metadata <- shortcut_metadata(object)
  if (!all(c(x_column, fill_column) %in% colnames(metadata))) {
    return(empty_shortcut_plot(paste("Bar plot columns not found:", x_column, "and", fill_column)))
  }
  plot_data <- data.frame(
    x_value = metadata[[x_column]],
    fill_value = metadata[[fill_column]],
    stringsAsFactors = FALSE
  )
  result <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = x_value, fill = fill_value)
  ) +
    ggplot2::geom_bar() +
    ggplot2::labs(x = x_column, fill = fill_column) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1))
  if (isTRUE(as.logical(label))) {
    result <- result + ggplot2::geom_text(
      stat = "count",
      ggplot2::aes(label = ggplot2::after_stat(count)),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3.5
    )
  }
  result
}

shortcut_marker_table <- function(object) {
  stored_result <- shortcut_stored_stage_result(object, "marker")
  misc <- shortcut_misc(object)
  candidates <- list(
    if (is.list(stored_result)) stored_result$data1 else stored_result,
    misc$VSTDAVis$markers,
    misc$vstdavis$markers,
    misc$ScRDAVis$markers,
    misc$markers
  )
  for (candidate in candidates) {
    if (is.data.frame(candidate)) {
      return(candidate)
    }
  }
  data.frame(
    Message = paste(
      "Marker results were not stored inside this Seurat object.",
      "Rerun marker identification to regenerate this table."
    ),
    stringsAsFactors = FALSE
  )
}

shortcut_annotation_column <- function(object) {
  metadata_names <- colnames(shortcut_metadata(object))
  misc <- shortcut_misc(object)
  stored_field <- shortcut_first_non_null(
    misc$vstdavis_annotation_field,
    misc$VSTDAVis$annotation_field,
    misc$vstdavis$annotation_field
  )
  if (length(stored_field) == 1L && stored_field %in% metadata_names) {
    return(stored_field)
  }
  candidates <- c("sctype_classification", "singleR_labels", "GPTCelltype", "cell_type")
  available <- candidates[candidates %in% metadata_names]
  if (length(available)) available[[1]] else "seurat_clusters"
}

shortcut_stored_plot <- function(stored_result, plot_name, message) {
  plot <- if (is.list(stored_result)) stored_result[[plot_name]] else NULL
  if (is.null(plot)) empty_shortcut_plot(message) else plot
}

make_shortcut_stats_result <- function(object, file_name = NULL, show_dots = TRUE) {
  object <- ensure_shortcut_seurat_object(object)
  sample_table <- shortcut_count_table(object, "orig.ident", "Sample names", "Cell counts")
  sample_names <- if ("Sample names" %in% colnames(sample_table)) {
    sample_table[["Sample names"]]
  } else {
    unique(as.character(shortcut_metadata(object)$orig.ident))
  }
  split_object <- tryCatch(
    Seurat::SplitObject(object, split.by = "orig.ident"),
    error = function(e) list(All = object)
  )
  qc_features <- shortcut_qc_features(object)

  # Keep the same nine positions returned by datainput_multiple_sample().
  list(
    is_valid = TRUE,
    text_summary = if (is.null(file_name)) "Previously generated VST-DAVis Seurat object" else file_name,
    plot1 = safe_shortcut_vln_plot(object, qc_features, group_by = "orig.ident", show_dots = show_dots),
    Plot3 = safe_shortcut_feature_scatter(object),
    data2 = object,
    data3 = sample_names,
    data4 = split_object,
    Plot2 = safe_shortcut_spatial_feature_plot(object, qc_features),
    data1 = sample_table
  )
}

make_shortcut_qc_result <- function(object, show_dots = TRUE) {
  object <- ensure_shortcut_seurat_object(object)
  qc_features <- shortcut_qc_features(object)
  qc_counts <- shortcut_qc_count_tables(object)

  # Keep the same eight positions returned by datainput_multiple_qc_filter().
  list(
    plot1 = safe_shortcut_vln_plot(object, qc_features, group_by = "orig.ident", show_dots = show_dots),
    plot2 = safe_shortcut_vln_plot(object, qc_features, group_by = "condition", show_dots = show_dots),
    plot3 = shortcut_qc_bar_plot(qc_counts$sample_before, qc_counts$sample_after, "Samples"),
    plot4 = shortcut_qc_bar_plot(qc_counts$group_before, qc_counts$group_after, "Groups"),
    data1 = qc_counts$sample_after,
    data2 = qc_counts$group_after,
    data3 = object,
    plot5 = safe_shortcut_spatial_feature_plot(object, qc_features)
  )
}

make_shortcut_normalization_result <- function(object) {
  object <- ensure_shortcut_seurat_object(object)
  has_pca <- "pca" %in% shortcut_reductions(object)
  pca_heatmap <- if (has_pca) {
    tryCatch(
      Seurat::DimHeatmap(object, dims = 1, cells = min(500, ncol(object)), balanced = TRUE, fast = FALSE),
      error = function(e) empty_shortcut_plot(paste("PCA heatmap unavailable:", conditionMessage(e)))
    )
  } else {
    empty_shortcut_plot("No PCA reduction was found in the uploaded Seurat object.")
  }
  elbow_plot <- if (has_pca) {
    tryCatch(
      Seurat::ElbowPlot(object),
      error = function(e) empty_shortcut_plot(paste("Elbow plot unavailable:", conditionMessage(e)))
    )
  } else {
    empty_shortcut_plot("No PCA reduction was found in the uploaded Seurat object.")
  }

  list(
    plot1 = pca_heatmap,
    plot2 = elbow_plot,
    plot3 = safe_shortcut_dim_plot(object, preferred_reduction = "pca"),
    plot4 = safe_shortcut_dim_plot(object, group_by = "condition", preferred_reduction = "pca"),
    data1 = object
  )
}

make_shortcut_clustering_result <- function(object, preferred_reduction = NULL, label = FALSE) {
  object <- ensure_shortcut_seurat_object(object)
  metadata <- shortcut_metadata(object)
  clusters <- unique(as.character(metadata$seurat_clusters))
  numeric_clusters <- suppressWarnings(as.numeric(clusters))
  max_cluster <- suppressWarnings(max(numeric_clusters, na.rm = TRUE))
  if (!length(max_cluster) || !is.finite(max_cluster)) {
    max_cluster <- length(clusters)
  }

  # Keep the same 19 positions returned by both VST clustering functions.
  list(
    plot1 = safe_shortcut_dim_plot(object, "seurat_clusters", preferred_reduction = preferred_reduction, label = label),
    plot2 = shortcut_bar_plot(object, "seurat_clusters", "seurat_clusters", label = label),
    plot3 = safe_shortcut_dim_plot(object, "condition", preferred_reduction = preferred_reduction, label = label),
    plot4 = shortcut_bar_plot(object, "seurat_clusters", "condition", label = label),
    plot5 = safe_shortcut_dim_plot(object, "orig.ident", preferred_reduction = preferred_reduction, label = label),
    plot6 = shortcut_bar_plot(object, "seurat_clusters", "orig.ident", label = label),
    data1 = shortcut_count_table(object, "seurat_clusters", "Clusters", "Counts"),
    data2 = shortcut_wide_count_table(object, "condition", "seurat_clusters", "condition"),
    data3 = shortcut_wide_count_table(object, "orig.ident", "seurat_clusters", "Clusters"),
    data4 = object,
    data5 = clusters,
    data6 = max_cluster,
    data7 = unique(as.character(metadata$condition)),
    text_summary = "seurat_clusters",
    plot7 = safe_shortcut_spatial_dim_plot(object, "seurat_clusters", label = label),
    plot8 = safe_shortcut_spatial_dim_plot(object, "seurat_clusters", label = FALSE),
    plot9 = safe_shortcut_dim_plot(object, "seurat_clusters", "condition", preferred_reduction, label),
    plot10 = safe_shortcut_dim_plot(object, "seurat_clusters", "orig.ident", preferred_reduction, label),
    plot11 = safe_shortcut_dim_plot(object, "condition", "seurat_clusters", preferred_reduction, label)
  )
}

make_shortcut_marker_result <- function(object) {
  object <- ensure_shortcut_seurat_object(object)
  stored_result <- shortcut_stored_stage_result(object, "marker")
  list(
    data1 = shortcut_marker_table(object),
    data2 = object,
    plot1 = shortcut_stored_plot(
      stored_result,
      "plot1",
      "Marker heatmap was not stored in the uploaded Seurat object. Rerun marker identification to regenerate it."
    )
  )
}

make_shortcut_celltype_result <- function(object, preferred_reduction = NULL, label = FALSE) {
  object <- ensure_shortcut_seurat_object(object)
  metadata <- shortcut_metadata(object)
  annotation_column <- shortcut_annotation_column(object)
  stored_result <- shortcut_stored_stage_result(object, "celltype")
  stored_table <- if (is.list(stored_result) && is.data.frame(stored_result$table1)) {
    stored_result$table1
  } else {
    shortcut_count_table(object, annotation_column, "Cell type", "Cell counts")
  }

  list(
    data1 = object,
    data2 = unique(as.character(metadata$seurat_clusters)),
    data3 = unique(as.character(metadata[[annotation_column]])),
    text_summary = annotation_column,
    plot1 = safe_shortcut_dim_plot(object, annotation_column, preferred_reduction = preferred_reduction, label = label),
    plot2 = safe_shortcut_spatial_dim_plot(object, annotation_column, label = label),
    table1 = stored_table,
    plot3 = shortcut_stored_plot(
      stored_result,
      "plot3",
      "The cell type diagnostic plot was not stored in the uploaded Seurat object."
    ),
    plot4 = shortcut_stored_plot(
      stored_result,
      "plot4",
      "The additional cell type diagnostic plot was not stored in the uploaded Seurat object."
    )
  )
}

make_shortcut_clusterbased_result <- function(object) {
  object <- ensure_shortcut_seurat_object(object)
  stored_result <- shortcut_stored_stage_result(object, "clusterbased")
  stored_data <- if (is.list(stored_result) && !is.null(stored_result$data2)) {
    stored_result$data2
  } else {
    data.frame(
      Message = "Cluster-based plot results were not stored inside this Seurat object.",
      stringsAsFactors = FALSE
    )
  }
  list(
    plot1 = shortcut_stored_plot(
      stored_result,
      "plot1",
      "Cluster-based plot settings were not stored in the uploaded Seurat object. Generate the plot again to recreate this output."
    ),
    data1 = object,
    data2 = stored_data
  )
}

make_shortcut_conditionbased_result <- function(object) {
  object <- ensure_shortcut_seurat_object(object)
  stored_result <- shortcut_stored_stage_result(object, "conditionbased")
  stored_data <- if (is.list(stored_result) && is.data.frame(stored_result$data1)) {
    stored_result$data1
  } else {
    data.frame(
      Message = "Condition-based differential expression results were not stored inside this Seurat object.",
      stringsAsFactors = FALSE
    )
  }
  list(
    plot1 = shortcut_stored_plot(
      stored_result,
      "plot1",
      "Condition-based plot settings were not stored in the uploaded Seurat object. Submit the analysis again to recreate this output."
    ),
    data1 = stored_data,
    data2 = object
  )
}

make_shortcut_subclustering_stats_result <- function(object, show_dots = TRUE) {
  object <- ensure_shortcut_seurat_object(object)
  qc_features <- shortcut_qc_features(object, include_percent = FALSE)
  list(
    plot = safe_shortcut_vln_plot(object, qc_features, show_dots = show_dots),
    data1 = shortcut_count_table(object, "orig.ident", "Sample names", "Cell counts"),
    data2 = object,
    plot2 = safe_shortcut_spatial_feature_plot(object, qc_features)
  )
}

saved_seurat_restore_error <- function(error_type, title, message, file_name = NULL, detail = NULL) {
  list(
    ok = FALSE,
    error_type = error_type,
    title = title,
    message = message,
    file_name = file_name,
    detail = detail,
    accepted_names = accepted_saved_seurat_object_names
  )
}

saved_seurat_upload_fields <- function(uploaded_file) {
  if (is.character(uploaded_file) && length(uploaded_file) == 1L) {
    return(list(name = basename(uploaded_file), datapath = uploaded_file))
  }
  if (!is.list(uploaded_file) && !is.data.frame(uploaded_file)) {
    return(NULL)
  }
  file_name <- uploaded_file$name
  data_path <- uploaded_file$datapath
  if (!length(file_name) || !length(data_path)) {
    return(NULL)
  }
  list(name = as.character(file_name[[1]]), datapath = as.character(data_path[[1]]))
}

load_saved_seurat_shortcut <- function(uploaded_file) {
  upload <- saved_seurat_upload_fields(uploaded_file)
  if (is.null(upload) || !nzchar(upload$name) || !nzchar(upload$datapath)) {
    return(saved_seurat_restore_error(
      "missing_file",
      "No Seurat object uploaded",
      "Please choose a previously generated VST-DAVis Seurat object."
    ))
  }

  spec <- get_saved_seurat_spec(upload$name)
  if (is.null(spec)) {
    return(saved_seurat_restore_error(
      "filename_mismatch",
      "Incorrect Seurat object filename",
      paste(
        "Please upload a previously generated VST-DAVis Seurat object with one of the accepted filenames.",
        "Uploaded filename:",
        upload$name
      ),
      file_name = upload$name
    ))
  }

  read_result <- tryCatch(
    list(ok = TRUE, object = base::readRDS(upload$datapath)),
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )
  if (!isTRUE(read_result$ok)) {
    return(saved_seurat_restore_error(
      "read_error",
      "Unable to read Seurat object",
      paste("The uploaded file could not be read as an RDS file:", upload$name),
      file_name = upload$name,
      detail = read_result$error
    ))
  }

  object <- read_result$object
  if (is.list(object) && length(object) == 1L && is_seurat_object(object[[1]])) {
    object <- object[[1]]
  }
  if (!is_seurat_object(object)) {
    return(saved_seurat_restore_error(
      "not_seurat",
      "Uploaded RDS is not a Seurat object",
      paste("The filename is accepted, but the RDS content is not a Seurat object:", upload$name),
      file_name = upload$name
    ))
  }

  normalized <- tryCatch(
    list(ok = TRUE, object = ensure_shortcut_seurat_object(object)),
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )
  if (!isTRUE(normalized$ok)) {
    return(saved_seurat_restore_error(
      "normalization_error",
      "Unable to prepare Seurat object",
      paste("The Seurat object was read, but its metadata could not be prepared:", upload$name),
      file_name = upload$name,
      detail = normalized$error
    ))
  }

  spec_list <- lapply(spec[1, , drop = FALSE], function(value) value[[1]])
  list(
    ok = TRUE,
    file_name = upload$name,
    object = normalized$object,
    spec = spec_list,
    title = "Seurat object loaded",
    message = paste("Loaded", upload$name, "and restored", spec_list$tab, "for continued analysis.")
  )
}
