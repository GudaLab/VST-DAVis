datainput_multiple_qc_filter <- function(index_multiple_qc_input,
                                         index_multiple_qc_input1,
                                         index_multiple_group_count,
                                         index_group1_name,
                                         index_group1_samples,
                                         index_group2_name,
                                         index_group2_samples,
                                         index_group3_name,
                                         index_group3_samples,
                                         index_group4_name,
                                         index_group4_samples,
                                         index_group5_name,
                                         index_group5_samples,
                                         index_group6_name,
                                         index_group6_samples,
                                         index_multiple_sample_min_count,
                                         index_multiple_sample_max_count,
                                         index_multiple_sample_min_ncount = 0,
                                         index_multiple_sample_max_ncount = Inf,
                                         index_multiple_sample_max_mito_perc) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
      return(y)
    }
    x
  }

  safe_plot <- function(expr) {
    tryCatch(expr, error = function(e) NULL)
  }

  combine_plots <- function(...) {
    plots <- list(...)
    plots <- Filter(Negate(is.null), plots)
    if (length(plots) == 0) {
      return(NULL)
    }
    Reduce(`+`, plots)
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

  first_existing_column <- function(meta, candidates) {
    found <- candidates[candidates %in% colnames(meta)]
    found[[1]] %||% NA_character_
  }

  merge_selected_samples <- function(sample_names, sample_list, project_name) {
    sample_names <- intersect(as.character(sample_names), names(sample_list))
    if (length(sample_names) == 0) {
      stop("Each group must contain at least one sample.")
    }

    if (length(sample_names) == 1) {
      return(sample_list[[sample_names[[1]]]])
    }

    merge(
      sample_list[[sample_names[[1]]]],
      y = sample_list[sample_names[-1]],
      add.cell.ids = sample_names,
      project = project_name
    )
  }

  complete_counts_table <- function(before_df, after_df, id_col, before_name, after_name) {
    out <- dplyr::full_join(before_df, after_df, by = id_col)
    out[[before_name]][is.na(out[[before_name]])] <- 0
    out[[after_name]][is.na(out[[after_name]])] <- 0
    out
  }

  multiple_list <- index_multiple_qc_input1
  if (is.null(multiple_list) || length(multiple_list) == 0) {
    stop("No sample objects are available for QC filtering.")
  }

  group_count <- suppressWarnings(as.integer(index_multiple_group_count))
  if (is.na(group_count) || group_count < 1) {
    group_count <- 1L
  }

  group_names <- c(
    index_group1_name,
    index_group2_name,
    index_group3_name,
    index_group4_name,
    index_group5_name,
    index_group6_name
  )
  group_samples <- list(
    index_group1_samples,
    index_group2_samples,
    index_group3_samples,
    index_group4_samples,
    index_group5_samples,
    index_group6_samples
  )

  selected_groups <- Map(
    function(group_name, samples) {
      list(name = as.character(group_name %||% "Group"), samples = as.character(samples %||% character(0)))
    },
    group_names[seq_len(group_count)],
    group_samples[seq_len(group_count)]
  )
  selected_groups <- Filter(function(x) length(x$samples) > 0, selected_groups)

  if (length(selected_groups) == 0) {
    stop("Please select at least one sample for QC filtering.")
  }

  group_objects <- lapply(selected_groups, function(group_def) {
    obj <- merge_selected_samples(group_def$samples, multiple_list, project_name = group_def$name)
    obj$condition <- group_def$name
    obj
  })
  names(group_objects) <- vapply(selected_groups, `[[`, character(1), "name")

  group_level_object <- if (length(group_objects) == 1) {
    group_objects[[1]]
  } else {
    merge(
      group_objects[[1]],
      y = group_objects[-1],
      add.cell.ids = names(group_objects),
      project = paste(names(group_objects), collapse = "_vs_")
    )
  }

  feature_col <- first_existing_column(
    group_level_object@meta.data,
    c("nFeature_Spatial", "nFeature_RNA", "nFeature_bin")
  )
  count_col <- first_existing_column(
    group_level_object@meta.data,
    c("nCount_Spatial", "nCount_RNA", "nCount_bin")
  )

  if (is.na(feature_col) || is.na(count_col)) {
    stop("QC metadata columns were not found in the Seurat object.")
  }

  mt_pat <- guess_mt_pattern(rownames(group_level_object[["Spatial"]]))
  if (is.na(mt_pat)) {
    group_level_object$percent.mt <- 0
  } else {
    group_level_object$percent.mt <- PercentageFeatureSet(group_level_object, pattern = mt_pat, assay = "Spatial")
    group_level_object$percent.mt[is.na(group_level_object$percent.mt)] <- 0
  }

  min_feature <- suppressWarnings(as.numeric(index_multiple_sample_min_count %||% 0))
  max_feature <- suppressWarnings(as.numeric(index_multiple_sample_max_count %||% Inf))
  min_count <- suppressWarnings(as.numeric(index_multiple_sample_min_ncount %||% 0))
  max_count <- suppressWarnings(as.numeric(index_multiple_sample_max_ncount %||% Inf))
  max_mito <- suppressWarnings(as.numeric(index_multiple_sample_max_mito_perc %||% Inf))

  if (is.na(min_feature)) min_feature <- 0
  if (is.na(max_feature)) max_feature <- Inf
  if (is.na(min_count)) min_count <- 0
  if (is.na(max_count)) max_count <- Inf
  if (is.na(max_mito)) max_mito <- Inf

  keep_cells <- group_level_object@meta.data[[feature_col]] >= min_feature &
    group_level_object@meta.data[[feature_col]] <= max_feature &
    group_level_object@meta.data[[count_col]] >= min_count &
    group_level_object@meta.data[[count_col]] <= max_count &
    group_level_object@meta.data$percent.mt <= max_mito

  if (!any(keep_cells)) {
    stop("No spots or bins remain after QC filtering. Please relax the QC thresholds.")
  }

  groups_merged <- subset(group_level_object, cells = rownames(group_level_object@meta.data)[keep_cells])

  groups_table1 <- table(group_level_object$orig.ident) %>% as.data.frame()
  colnames(groups_table1) <- c("Samples", "Cell counts before QC")
  groups_table2 <- table(group_level_object$condition) %>% as.data.frame()
  colnames(groups_table2) <- c("Groups", "Cell counts before QC")
  groups_table3 <- table(groups_merged$orig.ident) %>% as.data.frame()
  colnames(groups_table3) <- c("Samples", "Cell counts after QC")
  groups_table4 <- table(groups_merged$condition) %>% as.data.frame()
  colnames(groups_table4) <- c("Groups", "Cell counts after QC")

  sample_count <- complete_counts_table(
    groups_table1,
    groups_table3,
    id_col = "Samples",
    before_name = "Cell counts before QC",
    after_name = "Cell counts after QC"
  )
  sample_count_bar <- reshape2::melt(sample_count, id.vars = "Samples")
  colnames(sample_count_bar) <- c("Samples", "variable", "Cell_counts")

  group_count_table <- complete_counts_table(
    groups_table2,
    groups_table4,
    id_col = "Groups",
    before_name = "Cell counts before QC",
    after_name = "Cell counts after QC"
  )
  group_count_bar <- reshape2::melt(group_count_table, id.vars = "Groups")
  colnames(group_count_bar) <- c("Groups", "variable", "Cell_counts")

  plots6 <- safe_plot(VlnPlot(groups_merged, features = feature_col, ncol = 1, raster = FALSE))
  plots7 <- safe_plot(VlnPlot(groups_merged, features = count_col, ncol = 1, raster = FALSE))
  plots8 <- safe_plot(VlnPlot(groups_merged, features = "percent.mt", ncol = 1, raster = FALSE))
  plots9 <- safe_plot(
    VlnPlot(groups_merged, features = c(feature_col, count_col, "percent.mt"), ncol = 1, group.by = "condition", raster = FALSE)
  )
  plots12 <- safe_plot(SpatialFeaturePlot(groups_merged, features = c(feature_col, count_col, "percent.mt")))

  plots10 <- ggplot(sample_count_bar, aes(x = Samples, y = Cell_counts, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(aes(label = Cell_counts), vjust = 1.6, position = position_dodge(0.9), color = "white", size = 3.5) +
    theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), strip.background = element_blank(), plot.margin = unit(c(1, 1, 1, 1), "line")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1))

  plots11 <- ggplot(group_count_bar, aes(x = Groups, y = Cell_counts, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(aes(label = Cell_counts), vjust = 1.6, position = position_dodge(0.9), color = "white", size = 3.5) +
    theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), strip.background = element_blank(), plot.margin = unit(c(1, 1, 1, 1), "line")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1))

  return(list(
    plot1 = combine_plots(plots6, plots7, plots8),
    plot2 = plots9,
    plot3 = plots10,
    plot4 = plots11,
    data1 = groups_table3,
    data2 = groups_table4,
    data3 = groups_merged,
    plot5 = plots12
  ))
}
