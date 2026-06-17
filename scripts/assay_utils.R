get_assay_layer_matrix <- function(object,
                                   assay = "RNA",
                                   layer = "counts",
                                   slot_fallback = "counts",
                                   as_sparse = TRUE) {
  data <- tryCatch(
    SeuratObject::LayerData(object = object, assay = assay, layer = layer),
    error = function(e) NULL
  )

  if (is.null(data)) {
    data <- tryCatch(
      Seurat::GetAssayData(object, assay = assay, layer = layer),
      error = function(e) NULL
    )
  }

  if (is.null(data)) {
    data <- tryCatch(
      Seurat::GetAssayData(object, assay = assay, slot = slot_fallback),
      error = function(e) NULL
    )
  }

  if (is.null(data)) {
    assay_obj <- tryCatch(object[[assay]], error = function(e) NULL)

    if (!is.null(assay_obj)) {
      layer_names <- tryCatch(SeuratObject::Layers(assay_obj), error = function(e) character(0))
      if (layer %in% layer_names) {
        data <- tryCatch(SeuratObject::LayerData(assay_obj, layer = layer), error = function(e) NULL)
      }

      if (is.null(data) && slot_fallback %in% slotNames(assay_obj)) {
        data <- tryCatch(slot(assay_obj, slot_fallback), error = function(e) NULL)
      }
    }
  }

  if (is.null(data)) {
    stop("Could not retrieve assay data for assay '", assay, "' and layer '", layer, "'.")
  }

  if (as_sparse && !inherits(data, "dgCMatrix")) {
    data <- as(data, "dgCMatrix")
  }

  data
}

sanitize_seurat_for_normalization <- function(object,
                                              assay = NULL,
                                              sample_name = NULL,
                                              min_cells = 2L,
                                              min_features = 2L) {
  assay_name <- assay
  if (is.null(assay_name) || !nzchar(assay_name)) {
    assay_name <- tryCatch(Seurat::DefaultAssay(object), error = function(e) NULL)
  }
  if (is.null(assay_name) || !nzchar(assay_name)) {
    stop("Could not determine the assay to normalize.")
  }

  sample_label <- sample_name
  if (is.null(sample_label) || !nzchar(sample_label)) {
    sample_label <- "sample"
  }

  Seurat::DefaultAssay(object) <- assay_name
  counts <- get_assay_layer_matrix(
    object = object,
    assay = assay_name,
    layer = "counts",
    slot_fallback = "counts",
    as_sparse = TRUE
  )

  if (inherits(counts, "dgCMatrix") && any(!is.finite(counts@x))) {
    stop(
      sprintf(
        "Sample '%s' contains non-finite count values in assay '%s'. Please re-check the uploaded matrix.",
        sample_label,
        assay_name
      )
    )
  }

  cell_totals <- Matrix::colSums(counts)
  keep_cells <- is.finite(cell_totals) & (cell_totals > 0)
  removed_cells <- sum(!keep_cells)

  if (!all(keep_cells)) {
    object <- object[, colnames(counts)[keep_cells], drop = FALSE]
  }

  counts <- get_assay_layer_matrix(
    object = object,
    assay = assay_name,
    layer = "counts",
    slot_fallback = "counts",
    as_sparse = TRUE
  )
  feature_totals <- Matrix::rowSums(counts)
  keep_features <- is.finite(feature_totals) & (feature_totals > 0)
  removed_features <- sum(!keep_features)

  if (!all(keep_features)) {
    object <- object[rownames(counts)[keep_features], , drop = FALSE]
  }

  counts <- get_assay_layer_matrix(
    object = object,
    assay = assay_name,
    layer = "counts",
    slot_fallback = "counts",
    as_sparse = TRUE
  )

  if (ncol(counts) < as.integer(min_cells)) {
    stop(
      sprintf(
        "Sample '%s' has %d valid bins/spots after removing zero-count bins. Please relax QC thresholds or re-check the uploaded data.",
        sample_label,
        ncol(counts)
      )
    )
  }

  if (nrow(counts) < as.integer(min_features)) {
    stop(
      sprintf(
        "Sample '%s' has %d valid genes after removing zero-count genes. Please relax QC thresholds or re-check the uploaded data.",
        sample_label,
        nrow(counts)
      )
    )
  }

  remaining_totals <- Matrix::colSums(counts)
  if (any(!is.finite(remaining_totals)) || any(remaining_totals <= 0)) {
    stop(
      sprintf(
        "Sample '%s' still has non-finite or zero total counts after cleanup, so normalization cannot continue.",
        sample_label
      )
    )
  }

  if (removed_cells > 0 || removed_features > 0) {
    message(
      sprintf(
        "Normalization cleanup for '%s': removed %d zero-count bins/spots and %d zero-count genes.",
        sample_label,
        removed_cells,
        removed_features
      )
    )
  }

  object
}
