# Requires: library(Seurat)

datainput_subclustering_multiple_normalization_pca <- function(
    index_subclustering_multiple_normalization_pca_input,
    index_subclustering_multiple_sample_normalization_method,         # "LogNormalize" | "SCTransform"
    index_subclustering_multiple_sample_scale_factor,                 # numeric (LogNormalize)
    index_subclustering_multiple_sample_var_genes,                    # integer (LogNormalize)
    index_subclustering_multiple_sample_var_genes1,                   # integer (SCT integration features)
    index_subclustering_multiple_sample_normalization_variable_genes, # "vst" | "mean.var.plot" | "dispersion"
    index_subclustering_multiple_sample_pca_dim                       # integer PCA dims
){
  # ---------- helpers ----------
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  coerce_scalar_chr <- function(x, default) {
    if (missing(x) || is.null(x) || length(x) == 0) return(default)
    as.character(x[[1]])
  }
  # Defensive assay picker (avoids %in% calling match() on NULL)
  pick_assay <- function(s) {
    ass <- tryCatch(Seurat::Assays(s), error = function(e) character(0))
    if (length(ass) == 0) return(NA_character_)
    if ("Spatial" %in% ass) return("Spatial")
    if ("RNA" %in% ass)     return("RNA")
    if ("SCT" %in% ass)     return("SCT")
    Seurat::DefaultAssay(s)
  }
  
  # ---------- basic checks ----------
  if (is.null(index_subclustering_multiple_normalization_pca_input) ||
      !inherits(index_subclustering_multiple_normalization_pca_input, "Seurat")) {
    stop("Input must be a non-NULL Seurat object.")
  }
  if (!"orig.ident" %in% colnames(index_subclustering_multiple_normalization_pca_input@meta.data)) {
    stop("meta.data must contain 'orig.ident' to split samples.")
  }
  
  # ---------- sanitize UI inputs (prevents 'match' errors) ----------
  index_subclustering_multiple_sample_normalization_method         <-
    coerce_scalar_chr(index_subclustering_multiple_sample_normalization_method, "SCTransform")
  index_subclustering_multiple_sample_normalization_variable_genes <-
    coerce_scalar_chr(index_subclustering_multiple_sample_normalization_variable_genes, "vst")
  
  index_subclustering_multiple_sample_scale_factor <- as.numeric(index_subclustering_multiple_sample_scale_factor %||% 1e4)
  index_subclustering_multiple_sample_var_genes    <- as.integer(index_subclustering_multiple_sample_var_genes %||% 2000)
  index_subclustering_multiple_sample_var_genes1   <- as.integer(index_subclustering_multiple_sample_var_genes1 %||% 3000)
  index_subclustering_multiple_sample_pca_dim      <- as.integer(index_subclustering_multiple_sample_pca_dim %||% 30)
  
  # Manual validation (avoids match.arg() crashes)
  if (!index_subclustering_multiple_sample_normalization_method %in% c("LogNormalize","SCTransform")) {
    stop("index_subclustering_multiple_sample_normalization_method must be 'LogNormalize' or 'SCTransform'")
  }
  if (!index_subclustering_multiple_sample_normalization_variable_genes %in% c("vst","mean.var.plot","dispersion")) {
    stop("index_subclustering_multiple_sample_normalization_variable_genes must be one of 'vst','mean.var.plot','dispersion'")
  }
  
  npcs <- max(2, min(index_subclustering_multiple_sample_pca_dim, 100))
  integ_dims <- 1:min(30, npcs)
  
  # ---------- split & drop empties; ensure names ----------
  obj_list <- Seurat::SplitObject(index_subclustering_multiple_normalization_pca_input, split.by = "orig.ident")
  obj_list <- obj_list[vapply(obj_list, function(x) !is.null(x) && ncol(x) > 0, logical(1))]
  if (length(obj_list) == 0) stop("No cells after splitting by 'orig.ident'.")
  if (is.null(names(obj_list)) || any(is.na(names(obj_list))) || any(names(obj_list) == "")) {
    names(obj_list) <- paste0("sample_", seq_along(obj_list))
  }
  nsamp <- length(obj_list)
  
  # ---------- per-sample runners ----------
  run_single_lognorm <- function(x, nm) {
    a <- pick_assay(x)
    if (is.na(a)) stop(sprintf("Split '%s' has no usable assays.", nm))
    Seurat::DefaultAssay(x) <- a
    x <- Seurat::NormalizeData(x, normalization.method = "LogNormalize",
                               scale.factor = index_subclustering_multiple_sample_scale_factor, verbose = FALSE)
    x <- Seurat::FindVariableFeatures(x, selection.method = index_subclustering_multiple_sample_normalization_variable_genes,
                                      nfeatures = index_subclustering_multiple_sample_var_genes, verbose = FALSE)
    x <- Seurat::ScaleData(x, verbose = FALSE)
    Seurat::RunPCA(x, npcs = npcs, verbose = FALSE)
  }
  
  run_single_sct <- function(x, nm) {
    a <- pick_assay(x)
    if (is.na(a)) stop(sprintf("Split '%s' has no usable assays.", nm))
    Seurat::DefaultAssay(x) <- a
    x <- Seurat::SCTransform(x, assay = a, verbose = FALSE)
    Seurat::DefaultAssay(x) <- "SCT"
    Seurat::RunPCA(x, npcs = npcs, verbose = FALSE)
  }
  
  # ---------- main logic ----------
  if (index_subclustering_multiple_sample_normalization_method == "LogNormalize") {
    if (nsamp == 1) {
      nm <- names(obj_list)[1]
      obj <- run_single_lognorm(obj_list[[1]], nm)
    } else {
      # per-split processing
      obj_list <- lapply(seq_along(obj_list), function(i) run_single_lognorm(obj_list[[i]], names(obj_list)[i]))
      # integrate
      feats   <- Seurat::SelectIntegrationFeatures(object.list = obj_list, nfeatures = index_subclustering_multiple_sample_var_genes)
      anchors <- Seurat::FindIntegrationAnchors(object.list = obj_list, anchor.features = feats,
                                                normalization.method = "LogNormalize",
                                                dims = integ_dims, verbose = FALSE)
      obj <- Seurat::IntegrateData(anchorset = anchors, normalization.method = "LogNormalize",
                                   dims = integ_dims, verbose = FALSE)
      Seurat::DefaultAssay(obj) <- "integrated"
      obj <- Seurat::ScaleData(obj, verbose = FALSE)
      obj <- Seurat::RunPCA(obj, npcs = npcs, verbose = FALSE)
    }
  } else { # SCTransform
    if (nsamp == 1) {
      nm <- names(obj_list)[1]
      obj <- run_single_sct(obj_list[[1]], nm)
    } else {
      obj_list <- lapply(seq_along(obj_list), function(i) {
        x  <- obj_list[[i]]
        nm <- names(obj_list)[i]
        a <- pick_assay(x)
        if (is.na(a)) stop(sprintf("Split '%s' has no usable assays.", nm))
        Seurat::DefaultAssay(x) <- a
        Seurat::SCTransform(x, assay = a, verbose = FALSE)
      })
      experiment.features <- Seurat::SelectIntegrationFeatures(object.list = obj_list, nfeatures = index_subclustering_multiple_sample_var_genes1)
      experiment <- Seurat::PrepSCTIntegration(object.list = obj_list, anchor.features = experiment.features, verbose = TRUE)
      experiment.anchors <- Seurat::FindIntegrationAnchors(object.list = experiment, normalization.method = "SCT",
                                                           anchor.features = experiment.features, verbose = TRUE, dims = integ_dims)
      obj <- Seurat::IntegrateData(anchorset = experiment.anchors, normalization.method = "SCT",
                                   verbose = TRUE, dims = integ_dims)
      Seurat::DefaultAssay(obj) <- "integrated"
      obj <- Seurat::RunPCA(obj, npcs = npcs, verbose = FALSE)
    }
  }
  
  # ---------- plots ----------
  p1 <- Seurat::DimHeatmap(obj, dims = 1, cells = 500, balanced = TRUE, fast = FALSE)
  p2 <- Seurat::ElbowPlot(obj, ndims = npcs)
  p3 <- Seurat::DimPlot(obj, reduction = "pca")
  p4 <- if ("condition" %in% colnames(obj@meta.data)) {
    Seurat::DimPlot(obj, reduction = "pca", group.by = "condition")
  } else {
    Seurat::DimPlot(obj, reduction = "pca")
  }
  
  list(plot1 = p1, plot2 = p2, plot3 = p3, plot4 = p4, data1 = obj)
}
