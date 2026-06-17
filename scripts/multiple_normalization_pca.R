# scripts/multiple_normalization_pca.R
# Requires: library(Seurat)
source_app_script("scripts/assay_utils.R")

datainput_multiple_normalization_pca <- function(
    index_multiple_normalization_pca_input,
    index_multiple_sample_normalization_method,         # "LogNormalize" | "SCTransform"
    multiple_sample_normalization_method1,              # "cca" | "rpca" | "harmony" | "none"
    index_multiple_sample_scale_factor,                 # numeric (LogNormalize)
    index_multiple_sample_var_genes,                    # integer (LogNormalize)
    index_multiple_sample_var_genes1,                   # integer (SCT integration features)
    index_multiple_sample_normalization_variable_genes, # "vst" | "mean.var.plot" | "dispersion"
    index_multiple_sample_pca_dim,                      # integer PCA dims
    index_multiple_sample_assay = "auto"                # "auto" | "RNA" | "Spatial" | "SCT"
){
  # ---------- helpers ----------
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  coerce_scalar_chr <- function(x, default) {
    if (missing(x) || is.null(x) || length(x) == 0) return(default)
    as.character(x[[1]])
  }
  uses_merge_only_integration <- function(reduction_name) {
    tolower(as.character(reduction_name %||% "cca")) %in% c("none", "merge", "merge_only")
  }
  uses_harmony_integration <- function(reduction_name) {
    tolower(as.character(reduction_name %||% "cca")) %in% c("harmony", "harmonyintegration")
  }
  get_anchor_reduction <- function(reduction_name, default = "cca") {
    reduction_name <- tolower(as.character(reduction_name %||% default))
    if (reduction_name %in% c("none", "merge", "merge_only", "harmony", "harmonyintegration")) {
      default
    } else {
      reduction_name
    }
  }
  get_reductions <- function(object) {
    tryCatch(Seurat::Reductions(object), error = function(e) character(0))
  }
  choose_plot_reduction <- function(object) {
    if ("harmony" %in% get_reductions(object)) "harmony" else "pca"
  }
  run_harmony_reduction <- function(object, dims, group.by.vars = "orig.ident") {
    if (!requireNamespace("harmony", quietly = TRUE)) {
      stop("HarmonyIntegration requires the 'harmony' package. Please install it with install.packages('harmony').")
    }
    if (!group.by.vars %in% colnames(object@meta.data)) {
      stop(sprintf("HarmonyIntegration requires '%s' in object metadata.", group.by.vars))
    }
    if (!"pca" %in% get_reductions(object)) {
      object <- Seurat::RunPCA(object, npcs = max(dims), verbose = FALSE)
    }

    harmony_result <- tryCatch(
      harmony::RunHarmony(
        object = object,
        group.by.vars = group.by.vars,
        reduction.use = "pca",
        dims.use = dims,
        reduction.save = "harmony",
        project.dim = FALSE,
        verbose = FALSE
      ),
      error = function(e) e
    )
    if (inherits(harmony_result, "error")) {
      harmony_result <- tryCatch(
        harmony::RunHarmony(
          object = object,
          group.by.vars = group.by.vars,
          reduction = "pca",
          dims.use = dims,
          reduction.save = "harmony",
          project.dim = FALSE,
          verbose = FALSE
        ),
        error = function(e) e
      )
    }
    if (inherits(harmony_result, "error")) {
      harmony_result <- harmony::RunHarmony(
        object = object,
        group.by.vars = group.by.vars,
        dims.use = dims,
        reduction.save = "harmony",
        verbose = FALSE
      )
    }
    harmony_result@misc$vstdavis_integration_reduction <- "harmony"
    harmony_result
  }
  merge_processed_samples <- function(object_list, assay_name = "RNA", project_name = "merged_normalized") {
    if (length(object_list) == 0) {
      stop("No objects available to merge.")
    }
    if (length(object_list) == 1) {
      merged_obj <- object_list[[1]]
    } else {
      merged_obj <- merge(
        x = object_list[[1]],
        y = object_list[-1],
        merge.data = TRUE,
        project = project_name
      )
    }

    Seurat::DefaultAssay(merged_obj) <- assay_name
    merged_obj
  }
  normalize_assay_choice <- function(x, default = "auto") {
    x <- tolower(coerce_scalar_chr(x, default))
    if (x %in% c("auto", "default")) return("auto")
    if (x == "rna") return("RNA")
    if (x == "spatial") return("Spatial")
    if (x == "sct") return("SCT")
    x
  }
  # Defensive assay picker (prevents %in% -> match() on NULL)
  pick_assay <- function(s, requested_assay = "auto") {
    ass <- tryCatch(Seurat::Assays(s), error = function(e) character(0))
    if (length(ass) == 0) return(NA_character_)  # signal "no assays"
    if (!identical(requested_assay, "auto")) {
      if (requested_assay %in% ass) return(requested_assay)
      stop(sprintf(
        "Selected assay '%s' is not available. Available assays: %s",
        requested_assay,
        paste(ass, collapse = ", ")
      ))
    }
    if ("Spatial" %in% ass) return("Spatial")
    if ("RNA" %in% ass)     return("RNA")
    if ("SCT" %in% ass)     return("SCT")
    Seurat::DefaultAssay(s)
  }
  
  # ---------- basic checks ----------
  if (is.null(index_multiple_normalization_pca_input) || !inherits(index_multiple_normalization_pca_input, "Seurat")) {
    stop("Input must be a non-NULL Seurat object.")
  }
  if (!"orig.ident" %in% colnames(index_multiple_normalization_pca_input@meta.data)) {
    stop("meta.data must contain 'orig.ident' to split samples.")
  }
  
  # ---------- sanitize UI inputs (prevents 'match' requires vector arguments) ----------
  index_multiple_sample_normalization_method         <- coerce_scalar_chr(index_multiple_sample_normalization_method, "SCTransform")
  multiple_sample_normalization_method1              <- coerce_scalar_chr(multiple_sample_normalization_method1, "cca")
  index_multiple_sample_normalization_variable_genes <- coerce_scalar_chr(index_multiple_sample_normalization_variable_genes, "vst")
  index_multiple_sample_assay                        <- normalize_assay_choice(index_multiple_sample_assay, "auto")
  
  index_multiple_sample_scale_factor <- as.numeric(index_multiple_sample_scale_factor %||% 1e4)
  index_multiple_sample_var_genes    <- as.integer(index_multiple_sample_var_genes %||% 2000)
  index_multiple_sample_var_genes1   <- as.integer(index_multiple_sample_var_genes1 %||% 3000)
  index_multiple_sample_pca_dim      <- as.integer(index_multiple_sample_pca_dim %||% 30)
  
  # Manual validation (avoids match.arg() crashing on NULL/factors)
  if (!index_multiple_sample_normalization_method %in% c("LogNormalize","SCTransform")) {
    stop("index_multiple_sample_normalization_method must be 'LogNormalize' or 'SCTransform'")
  }
  if (!multiple_sample_normalization_method1 %in% c("cca","rpca","harmony","harmonyintegration","none","merge","merge_only")) {
    stop("multiple_sample_normalization_method1 must be 'cca', 'rpca', 'harmony', or 'none'")
  }
  if (!index_multiple_sample_normalization_variable_genes %in% c("vst","mean.var.plot","dispersion")) {
    stop("index_multiple_sample_normalization_variable_genes must be one of 'vst','mean.var.plot','dispersion'")
  }
  if (!index_multiple_sample_assay %in% c("auto","RNA","Spatial","SCT")) {
    stop("index_multiple_sample_assay must be one of 'auto', 'RNA', 'Spatial', or 'SCT'")
  }
  if (identical(index_multiple_sample_normalization_method, "LogNormalize") &&
      identical(index_multiple_sample_assay, "SCT")) {
    stop("SCT assay can be selected only with SCTransform. Choose RNA or Spatial for LogNormalize.")
  }
  
  npcs <- max(2, min(index_multiple_sample_pca_dim, 100))
  integ_dims <- 1:min(30, npcs)
  source_assay_used <- NULL
  
  # ---------- split & drop empties; ensure names ----------
  obj_list <- Seurat::SplitObject(index_multiple_normalization_pca_input, split.by = "orig.ident")
  obj_list <- obj_list[vapply(obj_list, function(x) !is.null(x) && ncol(x) > 0, logical(1))]
  if (length(obj_list) == 0) stop("No cells after splitting by 'orig.ident'.")
  if (is.null(names(obj_list)) || any(is.na(names(obj_list))) || any(names(obj_list) == "")) {
    names(obj_list) <- paste0("sample_", seq_along(obj_list))
  }
  nsamp <- length(obj_list)
  
  # ---------- core logic ----------
  sanitize_split <- function(x, nm) {
    a <- pick_assay(x, requested_assay = index_multiple_sample_assay)
    if (is.na(a)) {
      stop(sprintf("Split '%s' has no usable assays.", nm))
    }
    if (is.null(source_assay_used)) {
      source_assay_used <<- a
    }
    Seurat::DefaultAssay(x) <- a
    x <- sanitize_seurat_for_normalization(x, assay = a, sample_name = nm)
    list(object = x, assay = a)
  }

  run_single_lognorm <- function(x, nm) {
    prep <- sanitize_split(x, nm)
    x <- prep$object
    a <- prep$assay
    Seurat::DefaultAssay(x) <- a
    x <- Seurat::NormalizeData(x, normalization.method = "LogNormalize",
                               scale.factor = index_multiple_sample_scale_factor, verbose = FALSE)
    x <- Seurat::FindVariableFeatures(x, selection.method = index_multiple_sample_normalization_variable_genes,
                                      nfeatures = index_multiple_sample_var_genes, verbose = FALSE)
    x <- Seurat::ScaleData(x, verbose = FALSE)
    Seurat::RunPCA(x, npcs = npcs, verbose = FALSE)
  }
  
  run_single_sct <- function(x, nm) {
    prep <- sanitize_split(x, nm)
    x <- prep$object
    a <- prep$assay
    Seurat::DefaultAssay(x) <- a
    if (!identical(a, "SCT")) {
      x <- Seurat::SCTransform(x, assay = a, verbose = FALSE)
    }
    Seurat::DefaultAssay(x) <- "SCT"
    Seurat::RunPCA(x, npcs = npcs, verbose = FALSE)
  }
  
  if (index_multiple_sample_normalization_method == "LogNormalize") {
    if (nsamp == 1) {
      nm <- names(obj_list)[1]
      obj <- run_single_lognorm(obj_list[[1]], nm)
    } else {
      # per-sample
      obj_list <- lapply(seq_along(obj_list), function(i) {
        run_single_lognorm(obj_list[[i]], names(obj_list)[i])
      })
      lognorm_assay <- Seurat::DefaultAssay(obj_list[[1]])
      # integration
      feats   <- Seurat::SelectIntegrationFeatures(object.list = obj_list, nfeatures = index_multiple_sample_var_genes)
      if (uses_merge_only_integration(multiple_sample_normalization_method1)) {
        message("Merge-only integration requested; skipping anchor-based integration and merging normalized samples.")
        obj <- merge_processed_samples(
          obj_list,
          assay_name = lognorm_assay,
          project_name = "merged_lognormalized"
        )
        feats <- intersect(feats, rownames(obj))
        if (length(feats) < 2) stop("Too few shared variable features are available after merging samples.")
        Seurat::VariableFeatures(obj) <- feats
        Seurat::DefaultAssay(obj) <- lognorm_assay
        obj <- Seurat::ScaleData(obj, features = feats, verbose = FALSE)
        obj <- Seurat::RunPCA(obj, features = feats, npcs = npcs, verbose = FALSE)
      } else if (uses_harmony_integration(multiple_sample_normalization_method1)) {
        message("HarmonyIntegration requested; merging normalized samples, running PCA, and correcting on orig.ident.")
        obj <- merge_processed_samples(
          obj_list,
          assay_name = lognorm_assay,
          project_name = "harmony_lognormalized"
        )
        feats <- intersect(feats, rownames(obj))
        if (length(feats) < 2) stop("Too few shared variable features are available for HarmonyIntegration.")
        Seurat::VariableFeatures(obj) <- feats
        Seurat::DefaultAssay(obj) <- lognorm_assay
        obj <- Seurat::ScaleData(obj, features = feats, verbose = FALSE)
        obj <- Seurat::RunPCA(obj, features = feats, npcs = npcs, verbose = FALSE)
        obj <- run_harmony_reduction(obj, dims = seq_len(npcs), group.by.vars = "orig.ident")
      } else {
        anchors <- Seurat::FindIntegrationAnchors(object.list = obj_list, anchor.features = feats,
                                                  normalization.method = "LogNormalize",
                                                  reduction = get_anchor_reduction(multiple_sample_normalization_method1, default = "cca"),
                                                  dims = integ_dims, verbose = FALSE)
        obj <- Seurat::IntegrateData(anchorset = anchors, normalization.method = "LogNormalize",
                                     dims = integ_dims, verbose = FALSE)
        Seurat::DefaultAssay(obj) <- "integrated"
        obj <- Seurat::ScaleData(obj, verbose = FALSE)
        obj <- Seurat::RunPCA(obj, npcs = npcs, verbose = FALSE)
      }
    }
  } else { # SCTransform
    if (nsamp == 1) {
      nm <- names(obj_list)[1]
      obj <- run_single_sct(obj_list[[1]], nm)
    } else {
      # per-sample SCT
      obj_list <- lapply(seq_along(obj_list), function(i) {
        x  <- obj_list[[i]]
        nm <- names(obj_list)[i]
        prep <- sanitize_split(x, nm)
        x <- prep$object
        a <- prep$assay
        Seurat::DefaultAssay(x) <- a
        if (!identical(a, "SCT")) {
          x <- Seurat::SCTransform(x, assay = a, verbose = FALSE)
        } else {
          Seurat::DefaultAssay(x) <- "SCT"
          x
        }
      })
      # integration
      experiment.features <- Seurat::SelectIntegrationFeatures(object.list = obj_list, nfeatures = index_multiple_sample_var_genes1)
      if (uses_merge_only_integration(multiple_sample_normalization_method1)) {
        message("Merge-only integration requested; skipping SCT anchor integration and merging SCT-normalized samples.")
        obj <- merge_processed_samples(
          obj_list,
          assay_name = "SCT",
          project_name = "merged_sct"
        )
        experiment.features <- intersect(experiment.features, rownames(obj))
        if (length(experiment.features) < 2) stop("Too few shared variable features are available after merging SCT samples.")
        Seurat::VariableFeatures(obj) <- experiment.features
        Seurat::DefaultAssay(obj) <- "SCT"
        obj <- Seurat::RunPCA(obj, features = experiment.features, npcs = npcs, verbose = FALSE)
      } else if (uses_harmony_integration(multiple_sample_normalization_method1)) {
        message("HarmonyIntegration requested; merging SCT-normalized samples, running PCA, and correcting on orig.ident.")
        obj <- merge_processed_samples(
          obj_list,
          assay_name = "SCT",
          project_name = "harmony_sct"
        )
        experiment.features <- intersect(experiment.features, rownames(obj))
        if (length(experiment.features) < 2) stop("Too few shared variable features are available for SCT HarmonyIntegration.")
        Seurat::VariableFeatures(obj) <- experiment.features
        Seurat::DefaultAssay(obj) <- "SCT"
        obj <- Seurat::RunPCA(obj, features = experiment.features, npcs = npcs, verbose = FALSE)
        obj <- run_harmony_reduction(obj, dims = seq_len(npcs), group.by.vars = "orig.ident")
      } else {
        experiment <- Seurat::PrepSCTIntegration(object.list = obj_list, anchor.features = experiment.features, verbose = TRUE)
        experiment.anchors <- Seurat::FindIntegrationAnchors(object.list = experiment, normalization.method = "SCT",
                                                             anchor.features = experiment.features,
                                                             reduction = get_anchor_reduction(multiple_sample_normalization_method1, default = "cca"),
                                                             verbose = TRUE, dims = integ_dims)
        obj <- Seurat::IntegrateData(anchorset = experiment.anchors, normalization.method = "SCT",
                                     verbose = TRUE, dims = integ_dims)
        Seurat::DefaultAssay(obj) <- "integrated"
        obj <- Seurat::RunPCA(obj, npcs = npcs, verbose = FALSE)
      }
    }
  }
  
  # ---------- plots ----------
  obj@misc$vstdavis_input_assay <- index_multiple_sample_assay
  obj@misc$vstdavis_source_assay_used <- source_assay_used %||% index_multiple_sample_assay
  obj@misc$vstdavis_default_assay_after_normalization <- Seurat::DefaultAssay(obj)
  plot_reduction <- choose_plot_reduction(obj)
  p1 <- Seurat::DimHeatmap(obj, dims = 1, cells = 500, balanced = TRUE, fast = FALSE)
  p2 <- Seurat::ElbowPlot(obj, ndims = npcs)
  p3 <- Seurat::DimPlot(obj, reduction = plot_reduction)
  p4 <- if ("condition" %in% colnames(obj@meta.data)) {
    Seurat::DimPlot(obj, reduction = plot_reduction, group.by = "condition")
  } else {
    Seurat::DimPlot(obj, reduction = plot_reduction)
  }
  
  list(plot1 = p1, plot2 = p2, plot3 = p3, plot4 = p4, data1 = obj)
}
