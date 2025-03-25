datainput_multiple_normalization_pca <- function(index_multiple_normalization_pca_input, index_multiple_sample_normalization_method, multiple_sample_normalization_method1, index_multiple_sample_scale_factor, index_multiple_sample_var_genes, index_multiple_sample_var_genes1, index_multiple_sample_normalization_variable_genes, index_multiple_sample_pca_dim){
 
  if (index_multiple_sample_normalization_method == "LogNormalize")
  { 
  multiple_list <- SplitObject(index_multiple_normalization_pca_input, split.by = "orig.ident")
  multiple_list <- lapply(multiple_list, function(x, normalization.method = index_multiple_sample_normalization_method, scale.factor = index_multiple_sample_scale_factor) {
  #DefaultAssay(x) <- "Spatial"
  x <- NormalizeData(x, normalization.method = index_multiple_sample_normalization_method, scale.factor = index_multiple_sample_scale_factor)  # Ensure each dataset is normalized
  x <- FindVariableFeatures(x, selection.method = index_multiple_sample_normalization_variable_genes, nfeatures = index_multiple_sample_var_genes)
  x <- ScaleData(x, verbose = FALSE)  # Scale before PCA
  x <- RunPCA(x, npcs = index_multiple_sample_pca_dim)  # Compute PCA
  return(x)
  })

  index_multiple_normalization_pca_input_anchors <- FindIntegrationAnchors(object.list = multiple_list, dims = 1:30,  normalization.method = "LogNormalize", reduction=multiple_sample_normalization_method1)
  index_multiple_normalization_pca_input_integrated <- IntegrateData(anchorset = index_multiple_normalization_pca_input_anchors)
  #DefaultAssay(index_multiple_normalization_pca_input_integrated) <- "integrated"
  #index_multiple_normalization_pca_input_integrated <- merge(x = multiple_list[[1]], y = multiple_list[-1], add.cell.ids = names(multiple_list), merge.data = TRUE)
  multiple_sample_normalized <- ScaleData(index_multiple_normalization_pca_input_integrated, verbose = FALSE)
  multiple_sample_normalized <- RunPCA(multiple_sample_normalized, npcs = index_multiple_sample_pca_dim)
  plots12 <- DimHeatmap(multiple_sample_normalized, dims = 1, cells = 500, balanced = TRUE, fast = FALSE)
  plots13 <- ElbowPlot(multiple_sample_normalized)
  plots14 <- DimPlot(multiple_sample_normalized, reduction = "pca")
  plots15 <- DimPlot(multiple_sample_normalized, reduction = "pca", group.by = "condition")
  #multiple_sample_normalized <- JoinLayers(multiple_sample_normalized) 
  }
  

  else if (index_multiple_sample_normalization_method == "SCTransform")
  {
  experiment.list <- SplitObject(index_multiple_normalization_pca_input, split.by = "orig.ident")
  for (i in 1:length(experiment.list)) {
  DefaultAssay(experiment.list[[i]]) <- "Spatial" 
  experiment.list[[i]] <- SCTransform(experiment.list[[i]], assay = "Spatial", verbose = FALSE)
  }

  experiment.features <- SelectIntegrationFeatures(object.list = experiment.list, nfeatures = index_multiple_sample_var_genes1)
  experiment <- PrepSCTIntegration(object.list = experiment.list, anchor.features = experiment.features,  verbose = TRUE)
  experiment.anchors <- FindIntegrationAnchors(object.list = experiment, normalization.method = "SCT", anchor.features = experiment.features, verbose = TRUE, dims = 1:30)
  index_multiple_normalization_pca_input_integrated <- IntegrateData(anchorset = experiment.anchors, normalization.method = "SCT", verbose = TRUE, dims = 1:30)
  #DefaultAssay(index_multiple_normalization_pca_input_integrated) <- "integrated"
  multiple_sample_normalized <- RunPCA(index_multiple_normalization_pca_input_integrated, npcs = index_multiple_sample_pca_dim)
  plots12 <- DimHeatmap(multiple_sample_normalized, dims = 1, cells = 500, balanced = TRUE, fast = FALSE)
  plots13 <- ElbowPlot(multiple_sample_normalized)
  plots14 <- DimPlot(multiple_sample_normalized, reduction = "pca")
  plots15 <- DimPlot(multiple_sample_normalized, reduction = "pca", group.by = "condition")
  }
  
  return(list(plot1 = plots12, plot2 = plots13, plot3 = plots14, plot4 = plots15, data1 = multiple_sample_normalized))
}