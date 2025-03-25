datainput_subclustering_multiple_normalization_pca <- function(index_subclustering_multiple_normalization_pca_input, index_subclustering_multiple_sample_normalization_method, index_subclustering_multiple_sample_scale_factor, index_subclustering_multiple_sample_var_genes, index_subclustering_multiple_sample_var_genes1, index_subclustering_multiple_sample_normalization_variable_genes, index_subclustering_multiple_sample_pca_dim){
 
  if (index_subclustering_multiple_sample_normalization_method == "LogNormalize")
  { 
  subclustering_multiple_sample_normalized <- NormalizeData(index_subclustering_multiple_normalization_pca_input, normalization.method = index_subclustering_multiple_sample_normalization_method, scale.factor = index_subclustering_multiple_sample_scale_factor, assay="RNA")
  subclustering_multiple_sample_normalized <- FindVariableFeatures(subclustering_multiple_sample_normalized, selection.method = index_subclustering_multiple_sample_normalization_variable_genes, nfeatures = index_subclustering_multiple_sample_var_genes)
  subclustering_multiple_sample_normalized <- ScaleData(subclustering_multiple_sample_normalized, verbose = FALSE)
  subclustering_multiple_sample_normalized <- RunPCA(subclustering_multiple_sample_normalized, features = VariableFeatures(object = subclustering_multiple_sample_normalized), npcs = index_subclustering_multiple_sample_pca_dim)
  plots12 <- DimHeatmap(subclustering_multiple_sample_normalized, dims = 1, cells = 500, balanced = TRUE, fast = FALSE)
  plots13 <- ElbowPlot(subclustering_multiple_sample_normalized)
  plots14 <- DimPlot(subclustering_multiple_sample_normalized, reduction = "pca")
  plots15 <- DimPlot(subclustering_multiple_sample_normalized, reduction = "pca", group.by = "condition")
  }
  
  else if (index_subclustering_multiple_sample_normalization_method == "SCTransform")
  {
  experiment.list <- SplitObject(index_subclustering_multiple_normalization_pca_input, split.by = "orig.ident")
  for (i in 1:length(experiment.list)) {
  DefaultAssay(experiment.list[[i]]) <- "Spatial" 
  experiment.list[[i]] <- SCTransform(experiment.list[[i]], assay = "Spatial", verbose = FALSE)
  }
	#index_multiple_sample_var_genes1 <- 1000

  experiment.features <- SelectIntegrationFeatures(object.list = experiment.list, nfeatures = index_subclustering_multiple_sample_var_genes1)
  experiment <- PrepSCTIntegration(object.list = experiment.list, anchor.features = experiment.features,  verbose = TRUE)
  experiment.anchors <- FindIntegrationAnchors(object.list = experiment, normalization.method = "SCT", anchor.features = experiment.features, verbose = TRUE, dims = 1:30)
  subclustering_multiple_sample_normalized <- IntegrateData(anchorset = experiment.anchors, normalization.method = "SCT", verbose = TRUE, dims = 1:30)
  #subclustering_multiple_sample_normalized <- SCTransform(index_subclustering_multiple_normalization_pca_input, vst.flavor="v2", assay="SCT")
  #plots12 <- VariableFeaturePlot(subclustering_multiple_sample_normalized)
  #subclustering_multiple_sample_normalized <- ScaleData(subclustering_multiple_sample_normalized, verbose = FALSE)
  subclustering_multiple_sample_normalized <- RunPCA(subclustering_multiple_sample_normalized, features = VariableFeatures(object = subclustering_multiple_sample_normalized), npcs = index_subclustering_multiple_sample_pca_dim)
  plots12 <- DimHeatmap(subclustering_multiple_sample_normalized, dims = 1, cells = 500, balanced = TRUE, fast = FALSE)
  plots13 <- ElbowPlot(subclustering_multiple_sample_normalized)
  plots14 <- DimPlot(subclustering_multiple_sample_normalized, reduction = "pca")
  plots15 <- DimPlot(subclustering_multiple_sample_normalized, reduction = "pca", group.by = "condition")
  }
return(list(plot1 = plots12, plot2 = plots13, plot3 = plots14, plot4 = plots15, data1 = subclustering_multiple_sample_normalized))
}