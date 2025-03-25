datainput_multiple_clustering <- function(index_multiple_clustering_input, index_multiple_sample_normalization_method, index_m_clustering1, index_m_clustering2, index_m_clustering3, index_m_clustering4, index_m_clustering5, index_m_clustering6, index_m_clustering7, index_m_clustering8, index_m_clustering9, index_m_clustering10, index_m_clustering11, index_m_clustering12){
  index_m_clustering5 <- as.numeric(index_m_clustering5)
    
    multiple_sample_clustering<- FindNeighbors(index_multiple_clustering_input, dims = 1:index_m_clustering1 , k.param = index_m_clustering2, n.trees = index_m_clustering3)
    multiple_sample_clustering<- FindClusters(multiple_sample_clustering, resolution = index_m_clustering4, algorithm = index_m_clustering5)
    if (index_m_clustering6 == "umap")
    { 
   multiple_sample_clustering<- RunUMAP(multiple_sample_clustering, dims = 1:index_m_clustering7, n.neighbors = index_m_clustering8, min.dist = index_m_clustering9)
   plots16 <-DimPlot(multiple_sample_clustering, reduction = "umap", label = index_m_clustering10, group.by = "seurat_clusters")
   plots17 <-DimPlot(multiple_sample_clustering, reduction = "umap", label = index_m_clustering10, group.by = "condition")
   plots18 <-DimPlot(multiple_sample_clustering, reduction = "umap", label = index_m_clustering10, group.by = "orig.ident")
   plots22 <-SpatialDimPlot(multiple_sample_clustering, label = index_m_clustering10)
   #spatial_cells <- CellsByIdentities(multiple_sample_clustering)
   #plots23 <-SpatialDimPlot(multiple_sample_clustering, images = "TME_cold", label = FALSE, cells.highlight = spatial_cells[setdiff(names(spatial_cells), "NA")], cols.highlight = c("#FFFF00", "grey50"), facet.highlight = T, combine = T) + NoLegend()
   plots24 <-DimPlot(multiple_sample_clustering, reduction = "umap", label = index_m_clustering10, raster=FALSE, group.by = "seurat_clusters", split.by= "condition", ncol = 6)
   plots25 <-DimPlot(multiple_sample_clustering, reduction = "umap", label = index_m_clustering10, raster=FALSE, group.by = "seurat_clusters", split.by= "orig.ident", ncol = 6)
   plots26 <-DimPlot(multiple_sample_clustering, reduction = "umap", label = index_m_clustering10, raster=FALSE, group.by = "condition", split.by= "seurat_clusters", ncol = 6)
  }
    else if (index_m_clustering6 == "tsne")
    {
      multiple_sample_clustering<- RunTSNE(multiple_sample_clustering, dims = 1:index_m_clustering11)
      plots16 <-DimPlot(multiple_sample_clustering, reduction = "tsne", label = index_m_clustering12, group.by = "seurat_clusters")
      plots17 <-DimPlot(multiple_sample_clustering, reduction = "tsne", label = index_m_clustering12, group.by = "condition")
      plots18 <-DimPlot(multiple_sample_clustering, reduction = "tsne", label = index_m_clustering12, group.by = "orig.ident")
      plots22 <-SpatialDimPlot(multiple_sample_clustering, label = index_m_clustering10)
	  #spatial_cells <- CellsByIdentities(multiple_sample_clustering)
      #plots23 <-SpatialDimPlot(multiple_sample_clustering, images = "TME_cold", label = FALSE, cells.highlight = spatial_cells[setdiff(names(spatial_cells), "NA")], cols.highlight = c("#FFFF00", "grey50"), facet.highlight = T, combine = T) + NoLegend()
      plots24 <-DimPlot(multiple_sample_clustering, reduction = "tsne", label = index_m_clustering10, raster=FALSE, group.by = "seurat_clusters", split.by= "condition", ncol = 6)
      plots25 <-DimPlot(multiple_sample_clustering, reduction = "tsne", label = index_m_clustering10, raster=FALSE, group.by = "seurat_clusters", split.by= "orig.ident", ncol = 6)
	  plots26 <-DimPlot(multiple_sample_clustering, reduction = "tsne", label = index_m_clustering10, raster=FALSE, group.by = "condition", split.by= "seurat_clusters", ncol = 6)
  }
    #cell_couts_in_custer
    multiple_sample_clustering_cell_couts_in_custer <- table(multiple_sample_clustering@meta.data$seurat_clusters) %>% as.data.table
    colnames(multiple_sample_clustering_cell_couts_in_custer) <- c("Clusters", "Counts")
    
    plots19 <- ggplot(multiple_sample_clustering@meta.data, aes(seurat_clusters, fill = seurat_clusters)) +
      geom_bar(stat="count", position = position_dodge())+
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5, position = position_dodge(0.9), size=3.5)+
      theme(panel.background = element_blank(), panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(), plot.margin=unit(c(1,1,1,1),"line")) +
      theme(axis.text.x=element_blank())+ guides(fill=guide_legend(title="Cell count"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 1))
    
    #cell_couts_in_custer_for_each_condition
    multiple_sample_clustering_cell_couts_in_condition <- multiple_sample_clustering@meta.data %>% as.data.table
    multiple_sample_clustering_total_cell_couts_in_custer_for_each_condition <- data.frame(t(multiple_sample_clustering_cell_couts_in_condition[, .N, by = c("condition", "seurat_clusters")] %>% dcast(., condition ~ seurat_clusters, value.var = "N"))) %>%  rownames_to_column(var = "condition") %>% `colnames<-`(.[1, ]) %>%  .[-1, ]
    
    plots20 <- ggplot(multiple_sample_clustering@meta.data, aes(seurat_clusters, fill = condition)) +
      geom_bar(stat="count")+
      geom_text(stat='count', aes(label=after_stat(count)), position = position_stack(vjust = 0.5), size=3.5)+
      theme(panel.background = element_blank(), panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(), plot.margin=unit(c(1,1,1,1),"line")) +
      theme(axis.text.x=element_blank())+ guides(fill=guide_legend(title="Condition"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 1))
    
    #cell_couts_in_custer_for_each_samples
    multiple_sample_clustering_cell_couts_in_samples <- multiple_sample_clustering@meta.data %>% as.data.table
    multiple_sample_clustering_total_cell_couts_in_custer_for_each_samples <- data.frame(t(multiple_sample_clustering_cell_couts_in_samples[, .N, by = c("orig.ident", "seurat_clusters")] %>% dcast(., orig.ident ~ seurat_clusters, value.var = "N"))) %>%  rownames_to_column(var = "Clusters") %>% `colnames<-`(.[1, ]) %>%  .[-1, ]
    colnames(multiple_sample_clustering_total_cell_couts_in_custer_for_each_samples)[1] <- "Clusters"
    
    plots21 <- ggplot(multiple_sample_clustering@meta.data, aes(seurat_clusters, fill = orig.ident)) +
      geom_bar(stat="count")+
      geom_text(stat='count', aes(label=after_stat(count)), position = position_stack(vjust = 0.5), size=3.5)+
      theme(panel.background = element_blank(), panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(), plot.margin=unit(c(1,1,1,1),"line")) +
      theme(axis.text.x=element_blank())+ guides(fill=guide_legend(title="Clusters"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 1))
        
    cluster_type = "seurat_clusters"
	 # Load required package
  library(patchwork)
  
   # Define the target identities (TME_cold and TME_hot)
  target_idents <- unique(multiple_sample_clustering$orig.ident)
  
  # Create an empty list to store plots
  plot_list <- list()
  
  # Loop through the target identities and generate SpatialDimPlot
  for (i in seq_along(target_idents)) {
    ident <- target_idents[i]  # Get the current ident
    subset_clustering <- subset(multiple_sample_clustering, subset = orig.ident == ident)
    
    # Print message for debugging
    print(paste("Generating plot for:", ident))
    
    spatial_cells <- CellsByIdentities(subset_clustering)
    # Generate SpatialDimPlot with facet highlighting
    plot_list[[i]] <- SpatialDimPlot(subset_clustering, label = FALSE, 
                                     cells.highlight = spatial_cells[setdiff(names(spatial_cells), "NA")], 
                                     cols.highlight = c("#FFFF00", "grey50"), 
                                     facet.highlight = TRUE) + NoLegend()
  }
  
  # Combine plots vertically using patchwork
  plots23 <- wrap_plots(plot_list, ncol = 1)  # Stack vertically
  
  # Print the combined plot
  # plots23 <- combined_plot
  
 # if (index_multiple_sample_normalization_method == "LogNormalize"){
#	DefaultAssay(multiple_sample_clustering) <- "Spatial"
 #   multiple_sample_clustering <- JoinLayers(multiple_sample_clustering)
 # }
 # else if (index_multiple_sample_normalization_method == "SCTransform"){
 # multiple_sample_clustering <- PrepSCTFindMarkers(multiple_sample_clustering, assay = "SCT", verbose = TRUE)
 # }
 #multiple_sample_clustering <- JoinLayers(multiple_sample_clustering) 
  return(list(plot1 = plots16, plot2 = plots19, plot3 = plots17, plot4 = plots20, plot5 = plots18, plot6 = plots21, data1 = multiple_sample_clustering_cell_couts_in_custer, data2 = multiple_sample_clustering_total_cell_couts_in_custer_for_each_condition, data3 = multiple_sample_clustering_total_cell_couts_in_custer_for_each_samples, data4 = multiple_sample_clustering, data5 = unique(multiple_sample_clustering@meta.data$seurat_clusters), data6 = max(as.numeric(multiple_sample_clustering@meta.data$seurat_clusters)), data7 = unique(multiple_sample_clustering@meta.data$condition), text_summary=cluster_type, plot7 = plots22, plot8 = plots23, plot9 = plots24, plot10 = plots25, plot11 = plots26))
  
}