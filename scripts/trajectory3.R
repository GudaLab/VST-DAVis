datainput_single_multiple_sample_trajectory3 <- function(index_trajectory3_input1, index_trajectory3_input2, index_s_trajectory15, index_s_trajectory16){
  single_multiple_sample_clustering <- index_trajectory3_input1
  cds <- index_trajectory3_input2
  
table_deg <- graph_test(cds, neighbor_graph = index_s_trajectory15)
table_deg <- table_deg %>% arrange(q_value) %>% dplyr::filter(status == "OK")
#Add pseudotime values into the seuratobject
single_multiple_sample_clustering$pseudotime <- pseudotime(cds)
plots104 <- FeaturePlot(single_multiple_sample_clustering, features = "pseudotime", label=index_s_trajectory16)
 
 # Get spatial image names
  image_names <- names(single_multiple_sample_clustering@images)
  
  if (length(image_names) >= 2) {
    
    plot_list <- list()
    
    # Loop over the first two images
    for (i in 1:length(image_names)) {
      
      image_name <- image_names[i]
      
      # Extract spatial coordinates
      spatial_coords <- GetTissueCoordinates(single_multiple_sample_clustering, image = image_name)
      
      # Find common cells between Monocle3 CDS and spatial coordinates
      common_cells <- intersect(rownames(spatial_coords), colnames(cds))
      
      # Subset spatial coordinates and Monocle3 object
      spatial_coords <- spatial_coords[common_cells, , drop = FALSE]
      cds_subset <- cds[, common_cells]
      
      # Extract pseudotime for plotting
      pseudotime_values <- pseudotime(cds_subset)
      
      # Add pseudotime values to Seurat object metadata
      #seurat_obj_subset <- single_multiple_sample_clustering
      single_multiple_sample_clustering$monocle3_pseudotime <- NA  # Initialize column
      single_multiple_sample_clustering$monocle3_pseudotime[common_cells] <- pseudotime_values  # Assign pseudotime
      
      # Fix SpatialFeaturePlot call
      plot_list[[i]] <- SpatialFeaturePlot(
        single_multiple_sample_clustering,
        features = "monocle3_pseudotime",
        alpha = c(0.1, 1)
      ) 
    }
    
    # Combine both spatial plots side by side
    # plots1004 <- plot_list[[1]] + plot_list[[2]]
    plots1004 <- wrap_plots(plot_list, ncol = 1)
    
  } else {
    # If only one spatial image exists, use it
    image_name <- image_names[1]
    
    # Extract spatial coordinates
    spatial_coords <- GetTissueCoordinates(single_multiple_sample_clustering, image = image_name)
    
    # Find common cells between Monocle3 CDS and spatial coordinates
    common_cells <- intersect(rownames(spatial_coords), colnames(cds))
    
    # Subset Monocle3 object
    cds_subset <- cds[, common_cells]
    
    # Extract pseudotime for plotting
    pseudotime_values <- pseudotime(cds_subset)
    
    # Add pseudotime values to Seurat metadata
    #single_multiple_sample_clustering <- single_multiple_sample_clustering
    single_multiple_sample_clustering$monocle3_pseudotime <- NA
    single_multiple_sample_clustering$monocle3_pseudotime[common_cells] <- pseudotime_values
    
    # Fix SpatialFeaturePlot call
    plots1004 <- SpatialFeaturePlot(
      single_multiple_sample_clustering,
      features = "monocle3_pseudotime",
      alpha = c(0.1, 1)
    )
  }
  

return(list(data1 = single_multiple_sample_clustering, data2 = cds, plot1 = plots104, data1 = table_deg, plot2 = plots1004))
}