datainput_single_multiple_sample_cellchat2<- function(index_single_sample_cellchat2_input,  index_s_cellchat11, index_s_cellchat12){
  index_s_cellchat12 <- as.logical(index_s_cellchat12)
  cellchat <- index_single_sample_cellchat2_input
  

  #Identify Key Signals and Pathways
  # Access all the signaling pathways showing significant communications
  # Visualize a specific signaling pathway (e.g., WNT)
  plots606 <-netVisual_aggregate(cellchat, signaling = index_s_cellchat11, layout = "circle", label.edge = index_s_cellchat12)
  plots607 <-netVisual_chord_gene(cellchat, signaling = index_s_cellchat11, lab.cex = 1)
  #plots607 <-netVisual_aggregate(cellchat, signaling = index_s_cellchat11, layout ="chord")
  plots608 <-netVisual_heatmap(cellchat, signaling = index_s_cellchat11, color.heatmap = "Reds")
  vertex.receiver = seq(1,length(unique(cellchat@idents))/2)
  plots609 <-netVisual_aggregate(cellchat, signaling = index_s_cellchat11, layout = "hierarchy", vertex.receiver = vertex.receiver)
  plots610 <-netVisual_bubble(cellchat, signaling = index_s_cellchat11, remove.isolate = FALSE, title.name = index_s_cellchat11)
  plots611 <-netAnalysis_contribution(cellchat, signaling = index_s_cellchat11) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #netAnalysis_signalingRole_network(cellchat, signaling = index_s_cellchat11, font.size = 10)
  #violinplot
  plots612 <-plotGeneExpression(cellchat, signaling = index_s_cellchat11, enriched.only = TRUE)
  interaction_table_selected_pathway <- subsetCommunication(cellchat, signaling = index_s_cellchat11)
  
  unique_samples <- unique(cellchat@meta$samples)
  
  # Initialize an empty list to store plots
  spatial_cellchat_plot_list <- list()
  
  # Loop through each sample and generate network visualization
  for (sample in unique_samples) {
    print(paste("Processing Sample:", sample))
    
    # Generate the network visualization for the current sample
    p <- netVisual_aggregate(
      cellchat,
      signaling = index_s_cellchat11,
      sample.use = sample,  # Using the current sample in the loop
      layout = "spatial",
      edge.width.max = 2,
      vertex.size.max = 1,
      alpha.image = 0.2,
      vertex.label.cex = 0
    )
    
    # Store the plot in the list with sample name as key
    spatial_cellchat_plot_list[[sample]] <- p
  }
  
  # Print all plots together using wrap_plots()
  plots613 <- wrap_plots(spatial_cellchat_plot_list)
  
  return(list(plot1 = plots606, plot2 = plots607, plot3 = plots608, plot4 = plots609, plot5 = plots610, plot6 = plots611, plot7 = plots612, data1=interaction_table_selected_pathway, data2=cellchat, plot8 = plots613)) 
}
