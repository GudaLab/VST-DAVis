datainput_single_multiple_sample_cellchat1<- function(index_multiple_sample_cellchat1_input, index_subclustering_multiple_sample_cellchat1_input, index_multiple_sample_cellchat1_input2, index_subclustering_multiple_sample_cellchat1_input2, index_multiple_sample_normalization_method_cellchat1, index_subclustering_multiple_sample_normalization_method_cellchat1, index_s_cellchat1, index_s_cellchat2, index_s_cellchat3, index_s_cellchat4, index_s_cellchat5, index_s_cellchat6, index_s_cellchat7, index_s_cellchat8, index_s_cellchat9, index_s_cellchat10, index_s_cellchat13, index_s_cellchat14, index_s_cellchat15, index_s_cellchat16, index_s_cellchat17){
  suppressPackageStartupMessages(require("ggalluvial", character.only = TRUE, quietly = TRUE))
  index_s_cellchat10 <- as.logical(index_s_cellchat10)
  index_s_cellchat13 <- as.logical(index_s_cellchat13)
  index_s_cellchat16 <- as.logical(index_s_cellchat16)
  index_s_cellchat9 <- suppressWarnings(as.integer(index_s_cellchat9))
  if (is.na(index_s_cellchat9) || index_s_cellchat9 < 1) {
    index_s_cellchat9 <- 2L
  }

  make_info_plot <- function(label_text) {
    ggplot2::ggplot(data.frame(x = 1, y = 1, label = label_text), ggplot2::aes(x = x, y = y, label = label)) +
      ggplot2::geom_text(lineheight = 1.1, size = 4) +
      ggplot2::theme_void() +
      ggplot2::xlim(0.5, 1.5) +
      ggplot2::ylim(0.5, 1.5)
  }

  scale_pattern_matrix <- function(mat, margin = c("r1", "c1")) {
    margin <- match.arg(margin)
    mat <- as.matrix(mat)

    if (margin == "r1") {
      denom <- rowSums(mat, na.rm = TRUE)
      denom[!is.finite(denom) | denom == 0] <- 1
      return(sweep(mat, 1, denom, "/", check.margin = FALSE))
    }

    denom <- colSums(mat, na.rm = TRUE)
    denom[!is.finite(denom) | denom == 0] <- 1
    sweep(mat, 2, denom, "/", check.margin = FALSE)
  }

  safe_identify_communication_patterns <- function(object, pattern_name, k_value, slot.name = "netP") {
    pattern_name <- match.arg(pattern_name, c("incoming", "outgoing"))
    suppressPackageStartupMessages(require("NMF", character.only = TRUE, quietly = TRUE))
    prob <- methods::slot(object, slot.name)$prob

    if (pattern_name == "outgoing") {
      data_sender <- apply(prob, c(1, 3), sum)
      data_sender <- sweep(
        data_sender,
        2L,
        apply(data_sender, 2, function(x) max(x, na.rm = TRUE)),
        "/",
        check.margin = FALSE
      )
      data0 <- as.matrix(data_sender)
    } else {
      data_receiver <- apply(prob, c(2, 3), sum)
      data_receiver <- sweep(
        data_receiver,
        2L,
        apply(data_receiver, 2, function(x) max(x, na.rm = TRUE)),
        "/",
        check.margin = FALSE
      )
      data0 <- as.matrix(data_receiver)
    }

    data <- data0[rowSums(data0, na.rm = TRUE) != 0, , drop = FALSE]
    if (nrow(data) == 0 || ncol(data) == 0) {
      stop(sprintf("No non-zero signaling matrix is available for %s pattern analysis.", pattern_name))
    }

    max_rank <- min(nrow(data), ncol(data))
    if (!is.finite(max_rank) || max_rank < 1) {
      stop(sprintf("No valid NMF rank is available for %s pattern analysis.", pattern_name))
    }

    k_use <- min(as.integer(k_value), as.integer(max_rank))
    unique_row_count <- nrow(unique(as.data.frame(data)))
    if (is.finite(unique_row_count) && unique_row_count >= 1) {
      k_use <- min(k_use, as.integer(unique_row_count))
    }
    if (k_use < 1) {
      stop(sprintf("The requested pattern rank is invalid for %s pattern analysis.", pattern_name))
    }

    if (k_use != as.integer(k_value)) {
      message(sprintf("CellChat %s pattern rank reduced from %d to %d to match the available signaling matrix.", pattern_name, as.integer(k_value), k_use))
    }

    nmf_fit <- tryCatch(
      NMF::nmf(data, rank = k_use, method = "lee", seed = "nndsvd"),
      error = function(e1) {
        message(sprintf("CellChat %s pattern NMF fallback 1: %s", pattern_name, conditionMessage(e1)))
        tryCatch(
          NMF::nmf(data, rank = k_use, method = "lee", seed = 123456L),
          error = function(e2) {
            message(sprintf("CellChat %s pattern NMF fallback 2: %s", pattern_name, conditionMessage(e2)))
            tryCatch(
              NMF::nmf(data, rank = k_use, method = "brunet", seed = 123456L),
              error = function(e3) {
                message(sprintf("CellChat %s pattern NMF fallback 3: %s", pattern_name, conditionMessage(e3)))
                NULL
              }
            )
          }
        )
      }
    )

    if (inherits(nmf_fit, "NMFfit")) {
      W <- scale_pattern_matrix(nmf_fit@fit@W, "r1")
      H <- scale_pattern_matrix(nmf_fit@fit@H, "c1")
    } else {
      km_fit <- stats::kmeans(data, centers = k_use, nstart = max(10L, k_use * 5L), iter.max = 200)
      W <- matrix(0, nrow = nrow(data), ncol = k_use)
      W[cbind(seq_len(nrow(data)), km_fit$cluster)] <- 1
      rownames(W) <- rownames(data)
      H <- scale_pattern_matrix(km_fit$centers, "c1")
      message(sprintf("CellChat %s pattern analysis used k-means fallback because NMF fitting was unavailable.", pattern_name))
    }

    if (is.null(rownames(W))) {
      rownames(W) <- rownames(data)
    }
    colnames(W) <- paste0("Pattern ", seq_len(ncol(W)))
    if (is.null(colnames(H))) {
      colnames(H) <- colnames(data)
    }
    rownames(H) <- paste0("Pattern ", seq_len(nrow(H)))

    data_W <- as.data.frame(as.table(W))
    colnames(data_W) <- c("CellGroup", "Pattern", "Contribution")
    data_H <- as.data.frame(as.table(H))
    colnames(data_H) <- c("Pattern", "Signaling", "Contribution")

    methods::slot(object, slot.name)$pattern[[pattern_name]] <- list(
      data = data0,
      pattern = list(cell = data_W, signaling = data_H)
    )

    object
  }

  has_pattern_result <- function(object, pattern_name, slot.name = "netP") {
    pattern_res <- tryCatch(methods::slot(object, slot.name)$pattern[[pattern_name]], error = function(e) NULL)
    !is.null(pattern_res) && !is.null(pattern_res$pattern)
  }
  
  if (index_s_cellchat1 == "multiple_sample" & index_s_cellchat2 == "seurat_clusters"){
    single_multiple_sample_clustering <- index_multiple_sample_cellchat1_input 
    current_clusters <- levels(single_multiple_sample_clustering)
    # Create new names for clusters (e.g., "Cluster0", "Cluster1", ...)
    new_cluster_names <- paste0("Cluster", current_clusters)
    # Rename clusters in the Seurat object
    names(new_cluster_names) <- current_clusters
    single_multiple_sample_clustering <- RenameIdents(single_multiple_sample_clustering, new_cluster_names)
    levels(single_multiple_sample_clustering)
    single_multiple_sample_clustering$new_cluster_names <- Idents(single_multiple_sample_clustering)
    
	if (index_multiple_sample_normalization_method_cellchat1 == "LogNormalize"){
	DefaultAssay(single_multiple_sample_clustering) <- "RNA"
	#data.input <- GetAssayData(single_multiple_sample_clustering, assay = "RNA", layer = "data") # normalized data matrix
    }
	else if (index_multiple_sample_normalization_method_cellchat1 == "SCTransform"){
	DefaultAssay(single_multiple_sample_clustering) <- "SCT"
	#data.input <- GetAssayData(single_multiple_sample_clustering, assay = "SCT", layer = "data") # normalized data matrix
    }
	
	single_multiple_sample_clustering@meta.data$samples <- single_multiple_sample_clustering@meta.data$orig.ident
	
	 coordinates_list <- list()
     spatial_factors_list <- list()
  
  # Loop through available spatial images in Seurat object
  for (image_name in names(single_multiple_sample_clustering@images)) {
    
    # Extract spatial coordinates safely
    tissue_coords <- try(GetTissueCoordinates(single_multiple_sample_clustering, image = image_name), silent = TRUE)
    
    # If spatial coordinates are missing, generate random coordinates
    if (inherits(tissue_coords, "try-error")) {
      message(paste("No spatial coordinates found for", image_name, "- Generating random coordinates."))
      
      # Get metadata for the missing image
      meta_data <- subset(single_multiple_sample_clustering@meta.data, orig.ident == image_name)
      num_cells <- nrow(meta_data)
      
      # Generate random X, Y coordinates
      set.seed(123)  
      tissue_coords <- data.frame(
        x = runif(num_cells, min = 0, max = 100),
        y = runif(num_cells, min = 0, max = 100)
      )
      
      # Assign row names to match cell barcodes
      rownames(tissue_coords) <- rownames(meta_data)
    }
    
    # Store extracted coordinates
    coordinates_list[[image_name]] <- tissue_coords
    
    # Extract spatial scale factors safely
    spatial_factors <- try(single_multiple_sample_clustering@images[[image_name]]@scale.factors, silent = TRUE)
    
    # If scale factors are missing, assign default values
    if (inherits(spatial_factors, "try-error") || is.null(spatial_factors)) {
      message(paste("No scale factors found for", image_name, "- Using default values."))
      
      spatial_factors <- list(spot.diameter = 55, spot = 1)
    }
    
    # Calculate conversion factor
    conversion.factor <- 55 / spatial_factors$spot 
    
    # Create `spatial.factors` dataframe with `ratio` and `tol`
    spatial_factors_list[[image_name]] <- data.frame(
      ratio = conversion.factor,
      tol = spatial_factors$spot / 2
    )
  }
  
  # Combine extracted coordinates into a single matrix
  coordinates <- do.call(rbind, coordinates_list)
  
  # Ensure only two columns (X, Y)
  coordinates <- coordinates[, c("x", "y")]
  
  # Ensure row names match Seurat object metadata
  rownames(coordinates) <- gsub("^[^.]+\\.", "", rownames(coordinates))
  coordinates <- as.matrix(coordinates)
  
  # Combine all spatial factors into one dataframe
  spatial_factors_all <- do.call(rbind, spatial_factors_list)
  
  # Ensure row names match the expected format
  rownames(spatial_factors_all) <- names(single_multiple_sample_clustering@images)
  
  
  # Pass extracted coordinates and scale factors to createCellChat()
  cellchat <- createCellChat(object = single_multiple_sample_clustering, meta = single_multiple_sample_clustering@meta.data, datatype="spatial", group.by = "new_cluster_names", coordinates = coordinates, spatial.factors = spatial_factors_all)
  
  
	#Convert Seurat Object to CellChat Object
    #cellchat <- createCellChat(object = single_multiple_sample_clustering, meta = single_multiple_sample_clustering@meta.data, group.by = "new_cluster_names") 
  }
  
  else if (index_s_cellchat1 == "multiple_sample_subclustering" & index_s_cellchat2 == "seurat_clusters"){
    single_multiple_sample_clustering <- index_subclustering_multiple_sample_cellchat1_input  
    current_clusters <- levels(single_multiple_sample_clustering)
    # Create new names for clusters (e.g., "Cluster0", "Cluster1", ...)
    new_cluster_names <- paste0("Cluster", current_clusters)
    # Rename clusters in the Seurat object
    names(new_cluster_names) <- current_clusters
    single_multiple_sample_clustering <- RenameIdents(single_multiple_sample_clustering, new_cluster_names)
    levels(single_multiple_sample_clustering)
    
	single_multiple_sample_clustering$new_cluster_names <- Idents(single_multiple_sample_clustering)
    if (index_subclustering_multiple_sample_normalization_method_cellchat1 == "LogNormalize"){
	DefaultAssay(single_multiple_sample_clustering) <- "RNA"
	#data.input <- GetAssayData(single_multiple_sample_clustering, assay = "RNA", layer = "data") # normalized data matrix
    }
	else if (index_subclustering_multiple_sample_normalization_method_cellchat1 == "SCTransform"){
	DefaultAssay(single_multiple_sample_clustering) <- "SCT"
	#data.input <- GetAssayData(single_multiple_sample_clustering, assay = "SCT", layer = "data") # normalized data matrix
    }
	 coordinates_list <- list()
	 spatial_factors_list <- list()
  
  # Loop through available spatial images in Seurat object
  for (image_name in names(single_multiple_sample_clustering@images)) {
    
    # Extract spatial coordinates safely
    tissue_coords <- try(GetTissueCoordinates(single_multiple_sample_clustering, image = image_name), silent = TRUE)
    
    # If spatial coordinates are missing, generate random coordinates
    if (inherits(tissue_coords, "try-error")) {
      message(paste("No spatial coordinates found for", image_name, "- Generating random coordinates."))
      
      # Get metadata for the missing image
      meta_data <- subset(single_multiple_sample_clustering@meta.data, orig.ident == image_name)
      num_cells <- nrow(meta_data)
      
      # Generate random X, Y coordinates
      set.seed(123)  
      tissue_coords <- data.frame(
        x = runif(num_cells, min = 0, max = 100),
        y = runif(num_cells, min = 0, max = 100)
      )
      
      # Assign row names to match cell barcodes
      rownames(tissue_coords) <- rownames(meta_data)
    }
    
    # Store extracted coordinates
    coordinates_list[[image_name]] <- tissue_coords
    
    # Extract spatial scale factors safely
    spatial_factors <- try(single_multiple_sample_clustering@images[[image_name]]@scale.factors, silent = TRUE)
    
    # If scale factors are missing, assign default values
    if (inherits(spatial_factors, "try-error") || is.null(spatial_factors)) {
      message(paste("No scale factors found for", image_name, "- Using default values."))
      
      spatial_factors <- list(spot.diameter = 55, spot = 1)
    }
    
    # Calculate conversion factor
    conversion.factor <- 55 / spatial_factors$spot 
    
    # Create `spatial.factors` dataframe with `ratio` and `tol`
    spatial_factors_list[[image_name]] <- data.frame(
      ratio = conversion.factor,
      tol = spatial_factors$spot / 2
    )
  }
  
  # Combine extracted coordinates into a single matrix
  coordinates <- do.call(rbind, coordinates_list)
  
  # Ensure only two columns (X, Y)
  coordinates <- coordinates[, c("x", "y")]
  
  # Ensure row names match Seurat object metadata
  rownames(coordinates) <- gsub("^[^.]+\\.", "", rownames(coordinates))
  coordinates <- as.matrix(coordinates)
  
  # Combine all spatial factors into one dataframe
  spatial_factors_all <- do.call(rbind, spatial_factors_list)
  
  # Ensure row names match the expected format
  rownames(spatial_factors_all) <- names(single_multiple_sample_clustering@images)
  
  
  # Pass extracted coordinates and scale factors to createCellChat()
  cellchat <- createCellChat(object = single_multiple_sample_clustering, meta = single_multiple_sample_clustering@meta.data, datatype="spatial", group.by = "new_cluster_names", coordinates = coordinates, spatial.factors = spatial_factors_all)
  
	}
  
  else if (index_s_cellchat1 == "multiple_sample" & index_s_cellchat2 == "predicted"){
    single_multiple_sample_clustering <- index_multiple_sample_cellchat1_input
    #data.input <- GetAssayData(single_multiple_sample_clustering, assay = "RNA", layer = "data") # normalized data matrix
    #Convert Seurat Object to CellChat Object
    #cellchat <- createCellChat(object = single_multiple_sample_clustering, meta = single_multiple_sample_clustering@meta.data, group.by = index_multiple_sample_cellchat1_input2) 
   if (index_multiple_sample_normalization_method_cellchat1 == "LogNormalize"){
	DefaultAssay(single_multiple_sample_clustering) <- "RNA"
	#data.input <- GetAssayData(single_multiple_sample_clustering, assay = "RNA", layer = "data") # normalized data matrix
    }
	else if (index_multiple_sample_normalization_method_cellchat1 == "SCTransform"){
	DefaultAssay(single_multiple_sample_clustering) <- "SCT"
	#data.input <- GetAssayData(single_multiple_sample_clustering, assay = "SCT", layer = "data") # normalized data matrix
    }
	
	single_multiple_sample_clustering@meta.data$samples <- single_multiple_sample_clustering@meta.data$orig.ident
	
	 coordinates_list <- list()
     spatial_factors_list <- list()
  
  # Loop through available spatial images in Seurat object
  for (image_name in names(single_multiple_sample_clustering@images)) {
    
    # Extract spatial coordinates safely
    tissue_coords <- try(GetTissueCoordinates(single_multiple_sample_clustering, image = image_name), silent = TRUE)
    
    # If spatial coordinates are missing, generate random coordinates
    if (inherits(tissue_coords, "try-error")) {
      message(paste("No spatial coordinates found for", image_name, "- Generating random coordinates."))
      
      # Get metadata for the missing image
      meta_data <- subset(single_multiple_sample_clustering@meta.data, orig.ident == image_name)
      num_cells <- nrow(meta_data)
      
      # Generate random X, Y coordinates
      set.seed(123)  
      tissue_coords <- data.frame(
        x = runif(num_cells, min = 0, max = 100),
        y = runif(num_cells, min = 0, max = 100)
      )
      
      # Assign row names to match cell barcodes
      rownames(tissue_coords) <- rownames(meta_data)
    }
    
    # Store extracted coordinates
    coordinates_list[[image_name]] <- tissue_coords
    
    # Extract spatial scale factors safely
    spatial_factors <- try(single_multiple_sample_clustering@images[[image_name]]@scale.factors, silent = TRUE)
    
    # If scale factors are missing, assign default values
    if (inherits(spatial_factors, "try-error") || is.null(spatial_factors)) {
      message(paste("No scale factors found for", image_name, "- Using default values."))
      
      spatial_factors <- list(spot.diameter = 55, spot = 1)
    }
    
    # Calculate conversion factor
    conversion.factor <- 55 / spatial_factors$spot 
    
    # Create `spatial.factors` dataframe with `ratio` and `tol`
    spatial_factors_list[[image_name]] <- data.frame(
      ratio = conversion.factor,
      tol = spatial_factors$spot / 2
    )
  }
  
  # Combine extracted coordinates into a single matrix
  coordinates <- do.call(rbind, coordinates_list)
  
  # Ensure only two columns (X, Y)
  coordinates <- coordinates[, c("x", "y")]
  
  # Ensure row names match Seurat object metadata
  rownames(coordinates) <- gsub("^[^.]+\\.", "", rownames(coordinates))
  coordinates <- as.matrix(coordinates)
  
  # Combine all spatial factors into one dataframe
  spatial_factors_all <- do.call(rbind, spatial_factors_list)
  
  # Ensure row names match the expected format
  rownames(spatial_factors_all) <- names(single_multiple_sample_clustering@images)
  
  
  # Pass extracted coordinates and scale factors to createCellChat()
  cellchat <- createCellChat(object = single_multiple_sample_clustering, meta = single_multiple_sample_clustering@meta.data, datatype="spatial", group.by = index_multiple_sample_cellchat1_input2, coordinates = coordinates, spatial.factors = spatial_factors_all)
  
   
   }
  else if (index_s_cellchat1 == "multiple_sample_subclustering" & index_s_cellchat2 == "predicted"){
    single_multiple_sample_clustering <- index_subclustering_multiple_sample_cellchat1_input
    #data.input <- GetAssayData(single_multiple_sample_clustering, assay = "RNA", layer = "data") # normalized data matrix
    #Convert Seurat Object to CellChat Object
    #cellchat <- createCellChat(object = single_multiple_sample_clustering, meta = single_multiple_sample_clustering@meta.data, group.by = index_subclustering_multiple_sample_cellchat1_input2) 
    
	  if (index_subclustering_multiple_sample_normalization_method_cellchat1 == "LogNormalize"){
	DefaultAssay(single_multiple_sample_clustering) <- "RNA"
	#data.input <- GetAssayData(single_multiple_sample_clustering, assay = "RNA", layer = "data") # normalized data matrix
    }
	else if (index_subclustering_multiple_sample_normalization_method_cellchat1 == "SCTransform"){
	DefaultAssay(single_multiple_sample_clustering) <- "SCT"
	#data.input <- GetAssayData(single_multiple_sample_clustering, assay = "SCT", layer = "data") # normalized data matrix
    }
	
	single_multiple_sample_clustering@meta.data$samples <- single_multiple_sample_clustering@meta.data$orig.ident
	
	 coordinates_list <- list()
     spatial_factors_list <- list()
  
  # Loop through available spatial images in Seurat object
  for (image_name in names(single_multiple_sample_clustering@images)) {
    
    # Extract spatial coordinates safely
    tissue_coords <- try(GetTissueCoordinates(single_multiple_sample_clustering, image = image_name), silent = TRUE)
    
    # If spatial coordinates are missing, generate random coordinates
    if (inherits(tissue_coords, "try-error")) {
      message(paste("No spatial coordinates found for", image_name, "- Generating random coordinates."))
      
      # Get metadata for the missing image
      meta_data <- subset(single_multiple_sample_clustering@meta.data, orig.ident == image_name)
      num_cells <- nrow(meta_data)
      
      # Generate random X, Y coordinates
      set.seed(123)  
      tissue_coords <- data.frame(
        x = runif(num_cells, min = 0, max = 100),
        y = runif(num_cells, min = 0, max = 100)
      )
      
      # Assign row names to match cell barcodes
      rownames(tissue_coords) <- rownames(meta_data)
    }
    
    # Store extracted coordinates
    coordinates_list[[image_name]] <- tissue_coords
    
    # Extract spatial scale factors safely
    spatial_factors <- try(single_multiple_sample_clustering@images[[image_name]]@scale.factors, silent = TRUE)
    
    # If scale factors are missing, assign default values
    if (inherits(spatial_factors, "try-error") || is.null(spatial_factors)) {
      message(paste("No scale factors found for", image_name, "- Using default values."))
      
      spatial_factors <- list(spot.diameter = 55, spot = 1)
    }
    
    # Calculate conversion factor
    conversion.factor <- 55 / spatial_factors$spot 
    
    # Create `spatial.factors` dataframe with `ratio` and `tol`
    spatial_factors_list[[image_name]] <- data.frame(
      ratio = conversion.factor,
      tol = spatial_factors$spot / 2
    )
  }
  
  # Combine extracted coordinates into a single matrix
  coordinates <- do.call(rbind, coordinates_list)
  
  # Ensure only two columns (X, Y)
  coordinates <- coordinates[, c("x", "y")]
  
  # Ensure row names match Seurat object metadata
  rownames(coordinates) <- gsub("^[^.]+\\.", "", rownames(coordinates))
  coordinates <- as.matrix(coordinates)
  
  # Combine all spatial factors into one dataframe
  spatial_factors_all <- do.call(rbind, spatial_factors_list)
  
  # Ensure row names match the expected format
  rownames(spatial_factors_all) <- names(single_multiple_sample_clustering@images)
  
  
  # Pass extracted coordinates and scale factors to createCellChat()
  cellchat <- createCellChat(object = single_multiple_sample_clustering, meta = single_multiple_sample_clustering@meta.data, datatype="spatial", group.by = index_subclustering_multiple_sample_cellchat1_input2, coordinates = coordinates, spatial.factors = spatial_factors_all)
  
	
	}
  
  #Set the Database for Cell-Cell Interaction
  if(index_s_cellchat3 == "PPI.human"){
  CellChatDB <- CellChatDB.human  
  cellchat@DB <- CellChatDB
  }
  else if(index_s_cellchat3 == "PPI.mouse"){
    CellChatDB <- CellChatDB.mouse 
    cellchat@DB <- CellChatDB
  }
  
  # Subset the expression data and signaling genes
  cellchat <- subsetData(cellchat)  # This subsets the expression data to include only the signaling genes
  # Identify overexpressed genes and interactions
  cellchat <- identifyOverExpressedGenes(cellchat, thresh.pc = index_s_cellchat4, thresh.fc = index_s_cellchat5, thresh.p = index_s_cellchat6)
  cellchat <- identifyOverExpressedInteractions(cellchat)
  # Project the data onto the PPI network
  if(index_s_cellchat3 == "PPI.human"){
  cellchat <- smoothData(cellchat, adj = PPI.human)  # for human, or PPI.mouse for mouse
  }
  else if(index_s_cellchat3 == "PPI.mouse"){
  cellchat <- smoothData(cellchat, adj = PPI.mouse)  
  }
  
  
  # Compute the communication probability
  if (index_s_cellchat13 == FALSE){
  cellchat <- computeCommunProb(cellchat, type = index_s_cellchat7, trim = 0.1, distance.use = index_s_cellchat13, scale.distance = NULL, interaction.range = index_s_cellchat15, contact.dependent = index_s_cellchat16, contact.range = index_s_cellchat17)
  }
  else if (index_s_cellchat13 == TRUE){
  cellchat <- computeCommunProb(cellchat, type = index_s_cellchat7, trim = 0.1, distance.use = index_s_cellchat13, scale.distance = index_s_cellchat14, interaction.range = index_s_cellchat15, contact.dependent = index_s_cellchat16, contact.range = index_s_cellchat17)
  }
    
  # Filter communication based on a probability threshold
  cellchat <- filterCommunication(cellchat, min.cells = index_s_cellchat8)
  
  
  # Infer the communication network
  cellchat <- computeCommunProbPathway(cellchat)
  
  
  # Aggregate the communication network
  cellchat <- aggregateNet(cellchat)
  
  
  #Visualize the Results
  #Plot Interaction Networks
  # Visualize the interaction network of all signaling pathways
  groupSize <- as.numeric(table(cellchat@idents))
  
      
  #Signaling Role Analysis
  # Perform a role analysis (sender, receiver, mediator, influencer)
  cellchat <- netAnalysis_computeCentrality(cellchat)
  # Visualize signaling roles using circle plot
  #par(mfrow = c(1,2), xpd=TRUE)
  #grid.newpage() 
  plots601 <- netVisual_circle(cellchat@net$count, vertex.weight = groupSize, weight.scale = T, label.edge= index_s_cellchat10, title.name = "Number of interactions")
  plots602 <- netVisual_circle(cellchat@net$weight, vertex.weight = groupSize, weight.scale = T, label.edge= index_s_cellchat10, title.name ="Interaction weights/strength")
  #plots601+plots602
  #Heatmap of Interactions
  # Plot a heatmap of communication probabilities
  plots603 <- netVisual_heatmap(cellchat, measure = "weight")
  # Extract all communication interactions
  interaction_table <- subsetCommunication(cellchat)
  
  #Systems analysis of cell-cell communication network
  pathways_show_all <- as.data.frame(cellchat@netP$pathways)
  cellchat <- netAnalysis_computeCentrality(cellchat, slot.name = "netP") # the slot 'netP' means the inferred intercellular communication network of signaling pathways# Visualize the computed centrality scores using heatmap, allowing ready identification of major signaling roles of cell groups

  plots604 <- netAnalysis_signalingRole_heatmap(cellchat, pattern = "incoming", height = 20)
  plots605 <- netAnalysis_signalingRole_heatmap(cellchat, pattern = "outgoing", height = 20)
  #index_s_cellchat9
  pattern_messages <- character(0)
  cellchat <- tryCatch(
    safe_identify_communication_patterns(cellchat, pattern_name = "incoming", k_value = index_s_cellchat9),
    error = function(e) {
      pattern_messages <<- c(pattern_messages, paste0("Incoming pattern analysis skipped: ", conditionMessage(e)))
      cellchat
    }
  )
  cellchat <- tryCatch(
    safe_identify_communication_patterns(cellchat, pattern_name = "outgoing", k_value = index_s_cellchat9),
    error = function(e) {
      pattern_messages <<- c(pattern_messages, paste0("Outgoing pattern analysis skipped: ", conditionMessage(e)))
      cellchat
    }
  )

  plots606 <- if (has_pattern_result(cellchat, "incoming")) {
    tryCatch(
      netAnalysis_river(cellchat, pattern = "incoming"),
      error = function(e) make_info_plot(paste("Incoming communication pattern plot is unavailable.", conditionMessage(e), sep = "\n"))
    )
  } else {
    make_info_plot(paste(c("Incoming communication pattern plot is unavailable.", pattern_messages[grepl("^Incoming", pattern_messages)]), collapse = "\n"))
  }

  plots607 <- if (has_pattern_result(cellchat, "outgoing")) {
    tryCatch(
      netAnalysis_river(cellchat, pattern = "outgoing"),
      error = function(e) make_info_plot(paste("Outgoing communication pattern plot is unavailable.", conditionMessage(e), sep = "\n"))
    )
  } else {
    make_info_plot(paste(c("Outgoing communication pattern plot is unavailable.", pattern_messages[grepl("^Outgoing", pattern_messages)]), collapse = "\n"))
  }
  
  return(list(plot1 = plots601, plot2 = plots602, plot3 = plots603, plot4 = plots604+plots605, plot5 = plots606+plots607, data1 =interaction_table, data3=pathways_show_all, data2=cellchat)) 
  
}
    
