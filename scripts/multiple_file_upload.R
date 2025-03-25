datainput_multiple_sample <- function(index_multiple_sample_file, index_multiple_sample_file1, index_multiple_sample_file_names, index_multiple_sample_format, index_multiple_sample_name){
  index_multiple_sample_format <- as.character(index_multiple_sample_format)
  
  if (index_multiple_sample_format == "h5") {
    data_dirs <- sub('\\.zip$', '', basename(index_multiple_sample_file_names))
    #Create a list of count matrices
    path <- getwd()
    setwd(index_multiple_sample_file1)
    # Unzip all files
    lapply(index_multiple_sample_file_names, unzip)
    
    # Load10X_Spatial function applied to each dataset
    library(Seurat)
    
    load_spatial_data <- function(data_dir) {
      obj <- Load10X_Spatial(
        data.dir = data_dir,
        filename = "filtered_feature_bc_matrix.h5",
        assay = "Spatial",
        filter.matrix = TRUE,
        slice = data_dir
      )
      
      obj@project.name <- data_dir
      obj$orig.ident <- data_dir
      Idents(obj) <- 'orig.ident'
      
	  # Extract count matrix from Spatial
  counts_matrix <- obj@assays$Spatial$counts
  
  # Create an RNA assay from Spatial counts
  obj[["RNA"]] <- CreateAssayObject(counts = counts_matrix)
  
  # Set RNA as the default assay for analysis
  DefaultAssay(obj) <- "RNA"
  
      return(obj)
    }
    
    # Apply function to all extracted datasets
    spatial_objects <- mapply(load_spatial_data, data_dirs, SIMPLIFY = FALSE)
    
    # Display table of cell identities for each object
    lapply(spatial_objects, function(obj) table(obj$orig.ident))
    
    merged_spatial <- merge(spatial_objects[[1]], y = spatial_objects[-1], add.cell.ids = names(spatial_objects),project="merged")
    merged_spatial[["percent.mt"]] <- PercentageFeatureSet(merged_spatial, pattern = "^MT-")
    
    # Print basic information about the merged object
    Idents(merged_spatial) <- merged_spatial@meta.data$orig.ident
    table1 <- table(merged_spatial$orig.ident) %>% as.data.frame 
    colnames(table1) <- c("Sample names", "Cell counts")
    multiple_list <- SplitObject(merged_spatial, split.by = "orig.ident")
    setwd(path)
  }
  
  else if (index_multiple_sample_format == "MFB"){
    #index_multiple_sample_file1 <- list.files(pattern="*.gz")
    #files <- list.files(path = index_multiple_sample_file, pattern = "\\.zip$", full.names = TRUE)
    data_dirs <- sub('\\.zip$', '', basename(index_multiple_sample_file_names))
    #Create a list of count matrices
    path <- getwd()
    setwd(index_multiple_sample_file1)
    # Unzip all files
    lapply(index_multiple_sample_file_names, unzip)
    
    # Load necessary library
    library(Seurat)
    
    load_spatial_data <- function(data_dir) {
      matrix_dir <- file.path(data_dir, "filtered_feature_bc_matrix")
      spatial_dir <- file.path(data_dir, "spatial")
      
      obj <- CreateSeuratObject(
        counts = Read10X(matrix_dir),
        assay = "Spatial",
        project = data_dir
      )
      
      if (file.exists(spatial_dir)) {
        obj[[data_dir]] <- Read10X_Image(spatial_dir, assay = "Spatial", filter.matrix = TRUE, slice = data_dir)
      }
      # Extract count matrix from Spatial
  counts_matrix <- obj@assays$Spatial$counts
  
  # Create an RNA assay from Spatial counts
  obj[["RNA"]] <- CreateAssayObject(counts = counts_matrix)
  
  # Set RNA as the default assay for analysis
  DefaultAssay(obj) <- "RNA"
	  
      return(obj)
    }
    
    # Apply function to all extracted datasets
    spatial_objects <- mapply(load_spatial_data, data_dirs, SIMPLIFY = FALSE)
    
    # Display table of cell identities for each object
    lapply(spatial_objects, function(obj) table(obj$orig.ident))
    
    merged_spatial <- merge(spatial_objects[[1]], y = spatial_objects[-1], add.cell.ids = names(spatial_objects), project="merged")
    merged_spatial[["percent.mt"]] <- PercentageFeatureSet(merged_spatial, pattern = "^MT-")
    
    # Print basic information about the merged object
    Idents(merged_spatial) <- merged_spatial@meta.data$orig.ident
    table1 <- table(merged_spatial$orig.ident) %>% as.data.frame 
    colnames(table1) <- c("Sample names", "Cell counts")
    multiple_list <- SplitObject(merged_spatial, split.by = "orig.ident")
    setwd(path)

  }
 
  else if (index_multiple_sample_format == "exampledata")
  {
    index_multiple_sample_file1 <- "www/example_data/GSE230207"
    index_multiple_sample_file_names <- "Example data to test the tool (GSE230207)"
    files <- list.files(path = index_multiple_sample_file1, pattern = "\\.zip$", full.names = TRUE)
    
    # Extract file names without .zip extension
    data_dirs <- sub('\\.zip$', '', basename(files))
    
    # Unzip all files
    lapply(files, unzip)
    
    # Load10X_Spatial function applied to each dataset
    library(Seurat)
    
    load_spatial_data <- function(data_dir) {
      obj <- Load10X_Spatial(
        data.dir = data_dir,
        filename = "filtered_feature_bc_matrix.h5",
        assay = "Spatial",
        filter.matrix = TRUE,
        slice = data_dir
      )
      
      obj@project.name <- data_dir
      obj$orig.ident <- data_dir
      Idents(obj) <- 'orig.ident'
      
	  # Extract count matrix from Spatial
  counts_matrix <- obj@assays$Spatial$counts
  
  # Create an RNA assay from Spatial counts
  obj[["RNA"]] <- CreateAssayObject(counts = counts_matrix)
  
  # Set RNA as the default assay for analysis
  DefaultAssay(obj) <- "RNA"
  
      return(obj)
    }
    
    # Apply function to all extracted datasets
    spatial_objects <- mapply(load_spatial_data, data_dirs, SIMPLIFY = FALSE)
    
    # Display table of cell identities for each object
    lapply(spatial_objects, function(obj) table(obj$orig.ident))
    
    merged_spatial <- merge(spatial_objects[[1]], y = spatial_objects[-1], add.cell.ids = names(spatial_objects),project="merged")
    merged_spatial[["percent.mt"]] <- PercentageFeatureSet(merged_spatial, pattern = "^MT-")
    
    # Print basic information about the merged object
    Idents(merged_spatial) <- merged_spatial@meta.data$orig.ident
    table1 <- table(merged_spatial$orig.ident) %>% as.data.frame 
    colnames(table1) <- c("Sample names", "Cell counts")
    multiple_list <- SplitObject(merged_spatial, split.by = "orig.ident")
    
  }
  
  
  plots1 <- VlnPlot(merged_spatial, features = "nFeature_Spatial", ncol = 1)
  plots2 <- VlnPlot(merged_spatial, features = "nCount_Spatial", ncol = 1)
  plots3 <- VlnPlot(merged_spatial, features = "percent.mt", ncol = 1)
  plots4 <- SpatialFeaturePlot(merged_spatial, features = c("nFeature_Spatial", "nCount_Spatial", "percent.mt")) 
  plots5 <- FeatureScatter(merged_spatial, feature1 = "nFeature_Spatial", feature2 = "percent.mt")
  plots6 <- FeatureScatter(merged_spatial, feature1 = "nFeature_Spatial", feature2 = "nCount_Spatial")
  
  file_list <- list.files(index_multiple_sample_file1,recursive = TRUE)
  unique_files <- unique(file_list)
  
  return(list(plot1 = plots1 + plots2 + plots3, data1 = table1,  Plot3 = plots5 + plots6, text_summary = unique_files, data2 = merged_spatial, data3 = table1[,1], data4 = multiple_list, Plot2 = plots4))
}

