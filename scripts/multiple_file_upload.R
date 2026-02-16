datainput_multiple_sample <- function(index_multiple_sample_file,
                                      index_multiple_sample_file1,
                                      index_multiple_sample_file_names,
                                      index_multiple_sample_format,
                                      index_multiple_sample_name) {
  index_multiple_sample_format <- as.character(index_multiple_sample_format)
  
  # Save and restore working directory
  original_path <- getwd()
  on.exit(setwd(original_path), add = TRUE)
  
  library(Seurat)
  
  # Initialize output vars
  spatial_objects <- NULL
  path <- getwd()
  
  if (index_multiple_sample_format == "h5") {
    # Set working directory
    if (!is.null(index_multiple_sample_file1) && is.character(index_multiple_sample_file1)) {
      setwd(index_multiple_sample_file1)
    }
    
    data_dirs <- sub('\\.zip$', '', basename(index_multiple_sample_file_names))
    lapply(index_multiple_sample_file_names, unzip)
    
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
      obj[["RNA"]] <- CreateAssayObject(counts = obj@assays$Spatial$counts)
      DefaultAssay(obj) <- "RNA"
      return(obj)
    }
    
    try({
      spatial_objects <- mapply(load_spatial_data, data_dirs, SIMPLIFY = FALSE)
    }, silent = TRUE)
  }
  
  else if (index_multiple_sample_format == "MFB") {
    if (!is.null(index_multiple_sample_file1) && is.character(index_multiple_sample_file1)) {
      setwd(index_multiple_sample_file1)
    }
    
    data_dirs <- sub('\\.zip$', '', basename(index_multiple_sample_file_names))
    lapply(index_multiple_sample_file_names, unzip)
    
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
      
      obj[["RNA"]] <- CreateAssayObject(counts = obj@assays$Spatial$counts)
      DefaultAssay(obj) <- "RNA"
      return(obj)
    }
    
    try({
      spatial_objects <- mapply(load_spatial_data, data_dirs, SIMPLIFY = FALSE)
    }, silent = TRUE)
  }
  
  else if (index_multiple_sample_format == "exampledata") {
    index_multiple_sample_file1 <- "www/example_data/GSE230207"
    setwd(index_multiple_sample_file1)
    
    files <- list.files(pattern = "\\.zip$", full.names = TRUE)
    data_dirs <- sub('\\.zip$', '', basename(files))
    lapply(files, unzip)
    
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
      obj[["RNA"]] <- CreateAssayObject(counts = obj@assays$Spatial$counts)
      DefaultAssay(obj) <- "RNA"
      return(obj)
    }
    
    try({
      spatial_objects <- mapply(load_spatial_data, data_dirs, SIMPLIFY = FALSE)
    }, silent = TRUE)
  }
  
  # --- Validate spatial_objects before proceeding ---
  if (
    is.null(spatial_objects) ||
    !is.list(spatial_objects) ||
    length(spatial_objects) == 0 ||
    any(sapply(spatial_objects, function(x) is.null(x) || inherits(x, "try-error")))
  ) {
    return(list(
      is_valid = FALSE,
      text_summary = "❗ Please check your input file and refer to our example data format to ensure it is prepared correctly.❗"
    ))
  }
  
  # --- Proceed with downstream processing ---
  merged_spatial <- merge(spatial_objects[[1]], y = spatial_objects[-1],
                          add.cell.ids = names(spatial_objects), project = "merged")
  #merged_spatial[["percent.mt"]] <- PercentageFeatureSet(merged_spatial, pattern = "^MT-")
  mt_pat <- if (any(grepl("^MT-", rownames(merged_spatial[["Spatial"]]))) ) "^MT-" else "^mt-"
  merged_spatial[["percent.mt"]] <- PercentageFeatureSet(merged_spatial, pattern = mt_pat, assay = "Spatial")
  merged_spatial$percent.mt[is.na(merged_spatial$percent.mt)] <- 0
  Idents(merged_spatial) <- merged_spatial@meta.data$orig.ident
  
  table1 <- table(merged_spatial$orig.ident) %>% as.data.frame()
  colnames(table1) <- c("Sample names", "Cell counts")
  multiple_list <- SplitObject(merged_spatial, split.by = "orig.ident")
  
  plots1 <- VlnPlot(merged_spatial, features = "nFeature_Spatial", ncol = 1)
  plots2 <- VlnPlot(merged_spatial, features = "nCount_Spatial", ncol = 1)
  plots3 <- VlnPlot(merged_spatial, features = "percent.mt", ncol = 1)
  plots4 <- SpatialFeaturePlot(merged_spatial, features = c("nFeature_Spatial", "nCount_Spatial", "percent.mt")) 
  plots5 <- FeatureScatter(merged_spatial, feature1 = "nFeature_Spatial", feature2 = "percent.mt")
  plots6 <- FeatureScatter(merged_spatial, feature1 = "nFeature_Spatial", feature2 = "nCount_Spatial")
  
  file_list <- list.files(index_multiple_sample_file1, recursive = TRUE)
  unique_files <- unique(file_list)
  
  return(list(
    is_valid = TRUE,
    text_summary = unique(unique_files),
    plot1 = plots1 + plots2 + plots3,
    Plot3 = plots5 + plots6,
    data2 = merged_spatial,
    data3 = table1[, 1],
    data4 = multiple_list,
    Plot2 = plots4,
    data1 = table1
  ))
}
