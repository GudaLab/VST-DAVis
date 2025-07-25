## Check packages and install##
if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("DT")) install.packages("DT")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("shinyFiles")) install.packages("shinyFiles")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require('ggplotify')) install.packages("ggplotify")
if (!require("data.table")) install.packages("data.table")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tibble")) install.packages("tibble") 
if (!require("HGNChelper")) install.packages("HGNChelper") 
if (!require("openai")) install.packages("openai")
if (!require("metap")) install.packages("metap")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("R.utils")) install.packages("R.utils")
if (!require("circlize")) install.packages("circlize")
if (!require("hdf5r")) install.packages("hdf5r")
if (!require("ggupset")) install.packages("ggupset")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("ggalluvial")) install.packages("ggalluvial")
if (!require("NMF")) install.packages("NMF")
if (!require("ggraph")) install.packages("ggraph")
if (!require("igraph")) install.packages("igraph")
if (!require("cowplot"))install.packages("cowplot")
if (!require("pdftools"))install.packages("pdftools")
if (!require("xgboost"))install.packages("xgboost")
if (!require("msigdbr"))install.packages("msigdbr")
if (!require("msigdbdf"))install.packages("msigdbdf", repos = "https://igordot.r-universe.dev")
if (!require("BiocManager")) install.packages("BiocManager", update = FALSE)
if (!require("Seurat")) BiocManager::install("Seurat", update = FALSE)
if (!require("SeuratObject")) BiocManager::install("SeuratObject", update = FALSE)
if (!require("sctransform")) BiocManager::install("sctransform", update = FALSE)
if (!require("celldex")) BiocManager::install("celldex", update = FALSE)
if (!require("SingleR")) BiocManager::install("SingleR", update = FALSE)
if (!require("scRNAseq")) BiocManager::install("scRNAseq", update = FALSE)
if (!require("GenomicRanges"))BiocManager::install("GenomicRanges", update = FALSE)
if (!require("GPTCelltype")) BiocManager::install("Winnie09/GPTCelltype", update = FALSE)
if (!require("openxlsx")) BiocManager::install("ycphs/openxlsx", update = FALSE)
if (!require("glmGamPoi"))BiocManager::install("glmGamPoi", update = FALSE)
if (!require("presto"))BiocManager::install("immunogenomics/presto", update = FALSE)
if (!require("scran"))BiocManager::install("scran", update = FALSE)
if (!require("EnhancedVolcano"))BiocManager::install("EnhancedVolcano", update = FALSE)
if (!require("monocle3"))BiocManager::install("cole-trapnell-lab/monocle3", update = FALSE)
if (!require("SeuratWrappers")) BiocManager::install("satijalab/seurat-wrappers", update = FALSE)
if (!require("SeuratDisk")) BiocManager::install("mojaveazure/seurat-disk", update = FALSE)
if (!require("ComplexHeatmap")) BiocManager::install("ComplexHeatmap", update = FALSE)
if (!require("patchwork")) BiocManager::install("thomasp85/patchwork", update = FALSE)
if (!require("clusterProfiler"))BiocManager::install("clusterProfiler", update = FALSE)
if (!require("org.Hs.eg.db"))BiocManager::install("org.Hs.eg.db", update = FALSE)
if (!require("org.Mm.eg.db"))BiocManager::install("org.Mm.eg.db", update = FALSE)
if (!require("org.Mmu.eg.db"))BiocManager::install("org.Mmu.eg.db", update = FALSE)
if (!require("org.Rn.eg.db"))BiocManager::install("org.Rn.eg.db", update = FALSE)
if (!require("org.Ss.eg.db"))BiocManager::install("org.Ss.eg.db", update = FALSE)
if (!require("ReactomePA"))BiocManager::install("ReactomePA", update = FALSE)
if (!require("fgsea"))BiocManager::install("fgsea", update = FALSE)
if (!require("enrichplot"))BiocManager::install("enrichplot", update = FALSE)
if (!require("CellChat"))BiocManager::install("jinworks/CellChat", update = FALSE)
if (!require("multtest"))BiocManager::install("multtest", update = FALSE)
if (!require("genesorteR"))BiocManager::install("mahmoudibrahim/genesorteR", update = FALSE)
if (!require("WGCNA"))BiocManager::install("WGCNA", update = FALSE)
if (!require("hdWGCNA"))BiocManager::install("smorabit/hdWGCNA", update = FALSE)
# if (!require("motifmatchr"))BiocManager::install("motifmatchr", update = FALSE)
# if (!require("TFBSTools"))BiocManager::install("TFBSTools", update = FALSE)
if (!require("JASPAR2020"))BiocManager::install("JASPAR2020", update = FALSE)
if (!require("JASPAR2024"))BiocManager::install("JASPAR2024", update = FALSE)
if (!require("EnsDb.Hsapiens.v86"))BiocManager::install("EnsDb.Hsapiens.v86", update = FALSE)
if (!require("EnsDb.Mmusculus.v79"))BiocManager::install("EnsDb.Mmusculus.v79", update = FALSE)
if (!require("BSgenome.Hsapiens.UCSC.hg38"))BiocManager::install("BSgenome.Hsapiens.UCSC.hg38", update = FALSE)
if (!require("BSgenome.Mmusculus.UCSC.mm10"))BiocManager::install("BSgenome.Mmusculus.UCSC.mm10", update = FALSE)
source("scripts/PrctCellExpringGene.R")
options(shiny.maxRequestSize=2000*1024^2)
options(future.globals.maxSize= 925289600000)
Sys.setenv(OPENAI_API_KEY = 'ADD key here')  #Add your key here

if (.Platform$OS.type == "windows") {
  # For Windows
  cache_path <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache", "R", "BiocFileCache")
} else {
  # For Linux/macOS
  cache_path <- file.path(Sys.getenv("HOME"), ".cache", "R", "BiocFileCache")
}

# Create directory if it doesn't exist
if (!dir.exists(cache_path)) {
  dir.create(cache_path, recursive = TRUE, showWarnings = FALSE)
}

# Set BiocFileCache directory environment variable
Sys.setenv("BIOCFILECACHE_DIR" = cache_path)



# URL of the zip file
zip_url <- "https://www.gudalab-rtools.net/example_data.zip"

# Define target directory and subdirectory
target_dir <- file.path("www")
example_data_dir <- file.path(target_dir, "example_data")

# Check if example_data already exists
if (!dir.exists(example_data_dir)) {
  # Create www folder if it doesn't exist
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
  # Path for the downloaded zip file
  zip_file <- tempfile(fileext = ".zip")
  
  # Download the zip file
  download.file(zip_url, zip_file, mode = "wb")
  
  # Extract the zip file into the www folder
  unzip(zip_file, exdir = target_dir)
  
  # Remove the zip file after extraction
  file.remove(zip_file)
  
  cat("Files extracted to:", target_dir, "\n")
} else {
  cat("example_data folder already exists. Skipping download.\n")
}


