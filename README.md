# VST-DAVis

VST-DAVis is a user-friendly, browser-based R Shiny application designed for researchers without programming expertise to analyze and visualize 10x Genomics Spatial Transcriptomics Visium HD. It supports single and multiple sample analyses as well as group comparisons, offering a range of functionalities for comprehensive data exploration.

## Features

VST-DAVis includes nine functional modules:
1. **Single or Multiple Samples Analysis**
   - Stats
   - Sample Groups and QC Filtering
   - Normalization and PCA Analysis
   - Clustering
   - Marker Identification
   - Cell Type Prediction
   - Cluster-Based Plots
   - Condition-Based Analysis
2. **Subclustering**
3. **Correlation Network Analysis**
4. **Genome Ontology (GO) Terms**
5. **Pathway Analysis**
6. **GSEA Analysis**
7. **Cell-Cell Communication**
8. **Trajectory and Pseudotime Analysis**
9. **Co-Expression and TF Analysis**
   - Co-Expression Network Analysis
   - Transcription Factor Regulatory Network Analysis

## Use VST-DAVis Online
VST-DAVis is deployed online and accessible at:  
**[https://www.gudalab-rtools.net/VST-DAVis](https://www.gudalab-rtools.net/VST-DAVis)**

## Launch VST-DAVis Locally

### Prerequisites
Ensure the following software is installed:
- **R** (>= 4.4.3): [Download R](https://www.r-project.org/)
- **RStudio** (>= 2024.12.0): [Download RStudio](https://posit.co/download/rstudio-desktop/)
- **Bioconductor** (>= 3.20)
- **Shiny** (>= 1.10.0)

**Note:** VST-DAVis has been tested with these versions. Using older versions of R may cause errors during package installation. Updating to the latest R version is recommended.

### Installation

Run the following commands in an R session to install required packages:

```R
if (!require("BiocManager")) install.packages("BiocManager", update = FALSE)
if (!require("devtools")) install.packages("devtools", update = FALSE)

# Function to check and install CRAN packages
install_cran_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
  }
}

# Function to check and install Bioconductor packages
install_bioc_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      BiocManager::install(pkg, update = FALSE, dependencies = TRUE)
    }
  }
}

# Function to check and install GitHub packages
install_github_packages <- function(packages) {
  for (pkg in names(packages)) {
    if (!require(pkg, character.only = TRUE)) {
      devtools::install_github(packages[[pkg]], upgrade = "never", dependencies = TRUE)
    }
  }
}

# List of CRAN packages
cran_packages <- c("shiny", "DT", "shinythemes", "shinyjs", "shinyFiles", "shinyWidgets", "shinycssloaders", "ggplot2", "data.table", "ggpubr", "shinydashboard", "dplyr", "tibble", "HGNChelper", "openai", "metap", "ggrepel", "R.utils", "circlize", "hdf5r", "ggupset", "gridExtra", "ggalluvial", "NMF", "ggraph", "igraph", "cowplot", "pdftools", "xgboost", "msigdbr")

if (!require("msigdbdf"))install.packages("msigdbdf", repos = "https://igordot.r-universe.dev")

# List of Bioconductor packages
bioc_packages <- c("Seurat", "SeuratObject", "sctransform", "celldex", "SingleR", "scRNAseq", "glmGamPoi", "scran", "EnhancedVolcano", "ComplexHeatmap", "clusterProfiler", "org.Hs.eg.db", "org.Mm.eg.db", "org.Mmu.eg.db", "org.Rn.eg.db", "org.Ss.eg.db", "ReactomePA", "fgsea", "enrichplot", "multtest", "WGCNA", "hdWGCNA", "motifmatchr", "TFBSTools", "GenomicRanges", "JASPAR2020", "EnsDb.Hsapiens.v86", "BSgenome.Hsapiens.UCSC.hg38", "BSgenome.Mmusculus.UCSC.mm10")

# List of GitHub packages
github_packages <- list(
  "GPTCelltype" = "Winnie09/GPTCelltype",
  "openxlsx" = "ycphs/openxlsx",
  "presto" = "immunogenomics/presto",
  "monocle3" = "cole-trapnell-lab/monocle3",
  "SeuratWrappers" = "satijalab/seurat-wrappers",
  "SeuratDisk" = "mojaveazure/seurat-disk",
  "patchwork" = "thomasp85/patchwork",
  "CellChat" = "jinworks/CellChat",
  "genesorteR" = "mahmoudibrahim/genesorteR",
  "hdWGCNA" = "smorabit/hdWGCNA"
)

# Install all packages
install_cran_packages(cran_packages)
install_bioc_packages(bioc_packages)
install_github_packages(github_packages)
```
## Start the App

To launch VST-DAVis, follow one of these methods:

#### Option 1: Run Directly from GitHub
1. Open an R session in **RStudio**.
2. Execute the following lines of code:

```R
library(shiny)
shiny::runGitHub('VST-DAVis', 'GudaLab')
```

#### Option 2: Download the source code from GitHub and run:
```
library(shiny)
runApp('/path/to/the/VST-DAVis-master', launch.browser = TRUE)
```
Replace /path/to/the/VST-DAVis-master with the actual path to the downloaded folder
## Usage

A detailed user manual is available under the "Manual" tab at: [https://www.gudalab-rtools.net/VST-DAVis](https://www.gudalab-rtools.net/VST-DAVis)

## Example Datasets

To ensure seamless analysis and reproducibility, **VST-DAVis** includes one reference dataset for each input format. These datasets, sourced from **NCBI**, have been pre-tested with the tool and allow users to explore its functionalities and understand the analysis workflow effectively.

#### Available Datasets:
- **H5 File**: [GSE230207](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE230207)
- **Matrix Files**: [GSE244014](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE244014)

These datasets are ideal for:
- Demonstrating VST-DAVis functionalities.
- Familiarizing users with the tool's analysis workflow.
- Testing the application in different input formats.

Users can download these datasets directly from the provided links and use them to explore VST-DAVis.

## Tested Platforms

This application was tested on: Linux (Red Hat and Ubuntu) and Windows (10 and 11)
