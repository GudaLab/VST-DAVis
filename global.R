# ## Check packages and install ##
if (!exists("vstdavis_valid_paths", mode = "function", inherits = TRUE)) {
  vstdavis_valid_paths <- function(x) {
    if (is.null(x) || !length(x)) {
      return(character())
    }
    x <- as.character(x)
    x[!is.na(x) & nzchar(x)]
  }
}

if (!exists("vstdavis_normalize_dir", mode = "function", inherits = TRUE)) {
  vstdavis_normalize_dir <- function(path) {
    path <- vstdavis_valid_paths(path)
    if (!length(path)) {
      return(character())
    }
    normalized <- tryCatch(
      suppressWarnings(normalizePath(path[[1]], winslash = "/", mustWork = FALSE)),
      error = function(e) path[[1]]
    )
    vstdavis_valid_paths(normalized)
  }
}

if (!exists("vstdavis_has_scripts_dir", mode = "function", inherits = TRUE)) {
  vstdavis_has_scripts_dir <- function(path) {
    path <- vstdavis_valid_paths(path)
    if (!length(path)) {
      return(FALSE)
    }
    isTRUE(dir.exists(file.path(path[[1]], "scripts")))
  }
}

if (!exists("vstdavis_find_app_dir", mode = "function", inherits = TRUE)) {
  vstdavis_find_app_dir <- function() {
    frame_paths <- unlist(lapply(sys.frames(), function(frame) {
      vstdavis_valid_paths(frame$ofile)
    }), use.names = FALSE)

    app_paths <- frame_paths[grepl("server\\.R$|global\\.R$", frame_paths, ignore.case = TRUE)]
    candidate_dirs <- vstdavis_valid_paths(dirname(app_paths))
    current_root <- tryCatch(getwd(), error = function(e) character())
    candidate_dirs <- unique(c(candidate_dirs, vstdavis_valid_paths(current_root), "."))

    for (candidate in candidate_dirs) {
      normalized <- vstdavis_normalize_dir(candidate)
      if (length(normalized) && vstdavis_has_scripts_dir(normalized)) {
        return(normalized[[1]])
      }
    }

    fallback <- vstdavis_normalize_dir(vstdavis_valid_paths(current_root))
    if (length(fallback)) fallback[[1]] else "."
  }
}

vstdavis_app_dir <- vstdavis_find_app_dir()

if (!exists("vstdavis_app_file", mode = "function", inherits = TRUE)) {
  vstdavis_app_file <- function(...) {
    root <- vstdavis_valid_paths(vstdavis_app_dir)
    if (!length(root) || !vstdavis_has_scripts_dir(root)) {
      root <- vstdavis_valid_paths(vstdavis_find_app_dir())
    }
    if (!length(root)) {
      root <- "."
    }
    normalizePath(file.path(root, ...), winslash = "/", mustWork = FALSE)
  }
}

if (!exists("vstdavis_safe_getwd", mode = "function", inherits = TRUE)) {
  vstdavis_safe_getwd <- function() {
    current <- tryCatch(getwd(), error = function(e) character())
    current <- vstdavis_valid_paths(current)
    if (length(current) && isTRUE(dir.exists(current[[1]]))) {
      return(normalizePath(current[[1]], winslash = "/", mustWork = TRUE))
    }

    root <- vstdavis_valid_paths(vstdavis_app_dir)
    if (length(root) && isTRUE(dir.exists(root[[1]]))) {
      return(root[[1]])
    }

    tempdir()
  }
}

if (!exists("vstdavis_restore_wd", mode = "function", inherits = TRUE)) {
  vstdavis_restore_wd <- function(path) {
    path <- vstdavis_valid_paths(path)
    if (length(path) && isTRUE(dir.exists(path[[1]]))) {
      tryCatch(setwd(path[[1]]), error = function(e) NULL)
    }
    invisible(NULL)
  }
}

if (!exists("source_app_script", mode = "function", inherits = TRUE)) {
  source_app_script <- function(script_path, local = parent.frame()) {
    resolved_path <- if (grepl("^[A-Za-z]:|^/", script_path)) script_path else vstdavis_app_file(script_path)
    if (!file.exists(resolved_path)) {
      stop("Required script file not found: ", resolved_path)
    }
    source(resolved_path, local = local)
  }
}
# options(repos = c(CRAN = "https://cloud.r-project.org"))
# 
# if (.Platform$OS.type == "windows") {
#   r_minor <- strsplit(R.version$minor, "\\.")[[1]][1]
#   user_lib <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", paste(R.version$major, r_minor, sep = "."))
#   if (!dir.exists(user_lib)) {
#     dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
#   }
#   .libPaths(c(user_lib, .libPaths()))
# }
# 
# is_missing_pkg <- function(pkg) {
#   !requireNamespace(pkg, quietly = TRUE)
# }
# 
# install_cran_if_missing <- function(pkg, ..., repos = getOption("repos")) {
#   if (is_missing_pkg(pkg)) {
#     install.packages(pkg, repos = repos, dependencies = TRUE, ...)
#   }
# }
# 
# install_bioc_if_missing <- function(pkg, ...) {
#   install_cran_if_missing("BiocManager")
#   if (is_missing_pkg(pkg)) {
#     BiocManager::install(pkg, ask = FALSE, update = FALSE, ...)
#   }
# }
# 
# install_github_if_missing <- function(pkg, repo, ...) {
#   install_cran_if_missing("remotes")
#   if (is_missing_pkg(pkg)) {
#     remotes::install_github(repo, upgrade = "never", dependencies = TRUE, ...)
#   }
# }
# 
# cran_pkgs <- c(
#   "shiny", "DT", "shinythemes", "shinyjs", "shinyFiles", "shinyWidgets",
#   "shinycssloaders", "ggplot2", "devtools", "ggplotify", "data.table",
#   "ggpubr", "shinydashboard", "dplyr", "tibble", "pheatmap", "HGNChelper", "openai",
#   "metap", "ggrepel", "R.utils", "circlize", "hdf5r", "ggupset",
#   "gridExtra", "ggalluvial", "NMF", "filelock", "ggraph", "igraph",
#   "cowplot", "pdftools", "xgboost", "Seurat", "arrow", "msigdbr",
#   "SeuratObject", "openxlsx", "WGCNA", "patchwork"
# )
# 
# bioc_pkgs <- c(
#   "sctransform", "celldex", "SingleR", "scRNAseq", "GenomicRanges",
#   "glmGamPoi", "scran", "EnhancedVolcano", "ComplexHeatmap",
#   "clusterProfiler", "org.Hs.eg.db", "org.Mm.eg.db", "org.Mmu.eg.db",
#   "org.Rn.eg.db", "org.Ss.eg.db", "ReactomePA", "fgsea", "enrichplot",
#   "multtest", "JASPAR2020", "JASPAR2024", "EnsDb.Hsapiens.v86",
#   "EnsDb.Mmusculus.v79", "BSgenome.Hsapiens.UCSC.hg38",
#   "BSgenome.Mmusculus.UCSC.mm10"
# )
# 
# for (pkg in cran_pkgs) {
#   install_cran_if_missing(pkg)
# }
# 
# if (is_missing_pkg("msigdf")) {
#   install.packages(
#     "msigdf",
#     repos = c(getOption("repos"), igordot = "https://igordot.r-universe.dev")
#   )
# }
# 
# for (pkg in bioc_pkgs) {
#   install_bioc_if_missing(pkg)
# }
# 
# github_pkgs <- list(
#   GPTCelltype = "Winnie09/GPTCelltype",
#   presto = "immunogenomics/presto",
#   monocle3 = "cole-trapnell-lab/monocle3",
#   SeuratWrappers = "satijalab/seurat-wrappers",
#   SeuratDisk = "mojaveazure/seurat-disk",
#   CellChat = "jinworks/CellChat",
#   genesorteR = "mahmoudibrahim/genesorteR",
#   hdWGCNA = "smorabit/hdWGCNA"
# )
# 
# for (pkg in names(github_pkgs)) {
#   install_github_if_missing(pkg, github_pkgs[[pkg]])
# }
# 
# runtime_attach_pkgs <- c(
#   "shiny", "DT", "shinythemes", "shinyjs", "shinyFiles", "shinyWidgets",
#   "shinycssloaders", "shinydashboard", "ggplot2", "patchwork", "dplyr",
#   "data.table", "reshape2", "Seurat", "SeuratObject", "filelock",
#   "SingleR", "celldex", "HGNChelper", "GPTCelltype", "EnhancedVolcano", "clusterProfiler",
#   "ReactomePA", "fgsea", "enrichplot", "ComplexHeatmap", "hdWGCNA",
#   "WGCNA", "CellChat", "NMF", "monocle3", "SeuratWrappers", "SeuratDisk",
#   "msigdbr", "ggraph", "igraph", "cowplot", "gridExtra", "ggrepel", "ggalluvial",
#   "circlize", "R.utils", "openxlsx", "arrow"
# )
# 
# for (pkg in runtime_attach_pkgs) {
#   suppressPackageStartupMessages(require(pkg, character.only = TRUE, quietly = TRUE))
# }
## Check packages and install ##
## CRAN helpers ##
if (!require("BiocManager")) install.packages("BiocManager", dependencies = TRUE)
if (!require("remotes")) install.packages("remotes", dependencies = TRUE)
if (!require("remotes")) install.packages("remotes", dependencies = TRUE)
## CRAN packages ##
if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("DT")) install.packages("DT")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("shinyFiles")) install.packages("shinyFiles")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("devtools")) install.packages("devtools")
if (!require("ggplotify")) install.packages("ggplotify")
if (!require("data.table")) install.packages("data.table")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tibble")) install.packages("tibble")
if (!require("pheatmap")) install.packages("pheatmap")
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
if (!require("filelock")) install.packages("filelock")
if (!require("ggraph")) install.packages("ggraph")
if (!require("igraph")) install.packages("igraph")
if (!require("cowplot")) install.packages("cowplot")
if (!require("pdftools")) install.packages("pdftools")
if (!require("xgboost")) install.packages("xgboost")
if (!require("Seurat")) install.packages("Seurat")
if (!require("arrow")) install.packages("arrow")
if (!require("msigdbr")) install.packages("msigdbr")
if (!require("SeuratObject")) install.packages("SeuratObject")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("WGCNA")) install.packages("WGCNA")
if (!require("patchwork")) install.packages("patchwork")
if (!require("harmony")) install.packages("harmony")
if (!require("reshape2")) install.packages("reshape2")
#if (!require("msigdf"))install.packages("msigdf", repos = "https://cloud.r-project.org")
## Bioconductor packages ##
if (!require("sctransform")) BiocManager::install("sctransform", ask = FALSE, update = FALSE)
if (!require("celldex")) BiocManager::install("celldex", ask = FALSE, update = FALSE)
if (!require("SingleR")) BiocManager::install("SingleR", ask = FALSE, update = FALSE)
if (!require("scRNAseq")) BiocManager::install("scRNAseq", ask = FALSE, update = FALSE)
if (!require("GenomicRanges")) BiocManager::install("GenomicRanges", ask = FALSE, update = FALSE)
if (!require("glmGamPoi")) BiocManager::install("glmGamPoi", ask = FALSE, update = FALSE)
if (!require("scran")) BiocManager::install("scran", ask = FALSE, update = FALSE)
if (!require("EnhancedVolcano")) BiocManager::install("EnhancedVolcano", ask = FALSE, update = FALSE)
if (!require("ComplexHeatmap")) BiocManager::install("ComplexHeatmap", ask = FALSE, update = FALSE)
if (!require("clusterProfiler")) BiocManager::install("clusterProfiler", ask = FALSE, update = FALSE)
if (!require("org.Hs.eg.db")) BiocManager::install("org.Hs.eg.db", ask = FALSE, update = FALSE)
if (!require("org.Mm.eg.db")) BiocManager::install("org.Mm.eg.db", ask = FALSE, update = FALSE)
if (!require("org.Mmu.eg.db")) BiocManager::install("org.Mmu.eg.db", ask = FALSE, update = FALSE)
if (!require("org.Rn.eg.db")) BiocManager::install("org.Rn.eg.db", ask = FALSE, update = FALSE)
if (!require("org.Ss.eg.db")) BiocManager::install("org.Ss.eg.db", ask = FALSE, update = FALSE)
if (!require("ReactomePA")) BiocManager::install("ReactomePA", ask = FALSE, update = FALSE)
if (!require("fgsea")) BiocManager::install("fgsea", ask = FALSE, update = FALSE)
if (!require("enrichplot")) BiocManager::install("enrichplot", ask = FALSE, update = FALSE)
if (!require("multtest")) BiocManager::install("multtest", ask = FALSE, update = FALSE)
if (!require("JASPAR2020")) BiocManager::install("JASPAR2020", ask = FALSE, update = FALSE)
if (!require("JASPAR2024")) BiocManager::install("JASPAR2024", ask = FALSE, update = FALSE)
if (!require("EnsDb.Hsapiens.v86")) BiocManager::install("EnsDb.Hsapiens.v86", ask = FALSE, update = FALSE)
if (!require("EnsDb.Mmusculus.v79")) BiocManager::install("EnsDb.Mmusculus.v79", ask = FALSE, update = FALSE)
if (!require("BSgenome.Hsapiens.UCSC.hg38")) BiocManager::install("BSgenome.Hsapiens.UCSC.hg38", ask = FALSE, update = FALSE)
if (!require("BSgenome.Mmusculus.UCSC.mm10")) BiocManager::install("BSgenome.Mmusculus.UCSC.mm10", ask = FALSE, update = FALSE)
## GitHub packages ##
if (!require("GPTCelltype")) remotes::install_github("Winnie09/GPTCelltype", upgrade = "never", dependencies = TRUE)
if (!require("presto")) remotes::install_github("immunogenomics/presto", upgrade = "never", dependencies = TRUE)
if (!require("monocle3")) remotes::install_github("cole-trapnell-lab/monocle3", upgrade = "never", dependencies = TRUE)
if (!require("SeuratWrappers")) remotes::install_github("satijalab/seurat-wrappers", upgrade = "never", dependencies = TRUE)
if (!require("SeuratDisk")) remotes::install_github("mojaveazure/seurat-disk", upgrade = "never", dependencies = TRUE)
if (!require("CellChat")) remotes::install_github("jinworks/CellChat", upgrade = "never", dependencies = TRUE)
if (!require("genesorteR")) remotes::install_github("mahmoudibrahim/genesorteR", upgrade = "never", dependencies = TRUE)
if (!require("hdWGCNA")) remotes::install_github("smorabit/hdWGCNA", upgrade = "never", dependencies = TRUE)
Sys.setenv(OPENAI_API_KEY = '')  #Add your key here


source_app_script("scripts/PrctCellExpringGene.R")
options(shiny.maxRequestSize=3000*1024^2)
options(future.globals.maxSize= 925289600000)

if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  message("OPENAI_API_KEY is not set. GPTCelltype will remain unavailable until you set it in the R session or environment.")
}

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

  tryCatch({
    # Download the zip file
    download.file(zip_url, zip_file, mode = "wb")

    # Extract the zip file into the www folder
    unzip(zip_file, exdir = target_dir)

    # Remove the zip file after extraction
    file.remove(zip_file)

    cat("Files extracted to:", target_dir, "\n")
  }, error = function(e) {
    message("Example data download skipped: ", conditionMessage(e))
  })
} else {
  cat("example_data folder already exists. Skipping download.\n")
}



#views
vstdavis_prepare_runtime_dir <- function() {
  runtime_candidates <- c(
    if (vstdavis_has_scripts_dir(vstdavis_app_dir)) file.path(vstdavis_app_dir, "www", ".runtime") else character(),
    file.path(tempdir(), "VST-DAVis-runtime")
  )

  for (runtime_dir in runtime_candidates) {
    runtime_dir <- vstdavis_valid_paths(runtime_dir)
    if (!length(runtime_dir)) {
      next
    }
    runtime_dir <- runtime_dir[[1]]
    if (!dir.exists(runtime_dir)) {
      dir.create(runtime_dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (isTRUE(dir.exists(runtime_dir))) {
      return(runtime_dir)
    }
  }

  tempdir()
}

runtime_dir <- vstdavis_prepare_runtime_dir()
count_file <- file.path(runtime_dir, "view_counter.rds")
lock_file  <- file.path(runtime_dir, "view_counter.lock")

ensure_count_file <- function() {
  count_dir <- dirname(count_file)
  if (!dir.exists(count_dir)) {
    dir.create(count_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(count_file)) {
    saveRDS(0L, count_file)
  }
}

ensure_count_file()

read_count <- function() {
  ensure_count_file()
  tryCatch(readRDS(count_file), error = function(e) 0L)
}

increment_count <- function() {
  ensure_count_file()
  lock <- tryCatch(filelock::lock(lock_file, timeout = 5000), error = function(e) NULL)
  if (!is.null(lock)) {
    on.exit(filelock::unlock(lock), add = TRUE)
  }

  n <- read_count()
  n <- suppressWarnings(as.integer(n))
  if (is.na(n)) {
    n <- 0L
  }
  n <- n + 1L
  saveRDS(n, count_file)
  n
}
