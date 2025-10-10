## ====== libraries ======
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(msigdbr)
  library(fgsea)
  library(ggplot2)
  library(gridExtra)
})

## ====== helpers ======

# Normalize gene symbol case for organism
.norm_symbols <- function(genes, species){
  if (species == "Homo sapiens") {
    genes <- toupper(genes)
  } else if (species == "Mus musculus") {
    genes <- stringr::str_to_title(genes)
  }
  genes
}

# Ensembl -> SYMBOL mapping if needed
.map_ensembl_to_symbol <- function(genes, species){
  is_ens <- grepl("^ENS[A-Z]*G\\d+", genes)
  if (!any(is_ens)) return(genes)

  suppressPackageStartupMessages({
    library(AnnotationDbi)
    if (species == "Homo sapiens") {
      library(org.Hs.eg.db)
      map <- AnnotationDbi::select(org.Hs.eg.db, keys = genes, keytype = "ENSEMBL",
                                   columns = c("SYMBOL"))
      keycol <- "ENSEMBL"; symcol <- "SYMBOL"
    } else {
      library(org.Mm.eg.db)
      map <- AnnotationDbi::select(org.Mm.eg.db, keys = genes, keytype = "ENSEMBL",
                                   columns = c("SYMBOL"))
      keycol <- "ENSEMBL"; symcol <- "SYMBOL"
    }
  })
  map <- map[!is.na(map[[symcol]]), ]
  first_map <- map[!duplicated(map[[keycol]]), ]
  sym_vec <- setNames(first_map[[symcol]], first_map[[keycol]])
  out <- ifelse(genes %in% names(sym_vec), sym_vec[genes], genes)
  as.character(out)
}

# Collapse duplicate symbols by keeping the max logFC
.collapse_ranks <- function(scores, genes){
  df <- data.frame(gene = genes, score = scores, stringsAsFactors = FALSE)
  agg <- stats::aggregate(score ~ gene, data = df, FUN = max)
  stats <- agg$score
  names(stats) <- agg$gene
  stats
}

## ====== main function ======

datainput_single_multiple_sample_gsea <- function(
  index_multiple_sample_gsea_input,
  index_subclustering_multiple_sample_gsea_input,
  index_multiple_sample_gsea_input2,
  index_subclustering_multiple_sample_gsea_input2,
  index_multiple_sample_gsea_input3,
  index_subclustering_multiple_sample_gsea_input3,
  index_s_gsea1, index_s_gsea2, index_s_gsea3, index_s_gsea4,
  index_s_gsea5, # "Homo sapiens" or "Mus musculus"
  index_s_gsea6, # msigdbr category (e.g., "H", "C2", "C5", etc.)
  index_s_gsea7, # fgsea scoreType ("std", "pos", "neg")
  index_s_gsea8, # minSize
  index_s_gsea9, # maxSize
  index_s_gsea10, # nPermSimple
  index_s_gsea11, # "GSEA_plot" | "barplot" | "plotGseaTable"
  index_s_gsea12  # number of top pathways to visualize (even number recommended)
){
  # ----------------- choose DE table & make base ranking -----------------
  if (index_s_gsea1 == "multiple_sample" & index_s_gsea2 == "seurat_clusters"){
    single_multiple_sample_clustering <- index_multiple_sample_gsea_input
    single_sample_clustering_markers <- index_multiple_sample_gsea_input3 
    deg_genes <- subset(single_sample_clustering_markers, (cluster == index_s_gsea3 & p_val_adj < index_s_gsea4))
    gene_ranking <- deg_genes$avg_log2FC
    names(gene_ranking) <- deg_genes$gene

  } else if (index_s_gsea1 == "multiple_sample_subclustering" & index_s_gsea2 == "seurat_clusters"){
    single_multiple_sample_clustering <- index_subclustering_multiple_sample_gsea_input 
    single_sample_clustering_markers <- index_subclustering_multiple_sample_gsea_input3 
    deg_genes <- subset(single_sample_clustering_markers, (cluster == index_s_gsea3 & p_val_adj < index_s_gsea4))
    gene_ranking <- deg_genes$avg_log2FC
    names(gene_ranking) <- deg_genes$gene

  } else if (index_s_gsea1 == "multiple_sample" & index_s_gsea2 == "predicted"){
    if (index_multiple_sample_gsea_input2 == "sctype_classification"){
      single_multiple_sample_clustering <- index_multiple_sample_gsea_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_gsea_input2
      single_sample_clustering_markers <- index_multiple_sample_gsea_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>%
        dplyr::select(seurat_clusters, sctype_classification) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df,
                                                by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers,
                          (sctype_classification == index_s_gsea3 & p_val_adj < index_s_gsea4))
      gene_ranking <- deg_genes$avg_log2FC; names(gene_ranking) <- deg_genes$gene

    } else if (index_multiple_sample_gsea_input2 == "singleR_labels"){
      single_multiple_sample_clustering <- index_multiple_sample_gsea_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_gsea_input2
      single_sample_clustering_markers <- index_multiple_sample_gsea_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>%
        dplyr::select(seurat_clusters, singleR_labels) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df,
                                                by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers,
                          (singleR_labels == index_s_gsea3 & p_val_adj < index_s_gsea4))
      gene_ranking <- deg_genes$avg_log2FC; names(gene_ranking) <- deg_genes$gene

    } else if (index_multiple_sample_gsea_input2 == "GPTCelltype"){
      single_multiple_sample_clustering <- index_multiple_sample_gsea_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_gsea_input2
      single_sample_clustering_markers <- index_multiple_sample_gsea_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>%
        dplyr::select(seurat_clusters, GPTCelltype) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df,
                                                by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers,
                          (GPTCelltype == index_s_gsea3 & p_val_adj < index_s_gsea4))
      gene_ranking <- deg_genes$avg_log2FC; names(gene_ranking) <- deg_genes$gene

    } else if (index_multiple_sample_gsea_input2 == "cell_type"){
      single_multiple_sample_clustering <- index_multiple_sample_gsea_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_gsea_input2
      single_sample_clustering_markers <- index_multiple_sample_gsea_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>%
        dplyr::select(seurat_clusters, cell_type) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df,
                                                by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers,
                          (cell_type == index_s_gsea3 & p_val_adj < index_s_gsea4))
      gene_ranking <- deg_genes$avg_log2FC; names(gene_ranking) <- deg_genes$gene
    }

  } else if (index_s_gsea1 == "multiple_sample_subclustering" & index_s_gsea2 == "predicted"){
    if (index_subclustering_multiple_sample_gsea_input2 == "sctype_classification"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_gsea_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_gsea_input2
      single_sample_clustering_markers <- index_subclustering_multiple_sample_gsea_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>%
        dplyr::select(seurat_clusters, sctype_classification) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df,
                                                by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers,
                          (sctype_classification == index_s_gsea3 & p_val_adj < index_s_gsea4))
      gene_ranking <- deg_genes$avg_log2FC; names(gene_ranking) <- deg_genes$gene

    } else if (index_subclustering_multiple_sample_gsea_input2 == "singleR_labels"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_gsea_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_gsea_input2
      single_sample_clustering_markers <- index_subclustering_multiple_sample_gsea_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>%
        dplyr::select(seurat_clusters, singleR_labels) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df,
                                                by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers,
                          (singleR_labels == index_s_gsea3 & p_val_adj < index_s_gsea4))
      gene_ranking <- deg_genes$avg_log2FC; names(gene_ranking) <- deg_genes$gene

    } else if (index_subclustering_multiple_sample_gsea_input2 == "GPTCelltype"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_gsea_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_gsea_input2
      single_sample_clustering_markers <- index_subclustering_multiple_sample_gsea_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>%
        dplyr::select(seurat_clusters, GPTCelltype) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df,
                                                by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers,
                          (GPTCelltype == index_s_gsea3 & p_val_adj < index_s_gsea4))
      gene_ranking <- deg_genes$avg_log2FC; names(gene_ranking) <- deg_genes$gene

    } else if (index_subclustering_multiple_sample_gsea_input2 == "cell_type"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_gsea_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_gsea_input2
      single_sample_clustering_markers <- index_subclustering_multiple_sample_gsea_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>%
        dplyr::select(seurat_clusters, cell_type) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df,
                                                by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers,
                          (cell_type == index_s_gsea3 & p_val_adj < index_s_gsea4))
      gene_ranking <- deg_genes$avg_log2FC; names(gene_ranking) <- deg_genes$gene
    }
  }

  # ----------------- organism-aware harmonization -----------------
  gene_names <- names(gene_ranking)
  gene_names <- .map_ensembl_to_symbol(gene_names, index_s_gsea5)
  gene_names <- .norm_symbols(gene_names, index_s_gsea5)

  keep <- !is.na(gene_names) & nzchar(gene_names) & !is.na(gene_ranking)
  gene_ranking <- gene_ranking[keep]
  gene_names <- gene_names[keep]

  gene_ranking <- .collapse_ranks(scores = as.numeric(gene_ranking), genes = gene_names)
  gene_ranking <- sort(gene_ranking, decreasing = TRUE)

  # ----------------- get gene sets -----------------
  # index_s_gsea6 should be an MSigDB category (e.g. "H", "C2", "C5", "C7", etc.)
  msigdb_gene_sets <- msigdbr(species = index_s_gsea5, category = index_s_gsea6)
  gene_sets <- split(msigdb_gene_sets$gene_symbol, msigdb_gene_sets$gs_name)

  # ----------------- run fgsea -----------------
  if (length(gene_sets) == 0 || length(gene_ranking) == 0) {
    empty_plot <- ggplot() + theme_void() + ggtitle("No genes or gene sets available")
    return(list(plot1 = empty_plot,
                data1 = data.frame()))
  }

  fgsea_results <- fgsea(pathways = gene_sets,
                         stats = gene_ranking,
                         scoreType = index_s_gsea7,
                         minSize = index_s_gsea8,
                         maxSize = index_s_gsea9,
                         nPermSimple = index_s_gsea10)

  if (nrow(fgsea_results) == 0) {
    empty_plot <- ggplot() + theme_void() + ggtitle("No enriched pathways found")
    return(list(plot1 = empty_plot,
                data1 = fgsea_results))
  }

  # ----------------- select top pathways (balanced up/down) -----------------
  half_n <- max(1, floor(index_s_gsea12 / 2))
  topPathwaysup   <- fgsea_results[ES > 0] %>% arrange(padj) %>% head(n = half_n)
  topPathwaysdown <- fgsea_results[ES < 0] %>% arrange(padj) %>% head(n = half_n)
  topPathways <- rbind(topPathwaysup, topPathwaysdown)

  if (nrow(topPathways) == 0) {
    empty_plot <- ggplot() + theme_void() + ggtitle("No pathways to visualize")
    fgsea_results$leadingEdge <- sapply(fgsea_results$leadingEdge, function(x) paste(unlist(x), collapse = ", "))
    return(list(plot1 = empty_plot, data1 = fgsea_results))
  }

  topPathways1Up   <- fgsea_results[ES > 0][head(order(padj), n = half_n), pathway]
  topPathways1Down <- fgsea_results[ES < 0][head(order(padj), n = half_n), pathway]
  topPathways1 <- c(topPathways1Up, rev(topPathways1Down))
  topPathways1 <- unique(topPathways1) # avoid duplicates

  # ----------------- plotting -----------------
  if (index_s_gsea11 == "GSEA_plot") {
    valid_paths <- topPathways1[!is.na(topPathways1) & topPathways1 %in% names(gene_sets)]
    n_show <- min(length(valid_paths), index_s_gsea12)

    plot_list <- lapply(seq_len(n_show), function(i) {
      gs <- gene_sets[[ valid_paths[i] ]]
      if (is.null(gs) || length(gs) == 0) return(NULL)
      p <- plotEnrichment(unname(gs), gene_ranking) + labs(title = valid_paths[i])
      if (!inherits(p, "ggplot")) return(NULL)
      p
    })
    plot_list <- Filter(function(x) inherits(x, "ggplot"), plot_list)

    if (length(plot_list) == 0) {
      plots501 <- ggplot() + theme_void() + ggtitle("No pathways to plot")
    } else {
      # pass a flat list of grobs; no nested list
      plots501 <- do.call(gridExtra::grid.arrange, plot_list)
    }

  } else if (index_s_gsea11 == "barplot") {
    topPathways$Significant <- ifelse(topPathways$ES > 0, "Positive", "Negative")
    plots501 <- ggplot(topPathways,
                       aes(reorder(pathway, ES), ES, fill = Significant)) +
      geom_col() + coord_flip() +
      labs(title = "Top Pathways Enriched",
           x = "Pathway", y = "Normalized Enrichment Score") +
      theme_bw()

  } else if (index_s_gsea11 == "plotGseaTable") {
    # guard for missing paths
    valid_paths <- topPathways1[!is.na(topPathways1) & topPathways1 %in% names(gene_sets)]
    if (length(valid_paths) == 0) {
      plots501 <- ggplot() + theme_void() + ggtitle("No pathways to plot")
    } else {
      plots501 <- plotGseaTable(gene_sets[valid_paths], gene_ranking, fgsea_results, gseaParam = 0.5)
    }

  } else {
    # default fallback
    plots501 <- ggplot() + theme_void() + ggtitle("Unknown plot type")
  }

  # prepare result table
  fgsea_results$leadingEdge <- sapply(fgsea_results$leadingEdge,
                                      function(x) paste(unlist(x), collapse = ", "))

  return(list(plot1 = plots501, data1 = fgsea_results))
}
