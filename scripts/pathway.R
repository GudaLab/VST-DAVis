datainput_single_multiple_sample_pathway<- function(index_multiple_sample_pathway_input, index_subclustering_multiple_sample_pathway_input, index_multiple_sample_pathway_input2, index_subclustering_multiple_sample_pathway_input2, index_multiple_sample_pathway_input3, index_subclustering_multiple_sample_pathway_input3, index_s_pathway1, index_s_pathway2, index_s_pathway3, index_s_pathway4, index_s_pathway5, index_s_pathway6, index_s_pathway7, index_s_pathway8, index_s_pathway9, index_s_pathway10, index_s_pathway11, index_s_pathway12, index_s_pathway13, index_s_pathway14){
  make_info_plot <- function(label_text) {
    ggplot2::ggplot(data.frame(x = 1, y = 1, label = label_text), ggplot2::aes(x = x, y = y, label = label)) +
      ggplot2::geom_text(lineheight = 1.1, size = 4) +
      ggplot2::theme_void() +
      ggplot2::xlim(0.5, 1.5) +
      ggplot2::ylim(0.5, 1.5)
  }

  sanitize_gene_vector <- function(genes) {
    genes <- trimws(as.character(genes))
    genes <- genes[!is.na(genes) & nzchar(genes)]
    unique(genes)
  }

  sanitize_enrich_result <- function(enrich_obj) {
    res <- as.data.frame(enrich_obj@result)
    if (nrow(res) == 0) {
      enrich_obj@result <- res
      return(enrich_obj)
    }

    if ("geneID" %in% colnames(res)) {
      res$geneID <- vapply(strsplit(as.character(res$geneID), "/", fixed = TRUE), function(ids) {
        ids <- trimws(ids)
        ids <- ids[!is.na(ids) & nzchar(ids) & ids != "NA"]
        paste(unique(ids), collapse = "/")
      }, character(1))
      res <- res[!is.na(res$geneID) & nzchar(res$geneID), , drop = FALSE]
    }

    res <- res[!is.na(res$Description) & nzchar(res$Description), , drop = FALSE]
    if (nrow(res) == 0) {
      enrich_obj@result <- res
      return(enrich_obj)
    }

    row_id <- if ("ID" %in% colnames(res)) res$ID else res$Description
    row_id[is.na(row_id) | !nzchar(row_id)] <- paste0("term_", seq_len(sum(is.na(row_id) | !nzchar(row_id))))
    rownames(res) <- make.unique(as.character(row_id))
    enrich_obj@result <- res
    enrich_obj
  }

  make_barplot <- function(enrich_obj, show_n) {
    res <- as.data.frame(enrich_obj@result)
    if (nrow(res) == 0) {
      return(make_info_plot("No pathway terms are available to draw a bar plot."))
    }

    rank_col <- if ("p.adjust" %in% colnames(res)) "p.adjust" else if ("pvalue" %in% colnames(res)) "pvalue" else "Count"
    res <- res[order(res[[rank_col]], decreasing = FALSE, na.last = TRUE), , drop = FALSE]
    res <- head(res, show_n)

    count_values <- suppressWarnings(as.numeric(as.character(res$Count)))
    if (all(is.na(count_values)) && "GeneRatio" %in% colnames(res)) {
      count_values <- vapply(strsplit(as.character(res$GeneRatio), "/", fixed = TRUE), function(x) {
        x <- suppressWarnings(as.numeric(x))
        if (length(x) == 2 && all(is.finite(x)) && x[2] != 0) x[1] / x[2] else NA_real_
      }, numeric(1))
    }
    if (all(is.na(count_values))) {
      count_values <- seq_len(nrow(res))
    }

    fill_values <- if ("p.adjust" %in% colnames(res)) -log10(pmax(res$p.adjust, .Machine$double.xmin)) else count_values
    plot_df <- data.frame(
      Description = factor(res$Description, levels = rev(res$Description)),
      Value = count_values,
      Fill = fill_values
    )

    ggplot2::ggplot(plot_df, ggplot2::aes(x = Description, y = Value, fill = Fill)) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::labs(x = NULL, y = "Gene count", fill = if ("p.adjust" %in% colnames(res)) "-log10(adj p)" else "Value")
  }

  get_top_enrichment_terms <- function(enrich_obj, show_n) {
    res <- as.data.frame(enrich_obj@result)
    if (nrow(res) == 0) {
      return(res)
    }

    show_n <- suppressWarnings(as.integer(show_n))
    if (!is.finite(show_n) || show_n <= 0) {
      show_n <- 10L
    }

    rank_col <- if ("p.adjust" %in% colnames(res)) "p.adjust" else if ("pvalue" %in% colnames(res)) "pvalue" else if ("Count" %in% colnames(res)) "Count" else colnames(res)[1]
    decreasing <- identical(rank_col, "Count")
    res <- res[order(res[[rank_col]], decreasing = decreasing, na.last = TRUE), , drop = FALSE]
    head(res, show_n)
  }

  extract_term_gene_edges <- function(enrich_obj, show_n) {
    res <- get_top_enrichment_terms(enrich_obj, show_n)
    if (nrow(res) == 0 || !"geneID" %in% colnames(res)) {
      return(data.frame())
    }

    edge_list <- lapply(seq_len(nrow(res)), function(i) {
      genes <- trimws(unlist(strsplit(as.character(res$geneID[i]), "/", fixed = TRUE)))
      genes <- genes[!is.na(genes) & nzchar(genes)]
      if (!length(genes)) {
        return(NULL)
      }

      data.frame(
        term = as.character(res$Description[i]),
        gene = unique(genes),
        stringsAsFactors = FALSE
      )
    })

    edge_df <- do.call(rbind, edge_list)
    if (is.null(edge_df) || !nrow(edge_df)) {
      return(data.frame())
    }

    unique(edge_df)
  }

  make_cnetplot_fallback <- function(enrich_obj, show_n) {
    edges <- extract_term_gene_edges(enrich_obj, show_n)
    if (!nrow(edges)) {
      return(make_info_plot("No gene-term connections are available for the selected pathways."))
    }

    gene_frequency <- sort(table(edges$gene), decreasing = TRUE)
    show_n_int <- suppressWarnings(as.integer(show_n))
    if (!is.finite(show_n_int) || show_n_int <= 0) {
      show_n_int <- 10L
    }
    keep_gene_n <- min(length(gene_frequency), max(12L, show_n_int * 4L))
    keep_genes <- names(gene_frequency)[seq_len(keep_gene_n)]
    edges <- edges[edges$gene %in% keep_genes, , drop = FALSE]
    if (!nrow(edges)) {
      return(make_info_plot("The selected pathways did not contain enough gene connections to draw a network."))
    }

    top_terms <- unique(edges$term)
    top_genes <- names(sort(table(edges$gene), decreasing = TRUE))
    term_positions <- data.frame(term = rev(top_terms), x = 1, y = seq_along(rev(top_terms)), stringsAsFactors = FALSE)
    gene_positions <- data.frame(gene = top_genes, x = 2, y = seq_along(top_genes), stringsAsFactors = FALSE)

    edge_df <- merge(edges, term_positions, by = "term", all.x = TRUE)
    edge_df <- merge(edge_df, gene_positions, by = "gene", all.x = TRUE, suffixes = c("_term", "_gene"))

    ggplot2::ggplot() +
      ggplot2::geom_segment(
        data = edge_df,
        ggplot2::aes(x = x_term, xend = x_gene, y = y_term, yend = y_gene),
        color = "grey70",
        alpha = 0.45
      ) +
      ggplot2::geom_point(
        data = term_positions,
        ggplot2::aes(x = x, y = y),
        color = "#1B6CA8",
        size = 3.5
      ) +
      ggplot2::geom_point(
        data = gene_positions,
        ggplot2::aes(x = x, y = y),
        color = "#D95F02",
        size = 2.4
      ) +
      ggplot2::geom_text(
        data = term_positions,
        ggplot2::aes(x = x, y = y, label = term),
        hjust = 1.1,
        size = 3.2
      ) +
      ggplot2::geom_text(
        data = gene_positions,
        ggplot2::aes(x = x, y = y, label = gene),
        hjust = -0.1,
        size = 2.8
      ) +
      ggplot2::scale_x_continuous(breaks = c(1, 2), labels = c("Pathway", "Gene"), limits = c(0.8, 2.2)) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(10, 70, 10, 120)) +
      ggplot2::ggtitle("Pathway-gene network")
  }

  make_upsetplot_fallback <- function(enrich_obj, show_n) {
    edges <- extract_term_gene_edges(enrich_obj, show_n)
    if (!nrow(edges)) {
      return(make_info_plot("No gene-set intersections are available for the selected pathways."))
    }

    gene_sets <- split(edges$gene, edges$term)
    all_genes <- sort(unique(edges$gene))
    membership <- sapply(gene_sets, function(gs) all_genes %in% unique(gs))
    if (is.null(dim(membership))) {
      membership <- matrix(membership, ncol = 1, dimnames = list(all_genes, names(gene_sets)))
    }

    combo_labels <- apply(membership, 1, function(row_values) {
      selected_terms <- colnames(membership)[as.logical(row_values)]
      paste(selected_terms, collapse = " & ")
    })
    combo_labels <- combo_labels[nzchar(combo_labels)]
    if (!length(combo_labels)) {
      return(make_info_plot("No overlapping pathway intersections were found."))
    }

    combo_df <- as.data.frame(sort(table(combo_labels), decreasing = TRUE), stringsAsFactors = FALSE)
    colnames(combo_df) <- c("Combination", "Count")
    show_n_int <- suppressWarnings(as.integer(show_n))
    if (!is.finite(show_n_int) || show_n_int <= 0) {
      show_n_int <- 10L
    }
    combo_df <- head(combo_df, max(6L, show_n_int))
    combo_df$Combination <- factor(combo_df$Combination, levels = rev(combo_df$Combination))

    ggplot2::ggplot(combo_df, ggplot2::aes(x = Combination, y = Count)) +
      ggplot2::geom_col(fill = "#4C78A8", width = 0.75) +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title = "Pathway intersection summary",
        x = "Intersection",
        y = "Gene count"
      )
  }

  plot_enrichment <- function(enrich_obj, plot_type, show_n) {
    tryCatch(
      switch(
        plot_type,
        dotplot = enrichplot::dotplot(enrich_obj, showCategory = show_n),
        barplot = make_barplot(enrich_obj, show_n),
        goplot = enrichplot::goplot(enrich_obj, showCategory = show_n),
        cnetplot = enrichplot::cnetplot(enrich_obj, showCategory = show_n),
        upsetplot = enrichplot::upsetplot(enrich_obj, n = show_n),
        enrichplot::dotplot(enrich_obj, showCategory = show_n)
      ),
      error = function(e) {
        fallback_message <- paste0("Requested ", plot_type, " is unavailable.\n", conditionMessage(e))
        if (identical(plot_type, "cnetplot")) {
          return(make_cnetplot_fallback(enrich_obj, show_n) + ggplot2::ggtitle(fallback_message))
        }
        if (identical(plot_type, "upsetplot")) {
          return(make_upsetplot_fallback(enrich_obj, show_n) + ggplot2::ggtitle(fallback_message))
        }

        tryCatch(
          enrichplot::dotplot(enrich_obj, showCategory = show_n) + ggplot2::ggtitle(paste0(fallback_message, "\nShowing dotplot instead.")),
          error = function(e2) make_info_plot(paste(fallback_message, conditionMessage(e2), sep = "\n"))
        )
      }
    )
  }

  if (index_s_pathway1 == "gene_name_list"){
    deg_genes <- unlist(strsplit(index_s_pathway14, ","))
  }
  else if (index_s_pathway1 == "multiple_sample" & index_s_pathway2 == "seurat_clusters"){
    single_multiple_sample_clustering <- index_multiple_sample_pathway_input
    single_sample_clustering_markers <- index_multiple_sample_pathway_input3 
    deg_genes <- subset(single_sample_clustering_markers, (cluster ==  index_s_pathway3 & p_val_adj < index_s_pathway4))
    deg_genes <- deg_genes$gene
  }
  else if (index_s_pathway1 == "multiple_sample_subclustering" & index_s_pathway2 == "seurat_clusters"){
    single_multiple_sample_clustering <- index_subclustering_multiple_sample_pathway_input 
    single_sample_clustering_markers <- index_subclustering_multiple_sample_pathway_input3 
    deg_genes <- subset(single_sample_clustering_markers, (cluster ==  index_s_pathway3 & p_val_adj < index_s_pathway4))
    deg_genes <- deg_genes$gene
  }
  else if (index_s_pathway1 == "multiple_sample" & index_s_pathway2 == "predicted"){
    if (index_multiple_sample_pathway_input2 == "sctype_classification"){
      single_multiple_sample_clustering <- index_multiple_sample_pathway_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_pathway_input2
      single_sample_clustering_markers <- index_multiple_sample_pathway_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>% dplyr::select(seurat_clusters, sctype_classification) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df, by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers, (sctype_classification ==  index_s_pathway3  & p_val_adj < index_s_pathway4))
      deg_genes <- deg_genes$gene
    }
    else if (index_multiple_sample_pathway_input2 == "singleR_labels"){
      single_multiple_sample_clustering <- index_multiple_sample_pathway_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_pathway_input2
      single_sample_clustering_markers <- index_multiple_sample_pathway_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>% dplyr::select(seurat_clusters, singleR_labels) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df, by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers, (singleR_labels ==  index_s_pathway3 & p_val_adj < index_s_pathway4))
      deg_genes <- deg_genes$gene
    }
    else if (index_multiple_sample_pathway_input2 == "GPTCelltype"){
      single_multiple_sample_clustering <- index_multiple_sample_pathway_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_pathway_input2
      single_sample_clustering_markers <- index_multiple_sample_pathway_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>% dplyr::select(seurat_clusters, GPTCelltype) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df, by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers, (GPTCelltype ==  index_s_pathway3 & p_val_adj < index_s_pathway4))
      deg_genes <- deg_genes$gene
    }
    else if (index_multiple_sample_pathway_input2 == "cell_type"){
      single_multiple_sample_clustering <- index_multiple_sample_pathway_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_pathway_input2
      single_sample_clustering_markers <- index_multiple_sample_pathway_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>% dplyr::select(seurat_clusters, cell_type) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df, by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers, (cell_type ==  index_s_pathway3 & p_val_adj < index_s_pathway4))
      deg_genes <- deg_genes$gene
    }
  }
  
  else if (index_s_pathway1 == "multiple_sample_subclustering" & index_s_pathway2 == "predicted"){
    if (index_subclustering_multiple_sample_pathway_input2 == "sctype_classification"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_pathway_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_pathway_input2
      single_sample_clustering_markers <- index_subclustering_multiple_sample_pathway_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>% dplyr::select(seurat_clusters, sctype_classification) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df, by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers, (sctype_classification ==  index_s_pathway3 & p_val_adj < index_s_pathway4))
      deg_genes <- deg_genes$gene
    }
    else if (index_subclustering_multiple_sample_pathway_input2 == "singleR_labels"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_pathway_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_pathway_input2
      single_sample_clustering_markers <- index_subclustering_multiple_sample_pathway_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>% dplyr::select(seurat_clusters, singleR_labels) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df, by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers, (singleR_labels ==  index_s_pathway3 & p_val_adj < index_s_pathway4))
      deg_genes <- deg_genes$gene
    }
    else if (index_subclustering_multiple_sample_pathway_input2 == "GPTCelltype"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_pathway_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_pathway_input2
      single_sample_clustering_markers <- index_subclustering_multiple_sample_pathway_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>% dplyr::select(seurat_clusters, GPTCelltype) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df, by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers, (GPTCelltype ==  index_s_pathway3 & p_val_adj < index_s_pathway4))
      deg_genes <- deg_genes$gene
    }
    else if (index_subclustering_multiple_sample_pathway_input2 == "cell_type"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_pathway_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_pathway_input2
      single_sample_clustering_markers <- index_subclustering_multiple_sample_pathway_input3 
      export_df <- single_multiple_sample_clustering@meta.data %>% dplyr::select(seurat_clusters, cell_type) %>% distinct()
      single_sample_clustering_markers <- merge(single_sample_clustering_markers, export_df, by.x = "cluster", by.y = "seurat_clusters")
      deg_genes <- subset(single_sample_clustering_markers, (cell_type ==  index_s_pathway3 & p_val_adj < index_s_pathway4))
      deg_genes <- deg_genes$gene
    }
  }

  deg_genes <- sanitize_gene_vector(deg_genes)
  if (length(deg_genes) == 0) {
    return(list(plot1 = make_info_plot("No valid genes were available for pathway enrichment."), data1 = data.frame()))
  }

  entrez_ids <- tryCatch(
    clusterProfiler::bitr(deg_genes, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = index_s_pathway5),
    error = function(e) data.frame()
  )
  entrez_ids <- unique(entrez_ids[!is.na(entrez_ids$ENTREZID) & nzchar(entrez_ids$ENTREZID), , drop = FALSE])
  if (nrow(entrez_ids) == 0) {
    return(list(plot1 = make_info_plot("No genes could be mapped to Entrez IDs for pathway enrichment."), data1 = data.frame()))
  }

  entrez_gene_list <- entrez_ids$ENTREZID
  
  if (index_s_pathway6 == "KEGG"){
    if(index_s_pathway5 == "org.Hs.eg.db"){
    pathway_enrichment <- clusterProfiler::enrichKEGG(gene = entrez_gene_list, organism = 'hsa', pAdjustMethod = index_s_pathway7, pvalueCutoff = index_s_pathway8, qvalueCutoff = index_s_pathway9, minGSSize = index_s_pathway10, maxGSSize = index_s_pathway11)
	}
    else if(index_s_pathway5 == "org.Mm.eg.db"){
    pathway_enrichment <- clusterProfiler::enrichKEGG(gene = entrez_gene_list,organism = 'mmu', pAdjustMethod = index_s_pathway7, pvalueCutoff = index_s_pathway8, qvalueCutoff = index_s_pathway9, minGSSize = index_s_pathway10, maxGSSize = index_s_pathway11)
	}
    else if(index_s_pathway5 == "org.Rn.eg.db"){
    pathway_enrichment <- clusterProfiler::enrichKEGG(gene = entrez_gene_list, organism = 'rno', pAdjustMethod = index_s_pathway7, pvalueCutoff = index_s_pathway8, qvalueCutoff = index_s_pathway9, minGSSize = index_s_pathway10, maxGSSize = index_s_pathway11)
	}
    pathway_enrichment <- tryCatch(clusterProfiler::setReadable(pathway_enrichment, OrgDb = index_s_pathway5, keyType = "ENTREZID"), error = function(e) pathway_enrichment)
    pathway_results <- as.data.frame(pathway_enrichment@result)
    
  pathway_results <- pathway_results %>% dplyr::select(Description, geneID, everything())
    
  }
  else if (index_s_pathway6 == "Reactome"){
    if(index_s_pathway5 == "org.Hs.eg.db"){
      pathway_enrichment <- ReactomePA::enrichPathway(gene = entrez_gene_list, organism = 'human', pAdjustMethod = index_s_pathway7, pvalueCutoff = index_s_pathway8, qvalueCutoff = index_s_pathway9, minGSSize = index_s_pathway10, maxGSSize = index_s_pathway11, readable = TRUE)
    }
    else if(index_s_pathway5 == "org.Mm.eg.db"){
      pathway_enrichment <- ReactomePA::enrichPathway(gene = entrez_gene_list,organism = 'mouse', pAdjustMethod = index_s_pathway7, pvalueCutoff = index_s_pathway8, qvalueCutoff = index_s_pathway9, minGSSize = index_s_pathway10, maxGSSize = index_s_pathway11, readable = TRUE)
    }
    else if(index_s_pathway5 == "org.Rn.eg.db"){
      pathway_enrichment <- ReactomePA::enrichPathway(gene = entrez_gene_list, organism = 'rat', pAdjustMethod = index_s_pathway7, pvalueCutoff = index_s_pathway8, qvalueCutoff = index_s_pathway9, minGSSize = index_s_pathway10, maxGSSize = index_s_pathway11, readable = TRUE)
    } 
    pathway_results <- pathway_enrichment@result
  }

  pathway_enrichment <- sanitize_enrich_result(pathway_enrichment)
  pathway_results <- as.data.frame(pathway_enrichment@result)
  if (nrow(pathway_results) == 0) {
    return(list(plot1 = make_info_plot("No pathway terms passed the selected thresholds."), data1 = data.frame()))
  }

  plots301 <- plot_enrichment(pathway_enrichment, index_s_pathway12, index_s_pathway13)
  
  return(list(plot1 = plots301, data1 = pathway_results))
}
