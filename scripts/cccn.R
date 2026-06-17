datainput_single_multiple_sample_cccn<- function(index_multiple_sample_cccn_input, index_subclustering_multiple_sample_cccn_input, index_multiple_sample_cccn_input2, index_subclustering_multiple_sample_cccn_input2, index_multiple_sample_normalization_method_cccn, index_subclustering_multiple_sample_normalization_method_cccn, index_s_cccn1, index_s_cccn2, index_s_cccn3){
  source_app_script("scripts/assay_utils.R")
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  pick_cccn_assay <- function(obj, normalization_method) {
    assays <- tryCatch(Seurat::Assays(obj), error = function(e) character(0))
    if (identical(normalization_method, "SCTransform") && "SCT" %in% assays) {
      return("SCT")
    }
    if (identical(normalization_method, "LogNormalize") && "integrated" %in% assays) {
      return("integrated")
    }
    if ("RNA" %in% assays) {
      return("RNA")
    }
    if ("Spatial" %in% assays) {
      return("Spatial")
    }
    assays[[1]] %||% Seurat::DefaultAssay(obj)
  }

  get_cccn_matrix <- function(obj, assay_name) {
    for (layer_name in c("scale.data", "data", "counts")) {
      mat <- tryCatch(
        get_assay_layer_matrix(
          object = obj,
          assay = assay_name,
          layer = layer_name,
          slot_fallback = layer_name,
          as_sparse = FALSE
        ),
        error = function(e) NULL
      )
      if (!is.null(mat) && nrow(mat) > 1 && ncol(mat) > 1) {
        return(as.matrix(mat))
      }
    }
    stop("Could not retrieve a usable expression matrix for CCCN analysis.")
  }

  sanitize_cccn_matrix <- function(mat) {
    mat <- as.matrix(mat)
    if (!is.numeric(mat)) {
      storage.mode(mat) <- "numeric"
    }

    finite_entries <- is.finite(mat)
    if (!all(finite_entries)) {
      mat[!finite_entries] <- 0
    }

    keep_rows <- rowSums(is.finite(mat)) == ncol(mat)
    if (any(!keep_rows)) {
      mat <- mat[keep_rows, , drop = FALSE]
    }

    if (nrow(mat) < 2 || ncol(mat) < 2) {
      stop("CCCN analysis requires at least two genes and two cells/spots after cleanup.")
    }

    row_var <- apply(mat, 1, stats::var, na.rm = TRUE)
    keep_var <- is.finite(row_var) & row_var > 0
    if (sum(keep_var) >= 2) {
      mat <- mat[keep_var, , drop = FALSE]
    }

    if (nrow(mat) < 2) {
      stop("CCCN analysis requires at least two variable genes after removing non-finite values.")
    }

    mat
  }

  compute_cluster_means <- function(mat, ident_vector) {
    cluster_levels <- unique(as.character(ident_vector))
    cluster_levels <- cluster_levels[!is.na(cluster_levels) & nzchar(cluster_levels)]

    mean_list <- lapply(cluster_levels, function(cluster_name) {
      cluster_cells <- which(as.character(ident_vector) == cluster_name)
      if (!length(cluster_cells)) {
        return(NULL)
      }

      rowMeans(mat[, cluster_cells, drop = FALSE], na.rm = TRUE)
    })

    valid_clusters <- vapply(mean_list, function(x) !is.null(x), logical(1))
    mean_list <- mean_list[valid_clusters]
    cluster_levels <- cluster_levels[valid_clusters]

    if (length(mean_list) < 2) {
      stop("CCCN analysis requires at least two non-empty groups or clusters.")
    }

    cluster_means <- do.call(cbind, mean_list)
    colnames(cluster_means) <- cluster_levels
    rownames(cluster_means) <- rownames(mat)
    cluster_means
  }

  select_hvg_genes <- function(mat, ident_vector) {
    selected_genes <- character(0)

    sg <- tryCatch(genesorteR::sortGenes(mat, ident_vector), error = function(e) NULL)
    if (!is.null(sg)) {
      pv <- tryCatch(genesorteR::getPValues(sg), error = function(e) NULL)
      if (!is.null(pv) && !is.null(pv$adjpval)) {
        selected_genes <- names(which(apply(pv$adjpval, 1, function(x) any(is.finite(x) & x < 0.05))))
      }
    }

    if (!length(selected_genes)) {
      cluster_means <- compute_cluster_means(mat, ident_vector)
      gene_var <- apply(cluster_means, 1, stats::var, na.rm = TRUE)
      gene_var <- gene_var[is.finite(gene_var) & gene_var > 0]
      if (!length(gene_var)) {
        return(rownames(mat)[seq_len(min(200, nrow(mat)))])
      }
      selected_genes <- names(sort(gene_var, decreasing = TRUE))[seq_len(min(200, length(gene_var)))]
    }

    selected_genes[selected_genes %in% rownames(mat)]
  }

  build_cccn_correlation <- function(mat, ident_vector, selected_genes, cor_method) {
    cluster_means <- compute_cluster_means(mat, ident_vector)
    selected_genes <- unique(selected_genes[selected_genes %in% rownames(cluster_means)])

    if (length(selected_genes) < 2) {
      selected_genes <- rownames(cluster_means)[seq_len(min(200, nrow(cluster_means)))]
    }

    corr_input <- cluster_means[selected_genes, , drop = FALSE]
    corr_matrix <- stats::cor(corr_input, method = cor_method, use = "pairwise.complete.obs")

    if (!is.matrix(corr_matrix)) {
      corr_matrix <- matrix(corr_matrix, nrow = ncol(corr_input), ncol = ncol(corr_input))
      rownames(corr_matrix) <- colnames(corr_input)
      colnames(corr_matrix) <- colnames(corr_input)
    }

    corr_matrix[!is.finite(corr_matrix)] <- 0
    diag(corr_matrix) <- 1
    corr_matrix
  }

  if (index_s_cccn1 == "multiple_sample" & index_s_cccn2 == "seurat_clusters"){
    single_multiple_sample_clustering <- index_multiple_sample_cccn_input 
      }
  else if (index_s_cccn1 == "multiple_sample_subclustering" & index_s_cccn2 == "seurat_clusters"){
    single_multiple_sample_clustering <- index_subclustering_multiple_sample_cccn_input  
  }
  else if (index_s_cccn1 == "multiple_sample" & index_s_cccn2 == "predicted"){
    if (index_multiple_sample_cccn_input2 == "sctype_classification"){
    single_multiple_sample_clustering <- index_multiple_sample_cccn_input
    Idents(single_multiple_sample_clustering) <- index_multiple_sample_cccn_input2
    }
    else if (index_multiple_sample_cccn_input2 == "singleR_labels"){
      single_multiple_sample_clustering <- index_multiple_sample_cccn_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_cccn_input2
    }
    else if (index_multiple_sample_cccn_input2 == "GPTCelltype"){
      single_multiple_sample_clustering <- index_multiple_sample_cccn_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_cccn_input2
    }
    else if (index_multiple_sample_cccn_input2 == "cell_type"){
      single_multiple_sample_clustering <- index_multiple_sample_cccn_input
      Idents(single_multiple_sample_clustering) <- index_multiple_sample_cccn_input2
    }
     }
  else if (index_s_cccn1 == "multiple_sample_subclustering" & index_s_cccn2 == "predicted"){
    single_multiple_sample_clustering <- index_subclustering_multiple_sample_cccn_input
    if (index_subclustering_multiple_sample_cccn_input2 == "sctype_classification"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_cccn_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_cccn_input2
    }
    else if (index_subclustering_multiple_sample_cccn_input2 == "singleR_labels"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_cccn_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_cccn_input2
    }
    else if (index_subclustering_multiple_sample_cccn_input2 == "GPTCelltype"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_cccn_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_cccn_input2
    }
    else if (index_subclustering_multiple_sample_cccn_input2 == "cell_type"){
      single_multiple_sample_clustering <- index_subclustering_multiple_sample_cccn_input
      Idents(single_multiple_sample_clustering) <- index_subclustering_multiple_sample_cccn_input2
    }
  }

  normalization_method <- if (identical(index_s_cccn1, "multiple_sample")) {
    index_multiple_sample_normalization_method_cccn
  } else {
    index_subclustering_multiple_sample_normalization_method_cccn
  }

  cccn_assay <- pick_cccn_assay(single_multiple_sample_clustering, normalization_method)
  expr_mat <- sanitize_cccn_matrix(get_cccn_matrix(single_multiple_sample_clustering, cccn_assay))
  ident_vector <- Idents(single_multiple_sample_clustering)

  if (length(ident_vector) != ncol(expr_mat)) {
    stop("CCCN analysis requires the expression matrix columns to match the cluster identities.")
  }

  if (length(unique(as.character(ident_vector))) < 2) {
    stop("CCCN analysis requires at least two groups or clusters.")
  }

  hvg <- select_hvg_genes(expr_mat, ident_vector)
  if (length(hvg) == 0) {
    hvg <- rownames(expr_mat)[seq_len(min(200, nrow(expr_mat)))]
  }
  #calculate cluster correlations based on highly variable genes corMethod="pearson","spearman","kendall"
  pc <- build_cccn_correlation(expr_mat, ident_vector, hvg, cor_method = index_s_cccn3)
  pc1 <-  as.data.frame(pc)
  plots901 <- pheatmap::pheatmap(pc, main = "Correlation Heatmap", display_numbers = TRUE, number_color = "black", fontsize_number = 8)
  plots903 <- ggplotify::as.ggplot(plots901)
  #set diagonal entries to 0 (network lingo: no self-loops)
  diag(pc) = 0
  #set negative entries to 0 (if two clusters' correlation is below 0.1, they are not connected. You can experiment with different cutoffs.)
  pc[pc < 0.1] = 0
  #Step Three: Visualize the Correlation Network
  #create an igraph network object from the weighted adjacency matrix stored in pc
  net = igraph::graph_from_adjacency_matrix(pc, weighted = TRUE)
  #remove multiple edges (meaning keep only one connection between each two cell clusters)
  net = igraph::simplify(net, edge.attr.comb = "max")
  #let's give the clusters names (identified in the Seurat tutorial)
  #cluster_name = single_multiple_sample_clustering$
  #Some information to use in our plots
  Correlation = E(net)$weight
  Percent_of_Cells = as.vector(prop.table(table(as.character(ident_vector)))[igraph::V(net)$name] * 100)
  #plot network with ggraph
  set.seed(111)
  lay = ggraph::create_layout(net, layout = "fr")
  plots902 <-  ggraph(lay) + 
    geom_edge_link(aes(alpha = Correlation), edge_colour = "gray") + 
    geom_node_point(aes(size = Percent_of_Cells), colour = "blue") + 
    geom_node_text(aes(label = name), repel=TRUE) +
    theme(panel.background = element_blank())
  
  return(list(plot1 = plots903, plot2 = plots902, data1=pc1)) 
  
}
  
  
