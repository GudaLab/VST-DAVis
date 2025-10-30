# scripts/subclustering_multiple_clustering.R

datainput_subclustering_multiple_clustering <- function(
    index_subclustering_multiple_clustering_input,
    index_subclustering_multiple_sample_normalization_method,
    index_m_subclustering_clustering1,   # dims for neighbors
    index_m_subclustering_clustering2,   # k.param
    index_m_subclustering_clustering3,   # n.trees (when nn.method = "annoy")
    index_m_subclustering_clustering4,   # resolution
    index_m_subclustering_clustering5,   # algorithm (0-4)
    index_m_subclustering_clustering6,   # "umap" | "tsne"
    index_m_subclustering_clustering7,   # dims for UMAP
    index_m_subclustering_clustering8,   # n.neighbors (UMAP)
    index_m_subclustering_clustering9,   # min.dist (UMAP)
    index_m_subclustering_clustering10,  # label flag for UMAP plots (TRUE/FALSE)
    index_m_subclustering_clustering11,  # dims for tSNE
    index_m_subclustering_clustering12   # label flag for tSNE plots (TRUE/FALSE)
) {
  # ---- Libraries (quietly) ----
  suppressPackageStartupMessages({
    library(Seurat)
    library(data.table)
    library(ggplot2)
    library(patchwork)
    library(tidyr)
    library(dplyr)
  })
  
  # ---- Coerce inputs & basic guards ----
  index_m_subclustering_clustering5  <- as.numeric(index_m_subclustering_clustering5)
  index_m_subclustering_clustering10 <- as.logical(index_m_subclustering_clustering10)
  index_m_subclustering_clustering12 <- as.logical(index_m_subclustering_clustering12)
  
  if (!inherits(index_subclustering_multiple_clustering_input, "Seurat")) {
    stop("`index_subclustering_multiple_clustering_input` must be a Seurat object.")
  }
  
  # Ensure required meta columns exist (create stubs if missing to avoid crashes)
  sobj <- index_subclustering_multiple_clustering_input
  if (is.null(sobj@meta.data$condition))   sobj$condition   <- "condition_1"
  if (is.null(sobj@meta.data$orig.ident))  sobj$orig.ident  <- sobj$orig.ident %||% "sample_1"
  
  # ---- Graph-based clustering ----
  # If you intend to use Annoy's n.trees, set nn.method="annoy"
  sobj <- FindNeighbors(
    sobj,
    dims = 1:index_m_subclustering_clustering1,
    k.param = index_m_subclustering_clustering2,
    # Uncomment next line if you rely on n.trees via annoy:
    # nn.method = "annoy",
    # n.trees = index_m_subclustering_clustering3
  )
  
  sobj <- FindClusters(
    sobj,
    resolution = index_m_subclustering_clustering4,
    algorithm  = index_m_subclustering_clustering5
  )
  
  # ---- Dimensional reduction branch ----
  reduction_choice <- tolower(index_m_subclustering_clustering6)
  if (!reduction_choice %in% c("umap", "tsne")) {
    warning("Unknown reduction choice; defaulting to UMAP.")
    reduction_choice <- "umap"
  }
  
  # pick the right label flag for this branch
  label_flag <- if (reduction_choice == "umap") {
    index_m_subclustering_clustering10
  } else {
    index_m_subclustering_clustering12
  }
  
  # A numeric label size; 3.5 when labels on, 0 (invisible) when off
  label_size <- if (label_flag) 3.5 else 0
  
  if (reduction_choice == "umap") {
    sobj <- RunUMAP(
      sobj,
      dims        = 1:index_m_subclustering_clustering7,
      n.neighbors = index_m_subclustering_clustering8,
      min.dist    = index_m_subclustering_clustering9
    )
    
    plots16 <- DimPlot(sobj, reduction = "umap", label = label_flag, label.size = 3.5,
                       raster = FALSE, group.by = "seurat_clusters")
    plots17 <- DimPlot(sobj, reduction = "umap", label = label_flag, label.size = 3.5,
                       raster = FALSE, group.by = "condition")
    plots18 <- DimPlot(sobj, reduction = "umap", label = label_flag, label.size = 3.5,
                       raster = FALSE, group.by = "orig.ident")
    
    plots22 <- SpatialDimPlot(sobj, label = label_flag)
    
    plots24 <- DimPlot(sobj, reduction = "umap", label = label_flag, label.size = 3.5,
                       raster = FALSE, group.by = "seurat_clusters",
                       split.by = "condition", ncol = 6)
    plots25 <- DimPlot(sobj, reduction = "umap", label = label_flag, label.size = 3.5,
                       raster = FALSE, group.by = "seurat_clusters",
                       split.by = "orig.ident", ncol = 6)
    plots26 <- DimPlot(sobj, reduction = "umap", label = label_flag, label.size = 3.5,
                       raster = FALSE, group.by = "condition",
                       split.by = "seurat_clusters", ncol = 6)
    
  } else {
    sobj <- RunTSNE(sobj, dims = 1:index_m_subclustering_clustering11)
    
    plots16 <- DimPlot(sobj, reduction = "tsne", label = label_flag, label.size = 3.5,
                       raster = FALSE, group.by = "seurat_clusters")
    plots17 <- DimPlot(sobj, reduction = "tsne", label = label_flag, label.size = 3.5,
                       raster = FALSE, group.by = "condition")
    plots18 <- DimPlot(sobj, reduction = "tsne", label = label_flag, label.size = 3.5,
                       raster = FALSE, group.by = "orig.ident")
    
    # Use the branch-appropriate label flag here (was previously hard-coded)
    plots22 <- SpatialDimPlot(sobj, label = label_flag)
    
    plots24 <- DimPlot(sobj, reduction = "tsne", label = label_flag, label.size = 3.5,
                       raster = FALSE, group.by = "seurat_clusters",
                       split.by = "condition", ncol = 6)
    plots25 <- DimPlot(sobj, reduction = "tsne", label = label_flag, label.size = 3.5,
                       raster = FALSE, group.by = "seurat_clusters",
                       split.by = "orig.ident", ncol = 6)
    plots26 <- DimPlot(sobj, reduction = "tsne", label = label_flag, label.size = 3.5,
                       raster = FALSE, group.by = "condition",
                       split.by = "seurat_clusters", ncol = 6)
  }
  
  # ---- Basic counts per cluster ----
  subclustering_multiple_sample_clustering_cell_couts_in_custer <-
    table(sobj@meta.data$seurat_clusters) %>% as.data.table()
  setnames(subclustering_multiple_sample_clustering_cell_couts_in_custer, c("Clusters", "Counts"))
  
  plots19 <- ggplot(sobj@meta.data, aes(x = seurat_clusters, fill = seurat_clusters)) +
    geom_bar(stat = "count", position = position_dodge()) +
    geom_text(stat = "count", aes(label = after_stat(count)),
              vjust = -0.5, position = position_dodge(0.9), size = label_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          plot.margin = unit(c(1,1,1,1), "line")) +
    guides(fill = guide_legend(title = "Clusters")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
  
  # ---- Counts per cluster per condition ----
  meta_dt <- as.data.table(sobj@meta.data)
  tmp_cond <- meta_dt[, .N, by = .(condition, seurat_clusters)]
  tmp_cond <- dcast(tmp_cond, condition ~ seurat_clusters, value.var = "N", fill = 0)
  
  df_cond <- as.data.frame(t(tmp_cond))
  df_cond <- tibble::rownames_to_column(df_cond, var = "condition")
  colnames(df_cond) <- df_cond[1, ]
  subclustering_multiple_sample_clustering_total_cell_couts_in_custer_for_each_condition <- df_cond[-1, , drop = FALSE]
  
  plots20 <- ggplot(sobj@meta.data, aes(x = seurat_clusters, fill = condition)) +
    geom_bar(stat = "count") +
    geom_text(stat = "count", aes(label = after_stat(count)),
              position = position_stack(vjust = 0.5), size = label_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          plot.margin = unit(c(1,1,1,1), "line")) +
    guides(fill = guide_legend(title = "Condition")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
  
  # ---- Counts per cluster per sample ----
  tmp_samp <- meta_dt[, .N, by = .(orig.ident, seurat_clusters)]
  tmp_samp <- dcast(tmp_samp, orig.ident ~ seurat_clusters, value.var = "N", fill = 0)
  
  df_samp <- as.data.frame(t(tmp_samp))
  df_samp <- tibble::rownames_to_column(df_samp, var = "Clusters")
  colnames(df_samp) <- df_samp[1, ]
  subclustering_multiple_sample_clustering_total_cell_couts_in_custer_for_each_samples <- df_samp[-1, , drop = FALSE]
  colnames(subclustering_multiple_sample_clustering_total_cell_couts_in_custer_for_each_samples)[1] <- "Clusters"
  
  plots21 <- ggplot(sobj@meta.data, aes(x = seurat_clusters, fill = orig.ident)) +
    geom_bar(stat = "count") +
    geom_text(stat = "count", aes(label = after_stat(count)),
              position = position_stack(vjust = 0.5), size = label_size) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          plot.margin = unit(c(1,1,1,1), "line")) +
    guides(fill = guide_legend(title = "Samples")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
  
  # ---- Spatial facets per sample (Visium/Visium HD) ----
  # For each orig.ident, show SpatialDimPlot with facet highlighting of its cells
  target_idents <- unique(sobj$orig.ident)
  plot_list <- vector("list", length(target_idents))
  
  for (i in seq_along(target_idents)) {
    ident <- target_idents[i]
    sobj_sub <- subset(sobj, subset = orig.ident == ident)
    
    message(sprintf("Generating spatial plot for: %s", ident))
    
    spatial_cells <- CellsByIdentities(sobj_sub)
    # Remove potential "NA" key if present
    spatial_cells <- spatial_cells[setdiff(names(spatial_cells), "NA")]
    
    plot_list[[i]] <- SpatialDimPlot(
      sobj_sub,
      label = FALSE,
      cells.highlight = spatial_cells,
      cols.highlight = c("#FFFF00", "grey50"),
      facet.highlight = TRUE
    ) + NoLegend()
  }
  
  plots23 <- wrap_plots(plot_list, ncol = 1)
  
  # ---- Outputs ----
  cluster_type <- "seurat_clusters"
  
  return(list(
    plot1  = plots16,
    plot2  = plots19,
    plot3  = plots17,
    plot4  = plots20,
    plot5  = plots18,
    plot6  = plots21,
    data1  = subclustering_multiple_sample_clustering_cell_couts_in_custer,
    data2  = subclustering_multiple_sample_clustering_total_cell_couts_in_custer_for_each_condition,
    data3  = subclustering_multiple_sample_clustering_total_cell_couts_in_custer_for_each_samples,
    data4  = sobj,
    data5  = unique(sobj@meta.data$seurat_clusters),
    data6  = suppressWarnings(max(as.numeric(as.character(sobj@meta.data$seurat_clusters)), na.rm = TRUE)),
    data7  = unique(sobj@meta.data$condition),
    text_summary = cluster_type,
    plot7  = plots22,
    plot8  = plots23,
    plot9  = plots24,
    plot10 = plots25,
    plot11 = plots26
  ))
}
