

drawDimPlotLiger <- function(ligerObject){
  umap_plots <-plotByDatasetAndCluster(ligerObject, axis.labels = c("UMAP1","UMAP2"), return.plots = TRUE)
  #umap_plots[[1]]
  
  
  a <- umap_plots[[1]] + NoLegend()
  
  ggsave(plot =a,filename = paste0("Results/DimPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_integrated.png"),         
         width = 4,
         height = 4,
         units = "in",limitsize = FALSE)
  ggsave(plot = a,filename = paste0("Results/DimPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_integrated.pdf"),         
         width = 4,
         height = 4,
         units = "in",limitsize = FALSE)
  
  cluster <- plotByDatasetAndCluster(object = ligerObject,axis.labels = c("UMAP_1","UMAP_2")) +NoLegend()
  
  ggsave(plot = cluster,filename = paste0( "Results/DimPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_integrated.png"),         
         width = 4,
         height = 4,
         units = "in",limitsize = FALSE)
  ggsave(plot = cluster,filename = paste0( "Results/DimPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_integrated.pdf"),         
         width = 4,
         height = 4,
         units = "in",limitsize = FALSE)
  
}
