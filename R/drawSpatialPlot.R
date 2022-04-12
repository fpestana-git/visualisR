drawSpatialPlot <- function(features,dataset,metadata, subsample = FALSE){
  if (subsample != F) {
    metadata <- metadata[which(metadata$orig.ident %in% subsample),]
    dataset <- dataset[,metadata$Name]
  }
  # Create spatial Seurat
  spatialSeurat <- createSpatialSeurat(sampleCounts = dataset,sampleCoordinates = metadata)
  spatialSeurat@assays[["Spatial"]]@scale.data <- spatialClustering_norm@assays[["RNA"]]@scale.data
  
  # 
  spatialSeurat <- AddMetaData(object = spatialSeurat, metadata = metadata)
  plot <- SpatialFeaturePlot(spatialSeurat,features = features,stroke = 0,pt.size.factor = 2,slot = "scale.data")+ ggplot2::scale_fill_continuous(low = "gray95", high = "Black") 
  
  
  ggsave(plot = plot,
         filename = paste0("../Results/SpatialPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_SpatialPlot_",features,".png"),
         width = 5,
         height = 7.5,
         units = "in",limitsize = FALSE)
  ggsave(plot = plot,
         filename = paste0("../Results/SpatialPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_SpatialPlot_",features,".pdf"),
         width = 5,
         height = 7.5,
         units = "in",limitsize = FALSE)
  print(plot)
  
  
  
}