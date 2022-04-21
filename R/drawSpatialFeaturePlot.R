#' Draw Spatial Feature Plot for given genes
#' 
#' Function that generates spatial feature plots for a given gene and sample
#' 
#' @param dataset TEST
#' 
#' @export drawSpatialFeaturePlot

drawSpatialFeaturePlot <- function(features,dataset,metadata, subsample = FALSE){
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
  
  # Create folder
  dir.create(file.path("../Results/SpatialFeaturePlot/"),recursive = T,showWarnings = F)
  
  
  ggsave(plot = plot,
         filename = paste0("../Results/SpatialFeaturePlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_",features,".png"),
         width = 5,
         height = 7.5,
         units = "in",limitsize = FALSE)
  ggsave(plot = plot,
         filename = paste0("../Results/SpatialFeaturePlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_",features,".pdf"),
         width = 5,
         height = 7.5,
         units = "in",limitsize = FALSE)
  print(plot)
  
  
  
}