

# drawSpatialDim <- function(groupLevel,subsample,metadata,dataset,widthValue = 5,heightValue = 7.5,splitValue = FALSE){
#   if (subsample != F) {
#     metadata <- metadata[which(metadata$orig.ident %in% subsample),]
#     dataset <- dataset[,metadata$Name]
#   }
#   
#   # Create spatial Seurat
#   spatialSeurat <- createSpatialSeurat(sampleCounts = dataset,sampleCoordinates = metadata)
#   spatialSeurat@assays[["Spatial"]]@scale.data <- spatialClustering_norm@assays[["RNA"]]@scale.data
#   
#   # 
#   spatialSeurat <- AddMetaData(object = spatialSeurat, metadata = metadata)
#   
#   spatialPlot <- SpatialDimPlot(object = spatialSeurat,group.by = groupLevel,stroke = 0, pt.size.factor = 2)
#   
#   if (splitValue != F) {
#     groupVector <- list()
#     for (i in unique(spatialMetadata$cellType_L2)) {
#       cellHighlighed <- spatialMetadata[which(spatialMetadata$cellType_L2 %in% i),"sampleIDs"]
#       splitPlot <- SpatialDimPlot(object = spatialSeurat,stroke = 0, pt.size.factor = 2,cells.highlight = cellHighlighed,cols.highlight = c("#af132b","#6bd9ef")) + NoLegend() + labs(title = i)
#       print(splitPlot)
#       Sys.sleep(1)
#       ggsave(plot = splitPlot,
#              filename = paste0("../Results/SpatialPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),subsample,"spatialDimplot",".png"),
#              width = widthValue,
#              height = heightValue,
#              units = "in",limitsize = FALSE)
#       ggsave(plot = splitPlot,
#              filename = paste0("../Results/SpatialPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),subsample,"spatialDimplot",".pdf"),
#              width = widthValue,
#              height = heightValue,
#              units = "in",limitsize = FALSE)
#     }
#   }
#   
#   spatialPlot
#   ggsave(plot = spatialPlot,
#          filename = paste0("../Results/SpatialPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),subsample,"spatialDimplot",".png"),
#          width = widthValue,
#          height = heightValue,
#          units = "in",limitsize = FALSE)
#   ggsave(plot = spatialPlot,
#          filename = paste0("../Results/SpatialPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),subsample,"spatialDimplot",".pdf"),
#          width = widthValue,
#          height = heightValue,
#          units = "in",limitsize = FALSE)
#   
#   
#   
# }



drawSpatialPlot <- function(spatialObject,groupValue,nameDatasetValue){
  #if (subsample != F) {
  #   metadata <- metadata[which(metadata$orig.ident %in% subsample),]
  #   dataset <- dataset[,metadata$Name]
  # }
  # Create spatial Seurat
  #spatialSeurat <- createSpatialSeurat(sampleCounts = dataset,sampleCoordinates = metadata)
  #spatialSeurat@assays[["Spatial"]]@scale.data <- spatialClustering_norm@assays[["RNA"]]@scale.data
  
  # 
  #spatialSeurat <- AddMetaData(object = spatialSeurat, metadata = metadata)
  plot <- SpatialDimPlot(object = spatialObject,group.by = groupValue,stroke = 0, pt.size.factor = 2) + NoLegend()
  
  
  
  ggsave(plot = plot,
         filename = paste0("Results/SpatialPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_Spatial_",nameDatasetValue,"_",groupValue,".png"),
         width = 5,
         height = 7.5,
         units = "in",limitsize = FALSE)
  ggsave(plot = plot,
         filename = paste0("Results/SpatialPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_Spatial_",nameDatasetValue,"_",groupValue,".pdf"),
         width = 5,
         height = 7.5,
         units = "in",limitsize = FALSE)
  print(plot)
}