drawHeatmapPlot <- function(seuratobject, genesInterest,resultsFolder = resultsDirectory, plotwidth = 28, plotheight = 15, plotName, assaytype = "SCT",groupValue = "seurat_clusters",slotValue = "scale.data",drawLinesValue = T,cellsValue = NULL){

  #dotcolors <- dotcolors
  heatmapObject <- DoHeatmap(object = seuratobject,features = genesInterest,assay = assaytype,group.by = groupValue,slot = slotValue,draw.lines = drawLinesValue, cells = cellsValue)
  
  heatmapObject
  
  # Create folder
  dir.create(file.path("Results/HeatmapPlot/"))
  
  ggsave(plot = heatmapObject,
         filename = paste0(resultsFolder,"/HeatmapPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_HeatmapPlot_",plotName, ".png"),
         width = plotwidth, #28 default
         height = plotheight,
         units = "cm") # height was 4, width 15
  
  ggsave(plot = heatmapObject,
         filename = paste0(resultsFolder,"/HeatmapPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_HeatmapPlot_",plotName,".pdf"),
         width = plotwidth, #28 default
         height = plotheight,
         units = "cm") # height was 4
  
  heatmapObject
}