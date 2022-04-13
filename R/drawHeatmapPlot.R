#' Draw Heatmap Plot 
#' 
#' Function that generates heamap plots for given features
#' 
#' @param seuratObject seurat data object
#' @param datasetName name of the dataset (e.g., Pestana2022)
#' @param featureNames list of features to plot
#' @param plotName name of the plot
#' @param heightValue height of the final plot (default 15 cm)
#' @param widthValue width of the final plot (default 28 cm)
#' @param assaytype TEST
#' @param groupValue TEST
#' @param slotValue TEST
#' @param drawLinesValue TEST
#' @param cellsValue TEST
#' 
#' @export drawHeatmapPlot
#' 
#' @example 
#' drawHeatmapPlot(seuratObject = myseuratobject,featureNames =  c("Snap25","Slc17a7","Tbr1"),plotName = "neurons")


drawHeatmapPlot <- function(seuratObject, featureNames, widthValue = 28, heightValue = 15, plotName, assaytype = "SCT",groupValue = "orig.ident",slotValue = "scale.data",drawLinesValue = T,cellsValue = NULL){

  #dotcolors <- dotcolors
  heatmapObject <- DoHeatmap(object = seuratObject,features = featureNames,assay = assaytype,group.by = groupValue,slot = slotValue,draw.lines = drawLinesValue, cells = cellsValue)
  
  
  # Create folder
  dir.create(file.path("../Results/HeatmapPlot/"),recursive = T)
  
  # Save plot as png
  ggsave(plot = heatmapObject,
         filename = paste0("../Results/HeatmapPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_",plotName, ".png"),
         width = widthValue, #28 default
         height = heightValue,
         units = "cm") # height was 4, width 15
  
  # Save plot as pdf
  ggsave(plot = heatmapObject,
         filename = paste0("../Results/HeatmapPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_",plotName,".pdf"),
         width = widthValue, #28 default
         height = heightValue,
         units = "cm") # height was 4
  
  heatmapObject
}