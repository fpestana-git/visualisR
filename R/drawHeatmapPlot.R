#' Draw Heatmap Plot 
#' 
#' Function that generates heamap plots for given features
#' 
#' @param seuratObject seurat data object
#' @param datasetName name of the dataset (e.g., Pestana2022)
#' @param featureNames list of features to plot
#' @param plotName name of the plot
#' @param labelValue show or not the labels above the groups (default TRUE)
#' @param heightValue height of the final plot (default 15 cm)
#' @param widthValue width of the final plot (default 28 cm)
#' @param assaytype TEST
#' @param groupValue TEST
#' @param slotValue TEST
#' @param drawLinesValue whether to show the lines separating the groups (default = TRUE)
#' @param cellsValue TEST
#' @param groupColorValues list of colors for the groups (default = NULL)
#' @param showLegend show or hide plot legend (default FALSE)
#' 
#' @examples 
#' drawHeatmapPlot(seuratObject = myseuratobject,featureNames =  c("Snap25","Slc17a7","Tbr1"),plotName = "neurons")
#' 
#' @export drawHeatmapPlot

drawHeatmapPlot <- function(seuratObject, featureNames, labelValue = TRUE,widthValue = 28, heightValue = 15, plotName, assaytype = "SCT",groupValue = "orig.ident",slotValue = "scale.data",drawLinesValue = TRUE,cellsValue = NULL,groupColorValues = NULL, showLegend = FALSE){

  #dotcolors <- dotcolors
  heatmapObject <- DoHeatmap(object = seuratObject,
                             features = featureNames,
                             assay = assaytype,
                             group.by = groupValue,
                             group.colors = groupColorValues,
                             slot = slotValue,
                             draw.lines = drawLinesValue, 
                             cells = cellsValue,
                             size = 4,
                             label = labelValue) + 
    theme(axis.text  = element_text(size=8, 
                                      face = 4, 
                                      family = "Helvetica"),text = element_text(size = 8,family = "Helvetica")) +  scale_fill_viridis() 
  
  # Extract legend
  legend <- get_legend(heatmapObject)
  
  # Remove legend
  if (showLegend == FALSE) {
    heatmapObject <- heatmapObject + NoLegend()
  }else {
    # Only include scalebar
    heatmapObject <- heatmapObject + NoLegend()
    # heatmapObject <- plot_grid(heatmapObject, legend$grobs[[1]],
    #                            nrow = 1,ncol = 2)
    heatmapObject <- heatmapObject + legend$grobs[[1]] 
    # heatmapObject <- heatmapObject
    
  }
  
  
  
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