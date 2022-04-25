#' Draw Dot Plot 
#' 
#' Function that generates dot plots for a given Seurat dataset
#' 
#' @param seuratObject seurat data object
#' @param plotName define name of the plot
#' @param featureValues define which features (genes) to show
#' @param splitby metadata variable to split (default NULL)
#' @param heightValue height of the final plot (default 6 cm)
#' @param widthValue width of the final plot (default 6 cm)
#' 
#' @examples 
#' drawDotPlot(seuratObject = seuratLOG,plotName = "test",featureValues = c("S100A9","NKG7","LDHB","CD79A"))
#' 
#' 
#' @export drawDotPlot

drawDotPlot <- function(seuratObject, plotName,featureValues, splitby = NULL ,widthValue = 10, heightValue = 10){
  
  # Create dot plot
  dotPlotClusters <- DotPlot(seuratObject, 
                             features = featureValues,
                             split.by = splitby,
                             scale = T,group.by = NULL) + 
    theme(axis.text.x = element_text(angle = 90, size = 8),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          axis.text.x.top = element_text(face = "italic"),
          axis.title.x.bottom = element_text(vjust = -10),
          axis.text.y = element_text(size = 8,face = "italic"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8),
          legend.position = "right",
          legend.box = "horizontal",
          legend.key.height = unit(0.3,"cm"))+
    labs(x="",y="") +
    scale_x_discrete(position = "top") +
    scale_y_discrete(position = "left") +
    coord_flip() 
  scale_colour_gradient2(low = "white", high = "blue")

  # Create folder
  dir.create(file.path("../Results/DotPlot/"),recursive = T,showWarnings = F)
  
  # Save plot
  savePlots(plotObject = dotPlotClusters,
            plotTypeValue = "DotPlot",
            widthValue = widthValue,
            heightValue = heightValue,
            extraInfoValue = paste0("_",plotName))
  
  # Return object
  dotPlotClusters
}