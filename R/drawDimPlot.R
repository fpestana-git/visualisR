#' Draw Dim Plot 
#' 
#' Function that generates dim plots for a given Seurat dataset
#' 
#' @param seuratObject seurat data object
#' @param datasetName name of dataset to use on the saved objects
#' @param mapType set umap as default
#' @param splitValue if the umap is to be splitted
#' @param heightValue height of the final plot (default 6 cm)
#' @param widthValue width of the final plot (default 6 cm)
#' @param colorVector 
#' @param cells2highlight highlight specific cells
#' @param sizeHighlightedCells change size of highlighted cells
#' @param groupbyValue metadava variable to visualize in the dim plot 
#' @param labelValue show or hide cluster labels (default FALSE)
#' @param showLegend show or hide plot legend (default FALSE), 
#' @param resultsFolder directory where to save the results 
#' @param plotMinimal show a minimal plot
#' 
#' @export drawDimPlot

drawDimPlot <- function(seuratObject, datasetName, mapType = "umap",splitValue = NULL, heightValue = 10,widthValue = 10,colorVector = NULL,cells2highlight = NULL, sizeHighlightedCells = NULL,groupbyValue = NULL,labelValue = F,showLegend =FALSE, resultsFolder = resultsDirectory, plotMinimal = FALSE){
  dimplot_n <- DimPlot(seuratObject, reduction = mapType, 
                       label = labelValue, 
                       label.size = 6,
                       pt.size = 0.1,
                       split.by = splitValue,
                       cols = colorVector,
                       group.by = groupbyValue,
                       cells.highlight = cells2highlight,
                       sizes.highlight = sizeHighlightedCells) + 
    theme(axis.line=element_blank(),
          panel.border = element_rect(colour = "black"),plot.title = element_blank())  +
    scale_y_continuous(breaks=c(-10,0, 10)) +
    scale_x_continuous(breaks=c(-10,0, 10)) + 
    guides(colour = guide_legend(override.aes = list(shape = 15,size=6,ncol=1)))
  
  if (showLegend == FALSE) {
    dimplot_n <- dimplot_n + NoLegend()
  }
  
  if(plotMinimal){
    dimplot_n <- dimplot_n + theme_nothing()
  }
  
  # Extract resolution and pca value
  resolutionValue <- seuratObject@reductions$resolutionValue
  pcaValue <- seuratObject@reductions$pcaValueOptimal
  
  # Create folder
  dir.create(file.path("../Results/DimPlot/"),recursive = T,showWarnings = F)

  ggsave(plot = dimplot_n,filename = paste0("../Results/DimPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",gsub(pattern = "[/]",replacement = "",datasetName), "_DimPlot_",pcaValue,"_",resolutionValue,".png"),         
         width = widthValue,
         height = heightValue,
         units = "cm",limitsize = FALSE)
  ggsave(plot = dimplot_n,filename = paste0("../Results/DimPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",gsub(pattern = "[/]",replacement = "",datasetName), "_DimPlot_",pcaValue,"_",resolutionValue,".pdf"),         
         width = widthValue,
         height = heightValue,
         units = "cm",limitsize = FALSE)
  # Return plot
  dimplot_n
}
