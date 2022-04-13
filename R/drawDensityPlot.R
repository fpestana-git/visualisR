#' Draw Density Plot 
#' 
#' Function that generates density plots for a given data
#' 
#' @param metadata seurat data object
#' @param cellType name of dataset to use on the saved objects
#' @param heightValue height of the final plot (default 6 cm)
#' @param widthValue width of the final plot (default 6 cm)
#' 
#' @export drawDensityPlot

# Function that draws density plots
drawDensityPlot <- function(metadata, cellType,widthValue = 4,heightValue = 9.8,resultsFolder = resultsDirectory){
  # Generate density data
  densityData <- metadata[metadata$LIGER_iNMF_label == cellType,]
  
  
  singleDensityPlot <- ggplot(densityData,aes(x = -normCTXDistance)) + 
    geom_density(color="black", fill="lightblue") + 
    labs(title = cellType,x= "",y="") + 
    coord_flip() + 
    theme(panel.background = element_blank(), 
          axis.line.y  = element_line()) + 
    scale_y_discrete( expand = c(0, 0))
  
  # Create folder
  dir.create(file.path(resultsFolder,"/DimPlot/"))
  
  # Save graph in png and pdf format
  ggsave(plot = singleDensityPlot,filename = paste0(resultsFolder,"/DensityPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",gsub(pattern = "/",replacement = "",x = cellType),".png"),         
         width = widthValue,
         height = heightValue,
         units = "cm",limitsize = FALSE)
  ggsave(plot = singleDensityPlot,filename = paste0(resultsFolder,"/DensityPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",gsub(pattern = "/",replacement = "",x = cellType),".pdf"),         
         width = widthValue,
         height = heightValue,
         units = "cm",limitsize = FALSE)
  
  singleDensityPlot
  
}