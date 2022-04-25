#' Draw Feature Plot 
#' 
#' Function that generates feature plots for given features
#' 
#' @param seuratObject seurat data object
#' @param datasetName name of dataset to use on the saved objects
#' @param featureValues name of the features
#' @param reductionValue which dimensionality reduction method to use (default umap)
#' @param pointSizeValue size of the points (default 0.3)
#' @param nrowValue number of rows (default 1 for only one feature)
#' @param ncolValue number of colums (default 1 for only one feature)
#' @param widthValue width of the final plot (default 7 cm)
#' @param heightValue height of the final plot (default 7 cm)
#' 
#' @export drawFeaturePlot
#' 
#' @examples 
#' drawFeaturePlot(seuratObject = seuratLOG,feature = c("S100A9","NKG7","LDHB","CD79A"),nrowValue = 2,ncolValue = 2,datasetName = "test")

drawFeaturePlot <- function(seuratObject, featureValues,datasetName,reductionValue = "umap", widthValue = 7, heightValue = 7,pointSizeValue = 0.3,nrowValue = 1, ncolValue = 1){

  # Create empty plot list
  plotList <- list()
  
  # Count number of features to plot
  numberFeatures <- length(featureValues)
  
  # Initialize feature count
  numberFeature <- 0
  
  # Loop to generate single violin plots for each feature defined
  for (i in seq_along(featureValues)) {
    # Update step
    numberFeature <- numberFeature + 1
    
    # Print which feature is done
    print(paste0(numberFeature, " of ", numberFeatures," total features done. ", featureValues[i]))
    
    # Generate a single Violin plot
    singlePlot <- FeaturePlot(seuratObject, features = featureValues[i], 
                             label = F, 
                             label.size = 8,
                             pt.size = pointSizeValue,order = F,reduction = reductionValue)+ 
      theme(axis.line=element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.title = element_text(face = "italic")) 
    
    # Append plots to a list
    plotList <- append(plotList, list(singlePlot))
  }
  
  # Generate combined plot 
  dimplot_n <- ggarrange(plotlist = plotList, nrow = nrowValue,ncol = ncolValue)
  
  # Define which feature names to show on the filename
  if(numberFeatures == 1){featureValues <- featureValues}else{featureValues <- "Multiple"}
  
   # Create folder
  dir.create(file.path("../Results/FeaturePlot/"),recursive = T,showWarnings = F)
  
  # Redefine plot size based on the number of features shown
  widthValue <- 7 * ncolValue
  heightValue <- 7 * nrowValue
  
  # Save plot
  savePlots(plotObject = dimplot_n,
            plotTypeValue = "FeaturePlot",
            widthValue = widthValue,
            heightValue = heightValue,
            extraInfoValue = paste0("_",datasetName,"_",featureValues))
  
  # Return object
  dimplot_n
}