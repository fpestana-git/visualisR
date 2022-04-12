#' Draw Feature Plot 
#' 
#' Function that generates feature plots for given features
#' 
#' @param seuratObject seurat data object
#' @param datasetName name of dataset to use on the saved objects
#' @param featureNames name of the features
#' @param reductionValue which dimensionality reduction method to use (default umap)
#' @param pointSizeValue size of the points (default 0.3)
#' @param nrowValue number of rows (default 1 for only one feature)
#' @param ncolValue number of colums (default 1 for only one feature)

drawFeaturePlot <- function(seuratObject, featureNames,datasetName,reductionValue = "umap", widthValue = 5, heightValue = 5,pointSizeValue = 0.3,nrowValue = 1, ncolValue = 1){

  #feature <- feature
  #FeaturePlot(dataset,features = feature)
  featureName <- "test"
  # dimplot_n <- FeaturePlot(dataset, features = feature, 
  #                          label = F, 
  #                          label.size = 5,
  #                          pt.size = pointSizeValue,order = T,reduction = reductionValue)+ 
  #   theme(axis.line=element_blank(),
  #         axis.ticks = element_blank(),
  #         axis.text = element_blank(),
  #         plot.title = element_text(face = "italic")) 
  # dimplot_n
  # 
  plotList <- list()
  numberFeatures <- length(feature)
  numberFeature <- 0
  # Loop to generate single violin plots for each feature defined
  for (i in seq_along(feature)) {
    #print(feature[i][[i]])
    # Update step
    numberFeature <- numberFeature + 1
    print(paste0(numberFeature, " of ", numberFeatures," total features done. ", feature[i]))
    
    # Which feature will be plotted
    #print(feature[i])
    
    # Generate a single Violin plot
    singlePlot <- FeaturePlot(seuratObject, features = feature[i], 
                             label = F, 
                             label.size = 5,
                             pt.size = pointSizeValue,order = F,reduction = reductionValue)+ 
      theme(axis.line=element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.title = element_text(face = "italic")) 
    
    # Append plots to a list
    plotList <- append(plotList, list(singlePlot))
  }
  
  # Generate combined plot 
  dimplot_n <- ggarrange(plots = plotList, nrow = nrowValue,ncol = ncolValue)
  
  
  
  
  # Create folder
  dir.create(file.path(resultsFolder, "/FeaturePlot/"),showWarnings = F)
  
  # Save graph
  ggsave(plot = dimplot_n,
         filename = paste0(resultsFolder, "/FeaturePlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_",datasetName, "_FeaturePlot_",featureName,".png"),
         width = widthValue,
         height = heightValue,
         units = "cm",limitsize = FALSE)
  ggsave(plot = dimplot_n,
         filename = paste0(resultsFolder, "/FeaturePlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_",datasetName,"_FeaturePlot_",featureName,".pdf"),
         width = widthValue,
         height = heightValue,
         units = "cm",limitsize = FALSE) # height was 4
  print("Multiplot done")
  #dimplot_n
}