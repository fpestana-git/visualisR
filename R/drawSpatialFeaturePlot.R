#' Draw Spatial Feature Plot for given genes
#' 
#' Function that generates spatial feature plots for a given gene. Can generate for single samples or recursively to all samples
#' 
#' @param spatialObject spatial Seurat object to use
#' @param featureValue name of the feature to plot
#' @param recursiveSection if plots should be made for all sections (default FALSE)
#' @param sectionID if recursiveSection is FALSE, then a specific section ID should be defined (default NULL)
#' 
#' @export drawSpatialFeaturePlot

drawSpatialFeaturePlot <- function(spatialObject, featureValue, recursiveSection = FALSE,sectionID = NULL){
  # Setup Ident as section ID 
  Idents(object = spatialObject) <- spatialObject@meta.data[["sectionID"]]
  
  # Create folder
  dir.create(file.path("../Results/SpatialFeaturePlot/"),recursive = T,showWarnings = F)
  
  # Make spatial dimplot either for all sections or for a specific section
  if (recursiveSection) {
    for (i in levels(spatialObject)) {
      # Subset section
      individualSection <- subset(x  = spatialObject,idents = i)
      # Make plot
      individualSection <- SpatialFeaturePlot(object = individualSection,
                                              features = featureValue,
                                              stroke = 0, 
                                              pt.size.factor = 2) + ggplot2::scale_fill_continuous(low = "gray95", high = "Black") + NoLegend()
      # Save plot
      savePlots(plotObject = individualSection,
                plotTypeValue = "SpatialFeaturePlot",
                widthValue = 5,
                heightValue = 7.5,
                extraInfoValue = paste0("_",featureValue,"_",i))
      
    }
    
  }else {
    # Subset section
    individualSection <- subset(x  = spatialObject,idents = sectionID)
    # Make plot
    individualSection <- SpatialFeaturePlot(object = individualSection,
                                            features = featureValue,
                                            stroke = 0, 
                                            pt.size.factor = 2) + ggplot2::scale_fill_continuous(low = "gray95", high = "Black") + NoLegend()
    # Save plot
    savePlots(plotObject = individualSection,
              plotTypeValue = "SpatialFeaturePlot",
              widthValue = 5,
              heightValue = 7.5,
              extraInfoValue = paste0("_",featureValue,"_",sectionID))
  }
  # Return plot
  print(individualSection)
}