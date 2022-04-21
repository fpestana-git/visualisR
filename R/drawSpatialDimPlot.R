#' Draw Spatial Plot 
#' 
#' Function that generates spatial dim plots for a given Seurat dataset
#' 
#' @param spatialObject spatial Seurat object to use
#' @param groupValue metadata variable to show on the plot
#' @param recursiveSection if plots should be made for all sections (default TRUE)
#' @param sectionID if recursiveSection is FALSE, then a specific section ID should be defined (default NULL)
#' 
#' @examples 
#' drawSpatialDimPlot(spatialObject = spatial,groupValue = "normCTXDistance_Bin5",recursiveSection = TRUE )
#' drawSpatialDimPlot(spatialObject = spatial,groupValue = "normCTXDistance_Bin5",recursiveSection = FALSE,sectionID = "B1_A1" )
#' 
#' @return This function generates and saves a spatial Dim plot in the Results folder and prints in the screen.
#' 
#' @export drawSpatialDimPlot

## add option for specific sample or recursive, if true, then pltos for all samples
drawSpatialDimPlot <- function(spatialObject,groupValue,recursiveSection = TRUE,sectionID = NULL){

  # Setup Ident as section ID 
  Idents(object = spatialObject) <- spatialObject@meta.data[["sectionID"]]
  
  # Create folder
  dir.create(file.path("../Results/SpatialDimPlot/"),recursive = T,showWarnings = F)
  
  
  # Make spatial dimplot either for all sections or for a specific section
  if (recursiveSection) {
    for (i in levels(spatialObject)) {
      # Subset section
      individualSection <- subset(x  = spatial,idents = i)
      # Make plot
      individualSection <- SpatialDimPlot(object = individualSection,
                             group.by = groupValue,
                             stroke = 0, 
                             pt.size.factor = 2) + NoLegend()
      # Save plot
      savePlots(plotObject = individualSection,
                plotTypeValue = "SpatialDimPlot",
                widthValue = 5,
                heightValue = 7.5,
                extraInfoValue = paste0("_",groupValue,"_",i))
      
    }
      
    }else {
      # Subset section
      individualSection <- subset(x  = spatial,idents = sectionID)
      # Make plot
      individualSection <- SpatialDimPlot(object = individualSection,
                                 group.by = groupValue,
                                 stroke = 0, 
                                 pt.size.factor = 2) + NoLegend()
      # Save plot
      savePlots(plotObject = individualSection,
               plotTypeValue = "SpatialDimPlot",
               widthValue = 5,
               heightValue = 7.5,
               extraInfoValue = paste0("_",groupValue,"_",sectionID))
  }
  
  print(individualSection)
}