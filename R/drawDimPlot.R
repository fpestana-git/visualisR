
# Function to generate DimPlots
drawDimPlot <- function(dataset, datasetName, pca, mapType = "umap", resolution,splitValue = NULL, heightValue = 10,widthValue = 10,colorVector = NULL,cells2highlight = NULL, sizeHighlightedCells = NULL,groupbyValue = NULL,labelValue = F,showLegend =FALSE, resultsFolder = resultsDirectory, plotMinimal = FALSE){
  dimplot_n <- DimPlot(dataset, reduction = mapType, 
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
  
  # Create folder
  dir.create(file.path(resultsFolder,"/DimPlot/"))
  
  # Save graph in png and pdf format
  ggsave(plot = dimplot_n,filename = paste0(resultsFolder, "/DimPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",gsub(pattern = "[/]",replacement = "",datasetName), "_DimPlot_",pca,"_",resolution,".png"),         
         width = widthValue,
         height = heightValue,
         units = "cm",limitsize = FALSE)
  ggsave(plot = dimplot_n,filename = paste0(resultsFolder, "/DimPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",gsub(pattern = "[/]",replacement = "",datasetName), "_DimPlot_",pca,"_",resolution,".pdf"),         
         width = widthValue,
         height = heightValue,
         units = "cm",limitsize = FALSE)
  # Return plot
  dimplot_n
}
