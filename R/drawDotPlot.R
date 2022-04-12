#' Draw Dot Plot 
#' 
#' Function that generates dot plots for a given Seurat dataset
#' 
#' @param dataset seurat data object
#' @param genesInterest define which features (genes) to show
#' @param splitby 
#' @param plotName define name of the plot
#' @param heightValue height of the final plot (default 6 cm)
#' @param widthValue width of the final plot (default 6 cm)


drawDotPlot <- function(dataset, genesInterest, splitby = NULL, plotName,widthValue = 10, heightValue = 10){
  # dataset <- dataset
  # genesInterest <- genesInterest
  # splitby <- splitby
  #dotcolors <- dotcolors
  dotPlotClusters <- DotPlot(dataset, 
                             features = genesInterest,
                             split.by = splitby,
                             scale = T,group.by = NULL) + 
    theme(axis.text.x = element_text(angle = 90, size = 7),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          axis.text.x.top = element_text(face = "italic"),
          axis.title.x.bottom = element_text(vjust = -10),
          axis.text.y = element_text(size = 7,face = "italic"),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          legend.position = "right",
          legend.box = "horizontal",
          legend.key.height = unit(0.3,"cm"))+
    labs(x="",y="") +
    scale_x_discrete(position = "top") +
    scale_y_discrete(position = "left") +
    coord_flip() 
  scale_colour_gradient2(low = "white", high = "blue")
  dotPlotClusters
  
  # Create folder
  dir.create(file.path("../Results/DotPlot/"))
  
  ggsave(plot = dotPlotClusters,
         filename = paste0("../Results/DotPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_DotPlot_",plotName, ".png"),
         width = widthValue, #28 default
         height = heightValue,
         units = "cm") # height was 4, width 15
  
  ggsave(plot = dotPlotClusters,
         filename = paste0("../Results/DotPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_DotPlot_",plotName,".pdf"),
         width = widthValue, #28 default
         height = heightValue,
         units = "cm") # height was 4
  
  dotPlotClusters
}