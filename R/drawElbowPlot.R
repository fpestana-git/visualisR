
drawElbowPlot <- function(dataset, plotName){
  dataset <- dataset
  elbowPlot <- ElbowPlot(dataset,ndims = 30) + 
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 10))
  # Save graph
  ggsave(plot = elbowPlot,
         filename = paste0(resultsPath,format(Sys.time(), "%Y%m%d_%H%M%S"),"_ElbowPlot_", plotName,".png"),
         width = 9,
         height = 4.5,
         units = "cm")
  ggsave(plot = elbowPlot,
         filename = paste0(resultsPath,format(Sys.time(), "%Y%m%d_%H%M%S"),"_ElbowPlot_", plotName,".pdf"),
         width = 9,
         height = 4.5,
         units = "cm")
  elbowPlot
}