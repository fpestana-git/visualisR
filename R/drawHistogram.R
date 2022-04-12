

drawHistogram <- function(dataset, xlabel, ylabel,plotName, binWitdhValue = NULL,logValue = FALSE){
  # # Define label names
  # if (plotName == "NumberCells") {
  #   xlabel <- "Number of cells"
  #   ylabel <- "Number of genes"
  # }else if(plotName == "NumberGenes"){
  #   xlabel <- "Number of genes"
  #   ylabel <- "Number of cells"
  # }
  # 
  # Draw the histogram plot
  histplot <- ggplot(data=dataset, aes(if (logValue)log(dataset[,1]) else dataset[,1] )) + 
    geom_histogram(binwidth = binWitdhValue) +
    theme(panel.background = element_blank(),
          axis.line = element_line(color="black", size = 0.5)) +
    labs(x = xlabel, y = ylabel)
  
  
  #format(Sys.time(), "%Y-%m-%d %I-%p")
  # Create folder
  dir.create(file.path("Results/HistPlot/"),showWarnings = F)
  
  # Save the plots
  ggsave(plot = histplot,
         filename = paste0(resultsDirectory, "/HistPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_",plotName,".png"),
         width = 7,
         height = 5,
         units = "cm")
  ggsave(plot = histplot,
         filename = paste0(resultsDirectory, "/HistPlot/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_",plotName,".pdf"),
         width = 7,
         height = 5,
         units = "cm")
  
  histplot
}