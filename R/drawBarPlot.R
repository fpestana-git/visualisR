

# Function to generate BarPlots
drawBarPlot <- function(dataset, datasetName,heightValue = 5.5,widthValue = 11, resultsFolder = resultsDirectory,typeBarPlot = "normal"){
  if(typeBarPlot == "normal"){
    barPlot <- ggplot(data = dataset, aes(fill=Var1, y=Freq, x=Var1)) + 
      geom_bar(position="stack", stat="identity") + NoLegend() +
      theme(panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.text = element_text(size = 18),
            axis.line = element_line(color = "black"),
            axis.title = element_text(size = 18)) +
      labs(y = "Number of cells")
  }
  
  if(typeBarPlot == "stacked_percentage"){
    barPlot <- ggplot(data = dataset, aes(fill=Var2, y=Freq, x=Var1)) + 
      geom_bar(position="fill", stat="identity") + NoLegend() +
      theme(panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.text = element_text(size = 18),
            axis.line = element_line(color = "black"),
            axis.title = element_text(size = 18)) +
      labs(y = "Number of cells")
  }
  
  
  # Create folder
  dir.create(file.path(resultsFolder,"/BarPlot/"))
  
  
  # Save graph in png and pdf format
  ggsave(plot = barPlot,filename = paste0(resultsFolder, "/BarPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",datasetName, "_BarPlot.png"),         
         width = widthValue,
         height = heightValue,
         units = "in",limitsize = FALSE)
  ggsave(plot = barPlot,filename = paste0(resultsFolder, "/BarPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",datasetName, "_BarPlot.pdf"),         
         width = widthValue,
         height = heightValue,
         units = "in",limitsize = FALSE)
  # Return plot
  barPlot
}