# Pie chart cells/condition
drawscatterPlot <- function(metadata, labeling){
  dataset_metadata <- metadata
  labeling <- labeling
  scatterPlot <- ggplot(dataset_metadata, aes(x=PositionX, y=PositionY, color = labeling), size = 0.01)+ #fill?
    geom_point(stat = "identity") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.key.size = unit(1,"cm"),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(size = 20),
          axis.title = element_text(size = 30),
          plot.title = element_text(hjust = 0.5,size = 20,margin = margin(b = -10),face = "italic")) +
    guides(colour = guide_legend(override.aes = list(size=10))) #+
  #scale_color_grey() +
  #scale_color_manual(values=c("#DCDCDC", "#000000"))
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
  #piechart
  # Save graph
  ggsave(plot = scatterPlot,
         filename = paste0(resultsPath,format(Sys.time(), "%Y%m%d_%H%M%S"),"_scatterPlot.png"),
         width = 30,
         height = 30,
         units = "cm")
  ggsave(plot = scatterPlot,
         filename = paste0(resultsPath,format(Sys.time(), "%Y%m%d_%H%M%S"),"_scatterPlot.pdf"),
         width = 30,
         height = 30,
         units = "cm")
  scatterPlot
}