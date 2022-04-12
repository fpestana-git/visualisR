# Pie chart cells/condition
drawPieChart <- function(column_name, metadata){
  dataset_metadata <- metadata
  metadataFile <<-data.frame(table(dataset_metadata[,..column_name] ))
  numberObservations <- nrow(dataset_metadata)
  piechart <- ggplot(metadataFile, aes(x="", y=Freq, fill=Var1))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x=element_blank(),
          legend.key.size = unit(0.5,"cm"),
          plot.title = element_text(hjust = 0.5,size = 10,margin = margin(b = -10),face = "italic")) +
    labs(x="",y="",title = paste0(numberObservations," cells"), fill = "") +
    guides(fill=guide_legend(ncol=3))
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
  #piechart
  # Save graph
  ggsave(plot = piechart,
         filename = paste0(resultsPath,format(Sys.time(), "%Y%m%d_%H%M%S"),"_PieChart_", column_name,".png"),
         width = 16,
         height = 5,
         units = "cm")
  ggsave(plot = piechart,
         filename = paste0(resultsPath,format(Sys.time(), "%Y%m%d_%H%M%S"),"_PieChart_", column_name,".pdf"),
         width = 16,
         height = 5,
         units = "cm")
  piechart
}
