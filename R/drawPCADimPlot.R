# Function to plot PCA dimplots
drawPCADimPlot <- function(dataset,pca, resolution,datasetName, resultsPath, groupbyValue = NULL ){
  dimplot_n <- DimPlot(dataset, reduction = "umap", 
                       label = T, 
                       label.size = 5,
                       pt.size = 0.5,
                       group.by = groupbyValue)+ NoLegend()+
    theme(axis.line=element_blank(),
          panel.border = element_rect(colour = "black"))  +
    scale_y_continuous(breaks=c(-10,-5, 0, 5, 10,20)) +
    scale_x_continuous(breaks=c(-10,-5,0, 5,10,20)) + labs(title = paste0("PCA ", pca))
  # theme(axis.line=element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.text = element_blank()) 
  
  
  # Save graph
  ggsave(plot = dimplot_n,
         filename = paste0("../Results/PCAplots/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_",datasetName,"_PCADimPlot_",pca,"_",resolution,".png"),
         width = 5.5,
         height = 5.5,
         units = "in")
  ggsave(plot = dimplot_n,
         filename = paste0("../Results/PCAplots/",format(Sys.time(), "%Y%m%d_%H%M%S"),"_",datasetName,"_PCADimPlot_",pca,"_",resolution,".pdf"),
         width = 5.5,
         height = 5.5,
         units = "in")
  
  #save_plot_pdf <- function(prob_threshold, geneName){
  #pdf(file=paste0("/Users/u0117021/Documents/Allen/Genes/", geneName,".pdf"),
  #    width = 3, height = 4)
  #scatterPlot_m(prob_threshold = prob_threshold, geneName = geneName)
  #}
  
  dimplot_n
}