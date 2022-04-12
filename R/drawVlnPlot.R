#' Draw Violin Plot 
#' 
#' Function that generates violin plots for N number of indicated features
#' 
#' @param dataset seurat data object
#' @param feature list of features to be plotted
#' @param splitby if violins are to be splitted between an indicated metadata variable
#' @param pointSize define the size of the dots in the plot. 0 indicates no points should be shown
#' @param colorVector vector of colors to manual color each violin
#' @param heightValue height of the final plot (default 6 cm)
#' @param widthValue width of the final plot (default 6 cm)
#' @param nrowValue number of rows in the final panel
#' @param ncolValue number of columns in the final panel
#' @param fontsizeValue size of text in y axis
#' @param addXlabel if x axis ticks should be labeled

drawVlnPlot <- function(dataset, feature = featureList,plotName = featureGroup, orderIdentity = orderIdentity, identity = identityValue,splitby = NULL,datasetName,pointSize = 0.1, colorVector = NULL,heightValue = 6,widthValue = 12, nrowValue = 2, ncolValue = 1,fontsizeValueX = 15,fontsizeValueY=15, addXlabel = T,resultsFolder = resultsDirectory){
  # Change main identities
  Idents(dataset) <- dataset@meta.data[[identityValue]]
  
  # Relevel object@ident
  dataset@active.ident <- factor(x = dataset@active.ident, levels = orderIdentity)
  
  # # Rename cell clusters
  # if(identityValue == "cellType_L1"){
  #   dataset <- RenameIdents(object = dataset, 
  #                           `NPC/IPC` = "IPC", 
  #                           `Excitatory Neurons` = "ExN", 
  #                           `Inhibitory Neurons` = "InN",
  #                           `NEP/RGC` = "RGC",
  #                           `Astrocytes` = "Astro")
  # }
  # 
  
  print(length(feature))
  print(paste0("The number of cell clusters is ",length(Idents(dataset))))
  numberFeatures <- length(feature)
  print(paste0("Number of features is ",numberFeatures))
  heightValue <- 3 * length(feature)
  print(paste0("The height of the plot is ",heightValue))
  
  if (widthValue == 12) {
    widthValue <- 2 * nrow(table(Idents(dataset)))
  }
  print(paste0("The width of the plot is ", widthValue))
  nrowValue <- length(feature)
  print(plotName)
  # Initiate empty list
  plotList <- list()
  numberFeature <- 0
  
  # Loop to generate single violin plots for each feature defined
  for (i in seq_along(feature)) {
    #print(feature[i][[i]])
    # Update step
    numberFeature <- numberFeature + 1
    print(paste0(numberFeature, " of ", numberFeatures," total features done. ", feature[i]))
    
    # Which feature will be plotted
    #print(feature[i])
    
    # Generate a single Violin plot
    singlePlot <- VlnPlot(object = dataset,
                          features = feature[i],
                          pt.size = pointSize, 
                          split.by = splitby,
                          cols = colorVector) + NoLegend() +
      theme(axis.text.x = element_text(size = fontsizeValueX, angle = 0, hjust = 0.3),
            axis.text.y = element_text(angle = 90),
            axis.title.y = element_text(size = fontsizeValueY,angle = 90,vjust = 0.5, face = "italic"),plot.margin=unit(c(-0.5,0.25,-0.5,0.25), "cm")#,
            #plot.title = element_text(size = 10, face = "italic",vjust = -6)
      ) +
      #labs(x="", y="Expression") +
      labs(x="", y = feature[i], title = "") +
      scale_y_continuous(labels = scales::number_format(accuracy = 1), n.breaks = 3,breaks = waiver()) +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = if (addXlabel & numberFeature == numberFeatures) element_text() else element_blank(),
            axis.line.x = element_blank())
    

    
    # Determine minimum and maximum expression values for the feature plotted
    maxValue <- as.numeric(max(singlePlot$data[,1], na.rm = TRUE)) # WORKS
    minValue <- as.numeric(min(singlePlot$data[,1], na.rm = TRUE)) # WORKS
    
    # Changes the y axis to only show the minimum and maximum values above defined
    singlePlot <- singlePlot +   scale_y_continuous(labels = scales::number_format(accuracy = 1), n.breaks = 2,breaks = c(minValue,maxValue))
    
    # Append plots to a list
    plotList <- append(plotList, list(singlePlot))
  }
  
  # Generate combined plot 
  p <- ggarrange(plots = plotList, nrow = nrowValue,ncol = ncolValue)
  
  # Create folder
  dir.create(file.path("../Results/VlnPlot/"))
  
  # Save plot in png format
  ggsave(plot = p,
         filename = paste0("../Results/VlnPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",datasetName,"_", plotName,".png"),
         width = widthValue,
         height = heightValue,
         units = "cm",limitsize = FALSE)
  
  # Save plot in pdf format
  ggsave(plot = p,
         filename = paste0("../Results/VlnPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",datasetName,"_", plotName,"_ViolinPlot.pdf"),
         width = widthValue,
         height = heightValue,
         units = "cm",limitsize = FALSE)
  
  # Return plot
  p
  
}