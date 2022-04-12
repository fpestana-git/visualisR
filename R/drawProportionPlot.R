# This function computes the number of cells for each metadata variable and for all the different studies
# It outputs a dataframe with:
## First column named Var1 - metadata variable values
## Second column named Freq with the number of cells that have that variable assigned
## Third column named Study with the name of the study where the cells belong
# This dataframe will be the input to draw plots
computeCellProportions <- function(referenceNameFile = "../celltypes.csv", metadataVariable = "cellType_L1"){
  #referenceNameFile = "Documents/_Drylab/age.csv"
  #metadataVariable = "age"
  # Load cell type common meta names
  referenceNames <- fread(referenceNameFile)
  
  # Initiate empty proportion object
  proportion <- data.table(Var1 = character(), Freq = integer(), Study = character())
  
  # Gets the reference cell type names
  referenceNames <- unique(referenceNames$NewNames)

  # Computes proportions for each study metadata
  for (i in seq_along(allMetadata)) {
    # Gets metadata and name of study to analyze
    metadata <- allMetadata[[i]]
    studyName <- names(allMetadata[i])
    
    # Computes proportion (what if the column is not available?)
    if (metadataVariable %in% names(allMetadata[[i]])) {
      singleStudyProportion <- as.data.frame(table(metadata[,metadataVariable]))
      
      # Add column with the number of the study
      singleStudyProportion$Study <- studyName
      
      # Check which cell types are not included in the metadata being analyzed
      first <- unfactor(singleStudyProportion$Var1)
      second <- referenceNames
      onlysecond <- second[!second %in% first]
      
      # Iterates through the reference cell type names and adds to the cell proportion dataframe
      for (j in seq_along(onlysecond)) {
        newRow <- data.table(Var1 = onlysecond[j],
                             Freq = 0,
                             Study = studyName)
        # Add to dataframe
        singleStudyProportion <- rbind(singleStudyProportion,newRow)
        # Sort
        singleStudyProportion <- singleStudyProportion[order(singleStudyProportion$Var1),]
        
      }
    } else {
      #singleStudyProportion <- data.table(Var1 = c(referenceNames,"Not defined"), Freq = c(0,1), Study = studyName)
      
      singleStudyProportion <- data.table(Var1 = referenceNames, Freq = 0, Study = studyName)
      singleStudyProportion <- rbind(singleStudyProportion, data.table(Var1 = "Not defined",Freq = 1, Study = studyName))
    }
    
    # Concatenate to proportion dataframe
    proportion <- rbind(proportion, singleStudyProportion)
  }
  
  # Make sure the plots are done in the correct order
  proportion$Study <- factor(proportion$Study,levels = unique(proportion$Study))
  
  # Return proportions
  proportion
}


# This function plots horizontal bar plots with the proportions of cells belonging specific metadata variables (cellType_L1, sex, etc)
plotProportions <- function(proportions, metadataVariable,referenceNameFile){
  # Calculate number of studies
  numberStudies <- length(unique(proportions$Study))
  
  # Load cell type common meta names
  referenceNames <- fread(referenceNameFile)
  
  # Gets correct order for ages
  levelsVariables <- rev(unique(referenceNames$NewNames))
  
  # Generates plot
  combinedPlot <- ggplot(data = proportions, aes(x = Study, y = Freq, fill = factor(Var1, levels = levelsVariables))) + 
    geom_bar(position = "fill", stat = "identity") + 
    coord_flip() +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),legend.position = "bottom") +
    scale_fill_discrete(name = "", guide = guide_legend(reverse = T)) 
  
  # Extracts legend
  legend <- get_legend(combinedPlot)
  
  # Removes legend from plot
  combinedPlot <- combinedPlot+ NoLegend()
  
  
  # Create folder
  dir.create(file.path("../Results/BarPlot/"))
  
  # Save plot in png format
  ggsave(plot = combinedPlot,
         filename = paste0("../Results/BarPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",metadataVariable,"_proportions.png"),
         width = 55,
         height = numberStudies * 5,
         units = "cm",limitsize = FALSE)
  
  # Save plot in pdf format
  ggsave(plot = combinedPlot,
         filename = paste0("../Results/BarPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",metadataVariable,"_proportions.pdf"),
         width = 55,
         height = numberStudies * 5,
         units = "cm",limitsize = FALSE,device = cairo_pdf)
  
  ggsave(plot = legend,
         filename = paste0("../Results/BarPlot/", format(Sys.time(), "%Y%m%d_%H%M%S"),"_",metadataVariable,"_proportions_legend.png"),
         width = 15,
         height =  5,
         units = "cm",limitsize = FALSE)
  
  # Return graph
  combinedPlot
}