# Function that generates PCA plots for each PCA value
generatePCAs <- function(checkPCAs,dataset,mapTypeValue,metadata,specificPCA){
  datasetSeurat <- dataset
  if (checkPCAs) {
    # Generate plots for different PCAs
    i <- 2
    while (i < 25) {
      # Generate UMAP
      datasetSeurat <- createMap(dataset = dataset,mapType = mapTypeValue,pca = i,resolution = resolutionValue)
      # Add metadata
      if (metadataAvailable == TRUE) {
        datasetSeurat <- AddMetaData(object = datasetSeurat,metadata = metadata)
      }
      # Generate plot
      drawPCADimPlot(datasetSeurat, pca = i,resolution = resolutionValue,datasetName = datasetName)
      print(i)
      i <- i + 1
    }
    PCAvalue <- i
  }else{
    if (specificPCA != FALSE) {
      PCAvalue <- specificPCA
      # Generate UMAP
      datasetSeurat <- createMap(dataset = datasetSeurat, mapType = mapTypeValue,pca = PCAvalue,resolution = resolutionValue)
      
    }else{
      # Check optimal PCA value
      PCAvalue <- findOptimalPCA(datasetSeurat)
      # Generate UMAP
      datasetSeurat <- createMap(dataset = datasetSeurat, mapType = mapTypeValue,pca = PCAvalue,resolution = resolutionValue)
    }
    
  }
  datasetSeurat
}