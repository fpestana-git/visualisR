% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drawSpatialFeaturePlot.R
\name{drawSpatialFeaturePlot}
\alias{drawSpatialFeaturePlot}
\title{Draw Spatial Feature Plot for given genes}
\usage{
drawSpatialFeaturePlot(
  spatialObject,
  featureValue,
  recursiveSection = FALSE,
  sectionID = NULL
)
}
\arguments{
\item{spatialObject}{spatial Seurat object to use}

\item{featureValue}{name of the feature to plot}

\item{recursiveSection}{if plots should be made for all sections (default FALSE)}

\item{sectionID}{if recursiveSection is FALSE, then a specific section ID should be defined (default NULL)}
}
\description{
Function that generates spatial feature plots for a given gene. Can generate for single samples or recursively to all samples
}
