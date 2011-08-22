\name{rasclass-package}
\alias{rasclass}
\alias{rasclass-package}
\docType{package}
\title{Supervised Raster Image Classification}

\description{
This package is built to perform supervised, per-pixel based image classification using raster data.
}

\details{
The raster image classification can be performed by calling a sequence of functions that load data, calculate the classification grid and produce an accuracy assessment of the classification.

The package provides function to load external data from a folder containing raster images in the ESRI asciigrid format (\emph{.asc} file extension). All files in the specified folder are parsed and stored. The requirements on the input raster files is that they are all in the same projection, are aligned and have the same grid-size (i.e. all raster files should have the same header).  Furthermore one raster file has to be specified as sample data.

For the classification, the package contains two supervised, per-pixel classification methods: A classifier using Multinomial Logistic Regression and a Bayesian Maximum Likelihood Classifier (MLC). The output of the classifications is the classified raster grid.

Standard accuracy assessment variables can be calculated. The assessment includes user and producer accuracies, the confusion matrix and the kappa coefficient.
}

\author{Daniel Wiesmann \email{daniel.wiesmann@ist.utl.pt} and David Quinn \email{djq@mit.edu}}

\seealso{
\code{\link{accuracyAssessment}},
\code{\link{buildFormula}},
\code{\link{checkRasclass}},
\code{\link{classifyMLC}},
\code{\link{classifyMlogit}},
\code{\link{rasclass-class}},
\code{\link{rasclassRaster-class}},
\code{\link{readRaster}},
\code{\link{readRasterFolder}},
\code{\link{setRasclassData}},
\code{\link{writeRaster}}
}

\examples{
\dontrun{
# Load data from external folder
object <- readRasterFolder(path = "mypath", samplename = "mysample",
	filenames = c('myvar1.asc', 'myvar2.asc'))
}

# For this example, get data from a random data frame
mysample <- sample(c(NA, 1, 2, 3), 20000, rep = TRUE)
red <- sample(c(NA,1:255), 20000, rep = TRUE)
green <- sample(c(NA,1:255), 20000, rep = TRUE)
blue <- sample(c(NA,1:255), 20000, rep = TRUE)
newdata <- data.frame(mysample,red,green,blue)

# Prepare object using the dataframe and specifying raster properties
object <- new('rasclass')
object <- setRasclassData(newdata, object, ncols = 100, nrows = 200,
	xllcorner = 0, yllcorner = 0, cellsize = 10, NAvalue = -9999,
	samplename = 'mysample')

# Classify and compute accuracy
object <- classifyMlogit(object, anova = TRUE)
object <- accuracyAssessment(object)

# Summarize and plot results
summary(object)
image(object@predictedGrid)
}

\keyword{package}
\keyword{classification}
\keyword{raster}
\keyword{ascii}
\keyword{landcover}
\keyword{multinom}
\keyword{MLC}
