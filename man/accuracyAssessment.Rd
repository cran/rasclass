\name{accuracyAssessment}
\alias{accuracyAssessment}
\alias{accuracyAssessment,rasclass-method}

\title{Accuracy Assessment of the Classification}

\description{
This function calculates the standard accuracy assessment of a classified grid in a \code{\link{rasclass}} object.
}

\details{
The accuracy assessment includes the confusion matrix, the user and producer accuracies and the kappa coefficient. The confusion matrix is combined into one table accuracy matrix containing the user and producer accuracies. The combined table is stored in the \code{accuracy} slot of the \code{rasclass-class} object. The kappa coefficient is stored in the \code{kappa} slot.
}

\usage{accuracyAssessment(object)}

\arguments{
  \item{object}{{A \code{\link{rasclass-class}} object}.}
 }

\value{
A \code{\link{rasclass-class}} object, containing the newly calculated accuracy matrix and kappa-coefficient.
}

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

\keyword{raster}
\keyword{classifier}
\keyword{landcover}
\keyword{multinom}

\references{
Foody, G. M. (2002) Status of land cover classification accuracy assessment. \emph{Remote Sensing of Environment} 80(1):185-201.
}