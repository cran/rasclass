\name{classifyMlogit}
\alias{classifyMlogit}
\alias{classifyMlogit,rasclass-method}
\title{Multinomial Logit Classsifier}

\description{
A function to classify raster data using Multinomial Logistic Regression.
}

\usage{classifyMlogit(object, anovatable = FALSE, splitfraction = 1)}

\arguments{
  \item{object}{A \code{\link{rasclass-class}} object containing the data and a built formula.}
  
  \item{anovatable}{An optional \code{logical} variable specifying if anova statistics should be calculated.}
  
  \item{splitfraction}{An optional \code{numeric} argument specifying a fraction to split data.}    
}

\details{
Data is classified using Multinomial Logistic Regression and the results are stored in the \code{predictedGrid} slot and the coefficients of the regression in the \code{coefficients} slot. The regression is calculated using the \code{\link[nnet]{multinom}} function from the \code{\link[nnet]{nnet}} package. The formula to classify the data is taken from the \code{call} slot of the input rasclass object. 

If the argument \code{anovatable} is set \code{TRUE}, the Analysis of Variance (anova) table is calculated. For large datasets, this will significantly increase the runtime of the function.

The optional argument \code{splitfraction} allows splitting the data randomly into two parts with the fraction specified. The classification model will then be trained on one fraction and the other fraction will be used for prediction. This allows testing the preformance of the classification on data that was not an input for training the model.
}

\value{A \code{\link{rasclass-class}} object, containing the classified raster in the predictedGrid slot.}

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

# For this example, get data from a random dataframe
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

\keyword{raster classification}
\keyword{landcover}
\keyword{multinom}