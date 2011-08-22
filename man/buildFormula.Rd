\name{buildFormula}
\alias{buildFormula}
\alias{buildFormula,rasclass-method}
\title{Build a formula for Raster Classification}

\description{
This function builds a formula from the dataframe from the \code{data} slot in the specified rasclass object.
}

\usage{buildFormula(object, varlist = NULL)}

\arguments{
  \item{object}{A \code{\link{rasclass-class}} object with non-empty \code{data} and \code{samplename} slots.}
  
  \item{varlist}{An optional \code{character} vector containing the names of the variables that shall be used in the classification algorithms.}
}

\details{
A formula is built automatically using all the columns in the dataframe from the \code{data} slot of the specified object. The formula is stored in the \code{call} slot of the \code{\link{rasclass-class}} object. The dependent variable in the formula will be the name specified in the \code{samplename} slot of the given input object.

If not all columns from the \code{data} slot should be used, specify the list of variables to include using the optional argument \emph{varlist}.
}

\value{
A \code{\link{rasclass-class}} object with the newly built formula in the \code{call} slot.
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
