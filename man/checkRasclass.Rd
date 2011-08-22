\name{checkRasclass}
\alias{checkRasclass}
\alias{checkRasclass,rasclass-method}
\title{Check rasclass object for internal consistency}

\description{
This function checks wether a rasclass-class object is internally consistent.
}

\usage{checkRasclass(object)}

\arguments{
	\item{object}{
	A \code{\link{rasclass}} object.
	}
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
# Instanciate rasclass object
object <- new('rasclass')

# Create artificial data
mysample <- sample(c(NA, 1, 2, 3), 20000, rep = TRUE)
red <- sample(c(NA,1:255), 20000, rep = TRUE)
green <- sample(c(NA,1:255), 20000, rep = TRUE)
blue <- sample(c(NA,1:255), 20000, rep = TRUE)
newdata <- data.frame(mysample,red,green,blue)

# Prepare object using a dataframe and specifying raster properties
object <- setRasclassData(newdata, object, ncols = 100, nrows = 200,
	xllcorner = 0, yllcorner = 0, cellsize = 10, NAvalue = -9999,
	samplename = 'mysample')

# Change some values number of rows to an inconsitent value
object@gridSkeleton@nrows <- 12345
object@samplename <- 'wrongName'
checkRasclass(object)
}