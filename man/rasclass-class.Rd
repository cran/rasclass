\name{rasclass-class}
\docType{class}
\title{Class "rasclass"}

\alias{rasclass-class}
\alias{image,rasclass-method}
\alias{summary,rasclass-method}
\alias{View,rasclass-method}

\description{
This class object is a container for all the variables used in the classification algorithms of the \code{\link{rasclass}} package. The methods provided in the package sequentially fill the slots with data and results.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("rasclass")}.
}

\section{Slots}{
  \describe{
    \item{\code{path}:}{Object of class \code{\link{character}}. The path from which the data is loaded.}
    
    \item{\code{data}:}{Object of class \code{\link{data.frame}}. The dataframe that contains the data from the loaded ascii files.}
    
    \item{\code{samplename}:}{Object of class \code{\link{character}}. The name of a column in the \code{data} slot that will be used as sample in the supervised classification.}
    
    \item{\code{gridSkeleton}:}{Object of class \code{\link{rasclassRaster}}. The skeleton stores the common grid header of the input files. The gridSkeleton values are a special NAhandle vector storing the location of all the NA values in the input asciigrids to reduce memory usage.}
    
    \item{\code{predictedGrid}:}{Object of type \code{\link{rasclassRaster}}. The predicted grid resulting from the classification.}
    
    \item{\code{kappa}:}{Object of class \code{\link{numeric}}. The kappa accuracy coefficient of the classification, a measure for the accuracy of the classification.}
    
    \item{\code{accuracy}:}{Object of class \code{\link{matrix}}. The accuracy matrix of the classification, including user and producer accuracies.}
    
    \item{\code{call}:}{Object of class \code{\link{formula}}. The formula used for the the classification, relating the sample with the independent variables.}
    
    \item{\code{coefficients}:}{Object of class \code{\link{matrix}}. This slot will only be used in the multinomial logistic regression. The coefficients of the estimated model will be stored in this slot.}
    
    \item{\code{anova}:}{Object of class \code{\link{data.frame}}. Anova statistics testing the significance of each explanatory variable. This statistic can only be calculated in the multinomial regression classifier.}
    
    \item{\code{method}:}{Object of class \code{\link{character}}. Stores the type of classification that has been used.}
  }
}

\section{Methods}{
  \describe{
    \item{\link{accuracyAssessment}}{Calculates accuracy statistics of classification, including the confusion matrix and the kappa coefficient.}
	
	\item{\link{buildFormula}}{Builds a formula for classification, based on column names of the data in rasclass object.}

	\item{\link{checkRasclass}}{Checks internal consistency of rasclass object.}
	
    \item{\link{classifyMLC}}{Classification using a maximum likelihood classifier.}

    \item{\link{classifyMlogit}}{Classification using multinomial logistic regression.}

    \item{\link{readRasterFolder}}{Loads data from several asciigrid files in a folder.}
    
	\item{\link{setRasclassData}}{Sets data in rasclass object based on a dataframe.}

    \item{\link{summary}}{Prints a summary of the rasclass-object.}

    \item{\link{image}}{Plots the classified raster from the rasclass-object.}

    \item{\link{View}}{Shows the data frame in the rasclass-object.}    
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

\keyword{classes}
\keyword{raster classification}
\keyword{landcover}
\keyword{multinom}
