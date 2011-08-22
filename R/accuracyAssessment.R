##################################################################################
# Set Method: accuracyAssessment
##################################################################################
accuracyAssessment <- function(object){}
setMethod('accuracyAssessment', signature(object = 'rasclass'),

function(object){
	
	# Check if there is a predicted Grid
	if(length(object@predictedGrid@grid) == 1){
		stop('No predicted grid specified')
	}

	# Nr of Categories
	Categories <- as.numeric(levels(as.factor(object@data[object@samplename][,1])))
	nrOfCategories <- length(Categories)
	
	accuracytable <- matrix(rep(rep(NA, nrOfCategories), nrOfCategories), ncol = nrOfCategories)
	
	reduced <- na.omit(data.frame(object@data[object@samplename], na.omit(object@predictedGrid@grid)))
	names(reduced) <- c('sample','predicted')
	
	for(i in 1:nrOfCategories){
		for(j in 1:nrOfCategories){
			accuracytable[i,j] <- sum((reduced$sample == Categories[i]) * (reduced$predicted == Categories[j]))
		}
	}
	accuracytable <- as.data.frame(accuracytable)
	rownames(accuracytable) <- paste('Sample', as.character(Categories))
	colnames(accuracytable) <- paste('Predicted', as.character(Categories))

	# Kappa coefficient	
	thissum = sum(rowSums(accuracytable)*colSums(accuracytable))
	n <- sum(rowSums(accuracytable))
	diag <- sum(diag(as.matrix(accuracytable)))
	object@kappa <- (n*diag - thissum)/(n^2 - thissum)
	
	# Accuracytable (including User's and Producer's accuracy)
	produceraccuracy <- as.matrix(accuracytable/rowSums(accuracytable))
	useraccuracy     <- as.matrix(accuracytable/colSums(accuracytable))
	
	accuracytable <- cbind(accuracytable, diag(produceraccuracy))
	colnames(accuracytable)[nrOfCategories+1] <- 'Producer Acc'

	accuracytable <- rbind(accuracytable,append(diag(useraccuracy),c(NA)))
	rownames(accuracytable)[nrOfCategories+1] <- 'User Acc'
	
	object@accuracy <- as.matrix(accuracytable)

	object	
}
)
