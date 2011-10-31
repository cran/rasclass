##################################################################################
# Set Method: rasclassMLC
##################################################################################
rasclassMlc <- function(rasclassObj){}
setMethod('rasclassMlc', signature(rasclassObj = 'rasclass'), definition = 

function(rasclassObj){

	# Create required Lists and varaibles
	varlist    <- strsplit(as.character(rasclassObj@formula),' ~ ', TRUE)[[1]]
	samplename <- varlist[1]
	varlist    <- unlist(strsplit(as.character(varlist[2]),' + ', TRUE))
	
	# Check consistency of input
	if((sum(sapply(varlist, function(x) !is.element(x, names(rasclassObj@data)))) != 0) | (!is.element(samplename, names(rasclassObj@data)))){
		stop('Formula not consistent with data frame names')
	}
	
	# Calculate parameters of the multivariate normal distribution
	cat('classifying... ')
	coefs <- list()
	if(is.null(rasclassObj@training)){
		byClass <- split(rasclassObj@data[, varlist], rasclassObj@data[, samplename])
		samplesize <- length(na.omit(rasclassObj@data[, samplename]))
	} else {
		byClass <- split(rasclassObj@data[rasclassObj@training, varlist], rasclassObj@data[rasclassObj@training, samplename])
		samplesize <- sum(rasclassObj@training)
	}
	
	classes <- as.numeric(names(byClass))
	for(cat in names(byClass)){
		frame <- byClass[[cat]]
		
		# Calculate parameters of the multivariate normal distribution
		prior <- log(nrow(frame)/samplesize)
		meanVector  <- colMeans(frame)
		determinant <- try(log(det(cov(frame))), silent = TRUE)
		inverseCov <- try(solve(cov(frame)), silent = TRUE)
		if(class(determinant) == 'try-error'){
			determinant <- 0
		}
		if(class(inverseCov) == 'try-error'){
			inverseCov <- diag(0, nrow = ncol(frame))
		}
		coefs[[cat]] <- list(prior, determinant, meanVector, inverseCov)
	}
	rm(byClass)
	gc()

	# Predict grid values based on probabilities
	cat('predicting...\n')
	dataVars <- as.matrix(rasclassObj@data[, varlist])
	predicted <- rep(NA, nrow(dataVars))
	probs <- rep(NA, length(classes))

	for(i in 1:nrow(dataVars)){
		for(j in 1:length(probs)){
			delta <- dataVars[i,] - coefs[[j]][[3]]
			probs[j] <- coefs[[j]][[1]] - coefs[[j]][[2]] - t(delta)%*%coefs[[j]][[4]]%*%delta
		}
		predicted[i] <- classes[probs==max(probs)]
	}
	
	#Return predicted vector
	out <- list()
	out[[1]] <- coefs
	out[[2]] <- predicted
	out
}
)
