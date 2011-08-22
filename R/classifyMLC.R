##################################################################################
# Set Method: classifyMLC
##################################################################################
classifyMLC <- function(object, splitfraction = 1){}

setMethod('classifyMLC', signature(object = 'rasclass'),

function(object, splitfraction = 1){
		
	# Control for Forumla
	if(is.null(object@call)){
		stop('Please build or specify formula in rasclass object')
	}

	# Check consistency
	if(!checkRasclass(object)){
		stop('Data object is not consistent, check data and try again')
	}

	# Set Method type
	object@method <- 'MLC'
	cat('Classification using MLC.\n')
	
	##################################################################################
	# Split data in two parts
	if(splitfraction < 1){
		
		object@method <- paste(object@method, paste('Data Split with', splitfraction))

		object@data <- na.omit(object@data)
		
		data2Estimate <- NULL
		data2Predict  <- NULL

		for(templevel in levels(factor(object@data[object@samplename][,1]))){
			
			splitnr <- nrow(object@data[object@data[object@samplename] == templevel, ])			
			splitvect <- append(rep(TRUE, max(1, round(splitnr*splitfraction))), rep(FALSE, splitnr - max(1, round(splitnr*splitfraction))))
			splitvect <- sample(splitvect)
			
			data2Estimate <- rbind(data2Estimate, object@data[object@data[object@samplename] == templevel, ][ splitvect, ])
			data2Predict  <- rbind(data2Predict,  object@data[object@data[object@samplename] == templevel, ][!splitvect, ])
			
			# Update gridSkeleton
			object@gridSkeleton@grid <- as.integer(object@gridSkeleton@grid * !splitvect)
		}
				
		object@data <- data2Estimate
		rm(data2Estimate)
	}
	
	##################################################################################
	# Estimate Maximum Likelihood Classifier Variables

	#Create classifier
	varlist <- strsplit(as.character(object@call),' ~ ')
	varlist <- unlist(strsplit(as.character(varlist[3]),' + ', TRUE))
	classes <- as.numeric(levels(factor(object@data[object@samplename][,1])))
	samplesize <- length(na.omit(object@data[object@samplename][,1]))
	
	# Create required Lists
	meanlist <- list()
	invcovlist <- list()
	detcorlist <- list()
	priorlist <- list()

	for(i in 1:length(classes)){
		dataCorMean <- subset(subset(object@data, object@data[object@samplename] == classes[i]), select=varlist)
		priorlist[[i]] <- log(length(dataCorMean[,1])/samplesize)
		meanlist[[i]] <- as.numeric(mean(dataCorMean))
		invcovlist[[i]] <- cov(dataCorMean)
		
		for(j in 1:length(varlist)){
			if(var(dataCorMean[varlist[j]]) == 0){
				cat('\nWARNING: Zero variance of sample data for variable...', varlist[j], '\n')
				invcovlist[[i]][j,j] <- 1
			}
		}
		detcorlist[[i]] <- log(det(invcovlist[[i]]))
		invcovlist[[i]] <- solve(invcovlist[[i]])
		
	}

	##################################################################################
	# Predict grid values based on probabilities
	if(splitfraction < 1){
		object@data <- data2Predict
		rm(data2Predict)
	}
	
	dataVars  <- as.matrix(subset(object@data, select=varlist))
	predicted <- rep(NA,length(dataVars[,1]))
	probs <- rep(NA,length(classes))

	for(i in 1:length(dataVars[,1])){
		for(j in 1:length(classes)){
			delta <- dataVars[i,] - meanlist[[j]]
			probs[j] <- priorlist[[j]] - detcorlist[[j]] - t(delta)%*%invcovlist[[j]]%*%delta
		}
		predicted[i] <- classes[probs==max(probs)]
	}

	# Store predicted Grid
	object@predictedGrid <- object@gridSkeleton
	temp <- rep(NA, length(object@gridSkeleton@grid))
	temp[as.logical(object@gridSkeleton@grid)] <- predicted
	object@predictedGrid@grid <- temp
	
	object
}
)