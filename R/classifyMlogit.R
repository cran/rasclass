##################################################################################
# Set Method: classifyMlogit
##################################################################################
classifyMlogit <- function(object, anovatable = FALSE, splitfraction = 1){}
setMethod('classifyMlogit', signature(object = 'rasclass'),

function(object, anovatable = FALSE, splitfraction = 1){
	
	# Control for Forumla
	if(is.null(object@call)){
		stop('Please build or specify formula in rasclass object')
	}
	
	# Check consistency
	if(!checkRasclass(object)){
		stop('Data object is not consistent, check data and try again')
	}

	# Set Method type
	object@method <- 'Mlogit'
	cat('Classification using Multinomial Logit\n')

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
	# Estimate multinomial logistic model
		
	# Calculate Regression
	multinom.out <- multinom(data=na.omit(object@data), object@call, maxit = 1000)

	# Store coefficients
	object@coefficients <- coef(multinom.out)
	
	# Create Output of anova Statistics	
	if(anovatable){
		anovaStats <- Anova(multinom.out)
		object@anova <- anovaStats
	}

	rm(multinom.out)
	gc()
	
	##################################################################################
	# Predict piecwise grid values based on probabilities

	if(splitfraction < 1){
		object@data <- data2Predict
		rm(data2Predict)
	}
	
	len = 100000
	piece = 1:len
	predicted <- NA
	catnames <- levels(factor(object@data[object@samplename][,1]))

	while(piece[len] < length(object@data[,1]) + len){
		
		if(piece[len] < length(object@data[,1])){
			temp <- object@data[piece, ]
		} else {
			temp <- object@data[piece[1]:length(object@data[,1]), ]
		}
		
		probs <- as.data.frame(as.matrix(cbind(rep(1, length(temp[,1])), temp[names(object@data) != object@samplename])) %*% as.matrix(t(object@coefficients)))

		probs <- cbind(rep(0, length(temp[,1])), probs)

		probs <- exp(probs)

		probs <- probs/rowSums(probs)

		names(probs) <- catnames
		
		if(piece[len] == len){
			predicted <- as.numeric(names(probs)[unlist(apply(probs, 1, which.max))])
		} else {
			predicted <- append(predicted, as.numeric(names(probs)[unlist(apply(probs, 1, which.max))]))
		}

		piece <- piece + len	
	}

	# Store predicted Grid
	object@predictedGrid <- object@gridSkeleton
	temp <- rep(NA, length(object@gridSkeleton@grid))
	temp[as.logical(object@gridSkeleton@grid)] <- predicted
	object@predictedGrid@grid <- temp

	object
}
)