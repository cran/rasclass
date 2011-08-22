##################################################################################
# Set Method: summary
##################################################################################
setMethod('summary', signature(object = 'rasclass'),

function(object){
	
	cat('Data:\n')
	print(summary(object@data))

	cat('\nClassification Method:', object@method)

	cat('\n\nCall:\n')
	print(object@call, showEnv = FALSE)
		
	cat('\nKappa Coefficient:', object@kappa)
	
	cat('\n\nAccuarcy Table:\n')
	show(object@accuracy)
	
	if((object@method == 'Mlogit') && (!is.null(object@anova))){
		cat('\n')
		print(object@anova)
	}
}
)

##################################################################################
# Set Method: image
##################################################################################
setMethod('image', signature(x = 'rasclassRaster'),

function(x){	
	mymatrix <- matrix(x@grid, nrow = x@ncols)
	mymatrix <- t(apply(mymatrix, 1, rev))
	X <-  x@xllcorner + x@cellsize * 1:x@ncols # Longitude
	Y <-  x@yllcorner + x@cellsize * 1:x@nrows # Latitude
	image(X, Y, mymatrix, col = topo.colors(length(levels(factor(x@grid)))))
}
)

##################################################################################
# Set Method: image
##################################################################################
setMethod('image', signature(x = 'rasclass'), function(x){image(x@predictedGrid)})

##################################################################################
# Set Method: View
##################################################################################
setMethod('View', signature(x = 'rasclass'), function(x, title){View(x@data, title)})
