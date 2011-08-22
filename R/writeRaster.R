##################################################################################
# Set Method: writeRaster
##################################################################################
writeRaster <- function(object, path = 'predictedGrid.asc'){}
setMethod('writeRaster', signature(object = 'rasclassRaster'),

function(object, path = 'predictedGrid.asc'){
	
	
	temp <- paste('ncols         ', object@ncols)
	temp <- append(temp, paste('nrows         ', object@nrows))
	temp <- append(temp, paste('xllcorner     ', object@xllcorner))
	temp <- append(temp, paste('yllcorner     ', object@yllcorner))
	temp <- append(temp, paste('cellsize      ', object@cellsize))
	temp <- append(temp, paste('NODATA_value  ', object@NAvalue))

	counter = 1

	for(i in 1:object@nrows){
		# Go through rows taking the NA values into account
		temprow <- NA
		for(j in 1:object@ncols){

			if(is.na(object@grid[j + (i-1)*object@ncols])){
				temprow[j] <- object@NAvalue
			}
			else {
				temprow[j] <- object@grid[j + (i-1)*object@ncols]
			}
		}
		temp <- append(temp, paste(temprow, collapse = ' '))
	}
	writeLines(temp, con = path)
}
)