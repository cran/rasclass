##################################################################################
# Set Method: setRasclassData
##################################################################################
setRasclassData <- function(newdata, object, ncols = NA, nrows = NA, xllcorner = NA, yllcorner = NA, cellsize = NA, NAvalue = -9999, samplename = 'sample'){}

setMethod('setRasclassData', signature(newdata = 'data.frame', object = 'rasclass'),

function(newdata, object, ncols = NA, nrows = NA, xllcorner = NA, yllcorner = NA, cellsize = NA, NAvalue = -9999, samplename = 'sample'){
	
	# Set path
	object@path <- 'Data specified manually using setRasclassData()'
	
	# Set sample name
	object@samplename <- samplename
	
	# Remove data path
	object@path <- as.character(NA)
	
	# Set up the gridSkeleton header
	newgrid <- new('rasclassRaster')
	newgrid@grid <- as.numeric(NA)
	newgrid@NAvalue <- NAvalue
		
	if(!is.na(ncols)) 		newgrid@ncols     <- ncols
	if(!is.na(nrows)) 		newgrid@nrows     <- nrows
	if(!is.na(xllcorner)) 	newgrid@xllcorner <- xllcorner
	if(!is.na(yllcorner)) 	newgrid@yllcorner <- yllcorner
	if(!is.na(cellsize)) 	newgrid@cellsize  <- cellsize
		
	object@gridSkeleton <- newgrid
	
	# Set up the gridSkeleton grid (NAhandle)
	object@gridSkeleton@grid <- as.integer(unlist(apply(newdata[names(newdata) != samplename], 1, function(x) !is.element(NA, x))))
	
	# Remove NAs and set data
	object@data <- newdata[as.logical(object@gridSkeleton@grid), ]
	
	# Build Formula
	object <- buildFormula(object)
	
	# Check consistency
	if(!checkRasclass(object)){
		stop('Data object is not consistent, check data and try again')
	}
	
	# Return object
	object
}
)