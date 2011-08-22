##################################################################################
# Set Class: rasclassRaster
##################################################################################
setClass('rasclassRaster',

representation(
# Header
ncols     = 'numeric',
nrows     = 'numeric',
xllcorner = 'numeric',
yllcorner = 'numeric',
cellsize  = 'numeric',
NAvalue   = 'numeric',

# Data
grid = 'numeric'
),

prototype = list(ncols = NULL, nrows = NULL, xllcorner = NULL, yllcorner = NULL, cellsize = NULL, NAvalue = NULL, grid = NULL)
)


##################################################################################
# Set Class: rasclass
##################################################################################
setClass('rasclass',

representation(
# Input Variables
path = 'character',
data = 'data.frame',
samplename = 'character',
call = 'formula',
method = 'character',

# Memory saving
gridSkeleton = 'rasclassRaster',

# Predicted Variables
predictedGrid = 'rasclassRaster',

# Multinom specifics
coefficients = 'matrix',
anova = 'data.frame',

# Accuracy and reconstruction statistics
kappa = 'numeric',
accuracy = 'matrix'),

prototype = list(
path = NULL, data = NULL, samplename = NULL, call = NULL, method = NULL,
gridSkeleton = new('rasclassRaster'),
predictedGrid = new('rasclassRaster'),
coefficients = NULL, anova = NULL,
kappa = NULL, accuracy = NULL))
