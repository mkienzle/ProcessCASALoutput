#################################################################################################################################
# A function to create a data.frame of proportion at age observed and predicted for every year
# ARGUMENT: obj is created from a CASAL standard output file using the function extract.fits in the casal package

FromObjToDF <- function(obj){

  # Get
  year <- obj$year

  # Create a matrix of age-groups corresponding to each observations, remove leading X
  age.group <- as.numeric(substr(dimnames(obj$obs)[[2]], 2, 10))
  age.groups <- outer(rep(1, nrow(obj$obs)), age.group)

  # Create a matrix of years corresponding to every observations
  years <- outer(year, rep(1, ncol(obj$obs)))

  # Format into a data.frame
  df <- data.frame(year = c(years),
                   age.group = c(age.groups),
                   observations = c(as.matrix(obj$obs)),
                   predictions = c(as.matrix(obj$fits)),
                   pearson.residuals = c(as.matrix(obj$pearson.resids)))
}
