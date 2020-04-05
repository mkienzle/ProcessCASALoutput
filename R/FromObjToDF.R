#################################################################################################################################
# A function to create a data.frame of proportion at age observed and predicted for every year
# ARGUMENT: obj is created from a CASAL standard output file using the function extract.fits in the casal package

FromObjToDF <- function(obj){


  # Here we assume that if the obs in the obj is a data.frame, it is a table of year x age
  if( is.data.frame(obj$obs)){
    year <- obj$year
    age.group <- as.numeric(substr(dimnames(obj$obs)[[2]], 2,
                                   10))
    age.groups <- outer(rep(1, nrow(obj$obs)), age.group)
    years <- outer(year, rep(1, ncol(obj$obs)))
    df <- data.frame(year = c(years), age.group = c(age.groups),
                     observations = c(as.matrix(obj$obs)), predictions = c(as.matrix(obj$fits)),
                     pearson.residuals = c(as.matrix(obj$pearson.resids)))

  } # End of the case where it's a data.frame

  # Otherwise we assume it's a vector of observations per year (no aged)
  else {
    age.groups <- NA
    years <- obj$year
    df <- data.frame(year = c(years), age.group = c(age.groups),
                     observations = c(as.matrix(obj$obs)), predictions = c(as.matrix(obj$fits)),
                     pearson.residuals = c(as.matrix(obj$pearson.resids)))

  } # End of the case where it's NOT a data.frame


}
