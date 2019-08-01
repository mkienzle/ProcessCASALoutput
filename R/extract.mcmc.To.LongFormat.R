# A function that reads the output of a CASAL MCMC (a wide format table) and convert it into a long format with 2 columns
# key: contains the value of the parameter, quantity, etc...
# value:: contains the value of the parameter, quantity, etc...
extract.mcmc.To.LongFormat = function(mcmcfilename, path){

  # Load the MCMC file
  library(casal)
  mcmc.InputValues = extract.mcmc(samples =  mcmcfilename, path = path)

  # Reset the class of the object to matrix to allow manipulation
  class(mcmc.InputValues) <- "matrix"

  # Convert data input from wide to long format
  library(tidyr)
  mcmc.InputValues.long = as_tibble(gather(data = as.data.frame(mcmc.InputValues)))

  return(mcmc.InputValues.long)
}

# mcmcfilename = "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN8_ALLsurveyLogistic/quant.v1"; path = ""
