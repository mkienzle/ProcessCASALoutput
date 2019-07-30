#################################################################################################################################
# A function to create a data.frame of proportion at age retained in the gear
# ARGUMENT: filename is a file created by CASAL in estimation (-e) mode

FromSelectivityOutputsToDF <- function(filename, path, label = "NA"){

  library(casal)

  # Extract CASAL output, keep only that in relation to ogives
  ogives <- extract.quantities(filename, path)$`Ogive parameter values`

  # which element of the list of par.est correspond to selectivity
  sel.idx <- grep("selectivity", names(ogives))

  # Replace acronyms by easier names to understand
  for(i in 1:nrow(Synonyms)) names(ogives)[ grep(Synonyms$Acronym[i], x = names(ogives), value = FALSE) ] <- Synonyms$Vernacular[i]

  # Build a data frame
  for(i in 1:length(sel.idx)){

    if(i == 1) {
      df <- data.frame(Type = names(ogives)[i], Model = label,
                       age = seq(1, length(ogives[[i]])), proportion = ogives[[i]])
    } else {
      df <- rbind(df,
                  data.frame(Type = names(ogives)[i], Model = label,
                             age = seq(1, length(ogives[[i]])), proportion = ogives[[i]]))
    }
  }
  return(df)
}
