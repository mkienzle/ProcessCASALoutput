compare.multiple.objective.functions <- function(filenames, models.names){

  # PURPOSE create a table that compares the likelihood value of each components

  library(casal)

  # Read likelihood values from files
  for(file.nb in 1:length(filenames)){

    if(file.nb == 1) {
                tmp.data = extract.multiple.objective.functions(filenames[file.nb])
                my.df = data.frame( loglik.comp = dimnames(tmp.data)[[1]], value = tmp.data, model.name = models.names[file.nb])
                #my.df = rbind(my.df, data.frame())
                } else {
                tmp.data = extract.multiple.objective.functions(filenames[file.nb])
                my.df = rbind(my.df, data.frame( loglik.comp = dimnames(tmp.data)[[1]], value = tmp.data, model.name = models.names[file.nb]))}
    }

  # convert from long to wide format
  library(tidyverse)

  fct.result = as_tibble(my.df) %>% spread(model.name, value)
  fct.result %>% print(n=100)
  print("Total"); dummy %>% summarise_if(is.numeric, funs(sum)) %>% print(n=10)

  return(fct.result)
}

# Example

# dummy = compare.multiple.objective.functions(
#   filenames = c("C:/Users/kienzlemj/OneDrive - NIWA/Projects/Stock assessments/Ling/LIN 7/LIN201903/CASAL/LIN7_comb_v44/CASAL-MPDoutput.txt",
#                 "C:/Users/kienzlemj/OneDrive - NIWA/Projects/Stock assessments/Ling/LIN 7/LIN201903/CASAL/LIN7_logn_v44/CASAL-MPDoutput.txt"),
#   models.names = c("Model A", "Model B")
#   )

