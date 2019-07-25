#################################################################################################################################
# COMBINE 2 CASAL OUTPUT OBJECTS
# ARGUMENT: obj1, obj2 is created from a CASAL standard output file using the function extract.fits in the casal package

comb.obj = function(obj1, obj2){

  res = obj1

  for(element in 1:length(res)){

    if(is.vector(res[[element]])) res[[element]] = c(obj1[[element]], obj2[[element]])
    if(is.data.frame(res[[element]])) res[[element]] = rbind(obj1[[element]], obj2[[element]])

  }

  return(res)
}
