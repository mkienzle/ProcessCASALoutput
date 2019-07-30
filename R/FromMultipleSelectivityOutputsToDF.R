#################################################################################################################################
# A function to create a data.frame of proportion at age retained in the gear
# ARGUMENT: filenames is a vector of filenames

FromMultipleSelectivityOutputsToDF <- function(filenames, path, label = seq(1, length(filenames))){

  if(length(filenames) == 1){

    return(FromSelectivityOutputsToDF(filenames, path, label))

  } else{

    # Create the df using the first file
    df <- FromSelectivityOutputsToDF(filenames[1], path, as.factor(label[1]))

    # Add all the rest of data
    for(i in 2:length(filenames))
      df <- rbind(df, FromSelectivityOutputsToDF(filenames[i], path, as.factor(label[i])))

    return(df)
  }
}
