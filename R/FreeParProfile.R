FreeParProfile <- function(filename, free.par = "initialization.B0", comp = NA, combine.penalties = TRUE, add.total = FALSE,
                                 x.axis.title = "NA", prior.penalties.keywords = c("prior","cl"), legend.position = "right"){

# filename: containing the profile data produce by the command: casal -p > filename
# free.par: the parameter estimated by CASAL that we want to plot the profile of    
# comp: a vector giving the indices of the component to plot
# combine.penalties: boolean to combine or not the many priors and penalties component of a Bayesian objective function
# add.total: boolean to indicate whether or not plotting the total objective function
# x.axis.title: a character string to indicate the x-axis title
# prior.penalties.keywords: vector of character used to identify profiles of priors and penalties

# Extract profiles data from CASAL files
library(casal)

# Load profiles data from filename the file that contains CASAL profiles calculations
a<-extract.multiple.objective.functions(filename)
a2<-extract.profiles(filename)[[free.par]]$values[,"Value"]


# combine penalties and prior
if(combine.penalties){
  which.line.contains.pen.and.prior <- grep(paste(prior.penalties.keywords, collapse = "|", sep=""), dimnames(a)[[1]])
  a.tmp <- a[-which.line.contains.pen.and.prior,]
  a.tmp <- rbind(a.tmp, colSums(a[which.line.contains.pen.and.prior,]) )
  dimnames(a.tmp)[[1]][nrow(a.tmp)] <- "penalties and priors"
  a <- a.tmp
}

# If using only a subset of the data
if(length(comp) > 1 | !is.na(comp[1])) {a <- a[comp,]; comp <- 1:nrow(a)}

# Create total
    if(add.total) {
        a = rbind(a, colSums(a))
        dimnames(a)[[1]][nrow(a)] = "Total"
    }

# Scale the objective function
a = t(apply(X = a, MARGIN = 1, FUN = function(x) (x - min(x)) / max(x - min(x))))

# Convert the data format from wide to long
for(i in 1:nrow(a)){

  if(i == 1) {
    my.df = data.frame(component = dimnames(a)[[1]][i],
                       par.value = a2,
                       obj.fct = a[i,])
  } else{

    my.df = rbind(my.df,
                  data.frame(component = dimnames(a)[[1]][i],
                            par.value = a2,
                            obj.fct = a[i,]))
}
}

# Plot the profile
library(ggplot2)

p = ggplot(data = my.df) +
    geom_line(mapping = aes(x = par.value, y = obj.fct, col = component, linetype = component)) +
    ylab("Scaled objective function") +
    ylim(c(-0.1, 1)) +
    theme_light() + theme(legend.position = legend.position)

    # custom x-axis
    if(x.axis.title == "B0")
    { p = p + xlab(expression(B[0])) }
    else {
        if(x.axis.title == "R0")
        {p = p + xlab(expression(R[0])) }
        else {p = p + xlab(x.axis.title)}}
    
    return(p)

}


# Example
#filename = "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN9_ALLsurveyLogisticSigR08/Profile-Output.txt"

#combine.penalties = TRUE
#add.total = TRUE
#x.axis.title = "B0"

#print(FreeParProfile("Profile-Output.txt", free.par = "initialization.R0", combine.penalties=FALSE, add.total=TRUE, x.axis.title="R0", legend.position = "left"))

