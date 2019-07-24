ggplot.ProfileLogLik <- function(filename, comp = NA, combine.penalties = TRUE, add.total = FALSE,
                                 x.axis.title = "NA"){

# filename: containing the profile data
# free.par: the parameter that has been profiled

# Extract profiles data from CASAL files
library(casal)

# Load the file that contains CASAL profiles calculations
a<-extract.multiple.objective.functions(filename)
a2<-extract.profiles(filename)$initialization.B0$values[,"Value"]


# combine penalties and prior
if(combine.penalties){
  which.line.contains.pen.and.prior <- grep("prior|cl", dimnames(a)[[1]])
  a.tmp <- a[-which.line.contains.pen.and.prior,]
  a.tmp <- rbind(a.tmp, colSums(a[which.line.contains.pen.and.prior,]) )
  dimnames(a.tmp)[[1]][nrow(a.tmp)] <- "penalties and priors"
  a <- a.tmp
}

# If using only a subset of the data
if(length(comp) > 1 | !is.na(comp[1])) {a <- a[comp,]; comp <- 1:nrow(a)}

# Create total
if(add.total) a = rbind(a, colSums(a))
dimnames(a)[[1]][nrow(a)] = "Total"

# Scale the objective function
a = t(apply(X = a, MARGIN = 1, FUN = function(x) (x - min(x)) / max(x - min(x))))

# From wide to long
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

library(ggplot2)
p = ggplot(data = my.df, aes(x = par.value, y = obj.fct, col = component)) +
    geom_line(aes(linetype = component)) +
    ylab("Scaled objective function") +
    ylim(c(-0.1, 1)) +
    theme_light()
if(x.axis.title == "B0"){ p = p + xlab(expression(B[0])) } else { p = p + xlab(a.axis.title)}

return(p)
}


# Example
#filename = "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/model 1-example-basecaseHAK2018/Profile-Output.txt"

#combine.penalties = TRUE
#add.total = TRUE
#x.axis.title = "B0"

#print(ggplot.ProfileLogLik(filename, comp = NA, combine.penalties = TRUE, add.total = TRUE, x.axis.title = "B0"))

