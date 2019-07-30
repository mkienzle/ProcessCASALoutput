#################################################################################################################################
# A function to plot timeseries of mean age overlayed with model prediction
# ARGUMENT: obj1, obj2, ... are an element of the object created from a CASAL standard output file using the function extract.fits in the casal package

plot.MeanAgeOverlayedWithFit <- function(obj1, obj2, obj3, obj4, model.label = 1:4, add.line = TRUE, legend.position = "right"){

  # Prepare the data
  if(missing(obj2)){
    obj.df1 <- FromObjToDF2(obj1, label = paste("model", model.label[1]))

    df <- obj.df1
  }

  if(!missing(obj1) & !missing(obj2) & missing(obj3)){
    obj.df1 <- FromObjToDF2(obj1, label = paste("model", model.label[1]))
    obj.df2 <- FromObjToDF2(obj2, label = paste("model", model.label[2]))

    df <- rbind( obj.df1,
                 subset(obj.df2, !( type %in% "observation"))
    )
  }

  if(!missing(obj1) & !missing(obj2) & !missing(obj3) & missing(obj4)){
    obj.df1 <- FromObjToDF2(obj1, label = paste("model", model.label[1]))
    obj.df2 <- FromObjToDF2(obj2, label = paste("model", model.label[2]))
    obj.df3 <- FromObjToDF2(obj3, label = paste("model", model.label[3]))


    df <- rbind( obj.df1,
                 subset(obj.df2, !( type %in% "observation")),
                 subset(obj.df3, !( type %in% "observation"))
    )
  }

  # Calculate the mean age
  obj.df.mean <- with(df, aggregate(cbind("mean.m" = age.group * proportion), by = list(year = year, type = type), sum))

  # Calculate the sd of age
  df1 <- merge(df, obj.df.mean)
  obj.df.sd <- with(df1, aggregate(cbind("sd.m" = (age.group - mean.m)^2 * proportion), by = list(year = year, type = type),
                                   FUN = function(x) sqrt(sum(x))))

  obj.df.mean.sd <- merge(obj.df.mean, obj.df.sd)
  obj.df.mean.sd.obs <- subset(obj.df.mean.sd, type %in% "observation")

  #print(subset(obj.df.mean.sd, type %in% "observation"))

  library(ggplot2)

  p <- ggplot(data = obj.df.mean.sd.obs) +
    geom_point(mapping = aes(x = year, y = mean.m), size = 3) +
    #geom_errorbar(data = obj.df.mean.sd, aes())
    geom_errorbar(data = obj.df.mean.sd.obs, mapping = aes(x = year, ymin = mean.m - sd.m, ymax = mean.m + sd.m), width = 0.75, col = "black") +
    geom_point(data = subset(obj.df.mean, ! (type %in% "observation")), aes(x = year, y = mean.m, col = type), size = 2.5) +
    xlab("") + ylab("Mean age (years)") + labs(color = '') +
    scale_x_continuous(minor_breaks = seq(1960, 2050)) + scale_y_continuous(minor_breaks = seq(0,30), limits = c(0,18)) +
    theme(axis.title.y = element_text(size = rel(1.8)),
          axis.text = element_text(size = 14),
          strip.text = element_text(size = 14),
          panel.grid.major = element_line(size = 1),
          panel.grid.minor = element_line(size = 0.5)) +
    theme_light()  + theme(legend.position = legend.position)


  if(add.line == TRUE) p <- p + geom_line(data = subset(obj.df.mean, ! (type %in% "observation")), aes(x = year, y = mean.m, col = type), lwd = 1)
  return(p)
}

#filename1 = "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN9_ALLsurveyLogisticSigR08/CASAL-MPDoutput.txt"
#filename2 = "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN10_ALLsurveyLogisticSingleSelectivity/CASAL-MPDoutput.txt"

#plot.MeanAgeOverlayedWithFit(comb.obj(extract.fits(filename2)$wcsiTRLagePre2005, extract.fits(filename2)$wcsiTRLageFrom2005))
#plot.MeanAgeOverlayedWithFit(obj1 = comb.obj(extract.fits(filename1)$wcsiTRLagePre2005, extract.fits(filename1)$wcsiTRLageFrom2005),
#                             obj2 = extract.fits(filename2)$wcsiTRLage, model.label = c("Two selectivity", "One selectivity"), add.line = FALSE)

