#################################################################################################################################
# Plot Pearson residuals

# ARGUMENT: obj1, obj2, ... are an element of the object created from a CASAL standard output file using the function extract.fits in the casal package

plot.pearson.residuals <- function(obj1, obj2, combine = FALSE, year.lab = 1990:2010, age.lab = seq(3,20,2)){

  library(ggplot2)
  library(ggpubr)

  if(missing(obj2)){

    df =  FromObjToDF(obj1)
    df = within(df, {
      year = as.factor(year)
      age.group = as.factor(age.group)
    })

    if(!missing(obj2) & combine){

      df =  FromObjToDF(obj1)

      df2 =  FromObjToDF(obj2)

      df = rbind(df, df2)

      df = within(df, {
        year = as.factor(year)
        age.group = as.factor(age.group)
      })

    }
  }

  p1 = ggplot(data = df, aes(x = year, y = pearson.residuals)) +
    geom_boxplot() +
    scale_x_discrete(breaks = as.factor(year.lab)) +
    geom_hline(yintercept = c(-2, 0, 2), lty = c(3,1,3), col = "red", size = 1) +
    xlab("") + ylab("Pearson residuals") + theme_light() +
    theme(axis.title.y = element_text(size = rel(1.8)),
          axis.text = element_text(size = 12))



  p2 = ggplot(data = df, aes(x = age.group, y = pearson.residuals)) +
    geom_boxplot() +
    scale_x_discrete(breaks = as.factor(age.lab)) +
    geom_hline(yintercept = c(-2, 0, 2), lty = c(3,1,3), col = "red", size = 1) +
    xlab("Age group") + ylab("Pearson residuals") + theme_light() +
    theme(axis.title.y = element_text(size = rel(1.8)),
          axis.text = element_text(size = 14))

  p = ggarrange(p1, p2, ncol = 2, nrow = 1)
  return(p)


}

