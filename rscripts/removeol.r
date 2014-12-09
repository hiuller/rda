
# removeol: remove outliers based on the criteria of 1.5 times the interquartile range
# Author: Hiuller Araujo, 11/12/2013
# This is my first R function

# cols -> a vector with the columns of the data.frame to be taken into account
# draw -> if TRUE a boxplot is drawned

removeol <- function(x, cols, draw=TRUE)
{ 
  ds <- x
  if (is.data.frame(x))
  {
    for(i in cols)
    {
      nome <- names(ds)[i]
      ori.ds <- ds[,i]
      sz <- length(ori.ds) # initial dataset's length
      ols1 <- ol(ori.ds)
      ol.count.start <- sum(as.integer(ols1))
      
      new.ds <- ori.ds[!ols1]
      ols2 <- ol(new.ds)
      ol.count.end <- sum(as.integer(ols2))
      if(draw)
      {
        boxplot(ori.ds, new.ds, names=c(paste(nome, "[Old]"), paste(nome, "[New]")))
      }
      cat(ol.count.start, " outlier(s) where removed. ", ol.count.end, " still remainning", "\n")
      
      ds <- subset(ds, !ols1)
    }
    return(ds)
  }
  else
    cat("FAILED: x is not a data.frame")
}

# OL returns a vector with the position of every OL
ol <- function(x)
{
  q <- quantile(x=x, probs=c(0.25, 0.50, 0.75)) # get the first and third quartile; q[1] = q1, med = q2 and q[3]=q3
  ub <- q[3] + 1.5*(q[3]-q[1]) # upper bound
  lb <- q[1] - 1.5*(q[3]-q[1]) # lower bound
  return(x < lb | x > ub)  
}

normp <- function(x) 
{
  qqnorm(x, pch=19, col="blue", cex=1.6)
  qqline(x, col="darkgrey")
  library(nortest)
  ad.test(x)  
}