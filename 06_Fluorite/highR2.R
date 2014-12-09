# This function evaluates all combinations of columns in a data.frame and
# lists all pairs of variables which have r2 greater than the threshold
highR2 <- function(dframe, threshold) {
  for(i in 1:length(dframe)[1]) {
    for( j in 1:length(dframe)[1]) {
      if( i!=j ) {
        r2 <- (cor(dframe[,i], dframe[,j])^2)*100.0
        if(is.na(r2)) r2 <- 0.0
        if(r2 > threshold) {
          print( sprintf("r2 between <%s> and <%s>: %.2f", names(dframe)[i], names(dframe)[j], r2 ))
        }
      }        
    }
  }
}