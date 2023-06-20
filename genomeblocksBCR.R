genomeblocksBCR <- function(x,y) {
  BCR <- matrix(NA, dim(x)[1], 1, dimnames = list(NULL, c('BCR')))
  out <- cbind(x,BCR)
  
  for(i in 1:dim(x)[1]) {
    if (dim(y)[1] > 0) {  
      out[i,5] <- sum(as.numeric(y$Start) > as.numeric(x[i,"start"]) & as.numeric(y$End) < as.numeric(x[i,"end"]))/(as.numeric(x[i,"end"])-as.numeric(x[i,"start"]))*1000000
    }
    else
      out[i,5] <- 0  
  }
  
#  if (out[i,5] == "NaN") {  
#    out[i,5] <- 0
#  }
  
  out
}   