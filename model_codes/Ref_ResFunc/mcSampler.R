mcSampler = function(resFun, iter, TP, ...) {
  
  res <- array(dim=c((TP),8,iter))
  k <- 1
  while (k<iter) { 
    res[,,k] <- resFun(TP, ...)
    k <- k + 1
  }
  return(res)
}