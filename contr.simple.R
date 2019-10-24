contr.simple = function(n){
  if(n == 2){
    return(contr.sum(2)/2)
  }else{
    firstline = matrix(-1/n, nrow = 1, ncol = n-1)
    restlines = matrix(0, nrow = n-1, ncol = n-1)
    restlines[upper.tri(restlines)] = -1/n
    restlines[lower.tri(restlines)] = -1/n
    restlines[row(restlines)==col(restlines)] = 1-1/n
    return(rbind(firstline, restlines))
  }
}
