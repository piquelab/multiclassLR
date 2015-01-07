##' @title YHatMutli
##'

YHatMulti <-
function(w, X, baseline=TRUE){
  p <- dim(X)[2]
  W <- matrix(w, nrow=p) 
  class_inner_prod <- exp(X %*% W)
  denom <- (1 + rowSums(class_inner_prod))^-1
  if(baseline){
    cbind(class_inner_prod * denom, denom)
  } else {
    class_inner_prod * denom
  }
}
