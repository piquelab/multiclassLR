##' @title MultiNegLlk
##'

MultiNegLlk <-
function(w, X, Y){
  Y_hat <- YHatMulti(w, X)  
  llk <- sum(diag(t(Y) %*% log(Y_hat)))  # trace is invariant to cyclic permutations
  -llk
}
