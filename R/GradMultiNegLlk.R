##' @title GradMultiNegLlk
##' 

GradMultiNegLlk <-
function(w, X, Y){
  n <- dim(Y)[1]
  c <- dim(Y)[2]
  p <- dim(X)[2]
  if(n != dim(X)[1]){stop("Err: inconsistent input")}
  Y_hat <- YHatMulti(w, X)
  Y_error <- (Y_hat - Y)[, -c] ## (N X C-1) ## category C is baseline
  k_prod <- sapply(1:n, function(ii){
    kronecker(Y_error[ii, ], X[ii, ])
  })
  if(dim(k_prod)[1] != (p * (c - 1))){stop("Kronecker product has inconsitent row dimension")}
  if(dim(k_prod)[2] != n){stop("Kronecker product has inconsitent column dimension")}
  grad <- rowSums(k_prod)
  -grad
}
