##' @title HessMultiNegLlk
##'

HessMultiNegLlk <-
function(w, X){
  n <- dim(X)[1]
  p <- dim(X)[2]
  Y_hat <- YHatMulti(w, X, baseline=FALSE)  ## N X C-1 matrix of predictions
  hess_list <- vector("list", n)

  ## this might take forever. like, actually forever. 
  invisible(sapply(1:n, function(ii){
    Y_hat_vec  <- Y_hat[ii, ]
    X_vec <- X[ii, ]
    X_outer_prod <- X_vec %*% t(X_vec)
    ##DANGER## k_prod <- kronecker(diag(Y_hat_vec), X_outer_prod)
    hess_list[[ii]] <<- k_prod
  }))
  
  hess <- Reduce("+", hess_list)
  -hess
}
