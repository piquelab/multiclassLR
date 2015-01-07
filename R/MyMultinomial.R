##' @title MyMultinomial
##'

MyMultinomial <-
function(parms, X, Y, var.names, hessian=TRUE, maxit=1E5){

  ## check argument validity ##
  ## TODO ##
  cat('=== parameters are valid ===', '\n\n')

  ## model fitting
  cat('=== begin optimization ===', '\n\n')
  st <- proc.time()
  model <- optim(parms, fn=MultiNegLlk, X=as.matrix(X), Y=as.matrix(Y), method="BFGS", hessian=TRUE, control=list(maxit=maxit));
  en <- proc.time()
  cat('Run time: ', en-st, '\n\n');
  
  ## results: model fit and parameter estimates
  cat('=== processing ouput ===', '\n\n')
  nllk <- model$value 
  converged <- model$convergence
  hessian <- model$hessian
  model_res <- MungeResults(model, var.names, classes=4)

  retval <- list(nllk=nllk, converged=converged, hessian=hessian, results=model_res)
  retval
}
