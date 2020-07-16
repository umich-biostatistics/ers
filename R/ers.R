
# standalone function for steps 1-3 of algorithm
ers.enet = function(x, y, lambda2, nfolds, foldid, pf, pf2, method) {
  cv.lambda2 = vapply(lambda2, function(lambda) {
    min(cv.gcdnet(x = x, y = y, lambda2 = lambda2, nfolds = nfolds, 
                  foldid = foldid, pf = pf, pf2 = pf2, method = method)$cvm)
  }, FUN.VALUE = numeric(length(lambda2)))
  cv.lambda2.min = lambda2[which.min(cv.lambda2)]
  cv.lambda1.min = cv.gcdnet(x = x, y = y, lambda2 = cv.lambda2.min, nfolds = nfolds, foldid = foldid, 
            method = method, pf = pf, pf2 = pf2)$lambda.min
  return(gcdnet(x = x, y = y, lambda = cv.lambda1.min, lambda2 = cv.lambda2.min, 
                nfolds = nfolds, foldid = foldid, pf = pf, pf2 = pf2, 
                method = method))
}

adaptive.weights = function(coef, sd, n) {
  v.star = log(sum(coef != 0)) / log(n)
  gamma = ceiling((2*v.star) / (1 - v.star)) + 1
  w = (abs(coef*sd) + 1/n)^(-gamma)
}

ers = function(x, y, covar, lambda2.start = NULL, lambda2.adapt = NULL, 
               method = 'ls', scaled = FALSE, nfold = 5, seed = NULL, ...) {
  
  #family = gaussian, binomial, poisson, cox?
  
  if(any(!complete.cases(x)) | any(!complete.cases(y)) | any(!complete.cases(covar))) {
    stop('x, y, or covar contain missing values. This method requires complete data.') }
  n = length(y)
  if(nrow(x) != n | nrow(covar) != n) {
    stop('y is not the same length as x or covar. y should be a vector of same 
         length as the number of rows in x and covar.')
  }
  if(!isTRUE(scaled)) { x = scale(x, center = TRUE, scale = TRUE) } # scale, center
  if(!is.null(seed)) { set.seed(seed) }
  if(is.null(lambda2.start)) {
    # auto-generate lambda2.start sequence.
  }
  foldid = matrix(data = c(sample(n), rep(1:nfold, length = n)), nrow = n, ncol = 2)
  data.mod = cbind(model.matrix(~-1+.^2, data = x), x^2, covar)
  
  # steps 1-3
  enet.init = ers.enet(x = data.mod, y = y, lambda2 = lambda2, nfolds = nfolds,
                       foldid = foldid, pf = pf, pf2 = pf2, method = method)
  
  # steps 4
  enet.adapt = ers.enet(x = data.mod.nonzero, y = y, lambda2 = lambda2, nfolds = nfolds,
                        foldid = foldid, pf = pf, pf2 = pf2, method = method)
  
  # step 5
  er.score = beta * x
  
  # adjusted R2
  # and out-of-bag (OOB) adjusted R2
  # using
  # cross-validation. We also computed the mean squared error (MSE) and the mean squared prediction
  # error (MSPE) to compare the prediction performance. 
  
}

# define sets of model parameter values to evaluate
# for each parameter set
#  for each resampling iteration
#   hold out specific samples
#   fit the model on the remainder
#   predict the hold-out samples
#  end
#  calculate the average performance across hold-out predictions
# end
# determine the optimal parameter set
# fit the final model to all the training data using the optimal parameter set
train = function() {
  
}

print.ers = function(x, ...) {
  
}

plot.ers = function(x, ...) {
  
}

#' @description desc
#' 
"_PACKAGE"
