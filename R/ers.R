
#' ERS ENET
#'
#' @examples 
#' n = length(Y)
#' nfold = 5
#' foldid = matrix(data = c(sample(n), rep(1:nfold, length = n)), nrow = n, ncol = 2)
#' foldid = foldid[order(foldid[,1]),]
#' p = ncol(metal)
#' ers.enet(x = metal, y = as.numeric(Y), foldid = foldid[,2], lambda2 = seq(0.00001, 0.01, by = 0.001))
# standalone function for steps 1-3 of algorithm
ers.enet = function(x, y, lambda2, nfolds = 5, foldid, pf = rep(1, p), pf2 = rep(1, p), method = 'ls') {
  # x = metal
  # y = as.numeric(Y)
  # lambda2 = seq(0.00001, 0.01, by = 0.001)
  # nfolds = 5
  # n = length(Y)
  # foldid = matrix(data = c(sample(n), rep(1:nfold, length = n)), nrow = n, ncol = 2)
  # foldid = foldid[order(foldid[,1]),]
  # foldid = foldid[,2]
  
  cv.lambda2 = vapply(lambda2, function(lambda) {
    min(cv.gcdnet(x = x, y = y, lambda2 = lambda, nfolds = nfolds, 
                  foldid = foldid, pf = pf, pf2 = pf2, method = method)$cvm)
  }, FUN.VALUE = numeric(1))
  cv.lambda2.min = lambda2[which.min(cv.lambda2)]
  cv.lambda1.min = cv.gcdnet(x = x, y = y, lambda2 = cv.lambda2.min, nfolds = nfolds, foldid = foldid, 
            method = method, pf = pf, pf2 = pf2)$lambda.min
  return(gcdnet(x = x, y = y, lambda = cv.lambda1.min, lambda2 = cv.lambda2.min, 
                pf = pf, pf2 = pf2, method = method))
}

#' Adaptive Weights
#'
#' @examples 
#' coef = c(0.001, 0.003, -0.004)
#' sd = c(0.1, 0.4, 0.2)
#' n = 80
#' adaptive.weights(coef, sd, n)
adaptive.weights = function(coef, sd, n) {
  v.star = log(sum(coef != 0)) / log(n)
  gamma = ceiling((2*v.star) / (1 - v.star)) + 1
  w = (abs(coef*sd) + 1/n)^(-gamma)
  return(w)
}

#' ERS Adaptive Step
#'
#' @examples 
#' x = metal
#' y = as.numeric(Y)
#' n = length(y)
#' lambda2.start = seq(0.00001, 0.01, by = 0.001)
#' lambda2.adapt = seq(0.00001, 0.01, by = 0.001)
#' nfolds = 5
#' foldid = matrix(data = c(sample(n), rep(1:nfolds, length = n)), nrow = n, ncol = 2)
#' foldid = foldid[order(foldid[,1]),]
#' foldid = foldid[,2]
#' data.mod = model.matrix(~-1+.^2, data = x) #, x^2, covs)
#' x.sq = x^2
#' names(x.sq) = paste0(names(x), '^2')
#' data.mod = cbind(data.mod, x.sq, covs)
#' #names(data.mod) # = c(names(x), paste0(names(x), '^2'), names(covs))
#' pf = c(rep(1, ncol(data.mod)))
#' pf2 = c(rep(1, ncol(data.mod)))
#' method = 'ls'
#' ers.aenet(data.mod, y, lambda2.start, lambda2.adapt, nfolds, foldid, pf, pf2, method)
# guts of the algorithm for train() and ers()
ers.aenet = function(data.mod, y, lambda2.start, lambda2.adapt, nfolds, foldid, pf, pf2, method) {
  # steps 1-3
  enet.init = ers.enet(x = data.mod, y = y, lambda2 = lambda2.start, nfolds = nfolds,
                       foldid = foldid, pf = pf, pf2 = pf2, method = method)
  beta.enet = coef(enet.init)[-1]
  beta.enet.nonzero = beta.enet != 0
  adapt.weights = adaptive.weights(coef = beta.enet, sd = sapply(data.mod, sd), n = nrow(data.mod))
  adapt.weights[pf == 0] = 0
  data.mod.nonzero = data.mod[,beta.enet.nonzero]
  adapt.weights = adapt.weights[beta.enet.nonzero]
  pf2 = rep(1, length(adapt.weights))
  return(ers.enet(x = data.mod.nonzero, y = y, lambda2 = lambda2.adapt, nfolds = nfolds,
                  foldid = foldid, pf = adapt.weights, pf2 = pf2, method = method))
}

#'
#'
#' @examples 
#' 
ers.score = function(data, coef) {
  score = data %*% coef
  colnames(score) = 'ERS'
  return(score)
}

#' Environmental Risk Score
#' 
#' Compute environmental risk score
#'
#' @examples 
#' covar = covs
#' ers(x = metals, y = as.numeric(Y), covar = covs, 
#'     lambda2.start = seq(0.00001, 0.01, by = 0.001),
#'     lambda2.adapt = seq(0.00001, 0.01, by = 0.001))
ers = function(x, y, covar, lambda2.start = NULL, lambda2.adapt = NULL, 
               method = 'ls', scaled = FALSE, nfold = 5, seed = NULL, ...) {
  x 
  y
  covar
  lambda2.start = NULL 
  lambda2.adapt = NULL 
  method = 'ls' 
  scaled = FALSE 
  nfold = 5 
  seed = NULL
  
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
  foldid = foldid[order(foldid[,1]),]
  foldid = foldid[,2]
  
  data.mod = model.matrix(~-1+.^2, data = x)
  x.sq = x^2
  pf = c(rep(1, ncol(data.mod) + ncol(x.sq)), rep(0, ncol(covar)))
  names(x.sq) = paste0(names(x), '^2')
  data.mod = cbind(data.mod, x.sq, covar)
  
  pf2 = c(rep(1, ncol(data.mod)))
  method = 'ls'
  
  ers.fit = ers.aenet(data.mod, y, lambda2.start, lambda2.adapt, nfolds, foldid, pf, pf2, method)
  ers.beta = as.matrix(coef(ers.fit))
  ers.beta.keep = ers.beta != 0
  tab = matrix(0, sum(ers.beta.keep), 1)
  rownames(tab) = rownames(ers.beta)[ers.beta.keep]
  tab[,1] = ers.beta[ers.beta.keep,]
  tab.metals = subset(tab, !(row.names(tab) %in% c('(Intercept)', colnames(covar))))
  
  # step 5
  ers.score(data = as.matrix(data.mod[,rownames(tab.metals)]), coef = as.numeric(tab.metals))
  
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
#'
#'
#' @examples 
#' 
train = function() {
  
}

#'
#'
#' @examples 
#' 
# simulate data under different assumptions for testing of the method
simulate = function() {
  
}

#'
#'
#' @examples 
#' 
print.ers = function(x, ...) {
  
}

#'
#'
#' @examples 
#' 
plot.ers = function(x, ...) {
  
}

# #' @description Environmental risk score using adaptive elastic net regression.
# #' 
# "_PACKAGE"
