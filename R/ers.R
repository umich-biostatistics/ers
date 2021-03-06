
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
  cv.lambda2 = vapply(lambda2, function(lambda) {
    min(cv.gcdnet(x = x, y = y, lambda2 = lambda, nfolds = nfolds, 
                  foldid = foldid, pf = pf, pf2 = pf2, method = method)$cvm)
  }, FUN.VALUE = numeric(1))
  cv.lambda2.min = lambda2[which.min(cv.lambda2)]
  cv.lambda1.min = cv.gcdnet(x = x, y = y, lambda2 = cv.lambda2.min, nfolds = nfolds, foldid = foldid, 
            method = method, pf = pf, pf2 = pf2)$lambda.min
  #cat(cv.lambda2.min)
  #cat('\n')
  #cat(cv.lambda1.min)
  #cat('\n')
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
  adapt.weights = adaptive.weights(coef = beta.enet, sd = apply(data.mod, 2, sd), n = nrow(data.mod))
  adapt.weights[pf == 0] = 0
  data.mod.nonzero = data.mod[,beta.enet.nonzero]
  adapt.weights = adapt.weights[beta.enet.nonzero]
  pf2 = rep(1, length(adapt.weights))
  return(ers.enet(x = data.mod.nonzero, y = y, lambda2 = lambda2.adapt, nfolds = nfolds,
                  foldid = foldid, pf = adapt.weights, pf2 = pf2, method = method))
}

#' ERS score
#' 
#' Computes ERS score. For internal use.
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
#' Estimate environmental risk scores from a number of potentially correlated risk 
#' factors using adaptive elastic net (AENET) regression. ENET regression does variable 
#' selection and can select multiple non-zero collinear variables without overfitting. 
#' AENET is an adaptive version of elastic net (ENET) that satisfies the asymptotic 
#' normality assumption that allows us to conduct statistical inference any hypothesis 
#' testing by providing large sample standard errors and p-values.
#'
#' @examples
#' set.seed(7794)
#' fit = ers(x = metal, y = as.numeric(Y), covar = covs,
#'           control = list(lambda2.start = seq(0.001, 0.5, by = 0.01),
#'                          lambda2.adapt = seq(0.001, 0.5, by = 0.01)))
ers = function(x, y, covar = NULL, control = list(lambda2.start = NULL, lambda2.adapt = NULL), 
               method = 'ls', scaled = FALSE, nfold = 5, seed = NULL, ...) {
  #family = gaussian, binomial, poisson, cox?
  
  x = as.data.frame(x)
  lambda2.start = control$lambda2.start
  lambda2.adapt = control$lambda2.adapt
  
  if(any(!complete.cases(x)) | any(!complete.cases(y)) | any(!complete.cases(covar))) {
    stop('x, y, or covar contain missing values. This method requires complete data.') }
  n = length(y)
  if(nrow(x) != n | nrow(covar) != n) {
    stop('y is not the same length as x or covar. y should be a vector of same 
         length as the number of rows in x and covar.')
  }
  #if(!isTRUE(scaled)) { x = as.data.frame(scale(x, center = TRUE, scale = TRUE)) } # scale, center
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
  if(!isTRUE(scaled)) { data.mod = as.matrix(scale(data.mod, center = TRUE, scale = TRUE)) }
  
  pf2 = c(rep(1, ncol(data.mod)))
  
  ers.fit = ers.aenet(data.mod, y, lambda2.start, lambda2.adapt, nfolds, foldid, pf, pf2, method)
  ers.beta = as.matrix(coef(ers.fit))
  ers.beta.keep = ers.beta != 0
  tab = matrix(0, sum(ers.beta.keep), 1)
  rownames(tab) = rownames(ers.beta)[ers.beta.keep]
  tab[,1] = ers.beta[ers.beta.keep,]
  tab.metals = subset(tab, !(row.names(tab) %in% c('(Intercept)', colnames(covar))))
  coef = as.numeric(tab.metals)
  dat.score = as.matrix(data.mod[,rownames(tab.metals)])
  # step 5
  ers.scores = ers.score(data = dat.score, coef = coef)
  
  ers.obj = list(
    ers.scores = ers.scores, 
    ers.fit = ers.fit,
    coef = coef, 
    dat.score = dat.score
  )
  class(ers.obj) = 'ers'
  
  return(ers.obj)
  # adjusted R2
  # and out-of-bag (OOB) adjusted R2
  # using
  # cross-validation. We also computed the mean squared error (MSE) and the mean squared prediction
  # error (MSPE) to compare the prediction performance. 
}

# this is just for testing purposes. I want to test the model fit to make 
# sure this method is actually working.
model.test = function(fit) {
  
}

#' Make Environmental Risk Score Prediction
#' 
#' Given an ers fit, predict Environmental Risk Score (ERS)
#'
#' @examples 
#' 
predict.ers = function(object, new.x, new.covar = NULL, type = c('class', 'link'), ...) {
  
  predict.gcdnet(object$ers.fit)
}

#' Test ERS results using performance measures
#' 
#' This function explores optimal tuning parameter values for the adaptive
#' elastic net model. Using the trained model results, environmental risk 
#' score is computed.
#' 
test = function(x, y, covar = NULL, fit) {
  
}

#' Train Adaptive ENET Model for ERS
#' 
#' This function explores optimal tuning parameter values for the adaptive
#' elastic net model. Using the trained model results, environmental risk 
#' score is computed.
#' 
#' \bold{Algorithm:}
#' \preformatted{
#' define sets of model parameter values to evaluate
#' |for each parameter set
#' | |for each resampling iteration
#' | |    hold out specific samples
#' | |    fit the model on the remainder
#' | |    predict the hold-out samples
#' | |  end
#' |   calculate the average performance across hold-out predictions
#' |  end
#'  determine the optimal parameter set
#'  fit the final model to all the training data using the optimal parameter set
#' }
#'  
#'
#' @examples 
#' 
train = function(x, y, covar = NULL, control = list(method = 'cv')) {
  
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

# # @description Environmental risk score using adaptive elastic net regression.
# # 
# "_PACKAGE"
