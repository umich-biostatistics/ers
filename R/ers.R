
# standalone fit function for AENET
ers.aenet = function() {
  
}

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


ers = function(x, y, conf, method = 'ls', scaled = FALSE, nfold = 5, seed = NULL, ...) {
  
  x = 
  y = 
  method = 'ls'
  #family = gaussian, binomial, poisson, cox?
  
  if(any(!complete.cases(x)) | any(!complete.cases(y))) {
    stop('x or y contain missing value')
  }
  
  if(!isTRUE(scaled)) {
    # scale, center
  }
  
  if(!is.null(seed)) {
    set.seed(seed)
  }
  
  methods = c('gcdnet', 'glmnet')
  
  
  fit =
    switch(which(method == methods),
           ers.gcdnet(),
           ers.glmnet())
  
  
  
  return(
    list(beta_test, beta_train)
  )
  
}

#' @description desc
#' 
"_PACKAGE"
