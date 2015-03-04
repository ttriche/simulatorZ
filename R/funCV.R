funCV <- function(obj, fold, y.var, trainFun=masomenos, funCvSubset=cvSubsets){

  setindex <- funCvSubset(obj, fold) 

  ## cross validation
  z <- numeric(fold)

  ## this is hideous
  for(i in 1:fold){
    print(paste("fold = ", i, sep=""))
    testeset <- obj[, setindex[[i]]]
    traineset <- obj[, -(setindex[[i]])]
    
    if(class(obj)=="ExpressionSet"){
      testX <- t(exprs(testeset))
      trainX <- t(exprs(traineset))
    }
    else if(class(obj)=="matrix"){
      testX <- t(testeset)
      trainX <- t(traineset)
    }
    else if(class(obj)=="SummarizedExperiment"){
      testX <- t(assay(testeset))
      trainX <- t(assay(traineset))
    }
    else stop("Wrong class of obj!")
          
    testY <- y.var[setindex[[i]], ]
    testY[, 1] <- as.numeric(as.character(testY[, 1]))
    testY[, 2] <- as.numeric(as.character(testY[, 2]))
    testY <- Surv(testY[, 1], testY[, 2])    
    trainY <- y.var[-setindex[[i]], ]
    trainY[, 1] <- as.numeric(as.character(trainY[, 1]))
    trainY[, 2] <- as.numeric(as.character(trainY[, 2]))
    trainY <- Surv(trainY[, 1], trainY[, 2])
    
    beta <- trainFun(trainX, trainY)
    lp <- testX %*% beta    
	  z[i] <- rcorr.cens(-lp, testY)[1]
  }
  z <- sum(z) / fold
  return(z)
  ### returns the c statistics of cross validation(CV)
}
