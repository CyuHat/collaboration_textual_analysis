# Import
modules::import("dplyr")
modules::import("stm")
modules::import("stats")

# Export
modules::export("lm_stm")

# Our estimateEffect function
lm_stm <- function(formula, K, stmobj, metadata, origcall = NULL, thetatype = "Global", documents = NULL, nsims = 25) {
  # Arguments
  # formula = ~ fyear + category
  # thetatype = "Global"
  # K = 3
  # stmobj = topic_16
  # origcall = "original call"
  # metadata = mystm$meta %>% mutate(fyear = factor(year))
  # documents = NULL
  # nsims = 25
  
  # K
  K <- 1:K
  
  # formula -> qx
  termobj <- terms(formula, data = metadata)
  mf <- model.frame(termobj, data = metadata)
  xmat <- model.matrix(termobj, data = metadata)
  qx <- qr(xmat)
  varlist <- all.vars(termobj)
  
  # topic_16 -> thetasims
  thetasims <- thetaPosterior(stmobj, nsims = 1, type = thetatype, 
                              documents = documents)
  thetasims <- do.call(rbind, thetasims)
  
  # TODO reduce to 3 topics (cbind of each col)
  
  t1 <- thetasims[,2] + thetasims[,4] + thetasims[,13]
  t2 <- thetasims[,7] + thetasims[,2] + thetasims[,16]
  t3 <- thetasims[,1] + thetasims[,5] + thetasims[,6] + thetasims[,8] + thetasims[,10] + thetasims[,11] + thetasims[,12] + thetasims[,14] + thetasims[,15]
  
  thetasims <- cbind(t1, t2, t3)
  
  # mod
  storage <- vector(mode = "list", length = length(K))
  for (i in 1:nsims) {
      for (k in K) {
        lm.mod <- stm:::qr.lm(thetasims[, k], qx)
        storage[[which(k == K)]][[i]] <- stm:::summary.qr.lm(lm.mod)
      }
  }
  
  # Return
  toreturn <- list(parameters = storage, topics = K, call = origcall, 
                   uncertainty = thetatype, formula = formula, data = metadata, 
                   modelframe = mf, varlist = varlist)
  class(toreturn) <- "estimateEffect"
  
  # Test summary
  summary(toreturn,
          topics = 1:3)
}
