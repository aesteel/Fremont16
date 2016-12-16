



ci.table = function(mod) { 
 options(digits=6)
  nvar = length(names(mod$coef))
  est = round(coef(summary(mod))[,"Estimate"],3)
  estse = round(coef(summary(mod))[,"Std. Error"],3)
  ci.l = round(est - (1.96*estse),3)
  ci.u = round(est + (1.96*estse),3)
  estp = round(coef(summary(mod))[,"Pr(>|t|)"],3)

  ci.df.form = data.frame(Variable = names(mod$coef), 
                          Estimate = est, 
                          CI95 = paste0("[",ci.l,",",ci.u,"]"), 
                          Pvalue = estp)
  return(ci.df.form)
  }

