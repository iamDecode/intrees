getRuleMetric <-
function(rulesList, X, target){
  #typeX = getTypeX(X)
  #ruleExec <- unique(t(sapply(allRulesList,RuleList2Exec,typeX=typeX)))
  #colnames(ruleExec) <- c("len","condition")

  ruleMetric <- t(apply(rulesList,1,measureRule,X,target))

  # rownames(ruleMetric) = NULL; 
  # ruleMetric <- cbind( ruleExec[,1] ,  ruleMetric )
  # 
  ruleMetric = data.frame(do.call(rbind, ruleMetric[!sapply(ruleMetric, is.null)]))
  colnames(ruleMetric) <- c("len","freq","err","ruleSet","prediction","liSet")

  cat(paste(NROW(rulesList$ruleSet) - NROW(ruleMetric)," paths are ignored.",sep=""))

  return(ruleMetric)
  #qIx = order((1- as.numeric(ruleMetric[,"err"])),
  #            as.numeric(ruleMetric[,"freq"]),
  #            -as.numeric(ruleMetric[,"len"]),
  #            decreasing=TRUE)
  #return(ruleMetric[qIx,])
}
