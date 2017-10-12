getRuleMetric <-
function(ruleExec, X, target){
  #typeX = getTypeX(X)
  #ruleExec <- unique(t(sapply(allRulesList,RuleList2Exec,typeX=typeX)))
  #colnames(ruleExec) <- c("len","condition")
  ruleMetric <- t(sapply(as.matrix(ruleExec[,"condition",drop=FALSE]),measureRule,X,target))
  rownames(ruleMetric) = NULL; 
  # ruleMetric <- cbind( ruleExec[,1] ,  ruleMetric )
  colnames(ruleMetric) <- c("len","freq","err","condition","pred")
  ruleMetric = cbind(ruleMetric, li=ruleExec[, "li"], liSet=ruleExec[, "liSet"], ruleSet=ruleExec[, "ruleSet"]) # CUSTOM
  #dIx <- which(ruleMetric[,"len"]=="-1")
  dIx <- which(ruleMetric[,"len"]=="-1" | ruleMetric[,"li"]=="0") # CUSTOM
  if(length(dIx)>0){
   ruleMetric <- ruleMetric[-dIx,]
   print(paste( length(dIx)," paths are ignored.",sep=""))
  }
  return(ruleMetric)
  #qIx = order((1- as.numeric(ruleMetric[,"err"])),
  #            as.numeric(ruleMetric[,"freq"]),
  #            -as.numeric(ruleMetric[,"len"]),
  #            decreasing=TRUE)
  #return(ruleMetric[qIx,])
}


