ruleList2Exec <-
function(X,allRulesList){
  typeX = getTypeX(X)
  ruleExec <- unique(t(sapply(allRulesList$ruleSet,singleRuleList2Exec,typeX=typeX)))
  ruleExec <- t(ruleExec)
  ruleExec = cbind(ruleExec, prediction=allRulesList$prediction)
  colnames(ruleExec) <- c("condition", "pred")
  return(ruleExec)
}
