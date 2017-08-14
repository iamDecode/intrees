ruleList2Exec <-
function(X,allRulesList){
  typeX = getTypeX(X)
  ruleExec <- unique(t(sapply(as.list(allRulesList$ruleSet),singleRuleList2Exec,typeX=typeX)))
  ruleExec <- t(ruleExec)
  li = lapply(allRulesList$liSet, function(x) { Reduce("+",unlist(x)) / NROW(unlist(x)) })
  #ruleExec = cbind(condition=ruleExec, pred=allRulesList$prediction, li=li)
  #colnames(ruleExec) <- c("condition", "pred", "li")
  allRulesList$condition = ruleExec
  allRulesList$li = li
  return(allRulesList)
}
