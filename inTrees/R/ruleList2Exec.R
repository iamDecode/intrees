ruleList2Exec <-
function(X,input){
  allRulesList = input$ruleSet
  typeX = getTypeX(X)
  ruleExec <- unique(t(sapply(allRulesList,singleRuleList2Exec,typeX=typeX)))
  ruleExec <- t(ruleExec)
  ruleExec = cbind(ruleExec, as.numeric(lapply(input$liSet, function(x) { Reduce("+",unlist(x)) / NROW(unlist(x)) })))
  colnames(ruleExec) <- c("condition", "li")
  return(ruleExec)
}
