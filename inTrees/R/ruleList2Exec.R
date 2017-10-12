ruleList2Exec <-
function(X,input){
  allRulesList = input$ruleSet
  typeX = getTypeX(X)
  ruleExec <- unique(t(sapply(allRulesList,singleRuleList2Exec,typeX=typeX)))
  ruleExec <- t(ruleExec)
  li = as.numeric(lapply(input$liSet, function(x) { Reduce("+",unlist(x)) / NROW(unlist(x)) })) # CUSTOM
  input = cbind(input, condition=ruleExec, li=li) # CUSTOM
  return(input)
}
