pruneRule <-
function(rules,X,target, maxDecay = 0.05, typeDecay = 2){
  newRuleMetric <- NULL
  for(i in 1:nrow(rules)){
    newRuleMetric <- rbind(newRuleMetric, pruneSingleRule(rules[i,],X,target, maxDecay, typeDecay))
  }

  # as.numeric(lapply(seq_along(newRuleMetric[,"liSet"]), function(i) { 
  #   x = newRuleMetric[i,"liSet"]
  #   print(i)
  #   Reduce("+",unlist(x)) / NROW(unlist(x)) 
  # }))
  
  li = as.numeric(lapply(newRuleMetric[,"liSet"], function(x) { Reduce("+",unlist(x)) / NROW(unlist(x)) }))
  newRuleMetric = cbind(newRuleMetric, li=li) # CUSTOM

  return(newRuleMetric)
}


# pruneRule <- function(rules, X, target, maxDecay = 0.05, typeDecay = 2, liDecay=0.05){
#   newRuleMetric <- apply(rules, 1, function(rule){
#   	return(pruneSingleRule(rule, X, target, maxDecay, typeDecay, liDecay))
#   })

#   newRuleMetric = data.frame(do.call(rbind, newRuleMetric))
#   newRuleMetric = newRuleMetric[newRuleMetric$len != 0,]
  
#   cat(paste(NROW(rules$ruleSet) - NROW(newRuleMetric)," paths are ignored.",sep=""))
#   return(newRuleMetric)
# }
