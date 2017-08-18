# pruneSingleRule <-
# function(rule, X, target, maxDecay, typeDecay){
# # typeDecay = 1: relative error increase; otherwise: absolute error increase

#   #A <- gregexpr("X\\[,[0-9]+\\]", s)
#   newRuleMetric <- measureRule(data.frame(t(rule[c("condition", "pred", "li")])),X,target)
#   errOrig <- as.numeric(newRuleMetric["err"])  

#   ruleV <- unlist(strsplit(rule["condition"],split= " & "))
#   pred <- rule["pred"]
#   # newRule <- NULL
#   if(length(ruleV)==1) return(newRuleMetric)
#   for(i in length(ruleV):1){
#     restRule <- ruleV[-i]
#     restRule <- paste(restRule,collapse= " & ")
#     metricTmp <- measureRule(cbind.data.frame(ruleExec=restRule, pred=rule["pred"], li=rule["li"]),X,target,pred)
#     errNew <- as.numeric(metricTmp["err"]) 
#     if(typeDecay == 1){
#       decay <- (errNew-errOrig)/max(errOrig,0.000001)
#     }else{
#       decay <- (errNew-errOrig)
#     }      
#     if( decay <= maxDecay){
#     #if( errNew-errOrig <= maxDecay){
#       ruleV <- ruleV[-i] 
#       # newRule saves the last changed rule and metrics
#       newRuleMetric <- metricTmp
#       if(length(ruleV)<=1)break
#     }
#   }
#   return(newRuleMetric)
#   #rule["condition"] <- paste(ruleV,collapse= " & ")
#   #return(rule)
# }


## ADAPTED FOR RULE LISTS
pruneSingleRule <-
function(rule, X, target, maxDecay, typeDecay, liDecay){
# typeDecay = 1: relative error increase; otherwise: absolute error increase
  #A <- gregexpr("X\\[,[0-9]+\\]", s)
  newRuleMetric <- rbind(measureRule(rule,X,target))
  colnames(newRuleMetric) <- c("len","freq","err","ruleSet","prediction","liSet")
  errOrig <- as.numeric(newRuleMetric[,"err"])

  test = data.frame(cbind(rule=rule$ruleSet, li=rule$liSet))
  test$li = as.numeric(test$li)

  test = test[order(abs(test$li)),]
  ruleV <- test$rule
  ruleLi <- test$li
  pred <- rule$prediction
  # newRule <- NULL
  if(length(ruleV)==1) return(newRuleMetric) # remove if li is small
  
  for(i in length(ruleV):1){
    newRule = rule
    
    restRule <- ruleV[-i]
    newRule$ruleSet = restRule
    
    newRuleLi <- ruleLi[-i]
    newRule$liSet = newRuleLi

    newRule$li = lapply(ruleLi, function(x) { Reduce("+",unlist(x)) / NROW(unlist(x)) })
    
    restRule <- paste(restRule,collapse= " & ")
    newRule$condition = restRule
    
    newRule$len = length(ruleV)
    
    metricTmp <- rbind(measureRule(newRule,X,target,pred))
    colnames(metricTmp) <- c("len","freq","err","ruleSet","prediction","liSet")
    
    errNew <- as.numeric(metricTmp[,"err"]) 
    if(typeDecay == 1){
      decay <- (errNew-errOrig)/max(errOrig,0.000001)
    }else{
      decay <- (errNew-errOrig)
    }      
    if (is.na(ruleLi[i])) {
      browser()  
    }
    
    if( decay <= maxDecay && ruleLi[i] < liDecay){
    #if( errNew-errOrig <= maxDecay){
      ruleV <- ruleV[-i] 
      ruleLi <- newRuleLi
      # newRule saves the last changed rule and metrics
      newRuleMetric <- metricTmp
      if(length(ruleV)<=1)break
    }
  }
  return(newRuleMetric)
  #rule["condition"] <- paste(ruleV,collapse= " & ")
  #return(rule)
}




# pruneSingleRule <-
# function(rule, maxDecay){
#   rule$ruleSet = rule$ruleSet[abs(unlist(rule$liSet))>maxDecay]
#   rule$len = NROW(rule$ruleSet)
#   rule$liSet = rule$liSet[abs(unlist(rule$liSet))>maxDecay]
#   return(rule)
# }
