# measureRule <-
# function(ruleExec,X,target,pred=NULL,regMethod="mean"){
#   li = ruleExec$li;
#   if (is.null(pred)){ pred = ruleExec$pred; ruleExec = ruleExec$condition }
#   len <- length(unlist(strsplit(as.character(ruleExec), split=" & ")))
#   origRule <- ruleExec
#   ruleExec <- paste("which(", ruleExec, ")")
#   ixMatch <- eval(parse(text=ruleExec)) 
#   if(length(ixMatch)==0){
#     v <- c("-1","-1", "-1", "", "", 0)
#     names(v) <- c("len","freq","err","condition","pred","li")
#     return(v)
#   }
#   ys <- target[ixMatch]
#   freq <- round(length(ys)/nrow(X),digits=3)

#   if(is.numeric(target))
#   {
#       if(regMethod == "median"){
#         ysMost = median(ys)
#       }else{
#         ysMost <- mean(ys)
#       }
#       err <- sum((ysMost - ys)^2)/length(ys)   
#   }else{ 
#     if(length(pred)>0){ #if pred of the rule is provided
#       ysMost = as.character(pred)
#     }else{
#       ysMost <- names(which.max(  table(ys))) # get back the first max
#     }
#     ly <- sum(as.character(ys)==ysMost)
#     conf <- round(ly/length(ys),digits=3)    
#     err <- 1 - conf
#   }
#   rule <- origRule

#   v <- c(len, freq, err, rule, ysMost, li)
#   names(v) <- c("len","freq","err","condition","pred","li")
#   return(v)
# }

measureRule <-
function(rulesList,X,target,regMethod="mean"){
  li = rulesList$liSet
  pred = rulesList$prediction
  ruleExec = singleRuleList2Exec(rulesList$ruleSet, getTypeX(X))
  
  len <- length(unlist(strsplit(as.character(ruleExec), split=" & ")))
  origRule <- ruleExec
  ruleExec <- paste("which(", ruleExec, ")")
  ixMatch <- eval(parse(text=ruleExec)) 
  if(length(ixMatch)==0){
    return(NULL)
  }
  ys <- target[ixMatch]
  freq <- round(length(ys)/nrow(X),digits=3)

  if(is.numeric(target))
  {
      if(regMethod == "median"){
        ysMost = median(ys)
      }else{
        ysMost <- mean(ys)
      }
      err <- sum((ysMost - ys)^2)/length(ys)   
  }else{ 
    if(length(pred)>0){ #if pred of the rule is provided
      ysMost = as.character(pred)
    }else{
      ysMost <- names(which.max(  table(ys))) # get back the first max
    }
    ly <- sum(as.character(ys)==ysMost)
    conf <- round(ly/length(ys),digits=3)    
    err <- 1 - conf
  }
  rule <- origRule

  v <- list(len, freq, err, rulesList$ruleSet, ysMost, li)
  return(v)
}