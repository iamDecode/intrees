
extractRules <-
function(treeList,X,li,ntree=100,maxdepth=6,random=FALSE){
	levelX = list()

	for(iX in 1:ncol(X))
		levelX <- c(levelX,list(levels(X[,iX])))

	# X <- NULL; target <- NULL
	ntree=min(treeList$ntree,ntree)
	allRulesList = data.frame()

	for(iTree in 1:ntree){
		if(random==TRUE){max_length = sample(1:maxdepth,1,replace=FALSE)}else{
		max_length = maxdepth}
		rule = list(); count = 0; rowIx = 1; 
		# tree = getTree(rf,iTree,labelVar=FALSE)
		tree <- treeList$list[[iTree]]
		ruleSet = vector("list", length(which(tree[,"status"]==-1)))
		liSet = vector("list", length(which(tree[,"status"]==-1)))
		treeli = li$forest$lIncrements[
		  (((iTree - 1) * treeList$nrnodes) + 1) : 
		  ((((iTree - 1) * treeList$nrnodes)) + NROW(tree))
		]
		tree = cbind(tree, li=treeli)

		res = treeVisit(tree,rowIx = rowIx,count,ruleSet,rule,levelX,length=0,max_length=max_length,liSet=liSet)
		allRulesList = rbind(allRulesList, cbind(ruleSet=res$ruleSet, prediction=res$prediction, liSet=res$liSet))
	}

	allRulesList <- allRulesList[!unlist(lapply(allRulesList$ruleSet, is.null)),]
	cat(paste(length(allRulesList$ruleSet)," rules (length<=",  
	max_length, ") were extracted from the first ", ntree," trees.","\n",sep=""))

	return(allRulesList)
	#rulesExec <- ruleList2Exec(X,allRulesList)
	#return(rulesExec)
}
