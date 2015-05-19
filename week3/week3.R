printf <- function(...) cat(sprintf(...))
require(lattice)
require(SDMTools)

isHomogenous <- function(d){
  return(all(d$label[1] == d$label))
}

entropy <- function(a){
  if(length(a)==0){return(0)}
  a <- table(a) # group by and count
  a <- a[a!=0] # avoid computing log2(0) which gives -Inf
  a <- a / sum(a) # empiric probability
  a <- -a * log2(a)
  return (sum(a) / 2)
}

Label <- function(d) {
  names(sort(table(d$label), decreasing=TRUE))[1]
}

BestSplit <- function(d, f){
  n <- nrow(d)
  
  # calculate initial entropy (d_prev)
  parent_entropy <- entropy(d$label)  #parent entropy without split
  info_gain <- c()
  
  # do split for each feature
  for(i in 1:length(f)) { 
    values <- levels(d[[f[i]]])
    child_entropy <- 0
    #iterate over values
    for(v_j in values) {  
      split_j <- d$label[d[[f[i]]] == v_j]  #get labels of entries that match values[j]
      entr_j <- entropy(split_j) 
      child_entropy <- child_entropy + length(split_j) / n * entr_j
    }
    info_gain <- c(info_gain, (parent_entropy - child_entropy))
  }
  
  splitFeature <- f[match(max(info_gain), info_gain)]
  return(splitFeature)
}

makeLeaf <- function (label) {
  list(label=label)
}

makeTree <- function (splitFeature, children = list()) {
  list(splitFeature=splitFeature, children=children)
}

isLeaf <- function (tree) {
  !is.null(tree[['label']])
}

printTree <- function(tree, prefix = '') {
  if (!is.null(tree[['edgeValue']])) {
    printf('%sEdge: %s\n', prefix, tree[['edgeValue']])
  }
  if (isLeaf(tree)) {
    printf('%sLabel: %s\n', prefix, tree[['label']])
  } else {
    printf('%sSplit feature: %s\n', prefix, tree[['splitFeature']])
    for (child in tree[['children']]) {
      printTree(child, sprintf('  %s', prefix))
    }
  }
  return(tree)
}

countNodes <- function(tree) {
  if (isLeaf(tree)) {
    1
  } else {
    1 + sum(sapply(tree[['children']], countNodes))
  }
}
countLeaves <- function(tree) {
  if (isLeaf(tree)) {
    1
  } else {
    0 + sum(sapply(tree[['children']], countLeaves))
  }
}
calcMaxDepth <- function(tree) {
  if (isLeaf(tree)) {
    1
  } else {
    1 + max(sapply(tree[['children']], calcMaxDepth))
  }
}
calcMinDepth <- function(tree) {
  if (isLeaf(tree)) {
    1
  } else {
    1 + min(sapply(tree[['children']], calcMinDepth))
  }
}

printPerformance <- function (predicted, actuals) {
  stopifnot(length(predicted) == length(actuals))
  acc = sum(predicted == actuals) / length(predicted)
  
  a <- data.frame()
  for (value in unique(actuals)) {
    tp <- sum((predicted == actuals) & (predicted == value))
    tn <- sum((predicted == actuals) & (predicted != value))
    fp <- sum((predicted != actuals) & (predicted == value))
    fn <- sum((predicted != actuals) & (predicted != value))
    # printf('"%s" %f %f %f %f =%f\n', value, tp, tn, fp, fn, tp + tn + fp + fn)
    prec <- tp / (tp + fp)
    rec <- tp / (tp + fn)
    f1 <- 2 * prec * rec / (prec + rec)
    
    printf('"%s" %f %f %f\n', value, prec, rec, f1)
    a <- rbind(a, data.frame(value=value, prec=prec, rec=rec, f1=f1))
  }
  
  printf('Accuracy: %f\n', acc)
  printf('Precision: %f\n', mean(a$prec))
  printf('Recall: %f\n', mean(a$rec))
  printf('F1-Score: %f\n', mean(a$f1))
  data.frame(acc=acc, prec=mean(a$prec), rec=mean(a$rec), f1=mean(a$f1))
}

discretize <- function (col, n = 10) {
  cut(col, co.intervals(col, n, 0)[c(1, (n+1):(n*2))], include.lowest = TRUE)
}

bottomUpREP <- function(tree,d) {
  if(!is.null(tree$label) && tree$label==NA){ #catch leaves without label (="NA") that have no associated instances
    return(0) 
  }
  
  if(is.null(tree[['children']])) {
    return(nrow(d[d$label != tree$label,])) # returns number of errors for this leaf
  }
  
  if(!is.null(tree[['children']])) {
    subtree_errors <- 0
    for(i in length(tree[['children']])){
      c <- tree[['children']][[i]]
      selected_data <- d[d[[tree$splitFeature]]==c$edgeValue,]
      n_errors <- bottomUp(tree[['children']][[i]],selected_data)
      subtree_errors <- subtree_errors + n_errors
    }
    
    # Perform Pruning if it reduces error
    t <- table(d$label) # get counts for each label value
    majority_class <- names(t)[match(max(t),t)]
    node_errors <- nrow(d[d$label != majority_class,])
    if(node_errors < subtree_errors){
      tree['label'] <- majority_class
      tree['children'] <- NULL
      tree['splitFeature'] <- NULL
      return(node_errors)
    }
    else{
      return(subtree_errors)
    }
  }

}

GrowTree <- function(d, f) {
  stopifnot(nrow(d) > 0)
  if (isHomogenous(d) | length(f) == 0) {
    return(makeLeaf(Label(d)))
  }
  
  splitFeature <- BestSplit(d, f)
  remainingFeatures <- subset(f, f != splitFeature)

  subsets <- split(d, d[[splitFeature]])
  
  children <- lapply(1:length(subsets), function (i) {
    if (nrow(subsets[[i]]) > 0) {
      childNode <- GrowTree(subsets[[i]], remainingFeatures)
    } else {
      childNode <- makeLeaf(Label(d))  
    }
    childNode[['edgeValue']] = names(subsets)[i]
    return(childNode)
  })
  
  return(makeTree(splitFeature, children))
}

predictSingle <- function(tree, x) {
  traverse <- function(subtree) {
    if (isLeaf(subtree)) {
      return(subtree[['label']])
    } else {
      splitValue <- x[[subtree[['splitFeature']]]]
      for (childNode in subtree[['children']]) {
        if (splitValue == childNode[['edgeValue']]) {
          return(traverse(childNode))
        }
      }
    }
  }
  traverse(tree)
}
predictMany <- function(tree, X) {
  apply(X, 1, function (x) { 
    predictSingle(tree, x) 
  })
}


# Ass 1

ass1 <- function () {
  # INPUT
  customers <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12")
  textiles <- c("medium", "few", "medium", "many", "few", "many", "few", "medium", "many", "few", "few", "many")
  gifts <- c("few", "medium", "many", "few", "medium", "medium", "many", "few", "few", "few", "many", "many")
  avgprice <- c("medium", "low", "medium", "high", "high", "low", "low", "low", "low", "high", "medium", "high")
  label <- c("T", "N", "TG", "T", "G", "TG", "G", "N", "T", "N", "G", "TG")
  
  features <- c('textiles', 'gifts', 'avgprice')
  data <- data.frame(customers, textiles, gifts, avgprice, label)
  
  tree <- GrowTree(data, features)
  printTree(tree)
  printf('Nodes: %i\n', countNodes(tree))
  printf('Leaves: %i\n', countLeaves(tree))
  printf('Max Depth: %i\n', calcMaxDepth(tree))
  printf('Min Depth: %i\n', calcMinDepth(tree))
  printf('Predicted: %s, Actual: %s\n', 
         predictSingle(tree, data[1,]), 
         data[1,][['label']])
  printPerformance(predictMany(tree, data), data$label)
}
ass1()

# Ass 2
ass2 <- function () {
  wines <- read.csv('winequality-white.csv')
  wineFeatures <- c('fixed.acidity', 'volatile.acidity', 'citric.acid', 'residual.sugar',
                    'chlorides', 'free.sulfur.dioxide', 'total.sulfur.dioxide',
                    'density', 'pH', 'sulphates', 'alcohol')
  
  preprocess <- function (data, features) {
    # Rename 'quality' to 'label'
    names(data)[names(data)=="quality"] <- "label"
    # Discretize all feature columns
    for(f in features) {
      data[[f]] <- discretize(data[[f]])
    }
    data
  }
  
  data <- preprocess(wines, wineFeatures)
  tree <- GrowTree(data, wineFeatures)
  printTree(tree)
  printf('Nodes: %i\n', countNodes(tree))
  printf('Leaves: %i\n', countLeaves(tree))
  printf('Max Depth: %i\n', calcMaxDepth(tree))
  printf('Min Depth: %i\n', calcMinDepth(tree))
  printf('Predicted: %s, Actual: %s\n', 
         predictSingle(tree, data[1,]), 
         data[1,][['label']])
  printPerformance(predictMany(tree, data), data$label)
}
ass2()

# Ass 3
#bottomUpREP(tree,wines)
