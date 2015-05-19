printf <- function(...) cat(sprintf(...))
require(lattice)

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
  
  a <- do.call(rbind, lapply(unique(actuals), function(value) {
    tp <- sum((predicted == actuals) & (predicted == value))
    tn <- sum((predicted == actuals) & (predicted != value))
    fp <- sum((predicted != actuals) & (predicted == value))
    fn <- sum((predicted != actuals) & (predicted != value))
    # printf('"%s" %f %f %f %f =%f\n', value, tp, tn, fp, fn, tp + tn + fp + fn)
    prec <- if (tp > 0) { tp / (tp + fp) } else { 1 }
    rec <- if (tp > 0) { tp / (tp + fn) } else { 1 }
    f1 <- if (prec + rec > 0) { 2 * prec * rec / (prec + rec) } else { 0 }
    
    # printf('"%s" %f %f %f\n', value, prec, rec, f1)
    data.frame(value=value, prec=prec, rec=rec, f1=f1)
  }))
  
  printf('Accuracy: %f\n', acc)
  printf('Precision: %f\n', mean(a$prec))
  printf('Recall: %f\n', mean(a$rec))
  printf('F1-Score: %f\n', mean(a$f1))
  data.frame(acc=acc, prec=mean(a$prec), rec=mean(a$rec), f1=mean(a$f1))
}

discretize <- function (col, n = 10) {
  cut(col, co.intervals(col, n, 0)[c(1, (n+1):(n*2))], include.lowest = TRUE)
}

pruneTree <- function(t, d) {

  newNode <- t # Copy node
  if (isLeaf(t)) {
    # returns number of errors for this leaf
    newNode[['errors']] <- sum(d[['label']] != t[['label']])
    return(newNode)
  } else {
    
    splitFeature <- t[['splitFeature']]
    
    newChildren <- lapply(t[['children']], function (childNode) {
      # Select data points that are covered by child subtree
      selectedData <- d[d[[splitFeature]] == childNode[['edgeValue']],]
      newChildNode <- pruneTree(childNode, selectedData)
      newChildNode
    })
    subtreeErrors <- sum(sapply(newChildren, function (childNode) {
      childNode[['errors']]
    }))
    
    # Perform Pruning if it reduces error
    majorityClass <- Label(d)
    nodeErrors <- sum(d[['label']] != majorityClass)
    if(nodeErrors < subtreeErrors){
      newNode <- makeLeaf(majorityClass)
      newNode[['edgeValue']] <- t[['edgeValue']]
      newNode[['errors']] <- nodeErrors
      return(newNode)
    }
    else{
      newNode[['errors']] <- subtreeErrors
      return(newNode)
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

kfold <- function(k, d) {
  n <- nrow(d)
  binSize <- floor(n / k)
  d <- d[sample(n),]
  lapply(1:k, function (i) {
    start <- (i - 1) * binSize + 1
    end <- i * binSize
    train <- d[-(start:end),]
    test <- d[(start:end),]
    stopifnot(nrow(train) + nrow(test) == n)
    list(train=train, test=test)
  })
}

crossValidation <- function (folds, features) {
  lapply(folds, function (fold) {
    tree <- GrowTree(fold$train, features)
    printPerformance(predictMany(tree, fold$test), fold$test$label)
  })
}
crossValidationWithPruning <- function (folds, features) {
  lapply(folds, function (fold) {
    tree <- GrowTree(fold$train, features)
    tree <- pruneTree(tree, fold$train)
    printPerformance(predictMany(tree, fold$test), fold$test$label)
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
  a <- crossValidation(kfold(3, data), features)
  str(do.call(rbind, a))
}
# ass1()

prepareWines <- function () {
  wines <- read.csv('winequality-white.csv')
  wineFeatures <- c('fixed.acidity', 'volatile.acidity', 'citric.acid', 'residual.sugar',
                    'chlorides', 'free.sulfur.dioxide', 'total.sulfur.dioxide',
                    'density', 'pH', 'sulphates', 'alcohol')
  
  preprocess <- function (data, features) {
    # Rename 'quality' to 'label'
    names(data)[names(data)=="quality"] <- "label"
    # Discretize all feature columns
    for(f in features) {
      data[[f]] <- discretize(data[[f]], 10)
    }
    data
  }
  
  data <- preprocess(wines, wineFeatures)
  list(data=data, features=wineFeatures)
}

# Ass 2
ass2 <- function () {
  tmp <- prepareWines()
  data <- tmp$data
  features <- tmp$features
  
  tree <- GrowTree(data, features)
  # printTree(tree)
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
ass3 <- function () {
  tmp <- prepareWines()
  data <- tmp$data
  features <- tmp$features
  
  data1 <- data[sample(nrow(data)),]
  mid <- floor(0.75 * nrow(data1))
  trainData <- data1[1:mid,]
  pruneData <- data1[-(1:mid),]
  
  tree <- GrowTree(trainData, features)
  # printPerformance(predictMany(tree, data), data$label)
  prunedTree <- pruneTree(tree, pruneData)
  printf('Nodes: %i %i\n', countNodes(tree), countNodes(prunedTree))
  printf('Leaves: %i\n', countLeaves(prunedTree))
  printf('Max Depth: %i\n', calcMaxDepth(prunedTree))
  printf('Min Depth: %i\n', calcMinDepth(prunedTree))
}
ass3()

# Ass 4
ass4 <- function () {
  tmp <- prepareWines()
  data <- tmp$data
  features <- tmp$features
  
  a <- crossValidation(kfold(10, data), features)
  a <- do.call(rbind, a)
  printf('Acc %f %f\n', mean(a$acc), sd(a$acc))
  printf('Prec %f %f\n', mean(a$prec), sd(a$prec))
  printf('Recall %f %f\n', mean(a$rec), sd(a$rec))
  printf('F1 %f %f\n', mean(a$f1), sd(a$f1))
  
  b <- crossValidationWithPruning(kfold(10, data), features)
  b <- do.call(rbind, b)
  printf('Acc %f %f\n', mean(b$acc), sd(b$acc))
  printf('Prec %f %f\n', mean(b$prec), sd(b$prec))
  printf('Recall %f %f\n', mean(b$rec), sd(b$rec))
  printf('F1 %f %f\n', mean(b$f1), sd(b$f1))
  
  a
}
# a4 <- ass4()
