printf <- function(...) cat(sprintf(...))

# INPUT
customers <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12")
textiles <- c("medium", "few", "medium", "many", "few", "many", "few", "medium", "many", "few", "few", "many")
gifts <- c("few", "medium", "many", "few", "medium", "medium", "many", "few", "few", "few", "many", "many")
avgprice <- c("medium", "low", "medium", "high", "high", "low", "low", "low", "low", "high", "medium", "high")
category <- c("T", "N", "TG", "T", "G", "TG", "G", "N", "T", "N", "G", "TG")


features <- c('textiles', 'gifts', 'avgprice')
data <- data.frame(customers, textiles, gifts, avgprice, category)

# OUTPUT


isHomogenous <- function(d){
  return(all(d$category[1] == d$category))
}

entropy <- function(a){
  a <- table(a) # group by and count
  a <- a / sum(a) # empiric probability
  a <- -a * log2(a)
  return (sum(a) / 2)
}

BestSplit <- function(d, f){
  n <- nrow(d)
  
  # calculate initial entropy (d_prev)
  d_0 <- entropy(d$category)  #entropy of d0 (no split)
  info_gain <- c()
  
  # do split for each feature
  for(i in 1:length(f)) { 
    levels <- levels(d[[f[i]]])
    entr_splits <- 0
    #iterate over levels
    for(j in 1:length(levels)){  
      d_j <- d$category[d[[f[i]]]==levels[j]]  #get category of entries that match levels[j]
      entr_j <- entropy(d_j) 
      entr_splits <- entr_splits + n * entr_j
    }
    info_gain <- c(info_gain,(entr_splits - d_0))
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

printTree <- function(tree, prefix = '') {
  if (!is.null(tree[['edgeValue']])) {
    printf('%sEdge: %s\n', prefix, tree[['edgeValue']])
  }
  if (!is.null(tree[['label']])) {
    printf('%sLabel: %s\n', prefix, tree[['label']])
  } else {
    printf('%sSplit feature: %s\n', prefix, tree[['splitFeature']])
    for (child in tree[['children']]) {
      printTree(child, sprintf('  %s', prefix))
    }
  }
}

GrowTree <- function(d, f) {
  if( isHomogenous(d) ) {
    return(makeLeaf(data$category[1]))
  }
  
  splitFeature <- BestSplit(d, f)
  remainingFeatures <- subset(f, f != splitFeature)
  
  #cat(features, "\n")
  #printf(' -> split with %s\n', splitFeature)
  
  subsets <- split(d, d[[splitFeature]])
  
  children <- list()
  
  for(i in 1:length(subsets)) {
    childNode <- GrowTree(subsets[[i]], remainingFeatures)
    childNode[['edgeValue']] = names(subsets)[i]
    children[[i]] <- childNode
  }
  
  return(makeTree(splitFeature, children))
}

tree <- GrowTree(data, features)
printTree(tree)


