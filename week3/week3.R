printf <- function(...) cat(sprintf(...))

require(lattice)

# INPUT
customers <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12")
textiles <- c("medium", "few", "medium", "many", "few", "many", "few", "medium", "many", "few", "few", "many")
gifts <- c("few", "medium", "many", "few", "medium", "medium", "many", "few", "few", "few", "many", "many")
avgprice <- c("medium", "low", "medium", "high", "high", "low", "low", "low", "low", "high", "medium", "high")
label <- c("T", "N", "TG", "T", "G", "TG", "G", "N", "T", "N", "G", "TG")


features <- c('textiles', 'gifts', 'avgprice')
data <- data.frame(customers, textiles, gifts, avgprice, label)

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
  sort(table(data$label),decreasing=TRUE)[1]
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
    #iterate over levels
    for(j in 1:length(values)){  
      split_j <- d$label[d[[f[i]]]==values[j]]  #get labels of entries that match values[j]
      entr_j <- entropy(split_j) 
      child_entropy <- child_entropy + length(split_j)/n * entr_j
    }
    info_gain <- c(info_gain,(parent_entropy - child_entropy))
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
  
  #cat(features, "\n")
  #printf(' -> split with %s\n', splitFeature)
  
  subsets <- split(d, d[[splitFeature]])
  
  children <- list()
  
  for(i in 1:length(subsets)) {
    if (nrow(subsets[[i]]) > 0) {
      childNode <- GrowTree(subsets[[i]], remainingFeatures)
    } else {
      childNode <- makeLeaf(Label(d))  
    }
    childNode[['edgeValue']] = names(subsets)[i]
    children[[i]] <- childNode
  }
  
  return(makeTree(splitFeature, children))
}


# Ass 1
tree <- GrowTree(data, features)
printTree(tree)

# # Ass 2
wines <- read.csv('winequality-white.csv')
names(wines)[names(wines)=="quality"] <- "label"
wineFeatures <- c('fixed.acidity', 'volatile.acidity', 'citric.acid', 'residual.sugar',
                  'chlorides', 'free.sulfur.dioxide', 'total.sulfur.dioxide',
                  'density', 'pH', 'sulphates', 'alcohol')


discretize <- function (col, n = 10) {
  stopifnot(require(lattice))
  cut(col, co.intervals(col, n, 0)[c(1, (n+1):(n*2))], include.lowest = TRUE)
}

for(f in wineFeatures) {
  wines[[f]] <- discretize(wines[[f]])
}

tree <- GrowTree(wines, wineFeatures)

printTree(tree)

# Ass 3
#bottomUpREP(tree,wines)
