
expression.graph <- function(f) {
  args <- as.list(f)
  variables <- .get.vars(args)

  raw <- args
  raw <- as.list(raw[[length(raw)]])[[2]]
  ast <- .ast(raw)
  ast <- if(ast[[1]] == "(") ast[[2:length(ast)]] else ast


  root <- as.character(ast[[1]])
  root <- Node$new(root, root, adjoint=1.0)
  .recurse(root, ast[2:length(ast)], variables)

  leafs <- .get.leafs(root)
  list(root=root, variables=variables, leafs=leafs)
}


.get.leafs <- function(node) {
  leafs <- list()
  st <- stack()
  insert(st, node)
  while (size(st) > 0) {
    n <- pop(st)
    for (c in n$children) insert(st, c)
    if (length(n$children) == 0) leafs <- c(leafs, n)
  }

  leafs
}


.get.vars <- function(args) {
  variables <- names(args)
  variables <- variables[variables != ""]
  variables <- lapply(variables, function(variable) {
    Node$new("variable", variable, TRUE)
  })

  hash <- list()
  for (i in seq(variables)) {
    v <- variables[[i]]
    hash[[v$node_name]] <- v
  }

  hash
}


.recurse <- function(parent, ast, variables) {
  for (i in seq(ast)) {
    n <- ast[[i]]
    if (.is.parens(n)) {
      .recurse(parent, ast[[(i + 1):length(ast)]], variables)
      return(1)
    }
    else if (is.list(n)) {
      .recurse(parent, n, variables)
    }
    else if (.is.scalar(n)) {
      n <- Node$new(n, n, value=.as.double(n))
      n$add_parent(parent)
      parent$add_child(n)
    }
    else if (.is.variable(n)) {
      n <- variables[[as.character(n)]]
      n$add_parent(parent)
      parent$add_child(n)
    }
    else if (.is.binary.op(n) || .is.unary.op(n)) {
      n <- Node$new(as.character(n), as.character(n))
      n$add_parent(parent)
      parent$add_child(n)
      .recurse(n, ast[(i + 1):length(ast)], variables)
      return(1)
    }
  }
}


.ast <- function(tree) purrr::map_if(as.list(tree), is.call, .ast)


.as.double <- function(s) {
  if (is.numeric(s)) {
    return(s)
  }
  switch(
    as.character(s),
    "pi"=3.141592653589793115998,
    stop("didnt find constant")
  )
}

.is.parens <- function(s){
  s <- as.character(s)
  length(s) == 1 &&  as.character(s) == "("
}

.is.binary.op <- function(s) {
  s <- as.character(s)
  length(s) == 1 && s %in% c("+", "-", "*", "/", "^", "**")
}

.is.unary.op <- function(s) {
  s <- as.character(s)
  length(s) == 1 && s %in% c("log", "exp")
}

.is.variable <- function(s) {
  .is.symbol(s) && ! .is.binary.op(s) && ! .is.unary.op(s)
}

.is.scalar <- function(s) {
  is.numeric(s) || as.character(s) %in% c("pi")
}

.is.symbol <- function(s) is.symbol(s)
