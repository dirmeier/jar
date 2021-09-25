

parse <- function(f) {
  .args <- as.list(f)
  variables <- .get.vars(.args)
  .raw <- .args
  .raw <- as.list(.raw[[length(.raw)]])[[2]]
  .ast <- ast(.raw)
  root <- as.character(.ast[[1]])
  root <- Node$new(root, root, adjoint=1.0)
  .recurse(root, .ast[2:length(.ast)], variables)

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


.recurse <- function(.parent, .ast, .variables) {
  for (i in seq(.ast)) {
    .n <- .ast[[i]]
    if (is.list(.n)) {
      .recurse(.parent, .n, .variables)
    }
    else if (is.variable(.n)) {
      .n <- .variables[[as.character(.n)]]
      .n$add_parent(.parent)
      .parent$add_child(.n)
    }
    else if (is.scalar(.n)) {
      .n <- Node$new(.n, .n, value=as.double(.n))
      .n$add_parent(.parent)
      .parent$add_child(.n)
    }
    else if (is.binary.op(.n)) {
      .n <- Node$new(as.character(.n), as.character(.n))
      .n$add_parent(.parent)
      .parent$add_child(.n)
      .recurse(.n, .ast[(i + 1):length(.ast)], .variables)
      return(1)
    }
  }
}

ast <- function(ee) purrr::map_if(as.list(ee), is.call, ast)

is.binary.op <- function(s) {
  s <- as.character(s)
  length(s) == 1 && s %in% c("+", "-", "*", "/", "^", "**")
}

is.variable <- function(s) {
  is.symbol(s) && ! is.binary.op(s)
}

is.scalar <- function(s) {
  is.numeric(s)
}
