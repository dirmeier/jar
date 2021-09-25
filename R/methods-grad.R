library("lobstr")
library("datastructures")
library("purrr")

grad <- function(f, argnum=1) {
  .f <- function(...) {
    graph      <- build.expression.graph(f)
    forward(graph, ...)
    graph$root$print_tree()

    backward(graph)

    ret <- graph$root$val
    adj <- graph$variables[[1]]$adj

    list(ret, adj, graph)
  }

  .f
}


build.expression.graph <- function(f) {
  parse(f)
}


forward <- function(graph, ...) {
  args      <- list(...)
  root       <- graph$root
  variables  <- graph$variables
  leafs      <- graph$leafs

  stopifnot(
    "argument sizes dont match. is your function properly set?"=length(args) == length(variables)
  )

  for (i in seq(names(variables))) {
    arg <- names(variables)[i]
    variables[[arg]]$val <- args[[i]]
  }

  st <- queue()
  for (l in leafs) insert(st, leafs)
  while (size(st) > 0) {
    n <- pop(st)
    if (is.null(n$val)) {
      stopifnot("all leaf needs values set"=!n$is_leaf)
      vals  <- sapply(n$children, function(n) n$val)
      n$val <- Reduce(n$op, vals)
    }
    for (c in n$parents) insert(st, c)
  }

  root
}


backward <- function(graph, ...) {
  root       <- graph$root
  variables  <- graph$variables
  leafs      <- graph$leafs

  st <- queue()
  insert(st, root)
  while (size(st) > 0) {
    n <- pop(st)
    for (p in n$parents) {
      vals <- lapply(p$children, function(c) c$val)
      idxs <- lapply(p$children, function(c) c$idx)
      idx <- which(idxs == n$idx)
      adj.fun <- adjs[[p$op]][[idx]]
      adj.n <- do.call(adj.fun, vals)
      adji <- p$adj * adj.n

      n$adj <- n$adj + adji
    }
    for (c in n$children) insert(st, c)
  }

}


f <- function(x) {
  0.5 * x^2
}


s <- grad(f)

v <- s(40)
print(v[[1]])
print(v[[2]])



