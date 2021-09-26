grad <- function(f, argnum=1) {
  .f <- function(...) {
    graph      <- expression.graph(f)
    .forward(graph, ...)
    .backward(graph, ...)

    ret <- graph$root$val
    adj <- graph$variables[[argnum]]$adj

    list(adj, graph)
  }

  .f
}


.forward <- function(graph, ...) {
  args      <- list(...)
  root       <- graph$root
  variables  <- graph$variables
  leafs      <- graph$leafs


  for (i in seq(names(variables))) {
    arg <- names(variables)[i]
    variables[[arg]]$val <- args[[i]]
  }

  st <- queue()
  for (l in leafs) insert(st, leafs)

  while (size(st) > 0) {
    n <- pop(st)
    if (length(n$children) > 0) {
      stopifnot("all leaf needs values set"=!n$is_leaf)
      vals  <- sapply(n$children, function(n) n$val)
      if (length(vals) == 1 && n$op == "-") vals <- c(0.0, vals)
      n$val <- n$execute(vals)
    }
    for (c in n$parents) insert(st, c)
  }

  root
}


.backward <- function(graph, ...) {
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


