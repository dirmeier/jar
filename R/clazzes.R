.idx.factory <- local({
  static <- 0
  function() { static <<- static + 1; static }
})



Node <- R6Class("node", list(
  idx = NULL,
  op = NULL,
  val = NULL,
  adj = NULL,
  parents = NULL,
  children = NULL,
  node_name = NULL,
  depth = 0,
  is_leaf = FALSE,
  args = NULL,
  initialize = function(op, node.name, is_leaf=FALSE, value=0, adjoint=0) {
    self$op <- op
    self$node_name <- node.name
    self$is_leaf <- is_leaf
    self$parents <- list()
    self$children <- list()
    self$val <- value
    self$adj <- adjoint
    self$idx <- .idx.factory()
  },
  execute = function(vals) {
    x <- if (length(vals) == 1) {
      .Primitive(self$op)(vals)
    } else {
      Reduce(self$op, vals)
    }
    x
  },
  add_parent = function(parent) {
    self$parents <- c(self$parents, parent)
  },
  add_child = function(child) {
    self$children <- c(self$children, child)
  },
  print_tree = function() {
    st <- stack()
    insert(st, self)
    while (size(st) > 0) {
      node <- pop(st)
      if (node$depth > 0)
        pr <- paste0(paste0(rep(" ", 2 * (node$depth - 1)), collapse=""), "-> ", node$node_name)
      else
        pr <- node$node_name
      cat(pr, "\n")
      for (child  in rev(node$children)) {
        child$depth <- node$depth + 1
        insert(st, child)
      }
    }
  }
))
