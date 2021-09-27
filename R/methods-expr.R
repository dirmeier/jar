# jar: reverse-mode autodiff for R
#
# Copyright (C) Simon Dirmeier
#
# This file is part of jar
#
# jar is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# jar is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with jar If not, see <http://www.gnu.org/licenses/>.


#' @title Construct an expression graph
#'
#' @description  Construct an expression graph from a function
#'
#' @export
#' @docType methods
#' @rdname expression-methods
#'
#' @param f a function for which the expression graph should be constructed
#'
#' @return returns a list with the root node, variable nodes and leaf nodes
#'
#' @examples
#' f <- function(x, y) {
#'   x + y
#' }
#' expression.graph(f)
setGeneric(
  "expression.graph",
  function(f) {
    standardGeneric("expression.graph")
  },
  package = "jar"
)


#' @rdname expression-methods
setMethod(
  "expression.graph",
  signature = signature(f = "function"),
  function(f) {
    args <- as.list(f)
    variables <- .get.vars(args)

    raw <- args
    raw <- as.list(raw[[length(raw)]])[[2]]
    ast <- .ast(raw)
    ast <- if (ast[[1]] == "(") ast[[2:length(ast)]] else ast

    root <- as.character(ast[[1]])
    root <- Node$new(root, root, adjoint = 1.0)
    .recurse(root, ast[2:length(ast)], variables)

    leaves <- .get.leaves(root)
    list(root = root, variables = variables, leaves = leaves)
  }
)


#' @noRd
#' @importFrom datastructures stack insert pop size
.get.leaves <- function(node) {
  leaves <- list()
  st <- datastructures::stack()
  datastructures::insert(st, node)

  while (datastructures::size(st) > 0) {
    n <- datastructures::pop(st)
    for (c in n$children) {
      datastructures::insert(st, c)
    }
    if (length(n$children) == 0) {
      leaves <- c(leaves, n)
    }
  }

  leaves
}


#' @noRd
.get.vars <- function(args) {
  variables <- names(args)
  variables <- variables[variables != ""]

  variables <- lapply(variables, function(variable) {
    Node$new("variable", variable, TRUE)
  })

  hash <- list()
  for (i in seq(variables)) {
    v <- variables[[i]]
    hash[[v$node.name]] <- v
  }

  hash
}


#' @noRd
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
      n <- Node$new(n, n, value = .as.double(n))
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
    else {
      stop("could not parse tree")
    }
  }
}


#' @noRd
#' @importFrom purrr map_if
.ast <- function(tree) purrr::map_if(as.list(tree), is.call, .ast)


#' @noRd
.as.double <- function(s) {
  if (is.numeric(s)) {
    return(s)
  }
  switch(
    as.character(s),
    "pi" = 3.141592653589793115998,
    stop("didnt find constant")
  )
}


#' @noRd
.is.parens <- function(s) {
  s <- as.character(s)
  length(s) == 1 && as.character(s) == "("
}


#' @noRd
.is.binary.op <- function(s) {
  s <- as.character(s)
  length(s) == 1 && s %in% c("+", "-", "*", "/", "^", "**")
}


#' @noRd
.is.unary.op <- function(s) {
  s <- as.character(s)
  length(s) == 1 && s %in% c("log", "exp")
}


#' @noRd
.is.variable <- function(s) {
  .is.symbol(s) && !.is.binary.op(s) && !.is.unary.op(s)
}


#' @noRd
.is.scalar <- function(s) {
  is.numeric(s) || as.character(s) %in% c("pi")
}


#' @noRd
.is.symbol <- function(s) is.symbol(s)
