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


#' @title Compute gradients of a function
#'
#' @description Compute gradients of a function with respect to one of its
#'  arguments
#'
#' @export
#' @docType methods
#' @rdname grad-methods
#'
#' @param f a function for which gradients should be computed
#' @param argnum the number of the argument of the function for which gradients
#'  are computed
#'
#' @return the gradient of f w.r.t argnum
#'
#' @examples
#' f <- function(x, y) {
#'   x + y
#' }
#' grad(f)(1.0, 1.0)
setGeneric(
  "grad",
  function(f, argnum = 1L) {
    standardGeneric("grad")
  },
  package = "jar"
)


#' @rdname grad-methods
setMethod(
  "grad",
  signature = signature(f = "function"),
  function(f, argnum = 1L) {
    .f <- function(...) {
      graph <- expression.graph(f)
      .forward(graph, ...)
      .backward(graph, ...)

      ret <- graph$root$val
      adj <- graph$variables[[argnum]]$adj

      adj
    }

    .f
  }
)


#' @noRd
#' @importFrom datastructures queue insert size pop
.forward <- function(graph, ...) {
  args <- list(...)
  root <- graph$root
  variables <- graph$variables
  leaves <- graph$leaves

  for (i in seq(names(variables))) {
    arg <- names(variables)[i]
    variables[[arg]]$val <- args[[i]]
  }

  st <- datastructures::queue()
  for (l in leaves) datastructures::insert(st, l)

  while (datastructures::size(st) > 0) {
    n <- datastructures::pop(st)
    if (length(n$children) > 0) {
      stopifnot("all leaf needs values set" = !n$is.leaf)
      vals <- sapply(n$children, function(n) n$val)
      n$val <- n$execute(vals)
    }
    for (c in n$parents) {
      datastructures::insert(st, c)
    }
  }

  root
}


#' @noRd
#' @importFrom datastructures queue insert size pop
.backward <- function(graph, ...) {
  root <- graph$root
  variables <- graph$variables

  st <- .get.topological(root)
  while (datastructures::size(st) > 0) {
    n <- datastructures::pop(st)

    for (p in n$parents) {
      vals <- lapply(p$children, function(c) c$val)
      idxs <- lapply(p$children, function(c) c$idx)
      idx <- which(idxs == n$idx)
      adj.fun <- adjs[[p$op]][[idx]]
      adji <- do.call(adj.fun, c(p$adj, p$val, vals))
      n$adj <- n$adj + adji
    }
  }
}


.get.topological <- function(root) {
  st <- datastructures::stack()
  datastructures::insert(st, root)

  cnts <- datastructures::hashmap()
  while (datastructures::size(st) > 0) {
    n <- datastructures::pop(st)
    idx <- as.character(n$idx)

    if (idx %in% datastructures::keys(cnts)) {
      cnts[idx] <- cnts[idx][[1]] + 1
    } else {
      cnts[idx] <- 1
      for (c in n$children) datastructures::insert(st, c)
    }
  }

  els <- datastructures::queue()
  st <- datastructures::queue()
  datastructures::insert(st, root)
  while (datastructures::size(st) > 0) {
    n <- datastructures::pop(st)
    datastructures::insert(els, n)
    for (c in n$children) {
      idx <- as.character(c$idx)
      if (cnts[idx] == 1) {
        datastructures::insert(st, c)
      } else {
        cnts[idx] <- cnts[idx][[1]] - 1
      }
    }
  }

  els
}
