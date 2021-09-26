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

#' @title Class Node
#'
#' @description Class that models a node in a expression graph
#'
#' @noRd
#'
#' @importFrom R6 R6Class
#' @importFrom datastructures pop size stack insert
#'
#' @examples
#' Node$new("+", "+")
#'
#'
Node <- R6::R6Class("node", list(
  idx = NULL,
  op = NULL,
  val = NULL,
  adj = NULL,
  parents = NULL,
  children = NULL,
  node_name = NULL,
  depth = 0,
  is.leaf = FALSE,
  args = NULL,
  initialize = function(op, node.name, is.leaf = FALSE, value = 0, adjoint = 0) {
    self$op <- op
    self$node_name <- node.name
    self$is.leaf <- is.leaf
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
    st <- datastructures::stack()
    datastructures::insert(st, self)
    while (datastructures::size(st) > 0) {
      node <- datastructures::pop(st)
      if (node$depth > 0) {
        pr <- paste0(
          paste0(rep(" ", 2 * (node$depth - 1)),
                 collapse = ""),
          "-> ",
          node$node_name
        )
      } else {
        pr <- node$node_name
      }
      cat(pr, "\n")
      for (child in rev(node$children)) {
        child$depth <- node$depth + 1
        datastructures::insert(st, child)
      }
    }
  }
))


.idx.factory <- local({
  static <- 0
  function() {
    static <<- static + 1
    static
  }
})

