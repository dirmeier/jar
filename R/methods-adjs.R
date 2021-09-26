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



#' @title Define custom adjoint methods
#'
#' @description Define methods to take analytic gradients for a custom function
#'
#' @docType methods
#' @rdname defadj-methods
#'
#' @param f a custom function
#' @param ... list of functions that define how gradients are computed w.r.t
#'  the number of an argument
#
setGeneric(
  "defadj",
  function(f, ...) {
    standardGeneric("defadj")
  },
  package = "jar"
)


#' @rdname defadj-methods
setMethod(
  "defadj",
  signature = signature(f = "ANY"),
  function(f, ...) {
    ajs <- list(...)
    h <- list()
    for (i in seq(ajs)) {
      h[[i]] <- ajs[[i]]
    }
    adjs[[f]] <<- h
  }
)
