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


#' @noRd
.onLoad <- function(libname, pkgname) {
  f1 <- function(x, y) 1
  f2 <- function(x, y) 1
  defadj("+", f1, f2)

  f1 <- function(x, y) 1
  f2 <- function(x, y) -1
  defadj("-", f1, f2)

  f1 <- function(x, y) y
  f2 <- function(x, y) x
  defadj("*", f1, f2)

  f1 <- function(x, y) 1 / y
  f2 <- function(x, y) x / y**2
  defadj("/", f1, f2)

  f1 <- function(x, y) {
    ps <- ifelse(y != 0, y - 1, 1)
    y * x^ps
  }
  f2 <- function(x, y) {
    ps <- ifelse(x == 0, 1, x)
    log(ps * x^y)
  }
  defadj("^", f1, f2)

  f1 <- function(x) {
    1 / x
  }
  defadj("log", f1)

  f1 <- function(x) {
    0.5 * x^(-0.5)
  }
  defadj("sqrt", f1)


  f1 <- function(x) {
    exp(x)
  }
  defadj("exp", f1)
}
