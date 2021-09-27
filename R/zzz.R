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
  f1 <- function(g, ans, x, y) g
  f2 <- function(g, ans, x, y) g
  defadj("+", f1, f2)

  f1 <- function(g, ans, x, y) {
    ifelse(missing(y), -g, g)
  }
  f2 <- function(g, ans, x, y) -g
  defadj("-", f1, f2)

  f1 <- function(g, ans, x, y) g * y
  f2 <- function(g, ans, x, y) g * x
  defadj("*", f1, f2)

  f1 <- function(g, ans, x, y) g / y
  f2 <- function(g, ans, x, y) -g * x * y^(-2)
  defadj("/", f1, f2)

  f1 <- function(g, ans, x, y) {
    ps <- ifelse(y != 0, y - 1, 1)
    g * y * x^ps
  }
  f2 <- function(g, ans, x, y) {
    ps <- ifelse(x == 0, 1, x)
    g * log(ps) * x^y
  }
  defadj("^", f1, f2)

  f1 <- function(g, ans, x) {
    g / x
  }
  defadj("log", f1)

  f1 <- function(g, ans, x) {
    g * 0.5 * x^(-0.5)
  }
  defadj("sqrt", f1)


  f1 <- function(g, ans, x) {
    g * ans
  }
  defadj("exp", f1)
}
