% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/NetworkVariable.r
\name{netvar}
\alias{netvar}
\title{Creates a NetworkVariable object}
\usage{
netvar(x, n.col.by.network, valids = valids(x[[1]]),
  missings = missings(x[[1]]), description, directed = T, metadata.depends,
  ...)
}
\arguments{
\item{x}{A \code{Rsocialdata} object. The variables specifying who cites who within the networks, given in a tabular form.}

\item{n.col.by.network}{a}

\item{valids}{A \code{named numeric}. Specify which survey codes correspond to valids cases and label them.}

\item{missings}{A \code{named numeric}. Specify which survey codes correspond to valids cases and label them.}

\item{description}{A \code{character} of length 1. A short description for the \code{NetworkMetavar} object.}

\item{directed}{A logical. Are the networks directed? Default is \code{TRUE}.}

\item{metadata.depends}{A \code{character} of length 1. Specify the \code{NetworkMetadata} object containing covariates describing individuals of the networks.}

\item{...}{Additional parameters. When the number of cited people is not the same for all networks, the \code{n.cited.by.network} (a \code{numeric}) allows to specify them.}
}
\description{
The function transform network data given in a tabular form to a \code{NetworkVariable} object.
}

