% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forest.varIJ.R
\name{forest.varIJ}
\alias{forest.varIJ}
\title{The infinitesimal jackknife for random forests}
\usage{
forest.varIJ(object)
}
\arguments{
\item{object}{A random forest trained with replace = TRUE}
}
\description{
Calculate the infinitesimal jackknife variance
}
\examples{
features = birds[,setdiff(names(birds),"detected")]
response = birds[,"detected"]
forestobject = forest(x=features,y=response)
varIJ = forest.varIJ(forestobject)
}
\keyword{forest,}
\keyword{infinitesimal}
\keyword{jackknife}
\keyword{random}
\keyword{variance,}
