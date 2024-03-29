% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimplaceWrapper.R
\name{resultToDataframe}
\alias{resultToDataframe}
\title{Convert result to dataframe}
\usage{
resultToDataframe(result, expand = FALSE, from = NULL, to = NULL)
}
\arguments{
\item{result}{handle to the data container returned by \code{\link{getResult}}}

\item{expand}{if true columns with arrays are partially expanded}

\item{from}{start of the result range, if to/from are not set, full result is returned}

\item{to}{end of the result range, if to/from are not set, full result is returned}
}
\value{
data.frame with scalar output columns
}
\description{
All scalar output columns are transformed to appropriate R objects
and then glued together in a dataframe. Array outputs columns are ignored.
}
\examples{
\dontrun{
simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
openProject(simplace, Solution)
parameter <- list(vTempLimit = 32)
simid <- createSimulation(simplace,parameter)
runSimulations(simplace)
result <- getResult(simplace,"DIAGRAM_OUT", simid);
closeProject(simplace)
resultframe <- resultToDataframe(result)
resultframe[3,]}
}
\seealso{
\code{\link{resultToList}} returns the output columns as list
}
