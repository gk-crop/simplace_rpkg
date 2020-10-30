#' Search for simplace installation
#' 
#' Checks directories if they contain simplace_core, simplace_modules and 
#' optionally simplace_run (or a data directory given by the user) and returns
#' the first match (or optionally all matches).
#' There is no check whether the installation is really working.  
#' 
#' Beside the checks for some standard directories (like home directory, current
#' working dir and drives c: to g:) and their subdirectories (workspace, simplace,
#' java/simplace) the user can give a vector of additional directories.
#' 
#' @param directories a list of additional directories where to look - 
#' @param tryStandardDirs whether to check for typical installation directories (default)
#' @param firstMatchOnly returns only the first installation found
#' @param simulationsDir directory that contains user simulations (e.g. simplace_run)
#' @param ignoreSimulationsDir don't check for the simulation dir
#' @param verbose prints messages if no or more than one installation found
#' @return matching directory/ies as character vector
#' @export
findSimplaceInstallations <- function (directories=c(), 
                                       tryStandardDirs=TRUE,
                                       firstMatchOnly = TRUE, 
                                       simulationsDir = "simplace_run",
                                       ignoreSimulationsDir = FALSE,
                                       verbose=TRUE
                                       )
{
  parents <- c(path.expand("~"),"d:","c:","e:","f:","g:",getwd())
  subdirs <- c("workspace/","simplace/","java/simplace/")
  standarddirs <- paste(rep(parents,length(subdirs)),rep(subdirs,each=length(parents)),sep="/")
  dirs <- if(tryStandardDirs) c(directories, standarddirs) else directories
  required <- c("simplace_core","simplace_modules")
  required <- if(ignoreSimulationsDir) required else c(required,simulationsDir)
  
  found <- character(0)
  if(length(dirs)>0)
  {
    found <- dirs[sapply(dirs,function(d) length(intersect(dir(d),required))==length(required))]
  }
  if(verbose & length(found)==0)
  {
    message("Could not detect Simplace automatically.")
  }
  if(verbose & length(found)>1 & firstMatchOnly)
  {
    message("Found more than one Simplace installation. Returning the first one. To get all installations, please use the option allmatches=TRUE")
  }
  
  ifelse(firstMatchOnly & length(found)>0,found[1],found)
  
}
