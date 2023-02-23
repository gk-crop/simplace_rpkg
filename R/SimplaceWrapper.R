# Functions to communicate with Simplace
#
# Provides methods to run Solutions and Projects in Simplace 
# and to retrieve and convert the data 
# 
# @author Gunther Krauss
###############################################################################

#' simplace: Interface to use the modelling framework SIMPLACE
#' 
#' Interface to interact with the modelling framework SIMPLACE and to
#' parse the results of simulations
#' 
#' Package needs a Java Runtime Environment as well as an installation of SIMPLACE. 
#' See \href{http://www.simplace.net/}{www.simplace.net} for more information about SIMPLACE.
#' 
#' @author {Gunther Krauss}
#'   
#' 
#' @examples
#'   \dontrun{
#'     SimplaceInstallationDir <- "D:/java/simplace/"
#'     
#'     SimplaceWorkDir <- "D:/java/simplace/simplace_run/simulation/"
#'     SimplaceOutputDir <-  "D:/java/simplace/simplace_run/output/"
#'     
#'     Solution <- "D:/java/simplace/simplace_run/simulation/gk/solution/complete/Complete.sol.xml"
#'     
#'     simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#'     
#'     openProject(simplace, Solution)
#'     
#'     parameter <- list()
#'     parameter$vTempLimit <- 32
#'     
#'     simid <- createSimulation(simplace,parameter)
#'     runSimulations(simplace)
#'     
#'     result <- getResult(simplace,"DIAGRAM_OUT", simid);
#'     
#'     closeProject(simplace)
#'     
#'     resultlist <- resultToList(result)
#'     resultframe <- resultToDataframe(result)
#'   }
#' 
#' @references \href{http://www.simplace.net/}{www.simplace.net}
#'
#' @docType package
#' @name simplace
NULL

########## Some helper definitions for calling/parsing ############

nullObject <- rJava::.jnull(class="java/lang/Object") # java null object
nullString <- rJava::.jnull(class="java/lang/String") # java null string


########## Initialisation ####################

#' Initialisation of Framework
#' 
#' Initializes the JVM and creates the SimplaceWrapper object which 
#' is used to interact with Simplace.
#' 
#' @param InstallationDir directory where simplace_core, simplace_modules and simplace_run are located
#' @param WorkDir working directory where solutions, projects and data resides (_WORKDIR_)
#' @param OutputDir directory for output (_OUTPUTDIR_)
#' @param ProjectsDir optional directory for project data  (_PROJECTSDIR_)
#' @param DataDir optional directory for data (_DATADIR_)
#' @param additionalClasspaths vector with class paths relative to InstallationDir that are to be added
#' @param javaparameters parameters that are passed to the java virtual machine
#' @param force.init (re)initialize a running JVM, see \code{\link{.jinit}}
#' @return handle to the SimplaceWrapper object
#' @export
initSimplace <- function(InstallationDir = findFirstSimplaceInstallation(),
                         WorkDir = paste0(InstallationDir,"simplace_run/simulation/"),
                         OutputDir = paste0(InstallationDir,"simplace_run/output/"),
                         ProjectsDir = nullString, 
                         DataDir = nullString, 
                         additionalClasspaths = c(),
                         javaparameters = getOption("java.parameters"), 
                         force.init=TRUE)
{
  
  rJava::.jinit(parameters=javaparameters, force.init=force.init) # inits java
  
  
  
  libjars = paste0("simplace_core/lib/",
                   dir(paste0(InstallationDir,"simplace_core/lib/"),
                       recursive=TRUE, pattern=".jar|.JAR"))
  
  # add the classpaths  
  classpaths = c(
    "simplace_core/build/classes",
    "simplace_core/conf",
    "simplace_modules/build/classes",
    "simplace_modules/conf",
    "simplace_run/build/classes",
    "simplace_run/conf",
    "simplace_core/res/files",
    libjars,
    additionalClasspaths
  )
  sapply(classpaths, function(s) rJava::.jaddClassPath(paste(InstallationDir,s,sep="")))  
  
  # create and return an instance of RSimplaceWrapper class
  rJava::.jnew("net/simplace/sim/wrapper/SimplaceWrapper", WorkDir, OutputDir, ProjectsDir, DataDir)
}



######################## Call Simplace Methods ######################


#' Opens a Simplace project
#' 
#' Initializes a project. The absolute path to a solution file is mandatory. 
#' Project file is optional.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param solution solution file with absolute path or path relative to workdir
#' @param project project file with absolute path or path relative to workdir, can be omitted to run solution only
#' @param parameterList a list with the parameter name as key and parametervalue as value
#' @seealso \code{\link{closeProject}}
#' @export
openProject <- function (simplace, solution, project=nullString, parameterList=NULL)
{
  wd <- trimws(getSimplaceDirectories(simplace)['_WORKDIR_'],"right","[/\\\\]")
  if(!file.exists(solution)) {
    newsol <- paste0(wd,"/",solution)
    if(file.exists(newsol)){
      solution <- newsol
    }
  }

  if(is.character(project) && nchar(project)>0) {
    if(!file.exists(project)) {
      newproj <- paste0(wd,"/",project)
      if(file.exists(newproj)){
        project <- newproj
      }
    }
  }
  paramObject <- parameterListToStringArray(parameterList)
  invisible(rJava::.jcall(simplace, "Lnet/simplace/sim/FWSimSession;", "prepareSession", project, solution, paramObject))
}


#' Close Project
#' 
#' Call to the shutDown method of the simulation.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @seealso \code{\link{openProject}}
#' @export
closeProject <- function (simplace)
{
  rJava::.jcall(simplace,"V","shutDown")
}


#' Creates a simulation and substitute parameters
#' 
#' Creates a simulation from the opened project and substitutes
#' the values of the parameters given in the parameter list. 
#' Simulation will be put into the queue by default.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param parameterList a list with the parameter name as key and parametervalue as value
#' @param queue boolean - simulation is added to queue if true, else start new queue 
#' @return id of the created simulation
#' @export
#' @seealso \code{\link{runSimulations}}, \code{\link{resetSimulationQueue}}
createSimulation <- function (simplace,parameterList=NULL, queue=FALSE) {
  paramObject <- parameterListToStringArray(parameterList)
  if(!queue)
  {
    rJava::.jcall(simplace,"V","resetSimulationQueue") 
  }
  id <- rJava::.jcall(simplace,"Lnet/simplace/sim/FWSimSimulation;","createSimulation",paramObject)
  invisible(rJava::.jcall(id,"S","getID"))
}


#' Run the created simulations
#' 
#' Run the created simulations from the queue. If the queue is empty, the
#' last created simulation will be run.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param selectsimulation if true keeps a selected simulation
#' @export
#' @seealso \code{\link{createSimulation}}, \code{\link{resetSimulationQueue}}
#' @examples
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution)
#' parameters <- list()
#' parameters$vLUE <- 3.0
#' s1 <- createSimulation(simplace, parameters,queue=TRUE)
#' parameters$vLUE <- 3.2
#' s2 <- createSimulation(simplace, parameters,queue=TRUE)
#' runSimulations(simplace)
#' ...
#' parameters$vLUE <- 2,8
#' s3 <- createSimulation(simplace, parameters,queue=TRUE)
#' runSimulations(simplace)
#' ...
#' closeProject(simplace)   }
runSimulations <- function(simplace, selectsimulation=FALSE)
{
  rJava::.jcall(simplace, "V", "runSimulations", selectsimulation )
}


#' Clears the list of simulations
#' 
#' Simulation list is cleared
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @export
#' @seealso \code{\link{createSimulation}}, \code{\link{runSimulations}}
resetSimulationQueue <- function (simplace) {
  rJava::.jcall(simplace,"V","resetSimulationQueue")
}


#' Runs the opened project
#' 
#' Runs the simulation(s) as defined in the solution and project files. 
#' There is no accessible MEMORY output, but one can load the CSV or 
#' database output.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @export
#' @examples
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution, Project)
#' runProject(simplace)
#' ...
#' closeProject(simplace)   }
runProject <- function(simplace) {
  rJava::.jcall(simplace,"V","run")
}


#' Changes values of the current simulation
#' 
#' Sets values of arbitrary SimVariables in a simplace simulation.
#' Useful if you want to couple simplace with another simulation 
#' and interchange values daily.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param parameterList a list with the parameter name as key and parametervalue as value
#' @param simulationNumber number of simulation in the queue whose parameters should be set (default first simulation)
#' @export
#' @examples
#' \dontrun{
#' for(i in 1:365)
#' {
#'   param <- list(iRain=0.0)
#'   setSimulationValue(simplace,param)
#'   step(simplace)
#' }
#' }
#' 
setSimulationValues <- function(simplace, parameterList=NULL, simulationNumber = 1) {
  paramObject <- parameterListToStringArray(parameterList)
  rJava::.jcall(simplace,"V","setSimulationValues", as.integer(simulationNumber-1), paramObject)
}

#' Changes values of the all simulations in queue
#' 
#' Sets values of arbitrary SimVariables in a simplace simulation.
#' Useful if you want to couple simplace with another simulation 
#' and interchange values daily.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param parameterLists a list of parameter lists for each simulation 
#' @export
#' @examples
#' \dontrun{
#' for(i in 1:365)
#' {
#'   params <- list()
#'   params[[1]] <- list(iRain=0.0)
#'   params[[2]] <- list(iRain=2.0)
#'   setAllSimulationValue(simplace,params)
#'   stepAll(simplace)
#' }
#' }
#' 
setAllSimulationValues <- function(simplace, parameterLists=NULL) {
  paramObject <- parameterListsToStringArray(parameterLists)
  rJava::.jcall(simplace,"V","setAllSimulationValues",paramObject)
}

#' Run simulation stepwise
#' 
#' Performs \code{count} steps of the simulation and returns the values from 
#' the actual variable map. Can be called consecutively.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param count number of steps to be performed
#' @param filter vector of the variable names to be included in the result. If not set, all variables are returned
#' @param parameterList list of parameter values indexed by parameter name
#' @param simulationNumber number of simulation in the queue that should be run stepwise (default first simulation)
#' @return handle to the data container which has to be processed afterwards
#' @export
#' @examples
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution)
#' createSimulation(simplace)
#' vm <- stepSimulation(simplace,count=22)
#' vm_s <- stepSimulation(simplace,filter=c("CURRENT.DATE","LintulBiomass.sWSO"),count=18)
#' closeProject(simplace)   }
#' 
stepSimulation <- function (simplace, count=1, filter=NULL, parameterList=NULL, simulationNumber = 1)
{
  returntype = "Lnet/simplace/sim/wrapper/DataContainer;"
  withFilter = ("character" %in% class(filter) && length(filter)>0)
  withParameter = ("list" %in% class(parameterList) && length(parameterList)>0)
  simnr = as.integer(simulationNumber-1)
  cnt = as.integer(count)
  if (withParameter) {
    paramObject = parameterListToStringArray(parameterList)
    if (withFilter) {
      rJava::.jcall(simplace, returntype, "stepSpecific", simnr, paramObject, filter, cnt)
    } else {
      rJava::.jcall(simplace, returntype, "stepSpecific", simnr, paramObject, cnt)
      
    }
  } else {
    if (withFilter) {
      rJava::.jcall(simplace, returntype, "stepSpecific", simnr, filter, cnt)
    } else {
      rJava::.jcall(simplace, returntype, "stepSpecific", simnr, cnt)
    }
    
  }
}



#' Run all simulations in queue stepwise
#' 
#' Performs \code{count} steps of the simulation and returns the values from 
#' the actual variable map. Can be called consecutively.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param count number of steps to be performed
#' @param filter vector of the variable names to be included in the result. If not set, all variables are returned
#' @param parameterLists a list of parameter lists for each simulation 
#' @return handle to an array of data containers which has to be processed afterwards
#' @export
#' @examples
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution)
#' createSimulation(simplace)
#' vm <- stepAllSimulation(simplace,count=22)
#' vm_s <- stepAllSimulation(simplace,filter=c("CURRENT.DATE","LintulBiomass.sWSO"),count=18)
#' closeProject(simplace)   }
#' 
stepAllSimulations <- function (simplace, count=1, filter=NULL, parameterLists=NULL)
{
  result <- NULL
  returntype = "[Lnet/simplace/sim/wrapper/DataContainer;"
  withFilter = ("character" %in% class(filter) && length(filter)>0)
  withParameter = ("list" %in% class(parameterLists) && length(parameterLists)>0)
  cnt = as.integer(count)
  if (withParameter) {
    paramObject <- parameterListsToStringArray(parameterLists)
    if (withFilter) {
      result <- rJava::.jcall(simplace, returntype, "stepAll", paramObject, filter, cnt)
    } else {
      result <- rJava::.jcall(simplace, returntype, "stepAll", paramObject, cnt)
      
    }
  } else {
    if (withFilter) {
      result <- rJava::.jcall(simplace, returntype, "stepAll", filter, cnt)
    } else {
      result <- rJava::.jcall(simplace, returntype, "stepAll", cnt)
    }
    
  }
  result
}

#' Lists IDs of the performed simulations
#' 
#' Returns a vector with the IDs of the simulations. IDs are required to
#' get the output of the simulations.
#' 
#' @param simplace handle to the SimplaceWrapper object
#' @return list with the IDs
#' @export
getSimulationIDs <- function(simplace)
{
  rJava::.jcall(simplace, "[S", "getSimulationIDs")
}


#' Fetch output from a simulation
#' 
#' The output is a JavaObject containing the variable names, 
#' data types, units and the values. Output can be converted
#' with \code{\link{resultToList}} or \code{\link{resultToDataframe}}
#' to R objects. Only MEMORY outputs are accessible. For CSV or 
#' database outputs you have to read the data by generic methods.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param outputId id of the output. Only MEMORY outputs are accessible.
#' @param simulationId id of the simulation
#' @return handle to the data container which has to be processed afterwards
#' @export
getResult <- function(simplace, outputId, simulationId = nullString)
{
  rJava::.jcall(simplace,"Lnet/simplace/sim/wrapper/DataContainer;","getResult",outputId, simulationId);
}



############# Convert Java Objects to R #################


#' Converts a list of named parameters to a java string array
#'
#' @param parameterList list of parameter values indexed by parameter name
#' @return a java object of type Object[n][2]
#' @keywords internal
parameterListToStringArray <- function (parameterList) 
{
  if(!is.null(parameterList) && length(parameterList)>0)
  {
    objlist <- vector("list",length(parameterList));    # creates an empty list
    names <- names(parameterList)    # get the keys
    for(i in 1:length(parameterList))
    {
      name <-  rJava::.jnew("java/lang/String",names[[i]])   # key
      if(length(parameterList[[i]])==1) {
        value <- rJava::.jnew("java/lang/String",toString(parameterList[[i]]))  
      } else {
        value <- rJava::.jarray(parameterList[[i]])
      }  
      objlist[[i]] <-rJava::.jarray(c(name, value))   # add array entry = {key, value}
    }
    rJava::.jcast(rJava::.jarray(objlist),"[[Ljava/lang/Object;") # convert list to java array and cast it to Object[][]
  }
  else
  {
    rJava::.jcast(nullObject,"[[Ljava/lang/Object;")
  }
}



#' Converts a list of named parameters to a java string array
#'
#' @param parameterLists list of parameter list (values indexed by parameter name)
#' @return a java object of type Object[k][n][2]
#' @keywords internal
parameterListsToStringArray <- function (parameterLists) 
{
  if(!is.null(parameterLists) && length(parameterLists)>0)
  {
    objlist <- vector("list",length(parameterLists));
    for(i in seq_along(parameterLists))
    {
      objlist[[i]] = parameterListToStringArray(parameterLists[[i]])
    }
    rJava::.jcast(rJava::.jarray(objlist),"[[[Ljava/lang/Object;")
  }
  else
  {
    rJava::.jcast(nullObject,"[[[Ljava/lang/Object;")
  }

}

#' Converts the varmap to a list
#' 
#' Converts the varMap to a list. All elements are converted to appropriate
#' R objects. Arrays are expanded to vectors by default.
#' 
#' @param varmap the varMap returned by \code{\link{stepSimulation}}
#' @param expand if \code{TRUE} expand array objects to vector.
#' @return list with parameter name as key and parameter value as value
#' @export
#' @examples
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution)
#' varmap <- stepSimulation(simplace,count=22)
#' closeProject(simplace)   
#' 
#' varlist <- varmapToList(varmap)
#' 
#' varlist$startdate - 24*3600
#' varlist$LintulBiomass.sWSO}
varmapToList <- function(varmap,expand=TRUE)
{
  headerarray <- rJava::.jcall(varmap,"[S","getHeaderStrings")
  types <- getDatatypesOfResult(varmap)
  units <- getUnitsOfResult(varmap)
  data <- rJava::.jcall(varmap,"[Ljava/lang/Object;","getDataObjects")
  if(expand)
  {
    for(i in (1:length(headerarray)))
    {
      if(types[i]=="NULL")
        data[[i]] <- NA
      else if (types[i]=="DOUBLEARRAY" || types[i]=="INTARRAY")
      {
        data[[i]] <-rJava::.jevalArray(data[[i]],simplify=TRUE)
      }  
      else if (types[i]=="DATE")
      {
        data[[i]] <- as.Date(rJava::.jcall(data[[i]],"S","toString"))   
      }  
      else
        data[[i]] <- rJava::.jsimplify(data[[i]])
      
    }
  }
  names(data) <- headerarray
  attr(data,"units") <- units
  attr(data,"datatypes") <- types
  data
}


#' Convert result to list
#' 
#' Converts all scalar output columns to appropriate R lists. Columns containing
#' arrays are left unchanged, unless expand is TRUE. 
#' 
#' @param result handle to the data container returned by \code{\link{getResult}}
#' @param expand if true columns with arrays are partially expanded
#' @param from start of the result range, if to/from are not set, full result is returned
#' @param to end of the result range, if to/from are not set, full result is returned
#' @return list with output columns
#' @seealso \code{\link{resultToDataframe}} returns the scalar output columns as \code{\link{data.frame}}
#' @export
#' @examples 
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution)
#' parameter <- list()
#' parameter$vTempLimit <- 32
#' createSimulation(simplace,parameter)
#' runSimulations(simplace)
#' simulationlist <-getSimulationIDs(simplace)
#' result <- getResult(simplace,"DIAGRAM_OUT", simulationlist[[1]]);
#' closeProject(simplace)
#' resultlist <- resultToList(result)
#' resullist$CURRENT.DATE}
resultToList <-function(result,expand=FALSE,from=NULL,to=NULL) {
  headerarray <- rJava::.jcall(result,"[S","getHeaderStrings")
  types <- getDatatypesOfResult(result)
  units <- getUnitsOfResult(result)
  if(!is.null(from) && !is.null(to) && to>=from && from >=0)
    data <- rJava::.jcall(result,"[Ljava/lang/Object;","getDataObjects",as.integer(from),as.integer(to))
  else
    data <- rJava::.jcall(result,"[Ljava/lang/Object;","getDataObjects")
  for(i in (1:length(headerarray)))
  {
    if(types[i]=="NULL")
      data[[i]] <- NA
    else if (expand & types[i]=="INTARRAY")
    {
      data[[i]] <-rJava::.jevalArray(data[[i]],rawJNIRefSignature="[Ljava/lang/Integer;")
      data[[i]] <-lapply(data[[i]],function(a)rJava::.jevalArray(a, simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Integer;"))
    }  
    else if (expand & types[i]=="DOUBLEARRAY")
    {
      data[[i]] <-rJava::.jevalArray(data[[i]],rawJNIRefSignature="[Ljava/lang/Double;")
      data[[i]] <-lapply(data[[i]],function(a)rJava::.jevalArray(a, simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Double;"))
    }  
    else if (types[i]=="DATE")
      data[[i]] <- as.Date(rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/String;")) 
    else if(types[i]=="DOUBLE")
      data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Double;")
    else if(types[i]=="INT")
      data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Integer;")
    else if(types[i]=="CHAR")
      data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/String;")
    else if(types[i]=="BOOLEAN")
      data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Boolean;")
  }
  names(data) <- headerarray
  attr(data,"units") <- units
  attr(data,"datatypes") <- types
  
  data
}

transdf <- function(l, n) {
  if(is.list(l))
  {
    d <- as.data.frame(t(as.data.frame(l)))
    names(d) <- gsub("V",paste0(n,"_"),names(d))
  }
  else {
    d <- data.frame(l)
    names(d)<-n
  }
  d
}

#' Convert result to dataframe
#' 
#' All scalar output columns are transformed to appropriate R objects
#' and then glued together in a dataframe. Array outputs columns are ignored.
#' 
#' 
#' @param result handle to the data container returned by \code{\link{getResult}}
#' @param expand if true columns with arrays are partially expanded
#' @param from start of the result range, if to/from are not set, full result is returned
#' @param to end of the result range, if to/from are not set, full result is returned
#' @return data.frame with scalar output columns
#' @seealso \code{\link{resultToList}} returns the output columns as list
#' @export
#' @examples 
#' \dontrun{
#' simplace <- initSimplace(SimplaceInstallationDir,SimplaceWorkDir,SimplaceOutputDir)
#' openProject(simplace, Solution)
#' parameter <- list()
#' parameter$vTempLimit <- 32
#' createSimulation(simplace,parameter)
#' runSimulations(simplace)
#' simulationlist <-getSimulationIDs(simplace)
#' result <- getResult(simplace,"DIAGRAM_OUT", simulationlist[[1]]);
#' closeProject(simplace)
#' resultframe <- resultToDataframe(result)
#' resultframe[3,]}
resultToDataframe <- function(result,expand=FALSE, from=NULL,to=NULL) {
  if(!expand)
  {
    oheaderarray <- rJava::.jcall(result,"[S","getHeaderStrings")
    otypes <- getDatatypesOfResult(result)
    ounits <- getUnitsOfResult(result)
    if(!is.null(from) && !is.null(to) && to>=from && from >=0)
      odata <- rJava::.jcall(result,"[Ljava/lang/Object;","getDataObjects",as.integer(from),as.integer(to))
    else
      odata <- rJava::.jcall(result,"[Ljava/lang/Object;","getDataObjects")
    
    index <- (otypes!="DOUBLEARRAY" & otypes!="INTARRAY" & otypes!="CHARARRAY")
    headerarray <- oheaderarray[index]
    types <- otypes[index]
    units <- ounits[index]
    data <- odata[index]
    for(i in (1:length(headerarray)))
    {
      if(types[i]=="NULL")
        data[[i]] <- NA
      else if (types[i]=="DATE")
        data[[i]] <- as.Date(rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/String;"))
      else if(types[i]=="DOUBLE")
        data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Double;")
      else if(types[i]=="INT")
        data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Integer;")
      else if(types[i]=="CHAR")
        data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/String;")
      else if(types[i]=="BOOLEAN")
        data[[i]] <- rJava::.jevalArray(data[[i]],simplify=TRUE,rawJNIRefSignature="[Ljava/lang/Boolean;")
    }
    names(data)<-headerarray
    data <- do.call(cbind.data.frame,data)
    attr(data,"units") <- units
    attr(data,"datatypes") <- types
    data
  }
  else {
    reslist <- resultToList(result,TRUE,from=from,to=to)
    data<-do.call(cbind,lapply(names(reslist),\(n)transdf(reslist[[n]], n)))
    rownames(data)<-NULL
    attr(data,"units") <- attr(reslist,"units")
    attr(data,"datatypes") <- attr(reslist, "datatypes")
    data
  }
}


#' Get the units of the result variables
#' 
#' Get the units of each variable (i.e. data column) in a 
#' human readable format.
#' The output is a named character vector, where each element is named
#' by the variables name.
#' 
#' @param result handle to the data container returned by \code{\link{getResult}}
#' @return named character vector with the units
#' @export
getUnitsOfResult <- function(result)
{
  units <- rJava::.jcall(result,"[S","getHeaderUnits")
  names(units) <-  rJava::.jcall(result,"[S","getHeaderStrings")
  units
}

#' Get the datatypes of the result variables
#' 
#' Get the datatypes of each variable (i.e. data column).
#' The output is a named character vector, where each element is named
#' by the variables name.
#' 
#' @param result handle to the data container returned by \code{\link{getResult}}
#' @return named character vector with the datatypes
#' @export
getDatatypesOfResult <- function(result)
{
  types <- rJava::.jcall(result,"[S","getTypeStrings")
  names(types) <-  rJava::.jcall(result,"[S","getHeaderStrings")
  types
}

############# Additional functions ################


#' Set working-, output-, projects- and data-directory
#' 
#' One can specify all or only some of the directories. 
#' Only the directories specified will be set. 
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param WorkDir working directory where solutions, projects and data resides (_WORKDIR_)
#' @param OutputDir directory for output (_OUTPUTDIR_)
#' @param ProjectsDir optional directory for project data  (_PROJECTSDIR_)
#' @param DataDir optional directory for data (_DATADIR_)
#' @seealso \code{\link{getSimplaceDirectories}}
#' @export
setSimplaceDirectories <- function(simplace ,WorkDir = nullString, OutputDir = nullString, ProjectsDir = nullString, DataDir = nullString)
{
  rJava::.jcall(simplace,"V","setDirectories", WorkDir, OutputDir, ProjectsDir, DataDir)
}
  
#' Get the directories (work-, output-, projects- and data-dir)
#'
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @return character vector with the directories
#' @seealso \code{\link{setSimplaceDirectories}}
#' @export
getSimplaceDirectories <- function(simplace)
{
  res <- rJava::.jcall(simplace,"[S","getDirectories")
  names(res)<-c('_WORKDIR_', '_OUTPUTDIR_', '_PROJECTSDIR_', '_DATADIR_')
  res
}


#' Sets the lines of the project data files that should be used
#' when running a project.
#' 
#' You have to call the function after \code{\link{initSimplace}}
#' but before \code{\link{openProject}}.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param lines either a vector of integers or a string of numbers separated by commas
#' @export
#' @examples 
#' \dontrun{
#' setProjectLines(simplace, "1,3,6,9-17,33")
#' setProjectLines(simplace, c(1,2,3,9:17,33))}
setProjectLines <- function(simplace, lines)
{
    rJava::.jcall(simplace,"V","setProjectLines",as.character(Reduce(function(x,y) paste(x,y,sep=","),lines)))
  
}

#' Sets the log level of simplace
#' 
#' Sets the level of logger output - FATAL is least verbose,
#' TRACE most verbose. You have to call \code{\link{initSimplace}} first.
#' 
#' @param level is a string with possible values: FATAL, ERROR, WARN, INFO, DEBUG, TRACE
#' @export
#' @examples 
#' \dontrun{
#' setLogLevel("INFO")}
setLogLevel <- function(level)
{
  lg <- rJava::.jnew("net/simplace/core/logging/Logger")
  lgl <- switch(level,
    FATAL = lg$LOGLEVEL$FATAL,
    ERROR = lg$LOGLEVEL$ERROR,
    WARN = lg$LOGLEVEL$WARN,
    INFO = lg$LOGLEVEL$INFO,
    DEBUG = lg$LOGLEVEL$DEBUG,
    TRACE = lg$LOGLEVEL$TRACE,
    lg$LOGLEVEL$INFO
    )
  rJava::.jcall("net/simplace/core/logging/Logger","V","setLogLevel",lgl)
}


#' Sets the check level of simplace
#' 
#' Sets the check level. OFF does no check at all, STRICT the most severe.
#' You have to call \code{\link{initSimplace}} first.
#' 
#' @param simplace handle to the SimplaceWrapper object returned by \code{\link{initSimplace}}
#' @param level is a string with possible values: "CUSTOM,"STRICT","INTENSE","LAZY","OFF","ONLY"
#' @export
#' @examples 
#' \dontrun{
#' setCheckLevel(simplace, "STRICT")}
setCheckLevel <- function(simplace, level)
{
  rJava::.jcall(simplace,"V","setCheckLevel",level)
}

#' Sets number of used CPUs
#' 
#' Sets the number of processors that are used parallel.
#' The function can be used only after \code{\link{initSimplace}} has been
#' called.
#'
#' @param count number of processors
#' @export
setSlotCount <- function(count) {
  rJava::.jcall("net/simplace/sim/FWSimEngine","V","setSlotCount",as.integer(count))
}
