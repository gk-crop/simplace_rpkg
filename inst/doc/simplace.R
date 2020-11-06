## ----eval=FALSE---------------------------------------------------------------
#  install.packages('simplace',
#     repos=c('http://r-forge.r-project.org','http://cran.r-project.org'))

## ----running, results='hide'--------------------------------------------------
library(simplace)
SimplaceInstallationDir <- findFirstSimplaceInstallation()

Solution <- paste(SimplaceInstallationDir,
        "simplace_run/simulation/gk/solution/complete/Complete.sol.xml",sep="")

simplace <- initSimplace(SimplaceInstallationDir)

openProject(simplace, Solution)

parameter <- list()
parameter$enddate <- "31-12-1992"

sid <- createSimulation(simplace,parameter)
runSimulations(simplace)

result <- getResult(simplace,"DIAGRAM_OUT", sid);

closeProject(simplace)

## ----plotting, fig.height=5, fig.width=6--------------------------------------
resf <- resultToDataframe(result)

dates <- 300:730
weights <- resf[dates,
    c("TOP_LINE_Roots","TOP_LINE_Leaves","TOP_LINE_Stems","TOP_LINE_StorageOrgans")]
matplot(dates,weights,type="l",xlab="Days",ylab="Weight [g/m2]",main="Simulated Biomass")
legend(300,800,legend=c("Roots","Leaves","Stems","Storage Organs"),lty=1:4,col=1:4)


## ----contourplot, fig.height=5, fig.width=6-----------------------------------
resultlistexp <- resultToList(result,expand=TRUE)
water <- resultlistexp$BOTTOM_ARRAY_VolumetricWaterContent
wmat <- do.call(rbind,water)
wmatpart <- wmat[dates,]
layers <- dim(wmatpart)[2]
filled.contour(dates,-(layers:1),wmatpart[,layers:1],
               xlab="Day", ylab="Layer", main="Water content in soil",
               color.palette = function(n){rgb((n:1)/n,(n:1)/n,1)})


