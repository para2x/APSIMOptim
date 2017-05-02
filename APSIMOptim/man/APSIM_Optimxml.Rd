\name{apsimOptimxml}
\alias{apsimOptimxml}
\title{Tries to optimize APSIM input parameter unsing Metrapolis-Hasting algorithm}
\usage{
  apsimOptimxml(apsimWd, apsimExe, apsimFile, apsimVarL, Varinfo, tag, unlinkf=F,
  nitr=10, obs=NULL, Gibbs=T, vc=NULL, show.output = TRUE,verbose=T,
  Simname ="Simulation",usePrior=T, xmlfile=NULL, VarinfoXML=NULL)
}
\arguments{
  \item{apsimWd}{APSIM working directory}
  \item{apsimExe}{APSIM exe directory/ where APSIM is installed.}
  \item{apsimFile}{A character of the name of the APSIM simulation i.e. (BioMaize.apsim)}
  \item{apsimVarL}{List of the variebles of interets with their position in vector}
  \item{Varinfo}{Vecotr of the type of the variebles of interest ("Element" or "Single")}
  \item{tag}{Tag which will be aded to the end of edited file name}
  \item{unlinkf}{If the new generated/edited APSIM files need to be deleted.}
  \item{nitr}{MCMC iteration. }
\item{obs}{A dataframe of observed data. First column date in the format of Date and second column the data. }
\item{Gibbs}{If you want the Gibbs sampling. }
\item{oldCost}{Stating value of the cost (psudo liklihood). }
\item{show.output}{If you want to see the output of APSIM runs. }
\item{verbose}{Shows the detail of the current step.}
}
\value{
The out put is a dataframe with the result of simulation.
First column of the observed should be Date and second column should be the variable of interset just like the sim

}
\description{
This function tries to edit APSIM (.apsim) simulation for the variables of interest and run the simulations.
}
\examples{
library(APSIMOptim)
############
obsdf<-read.csv("observed.csv",stringsAsFactors = F)
obsdf$Date<-as.Date(unclass(obsdf$Date),format = "\%d/\%m/\%Y")
###########################
apsimWd <- paste0(getwd())
apsimExe <- "C:/Program Files (x86)/Apsim77-r3632/Model/Apsim.exe"
apsimFile<- "BioMaize.apsim"
##############################################
apsimVarL <- list("//SoilWater/SummerCona"=1,
                  "//SoilWater/SummerU"=1); ## parametres of intterest

Varinfo<-data.frame(Variable=c("SummerCona1","SummerU1"),
                    StartingV=c(10,5),
                    MeanP=c(10,5),
                    sdP=c(5,2),
                    Step=c(0.05,5),
                    Type=rep("Single",2),
                    BoundU=c(500,200),
                    BoundL=c(0,0),
                    Functions=rep("Normal",2),stringsAsFactors = F)


##########################################
#### SOIL.XML params
##########################################
VarinfoXML<-data.frame(Variable=c("nitrification_pot","wfmin_index"),
                       elp=c(1,3),
                       StartingV=c(0.05,1),
                       MeanP=c(1,1),
                       sdP=c(1,3),
                       Step=c(0.05,5),
                       BoundU=c(500,400),
                       BoundL=c(0,0),
                       Functions=rep("Normal",2),stringsAsFactors = F)


Result.sim<-apsimOptimxml(apsimWd, apsimExe, apsimFile, apsimVarL, Varinfo, tag="",
unlinkf=T,nitr=10,obs=obsdf,Gibbs=T,show.output = F,verbos = T,xmlfile="Soil.xml",
VarinfoXML=VarinfoXML)


plot(Result.sim,type="Simulations")
}
