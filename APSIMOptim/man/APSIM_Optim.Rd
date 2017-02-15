\name{APSIM.Optim}
\alias{APSIM.Optim}
\title{Tries to optimize APSIM input parameter unsing Metrapolis-Hasting algorithm}
\usage{
APSIM.Optim(apsimWd,apsimExe,apsimFile,apsimVarL,Varinfo,tag,unlinkf=F,nitr=10,obs=NULL,Gibbs=T,oldCost=1e6,show.output = TRUE)
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

}
\value{
The out put is a dataframe with the result of simulation.
First column of the observed should be Date and second column should be the variable of interset just like the sim

}
\description{
This function tries to edit APSIM (.apsim) simulation for the variables of interest and run the simulations.
}
\examples{
obsdf<-read.csv("observed.csv",stringsAsFactors = F)
obsdf$Date<-as.Date(unclass(obsdf$Date),format = "\%d/\%m/\%Y")

apsimWd <- paste0(getwd())

apsimExe <- "C:/Program Files (x86)/Apsim77-r3632/Model/Apsim.exe"

apsimFile<- "BioMaize.apsim"

apsimVarL <- list("Soil/Water/DUL"=2,"Soil/Water/SAT"=3,"folder/manager2/ui/biochar_loss"=1); ## variables of intterest

Varinfo<-data.frame(Variable=c("DUL2","SAT3","biochar_loss1"),
                    StartingV=c(0.3,0.45,0.02),
                    PriorM=c(0.35,0.4,0.02),
                    PriorSD=c(0.05,0.001,0.0001),
                    Step=c(0.005,0.001,0.0001),
                    Type=c("Element","Element","Single"),
                    BoundU=c(0.4,0.6,0.1),
                    BoundL=c(0.1,0.25,0.0001))



Result.sim<-APSIM.Optim(apsimWd, apsimExe, apsimFile, apsimVarL, Varinfo, tag="",
          unlinkf=F,nitr=2,obs=obsdf,Gibbs=F)

}
