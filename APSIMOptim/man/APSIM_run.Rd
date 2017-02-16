\name{apsimRun}
\alias{apsimRun}
\title{Edit and run APSIM simulations}
\usage{
apsimRun(apsimWd,apsimExe,apsimFile,apsimVarL,VarT,tag,unlinkf=F,Varvalues)
}
\arguments{
  \item{apsimWd}{APSIM working directory}
  \item{apsimExe}{APSIM exe directory/ where APSIM is installed.}
  \item{apsimFile}{A character of the name of the APSIM simulation i.e. (BioMaize.apsim)}
  \item{apsimVarL}{List of the variebles of interets with their position in vector}
  \item{VarT}{Vecotr of the type of the variebles of interest ("Element" or "Single")}
  \item{tag}{Tag which will be aded to the end of edited file name}
  \item{unlinkf}{If the new generated/edited APSIM files need to be deleted.}
  \item{Varvalues}{Vector of values corrosponding to the variables of interest. }

}
\value{
The out put is a dataframe with the result of simulation.
}
\description{
This function tries to edit APSIM (.apsim) simulation for the variables of interest and run the simulations.
}
\examples{
apsimWd <- paste0(getwd())

apsimExe <- "C:/Program Files (x86)/Apsim77-r3632/Model/Apsim.exe"

apsimFile<- "BioMaize.apsim"

apsimVarL <- list("Soil/Water/DUL"=2,"Soil/Water/SAT"=1,
                  "folder/manager2/ui/biochar_loss"=1); ## variables of intterest

VarT<-c("Element","Element","Single")

Result.sim<-APSIM.RUN(apsimWd, apsimExe, apsimFile, apsimVarL,VarT, tag="",
          unlinkf=F, Varvalues=c(0.4,0.1,0.1))

}
