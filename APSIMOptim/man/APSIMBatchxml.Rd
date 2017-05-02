\name{apsimBatchXML}
\alias{apsimBatchXML}
\title{Edit and run APSIM simulations batches with ability to modify xml files as well.}
\usage{
apsimBatchXML(apsimWd, apsimExe, apsimFile, apsimVarL, Varinfo, tag="",
                          unlinkf=F,nitr=2,show.output = F,verbose = T,xmlfile=NULL,
                          VarinfoXML=VarinfoXML)
}
\arguments{
  \item{apsimWd}{APSIM working directory}
  \item{apsimExe}{APSIM exe directory/ where APSIM is installed.}
  \item{apsimFile}{A character of the name of the APSIM simulation i.e. (BioMaize.apsim)}
  \item{apsimVarL}{List of the variebles of interets with their position in vector}

  \item{tag}{Tag which will be aded to the end of edited file name}
  \item{unlinkf}{If the new generated/edited APSIM files need to be deleted.}


}
\value{
The out put is a dataframe with the result of simulation.
}
\description{
This function tries to edit APSIM (.apsim) simulation for the variables of interest and run the simulations.
}
\examples{
library(APSIMOptim)
###########################
apsimWd <- paste0(getwd())
apsimExe <- "C:/Program Files (x86)/Apsim77-r3632/Model/Apsim.exe"
apsimFile<- "BioMaize.apsim"
###########################################
################# APSIM PARAMS
##############################################
apsimVarL <- list("//SoilWater/SummerCona"=1,
                  "//SoilWater/SummerU"=1); ## parametres of intterest

Varinfo<-data.frame(Variable=c("SummerCona1","SummerU1"),
                    Luni=c(10,0.005),
                    Uuni=c(500,2),
                    Type=rep("Single",2),
                    BoundU=c(500,2),
                    BoundL=c(10,0),
                    Functions=rep("Uniform",2),stringsAsFactors = F)
##########################################
#### SOIL.XML params
##########################################
VarinfoXML<-data.frame(Variable=c("nitrification_pot","wfmin_index"),
                    elp=c(1,3),
                    Luni=c(0,0),
                    Uuni=c(1,3),
                    BoundU=c(500,4),
                    BoundL=c(0,0),
                    Functions=rep("Uniform",2),stringsAsFactors = F)

someresult<-apsimBatchXML(apsimWd, apsimExe, apsimFile, apsimVarL, Varinfo, tag="",
                          unlinkf=F,nitr=2,show.output = F,verbose = T,xmlfile=NULL,
                          VarinfoXML=VarinfoXML)

}
