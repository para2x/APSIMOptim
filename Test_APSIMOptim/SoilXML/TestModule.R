library(APSIMOptim)
rm(list=ls())
#setwd("C:/Users/Para2x/Dropbox/Hamze Dokoohaki/Journal papers/Biochar module evaluation/R/APSIMOptimi_Package/Test_APSIMOptim")
############
apsimWd <- paste0(getwd())
apsimExe <- "C:/Program Files (x86)/Apsim77-r3632/Model/Apsim.exe"
apsimFile<- "BioMaize.apsim"
######## APSIM params
apsimVarL <- list("//Water/DUL"=2,"//Water/SAT"=1); ## variables of intterest
VarT<-c("Element","Element")
varval<-c(0.4,0.5)
#### SOIL.XML params
simVar <- c("nitrification_pot","wfnit_values")
xmlvarval<- list(abs(rnorm(1)), c(0,2,2,1))




apsimRun(apsimWd=apsimWd, apsimExe=apsimExe, apsimFile=apsimFile, apsimVarL,VarT, tag="",
                     unlinkf=F, Varvalues=varval,xmlfile = NULL,xmlvarL=NULL,
                     xmlvarValue=NULL,xmlelp=c(1,1))
