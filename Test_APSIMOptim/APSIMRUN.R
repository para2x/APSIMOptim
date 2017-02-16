library(APSIMOptim)
setwd("C:/Users/Para2x/Dropbox/Hamze Dokoohaki/Journal papers/Biochar module evaluation/R/APSIMOptimi_Package/Test_APSIMOptim")
############
apsimWd <- paste0(getwd())

apsimExe <- "C:/Program Files (x86)/Apsim77-r3632/Model/Apsim.exe"

apsimFile<- "BioMaize.apsim"

apsimVarL <- list("Soil/Water/DUL"=2,"Soil/Water/SAT"=1,
                  "folder/manager2/ui/biochar_loss"=1); ## variables of intterest

VarT<-c("Element","Element","Single")

Result.sim<-apsimRun(apsimWd, apsimExe, apsimFile, apsimVarL,VarT, tag="",
                      unlinkf=F, Varvalues=c(0.4,0.1,0.1))
