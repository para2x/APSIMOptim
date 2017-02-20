library(APSIMOptim)
setwd("C:/Users/Para2x/Dropbox/Hamze Dokoohaki/Journal papers/Biochar module evaluation/R/APSIMOptimi_Package/Test_APSIMOptim")
############
obsdf<-read.csv("observed.csv",stringsAsFactors = F)
obsdf$Date<-as.Date(unclass(obsdf$Date),format = "%d/%m/%Y")

apsimWd <- paste0(getwd())

apsimExe <- "C:/Program Files (x86)/Apsim77-r3632/Model/Apsim.exe"

apsimFile<- "BioMaize.apsim"

apsimVarL <- list("Soil/Water/DUL"=2,"Soil/Water/SAT"=3,"folder/manager2/ui/biochar_loss"=1); ## variables of intterest

Varinfo<-data.frame(Variable=c("DUL2","SAT3","biochar_loss1"),
                    StartingV=c(0.3,0.45,0.02),
                    PriorM=c(0.35,0.4,0.02),
                    PriorSD=c(0.02,0.02,0.01),
                    Step=c(0.05,0.05,0.05),
                    Type=c("Element","Element","Single"),
                    BoundU=c(0.4,0.6,0.1),
                    BoundL=c(0.1,0.25,0.0001),
                    Functions=c("Normal","Normal","Uniform"))



Result.sim<-apsimOptim(apsimWd, apsimExe, apsimFile, apsimVarL, Varinfo, tag="",
                        unlinkf=T,nitr=10,obs=obsdf,Gibbs=T,show.output = F,verbos = T)


plot(Result.sim,type="Posterior")

plot(Result.sim,type="Cost")

plot(Result.sim,type="Simulations",burnin = 0)


