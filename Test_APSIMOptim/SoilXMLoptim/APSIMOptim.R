library(APSIMOptim)
#setwd("C:/Users/Para2x/Dropbox/Hamze Dokoohaki/Journal papers/Biochar module evaluation/R/APSIMOptimi_Package/Test_APSIMOptim")
############
obsdf<-read.csv("observed.csv",stringsAsFactors = F)
obsdf$Date<-as.Date(unclass(obsdf$Date),format = "%d/%m/%Y")
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


