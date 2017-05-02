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
                          unlinkf=F,nitr=5,show.output = F,verbose = T,xmlfile="Soil.xml",
                          VarinfoXML=VarinfoXML)


APSIMOptim::ANOVA.SA(someresult)
