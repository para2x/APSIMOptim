################## Ready for the run and estimating the cost
APSIM.Optim<-function(apsimWd,apsimExe,apsimFile,apsimVarL,Varinfo,tag,unlinkf=F,nitr=10,obs=NULL,Gibbs=T,oldCost=1e6,show.output = TRUE){
  nvar<-length(apsimVarL)
  news <- matrix(NA, 1, nvar)
  tagc<-tag

  apsimVar<-names(apsimVarL) # finding the name of variables
  elp <- unlist(apsimVarL)   # finding the element which is the value in the list
  varnames<-unlist(lapply(apsimVar,function(x){tail(strsplit(x,split="/")[[1]],1)}))  # fidning the variables names
  varnames<-paste0(varnames,elp) # making the name to lookup in the dataframe

  ###################################################
  ## simulation variables
  ###################################################
  sws <- matrix(NA, length(obs[,2]), nitr)
  olds<-Varinfo[which(varnames%in%Varinfo$Variable),2] ## finding starting values
  out <- matrix(NA, nitr, nvar + 1) # output which contains all the parameters posterior
  Pmeans<-Varinfo[which(varnames%in%Varinfo$Variable),3] ## finding starting values
  Psds<-Varinfo[which(varnames%in%Varinfo$Variable),4] ## finding starting values
  stepsU<-Varinfo[which(varnames%in%Varinfo$Variable),5]
  VarT<-Varinfo[which(varnames%in%Varinfo$Variable),6]
  stepsL<-rep(0,length(varnames))
  boundU<-Varinfo[which(varnames%in%Varinfo$Variable),7]
  boundL<-Varinfo[which(varnames%in%Varinfo$Variable),8]

  if(nvar!=length(olds)) stop("At least one of the parameters is not in the Variableinfo dataframe.")
  ## finding the starting and end date based on swc being fed
  Start.date <-obs[1,1]
  End.date <-  obs[nrow(obs),1]
  j<-1
  #################### Stating the loop
  for (i in 1:nitr) {

    ###############################################################################
    ###### Doing the random sampling
    ###############################################################################
    turn <- sample(1:nvar, replace = TRUE)[1] ### Who's trun is that ? TAKING TURN FOR CHANGING THE PARAMETERS
    repeat {
      ##### Does the gibs sampling
      if (Gibbs) {
        news <- olds
        ## give a random walk to the one whos is has turn
        news[turn] <- olds[turn] + rnorm(1, stepsL[turn], stepsU[turn])
      } else {
        ###### MVN sampling
        news <- t(as.matrix(mvrnorm(n = 1, (stepsL + olds), vc)))
      }

      if(news[turn]<boundU[turn] & news[turn]>boundL[turn]){break;}

    }
    ###############################################################################
    ###### Running model
    ###############################################################################
    # cat(turn,"--",varnames,"--",Pmeans,"--",boundU,"\n")
    apsimValue <- as.list((news))
    VarT<-Varinfo[which(varnames%in%Varinfo$Variable),6]
    Fname<-paste0(unlist(strsplit(apsimFile, "[.]"))[1],"_edited",tagc,".apsim") #edited file
    ## Edit file
    simname<-(edit_apsim(file = apsimFile, wd = apsimWd, var = apsimVar, tag=paste0("_edited",tagc),
                         value = apsimValue, varType = VarT, Elpos = elp, overwrite = FALSE))

    outFname<-paste0(unclass(simname),".out") #outfile
    sumFname<-paste0(unclass(simname),".sum") #sumfile
    # Find the dited file location
    system(paste(addCommas(apsimExe),Fname, sep = " "), show.output.on.console = show.output)
    # run the edited file
    results <- (read_out(paste0(getwd(),"/",outFname)))%>%filter(as.Date(V1) >= Start.date & as.Date(V1) <= End.date)
    ## joining the simulation results with observed
    n1<-names(results)[1]
    n2<-names(obs)[1]
    attr(n2,"names")<-n1
    results<-results%>%right_join(obs,by=n2)
    ### estimating the cost for liklihood
    cost_c <- 0.5 * (sum((results[,2] - results[,3]) ^ 2)) / var(results[,2])
    ###############################################################################
    ################### Calculating the prior information piece
    ###############################################################################
    pratio<-1
    if(!Gibbs){
      for (i in 1:nvar){
        pratio <- pratio * ((dnorm(news[i], Pmeans[i], Psds[i])) / (dnorm(olds[i], Pmeans[i], Psds[i])))
      }
    }else{
      pratio <- pratio * ((dnorm(news[turn], Pmeans[turn], Psds[turn])) / (dnorm(olds[turn], Pmeans[turn], Psds[turn])))

    }
    ###############################################################################
    ################################
    ############ Making desicion about the propsed sample
    ################################
   # cat(turn,"---",news[turn],"--",Pmeans[turn],"--", Psds[turn],"--",olds[turn],"--",pratio)
    if (log(runif(1, 0, 1)) < ((oldCost- cost_c) + (log(pratio)))) { #log form
      olds <- news#updating
      oldCost <- cost_c	#cost
      out[j,]<-c(news, cost_c)
      sws[,j] <- results[,2]
      j<-j+1
    }
    ###############################################################################
    ################################
    ############ Killing the job ruuner of APSIM
    ###############################################################################
    job <- grep("^JobRunner", readLines(textConnection(system('tasklist', intern = TRUE))), value = TRUE)
    PIDS<-(read.table(text = job))
    if (nrow(PIDS)>20) {
      pskill(head(PIDS$V2,nrow(PIDS)-10))
    }

  }# end e loop
  if(unlinkf){
    unlink(c(Fname,outFname,sumFname), recursive = FALSE)
  }
  #building output
    output<-list("Param" = out, "Variable" = sws, "Pm" = Pmeans, "Psd" = Psds, "Proposal" = stepsU,
                  "var"=varnames,"Obs.swc"=obs,"Itration"=i,"APfile"=apsimFile,"Gsampling"=Gibbs)
    class(output)<-"APSIMOptim"
  return(output)
}
