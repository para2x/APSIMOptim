################## Ready for the run and estimating the cost
apsimOptim<-function(apsimWd, apsimExe, apsimFile, apsimVarL, Varinfo, tag, unlinkf=F, nitr=10,
                      obs=NULL, Gibbs=T, vc=NULL, show.output = TRUE,verbose=T,Simname="Simulation",usePrior=T){
  options(warn=-1) #supress warnings resulting from cbind and join
  nvar<-length(apsimVarL) # number of variables
  news <- matrix(NA, 1, nvar) # this vector keeps the values for the new set of poposed samples
  tagc<-tag # it will be added to the edited apsim file
  apsimVar<-names(apsimVarL) # finding the name of variables
  elp <- unlist(apsimVarL)   # finding the element which is the value in the list
  varnames<-unlist(lapply(apsimVar,function(x){tail(strsplit(x,split="/")[[1]],1)}))  # fidning the variables names
  varnames<-paste0(varnames,elp) # making the name to lookup in the dataframe
  ###################################################
  ## simulation variables
  ###################################################
  oldCost=1e6 ## starting cost for the psudo liklihood
  sws <- matrix(NA, nrow(obs), nitr) ## this matrix keeps the results of simulations from accepted samples
  olds<-Varinfo[which(varnames%in%Varinfo$Variable),2] ## finding starting values
  out <- matrix(NA, nitr, nvar + 1) # output which contains all the parameters posterior
  Pmeans<-Varinfo[which(varnames%in%Varinfo$Variable),3] ## finding starting values
  Psds<-Varinfo[which(varnames%in%Varinfo$Variable),4] ## finding starting values
  stepsU<-Varinfo[which(varnames%in%Varinfo$Variable),5] # proposal - step size
  VarT<-Varinfo[which(varnames%in%Varinfo$Variable),6] # variable type - from dataframe
  stepsL<-rep(0,length(varnames)) # it's not being used for now.
  boundU<-Varinfo[which(varnames%in%Varinfo$Variable),7] # upper bound for check the new sample
  boundL<-Varinfo[which(varnames%in%Varinfo$Variable),8]# lower bound for check the new sample
  Funcs<-Varinfo[which(varnames%in%Varinfo$Variable),9] # functions for  sample generation
  # check to see if we have information for all variables
  if(nvar!=length(olds)) stop("At least one of the parameters is not in the Variableinfo dataframe.")
  if(!Gibbs & is.null(vc)) stop("If gibbs is false, you need to provide the variance covarinace matrix.")
  ## finding the starting and end date for filtering simulation results
  Start.date <-obs[1,1]
  End.date <-  obs[nrow(obs),1]
  j<-1 # this counts the number of accepted samples and put it on matrix
  tsimstart<-proc.time()
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
       if(Funcs[turn]=="Normal"){
        news[turn] <- olds[turn] + rnorm(1, stepsL[turn], stepsU[turn])
       }else if(Funcs[turn]=="Uniform"){
         news[turn] <- olds[turn] + runif(1, stepsL[turn], stepsU[turn])
       }
        # check to see if the new sample is in specified range.
        if(news[turn]<boundU[turn] & news[turn]>boundL[turn]){break;}
      } else {
        ###### MVN sampling
        news <- t(as.matrix(mvrnorm(n = 1, (stepsL + olds), vc)))
        # check to see if all the new sample is in specified range.
        check<-T # variable for aggreagting the result of check
        for(p in 1:nvar){
          if(news[p]>boundU[p] & news[p]<boundL[p]){check<-F}
        }# end e for check
        if(check)break; # break if all the variables passed the check

      }
    }
    ###############################################################################
    ###### Running model
    ###############################################################################
    apsimValue <- as.list((news)) ##Takes the values form new generated samples
    Fname<-paste0(unlist(strsplit(apsimFile, "[.]"))[1],"_edited",tagc,".apsim") #edited file
    ## Edit file
    simname<-(edit_apsim(file = apsimFile, wd = apsimWd, var = apsimVar, tag=paste0("_edited",tagc),
                         value = apsimValue, varType = VarT, Elpos = elp, overwrite = FALSE))
 # cat("----------",simname,"\n")
    outFname<-paste0(unclass(simname),".out") #outfile
    sumFname<-paste0(unclass(simname),".sum") #sumfile

    # Find the dited file location
    system(paste(addCommas(apsimExe),Fname, sep = " "), show.output.on.console = show.output)
    # run the edited file
    results <- (read_out(paste0(getwd(),"/",outFname)))
    ## joining the simulation results with observed
    names(results)[1]<-names(obs)[1]
    n1<-names(results)[1]
    n2<-names(obs)[1]
    attr(n2,"names")<-n1



    results<-results%>%right_join(obs,by=n2)
    results[,2]<-as.numeric(results[,2])
  #  print(results)
    #cat(dim(sws),"--",dim(results),"--",dim(obs),"\n")
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
    if(is.nan(pratio)) {
      warning("Combination of prior mean and sd results in 0 probability. It needs to be refined.")
      next}
    ###############################################################################
    ################################
    ############ Making desicion about the propsed sample
    ################################
   if(usePrior){
     Desi<-log(runif(1, 0, 1)) < ((oldCost- cost_c) + (log(pratio)))
   }else{
     Desi<-log(runif(1, 0.8, 1)) < (oldCost- cost_c)
   }
  if(verbose)  {
    cat(i,"/",nitr,"-> Variable:",varnames[turn],"\n",
          "      -New sample:",news[turn],"\n",
          "      -Old sample:",olds[turn],"\n",
          "      -Prior mean:",Pmeans[turn],"\n",
          "      -Prior sd:",Psds[turn],"\n",
          "      -Prior ratio:",pratio,"\n",
          "      -Current cost:",cost_c,"\n",
          "      -Old cost:",oldCost,"\n",
          "      -Accetpted:",Desi,"\n")
  }
    if (Desi) { #log form
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
    job <- grep("^JobRunner", readLines(textConnection(system('tasklist', intern = TRUE))), value = TRUE) # finding the pid of the job runners
      if(length(job)>0){ # if there is job runner running
        PIDS<-(read.table(text = job))
      if (nrow(PIDS)>20) {
        pskill(head(PIDS$V2,nrow(PIDS)-10))
      }
    }
    if(unlinkf){
      unlink(c(Fname,outFname,sumFname), recursive = FALSE)
    }
  }# end e loop

  #building output
    output<-list("Param" = out[which(complete.cases(out)),], "Simulations" = sws[, colSums(is.na(sws)) != nrow(sws)]
, "Variable Info" = Varinfo,"var"=varnames,"Obs"=obs,"Itration"=i,"APfile"=apsimFile,"Gsampling"=Gibbs,"Simulation name"=Simname,
"Elapsed time"=proc.time()-tsimstart)

    class(output)<-"apsimOptim"
  return(output)
}
