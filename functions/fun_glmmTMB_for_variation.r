test=F



## renvoie la categorie EBCC de la tendance en fonction
## trend l'estimateur de la tendance
## pVal la p value
## ICinf ICsup l intervalle de confiance a 95 pourcent
affectCatEBCC <- function(trend,pVal,ICinf,ICsup){

    catEBCC <- ifelse(pVal>0.05,
               ifelse(ICinf < 0.95 | ICsup > 1.05,"Incertain","Stable"),
               ifelse(trend<1,
               ifelse(ICsup<0.95,"Fort déclin","Déclin modéré"),
               ifelse(ICinf>1.05,"Forte augmentation","Augmentation modée")))
    return(catEBCC)
}




## Calcul du VIF (adapté à glmmTMB, sinon il faut adapter v et nam)
## adapted from rms::vif
vif.mer <- function (fit) {
    ## Variance-Covariance Matrix
    v <- vcov(fit)$cond
    nam <- names(fixef(fit)$cond)

    ## exclude intercepts
    ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
    if (ns > 0) {
        v <- v[-(1:ns), -(1:ns), drop = FALSE]
        nam <- nam[-(1:ns)]
    }

    ## squarre root of the diagonal matrix (of the variance-covariance matrix)
                                        # !! doesn't work if diag(v) product negative values !!

    d <- diag(v)^0.5
    if(any(is.na(d))) {
        cat("! the diagonal matrice of the variance covariance matrix produce at least one negative value !\n")
        cat("  -> the VIFs values are not assessed\n\n")
        flush.console()
    }

    ## variance-covariance matrix on outer product of d
    d <- v/(d %o% d)
    ## inversing d
    d <- solve(d)
    ## and return the diag of d
    v <- diag(d)
    names(v) <- nam
    v
}




                                        #adapted from Charlotte Roemer's script

#' a relatively generic function to fit multiple regression with glmmTMB, scaling numeric variables, supporting interactions and conversion of numeric variables to factors
#' @param data data at data.frame or data.table format at least a column obs for the observed variable
#' @param varInterest the name of the variable to be regressed (a character value)
#' @param listEffects the names of the explanatory variables (a character vector). Polynomial effects should be written as "poly(x,n)"
#' @param interactions a list of numeric vectors giving the position of the explanatory variables composing interactions (TO DO : adapt script to give variable names instead), default to NA (= no interactions)
#' @param formulaRandom the random part of the formula starting by "+" (default to ="+1" = no random effects)
#' @param selSample numeric, to downsample data (for testing)
#' @param tagModel a character tag identifying model outputs
#' @param family distribution family of varInterest (default to "nbinom2", probably the better choice for abundance data)
#' @param asfactor a character vector giving the numeric variables to be treated as factor in the modelling, default to NA
#' @param data by default the function load the file from dataFile, it's also possible to declare the data.frame
#' @param repout le path for the output
#' @param checkRepout TRUE to verifiy the presence of repout and create it (and some sub-repertories) if no exist
#' @param saveFig TRUE to save the figures
#' @param output to get a multiple object from the function, by default FALSE
#' @param doBeep TRUE to do a beep after model fitting
#' @param printFormula TRUE to print the formula
#' @return write 5 files: (1) a .glm to save model fit in R format; (2) a "XXX_coefs.csv" table giving estimates and vif coefficients of the model; (3) a "XXX.log" to keep track of the formula of the model; (4) a "XXX_Res.csv" a table giving the residuals value; (5) a "forBackTransform_XXX.csv" table giving the mean and standard deviation value of numeric explanatory variables, to allow back transformation to real values when predicting
#' @example see at the end of this code
#' @author Yves Bas & Romain Lorrilliere
#'
glmmTMB_sp <- function(data,varInterest="obs",listEffects="year",interactions=NA
                      ,formulaRandom="+(1|carre)",selSample=1e10,tagModel=""
                      ,family="nbinom2",ziformula = "~1",weights =NULL ,asfactor=NULL
                      ,repout=NULL,checkRepout=TRUE,saveFig=FALSE,output=FALSE,doBeep=TRUE,printFormula=TRUE,printSummary=TRUE)
{


    obs_var=varInterest
    VarSimple=listEffects
    ##Interactions=list(c(6,7,8),c(6,7,9),c(6,8,9),c(6,4))
    Interactions=interactions
    FormulaRandom=formulaRandom


    if(!is.null(asfactor)) {
        obs_var <- paste0(obs_var,ifelse(obs_var %in% asfactor,"_as_factor",""))
        VarSimple <- paste0(VarSimple,ifelse(VarSimple %in% asfactor,"_as_factor",""))
        if(!is.na(Interactions))
            for(l in 1:length(Interactions))
                Interactions[[l]] <- paste0(Interactions[[l]],ifelse(Interactions[[l]] %in% asfactor,"_as_factor",""))
    }

    SelSample=selSample #for testing computing time
    TagModel=tagModel
    ## Famille
    familyMod=family
    FormulaY=paste0(obs_var,"~1")

    FormulaXList=VarSimple

    ##pour afficher les milisecondes
    op <- options(digits.secs=3)



    ## preparaton des repertoires de sortie

    if(is.null(repout)) repout <- "output/"
    repoutSummary <- paste0(repout,"summaries/")
    repoutLogs <- paste0(repout,"logs/")
    repoutBackTransform <- paste0(repout,"forBackTransform/")
    repoutModel <- paste0(repout,"model/")
    repoutResDiag <- paste0(repout,"residual_diagnostic/")
    if(saveFig)repoutFig <- paste0(repout,"figData/")

    if(checkRepout) {
        dir.create("output/",showWarnings=FALSE)
        dir.create(repout,showWarnings=FALSE)
        dir.create(repoutSummary,showWarnings=FALSE)
        dir.create(repoutLogs,showWarnings=FALSE)
        dir.create(repoutBackTransform,showWarnings=FALSE)
        dir.create(repoutModel,showWarnings=FALSE)
        dir.create(repoutResDiag,showWarnings=FALSE)
        if(saveFig) dir.create(repoutFig,showWarnings=FALSE)
    }


    FormulaFix_TtSp=FormulaY
    for (i in 1:length(FormulaXList))
    {
        FormulaFix_TtSp=paste(FormulaFix_TtSp,FormulaXList[i],sep="+")
    }
    if(!is.na(Interactions))
    {
        for (i in 1:length(Interactions))
        {
            ##    Intemp=paste(FormulaXList[Interactions[[i]][1]]
            ##                ,FormulaXList[Interactions[[i]][2]],sep="*")
            Intemp=paste(FormulaXList[Interactions[[i]]],collapse="*")

            FormulaFix_TtSp=paste(FormulaFix_TtSp,Intemp,sep="+")
        }
    }



    if(!is.null(asfactor))
    {
        setDF(data)
        for (i in 1:length(asfactor))
        {
            test <- match(asfactor[i],names(data))
            newcolname <- paste0(colnames(data)[test],"_as_factor")
            data[,newcolname] <-  as.factor(data[,test])
        }
        setDT(data)

    }




    ##compute summaries

    setDT(data)
    dataObs_var <- data[,..obs_var]
    dataObs_var[,occ := ifelse(obs>0,1,0)]

    datawoNA <- data[!is.na(obs),]
    datawoNA_obsvar <- datawoNA[,..obs_var]
    datawoNA_obsvar[,occ := ifelse(obs>0,1,0)]


    Occ <- datawoNA_obsvar[obs>0,]
    nbOcc <- nrow(Occ)




   ## SpA1=aggregate(ObswoNA,by=list(datawoNA$espece),FUN=mean)

    ##  if(length(unique(data$espece))>1) {
    if(saveFig) {
        gg <- ggplot(data=datawoNA_obsvar,aes(x=obs)) + geom_histogram()
        gg <- gg + labs(title = tagModel)
        ggsave(paste0(repoutFig,"distribution_",tagModel,".png"),gg,height=7,width=7)
    }

    ## SpPos=subset(datawoNA,ObswoNA>0)


  ##  ObsPos=subset(Obs,Obs>0)

    cat("\n Number of occurence:",nbOcc,"\n")
    flush.console()
    if(nbOcc<=length(VarSimple))
    {
        cat(paste("too few positive data to fit model"),"\n")
	flush.console()
    }else
    {
  ##      if(length(unique(data$espece))>1) {
  ##          SpOcc=aggregate(ObsPos,by=list(SpPos$espece),FUN=length)
  ##          barplot(SpOcc$x,names.arg=SpOcc$Group.1,las=2,cex.names=0.6)
  ##
  ##          SpObsIfP=aggregate(ObsPos,by=list(SpPos$espece),FUN=mean)
  ##          barplot(SpObsIfP$x,names.arg=SpObsIfP$Group.1,las=2,cex.names=0.6)
  ##      }


        ## Pour correction autocorrelation spatiale
        ##MaDataActiNew$FauxGroupe=rep(1,nrow(MaDataActiNew))
        ##MaDataActiNew$Coord=numFactor(MaDataActiNew$X,MaDataActiNew$Y)


        data_SLPAGN=as.data.frame(datawoNA)

        OtherVariables=subset(names(data),!(names(data) %in% VarSimple))

        data_Scale=subset(data_SLPAGN,select=OtherVariables)


        Mean=vector()
        Sdev=vector()
        VarList=vector()

        cat("Variable transformation to reduced centered variable\n")
        cat("   ",length(VarSimple),"variable(s) to transform")
        start <- Sys.time() ## heure de demarage
        cat("  ",format(start, "%d-%m-%Y %HH%M"),"  ... \n ")
	flush.console()

        for (i in 1:length(VarSimple))
        {
            cat(i,"")
            flush.console()
            if(substr(VarSimple[i],1,5)=="poly(")
            {
                Var=gsub("poly","",VarSimple[i])
                Terms=tstrsplit(Var,split=",")
                VarTemp=substr(Terms[[1]],2,nchar(Terms[[1]]))
            }else{
                VarTemp=VarSimple[i]
            }
            VarList=c(VarList,VarTemp)
            Vinit=(data_SLPAGN)[,VarTemp]
            if(is.numeric(Vinit))
            {

                Vscale=scale(Vinit)
                Mean=c(Mean,mean(Vinit))
                Sdev=c(Sdev,sd(Vinit))
            }else{
                Vscale=Vinit
                Mean=c(Mean,NA)
                Sdev=c(Sdev,NA)
            }
            data_Scale=cbind(data_Scale,Vscale)
            names(data_Scale)[ncol(data_Scale)]=VarTemp
            if(i%%10==1 & i != 1){
                timeAvancement <- Sys.time() ## heure de d'avancement
                timeAvancement <- format(timeAvancement, "%d-%m-%Y %HH%M")
                cat("\n       ",timeAvancement,"  ... \n")
		flush.console()}




        }


        cat("DONE\n")
        end <- Sys.time() ## heure de fin
        diff <- end-start
        diff <- paste(round(diff,1),units(diff))
        cat("      ", format(end, "%d-%m-%Y %HH%M")," -> ",diff,"\n\n")
	flush.console()

        forBackTransform=data.frame(cbind(VarList,Mean,Sdev))


        csvfile <- paste0(repoutBackTransform,TagModel,".csv")
        cat("  --> [CSV]:",csvfile)
	flush.console()
        fwrite(forBackTransform,csvfile)
        cat("    DONE !\n")
	flush.console()

        ColNumTest=unlist(lapply(data_Scale[1,],FUN=function(x) is.numeric(x)))
        ColNum=subset(names(data_Scale),ColNumTest)
        data_ColNum=subset(data_Scale,select=ColNum)
        MatCor=cor(data_ColNum)


        if(saveFig){
            pngfile <- paste0(repoutFig,"corrplot","_",tagModel,".png")
            cat("  --> [PNG]:",pngfile)
            flush.console()

            png(pngfile)
            corrplot::corrplot(MatCor)
            dev.off()
            cat("    DONE !\n")
            flush.console()

        }

        Formula=as.formula(paste0(FormulaFix_TtSp
                                 ,FormulaRandom))

        if(SelSample<nrow(data_Scale))
        {
            data_Sample=data_Scale[sample.int(nrow(data_Scale),SelSample),]
        }else{
            data_Sample=data_Scale
        }

        cat("\nModel glmmTMB\n")
        start <- Sys.time() ## heure de demarage
        cat("  ",format(start, "%d-%m-%Y %HH%M"),"  ...  ")
	flush.console()

        if(printFormula) cat("\nglmmTMB(Formula= ",as.character(Formula)[2]," ",as.character(Formula)[1]," ",as.character(Formula)[3],", familiy= ",familyMod,")\n",sep="")

        flush.console()

        w <- NULL
        if(!is.null(weights)) {
            w <- data[,weights,with=FALSE][[1]]

        }

        ModSp=glmmTMB(Formula,data=data_Scale, family=familyMod,ziformula = as.formula(ziformula),weights=w)  #37 min
        if(doBeep) beep()
        if(printSummary) print(summary(ModSp))

        end <- Sys.time() ## heure de demarage
        diff <- end-start
        diff <- paste(round(diff,1),units(diff))
        cat(format(end, "%d-%m-%Y %HH%M")," -> ",diff,"\n\n")

        flush.console()


        simulationOutput <- simulateResiduals(fittedModel = ModSp)
        png(paste0(repoutResDiag,tagModel,".png"))
        plot(simulationOutput)
        dev.off()


        Res=residuals(ModSp)
        data_Sample$Res=Res

        Estimates=as.data.frame(coef(summary(ModSp))$cond)
	term <- row.names(Estimates)
        Estimates=cbind(term=term,Estimates)

	colnames(forBackTransform) <- c("term","bt_mean","bt_sd")
	forBackTransform$bt_sd <- as.numeric(as.character(forBackTransform$bt_sd))
	forBackTransform$bt_mean <- as.numeric(as.character(forBackTransform$bt_mean))

	Estimates <- merge(Estimates,forBackTransform,by="term",all.x=TRUE)

	if(family %in% c("nbinom","nbinom2","poisson","binomial")) Estimates$coef_raw <- exp(Estimates$Estimate) else Estimates$coef_raw <- Estimates$Estimate


	Estimates$coef <- ifelse(is.na(Estimates$bt_sd),Estimates$coef_raw,Estimates$coef_raw^(1/Estimates$bt_sd))


	mdIC <- as.data.frame(confint(ModSp)[,1:2])
        colnames(mdIC) <- c("ICinf","ICsup")
        mdIC <- cbind(term = gsub("cond.","",rownames(mdIC)),mdIC)

        Estimates <-merge(Estimates,mdIC,by="term",all.x=TRUE)


        Estimates$ICinf <- ifelse(is.na(Estimates$bt_sd),exp(Estimates$ICinf),exp(Estimates$ICinf)^(1/Estimates$bt_sd))
	Estimates$ICsup <- ifelse(is.na(Estimates$bt_sd),exp(Estimates$ICsup),exp(Estimates$ICsup)^(1/Estimates$bt_sd))

        glmfile <- paste0(repoutModel,TagModel,".glmmTMB")
        cat("  --> [GLM]:",glmfile)
	flush.console()
        save(ModSp,file=glmfile)
        cat("    DONE !\n")
	flush.console()

        VIFMod=c(1,vif.mer(ModSp))
        Estimates$VIF=VIFMod

        csvfile <- paste0(repoutSummary,TagModel,"_coefs.csv")
        cat("  --> [CSV]:",csvfile)
	flush.console()
        fwrite(Estimates,csvfile,sep=";")
        cat("    DONE !\n")
        flush.console()

        csvfile <- paste0(repoutSummary,TagModel,"_summary.txt")
        cat("  --> [LOG]:",csvfile)
        flush.console()

	cat("\n\n-----------------------------------------------------\n",file=csvfile,append=TRUE)
	cat(format(Sys.time(), "%Y%m%d-%HH%M"),tagModel,file=csvfile,append=TRUE)
	cat("\n-----------------------------------------------------\n\n",file=csvfile,append=TRUE)

	cat(FormulaFix_TtSp,file=csvfile,append=TRUE)
        cat("\n\n",xile=csvfile,append=TRUE)
        cat(capture.output(summary(ModSp),file=csvfile,append=TRUE))
	cat("\n\n=====================================================\n\n",file=csvfile,append=TRUE)

        cat("    DONE !\n")
        flush.console()

        csvfile <- paste0(repoutModel,TagModel,"_",tagModel,"_res.csv")
        cat("  --> [CSV]:",csvfile)
        flush.console()
	fwrite(data_Sample,csvfile)
        cat("    DONE !\n")
        flush.console()

        if(output) return(list(ModSp,Estimates,data_Sample))
    }
}


##for test
if(test)
{
    Sp_GLM_short(
        dataFile="./VigieChiro/DataSp/RPCirw0_50/Pipkuh.csv"
       ,
        varInterest="nb_contacts_strict"
       ,
        listEffects=c("year","poly(julian,2)","sample_cat","nb_Tron_strict"
                     ,"temps_enr_strict","latitude","longitude","expansion_direct"
                      )
       ,
        interactions=NA
       ,
        formulaRandom="+(1|site)"
       ,
        selSample=1e10
       ,
        tagModel="GLMalphatest_tendancesFY"
       ,
        family="nbinom2"
       ,
        asfactor="year"
    )

}
