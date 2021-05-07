




##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param id identifiant du batch de calcul utilisé pour les noms des fichiers de sortie, par defaut la date et l'heure au format(YYYYMDD-HHHMM)
##' @param file_data nom du fichier de données
##' @param list_effects vecteur des effets du modèle stat, par defaut "year"
##' @param as_factor_other_than_timestep vecteur des effects en facteur en plus de l'effet temps (souvent année) par defaut NULL, peut être "passage"
##' @param formula_random_effect formule de l'effet random par defaut  "+(1|site) + (1|year)". si pas d'effet "+1"
##' @param first_year filtre des données première année, si NULL plus petite année du jeux de données, par defaut NULL
##' @param last_year filtre des données dernière années, si NULL plus grande années du jeux de données, par defaut NULL
##' @param vecSp filtre des données, vecteur des espèces conservée pour l'analyse. Les espèces doivent être au format des données présent dans la colonne d_species_colname, par defaut NULL
##' @param d_species_colname nom de la colonne avec les identifiant des espèces, par defaut "id_sp"
##' @param d_observed_var_colname nom de la colonne de la variable observée (nombre, abondance...), par defaut "obs"
##' @param d_timestep_colname nom de la colonne de temps (annéé), par defaut "year"
##' @param species_file_name nom du fichier de la table de référence des nom d'espèce, pas utilisé si maquant
##' @param dsp_species_colname nom de la cololnne des identifiants des espces dans la table des espèces. Les identifiants doivent être correspondre à ceux de la table des données, par defaut "id_sp"
##' @param dsp_species_name_colname  nom de la colonne avec le nom des espèces, par defaut "name"
##' @param repinput nom du dossier où sont les data, par defaut "data/"
##' @param repout nom du dossier dans le quel les résultats seront enregistrés, par defaut "output/"
##' @param data_file_encoding encodage du fichier de données par defaut "Latin-1" peut être "UTF-8"
##' @param species_file_encoding  encodage du fichier des espèces par defaut "Latin-1" peut être "UTF-8"
##' @param printSummary TRUE FALSE d'affichage des summaries des modèles
##' @param saveFig TRUE FALSE sauvegarde des figures des tendances
##' @param saveFigGmm TRUE FALSE sauvegarde des figures informatives des modèles
##' @return
##' @author
##'
##'
main_abundance_variation <- function(
                                     id = format(Sys.time(), "%Y%m%d-%HH%M"), file_data = "data.txt"
                                   , list_effects= c("year"), as_factor_other_than_timestep=NULL
                                   , formula_random_effect= "+(1|site) + (1|year)"
                                   , family = "nbinom2"
                                   , ziformula = "~1"
                                   , first_year = NULL,last_year=NULL
                                   , vecSp = NA

                                   , d_species_colname = "id_sp",d_observed_var_colname = "obs"
                                   , d_timestep_colname = "year"

                                   , species_file_name = "library/espece.csv"

                                   , dsp_species_colname = "id_sp",dsp_species_name_colname = "name"

                                   , repinput = "data/", repout="output/"
                                   , data_file_encoding = "Latin-1", species_file_encoding = "Latin-1"

                                   , printSummary=TRUE,saveFig=TRUE, saveFigGlmm=TRUE)
{


    ##   , varsp_species = "sp"


    ##    , var_species = "espece", var_abundance = "nombre"
    ##    , varsp_species_name = "french_name"


    start <- Sys.time()
    if(is.na(id))id <- format(start, "%Y%m%d-%HH%M")


    filepath <- paste0(repinput,file_data)
    cat("Import:",filepath," ...")
    d <- fread(filepath,encoding=data_file_encoding)

    if(d_species_colname != "id_sp") {
        if("id_sp" %in% colnames(d)) setnames(d,"id_sp","id_sp_old")
        setnames(d, d_species_colname, "id_sp")

        list_effects[list_effects == d_species_colname]  <- "id_sp"
        if(!is.null(as_factor_other_than_timestep)) as_factor_other_than_timestep[as_factor_other_than_timestep == d_species_colname]  <- "id_sp"
        formula_random_effect=gsub(d_species_colname,"id_sp",formula_random_effect)
    }

    if(d_observed_var_colname != "obs") {
        if("obs" %in% colnames(d)) setnames(d,"obs","obs_old")
        setnames(d, d_observed_var_colname, "obs")

           list_effects[list_effects == d_species_colname]  <- "id_sp"
        if(!is.null(as_factor_other_than_timestep)) as_factor_other_than_timestep[as_factor_other_than_timestep == d_species_colname]  <- "id_sp"
        formula_random_effect=gsub(d_species_colname,"id_sp",formula_random_effect)
    }

    if(d_timestep_colname != "year") {
        if("year" %in% colnames(d)) setnames(d,"year","year_old")
        setnames(d, d_timestep_colname, "year")

           list_effects[list_effects == d_timestep_colname]  <- "year"
        if(!is.null(as_factor_other_than_timestep)) as_factor_other_than_timestep[as_factor_other_than_timestep == d_timestep_colname]  <- "year"
        formula_random_effect=gsub(d_timestep_colname,"year",formula_random_effect)
    }

    if(is.null(first_year))first_year <- min(d[,year])
    if(is.null(last_year))last_year <- max(d[,year])
    d <- d[year >= first_year & year <=last_year,]

    d <- d[id_sp %in% vecSp,]

    cat("\n\n")
    print(d)
    cat("\n\n")

    if(!is.null(species_file_name)){
        dsp <- fread(species_file_name,encoding="Latin-1",stringsAsFactors=FALSE)

        if(dsp_species_colname != "id_sp") {
            if("id_sp" %in% colnames(dsp)) setnames(d,"id_sp","id_sp_old")
            setnames(dsp, dsp_species_colname, "id_sp")
        }


        if( dsp_species_name_colname != "sp_name") {
            if("sp_name" %in% colnames(dsp)) setnames(d,"sp_name","sp_name_old")
            setnames(dsp, dsp_species_name_colname, "sp_name")
        }
    cat("\n\n")
        print(dsp)
            cat("\n\n")
    } else {
        dsp <- NULL
    }




    abundance_variation_multisp(id=id,d=d,dsp=dsp,vecSp=vecSp,list_effects=list_effects,asfactor_other_than_year=as_factor_other_than_timestep, formula_random_effect=formula_random_effect,family = family,ziformula = ziformula,printSummary=printSummary,saveFig=saveFig,saveFigGlmm=saveFigGlmm)




}












abundance_variation_multisp <- function(id = NA,d,dsp=NULL,vecSp=NA,list_effects= c("annee")
                                      , formula_random_effect= "+(1|carre)",asfactor_other_than_year=NULL
                                      , family = "nbinom2",ziformula = "~1"
                                      , seuilSignif  = 0.05, repout = "output/",output=FALSE,saveFig=TRUE
                                      , printSummary = TRUE, saveFigGlmm = TRUE) {




    start <- Sys.time() ## heure de demarage est utilisée comme identifiant par defaut

    if(is.na(id))id <- format(start, "%Y%m%d-%HH%M")

    dir.create(repout,showWarnings=FALSE)

    repout <- paste0(repout,id,"/")
    dir.create(repout,showWarnings=FALSE)
    repoutResult <- paste0(repout,"result_figure/")
    dir.create(repoutResult,showWarnings=FALSE)

    dir.create(paste0(repoutResult,"Incertain"),showWarnings=FALSE)


    file_trend <- paste0(repout,id,"_tendances.csv")
    file_An <- paste0(repout,id,"_variations.csv")
    file_gg <- paste0(repout,id,"_ggTable.csv")



    dAn <- NULL
    dgg <- NULL
    dTrend <- NULL

    if(!is.null(dsp))d <- d[id_sp %in% dsp[,id_sp],]
    if(!is.null(vecSp)) d <- d[id_sp %in% vecSp,]

    f <- formula("obs ~ id_sp")
    dagg <- setDT(aggregate(f,d,sum))
    setorderv(dagg,"obs",order = -1)

    listSp <- dagg[,id_sp]


    print(dagg)

    for(sp in listSp) {

	nomSp <- dsp[id_sp == sp,sp_name]
	cat("\n\n==========================\n",sp,nomSp,"\n===============================\n\n")
	flush.console()

	data_sp <- d[id_sp == sp,]


	cat("\nModele variations temporelles\n=======================\n")
	md_f <- glmmTMB_sp(data=data_sp,
                             varInterest="obs",
                             listEffects=list_effects,
                             asfactor=c("year",asfactor_other_than_year),
                             interactions=NA,
                             formulaRandom=formula_random_effect,
                             selSample=1e10,
                             tagModel=paste0(id,"_glmmTMB_VarAn_",sp),
                             family=family,ziformula = ziformula,
                             repout=repout,
                             checkRepout=TRUE,saveFig=saveFigGlmm,output=TRUE,
                             doBeep=FALSE,printFormula=TRUE,printSummary=printSummary)
	smd_f <- md_f[[2]]

        smd_f <- smd_f[grep("year_as_factor",smd_f$term),]
        smd_f$year <- as.numeric(gsub("year_as_factor","",smd_f$term))

        smd_f <- smd_f[order(smd_f$year),]
	theYears <- smd_f$year
        theYears <- c(min(theYears)-1,theYears)


	theta_f <- round(sigma(md_f[[1]]),3)
	coefan <- c(1,smd_f$coef)
	erreuran <- smd_f[,3]

	erreurannee1 <- c(0,erreuran *smd_f$coef)
	pval <- c(1,smd_f[,5])
	ic_inf_sim <-  c(1,smd_f$ICinf)
	ic_sup_sim <-  c(1,smd_f$ICsup)


	tab1 <- data.table(id = id,espece = sp, nom_espece = nomSp,
                           year = theYears,
                           val = coefan,
                           LL = ic_inf_sim, UL = ic_sup_sim,
                           catPoint = ifelse(pval < seuilSignif,"significatif",NA),pval)


	tabAn1 <- data.table(id,espece = sp, nom_espece = nomSp ,
                             year=theYears,
                             abondance_relative = round(coefan,3),
                             IC_inferieur = round(ic_inf_sim,3), IC_superieur = round(ic_sup_sim,3),
                             erreur_standard = round(erreurannee1,4),
                             p_value = round(pval,3),significatif = ifelse(pval < seuilSignif,"significatif",NA),theta=round(theta_f,3))



##  cat("\nModele Simulation de la tendance\n=======================\n")
##
##
##        pp <- fixef(md_f[[1]])$cond
##        vv <- vcov(md_f[[1]])$cond
##        samp <- MASS::mvrnorm(100, mu=pp, Sigma=vv)
##
##
##        ##head(samp)
##        dsamp <- melt(samp)
##        colnames(dsamp) <- c("id","var","obs_brut")
##        setDT(dsamp)
##        firstYear <- min(theYears)
##        dsamp[,year := as.numeric(gsub("year_as_factor","",var))]
##
##        dsamp[var == "(Intercept)",year := firstYear]
##        dsamp[year == firstYear, obs_brut := 0]
##
##        dsamp[,obs := exp(obs_brut)]
##
##        dagg <- aggregate(obs~year,data=dsamp,FUN=mean)
##        head(dagg)
##
##  ##      gg <- ggplot(dsamp,aes(x=year,obs)) + geom_violin(aes(group=year)) + geom_smooth()#method="lm",se=TRUE,formula = as.formula("y~exp(x)"))
##  ##      gg <- gg + geom_line(data=dagg)
##
##
##
##
##        formule <- as.formula(paste("obs_brut ~ year",sep=""))
##        md2 <- lm(formule,data=dsamp)#,family=quasipoisson)
##        smd2 <- summary(md2)
##		## tendences sur la periode
##        coefannee <- tail(matrix(coefficients(md2)),1)
##        intercept <- matrix(coefficients(md2))[1,1]
##
##        trend_sim <- round(exp(coefannee),3)
##        ## pourcentage de variation sur la periode
##        pasdetemps <- length(theYears)-1
##
##        pourcentage_sim <- round((exp(coefannee*pasdetemps)-1)*100,2)
##
##
##        pval_sim <- smd2$coefficients[2,4]
##
##        erreuran <- as.data.frame(tail(matrix(summary(md2)$coefficients[,2]),1))
##		## erreur standard
##        erreurannee2 <- as.vector(erreuran*exp(coefannee))[,1]
##
##
##		## calcul des intervalle de confiance
##        md2.sim <- sim(md2)
##        log_LL_sim <- tail(apply(coef(md2.sim), 2, quantile,.025),1)
##        LL_sim <- round(exp(log_LL_sim),3)
##        log_UL_sim <- tail(apply(coef(md2.sim), 2, quantile,.975),1)
##        UL_sim <- round(exp(log_UL_sim),3)
##
##
##        pourcent_LL_sim <- round((exp(log_LL_sim*pasdetemps)-1)*100,2)
##        pourcent_UL_sim <- round((exp(log_UL_sim*pasdetemps)-1)*100,2)
##
##
##        decay <- function(t, r,i) exp(i)*exp(r*t)
##        lastyear <- max(d$year)
##        ddecay <- setDT(data.frame(year=(seq(from = firstYear,to=lastyear,by=0.1)),txR=coefannee,intercept=intercept))
##        ddecay[,obs:=decay(year,txR,intercept)]
##
##
##        titre <- paste(tab1$nom_espece[1]," (",tab1$espece[1],")\n",min(tab1$year) ," - ",max(tab1$year),sep="")
##
##        txtPente <- paste0(trend_sim,
##                           "  [",LL_sim," , ",UL_sim,"]",
##                           "\n",ifelse(pourcentage_sim>0,"+ ","- "),
##                                 round(abs(pourcentage_sim))," [",round(pourcent_LL_sim)," , ",round(pourcent_UL_sim),"] % en ",pasdetemps," ans")
##
##
##
##    ## table du texte de la tendence
##
##    tabTextPent <- data.frame(x=ifelse(pourcentage_sim>0,-Inf,Inf),
##                              text_hjust= ifelse(pourcentage_sim>0,-0.1,1.1),
##                              txt=txtPente)
##
##
##
##    if(saveFig) {
##
##
##        figname <- paste0(repoutResult,
##                          sp,"_sim_",id,".png")#ifelse(tabTrend1$valide=="Incertain","Incertain/","")
##
##
##        gg <- ggplot(dsamp,aes(x=year,y=obs)) +geom_smooth()+ #geom_violin(group=year) +
##        geom_point(alpha=.1)
##        gg
##        gg <- gg + geom_line(data = ddecay,colour="red",alpha=.5,size=1.5)
##        gg <- gg + labs(y="",x="Année",title=titre)
##        gg <- gg + geom_text(data=tabTextPent, mapping=aes(x=x,y=Inf,label=txt,hjust=text_hjust),vjust=1.1,colour="black",parse=FALSE,fontface=2, size=2.7)
##gg
##        cat("\n  [PNG]",figname,"\n")
##        flush.console()
##        ggsave(figname,gg,width=6,height=4)
##
##
##    }
##
##
##
        cat("\nModele tendance\n=======================\n")
	md_c <- glmmTMB_sp(data=data_sp,
                           varInterest="obs",
                           listEffects=list_effects,
                           asfactor=asfactor_other_than_year,
                           interactions=NA,
                           formulaRandom=formula_random_effect,
                           selSample=1e10,
                           tagModel=paste0(id,"_glmmTMB_Trend_",sp),
                           family=family,ziformula = ziformula,weights = NULL,
                           repout=repout,
                           checkRepout=TRUE,saveFig=FALSE,output=TRUE,
                           doBeep=FALSE,printFormula=TRUE,printSummary=printSummary)



	smd_c <- md_c[[2]]

        coefannee <- summary(md_c[[1]])$coefficients$cond[1,2]
        intercept <- summary(md_c[[1]])$coefficients$cond[1,1]

        vif_c_mean <- mean(smd_c$VIF)
	vif_c_max <- max(smd_c$VIF)
	theta_c <- sigma(md_c[[1]])
	smd_c <- smd_c[smd_c$term=="year",]
	coefan <- smd_c$coef
	trend <- round(coefan,3)
	## pourcentage de variation sur la periode

	pasdetemps <- length(theYears)-1
	pourcentage <- ((coefan^pasdetemps)-1)*100
	pval <- smd_c[,5]
	erreuran <- smd_c[,3]
	## erreur standard
	erreurannee_c <- erreuran*coefan
	vif_c <- smd_c$VIF
	ic_inf_sim <-  smd_c$ICinf
	ic_sup_sim <-  smd_c$ICsup
        pourcentage_IC_inf <- ((ic_inf_sim^pasdetemps)-1)*100
	pourcentage_IC_sup <- ((ic_sup_sim^pasdetemps)-1)*100



##    cat("\nModele tendance pondéré\n=======================\n")
##    md_c_w <- glmmTMB_sp(data=data_sp,
##                       varInterest="obs",
##                       listEffects=list_effects,
##                       asfactor=asfactor_other_than_year,
##                       interactions=NA,
##                       formulaRandom=formula_random_effect,
##                       selSample=1e10,
##                       tagModel=paste0(id,"_glmmTMB_Trend_weigthed_",sp),
##                       family=family,ziformula = ziformula,weights = trend_weights,
##                       repout=repout,
##                       checkRepout=TRUE,saveFig=FALSE,output=TRUE,
##                       doBeep=FALSE,printFormula=TRUE,printSummary=printSummary)
##
##
##    smd_c_w <- md_c_w[[2]]
##
##    vif_c_mean_w <- mean(smd_c_w$VIF)
##    vif_c_max_w <- max(smd_c_w$VIF)
##    theta_c_w <- sigma(md_c_w[[1]])
##    smd_c_w <- smd_c_w[smd_c_w$term=="year",]
##    coefan_w <- smd_c_w$coef
##    trend_w <- round(coefan_w,3)
##    ## pourcentage de variation sur la periode
##
##    pasdetemps <- length(theYears)-1
##    pourcentage_w <- ((coefan_w^pasdetemps)-1)*100
##    pval_w <- smd_c_w[,5]
##    erreuran_w <- smd_c_w[,3]
##    ## erreur standard
##    erreurannee_c_w<- erreuran_w*coefan_w
##    vif_c_w <- smd_c_w$VIF
##    ic_inf_sim_w <-  smd_c_w$ICinf
##    ic_sup_sim_w <-  smd_c_w$ICsup
##    pourcentage_IC_inf_w <- ((ic_inf_sim_w^pasdetemps)-1)*100
##    pourcentage_IC_sup_w <- ((ic_sup_sim_w^pasdetemps)-1)*100
##

#############################
####################
################
##
##        Est_sim=round(trend_sim,3),
##        LL_sim=round(LL_sim,3), UL=round(UL_sim,3),
##        pourcent_sim=round(pourcentage_sim,3),
##        pourcentage_IC_inf_sim = pourcent_LL_sim ,
##        pourcentage_IC_sup_sim  =  pourcent_UL_sim,
##        pval_sim=round(pval_sim,3),
##        Est_model_weight=round(trend_w,3),
##        LL_model_weight=round(ic_inf_sim_w,3), UL_model_weight=round(ic_sup_sim_w,3),
##        pourcent_model_weight=round(pourcentage_w,3),
##        pourcentage_IC_inf_model_weight = pourcentage_IC_inf_w ,
##        pourcentage_IC_sup_model_weight = pourcentage_IC_sup_w ,
##        signif_model_weight=pval_w<seuilSignif,pval_model_weight=round(pval_w,3),
##        vif_weight=round(vif_c_w,3),vif_mean_weight=round(vif_c_mean_w,3),vif_max_weight=round(vif_c_max_w,3),
##

	## tab1t table utile pour la realisation des figures
	tab1t <- data.frame(Est=round(trend,3),
                            LL=round(ic_inf_sim,3), UL=round(ic_sup_sim,3),
                            intercept=round(intercept,3),
                            pourcent=round(pourcentage,3),
                            pourcentage_IC_inf = pourcentage_IC_inf ,
                            pourcentage_IC_sup = pourcentage_IC_sup ,
                            signif=pval<seuilSignif,pval=round(pval,3),
                            vif=round(vif_c,3),vif_mean=round(vif_c_mean,3),vif_max=round(vif_c_max,3))
	trendsignif <- tab1t$signif

	## surdispersion
	## affectation des tendence EBCC
	catEBCC <- NA
	catEBCC <- affectCatEBCC(trend = tab1t$Est,pVal = tab1t$pval,ICinf=as.vector(tab1t$LL),ICsup=as.vector(tab1t$UL))
	## table complete de resultats
	vecLib <-  NULL
	if(is.na(vif_c_mean)) {
            catIncert <- "Incertain"
            if(is.na(vif_c_mean)) vecLib <- paste(vecLib,"VIF tendance non calculable")
	} else { # ELSE  if(is.na(vif_c_mean))
            if( vif_c_mean > 2 | vif_c_max > 5 | theta_c
               < .1 | theta_f > 10 | theta_c < .1 | theta_c > 10) {
                catIncert <- "Incertain"
                if(vif_c_mean > 2) vecLib <- c(vecLib,"moyenne vif tendance sup à 2")
                if(vif_c_max > 5) vecLib <- c(vecLib,"max vif tendance sup à 5")
                if(theta_f < 0.1) vecLib <- c(vecLib," theta variation inf à 0.1")
                if(theta_c < 0.1) vecLib <- c(vecLib," theta tendance inf à 0.1")
                if(theta_f > 10) vecLib <- c(vecLib," theta variation sup à 10")
                if(theta_c > 10) vecLib <- c(vecLib," theta tendance sup à 10")
            } else {
                catIncert <-"bon"
            }
	} # END ELSE  if(is.na(vif_c_mean))

	raisonIncert <-  paste(vecLib,collapse=" et ")

	firstY <- min(theYears)
	lastY <- max(theYears)

	tabTrend1 <- data.frame(
            id,espece=sp,nom_espece = nomSp ,
            nombre_annees = pasdetemps+1,premiere_annee = firstY,derniere_annee = lastY,
            tendance = round(trend,3) ,
            IC_inferieur= round(ic_inf_sim,3) , IC_superieur = round(ic_sup_sim,3) ,
            significatif = trendsignif,

            pourcentage_variation= round(pourcentage,3),
            pourcentage_IC_inf = pourcentage_IC_inf ,
            pourcentage_IC_sup = pourcentage_IC_sup ,
            erreur_standard = round(erreurannee_c,3), p_value = round(pval,3),
            intercept = round(intercept,3),
            significatif = trendsignif,#####
  categorie_tendance_EBCC=catEBCC,

            vif = round(vif_c,3),vif_mean=round(vif_c_mean,3),vif_max=round(vif_c_max,3),
            theta_variation = round(theta_f,3),theta_tendance = round(theta_c,3))


	dTrend <- rbind(dTrend,tabTrend1)
	dAn <- rbind(dAn,tabAn1)
	dgg <- rbind(dgg,tab1)#,tab2)

	cat("\nFigure\n=======================\n")
        titre <- paste(tab1$nom_espece[1]," (",tab1$espece[1],")\n",min(tab1$year) ," - ",max(tab1$year),sep="")

        txtPente <- paste(tabTrend1$tendance,
                          ifelse(tabTrend1$significatif," *",""),"  [",tabTrend1$IC_inferieur," , ",tabTrend1$IC_superieur,"]",
                          ifelse(tabTrend1$significatif,paste0("\n",ifelse(tabTrend1$pourcentage_variation>0,"+ ","- "),
                                                              round(abs(tabTrend1$pourcentage_variation))," [",round(tabTrend1$pourcentage_IC_inf)," , ",round(tabTrend1$pourcentage_IC_sup),"] % en ",pasdetemps," ans",sep=""),""),sep="")








### GAMM

##        gammgg <- gamm(obs~s(year), data=data_sp,random=reStruct(object = ~ 1| carre, pdClass="pdDiag"),family = "nbinom2")
##
##
##      #          gammgg <- gamm(obs~s(year), data=data_sp,random=reStruct(object = ~ 1| carre, pdClass="pdDiag"),correlation=corAR1(form=~year),family = "nbinom2")
##
##            maxyear<-max(d$year)
##            minyear<-min(d$year)
##            year.seq<-sort(unique(c(minyear:maxyear,(seq(minyear, maxyear,length=1000)))))
##            year.seq<-data.frame(year=year.seq)
##
##                                        # predict only the temperature term (the sum of the
##                                        # term predictions and the intercept gives you the overall
##                                        # prediction)
##
##            preds<-predict(gammgg$gam, newdata=year.seq, type="terms", se.fit=TRUE)
##
##
##                                        # set up the temperature, the fit and the upper and lower
##                                        # confidence interval
##
##            year <-year.seq$year
##            realYear <- sort(unique(d$year))
##            fit<-exp(as.vector(preds$fit))
##            init <- fit[1]
##
##            fit.up95<-fit-1.96*exp(as.vector(preds$se.fit))
##
##            fit.low95<-fit+1.96*exp(as.vector(preds$se.fit))
##
##           # ggGamData <- data.frame(year=year, csi=fit,ic_low95 = fit.low95, ic_up95 = fit.up95)
##
##        fit <- fit - init
##         fit.up95 <- fit.up95 - init
##        fit.low95 <- fit.low95 - init
##
##        ggGamData <- data.frame(year=year, val=fit,ic_low95 = fit.low95, ic_up95 = fit.up95)
##
##      #  browser()
##        ## The ggplot:
##            gg <- ggplot(data=ggGamData,aes(x=year,y=val))
##        gg <- gg + geom_ribbon(aes(ymin=ic_low95, ymax=ic_up95),alpha=.2)+ geom_line(size=1)
##        gg
##            gg <- gg +  geom_point(data = subset(ggGamData,year %in% realYear),size=3,colour=couleur) + geom_point(data = subset(ggGamData,year %in% realYear),size=1.5,colour="white")
##            gg <- gg + labs(y=titreY,x=titreX,title=titre)+scale_x_continuous(breaks=pretty_breaks())
##
##
##















        ## table du texte de la tendence

	tabTextPent <- data.table(x=ifelse(tabTrend1$pourcentage_variation>0,-Inf,Inf),
                                  text_hjust= ifelse(tabTrend1$pourcentage_variation>0,-0.1,1.1),
                                  txt=txtPente)

        if(saveFig) {

            figname <- paste0(repoutResult,ifelse(tabTrend1$valide=="Incertain","Incertain/",""),
                              sp,"_",id,".png")

            decay <- function(t, r,i) exp(i)*exp(r*t)
            lastyear <- max(d$year)
            firstyear <- min(d$year)
            ddecay <- setDT(data.frame(year=(seq(from = firstyear,to=lastyear,by=0.1)),txR=coefannee,intercept=intercept))
            ddecay[,val:=decay(yeart,txR,intercept)]

            gg <- ggplot(data=tab1,mapping=aes(x=year,y=val))
            gg <- gg + labs(y="",x="Année",title=titre)
            gg <- gg +  theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank())
            gg <- gg + geom_hline(yintercept=1,colour="white",size=2)
           ##   gg <- gg + geom_ribbon(data = ggGamData,aes(ymin=ic_low95, ymax=ic_up95),alpha=.2)+ geom_line(data = ggGamData,size=1)

            gg <- gg + geom_ribbon(aes(ymin=LL,ymax=UL),colour=NA,alpha=.2,fill="#3c47e0")
            gg <- gg + geom_pointrange(aes(y=val,ymin=LL,ymax=UL),alpha=.5,colour="#3c47e0")
            gg <- gg + geom_line(size = 1.2,alpha=.8,colour="#3c47e0")
            gg <- gg + geom_line(data = ddecay)
            gg <- gg + geom_point(size = 2,colour="#3c47e0")
            gg <- gg + geom_text(data=tabTextPent, mapping=aes(x=x,y=Inf,label=txt,hjust=text_hjust),vjust=1.1,colour="black",parse=FALSE,fontface=2, size=2.5)
            gg



            cat("\n  [PNG]",figname,"\n")
            flush.console()
            ggsave(figname,gg,width=6,height=4)
browser()
        }


        cat("\n  [CSV]",file_trend,"\n")
	flush.console()
	write.csv(dTrend,file_trend,row.names=FALSE)
	cat("\n  [CSV]",file_An,"\n")
	flush.console()
	write.csv(dAn,file_An,row.names=FALSE)
	cat("\n  [CSV]",file_gg,"\n")
	flush.console()
	write.csv(dgg,file_gg,row.names=FALSE)

    }

    cat("\n==============================\n  [CSV]",file_trend,"\n")
    flush.console()
    fwrite(dTrend,file_trend,sep="\t")
    cat("\n  [CSV]",file_An,"\n")
    flush.console()
    fwrite(dAn,file_An,sep="\t")
    cat("\n  [CSV]",file_gg,"\n")
    flush.console()
    fwrite(dgg,file_gg,sep="\t")

}


