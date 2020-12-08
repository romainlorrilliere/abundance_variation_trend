




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
##' @param saveFigGlmm TRUE FALSE sauvegarde des figures informatives des modèles
##' @return
##' @author
##'
##'
main_abundance_variation <- function(
                                     id = format(start, "%Y%m%d-%HH%M"), file_data = "data.txt"
                                   , list_effects= c("year"), as_factor_other_than_timestep=NULL
                                   , formula_random_effect= "+(1|site) + (1|year)"
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



    abundance_variation_multisp(id=id,d=d,dsp=dsp,vecSp=vecSp,list_effects=list_effects,asfactor_other_than_year=as_factor_other_than_timestep, formula_random_effect=formula_random_effect, printSummary=printSummary,saveFig=saveFig,saveFigGlmm=saveFigGlmm)




}












abundance_variation_multisp <- function(id = NA,d,dsp=NULL,vecSp=NA,list_effects= c("annee")
                                      , formula_random_effect= "+(1|carre)",asfactor_other_than_year=NULL
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
                             family="nbinom2",
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


        cat("\nModele tendance\n=======================\n")
	md_c <- glmmTMB_sp(data=data_sp,
                           varInterest="obs",
                           listEffects=list_effects,
                           asfactor=asfactor_other_than_year,
                           interactions=NA,
                           formulaRandom=formula_random_effect,
                           selSample=1e10,
                           tagModel=paste0(id,"_glmmTMB_Trend_",sp),
                           family="nbinom2",
                           repout=repout,
                           checkRepout=TRUE,saveFig=FALSE,output=TRUE,
                           doBeep=FALSE,printFormula=TRUE,printSummary=printSummary)


	smd_c <- md_c[[2]]

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


	## tab1t table utile pour la realisation des figures
	tab1t <- data.frame(Est=round(trend,3),
                            LL=round(ic_inf_sim,3), UL=round(ic_sup_sim,3),
                            pourcent=round(pourcentage,3),pourcentage_IC_inf ,pourcentage_IC_sup ,signif=pval<seuilSignif,pval=round(pval,3),
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
            tendance = round(trend,3) ,  IC_inferieur= round(ic_inf_sim,3) , IC_superieur = round(ic_sup_sim,3) ,
            pourcentage_variation= round(pourcentage,3),pourcentage_IC_inf ,pourcentage_IC_sup ,
            erreur_standard = round(erreurannee_c,3), p_value = round(pval,3),
            vif = round(vif_c,3),vif_mean=round(vif_c_mean,3),vif_max=round(vif_c_max,3),
            significatif = trendsignif,categorie_tendance_EBCC=catEBCC,
            theta_variation = round(theta_f,3),theta_tendance = round(theta_c,3),
            valide = catIncert,raison_incertitude = raisonIncert)


	dTrend <- rbind(dTrend,tabTrend1)
	dAn <- rbind(dAn,tabAn1)
	dgg <- rbind(dgg,tab1)#,tab2)

	cat("\nFigure\n=======================\n")
        titre <- paste(tab1$nom_espece[1]," (",tab1$espece[1],")\n",min(tab1$year) ," - ",max(tab1$year),sep="")

        txtPente <- paste(tabTrend1$tendance,
                          ifelse(tabTrend1$significatif," *",""),"  [",tabTrend1$IC_inf," , ",tabTrend1$IC_sup,"]",
                          ifelse(tabTrend1$significatif,paste0("\n",ifelse(tabTrend1$pourcentage_variation>0,"+ ","- "),
                                                              round(abs(tabTrend1$pourcentage_variation))," [",round(tabTrend1$pourcentage_IC_inf)," , ",round(tabTrend1$pourcentage_IC_sup),"] % en ",pasdetemps," ans",sep=""),""),sep="")



        ## table du texte de la tendence

	tabTextPent <- data.table(x=ifelse(tabTrend1$pourcentage_variation>0,-Inf,Inf),
                                  text_hjust= ifelse(tabTrend1$pourcentage_variation>0,-0.1,1.1),
                                  txt=txtPente)

        if(saveFig) {

            figname <- paste0(repoutResult,ifelse(tabTrend1$valide=="Incertain","Incertain/",""),
                              sp,"_",id,".png")


            gg <- ggplot(data=tab1,mapping=aes(x=year,y=val))
            gg <- gg + labs(y="",x="Année",title=titre)
            gg <- gg +  theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank())
            gg <- gg + geom_hline(yintercept=1,colour="white",size=2)
            gg <- gg + geom_ribbon(aes(ymin=LL,ymax=UL),colour=NA,alpha=.2,fill="#3c47e0")
            gg <- gg + geom_pointrange(aes(y=val,ymin=LL,ymax=UL),alpha=.5,colour="#3c47e0")
            gg <- gg + geom_line(size = 1.2,alpha=.8,colour="#3c47e0")
            gg <- gg + geom_point(size = 2,colour="#3c47e0")
            gg <- gg + geom_text(data=tabTextPent, mapping=aes(x=x,y=Inf,label=txt,hjust=text_hjust),vjust=1.1,colour="black",parse=FALSE,fontface=2, size=2.5)

            cat("\n  [PNG]",figname,"\n")
            flush.console()
            ggsave(figname,gg,width=6,height=4)

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

