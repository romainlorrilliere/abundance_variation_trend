library(data.table)
library(rtrim)
library(ggplot2)








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
main_abundance_variation_trim <- function(
                                          id = format(Sys.time(), "%Y%m%d-%HH%M")
                                        , file_data = "data.txt"
                                        , max_altitude=800
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

    #id <- NA; file_data <- "data_FrenchBBS_carre_trend_2021-02-24_2001_2019fr.csv";repinput = "data/"; repout="output/";data_file_encoding = "Latin-1"; species_file_encoding = "Latin-1"

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

##   dvieu <- fread("data/carre1989-2013trifinal.txt")
##   dvieu <- dvieu[,carre := paste0(substr(carre,8,9),substr(carre,11,14))]
##   dvieu <- melt(dvieu, id.vars = c("carre", "annee","x","y","alt","inf800"),variable.name = "id_sp", value.name = "obs")
##
##   dvieu[,id_carre_annee := paste0(annee,carre)]
##   dvieu <- dvieu[!(id_carre_annee %in% d$id_carre_annee),]
##   dvieu[,xy := paste(x,y,sep="_")]
##   carre_vieu <- unique(dvieu[carre == "",.(carre,xy)])
##
##   carre_vieu[,carre := paste0("99",str_pad(1:.N, 4, pad = "0"))]
##   dvieu[,carre := NULL]
##   dvieu <- merge(dvieu,carre_vieu)
##   dvieu[,id_carre_annee := paste0(annee,carre)]
##   dvieu[,etude := "STOC_1989"]
##   d[,etude := "STOC"]
##   d[,inf800 := altitude < 800]
##   dvieu[,dist := "?"]
##
##   setnames(dvieu,"alt","altitude")
##   setnames(dvieu,"annee","year")
##
##   vecCol <- c("id_carre_annee","carre","year","altitude","inf800","id_sp","obs","etude","dist" )
##
##   dvieu <- dvieu[,vecCol,with=FALSE]
##   d <- d[,vecCol,with=FALSE]


   # d <- rbind(dvieu,d)



    if(is.null(first_year))first_year <- min(d[,year])
    if(is.null(last_year))last_year <- max(d[,year])
    if(is.null(max_altitude)) max_altitude  <- 800
    d <- d[year >= first_year & year <=last_year,]
    d <- d[altitude < max_altitude,]

    if(is.null(vecSp)) vecSp <- unique(d$id_sp)

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

    abundance_variation_multisp_trim(id=id,d=d,dsp=dsp,vecSp=vecSp,printSummary=printSummary,saveFig=saveFig)




}












abundance_variation_multisp_trim <- function(id = NA,d,dsp=NULL,vecSp=NA
                                           , seuilSignif  = 0.05, repout = "output/",output=FALSE,saveFig=TRUE
                                           , printSummary = TRUE) {




    start <- Sys.time() ## heure de demarage est utilisée comme identifiant par defaut

    if(is.na(id))id <- format(start, "%Y%m%d-%HH%M")

    dir.create(repout,showWarnings=FALSE)

    repout <- paste0(repout,id,"/")
    dir.create(repout,showWarnings=FALSE)
    repoutResult <- paste0(repout,"result_figure/")
    dir.create(repoutResult,showWarnings=FALSE)


    file_trend <- paste0(repout,id,"_tendances.csv")
    file_An <- paste0(repout,id,"_variations.csv")
    file_gg <- paste0(repout,id,"_ggTable.csv")
    file_ggSlope <- paste0(repout,id,"_ggSlopeTable.csv")


    dAn <- NULL
    dgg <- NULL
    dTrend <- NULL
    dggSlope <- NULL

    if(!is.null(dsp))d <- d[id_sp %in% dsp[,id_sp],]
    if(!is.null(vecSp)) d <- d[id_sp %in% vecSp,]

    f <- formula("obs ~ id_sp")
    dagg <- setDT(aggregate(f,d,sum))
    setorderv(dagg,"obs",order = -1)

    listSp <- dagg[,id_sp]


    print(dagg)

    for(sp in listSp) {

        browser()
	nomSp <- dsp[id_sp == sp,sp_name]
        nomSc <- dsp[id_sp == sp,nomscientific]
	cat("\n\n==========================\n",sp,nomSp,"\n===============================\n\n")
	flush.console()

	data_sp <- d[id_sp == sp,]

        data_sp <- data_sp[,.(carre,year,obs)]

        sum_year <- aggregate(obs~year, data=data_sp, FUN = sum)
        sum_year <- sum_year[order(sum_year$year),]
        if(sum_year$obs[1] == 0) {
            cat("Data starts with 1 years without positive observations.\n")
            cat("First year change\n")
            setDT(sum_year)
            sum_year <- sum_year[obs>0,]
            firstYear <- min(sum_year[,year])

            data_sp <- data_sp[year >= firstYear,]
            cat("\n--> New first year:",firstYear,"\n\n")

        }




       ## d_expand <- expand.grid(carre = unique(data_sp$carre), year = unique(data_sp$year))
       ## data_sp <- left_join(d_expand,data_sp)
       ## data_sp[is.na(data_sp)] <- -1
        val_model = 2; val_changepoints = 'all'; val_serialcor = TRUE; val_overdisp = TRUE

        cat( "model =",val_model," changepoints =",val_changepoints,"\nserialcor =",val_serialcor," overdisp =", val_overdisp,"\n")
        fit <- trim(obs ~ carre + year, data = data_sp, model = val_model, changepoints = val_changepoints, serialcor = val_serialcor, overdisp = val_overdisp)

        sumfit <- summary(fit)
        rho_serialcorrelation <- sumfit$serialcorrelation
        overdispersion <- sumfit$overdispersion

        if(rho_serialcorrelation < 1 | overdispersion < 1)
        {
            val_serialcor = rho_serialcorrelation > 1
            val_overdisp = overdispersion > 1


            cat( "\n--> nouveau model\nmodel =",val_model," changepoints =",val_changepoints,"\nserialcor =",val_serialcor," overdisp =", val_overdisp,"\n")

            fit <- trim(obs ~ carre + year + etude, data = data_sp, model = val_model, changepoints = val_changepoints, serialcor = val_serialcor, overdisp = val_overdisp)



        }


        overall <- overall(fit, which = "imputed")
        start <- overall$tt[1]
        intercept <- overall$intercept
        trend <- overall$slope

        alpha <- 0.05
        n.years <- max(data_sp$year)-min(data_sp$year)+1
        df <- n.years - 2
        t <- qt((1 - alpha/2), df)
        M <- (1 + n.years)/2

        years <- 1: n.years
        years <- seq(1,n.years,0.1)
        dx2 <- (years - mean(years))^2
        sumdj2 <- sum((years - mean(years))^2)
        SSR <- overall$SSR
        dy <- t * sqrt((SSR/df) * (1/n.years + dx2/sumdj2))

        trend.est   <- exp(intercept$add + trend$add * years)
        trend.lower <- exp(intercept$add + trend$add * years - dy)
        trend.upper <- exp(intercept$add + trend$add * years + dy)

        tab_trend <- data.frame(id=id,sp=sp,nom_espece = nomSp ,nom_scientific = nomSc,years=years,year = seq(min(data_sp$year), max(data_sp$year),.1),
                                obs = trend.est /start, CI_inf = trend.lower/start,CI_sup = trend.upper/start)

      #

        oo <- overall
        oo$tt <- oo$tt/start
        oo$err <- oo$err/start
        oo$intercept$mul <- oo$intercept$mul / start
        oo$intercept$se_mul <- oo$intercept$se_mul / start
        oo$intercept$add <- log(overall$intercept$mul / start)
        oo$intercept$se_add <- log(overall$intercept$se_mul / start)

        doo <- setDT(data.frame(id=id,sp=sp,nom_espece = nomSp ,nom_scientific = nomSc,year = oo$timept,obs = oo$tt,err = oo$err))
        CI <- confint(fit)
        CI <- CI/start
        colnames(CI) <- c("CIinf","CIsup")
        doo <- cbind(doo,CI)

        pente <- round(oo$slope$mul,3)
        add_ICinf <- oo$slope$add - (1.96 * oo$slope$se_add)
        add_ICsup <- oo$slope$add + (1.96 * oo$slope$se_add)

        mul_ICinf <-  exp(add_ICinf)
        mul_ICsup <- exp(add_ICsup)

        firstYear <- min(data_sp$year)
        lastYear <- max(data_sp$year)
        pasdetemps <- lastYear - firstYear
        pourcent <- round(((oo$slope$mul^pasdetemps)-1)*100,1)
        pourcent_ICinf <- round(((mul_ICinf^pasdetemps)-1)*100,1)
        pourcent_ICsup <- round(((mul_ICsup^pasdetemps)-1)*100,1)
        pval <- overall$slope[1,7]

	catEBCC <- NA
	catEBCC <- affectCatEBCC(trend =pente,pVal = pval,ICinf= mul_ICinf,ICsup= mul_ICsup)

        tabTrend1 <- data.frame(
            id=id,espece=sp,nom_espece = nomSp ,nom_scientific = nomSc,
            nombre_annees = pasdetemps+1,premiere_annee = firstYear,derniere_annee = lastYear,
            tendance = pente ,
            IC_inferieur= round(mul_ICinf,3) , IC_superieur = round(mul_ICsup,3) ,
            significatif = TRUE,
            pourcentage_variation= pourcent,
            pourcentage_IC_inf =pourcent_ICinf,
            pourcentage_IC_sup =pourcent_ICsup,
            erreur_standard = oo$slope$se_add , p_value = pval,
            intercept = oo$intercept$add,
            significatif = pval<0.05,#####
            categorie_tendance_EBCC= catEBCC,
            categorie_tendance_EBCC_trim= overall$slope[1,8],
            vif = NA,vif_mean=NA,vif_max=NA,
            rho_serialcorrelation,overdispersion,
            model = val_model, changepoints = val_changepoints, serialcor = val_serialcor, overdisp = val_overdisp)




        txtPente <- paste(tabTrend1$tendance,
                          ifelse(tabTrend1$significatif," *",""),"  [",tabTrend1$IC_inferieur," , ",tabTrend1$IC_superieur,"]",
                          ifelse(tabTrend1$significatif,paste0("\n",ifelse(tabTrend1$pourcentage_variation>0,"+ ","- "),
                                                               round(abs(tabTrend1$pourcentage_variation))," [",tabTrend1$pourcentage_IC_inf," , ",tabTrend1$pourcentage_IC_sup,"] % en ",pasdetemps," ans",sep=""),""),sep="")



        tabTextPent <- data.table(x=ifelse(tabTrend1$pourcentage_variation>0,-Inf,Inf),
                                  text_hjust= ifelse(tabTrend1$pourcentage_variation>0,-0.1,1.1),
                                  txt=txtPente)




	dTrend <- rbind(dTrend,tabTrend1)
	dgg <- rbind(dgg,doo)#,tab2)
        dggSlope <- rbind(dggSlope,tab_trend)


        if(saveFig) {
            cat("\nFigure\n=======================\n")
            titre <- paste(nomSp," (",nomSc,")\n",firstYear ," - ",lastYear,sep="")


            gg <- ggplot(data = doo,aes(x=year,y=obs))
            gg <- gg + labs(y="",x="Année",title=titre)
            gg <- gg +  theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank())
            gg <- gg + geom_hline(yintercept=1,colour="white",size=2)
            gg <- gg + geom_ribbon(data = tab_trend,aes(ymin=CI_inf,ymax=CI_sup), colour=NA,fill="red",size=1.5,alpha=.2)
            gg <- gg + geom_line(data = tab_trend, colour="red",size=1.5,alpha=.5)
            gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),colour=NA,alpha=.2,fill="#3c47e0")
            gg <- gg + geom_pointrange(aes(ymin=CIinf,ymax=CIsup),alpha=.5,colour="#3c47e0")
            gg <- gg + geom_line(size = 1.2,alpha=.8,colour="#3c47e0")
            gg <- gg + geom_point(size = 2,colour="#3c47e0")
            gg <- gg + geom_text(data=tabTextPent, mapping=aes(x=x,y=Inf,label=txt,hjust=text_hjust),vjust=1.1,colour="black",parse=FALSE,fontface=2, size=2.5)

            gg

            figname <- paste0(repoutResult,sp,"_",id,".png")
            cat("\n  [PNG]",figname,"\n")
            flush.console()
            ggsave(figname,gg,width=6,height=4)
        }#END if(saveFig)

        cat("\n  [CSV]",file_trend,"\n")
        flush.console()
        write.csv(dTrend,file_trend,row.names=FALSE)
        cat("\n  [CSV]",file_An,"\n")
        flush.console()
        write.csv(dggSlope,file_An,row.names=FALSE)
        cat("\n  [CSV]",file_gg,"\n")
        flush.console()
        write.csv(dgg,file_gg,row.names=FALSE)

    }#END for(sp in listSp)


    cat("\n==============================\n  [CSV]",file_trend,"\n")
    flush.console()
    fwrite(dTrend,file_trend,sep="\t")
    cat("\n  [CSV]",file_trend,"\n")
    flush.console()
    fwrite(dggSlope,file_ggSlope,sep="\t")
    cat("\n  [CSV]",file_ggSlope,"\n")
    flush.console()
    fwrite(dgg,file_gg,sep="\t")
    cat("\n  [CSV]",file_ggSlope,"\n")
    flush.console()
}



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

