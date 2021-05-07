
load_pack <- function(vecPackage=c("data.table","ggplot2","rtrim"),theRepo = "https://pbil.univ-lyon1.fr/CRAN/") {
    ip <- installed.packages()[,1]

    for(p in vecPackage){
        if (!(p %in% ip))
            install.packages(pkgs=p,repos = theRepo,dependencies=TRUE)
        require(p,character.only=TRUE)
    }

}


load_pack()





var_abundance_trim <- function(id = NA, data,seuil_min=60,sp="",
                               col_count = "count",col_site = "site",col_year = "year",
                               val_model = 2, val_changepoints = 'all',
                               val_serialcor = TRUE, val_overdisp = TRUE,
                               fig_save = TRUE,fig = TRUE, fig_sp = "", fig_sub = "",
                               write = FALSE, repout = "output/",sufixOutput=NULL,output=TRUE,
                               printSummary = TRUE) {

    data.table::setDT(data)



    ## if necessary check the directories for the output
    if(write | fig_save) {
        dir.create(repout,showWarnings=FALSE)

        repout <- paste0(repout,id,"/")
        dir.create(repout,showWarnings=FALSE)
        repoutResult <- paste0(repout,"result_figure/")
        dir.create(repoutResult,showWarnings=FALSE)
        if(!is.null(sufixOutput)) sufixOutput <- paste0("_",sufixOutput)
        file_trend <- paste0(repout,id,sufixOutput,"_tendances.csv")
        file_An <- paste0(repout,id,sufixOutput,"_variations.csv")
        file_gg <- paste0(repout,id,sufixOutput,"_ggTable.csv")
        file_ggSlope <- paste0(repout,id,sufixOutput,"_ggSlopeTable.csv")
    }


    ## changing col names

    if(col_count != "count") setnames(data,col_count,"count")
    if(col_site != "site") setnames(data,col_site,"site")
    if(col_year != "year") setnames(data,col_year,"year")




    data <- data[,.(site,year,count)]

    sum_year <- aggregate(count~year, data=data, FUN = sum)
    sum_year <- sum_year[order(sum_year$year),]
    if(sum_year$count[1] == 0) {
        cat("Data starts with 1 years without positive observations.\n")
        cat("First year change\n")
        setDT(sum_year)
        sum_year <- sum_year[count>0,]
        first_year <- min(sum_year[,year])

        data <- data[year >= first_year,]
        cat("\n--> New first year:",first_year,"\n\n")
    }


    nb_data <- nrow(data[count>0,])
    cat("\nNb data:",nb_data,"\n")
    if(nb_data>seuil_min) {

    first_year <- min(data$year)
    last_year <- max(data$year)
    n.years <- last_year - first_year +1

    cat( "model =",val_model," changepoints =",val_changepoints,"\nserialcor =",val_serialcor," overdisp =", val_overdisp,"\n")

    fit <- trim(count ~ site + year, data = data, model = val_model, changepoints = val_changepoints, serialcor = val_serialcor, overdisp = val_overdisp)


    sumfit <- summary(fit)
    rho_serialcorrelation <- sumfit$serialcorrelation
    overdispersion <- sumfit$overdispersion

    if(rho_serialcorrelation < 1 | overdispersion < 1)
    {
        val_serialcor = rho_serialcorrelation > 1
        val_overdisp = overdispersion > 1


        cat( "\n--> nouveau model\nmodel =",val_model," changepoints =",val_changepoints,"\nserialcor =",val_serialcor," overdisp =", val_overdisp,"\n")

        fit <- trim(count ~ site + year , data = data, model = val_model, changepoints = val_changepoints, serialcor = val_serialcor, overdisp = val_overdisp)

    }

    overall <- overall(fit, which = "imputed")
    start <- overall$tt[1]
    intercept <- overall$intercept
    trend <- overall$slope

    alpha <- 0.05
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

    dSlopeCurve <- data.frame(id=id,id2=sufixOutput,sp=sp,years=years,year = seq(first_year,last_year,.1),
                            obs = trend.est /start, CI_inf = trend.lower/start,
                            CI_sup = trend.upper/start)



    oo <- overall
    oo$tt <- oo$tt/start
    oo$err <- oo$err/start
    oo$intercept$mul <- oo$intercept$mul / start
    oo$intercept$se_mul <- oo$intercept$se_mul / start
    oo$intercept$add <- log(overall$intercept$mul / start)
    oo$intercept$se_add <- log(overall$intercept$se_mul / start)

    dVariation <- setDT(data.frame(id=id,id2=sufixOutput,sp=sp,year = oo$timept,obs = oo$tt,err = oo$err))
    CI <- confint(fit)
    CI <- CI/start
    colnames(CI) <- c("CIinf","CIsup")
    dVariation <- cbind(dVariation,CI)

    pente <- round(oo$slope$mul,3)
    add_ICinf <- oo$slope$add - (1.96 * oo$slope$se_add)
    add_ICsup <- oo$slope$add + (1.96 * oo$slope$se_add)

    mul_ICinf <-  exp(add_ICinf)
    mul_ICsup <- exp(add_ICsup)


    pasdetemps <- last_year - first_year
    pourcent <- round(((oo$slope$mul^pasdetemps)-1)*100,1)
    pourcent_ICinf <- round(((mul_ICinf^pasdetemps)-1)*100,1)
    pourcent_ICsup <- round(((mul_ICsup^pasdetemps)-1)*100,1)
    pval <- overall$slope[1,7]

    catEBCC <- NA
    catEBCC <- affectCatEBCC(trend =pente,pVal = pval,ICinf= mul_ICinf,ICsup= mul_ICsup)

    dTrend <- data.frame(
        id=id,id2=sufixOutput,espece=sp,
        nombre_annees = pasdetemps+1,premiere_annee = first_year,derniere_annee = last_year,
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


    txtPente <- paste(dTrend$tendance,
                      ifelse(dTrend$significatif," *",""),"  [",dTrend$IC_inferieur," , ",dTrend$IC_superieur,"]",
                      ifelse(dTrend$significatif,paste0("\n",ifelse(dTrend$pourcentage_variation>0,"+ ","- "),
                                                           round(abs(dTrend$pourcentage_variation))," [",dTrend$pourcentage_IC_inf," , ",dTrend$pourcentage_IC_sup,"] % en ",pasdetemps," ans",sep=""),""),sep="")



    tabTextPent <- data.table(x=ifelse(dTrend$pourcentage_variation>0,-Inf,Inf),
                              text_hjust= ifelse(dTrend$pourcentage_variation>0,-0.1,1.1),
                              txt=txtPente)


    if(fig_save | fig) {
        cat("\nFigure\n=======================\n")
        titre <- paste0(fig_sp,"\n",first_year ," - ",last_year,sep="")


        gg <- ggplot(data = dVariation,aes(x=year,y=obs))
        gg <- gg + labs(y="",x="Année",title=titre,subtitle=fig_sub)
        gg <- gg +  theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank())
        gg <- gg + geom_hline(yintercept=1,colour="white",size=2)
        gg <- gg + geom_ribbon(data = dSlopeCurve,aes(ymin=CI_inf,ymax=CI_sup), colour=NA,fill="red",size=1.5,alpha=.2)
        gg <- gg + geom_line(data = dSlopeCurve, colour="red",size=1.5,alpha=.5)
        gg <- gg + geom_ribbon(aes(ymin=CIinf,ymax=CIsup),colour=NA,alpha=.2,fill="#3c47e0")
        gg <- gg + geom_pointrange(aes(ymin=CIinf,ymax=CIsup),alpha=.5,colour="#3c47e0")
        gg <- gg + geom_line(size = 1.2,alpha=.8,colour="#3c47e0")
        gg <- gg + geom_point(size = 2,colour="#3c47e0")
        gg <- gg + geom_text(data=tabTextPent, mapping=aes(x=x,y=Inf,label=txt,hjust=text_hjust),vjust=1.1,colour="black",parse=FALSE,fontface=2, size=2.5)

        if(fig) print(gg)

        figname <- paste0(repoutResult,sp,sufixOutput,"_",id,".png")
        cat("\n  [PNG]",figname,"\n")
        flush.console()
        ggsave(figname,gg,width=6,height=4)
    }#END if(saveFig)


    if(write){
        cat("\n==============================\n")
        cat("\n  [CSV]",file_trend,"\n")
        flush.console()
        write.csv(dTrend,file_trend,row.names=FALSE)
        cat("\n  [CSV]",file_An,"\n")
        flush.console()
        write.csv(dSlopeCurve,file_An,row.names=FALSE)
        cat("\n  [CSV]",file_gg,"\n")
        flush.console()
        write.csv(dVariation,file_gg,row.names=FALSE)
    }

    if(printSummary) print(sumfit)

    if(output){
        l.output <- list(dTrend,dSlopeCurve,dVariation)
        return(l.output)
    }
    }
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

