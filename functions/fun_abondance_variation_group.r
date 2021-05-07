library(data.table)
library(ggplot2)
library(scales)
library(psych)
## Analyse par groupe de specialisation à partir des resulats de variation d'abondance par especes
## id identifiant de la session
## ICfigureGroupeSp affichage des intervalles de confiances sur la figure
## correctionAbondanceNull correction des abondance NULL
analyseGroupe <- function(id=NA,ICfigureGroupeSp=TRUE,powerWeight=2,
					correctionAbondanceNull = 0.000001,   titre ="Variation de l'indicateur groupe de spécialisation",
          listGroupe=c("generaliste","milieux batis","milieux forestiers","milieux agricoles"),
          listCouleur=c("black","firebrick3","chartreuse4","orange")) {


# ----------------------------------

id="trend_2019_trim";ICfigureGroupeSp=FALSE;powerWeight=2;
					correctionAbondanceNull = 0.000001;   titre ="Variation de l'indicateur groupe de spécialisation";
          listGroupe=c("generaliste","milieux batis","milieux forestiers","milieux agricoles");
          listCouleur=c("black","firebrick3","chartreuse4","orange")


 ## listGroupe = "Montane"
 ## listCouleur = "darkgreen"
  col <- listCouleur
  names(col) <- listGroupe
    nameFile <- paste("output/",id,"/",id,"_ggTable.csv",sep="" )
    nameFileTrend <- paste("output/",id,"/",id,"_tendances.csv",sep="" )
    ## donnees variations d'abondance annuels
    donnees <-  fread(nameFile)
	## donnees tendences globales
    donneesTrend <- fread(nameFileTrend)
    donneesTrend <- subset(donneesTrend, select = c(espece,tendance))
	## table de reference espece
    tabsp <- fread("library/espece.csv")
    tabsp <- subset(tabsp, select= c(sp,nom,indicateur, specialisation))
#	donnees <- merge(donnees,donneesTrend,by="code_espece")
    donnees <- merge(donnees,tabsp,by="sp")
    ## table de correspondance de biais en fonction des medianes des occuerences

    donnees_spe <- donnees[specialisation != "non",]

    var_group <- setDT(aggregate(obs ~ specialisation + year,donnees_spe, FUN = geometric.mean))


    hist_group <- fread("library/historic_trend.csv")

    hist_group <- hist_group[specialisation != "toutes espèces",]
    hist_group <- hist_group[,c(2,1,3)]
    colnames(hist_group) <- colnames(var_group)

vecSpe <- unique(donnees_spe$specialisation)
    for(spe in 1:4){

        valRef <- hist_group[year == 2001 & specialisation == vecSpe[spe],obs]
          var_group[specialisation == vecSpe[spe],obs := obs * valRef]

}

    var_group <- rbind(hist_group,var_group[year>2001,])

        setDT(var_group)
    var_group[,nb_year := max(var_group$year)-min(var_group$year)]


    gg <- ggplot(data=var_group,aes(x=year,y=obs,group=specialisation,colour=specialisation))
    gg <- gg + geom_hline(aes(yintercept = 1), colour="white", alpha=1,size=1.2)
    gg <- gg + geom_line(size=1.5,alpha=0.7)
    gg <- gg +  ylab("") + xlab("Année")+ ggtitle(titre)
       gg <- gg + scale_colour_manual(values=col)
  gg <- gg + scale_x_continuous(breaks=pretty_breaks())
     gg <- gg +  theme(panel.grid.minor=element_blank(), panel.grid.major.y=element_blank())
print(gg)
    ggsave(paste0("output/",id,"/",id,"variation_group_hist.png"),gg,width=9,height=7)



  datasum <- data.frame(groupe=NULL,tendance=NULL,pourcentage_variation=NULL)
    for(spe in 1:4){
                                        # print(spe)
        subtab <- subset( var_group,specialisation==vecSpe[spe])
        if(nrow(subtab)>1) {
            sumlm <- summary(lm(obs~year,data=subtab))
            subdatasum <- data.frame(groupe = vecSpe[spe],
                                     tendance=round(sumlm$coefficients[2,1],3),
                                     pourcentage_variation=round(sumlm$coefficients[2,1]*(nrow(subtab)-1)*100,3),
                                     first_year = min(var_group$year) ,
                                     last_year = max(var_group$year),
                                     nb_year = max(var_group$year)-min(var_group$year))
            datasum <- rbind(datasum,subdatasum)

        }

    }


setDT(var_group)

   donnees_spe_agg <- donnees[specialisation != "non",]
donnees_spe_agg[specialisation != "generaliste",specialisation := "specialiste"]

    var_group_2 <- aggregate(obs ~ specialisation + year,donnees_spe_agg, FUN = geometric.mean)
    setDT(var_group_2)
    var_group_2[,nb_year := max(var_group_2$year)-min(var_group_2$year)]


    gg <- ggplot(data=var_group_2,aes(x=year,y=obs,group=specialisation,colour=specialisation))
    gg <- gg + geom_hline(aes(yintercept = 1), colour="white", alpha=1,size=1.2)
    gg <- gg + geom_line(size=1.5,alpha=0.7)
    gg <- gg +  ylab("") + xlab("Année")+ ggtitle(titre)
  ##     gg <- gg + scale_colour_manual(values=col)
  gg <- gg + scale_x_continuous(breaks=pretty_breaks())
     gg <- gg +  theme(panel.grid.minor=element_blank(), panel.grid.major.y=element_blank())
print(gg)
    ggsave(paste0("output/",id,"/",id,"variation_spe_gen.png"),gg,width=9,height=7)


    sumlm <- summary(lm(obs~year,data=var_group_2))


vecSpeAgg <- unique(donnees_spe_agg$specialisation)

      for(spe in 1:2){
                                        # print(spe)
          subtab <- subset( var_group_2,specialisation==vecSpeAgg[spe])
           sumlm <- summary(lm(obs~year,data=subtab))
            subdatasum <- data.frame(groupe=vecSpeAgg[spe],
                                     tendance=round(sumlm$coefficients[2,1],3),
                                     pourcentage_variation=round(sumlm$coefficients[2,1]*(nrow(subtab)-1)*100,3),
                                     first_year = min(var_group_2$year) ,
                                     last_year = max(var_group_2$year),
                                     nb_year = max(var_group_2$year)-min(var_group_2$year))
          datasum <- rbind(datasum,subdatasum)

          }


listExclu <- c("STRALU","ATHNOC","NUMARQ","ACTHYP","TADTAD","FULATR","TACRUF","PODCRI")
  donnees_all <- donnees[!(sp %in% listExclu),]
donnees_all[,specialisation := "toutes espèces"]



    hist_group <- fread("library/historic_trend.csv")

    var_group_3 <- setDT( aggregate(obs ~ specialisation + year,donnees_all, FUN = geometric.mean))

        hist_group <- hist_group[specialisation == "toutes espèces",]
    hist_group <- hist_group[,c(2,1,3)]
    colnames(hist_group) <- colnames(var_group_3)


    valRef <- hist_group[year == 2001 ,obs]

   var_group_3[,obs := obs * valRef]

   var_group_3 <- rbind(hist_group,var_group_3)
    setDT(var_group_3)
    var_group_3[,nb_year := max(var_group_3$year)-min(var_group_3$year)]

    gg <- ggplot(data=var_group_3,aes(x=year,y=obs,group=specialisation,colour=specialisation))
    gg <- gg + geom_hline(aes(yintercept = 1), colour="white", alpha=1,size=1.2)
    gg <- gg + geom_line(size=1.5,alpha=0.7)
    gg <- gg +  ylab("") + xlab("Année")+ ggtitle(titre)
  ##     gg <- gg + scale_colour_manual(values=col)
  gg <- gg + scale_x_continuous(breaks=pretty_breaks())
     gg <- gg +  theme(panel.grid.minor=element_blank(), panel.grid.major.y=element_blank())
print(gg)
    ggsave(paste0("output/",id,"/",id,"variation_122sp_hist.png"),gg,width=9,height=7)


sumlm <- summary(lm(obs~year,data=var_group_3))
            subdatasum <- data.frame(groupe="toutes_especes",
                                     tendance=round(sumlm$coefficients[2,1],3),
                                     pourcentage_variation=round(sumlm$coefficients[2,1]*(nrow(subtab)-1)*100,3),
                                     first_year = min(var_group_3$year) ,
                                     last_year = max(var_group_3$year),
                                     nb_year = max(var_group_3$year)-min(var_group_3$year))

            datasum <- rbind(datasum,subdatasum)


var_group_all <- rbind(rbind(var_group,var_group_2),var_group_3)




write.csv2(datasum,paste0("output/",id,"/",id,"_tendance_group_hist.csv"))
    write.csv2(var_group_all,paste0("output/",id,"/",id,"_variation_group_hist.csv"))



































    grpe <- donnees$specialisation

	## recherche d'un maximum
    ff <- function(x,y) max(which(y<=x))
    ## poids du à l'incertitude
    IncertW <- ifelse(donnees$valide=="Incertain",tBiais$biais[sapply(as.vector(donnees$mediane_occurrence),ff,y=tBiais$occurrenceMed)],1)
	## poids du à la qualité de l'estimation
 #   erreur_stW <- 1/((donnees$erreur_st+1)^powerWeight)
#	erreur_stW <- ifelse( is.na(donnees$IC_superieur),0,erreur_stW)
	erreur_stW <- ifelse(is.na(donnees$IC_superieur),0,1)
	## poids total
	W <- IncertW * erreur_stW

	## variable de regroupement pour les calculs
    grAn <- paste(donnees$specialisation,donnees$annee,sep="_")
	## data frame pour le calcul
    dd <- data.frame(grAn,annee = donnees$annee, grpe,W,ab=donnees$abondance_relative,ICinf= donnees$IC_inferieur, ICsup= ifelse(is.na(donnees$IC_superieur),10000,donnees$IC_superieur))
	## table resumer de tous les poids
	ddd <- data.frame(code_espece = donnees$code_espece,nom_espece = donnees$nom_espece,annee = donnees$annee,
		groupe_indicateur = grpe,
		poids_erreur_standard = round(erreur_stW,3), poids_incertitude = round(IncertW,3),poids_final = round(W,3),
		abondance_relative=donnees$abondance_relative,
		IC_inferieur= donnees$IC_inferieur,
		IC_superieur= ifelse(is.na(donnees$IC_superieur),10000,donnees$IC_superieur),
		valide = donnees$valide, mediane_occurrence = donnees$mediane_occurrence)

	nomFileResum <- paste("Resultats/",id,"/donneesGroupes_",id,
                          ".csv",sep="" )
	write.csv2(ddd,nomFileResum,row.names=FALSE)
	cat(" <--",nomFileResum,"\n")

	## calcul des moyennes pondéré par groupe par an et pour les estimates et les IC
	for(j in 5:7) dd[,j] <- ifelse(dd[,j]==0,correctionAbondanceNull,dd[,j])
    ag <- apply(dd[,5:7], 2,
                function(x) {
                    sapply(split(data.frame(dd[,1:4], x), dd$grAn),
                           function(y) round(geometriqueWeighted(y[,5], w = y$W),3))
                })
	gg <- subset(dd,as.character(dd$grAn)=="milieux forestier_2014")  #############################################################

    ag <- ifelse(is.na(ag),1,ag)
    ag <- as.data.frame(ag)
    ag$grAn <-  rownames(ag)
	dbon <- subset(donnees,valide=="bon")
    dIncert <- subset(donnees,valide=="Incertain")
	## calcul nombre d'espece "bonne" pour le calcul
    bon <- tapply(dbon$nom,dbon$specialisation,FUN=function(X)length(unique(X)) )
    bon <- ifelse(is.na(bon),0,bon)
    tbon <- data.frame(groupe=names(bon),bon)
	## calcul nombre d'especes "incertaines" pour le calcul
    Incert <- tapply(dIncert$nom,dIncert$specialisation,FUN=function(X)length(unique(X)) )
    Incert <- ifelse(is.na(Incert),0,Incert)
    tIncert <- data.frame(groupe=names(Incert),Incertain=Incert)

    tIncert <- merge(tIncert,tbon,by="groupe")

	## table de resultat
    da <- merge(unique(dd[,1:3]),ag,by="grAn")[,-1]
    colnames(da) <- c("annee","groupe","abondance_relative","IC_inferieur","IC_superieur")

	da$annee <- as.numeric(da$annee)
    da <-  merge(da,tIncert,by="groupe")
    da <- subset(da, groupe != "non")
		colnames(da)[6:7] <-  c("nombre_especes_incertaines","nombre_espece_bonnes")
	a <- data.frame(id,da)
    write.csv2(da,file=nameFileSpe,row.names=FALSE,quote=FALSE)

      cat(" <--",nameFileSpe,"\n")
    yearsrange <- c(min(da$annee),max(da$annee))

	## figure par ggplot2

    p <- ggplot(data = da, mapping = aes(x = annee, y = abondance_relative, colour=groupe,fill=groupe))
    p <- p + geom_hline(aes(yintercept = 1), colour="white", alpha=1,size=1.2)
	if(ICfigureGroupeSp)
    p <- p + geom_ribbon(mapping=aes(y=abondance_relative,ymin=IC_inferieur,ymax=IC_superieur),linetype=2,alpha=.1,size=0.1)
    p <- p + geom_line(size=1.5)
    p <- p +  ylab("") + xlab("Année")+ ggtitle(titre)
    p <- p + scale_colour_manual(values=col, name = "" ,
                                 breaks = names(col))+
                                     scale_x_continuous(breaks=unique(da$annee))
    p <- p +  scale_fill_manual(values=col, name = "" ,
                                breaks = names(col))
    p <- p +  theme(panel.grid.minor=element_blank(), panel.grid.major.y=element_blank())
    ggsave(nameFileSpepng, p,width=17,height=10,units="cm")

 #   cat(" <==",nameFileSpepng,"\n")

	## calul pour chaque groupe une pente de regression de la variation d'abondance
    vecSpe <- unique(da$groupe)
    datasum <- data.frame(groupe=NULL,tendance=NULL,pourcentage_variation=NULL)
    for(spe in 1:4){
                                        # print(spe)
        subtab <- subset(da,groupe==vecSpe[spe])
        if(nrow(subtab)>1) {
            sumlm <- summary(lm(abondance_relative~annee,data=subtab))
            subdatasum <- data.frame(groupe=vecSpe[spe],
                                     tendance=round(sumlm$coefficients[2,1],3),
                                     pourcentage_variation=round(sumlm$coefficients[2,1]*(nrow(subtab)-1)*100,3))
            datasum <- rbind(datasum,subdatasum)

        }

    }
    datasum <- merge(datasum,tIncert,by="groupe")
    datasum <- data.frame(id,datasum)
     #datasum$cat_tendance_EBCC <- affectCatEBCC(trend,pVal,ICinf,ICsup
    namefilesum <- paste("Resultats/",id,"/tendancesGlobalesGroupes_",id,
                         ".csv",sep="" )
    write.csv2(datasum,file=namefilesum,row.names=FALSE,quote=FALSE)
     cat(" <--",namefilesum,"\n")
}




test <- function() {
library(psych)
    d <- NULL
    for(i in 1:10)
        d <- cbind(d,rnorm(100,rpois(1,4),abs(rnorm(1,2,1))))
    d <-abs(round(d*100))
    dd <- apply(d,2,quantile,c(0.025,0.5,0.975))

    val <- geometric.mean(dd[2,])
    ICinf <-  geometric.mean(dd[1,])
    ICsup <- geometric.mean(dd[3,])

    t1 <- c(ICinf,val,ICsup)


    vec <- apply(d,1,geometric.mean)
    t2 <- quantile(vec,c(0.025,0.5,0.975))
}
