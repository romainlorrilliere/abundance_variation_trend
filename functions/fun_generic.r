
load_pack <- function(vecPackage=c("RODBC","dplyr","data.table","rgdal","lubridate","RPostgreSQL","doBy","reshape2","sf","maptools","maps","animation","dplyr","data.table"),theRepo = "https://pbil.univ-lyon1.fr/CRAN/") {
ip <- installed.packages()[,1]

for(p in vecPackage){
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = theRepo,dependencies=TRUE)
    require(p,character.only=TRUE)
}

}

