

# Calcul de tendance et de variation d'abondance 

Romain Lorrilliere & Yves Bas


fonction g�n�rique de calcul de tendance et de variation
d'abondance 


## sourcer le script
```R
source("main_abundance_variation.r")
```


 
 
## la fonction de calcul 

```R
main_abundance_variation()
```

### Les param�tres

- __id__ identifiant du batch de calcul utilis� pour les noms des fichiers de sortie, par defaut la date et l'heure au format(YYYYMDD-HHHMM)
- __file_data__ nom du fichier de donn�es
- __list_effects__ vecteur des effets du mod�le stat, par defaut "year"
- __as_factor_other_than_timestep__ vecteur des effects en facteur en plus de l'effet temps (souvent ann�e) par defaut NULL, peut �tre "passage"
- __formula_random_effect__ formule de l'effet random par defaut  "+(1|site) + (1|year)". si pas d'effet "+1"
- __first_year__ filtre des donn�es premi�re ann�e, si NULL plus petite ann�e du jeux de donn�es, par defaut NULL
- __last_year__ filtre des donn�es derni�re ann�es, si NULL plus grande ann�es du jeux de donn�es, par defaut NULL
- __vecSp__ filtre des donn�es, vecteur des esp�ces conserv�e pour l'analyse. Les esp�ces doivent �tre au format des donn�es pr�sent dans la colonne d_species_colname, par defaut NULL
- __d_species_colname__ nom de la colonne avec les identifiant des esp�ces, par defaut "id_sp"
- __d_observed_var_colname__ nom de la colonne de la variable observ�e (nombre, abondance...), par defaut "obs"
- __d_timestep_colname__ nom de la colonne de temps (ann��), par defaut "year"
- __species_file_name__ nom du fichier de la table de r�f�rence des nom d'esp�ce, pas utilis� si maquant
- __dsp_species_colname__ nom de la cololnne des identifiants des espces dans la table des esp�ces. Les identifiants doivent �tre correspondre � ceux de la table des donn�es, par defaut "id_sp"
- __dsp_species_name_colname__ nom de la colonne avec le nom des esp�ces, par defaut "name"
- __repinput__ nom du dossier o� sont les data, par defaut "data/"
- __repout__ nom du dossier dans le quel les r�sultats seront enregistr�s, par defaut "output/"
- __data_file_encoding__ encodage du fichier de donn�es par defaut "Latin-1" peut �tre "UTF-8"
- __species_file_encoding__ encodage du fichier des esp�ces par defaut "Latin-1" peut �tre "UTF-8"
- __printSummary__ TRUE FALSE d'affichage des summaries des mod�les,
  par defaut TRUE
- __saveFig__ TRUE FALSE sauvegarde des figures des tendances,
  par defaut TRUE
- __saveFigGlmm__ TRUE FALSE sauvegarde des figures informatives des mod�les,
  par defaut TRUE




### Exemples

mod�le : 

Mod�le classique

obs~1+year_as_factor Family: nbinom2  ( log )

```R
main_abundance_variation(id = "sans_effet_nb_passage", file_data = "Essai_analyse_categorie_passage.txt", list_effects= c("annee"), as_factor_other_than_timestep=NULL, formula_random_effect= "+(1|carre) + (1|annee)", first_year = 2003,last_year=NULL, vecSp =NULL, d_species_colname = "espece",d_observed_var_colname = "nombre", d_timestep_colname = "annee", species_file_name = "library/espece.csv", dsp_species_colname = "sp",dsp_species_name_colname = "nom")
```

Mod�le prenant un compte un effet du nombre de passage

obs~1+year+passage_as_factor Family: nbinom2  ( log )

```R
main_abundance_variation(id="effet_nb_passage",file_data = "Essai_analyse_categorie_passage.txt", list_effects= c("annee","passage"), as_factor_other_than_timestep=c("passage"), formula_random_effect= "+(1|carre) + (1|annee)", first_year = 2003,last_year=NULL, vecSp = "PHYCOL", d_species_colname = "espece",d_observed_var_colname = "nombre", d_timestep_colname = "annee", species_file_name = "library/espece.csv", dsp_species_colname = "sp",dsp_species_name_colname = "nom")
```





## Version "V.1.1_2020-12-07"



 
