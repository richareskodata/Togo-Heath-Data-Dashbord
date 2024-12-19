# Importation de la base choléra
setwd("C:/Users/dzidzinyok/OneDrive - World Health Organization/RICHARD DZIDZINYO/R/TEST SHINY")
path_cholera <- "C:/Users/dzidzinyok/OneDrive - World Health Organization/RICHARD DZIDZINYO/WHO 2023/EPR/Surveillance/SITUATION EPIDEMIOLOGIQUE/CHOLERA/Liste linéaire Choléra_Togo.xlsx"
Data_cholera <- read_excel(path_cholera, sheet = "Liste Linéaire_Togo",
                           col_types = c("numeric","text","numeric","text","numeric","text","text",
                                         "text","text","numeric", "numeric","text","text","text",
                                         "text","date", "text","date","text","text","text","text","text",
                                         "text","text","text","text","text","text","text","text","text",
                                         "text","text","date","text","text","text","text","text","text",
                                         "text"))
Data_cholera$Datenitif <- as.Date(Data_cholera$`Date de consultation`)
Data_cholera$Dateonset <- as.Date(Data_cholera$`Date de début des signes`)
Data_cholera$Dateout <- as.Date(Data_cholera$`Date de Sortie`)
Data_cholera$age <- as.integer(Data_cholera$`Age (année)`)
Data_cholera$Week <- isoweek(Data_cholera$`Date de début des signes`)
Data_cholera$Issue <- Data_cholera$`Mode de sortie (Guéri/Référé/dcd)`
Data_cholera$Classification <- Data_cholera$`Classification finale (Suspect/Probable/Confirmé)`

Data_cholera$n <- 1
## Recodage de Data_cholera$`Tranche d'age`
Data_cholera$`Tranche d'age` <- Data_cholera$`Tranche d'age` %>%
  fct_recode(
    "[0-2[" = "[0-2]",
    "[15-45[" = "[15-44]",
    "[2-5[" = "[2-4]",
    "[45-60[" = "[45-59]",
    "[5-15[" = "[5-14]",
    "[60 +[" = "[60 et plus]"
  )

## Réordonnancement de Data_cholera$`Tranche d'age` en Data_cholera$Tranche_age
Data_cholera$Tranche_age <- Data_cholera$`Tranche d'age` %>%
  fct_relevel(
    "[0-2[", "[2-5[", "[5-15[", "[15-45[", "[45-60[", "[60 +["
  )

Data_cholera$Tranche_age <- Data_cholera$`Tranche d'age`
Data_cholera$Région <- Data_cholera$Région %>% 
  fct_recode("Maritime"="MARITIME")
## Recodage de Data_cholera$`Principale source d’eau de boisson` en Data_cholera$`Principale source d’eau de boisson_rec`
Data_cholera$source_eau <- Data_cholera$`Principale source d’eau de boisson` %>%
  fct_recode(
    "Puits" = "eau de puits",
    "Puits" = "Eau de puits",
    "Rivière" = "Eau de rivière",
    "Robinet" = "eau de robinet",
    "Robinet" = "Eau de robinet, Eau de puits",
    "Robinet" = "eau de robonet",
    "Bouteille" = "Eau en bouteille",
    "Sachet" = "Eau en sachet",
    "Robinet" = "eu de robinet",
    "Forage" = "Forage/Puits",
    NULL = "NION",
    NULL = "Non",
    NULL = "NON",
    NULL = "Oui",
    NULL = "OUI",
    "Puits" = "puits",
    "Puits" = "puits peu profod; eau en bouteille",
    "Forage" = "Tde/Forage",
    "Robinet" = "TdE+Eau de fleuve"
  )
## Recodage de Data_cholera$Profession
Data_cholera$Profession <- Data_cholera$Profession %>%
  fct_recode(
    "Electronicien" = "APPRENTI ELECTRICIEN",
    "autres" = "APPRENTI HERBORISTE",
    "Maçon" = "APPRENTI MACON",
    "autres" = "Boulangère",
    "autres" = "CHARCUTIER",
    "Chauffeur" = "CHAUFFEUR",
    "Coiffure" = "COIFFEUR",
    "Coiffure" = "COIFFEUSE",
    "Coiffure" = "Coiffeuse/ménagère",
    "Commerçant(e)" = "Commerçant",
    "Commerçant(e)" = "Commercante",
    "autres" = "Condusteur Tricycle",
    "Couture" = "COUTURIERE",
    "Couture" = "Couturière",
    "Cultivateur" = "CULTIVATEUR",
    "Cultivateur" = "Cultivatrice",
    "autres" = "DESSINATEUR BATIMENT",
    "Pêcheur" = "Docker à l'ancien port de pêche",
    "Elève" = "ECOLIER",
    "Elève" = "ECOLIERE",
    "Electronicien" = "ELECTRO TECHNICIEN",
    "Elève" = "Eleve",
    "Elève" = "ELEVE",
    "Enfant" = "Enfant (moins de 4ans)",
    "autres" = "ENSEIGNANT",
    "Etudiant" = "ETUDIANT",
    "Etudiant" = "Etudiante",
    "autres" = "MASSEUSE",
    "autres" = "MECANICIEN AUTO",
    "Ménagère" = "MENAGERE",
    "Menusier" = "MENUISIER",
    "Menusier" = "MENUISIER ALU",
    "retraité" = "Militaire retraité",
    "N/A" = "NA",
    "autres" = "OUVRIER",
    "Pêcheur" = "PECHEUR",
    "Pêcheur" = "pêcheur",
    "Pêcheur" = "PECHEUSE",
    "retraité" = "RETRAITE",
    "Commerçant(e)" = "revendeur",
    "Commerçant(e)" = "Revendeur",
    "Commerçant(e)" = "REVENDEUR",
    "Commerçant(e)" = "Revendeur/se",
    "Commerçant(e)" = "Revendeuse",
    "Commerçant(e)" = "REVENDEUSE",
    "autres" = "SOUDEUR",
    "autres" = "TAXI MOTO",
    "Coiffure" = "Tresse",
    "autres" = "DECLARANT EN DOUANE",
    "autres" = "JOURNALISTE",
    "Maçon" = "MACON",
    "autres" = "Soudeur",
    "autres" = "Coursier",
    "autres" = "Informaticien",
    "autres" = "SANS PROFESSION",
    "autres" = "Cuisinier",
    "Menusier" = "Menuisier Alu",
    "autres" = "Humanitaire",
    "Enfant" = "NA (Enfant)"
  ) %>%
  fct_explicit_na("N/A")


write.table(Data_cholera, "Liste_lineaire", fileEncoding = "UTF-8")

