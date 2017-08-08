

library(tidyverse)
library(lubridate)

# How DateDeRetourALaClinique can be useful
# What is the relation between DerniereVisite and DatedeRetour
# What is the difference between PatientNonRetrouve et PatientIntrouvable

# observationvisite is NA for visite, no documentation , patient can be PatientRetrouve

# patient can have both 0 and 1 that why n_distinct is duplicated
# patient can have both appel and visite

# Flow of outcomes
# Top reason for patient not coming back to the clinic by TypeRelance, and by TypeSuivi, by Age, Sex, Site
# from tracked by cohort how many came back after x months

###############
# if Patientdecede is 0 does that mean patient is alive ? same for PatientRetrouve,Retournealaclinique
# PatientIntrouvable is updated how?
# How to know a patient is active / how do you calculate if a patient is active?
# patient a relancer et visite en cours where are does data
# explain PatientRefuse et patientensuiviailleurs
# it seems derniere visite is update across all follow up
# Pourquoi pas de trouvaille remplit dans plusieurs visites
# Auto-stigmatisation, Voyage inter-urbain, 
# how did you calulate re-lost
# Date de retour inferieur a date follow up
###############

# NULL and zero is the same
# Replace datesuvivie with fichecreele when necessary
# we dont know when alive
# fiche incomplet that's why ParientRetournelaClinique NULL
## look up which denierevisite to use ?? CHT vs EMR


#########
rm(list = ls())
rawdata <- read_csv("plr_072017.csv", na=c("NULL","NA"))
names(rawdata) <- make.names(names(rawdata))
rawdata$datesuivieffectue <- mdy(rawdata$datesuivieffectue)
rawdata$DateRetourALaClinique <- mdy(rawdata$DateRetourALaClinique)
rawdata$datenaissance <- mdy(rawdata$datenaissance)
rawdata$FicheCrééeLe <- mdy(rawdata$FicheCrééeLe)
rawdata$Derniere.visite <- mdy(rawdata$Derniere.visite)

rawdata <- cbind(id= as.numeric(rownames(rawdata)),rawdata)

plr <- filter(rawdata, year(datesuivieffectue) >= 2015)

plr$Trouvailles <- as.integer(plr$FraisTransportNonDisponible |
                                  plr$EtatTropMalade|
                                  plr$Oubli |
                                  plr$RechercheSoinsAlternatifs |
                                  plr$PeurDEtreVuDansUnSiteVIH |
                                  plr$ServicesInsatisfaisant |
                                  plr$Voyage | plr$Migration | plr$Stigmatisation |
                                  plr$OccupationOuManquedeTemps |
                                  plr$DecesDansLaFamille |
                                  plr$BesoinDeTransfert)

plr$TypeRelanceNew <- "NA"
plr[grep("Communautaire",plr$TypeRelance),]$TypeRelanceNew <- "DAC"
plr[grep("perdu",plr$TypeRelance),]$TypeRelanceNew <- "LTFU"
plr[grep("Rendez-vous",plr$TypeRelance),]$TypeRelanceNew <- "Missed_RDV"
plr[grep("Verification",plr$TypeRelance),]$TypeRelanceNew <- "Geolocate"
plr[grep("VIH+",plr$TypeRelance),]$TypeRelanceNew <- "HIV_POS"
plr[grep("PréARV",plr$TypeRelance),]$TypeRelanceNew <- "PreART"

LTFU <- plr %>% filter(TypeRelanceNew == "LTFU")
by_patient <- LTFU %>% group_by(id_patient) %>% summarise(nb=n())

LTFU_max_date <- LTFU %>% group_by(id_patient) %>% slice( which.max(datesuivieffectue))


LTFU_max_date$PatientContacte <- "Contacted"
LTFU_max_date$PatientDecede_New <- ifelse(is.na(LTFU_max_date$PatientDecede) & LTFU_max_date$PatientRetrouve == 1 , 0 ,
                                   LTFU_max_date$PatientDecede)
LTFU_max_date$vitalstatus <- ifelse( is.na(LTFU_max_date$PatientDecede_New) , "Unknown", 
                                    ifelse(LTFU_max_date$PatientDecede_New == 0, "Alive","Dead") )
#LTFU_max_date$outcomestatus <- ifelse(LTFU_max_date$vitalstatus == "Unknown","No outcome found","Outcome found")


LTFU_max_date$PatientRetrouve_New <- ifelse(LTFU_max_date$vitalstatus == "Dead","Found",
                                            ifelse(LTFU_max_date$PatientRetrouve == 0, "Not found","Found"))


LTFU_max_date$PatientRetourneALaClinique_New <- ifelse(LTFU_max_date$PatientRetourneALaClinique == 0 |
                                             is.na(LTFU_max_date$PatientRetourneALaClinique) |LTFU_max_date$vitalstatus == "Dead"
                                             , "LTFU", "Back in Care") 
 
# from janv 2015 to date
LTFU_Flow <- LTFU_max_date %>% 
    group_by(sexe,PatientRetrouve_New) %>%
    summarise(nb=n()) %>% spread(PatientRetrouve_New,nb) 

start_date <- ymd("2016-09-01")
end_date <- ymd("2017-06-30")

# from oct 2016 to june 2017
LTFU_Flow_oct2016 <- LTFU_max_date %>%  filter(datesuivieffectue >=start_date, datesuivieffectue <= end_date )  %>%
    group_by(PatientContacte,PatientRetrouve_New,vitalstatus,PatientRetourneALaClinique_New) %>% 
        summarise(n=n())



LTFU_Ins <- LTFU_max_date %>% 
    group_by(Institution) %>%
    summarise(n=n())
 

LTFU_Ins <- plr %>% filter(TypeRelanceNew == "LTFU") %>%
    group_by(Institution) %>%
    summarise(n= n_distinct(id_patient))


write_csv(LTFU,"LTFU.csv")
# write_csv(LTFU_max_date,"LTFU_max_date.csv")
# write_csv(LTFU_Flow, "LTFU_Flow_oct.csv")

###########################

findings <- c("FraisTransportNonDisponible","EtatTropMalade","Oubli",
              "RechercheSoinsAlternatifs","PeurDEtreVuDansUnSiteVIH","ServicesInsatisfaisant",
              "Voyage","Migration","Stigmatisation","OccupationOuManquedeTemps","DecesDansLaFamille",
              "BesoinDeTransfert","PatientSuiviAilleurs","PatientDecede")

tb_findings <- LTFU %>%  select(id_patient,typesuivi,datesuivieffectue,sexe,
                                datenaissance,Institution, one_of(findings))


tb_findings <- tb_findings %>% 
    gather("findings","Status",7:20) %>% filter(!is.na(Status), Status ==1) 

tb_findings %>%  group_by(findings) %>% summarise(n =n())

#######################

start_date <- ymd("2016-09-01")
end_date <- ymd("2017-06-30")

oct_june2017 <- LTFU_max_date %>% 
                filter(datesuivieffectue >=start_date, datesuivieffectue <= end_date ) 

oct_june2017$delaiRetour <- oct_june2017$DateRetourALaClinique - oct_june2017$datesuivieffectue
oct_june2017$delaiRetourFiche <- oct_june2017$DateRetourALaClinique - oct_june2017$FicheCrééeLe
oct_june2017$delaiLastVisit <- oct_june2017$Derniere.visite - oct_june2017$datesuivieffectue

delai <- oct_june2017 %>% 
    select(id_patient,datesuivieffectue,DateRetourALaClinique,FicheCrééeLe ,typesuivi,
           PatientRetourneALaClinique,Derniere.visite,delaiRetour,delaiLastVisit) %>%
    filter(delaiRetour <0)

delai$delaiRetourNbMois <- round(delai$delaiRetour/30,0)

delai$delaiRetourNbMoisLabel <- paste("Mois",delai$delaiRetourNbMois,sep= "")

delai$delaiRetourNbMoisFlag <- 1

delai <- spread(delai,delaiRetourNbMoisLabel,delaiRetourNbMoisFlag)

LTFU_retour <- delai %>% 
    group_by(year(datesuivieffectue),month(datesuivieffectue)) %>%
    summarise(n=n_distinct(id_patient),
              Mois0= sum(Mois0,na.rm = T),
              Mois1= sum(Mois1,na.rm=T),
              Mois2= sum(Mois2,na.rm = T),
              Mois3= sum(Mois3,na.rm =T),
              Mois4= sum(Mois4,na.rm = T),
              Mois5= sum(Mois5,na.rm = T),
              Mois6= sum(Mois6,na.rm = T),
              Mois7= sum(Mois7,na.rm = T),
              Mois8= sum(Mois8,na.rm = T))

