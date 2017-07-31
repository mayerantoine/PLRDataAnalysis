

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


###############
# if Patientdecede is 0 does that mean patient is alive ? same for PatientRetrouve,Retournealaclinique
# PatientIntrouvable is updated how?
# How to know a patient is active / how do you calculate if a patient is active?
# patient a relancer et visite en cours where are does data
# explain PatientRefuse et patientensuiviailleurs
# it seems derniere visite is update across all follow up
# Pourqui pas de trouvaille remplit dans plusieurs visites
# Auto-stigmatisation, Voyage inter-urbain, 

########

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

LTFU_max_date$PatientDecede_New <- ifelse(is.na(LTFU_max_date$PatientDecede) & LTFU_max_date$PatientRetrouve == 1 , 0 ,
                                    LTFU_max_date$PatientDecede)

LTFU_max_date$vitalstatus <- ifelse( is.na(LTFU_max_date$PatientDecede_New) , "Unknown", 
                                    ifelse(LTFU_max_date$PatientDecede_New == 0, "Alive","Dead") )

LTFU_max_date$outcomestatus <- ifelse(LTFU_max_date$vitalstatus == "Unknown","No outcome found","Outcome found")

LTFU_max_date$PatientRetrouve_New <- ifelse(LTFU_max_date$PatientRetrouve == 0, "Not found", 
                                            ifelse(LTFU_max_date$vitalstatus == "Dead","Not found","Found"))


LTFU_max_date$PatientRetourneALaClinique_New <- ifelse(LTFU_max_date$PatientRetourneALaClinique == 0, "LTFU",
                                                       "Back in Care") 
                                            

LTFU_Flow <- LTFU_max_date %>%  filter(Institution == "Hôpital Immaculée Conception des Cayes") %>% 
    group_by(outcomestatus,vitalstatus,PatientRetrouve_New,PatientRetourneALaClinique_New) %>% summarise(n=n())


LTFU_Ins <- LTFU_max_date %>% 
    filter(Institution == "Hôpital Immaculée Conception des Cayes") %>% 
    group_by(Institution) %>%
    summarise(n=n())
 


LTFU_Ins <- plr %>% 
    filter(Institution == "Hôpital Immaculée Conception des Cayes") %>% 
    group_by(Institution) %>%
    summarise(n= n_distinct(id_patient))


write_csv(LTFU,"LTFU.csv")
# write_csv(LTFU_max_date,"LTFU_max_date.csv")
# write_csv(LTFU_Flow, "LTFU_Flow.csv")

###########################

findings <- c("FraisTransportNonDisponible","EtatTropMalade","Oubli",
              "RechercheSoinsAlternatifs","PeurDEtreVuDansUnSiteVIH","ServicesInsatisfaisant",
              "Voyage","Migration","Stigmatisation","OccupationOuManquedeTemps","DecesDansLaFamille",
              "BesoinDeTransfert","PatientSuiviAilleurs","PatientDecede")

tb_findings <- LTFU %>%  select(id_patient,typesuivi,datesuivieffectue,sexe,
                                datenaissance,Institution, one_of(findings))


tb_findings <- tb_findings %>% filter(Institution == "Hôpital Immaculée Conception des Cayes") %>%
    gather("findings","Status",7:19) %>% filter(!is.na(Status), Status ==1) 

tb_findings %>%  group_by(findings) %>% summarise(n =n())
