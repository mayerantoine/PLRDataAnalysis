
library(tidyverse)
library(lubridate)

# How DateDeRetourALaClinique can be useful
# What is the relation between DerniereVisite and DatedeRetour
# How to know a patient is active
# What is the difference between PatientNonRetrouve et PatientIntrouvable

# patient can have both 0 and 1 that why n_distinct is duplicated
# patient can have both appel and visite

# Flow of outcomes
# Top reason for patient not coming back to the clinic by TypeRelance, and by TypeSuivi, by Age, Sex, Site


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

no_findings <- filter(plr, is.na(hasfindings) | hasfindings == 0)
with_findings <- filter(plr,  hasfindings == 1)


plr %>% filter(Institution == "Hôpital Universitaire la Paix") %>% 
    group_by(TypeRelanceNew) %>%
    summarise( n= n(), npatient = n_distinct(id_patient))

plr_nbsuivi <- plr %>% group_by(Institution) %>% summarise(nbsuivi=n())
plr_nbpatient <- plr  %>%  group_by(Institution) %>% summarise(n_distinct(id_patient))
by_patient <- plr %>% group_by(id_patient) %>% summarise(nb=n())


findings <- c("PatientDecede","FraisTransportNonDisponible","EtatTropMalade","Oubli",
            "RechercheSoinsAlternatifs","PeurDEtreVuDansUnSiteVIH","ServicesInsatisfaisant",
            "Voyage","Migration","Stigmatisation","OccupationOuManquedeTemps","DecesDansLaFamille",
            "BesoinDeTransfert")


plr_reason <- plr %>% select(id,PatientSuiviAilleurs,
                             PatientDecede,FraisTransportNonDisponible,EtatTropMalade,Oubli,
                             RechercheSoinsAlternatifs,PeurDEtreVuDansUnSiteVIH,ServicesInsatisfaisant,
                             Voyage,Migration,Stigmatisation,OccupationOuManquedeTemps,DecesDansLaFamille,
                             BesoinDeTransfert)
                                                              
plr_reason_tidy <- plr_reason %>% gather("findings","Status",2:15) %>% 
                                filter(!is.na(Status), Status ==1) 

n_typereason <- plr_reason_tidy %>%  group_by(TypeRaison) %>% summarise(count = n_distinct(id))


### Flow outcome LTFU

plr_outcome <- plr %>% select(id,id_patient,sexe,datenaissance,datesuivieffectue,DateRetourALaClinique,
               typesuivi,Institution,TypeRelanceNew,observationvisite, visitegéolocalisee,PatientRetrouve,
               PatientRefuse,PatientRetourneALaClinique, PatientDecede,Trouvailles) %>% 
               filter(TypeRelanceNew == "LTFU")

plr_outcome$PatientRetrouve <- as.logical(plr_outcome$PatientRetrouve)
plr_outcome$visitegéolocalisee <- as.logical(plr_outcome$visitegéolocalisee)
plr_outcome$PatientRefuse <- as.logical(plr_outcome$PatientRefuse)
plr_outcome$PatientRetourneALaClinique <- as.logical(plr_outcome$PatientRetourneALaClinique)
plr_outcome$PatientDecede <- as.logical(plr_outcome$PatientDecede)
plr_outcome$Trouvailles <- as.logical(plr_outcome$Trouvailles)


plr_outcome$VitalStatus <- ifelse(plr_outcome$PatientRetrouve,
                                          ifelse(plr_outcome$PatientDecede,"Decede",ifelse(is.na(plr_outcome$PatientDecede),"Vivant","Vivant")),
                                          ifelse(plr_outcome$PatientDecede,"Decede",ifelse(is.na(plr_outcome$PatientDecede),NA,"Vivant")))

plr_outcome$OutcomeStatus <- ifelse(plr_outcome$VitalStatus == "Vivant" | plr_outcome$VitalStatus == "Decede","Outcome","No Outcome")
plr_outcome$OutcomeStatus <- ifelse(is.na(plr_outcome$VitalStatus),"No Outcome",plr_outcome$OutcomeStatus )

write_csv(plr_outcome,"plr_outcome.csv")


n_distinct(plr_outcome$id_patient)

plr_outcome %>% group_by(OutcomeStatus) %>% 
    summarise(n = n_distinct(id_patient)) %>% write_csv("outcome2.csv")

plr_outcome %>% group_by(typesuivi,TypeRelanceNew) %>% 
           summarise(n = n_distinct(id_patient)) %>%
             spread("TypeRelanceNew", n)


plr_outcome %>% group_by(typesuivi,TypeRelanceNew) %>% 
    summarise(n = n_distinct(Institution)) %>%
    spread("TypeRelanceNew", n)

plr_outcome_patient <- plr_outcome %>% group_by(id_patient) %>% 
                        summarise(PatientRetrouve = Reduce("|",PatientRetrouve),
                                  PatientDecede = Reduce("|",PatientDecede),
                                  PatientRetourneALaClinique = Reduce("|",PatientRetourneALaClinique),
                                  Trouvailles = Reduce("|",Trouvailles))

plr_outcome_patient$VitalStatus <- ifelse(plr_outcome_patient$PatientRetrouve,
                                  ifelse(plr_outcome_patient$PatientDecede,"Decede",ifelse(is.na(plr_outcome_patient$PatientDecede),"Vivant","Vivant")),
                                  ifelse(plr_outcome_patient$PatientDecede,"Decede",ifelse(is.na(plr_outcome_patient$PatientDecede),NA,"Vivant")))


plr_outcome_patient$OutcomeStatus <- ifelse(plr_outcome_patient$VitalStatus == "Vivant" | plr_outcome_patient$VitalStatus == "Decede","Outcome","No Outcome")
plr_outcome_patient$OutcomeStatus <- ifelse(is.na(plr_outcome_patient$VitalStatus),"No Outcome",plr_outcome_patient$OutcomeStatus )



t <- plr_outcome_patient %>% group_by(OutcomeStatus,VitalStatus,PatientRetrouve,PatientDecede,PatientRetourneALaClinique,Trouvailles) %>%
                    summarise(nb=n())

write_csv(t,"outcome.csv")

plr_outcome %>% filter(TypeRelanceNew == "LTFU",typesuivi == "Visite") %>%
    group_by(PatientRetrouve) %>% 
    summarise(n = n_distinct(id_patient))

######## LTFU Analysis

plr_ltfu <- filter(plr,TypeRelanceNew == "LTFU")
n_distinct(plr_ltfu$id_patient)

p_by_patient  <- plr_ltfu %>% group_by(id_patient,sexe) %>%
                             summarise( PatientDecede = sum(PatientDecede, na.rm = T ),
                                        PatientRetrouve = sum (PatientRetrouve, na.rm = T), 
                                        PatientRetourneALaClinique = sum(PatientRetourneALaClinique, na.rm = T))

p_by_patient %>% group_by(PatientRetrouve) %>% summarise( c = n_distinct(id_patient))

plr %>% group_by(typesuivi,TypeRelanceNew) %>% 
       summarise(n = n_distinct(id_patient)) %>%
        spread(TypeRelanceNew, n)

