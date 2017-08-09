
library(tidyverse)
library(lubridate)

rm(list = ls())
rawdata <- read_csv("plr_20170803.csv", na=c("NULL","NA","N/A"))

names(rawdata) <- make.names(names(rawdata))
rawdata$datesuivieffectue <- mdy(rawdata$datesuivieffectue)
rawdata$DateRetourALaClinique <- mdy(rawdata$DateRetourALaClinique)
rawdata$datenaissance <- mdy(rawdata$datenaissance)
rawdata$FicheCrééeLe <- mdy(rawdata$FicheCrééeLe)
rawdata$lastserviceeventdateemr <- mdy(rawdata$lastserviceeventdateemr)
rawdata$lastserviceeventdatecht <- mdy(rawdata$lastserviceeventdatecht)
rawdata$NextVisitDate <- mdy(rawdata$NextVisitDate)
rawdata <- cbind(id= as.numeric(rownames(rawdata)),rawdata)

# Recode TypeRelance

plr <- rawdata
plr$TypeRelance_clean <- "NA"
plr[grep("Communautaire",plr$TypeRelance),]$TypeRelance_clean <- "DAC"
plr[grep("perdu",plr$TypeRelance),]$TypeRelance_clean <- "LTFU"
plr[grep("Rendez-vous",plr$TypeRelance),]$TypeRelance_clean <- "Missed RDV"
plr[grep("Verification",plr$TypeRelance),]$TypeRelance_clean <- "Geolocate"
plr[grep("VIH+",plr$TypeRelance),]$TypeRelance_clean <- "HIV Pos"
plr[grep("PréARV",plr$TypeRelance),]$TypeRelance_clean <- "PreART"


# import plr mapping to add partner, snu1, snu2
plr_mapping <-  read_csv("plr_mapping.csv")
plr <- left_join(plr,plr_mapping, by= c("Institution"="facility"))

# Calculate datesuivi clean
# suivi <- plr %>% select(id_patient,datesuivieffectue,FicheCrééeLe) %>%
#    mutate(diff= datesuivieffectue - FicheCrééeLe ) %>%
#    filter(diff >= 0 )

plr <- plr %>% mutate(datesuivieffectue_clean = if_else(datesuivieffectue - FicheCrééeLe < 0, 
                                                   FicheCrééeLe,datesuivieffectue))

plr$monthyr_suvi <- paste(month(plr$datesuivieffectue_clean,label = T),year(plr$datesuivieffectue_clean),sep=" ")

plr$year_suivi <- year(plr$datesuivieffectue_clean)

# Calculate age
plr <- plr %>% mutate(age_suivi = round((datesuivieffectue - datenaissance)/365,0))
plr <- plr %>% mutate(age_group = ifelse(age_suivi <= 15, "Peds","Adult"))

## Tracking by reason
follow_up_by_reason <- plr %>% group_by(TypeRelance_clean,typesuivi,SNU1,Partner,sexe) %>% 
                                summarise(n = n_distinct(id_patient))

# by reason
ggplot(follow_up_by_reason,aes(TypeRelance_clean,n)) + 
                                    geom_col()

# by type suivi
ggplot(follow_up_by_reason,aes(TypeRelance_clean,n)) + 
    geom_col()+
    facet_grid(~typesuivi)

# by partner
ggplot(follow_up_by_reason,aes(Partner,n)) + 
    geom_col()+
    coord_flip()+
    facet_wrap(~TypeRelance_clean)


ggplot(follow_up_by_reason,aes(TypeRelance_clean,n)) + 
    geom_col(aes(fill=Partner), position = "dodge")


# by departement
ggplot(follow_up_by_reason,aes(SNU1,n)) + 
    geom_col()+
    facet_wrap(~TypeRelance_clean) +
    theme(axis.text.x = element_text(angle=65, vjust=0.6))

## Trend by reason

trend_by_reason <- plr %>% group_by(TypeRelance_clean,SNU1,Partner,year_suivi,month = month(datesuivieffectue_clean, label= T)) %>% 
    summarise(n = n_distinct(id_patient))

# Overall trend
ggplot(trend_by_reason,aes(as.factor(month),n))+
    geom_col()+
    facet_grid(~year_suivi)

# trend by reason by year month
ggplot(trend_by_reason,aes(as.factor(month),n))+
    geom_col( width=.8)+
    facet_grid(TypeRelance_clean~year_suivi) +
    theme(axis.text.x = element_text(angle=65, vjust=0.6))

# trend by department  by year month
ggplot(trend_by_reason,aes(as.factor(month),n))+
    geom_col()+
    facet_grid(SNU1~year_suivi)+
    theme(axis.text.x = element_text(angle=65, vjust=0.6))

#trend by partner  by year month
ggplot(trend_by_reason,aes(as.factor(month),n))+
    geom_col()+
    facet_grid(Partner~year_suivi)+
    theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Top 10 sites

top_10 <- plr %>% group_by(Institution) %>% summarise(n = n_distinct(id_patient))
top_10 <- top_10[order(top_10$n,decreasing = T),]
top_10 <- head(top_10,10)

ggplot(top_10) + geom_col(aes(Institution,n))+
    coord_flip()+
    theme(axis.text.x = element_text(angle=65, vjust=0.6))

agg_site <- plr %>% group_by(TypeRelance_clean,Institution) %>% summarise(n = n_distinct(id_patient))
agg_site$TypeRelance_clean <- as.factor(agg_site$TypeRelance_clean)
agg_site$Institution <- as.factor(agg_site$Institution)
agg_site <- agg_site[order(agg_site$n,decreasing = T),]

## cannot  use  stack because of duplicate
 ggplot(agg_site) + 
    geom_bar(aes(x=reorder(Institution,n), y= n, fill=TypeRelance_clean),stat ="Identity", position="stack") +
    theme(axis.text.x = element_text(angle=90, vjust=0.1))


##################### LTFU

## outcome flow
LTFU <- plr %>% filter(TypeRelance_clean == "LTFU") %>% 
                group_by(id_patient) %>%
                slice( which.max(datesuivieffectue))
LTFU <- ungroup(LTFU)
LTFU$PatientDecede <- ifelse(is.na(LTFU$PatientDecede),0,LTFU$PatientDecede)
LTFU$PatientRetourneALaClinique <- ifelse(is.na(LTFU$PatientRetourneALaClinique),0,LTFU$PatientRetourneALaClinique)
LTFU$PatientRefuse <- ifelse(is.na(LTFU$PatientRefuse),0,LTFU$PatientRefuse)
LTFU$PatientSuiviAilleurs <- ifelse(is.na(LTFU$PatientSuiviAilleurs),0,LTFU$PatientSuiviAilleurs)
LTFU$PatientIntrouvable <- ifelse(is.na(LTFU$PatientIntrouvable),0,LTFU$PatientIntrouvable)

LTFU <- mutate(LTFU,outcome_check = PatientDecede+PatientRefuse+PatientSuiviAilleurs+PatientRetourneALaClinique+PatientIntrouvable)
LTFU <- filter(LTFU,outcome_check <=1)

## all outcome except patientintrouvable
LTFU$outcome_status <-as.integer(LTFU$PatientDecede | 
                                 LTFU$PatientRefuse |
                                 LTFU$PatientSuiviAilleurs | 
                                 LTFU$PatientRetourneALaClinique )

LTFU$no_outcome <- ifelse(LTFU$outcome_check == 0,1,0)




#outcome flow summary
outcome_summary <- LTFU %>%
    summarise(n=n_distinct(id_patient),
              outcome= sum(outcome_status),
              percent_outcome = round((outcome/n)*100,1),
              Dead = sum(PatientDecede),
              percent_dead = round((Dead/n)*100,1),
              PatientRefuse = sum(PatientRefuse),
              percent_refuse = round((PatientRefuse/n)*100,1),
              PatientTransfered = sum(PatientSuiviAilleurs),
              percent_transfered = round((PatientTransfered/n)*100,1),
              PatientBackinClinic = sum(PatientRetourneALaClinique),
              percent_BackinClinic = round((PatientBackinClinic/n)*100,1),
              PatientNotFound = sum(PatientIntrouvable),
              percent_NotFound =  round((PatientNotFound/n)*100,1),
              no_reported_outcome = sum(no_outcome),
              percent_No_outcome = round((no_reported_outcome/n)*100,1))

s <- outcome_summary %>% 
    select(percent_outcome,percent_dead,percent_refuse,percent_transfered,
           percent_BackinClinic,percent_NotFound,percent_No_outcome) %>% 
    gather("status","val",1:7)

ggplot(s) +
    geom_bar(stat = "identity", aes(reorder(status,val),val))

#outcome flow summary by year
outcome_summary_by_year <- LTFU %>%
    group_by(year_suivi) %>%
    summarise(n=n_distinct(id_patient),
              outcome= sum(outcome_status),
              percent_outcome = round((outcome/n)*100,1),
              Dead = sum(PatientDecede),
              percent_dead = round((Dead/n)*100,1),
              PatientRefuse = sum(PatientRefuse),
              percent_refuse = round((PatientRefuse/n)*100,1),
              PatientTransfered = sum(PatientSuiviAilleurs),
              percent_transfered = round((PatientTransfered/n)*100,1),
              PatientBackinClinic = sum(PatientRetourneALaClinique),
              percent_BackinClinic = round((PatientBackinClinic/n)*100,1),
              PatientNotFound = sum(PatientIntrouvable),
              percent_NotFound =  round((PatientNotFound/n)*100,1),
              no_reported_outcome = sum(no_outcome),
              percent_No_outcome = round((no_reported_outcome/n)*100,1))


#outcome flow summary by age
outcome_summary_by_age <- LTFU %>%
    group_by(age_group) %>%
    summarise(n=n_distinct(id_patient),
              outcome= sum(outcome_status),
              percent_outcome = round((outcome/n)*100,1),
              Dead = sum(PatientDecede),
              percent_dead = round((Dead/n)*100,1),
              PatientRefuse = sum(PatientRefuse),
              percent_refuse = round((PatientRefuse/n)*100,1),
              PatientTransfered = sum(PatientSuiviAilleurs),
              percent_transfered = round((PatientTransfered/n)*100,1),
              PatientBackinClinic = sum(PatientRetourneALaClinique),
              percent_BackinClinic = round((PatientBackinClinic/n)*100,1),
              PatientNotFound = sum(PatientIntrouvable),
              percent_NotFound =  round((PatientNotFound/n)*100,1),
              no_reported_outcome = sum(no_outcome),
              percent_No_outcome = round((no_reported_outcome/n)*100,1))


#outcome flow by sexe
outcome_summary_by_sexe <- LTFU %>%
    group_by(sexe) %>%
    summarise(n=n_distinct(id_patient),
              outcome= sum(outcome_status),
              percent_outcome = round((outcome/n)*100,1),
              Dead = sum(PatientDecede),
              percent_dead = round((Dead/n)*100,1),
              PatientRefuse = sum(PatientRefuse),
              percent_refuse = round((PatientRefuse/n)*100,1),
              PatientTransfered = sum(PatientSuiviAilleurs),
              percent_transfered = round((PatientTransfered/n)*100,1),
              PatientBackinClinic = sum(PatientRetourneALaClinique),
              percent_BackinClinic = round((PatientBackinClinic/n)*100,1),
              PatientNotFound = sum(PatientIntrouvable),
              percent_NotFound =  round((PatientNotFound/n)*100,1),
              no_reported_outcome = sum(no_outcome),
              percent_No_outcome = round((no_reported_outcome/n)*100,1))

LTFU$PatientContacted <- "Total"


## Tentative description table
outcome_data <- LTFU %>%
    group_by(PatientContacted) %>%
    summarise(n=n_distinct(id_patient),
              outcome= sum(outcome_status),
              Dead = sum(PatientDecede),
              PatientRefuse = sum(PatientRefuse),
              PatientTransfered = sum(PatientSuiviAilleurs),
              PatientBackinClinic = sum(PatientRetourneALaClinique),
              PatientNotFound = sum(PatientIntrouvable),
              no_reported_outcome = sum(no_outcome))

outcome_age<- LTFU %>%
    group_by(PatientContacted = age_group) %>%
    summarise(n=n_distinct(id_patient),
              outcome= sum(outcome_status),
              Dead = sum(PatientDecede),
              PatientRefuse = sum(PatientRefuse),
              PatientTransfered = sum(PatientSuiviAilleurs),
              PatientBackinClinic = sum(PatientRetourneALaClinique),
              PatientNotFound = sum(PatientIntrouvable),
              no_reported_outcome = sum(no_outcome))

outcome_sexe<- LTFU %>%
    group_by(PatientContacted = sexe) %>%
    summarise(n=n_distinct(id_patient),
              outcome= sum(outcome_status),
              Dead = sum(PatientDecede),
              PatientRefuse = sum(PatientRefuse),
              PatientTransfered = sum(PatientSuiviAilleurs),
              PatientBackinClinic = sum(PatientRetourneALaClinique),
              PatientNotFound = sum(PatientIntrouvable),
              no_reported_outcome = sum(no_outcome))
 
outcome_description <-  rbind(outcome_data,outcome_age,outcome_sexe)

# what is the trend for patient introuvable

################ Findings


findings <- c("FraisTransportNonDisponible","EtatTropMalade","Oubli",
              "RechercheSoinsAlternatifs","PeurDEtreVuDansUnSiteVIH","ServicesInsatisfaisant",
              "Voyage","Migration","Stigmatisation","OccupationOuManquedeTemps","DecesDansLaFamille",
              "BesoinDeTransfert")

tb_findings <- plr %>% filter(TypeRelance_clean == "LTFU") %>% 
                    select(id_patient,typesuivi,datesuivieffectue_clean,sexe,
                           age_group,age_suivi,year_suivi,monthyr,Institution,
                           SNU1,SNU2,Partner, one_of(findings))

## number of records with no findings

## overall findings
t <- tb_findings %>%  select(one_of(findings)) %>%  summarise_each(funs(sum(.,na.rm=T)))
t <- gather(t,"findings","n",1:12)

ggplot(t) +
    geom_bar(stat = "identity", aes(reorder(findings,n),n))+
    theme(axis.text.x = element_text(angle=90, vjust=0.4)) # i need proportion to better tell story

## by department
t_by_SNU1 <- tb_findings %>% group_by(SNU1) %>% select(one_of(findings)) %>% summarise_each(funs(sum(.,na.rm=T)))
t_by_SNU1 <- gather(t_by_SNU1,"findings","n",2:13)

ggplot(t_by_SNU1) +
    geom_bar(stat = "identity", aes(reorder(findings,n),n))+
    facet_wrap(~SNU1)+
    theme(axis.text.x = element_text(angle=90, vjust=0.4))


ggplot(t_by_SNU1) +
    geom_bar(stat = "identity", aes(reorder(SNU1,n),n))+
    facet_wrap(~findings)+
    theme(axis.text.x = element_text(angle=90, vjust=0.4))


### categorical model

var_outcome <- c("PatientDecede","PatientRefuse","PatientRetourneALaClinique","PatientSuiviAilleurs","PatientIntrouvable","no_outcome")

LTFU_2 <- select(LTFU,id_patient,sexe,age_group,age_suivi,datesuivieffectue_clean,year_suivi,
                 monthyr_suvi,Institution,Partner,SNU1,SNU2,one_of(var_outcome))


LTFU_2 <- gather(LTFU_2,"patient_outcome","var",12:17) %>% filter(var == 1)

tab1 <- table(LTFU_2$sexe,LTFU_2$age_group ,LTFU_2$patient_outcome)
ftable(tab1)
prop.table(ftable(tab1))

xtabs_partner <- xtabs(~Partner+patient_outcome,LTFU_2)

xtabs_partner <- xtabs(~SNU1+patient_outcome,LTFU_2)

