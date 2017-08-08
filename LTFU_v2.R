
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
    facet_grid(~TypeRelance_clean)


ggplot(follow_up_by_reason,aes(TypeRelance_clean,n)) + 
    geom_col(aes(fill=Partner), position = "dodge")


# by departement
ggplot(follow_up_by_reason,aes(SNU1,n)) + 
    geom_col()+
    coord_flip()+
    facet_grid(~TypeRelance_clean)

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

## cannot bt use because of duplicate
 ggplot(agg_site) + 
    geom_bar(aes(x=reorder(Institution,n), y= n, fill=TypeRelance_clean),stat ="Identity", position="stack") +
    theme(axis.text.x = element_text(angle=90, vjust=0.1))

# what is the trend for patient introuvable
### LTFU

## outcome flow
LTFU <- plr %>% filter(TypeRelance_clean == "LTFU") %>% 
                group_by(id_patient) %>%
                slice( which.max(datesuivieffectue))

LTFU$PatientDecede <- ifelse(is.na(LTFU$PatientDecede),0,LTFU$PatientDecede)
LTFU$PatientRetourneALaClinique <- ifelse(is.na(LTFU$PatientRetourneALaClinique),0,LTFU$PatientRetourneALaClinique)
LTFU$PatientRefuse <- ifelse(is.na(LTFU$PatientRefuse),0,LTFU$PatientRefuse)
LTFU$PatientSuiviAilleurs <- ifelse(is.na(LTFU$PatientSuiviAilleurs),0,LTFU$PatientSuiviAilleurs)
LTFU$PatientIntrouvable <- ifelse(is.na(LTFU$PatientIntrouvable),0,LTFU$PatientIntrouvable)

LTFU <- mutate(LTFU,outcome_check = PatientDecede+PatientRefuse+PatientSuiviAilleurs+PatientRetourneALaClinique+PatientIntrouvable)
LTFU <- filter(LTFU,outcome_check <=1)
LTFU$outcome_status <-as.integer(LTFU$PatientDecede | 
                                 LTFU$PatientRefuse |
                                 LTFU$PatientSuiviAilleurs | 
                                 LTFU$PatientRetourneALaClinique | 
                                 LTFU$PatientIntrouvable)

outcome <- LTFU %>% select(id_patient,datesuivieffectue_clean,PatientDecede,PatientRefuse,
                           PatientSuiviAilleurs,PatientRetourneALaClinique,PatientIntrouvable,statutfiche)

write_csv(outcome,"outcome2.csv")



flow <- LTFU %>%
group_by(PatientContacte,PatientRetrouve_clean,PatientDecede_clean,PatientSuiviAilleurs,PatientRefuse,PatientRetourneALaClinique) %>% 
    summarise(n=n_distinct(id_patient))