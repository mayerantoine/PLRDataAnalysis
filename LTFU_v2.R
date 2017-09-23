
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
ggplot(follow_up_by_reason,aes(Partner,n/sum(n))) + 
    geom_col()+
    coord_flip()+
    facet_wrap(~TypeRelance_clean)

ggplot(follow_up_by_reason,aes(TypeRelance_clean,n/sum(n))) + 
    geom_col()+
    coord_flip()+
    facet_wrap(~Partner)

ggplot(follow_up_by_reason,aes(TypeRelance_clean,n)) + 
    geom_col()+
    coord_flip()+
    facet_wrap(~Partner)

pie <- follow_up_by_reason %>%# filter(TypeRelance_clean %in%  c("DAC","Geolocate","Missed RDV","LTFU")) %>%
    ggplot(aes(x = "", y=n/sum(n), fill = factor(TypeRelance_clean))) + 
    geom_bar(width = 1, stat = "identity") +
    theme(axis.line = element_blank(), 
          plot.title = element_text(hjust=0.5)) + 
    labs(fill="class", 
         x=NULL, 
         y=NULL, 
         title="Pie Chart of class", 
         caption="Source: mpg")

pie + coord_polar(theta = "y", start=0) + facet_wrap(~Partner)


ggplot(follow_up_by_reason,aes(TypeRelance_clean,n)) + 
    geom_col(aes(fill=Partner), position = "dodge")


# by departement
ggplot(follow_up_by_reason,aes(SNU1,n)) + 
    geom_col()+
    facet_wrap(~TypeRelance_clean) +
    theme(axis.text.x = element_text(angle=65, vjust=0.6))

## Trend by reason

trend_by_reason <- plr %>% group_by(TypeRelance_clean,SNU1,Partner,date = floor_date(datesuivieffectue_clean, "month"),year_suivi,month = month(datesuivieffectue_clean, label= T)) %>% 
    summarise(n = n_distinct(id_patient))

# labels and breaks for X axis text
lbls <- paste0(month.abb[month(trend_by_reason$date)], " ", lubridate::year(trend_by_reason$date))
brks <- trend_by_reason$date


# Overall trend
ggplot(trend_by_reason,aes(date,n))+
    geom_bar(stat = "identity")+
    labs(title="Monthly Tracking", 
         subtitle="PLR Monthly Tracking", 
         caption="Source: PLR", 
         y="Nb Patients") +  # title and caption
    scale_x_date(labels = lbls, 
                 breaks = brks) +  # change to monthly ticks and labels
    theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
          panel.grid.minor = element_blank())  # turn off minor grid

# trend by reason by year month
trend_by_reason <- filter(trend_by_reason,!TypeRelance_clean %in% c("HIV Pos","PreART"))

ggplot(trend_by_reason,aes(date,n))+
    geom_col( width=.8)+
    facet_grid(TypeRelance_clean~year_suivi) +
    theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(trend_by_reason,aes(date,n))+
    geom_bar(stat = "identity")+
    facet_grid(TypeRelance_clean ~.)+
    labs(title="Monthly Tracking", 
         subtitle="PLR Monthly Tracking", 
         caption="Source: PLR", 
         y="Nb Patients") +  # title and caption
    scale_x_date(labels = lbls, 
                 breaks = brks) +  # change to monthly ticks and labels
    theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
          panel.grid.minor = element_blank())  # turn off minor grid

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
                slice( which.max(datesuivieffectue_clean))
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

### active patient

report_date <- date(max(LTFU$datesuivieffectue_clean))
LTFU$diff_NexVisit_LastVisitCHT <-LTFU$NextVisitDate - LTFU$lastserviceeventdatecht
LTFU$diff_reportdate_NexVisit <- report_date - LTFU$NextVisitDate

LTFU <- mutate(LTFU,PatientActif = ifelse(PatientRetourneALaClinique == 1,
                                          ifelse(NextVisitDate >= report_date,1,
                                                        ifelse((NextVisitDate < report_date) & diff_reportdate_NexVisit<=90,1,0)
                                                 ),0
                                          )
               )
actif <- filter(LTFU,PatientActif == 1)




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
                           age_group,age_suivi,year_suivi,monthyr_suvi,Institution,
                           SNU1,SNU2,Partner, one_of(findings))

## number of records with no findings ??????

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

#by partner
t_by_partner <-  tb_findings %>% group_by(Partner) %>% select(one_of(findings)) %>% summarise_each(funs(sum(.,na.rm=T)))
t_by_partner <- gather(t_by_partner,"findings","n",2:13)

ggplot(t_by_partner) +
    geom_bar(stat = "identity", aes(reorder(findings,n),n))+
    facet_wrap(~Partner)+
    theme(axis.text.x = element_text(angle=90, vjust=0.4))

ggplot(t_by_partner) +
    geom_bar(stat = "identity", aes(reorder(Partner,n),n))+
    facet_wrap(~findings)+
    theme(axis.text.x = element_text(angle=90, vjust=0.4))
### categorical model

var_outcome <- c("PatientDecede","PatientRefuse","PatientRetourneALaClinique","PatientSuiviAilleurs","PatientIntrouvable","no_outcome","PatientActif")

LTFU_2 <- select(LTFU,id_patient,sexe,typesuivi,age_group,age_suivi,datesuivieffectue_clean,year_suivi,
                 monthyr_suvi,Institution,Partner,SNU1,SNU2,one_of(var_outcome))


LTFU_2 <- gather(LTFU_2,"patient_outcome","var",13:19) %>% filter(var == 1)



## PLR Cascade From janv 2015 - to July 2017
tab <- table(LTFU_2$patient_outcome)
tab_cascade <- as_data_frame(tab)
names(tab_cascade) <- c("outcomes","nb")

f_cascade <- function (tab_cascade) {
    ko <- filter(tab_cascade, outcomes %in% c("PatientDecede","PatientRefuse","PatientRetourneALaClinique","PatientSuiviAilleurs"))
    tot_contacted <- sum(tab_cascade$nb)
    
    dt <- data.frame(c("Tot. Contacted","Known Outcomes"),c(tot_contacted,sum(ko$nb)))
    names(dt) <- c("outcomes","nb")
    tab_cascade <- rbind(tab_cascade,dt)
    tab_cascade$percent <- round((tab_cascade$nb/tot_contacted)*100,1)
    
    tab_cascade
}

tab_cascade <- f_cascade(tab_cascade)


# percentage
tab_cascade %>% 
    filter(outcomes %in% c("Tot. Contacted","Known Outcomes","PatientRetourneALaClinique","PatientActif")) %>%
    ggplot() +
    geom_bar(stat= "identity" ,aes(reorder(outcomes,-percent),percent) )+ 
    geom_text(aes(x=outcomes,y=percent,label=paste(nb,"(",percent,"%",")")), vjust=1.5, colour="white") +
    labs(title="PLR Cascade From janv 2015 - to July 2017", 
         caption="Source: PLR", 
         x= "outcomes",
         y="Nb Patients") +  
    theme(axis.text.x = element_text(angle = 30, vjust=0.5),  # rotate x axis text
          panel.grid.minor = element_blank())  # turn off minor grid


## PLR Cascade From janv 2015 - to July 2017 by typesuivi
tab_cascade_suivi <- table(LTFU_2$typesuivi,LTFU_2$patient_outcome)
tab_cascade_suivi <- as_data_frame(tab_cascade_suivi)
names(tab_cascade_suivi) <- c("typesuivi","outcomes","nb")

tab_cascade_visite <- filter(tab_cascade_suivi,typesuivi =="Visite") %>% select(outcomes,nb)
tab_cascade_appel <- filter(tab_cascade_suivi,typesuivi =="Appel") %>% select(outcomes,nb)

tab_cascade_visite <- f_cascade(tab_cascade_visite)
tab_cascade_appel <- f_cascade(tab_cascade_appel)


# percentage
tab_cascade_visite %>% 
    filter(outcomes %in% c("Tot. Contacted","Known Outcomes","PatientRetourneALaClinique","PatientActif")) %>%
    ggplot() +
    geom_bar(stat= "identity" ,aes(reorder(outcomes,-percent),percent) )+ 
    geom_text(aes(x=outcomes,y=percent,label=paste(nb,"(",percent,"%",")")), vjust=1.5, colour="white") +
    labs(title="LTFU PLR Cascade Viste From janv 2015 - to July 2017", 
         caption="Source: PLR", 
         x= "outcomes",
         y="Nb Patients") +  
    theme(axis.text.x = element_text(angle = 30, vjust=0.5),  # rotate x axis text
          panel.grid.minor = element_blank())  # turn off minor grid


# percentage
tab_cascade_appel %>% 
    filter(outcomes %in% c("Tot. Contacted","Known Outcomes","PatientRetourneALaClinique","PatientActif")) %>%
    ggplot() +
    geom_bar(stat= "identity" ,aes(reorder(outcomes,-percent),percent) )+ 
    geom_text(aes(x=outcomes,y=percent,label=paste(nb,"(",percent,"%",")")), vjust=1.5, colour="white") +
    labs(title="LTFU PLR Cascade Appel From janv 2015 - to July 2017", 
         caption="Source: PLR", 
         x= "outcomes",
         y="Nb Patients") +  
    theme(axis.text.x = element_text(angle = 30, vjust=0.5),  # rotate x axis text
          panel.grid.minor = element_blank())  # turn off minor grid



tab1 <- table(LTFU_2$sexe,LTFU_2$age_group ,LTFU_2$patient_outcome)
ftable(tab1)
prop.table(ftable(tab1))

xtabs_partner <- xtabs(~Partner+patient_outcome,LTFU_2)

xtabs_snu1 <- xtabs(~SNU1+patient_outcome,LTFU_2)

### LTFU PLR Cascade by partner
tab_partner <- table(LTFU_2$Partner, LTFU_2$patient_outcome)
tab_cascade_partner<- as_data_frame(tab_partner)
names(tab_cascade_partner) <- c("Partner","outcomes","nb")


tab_cascade_partner %>% 
    filter(outcomes %in% c("Tot. Contacted","Known Outcomes","PatientRetourneALaClinique","PatientActif")) %>%
    ggplot() +
    geom_bar(stat= "identity" ,aes(reorder(outcomes,-percent),percent) )+ 
    geom_text(aes(x=outcomes,y=percent,label=paste(nb,"(",percent,"%",")")), vjust=1.5, colour="white") +
    labs(title="PLR Cascade From janv 2015 - to July 2017", 
         caption="Source: PLR", 
         x= "outcomes",
         y="Nb Patients") +  
    theme(axis.text.x = element_text(angle = 30, vjust=0.5),  # rotate x axis text
          panel.grid.minor = element_blank())  # turn off minor grid


### active patient

start <- date("2015-10-01")
end <- date("2017-07-31")
active_LTFU <- LTFU %>% 
                filter(PatientRetourneALaClinique ==1,  datesuivieffectue_clean >= start, datesuivieffectue_clean <= end) %>% 
                select(id_patient,typesuivi,datesuivieffectue_clean,PatientRetourneALaClinique,
                      DateRetourALaClinique,lastserviceeventdateemr,lastserviceeventdatecht,
                      NextVisitDate,statutfiche)
report_date <- date(max(active_LTFU$datesuivieffectue_clean))
active_LTFU$diff_NexVisit_LastVisitCHT <- active_LTFU$NextVisitDate - active_LTFU$lastserviceeventdatecht
active_LTFU$diff_reportdate_NexVisit <- report_date - active_LTFU$NextVisitDate

a1 <- filter(active_LTFU, active_LTFU$NextVisitDate >= report_date)
a2 <- filter(active_LTFU, active_LTFU$NextVisitDate < report_date,active_LTFU$diff_reportdate_NexVisit <=90)

active_LTFU <- mutate(active_LTFU,PatientActif_1 = ifelse(NextVisitDate >= report_date,1,0))
active_LTFU <- mutate(active_LTFU,PatientActif_2 = ifelse(report_date - NextVisitDate <=90,1,0))


active_LTFU <- mutate(active_LTFU,PatientActif = ifelse(NextVisitDate >= report_date,1,
                                                         ifelse((NextVisitDate < report_date) & diff_reportdate_NexVisit<=90,1,0)))
actif <- filter(active_LTFU,PatientActif == 1)

####

LTFU_date <- LTFU %>%
    group_by(year_suivi,month_suivi = month(datesuivieffectue_clean)) %>%
    summarise(n=n_distinct(id_patient),
              outcome = sum(outcome_status),
              Dead = sum(PatientDecede),
              PatientRefuse = sum(PatientRefuse),
              PatientTransfered = sum(PatientSuiviAilleurs),
              PatientBackinClinic = sum(PatientRetourneALaClinique),
              return_perc = round(PatientBackinClinic/n,2),
              PatientNotFound = sum(PatientIntrouvable),
              no_reported_outcome = sum(no_outcome))


LTFU_date2 <- LTFU %>%
    group_by(date = floor_date(datesuivieffectue_clean, "month")) %>%
    summarise(n=n_distinct(id_patient),
              outcome = sum(outcome_status),
              Dead = sum(PatientDecede),
              PatientRefuse = sum(PatientRefuse),
              PatientTransfered = sum(PatientSuiviAilleurs),
              PatientBackinClinic = sum(PatientRetourneALaClinique),
              return_perc = round(PatientBackinClinic/n,2),
              PatientNotFound = sum(PatientIntrouvable),
              no_reported_outcome = sum(no_outcome))

LTFU_date$date <- as.Date(paste(LTFU_date$year_suivi,LTFU_date$month_suivi,"01",sep = "-"))

theme_set(theme_classic())
ggplot(LTFU_date, aes(x=date)) + 
    geom_line(aes(y=n)) + 
    labs(title="Time Series Chart", 
         subtitle="Returns Percentage from 'Economics' Dataset", 
         caption="Source: Economics", 
         y="Returns %")



# labels and breaks for X axis text
lbls <- paste0(month.abb[month(LTFU_date$date)], " ", lubridate::year(LTFU_date$date))
brks <- LTFU_date$date


# plot
theme_set(theme_bw())
ggplot(LTFU_date, aes(x=date)) + 
    geom_line(aes(y=n)) + 
    labs(title="Monthly Time Series", 
         subtitle="Returns Percentage from Economics Dataset", 
         caption="Source: PLR", 
         y="Nb Patients") +  # title and caption
    scale_x_date(labels = lbls, 
                 breaks = brks) +  # change to monthly ticks and labels
    theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
          panel.grid.minor = element_blank())  # turn off minor grid


theme_set(theme_bw())
p <- ggplot(LTFU_date2, aes(x=date)) 
    p <- p + geom_bar( stat="identity",aes(y=n),fill = "blue") 
    p <- p + geom_line(aes(y=PatientBackinClinic,colour = "Back in Care"),color = "red") + geom_point(aes(y=PatientBackinClinic),color = "red")
    p <- p + scale_colour_manual(values = c("blue", "red"))
    p <- p + labs(title="Monthly Tracking Series", 
         subtitle="LTFU tracked", 
         caption="Source: PLR", 
         y="Nb Patients") +  # title and caption
    scale_x_date(labels = lbls, 
                 breaks = brks) +  # change to monthly ticks and labels
    theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
          panel.grid.minor = element_blank())  # turn off minor grid
    p
    
    
    theme_set(theme_bw())
    f <- ggplot(LTFU_date2, aes(x=date)) 
    f <- f + geom_bar( stat="identity",aes(y=n),fill = "blue") 
    f <- f + geom_line(aes(y=return_perc,colour = "Back in Care"),color = "red") + geom_point(aes(y=return_perc),color = "red")
    f <- f + scale_y_continuous()
    f <- f + scale_colour_manual(values = c("blue", "red"))
    f <- f + labs(title="Monthly Tracking Series", 
                  subtitle="LTFU tracked", 
                  caption="Source: PLR", 
                  y="Nb Patients") +  # title and caption
        scale_x_date(labels = lbls, 
                     breaks = brks) +  # change to monthly ticks and labels
        theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
              panel.grid.minor = element_blank())  # turn off minor grid
    f

### relost
    par(mar = c(1, 1, 1, 1))
    openplotmat(main ="LTFU Flowchart of outcomes Janv 2015- July 2017")
    elpos <- coordinates (c(1, 2, 4, 2))
    fromto <- matrix(ncol = 2,byrow = T,data = c(1,2,1,3,3,4,3,5,3,6,3,7,7,9,7,8))
    nr <- nrow(fromto)
    arrpos <- matrix(ncol = 2, nrow = nr)
    for(i in 1:nr)
        arrpos[i,] <- straightarrow(to = elpos[fromto[i,2],],
                                    from = elpos[fromto[i,1],],
                                    lwd = 2,arr.pos = 0.6,
                                    arr.length = 0.5)
    textround(elpos[1,], 0.15,0.06, lab = "34,500 (100%) Total Contacted", cex = 0.8)
    textround(elpos[2,], 0.15,0.07, lab = "25500 (88%) No Outcomes found", cex = 0.8)
    textround(elpos[3,], 0.15,0.07, lab = "4563 (88%) Outcomes found",  cex = 0.8)
    textround(elpos[4,], 0.05,0.07, lab = c("Dead","234 (88%)"),  cex = 0.8)
    textround(elpos[5,], 0.05,0.07, lab = c("Silent transfer","1256 (23%)"),  cex = 0.8)
    textround(elpos[6,], 0.05,0.07, lab = c("Refuse Treatment","1236 (34%)"),  cex = 0.8)
    textround(elpos[7,], 0.05,0.07, lab = c("Back in clinic","4557 (88%)"),  cex = 0.8)
    textround(elpos[9,], 0.05,0.07, lab = c("active on treatment","3438 (88%)"),  cex = 0.8)
    textround(elpos[8,], 0.05,0.07, lab = c("not active","322 (88%)"),  cex = 0.8)   



