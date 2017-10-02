library(tidyverse)
library(lubridate)
library(scales)
library(diagram)
library(knitr)
library(kableExtra)
library(xtable)


rm(list = ls())
rawdata <- read_csv("plr_26_9_2017.csv", na=c("NULL","NA","N/A"))

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

plr$monthyr_suvi <- paste(month(plr$datesuivieffectue_clean,label =T),year(plr$datesuivieffectue_clean),sep="")

plr$year_suivi <- year(plr$datesuivieffectue_clean)

# Calculate age
plr <- plr %>% mutate(age_suivi = round((datesuivieffectue - datenaissance)/365,0))
plr <- plr %>% mutate(age_group = ifelse(age_suivi <= 15, "Peds","Adult"))

min_tracked_date <- min(plr$datesuivieffectue_clean)
max_tracked_date <- max(plr$datesuivieffectue_clean)

n <- nrow(rawdata)
c <- ncol(rawdata)
patient_distinct <- n_distinct(rawdata$id_patient)
site_distinct <- n_distinct(rawdata$Institution)



## DAC
DAC$MedicamentRecu <- as.numeric(DAC$MedicamentRecu)
DAC$DureeMedicament <- as.numeric(DAC$DureeMedicament)

start <- date("2015-03-18")
end <- date("2017-08-03")

DAC <- plr %>% filter(TypeRelance_clean == "DAC", datesuivieffectue_clean >=start & datesuivieffectue_clean <=end ) %>%
                select(id,id_fiche,id_patient,codest,sexe,age_suivi,datesuivieffectue_clean,datenaissance,TypeRelance_clean,Partner,Institution,PatientRetrouve,visitegéolocalisee,
                       observationvisite,PatientDecede,FicheCrééeLe,lastserviceeventdatecht,lastserviceeventdateemr,NextVisitDate,age_group,
                       DureeMedicament,MedicamentRecu)

n_distinct_DAC_patient <- n_distinct(DAC$id_patient)

DAC_last <- DAC %>%
    group_by(id_patient) %>%
    slice( which.max(datesuivieffectue_clean))

## age break down
DAC_last$age <- as.numeric(DAC_last$age_suivi)
DAC_last$age_cat1 <- cut(DAC_last$age,c(0,14,24,34,44,59))

DAC_last <- mutate(DAC_last,age_cat1 = ifelse(age <= 14,"0-14",
                                              ifelse(age>14 & age<=24,"15-24",
                                                     ifelse(age> 24 & age <=34,"25-34",
                                                            ifelse(age > 24 & age <=44,"35-44",
                                                                   ifelse(age>44 & age <=59,"45-59",">59")
                                                            )
                                                     )
                                              )
                                        )
                   )

DAC_age_break_down <- table(DAC_last$age_cat1)
DAC_age_break_down <- as.data.frame(DAC_age_break_down)
names(DAC_age_break_down) <- c("Age_group","n")


ggplot(DAC_age_break_down,aes(x=Age_group,y=n))+
    geom_bar(stat = "identity",position="dodge")+
    geom_text(aes(label=n), vjust=1.1, colour="white",
              position=position_dodge(.9), size=4)+
    labs(title="DAC by Sexe", 
         subtitle="# unique patient DAC From March 2015 - July 2017",
         caption="Source: PLR", 
         y="# unique Patients",
         x="")+
    # scale_fill_manual(values = c("#0066CC","#0033FF"))+
    # scale_fill_brewer(palette="Greys")+
    theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 14), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 14),
          plot.title = element_text(size = 16))

## sexe and age group

DAC_last_age_sexe <- table(DAC_last$age_group,DAC_last$sexe)
DAC_last_age_sexe <- as.data.frame(DAC_last_age_sexe)
names(DAC_last_age_sexe) <- c("Age_group","Sexe","n")

ggplot(DAC_last_age_sexe,aes(x=Age_group,y=n,fill=Sexe))+
    geom_bar(stat = "identity",position="dodge")+
    geom_text(aes(label=n), vjust=1.1, colour="white",
              position=position_dodge(.9), size=4)+
    labs(title="DAC by Sexe", 
         subtitle="# unique patient DAC From March 2015 - July 2017",
         caption="Source: PLR", 
         y="# unique Patients",
         x="")+
    theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 14), 
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title.x = element_text(size = 14),
          plot.title = element_text(size = 16)) 

## add actifs

report_date_DAC <- date(max(DAC_last$datesuivieffectue_clean))
DAC_last$diff_NexVisit_LastVisitCHT <-DAC_last$NextVisitDate - DAC_last$lastserviceeventdatecht
DAC_last$diff_reportdate_NexVisit <- report_date_DAC - DAC_last$NextVisitDate

DAC_last <- mutate(DAC_last,PatientActif = ifelse(NextVisitDate >= report_date_DAC,1,
                                                  ifelse((NextVisitDate < report_date_DAC) & diff_reportdate_NexVisit<=90,1,0)
)
)

table(DAC_last$MedicamentRecu)
table(DAC_last$PatientActif)

##monthly trend
 trend_dac <- DAC %>%
        group_by(date = floor_date(datesuivieffectue_clean, "month")) %>% 
            summarise(n = n_distinct(id_patient),
                      mr = sum(MedicamentRecu,na.rm = T))
 
 write_csv(trend_dac,"trend_dac.csv")


# group by partner

DAC_last %>%
    group_by(Partner) %>% 
    summarise(n = n_distinct(id_patient)) 

## by last visit

last_visit_dac <- DAC_last %>%
    group_by(year(lastserviceeventdatecht)) %>% 
    summarise(n = n_distinct(id_patient)) 


write_csv(last_visit_dac,"last_visit_dac.csv")

##monthly trend
trend_duree_dac <- DAC %>%
    group_by(date = floor_date(datesuivieffectue_clean, "month"),DureeMedicament)%>% 
    summarise(n = n_distinct(id_patient),
              mr = sum(MedicamentRecu,na.rm = T))

write_csv(trend_duree_dac,"trend_duree_dac.csv")