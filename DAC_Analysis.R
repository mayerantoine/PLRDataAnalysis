library(tidyverse)
library(lubridate)
library(scales)
library(diagram)
library(knitr)
library(kableExtra)
library(xtable)


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

write_csv(plr,"plr_dac_data.csv")

## DAC
DAC <- plr %>% filter(TypeRelance_clean == "DAC") 
n_distinct_DAC_patient <- n_distinct(DAC$id_patient)
DAC_last <- plr %>% filter(TypeRelance_clean == "DAC") %>% 
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

