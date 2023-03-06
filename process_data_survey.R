rm(list=ls())
####Dependencies####
library(tidyverse)
library(ggplot2)
# library(ggpattern)
library(stringr)
library(data.table)

Path <- "~/GenderEq/gender_eq_survey.csv"
df_fem <- read.csv(Path)
col_names <- c("fecha_hora", "genero",
               "dependent_people", "primary_caregiver",
               "minority_group", "position_CEAB",
               "organizational_culture", "personnel_commited",
               "decision_making", "work_with", "higher_prestige_tasks",
               "volunteer_tasks", "num_hours", "sources_dist",
               "workforce_trained", "integrate_gender_eq",
               "difference_gender", "opportunities_hired",
               "promotion", "encouraged_prom", "personal_work_balance",
               "reduced_hours", "leave_negative_eff", "lenguage_images",
               "sexual_harrasment","suffered_seen_harras", "communicated_harras",
               "response_harras")

# Data frame with the col names and the whole question
df_names_col_def <- data.frame(name_col = col_names, description = colnames(df_fem) )
colnames(df_fem) <- col_names

# Data from 2021, I set Early career researcher = predoc,postdoc and visiting students
# Administration/Manteinance = Administrative staff
df <- data.frame(x = rep("Permanent position", 9))
df <- rbind(df, data.frame(x = rep("Early career researcher", 4)))
df <- rbind(df, data.frame(x = rep("Early career researcher", 9)))
df <- rbind(df, data.frame(x = rep("Research technician", 33)))
df <- rbind(df, data.frame(x = rep("Administration/Manteinance", 6)))
df <- rbind(df, data.frame(x = rep("Early career researcher", 20)))
df$gender <- "female"
  
df2 <- data.frame(x = rep("Permanent position", 15))
df2 <- rbind(df2, data.frame(x = rep("Early career researcher", 6)))
df2 <- rbind(df2, data.frame(x = rep("Early career researcher", 5)))
df2 <- rbind(df2, data.frame(x = rep("Research technician", 40)))
df2 <- rbind(df2, data.frame(x = rep("Administration/Manteinance", 5)))
df2 <- rbind(df2, data.frame(x = rep("Early career researcher", 16)))
df2$gender <- "male"

df_plantilla_CEAB <- rbind(df,df2)
colnames(df_plantilla_CEAB) <- c("position","gender")
df_plantilla_CEAB <- df_plantilla_CEAB[ order(df_plantilla_CEAB$position),]
ggplot(df_plantilla_CEAB) +
  geom_bar(aes(position, fill = gender)) +
  theme_bw() + ylim(c(0,80)) +
  ylab("Number") + xlab("Position at CEAB") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_fill_manual(name = "", values=c("#3B429F",
                             "#AA7DCE"))
# Trying to add percentage inside the rectangles.
# df_plantilla_CEAB <- df_plantilla_CEAB %>% group_by(position, gender) %>%
#   summarise(Freq = n())
# df_aux <- df_plantilla_CEAB %>% group_by(position) %>%
#   summarise(total = sum(Freq))
# df_plantilla_CEAB <- merge(df_plantilla_CEAB,df_aux )
# df_plantilla_CEAB$Perc <- (df_plantilla_CEAB$Freq/df_plantilla_CEAB$total)*100
# 
# labs <- paste0(format(df_plantilla_CEAB$Perc, digits = 4), "%")
# 
# ggplot(df_plantilla_CEAB, aes(x=position, y=Freq)) + 
#   geom_bar(aes(position, fill = gender),stat="identity", color = "black")+
#   geom_text(label = labs, vjust=-1) + coord_cartesian(ylim = c(0, 8))

df_fem[which(substr(df_fem$genero,1,1) == "M"),]$genero <- "Male"
df_fem[which(substr(df_fem$genero,1,1) == "W"),]$genero <- "Female"
df_fem[which(substr(df_fem$position_CEAB,1,1) == "E"),]$position_CEAB <- "Early career researcher"
df_fem[which(substr(df_fem$position_CEAB,1,1) == "P"),]$position_CEAB <- "Permanent position"
df_fem[which(substr(df_fem$position_CEAB,1,1) == "R"),]$position_CEAB <- "Research technician"
df_fem[which(substr(df_fem$position_CEAB,1,1) == "A"),]$position_CEAB <- "Administration/Manteinance"

# Delete rows with no answer in gender or position
df_fem <- df_fem[ order(df_fem$position_CEAB),]
df_fem <- df_fem[-1,]
df_fem <- df_fem[ order(df_fem$genero),]
df_fem <- df_fem[-1,]
df_fem <- df_fem[ order(df_fem$position_CEAB),]

ggplot(df_fem) +
  geom_bar(aes(position_CEAB, fill = genero)) +
  theme_bw() + ylim(c(0,80)) +
  ylab("Number") + xlab("Position at CEAB") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_fill_manual(name = "", values=c("#3B429F",
                                        "#AA7DCE"))

df_fem[which(substr(df_fem$organizational_culture,1,1) == "Y"),]$organizational_culture <- "Yes"
df_fem[which(substr(df_fem$organizational_culture,1,1) == "N"),]$organizational_culture <- "No"
df_fem[which(substr(df_fem$organizational_culture,1,1) == "I"),]$organizational_culture <- "I do not know"

ggplot(df_fem) +
  geom_bar(aes(organizational_culture, fill = genero)) +
  theme_bw() + 
  ylab("Number") + xlab("") +
  ggtitle("Is gender equality part of the organizational culture of CEAB? ") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_fill_manual(name = "", values=c("#3B429F",
                                        "#AA7DCE"))

ggplot(df_fem) +
  geom_bar(aes(organizational_culture ,fill = position_CEAB)) +
  theme_bw() + scale_fill_discrete(name = "") + 
  ylab("Number") + xlab("") + ggtitle("Is gender equality part of the organizational culture of CEAB? ")  +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_fill_manual(name = "", values=c("#3B429F",
                                        "#AA7DCE",
                                        "#C7DFC5",
                                        "#F6FEAA"))

df_fem[which(substr(df_fem$personnel_commited,1,1) == "Y"),]$personnel_commited <- "Yes, most people"
df_fem[which(substr(df_fem$personnel_commited,1,1) == "N"),]$personnel_commited <- "No, minority"
df_fem[which(substr(df_fem$personnel_commited,1,1) == "I"),]$personnel_commited <- "I do not know"

ggplot(df_fem) +
  geom_bar(aes(personnel_commited, fill = genero)) +
  theme_bw() + 
  ylab("Number") + xlab("") + ggtitle("Are CEAB personnel committed to gender equality?") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_fill_manual(name = "", values=c("#3B429F",
                                        "#AA7DCE"))

ggplot(df_fem) +
  geom_bar(aes(personnel_commited ,fill = position_CEAB)) +
  theme_bw() + scale_fill_discrete(name = "") + 
  ylab("Number") + xlab("") + ggtitle("Are CEAB personnel committed to gender equality?")  +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_fill_manual(name = "", values=c("#3B429F",
                                        "#AA7DCE",
                                        "#C7DFC5",
                                        "#F6FEAA"))

df_fem[which(substr(df_fem$lenguage_images,1,2) == "I "),]$lenguage_images <- "I do not know"
df_fem[which(substr(df_fem$lenguage_images,1,2) == "It"),]$lenguage_images <- "It is not a collective best practice"
df_fem[which(substr(df_fem$lenguage_images,1,1) == "Y"),]$lenguage_images <- "Yes, it is an institutionalized practice"
df_fem[which(substr(df_fem$lenguage_images,1,1) == "N"),]$lenguage_images <- "No"

ggplot(df_fem) +
  geom_bar(aes(lenguage_images, fill = genero)) +
  theme_bw() +
  ylab("Number") + xlab("") + coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("Are non-gender biased language\n and non-sexist images part of the current \nbest practices towards gender equality at CEAB? ") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(name = "", values=c("#3B429F",
                                        "#AA7DCE"))

ggplot(df_fem) +
  geom_bar(aes(lenguage_images ,fill = position_CEAB)) +
  theme_bw() + scale_fill_discrete(name = "") +
  ylab("Number") + xlab("") + ggtitle("Are non-gender biased language\n and non-sexist images part of the current\n best practices towards gender equality at CEAB? ") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_fill_manual(name = "", values=c("#3B429F",
                                        "#AA7DCE",
                                        "#C7DFC5",
                                        "#F6FEAA"))

df_fem[which(substr(df_fem$workforce_trained,1,1) == "Y"),]$workforce_trained <- "Yes"
df_fem[which(substr(df_fem$workforce_trained,1,1) == "N"),]$workforce_trained <- "No"
df_fem[which(substr(df_fem$workforce_trained,1,1) == "I"),]$workforce_trained <- "I do not know"

df_fem <- df_fem[ order(df_fem$workforce_trained),]
df_fem <- df_fem[-1,]

ggplot(df_fem) +
  geom_bar(aes(workforce_trained, fill = genero)) +
  theme_bw() +
  ylab("Number") + xlab("") +
  ggtitle("Should all CEAB workforce be trained on gender equality? ") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(name = "", values=c("#3B429F",
                                        "#AA7DCE"))

ggplot(df_fem) +
  geom_bar(aes(workforce_trained ,fill = position_CEAB)) +
  theme_bw() + scale_fill_discrete(name = "") +
  ylab("Number") + xlab("") + ggtitle("Should all CEAB workforce be trained on gender equality? ")  +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14)) +
  scale_fill_manual(name = "", values=c("#3B429F",
                                        "#AA7DCE",
                                        "#C7DFC5",
                                        "#F6FEAA"))

dt_fem <- setDT(df_fem)
df <- data.frame(x = rep("Female",
                         sum(dt_fem$sexual_harrasment %like% "CEAB has a Protocol for tackling" 
                             & df_fem$genero == "Female")), y = "CEAB has a Protocol for tackling sexual and gender harassment ")
df <- rbind(df, data.frame(x = rep("Male",
                                   sum(dt_fem$sexual_harrasment %like% "CEAB has a Protocol for tackling" 
                                       & df_fem$genero == "Male")), y = "CEAB has a Protocol for tackling sexual and gender harassment "))
df <- rbind(df, data.frame(x = rep("Female",
                                   sum(dt_fem$sexual_harrasment %like% "CEAB personnel know the existing measures" 
                                       & df_fem$genero == "Female")), y = "CEAB personnel know the existing measures and how to act in case of suffering or being aware of a situation of harassment"))
df <- rbind(df, data.frame(x = rep("Male",
                                   sum(dt_fem$sexual_harrasment %like% "CEAB personnel know the existing measures" 
                                       & df_fem$genero == "Male")), y = "CEAB personnel know the existing measures and how to act in case of suffering or being aware of a situation of harassment"))
df <- rbind(df, data.frame(x = rep("Female",
                                   sum(dt_fem$sexual_harrasment %like% "but it is not used" 
                                       & df_fem$genero == "Female")), y = "CEAB has a protocol for tackling sexual and gender harassment, but it is not used"))
df <- rbind(df, data.frame(x = rep("Male",
                                   sum(dt_fem$sexual_harrasment %like% "but it is not used" 
                                       & df_fem$genero == "Male")), y = "CEAB has a protocol for tackling sexual and gender harassment, but it is not used"))
df <- rbind(df, data.frame(x = rep("Female",
                                   sum(dt_fem$sexual_harrasment %like% "but I do not trust its" 
                                       & df_fem$genero == "Female")), y = "CEAB has a protocol for tackling sexual and gender harassment,  but I do not trust its application."))
df <- rbind(df, data.frame(x = rep("Male",
                                   sum(dt_fem$sexual_harrasment %like% "but I do not trust its" 
                                       & df_fem$genero == "Male")), y = "CEAB has a protocol for tackling sexual and gender harassment,  but I do not trust its application."))
df <- rbind(df, data.frame(x = rep("Female",
                                   sum(dt_fem$sexual_harrasment %like% "Nothing has been done from" 
                                       & df_fem$genero == "Female")), y = "Nothing has been done from CEAB "))
df <- rbind(df, data.frame(x = rep("Male",
                                   sum(dt_fem$sexual_harrasment %like% "Nothing has been done from" 
                                       & df_fem$genero == "Male")), y = "Nothing has been done from CEAB "))
df <- rbind(df, data.frame(x = rep("Female",
                                   sum(dt_fem$sexual_harrasment %like% "I am not aware of any problem" 
                                       & df_fem$genero == "Female")), y = "I am not aware of any problem in this topic"))
df <- rbind(df, data.frame(x = rep("Male",
                                   sum(dt_fem$sexual_harrasment %like% "I am not aware of any problem" 
                                       & df_fem$genero == "Male")), y = "I am not aware of any problem in this topic"))
df <- rbind(df, data.frame(x = rep("Female",
                                   sum(dt_fem$sexual_harrasment %like% "CEAB personnel are sensitized" 
                                       & df_fem$genero == "Female")), y = "CEAB personnel are sensitized in relation to sexual harassment "))
df <- rbind(df, data.frame(x = rep("Male",
                                   sum(dt_fem$sexual_harrasment %like% "CEAB personnel are sensitized" 
                                       & df_fem$genero == "Male")), y = "CEAB personnel are sensitized in relation to sexual harassment "))


ggplot(df) +
  geom_bar(aes(y, fill = x)) +
  theme_bw() +
  ylab("Number") + xlab("") + coord_flip() +                                
  ggtitle("In relation to sexual and gender harassment...") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(name = "", values=c("#3B429F",
                                        "#AA7DCE"))


df_fem[which(substr(df_fem$suffered_seen_harras,1,14) == "Yes, I have su"),]$suffered_seen_harras <- "Yes, I have suffered it"
df_fem[which(substr(df_fem$suffered_seen_harras,1,14) == "Yes, I have se"),]$suffered_seen_harras <- "Yes, I have seen it "
df_fem[which(substr(df_fem$suffered_seen_harras,1,1) == "N"),]$suffered_seen_harras <- "No"
df_fem <- df_fem[ order(df_fem$suffered_seen_harras),]
df_fem <- df_fem[-1,]

ggplot(df_fem) +
  geom_bar(aes(suffered_seen_harras, fill = genero)) +
  theme_bw() +
  ylab("Number") + xlab("") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("From your point of view, in the last four years, \nhave you suffered or seen someone suffering \nsome form of sexual harassment? ") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(name = "", values=c("#3B429F",
                                        "#AA7DCE"))

ggplot(df_fem) +
  geom_bar(aes(suffered_seen_harras ,fill = position_CEAB)) +
  theme_bw() + scale_fill_discrete(name = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ylab("Number") + xlab("") + 
  ggtitle("From your point of view, in the last four years, have you suffered or seen someone suffering some form of sexual harassment? ") 

