rm(list=ls())

####Dependencies####
library(tidyverse)
library(ggplot2)


Path <- "~/Documentos/PHD/2023/Feminismo/gender_eq_survey.csv"
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

df_names_col_def <- data.frame(name_col = col_names, description = colnames(df_fem) )
colnames(df_fem) <- col_names
df_plantilla_CEAB <- data.frame(position = character(), gender = characher())
df <- data.frame(x = rep("Principal researcher", 9))
df <- rbind(df, data.frame(x = rep("Posdoctoral researchers", 4)))
df <- rbind(df, data.frame(x = rep("Predoctoral researchers", 9)))
df <- rbind(df, data.frame(x = rep("Researcher technicians", 33)))
df <- rbind(df, data.frame(x = rep("Administratin staff", 6)))
df <- rbind(df, data.frame(x = rep("Visiting students", 20)))
df$gender <- "female"
  
df2 <- data.frame(x = rep("Principal researcher", 15))
df2 <- rbind(df2, data.frame(x = rep("Posdoctoral researchers", 6)))
df2 <- rbind(df2, data.frame(x = rep("Predoctoral researchers", 5)))
df2 <- rbind(df2, data.frame(x = rep("Researcher technicians", 40)))
df2 <- rbind(df2, data.frame(x = rep("Administratin staff", 5)))
df2 <- rbind(df2, data.frame(x = rep("Visiting students", 16)))
df2$gender <- "male"

df_plantilla_CEAB <- rbind(df,df2)
colnames(df_plantilla_CEAB) <- c("position","gender")


ggplot(df_plantilla_CEAB) +
  geom_bar(aes(position, fill = gender))+
  theme_bw() + 
  ylab("Numero de personas") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
