
#packages
library(tidyverse)
library(wesanderson)

#data
setwd("/home/samira/Git/2021mt_negcop")
data <- read_tsv("data.csv")


#total and avg scores
data$total <- rowSums(data[ , c(10:33)], na.rm=TRUE)
data$avg <- (data$total / 24)

#remove proficiency question
no_prof <- data[-1,]

#average score of correct vs. incorrect examples
correct <- no_prof[(no_prof$correct == "yes"),]
incorrect <- no_prof[(no_prof$correct == "no"),]

correct_avg <- (sum(correct$avg)/55)
incorrect_avg <- (sum(incorrect$avg)/33)

#plot all auxiliaries with all subjects
q <- ggplot(no_prof, aes(subject_en, total)) + 
  geom_line(aes(colour = aux, group = aux))+
  geom_point(aes(colour = aux))+
  theme_classic()

q + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#plot all auxiliaries with all subjects by number
q <- no_prof %>%
  mutate(value = fct_reorder(subject_en, desc(avg))) %>%
  ggplot(aes(x=value, y=avg))+
  geom_line(aes(colour = aux, group = aux))+
  geom_point(aes(colour = aux))+
  theme_classic()+
  facet_grid(~number)

q + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#plot only auxiliaries of the innovative system
innov <- no_prof[(no_prof$system == "innovative"),]

q2 <- innov %>%
  mutate(subject = fct_reorder(subject_en, desc(avg))) %>%
  ggplot(aes(x=subject, y=avg))+
  geom_line(aes(colour = aux, group = aux))+
  geom_point(aes(colour = aux))+
  scale_color_manual(values=wes_palette(n=3, name="Moonrise3"))+
  theme_classic()

q2 + theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))

#plot only animate forms
an <- no_prof %>%
  filter(aux %in% c("лъич1и", "рук1ич1а"))

q3 <- ggplot(an, aes(fct_reorder(subject_en, desc(avg)), avg)) + 
  geom_line(aes(colour = aux, group = aux))+
  geom_point(aes(colour = aux))+
  scale_color_manual(values=c("#006B3C", "#8A2BE2"))+
  theme_classic()

q3 + theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1),
           axis.title.x=element_blank(), 
           axis.title.y=element_blank())

#score of individual auxiliaries overall
overall_aux <- no_prof %>%
  select(aux)%>%
  group_by(aux)%>%
  summarise(nr = n())

overall_aux_score <- no_prof %>%
  select(aux, avg) %>%
  group_by(aux) %>%
  summarise(score = sum(avg))

overall_aux$score <- overall_aux_score$score
overall_aux$avg_score <- (overall_aux$score / overall_aux$nr)

#overall score with animates/inanimates
aux_nr <- no_prof %>%
  select(aux, animate)%>%
  group_by(aux, animate) %>%
  summarise(nr = n())

aux_score <- no_prof %>%
  select(aux, animate, avg)%>%
  group_by(aux, animate) %>%
  summarise(score = sum(avg))

aux_score$nr <- aux_nr$nr
aux_score$avg_score <- (aux_score$score / aux_score$nr)


#score of хуч1и by animacy
xu <- no_prof[(no_prof$aux == "хуч1и"),]

xu_an_nr <- xu %>%
  select(aux, animate, avg) %>%
  group_by(aux, animate) %>%
  summarise(nr=n())

xu_score <- xu %>%
  select(aux, animate, avg) %>%
  group_by(aux, animate) %>%
  summarise(score=sum(avg))

xu_score$nr <- xu_an_nr$nr
xu_score$avg_score <- (xu_score$score / xu_score$nr)

#score of лъич1и
li <- no_prof[(no_prof$aux == "лъич1и"),]

li <- li %>%
  select(subject_en, number, avg)

#write.csv(li, "li_avg_scores.csv")

#score of рук1ич1а
ruki <- no_prof[(no_prof$aux == "рук1ич1а"),]

ruki <- ruki %>%
  select(subject_en, avg)

# score of inherited system
inherit <- no_prof[(no_prof$system == "inherited"),]

q4 <- inherit %>%
  mutate(subject = fct_reorder(subject_en, desc(avg))) %>%
  ggplot(aes(x=subject, y=avg))+
  geom_line(aes(colour = aux, group = aux))+
  geom_point(aes(colour = aux))+
  theme_classic()+
  facet_grid(~number)

q4 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

inherit_nr <- inherit %>%
  select(correct, number, avg) %>%
  group_by(correct, number) %>%
  summarise(nr = n())

inherit_score <- inherit %>%
  select(correct, number, avg) %>%
  group_by(correct, number) %>%
  summarise(score_total = sum(avg))

inherit_score$nr <- inherit_nr$nr
inherit_score$avg_score <- (inherit_score$score_total / inherit_score$nr)

#inherited score by number
inheritcop_nr <- inherit %>%
  select(aux, correct, number, avg) %>%
  group_by(aux, correct, number) %>%
  summarise(nr = n())

inheritcop_score <- inherit %>%
  select(aux, correct, number, avg) %>%
  group_by(aux, correct, number) %>%
  summarise(score_total = sum(avg))

inheritcop_score$nr <- inheritcop_nr$nr
inheritcop_score$avg_score <- (inheritcop_score$score_total / inheritcop_score$nr)
