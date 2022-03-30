# Niveau institutionnel --------------------------------------------------

#1. Importer les données ####
library(readxl)
df <- read_excel("df.pinel.xlsx")
View(df)

#2. Structure des données #### 
library(dplyr)
library(labelled)
library(sjmisc)

##2.1. Période financière (index) ####
frq(df,per)      # OK

##2.2. Dates de débuts et de fin des périodes financières ####
class(df$datedeb) # "POSIXct" "POSIXt"
class(df$datefin) # "POSIXct" "POSIXt"
df$datedeb <- as.Date(df$datedeb, "%Y-%M-%D")
df$datefin <- as.Date(df$datefin, "%Y-%M-%D")
class(df$datedeb) # Date
class(df$datefin) # Date

##2.3. Variable « période COVID » ####
df$cov <- ifelse(df$datefin > as.Date("2020-03-13"),1,0) 
df$cov <- factor(df$cov, levels = c(0,1),labels = c("Pré-COVID","Post-COVID"))
frq(df,cov) # OK

# Vérification
df %>%
  select(per,datefin,cov) %>%
  View() # OK

##2.4. Durée de la période financière ####
library(ggplot2)
frq(df,dureeper) # OK
df %>%
  ggplot(aes(datefin,dureeper)) +
  coord_cartesian(ylim = c(0,max(df$dureeper))) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_point() + 
  theme_classic()
# Toutes les periodes n'ont pas la même durée. 
# Pour les 6 périodes indexées 12 et 13, la durée diffère de 28 jours. 

##2.5. Lits dressés ####
frq(df,litsdres) # OK
df %>%
  ggplot(aes(datefin,litsdres)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_point() + 
  theme_classic()
# Le nombre total de lits dressé passe de 267 à 268 
# entre l'année financière 2019-2020 et l'année financière 2020-2021.

#3. Visualisation des données ####

##3.1. Admissions ####
frq(df,adm)      # OK
# Le nombre d'admission dépend de la durée de la période.
# Le nombre moyens d'admission est une meilleur mesure du nombre d'admission.
df$madm <- df$adm/df$dureeper
df %>%
  ggplot(aes(datefin,madm)) +
  coord_cartesian(ylim = c(0,max(df$madm))) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() 
# On voit une légère tendance à la baisse qui coincide avec le début de la pandémie
df %>%
  ggplot(aes(madm)) +
  geom_density(show.legend = F) +
  theme_classic()
df %>%
  ggplot(aes(madm,col = cov)) +
  geom_density(show.legend = T) +
  theme_classic()

##3.2. Départs ####
frq(df,dep)      # OK
# Le nombre de départ dépend de la durée de la période.
# Le nombre moyens de départ est une meilleur mesure du nombre de départs.
df$mdep <- df$dep/df$dureeper
df %>%
  ggplot(aes(datefin,mdep)) +
  coord_cartesian(ylim = c(0,max(df$mdep))) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() 
# On voit une légère tendance à la baisse.
df %>%
  ggplot(aes(mdep)) +
  geom_density(show.legend = F) +
  theme_classic()
df %>%
  ggplot(aes(mdep,col = cov)) +
  geom_density(show.legend = T) +
  theme_classic()

##3.3. Transferts ####
frq(df,trans1,trans2) 
df %>%
  
# Le nombre moyens de transferts est une meilleur mesure du nombre de transferts.
df$mtrans <- df$trans1/df$dureeper
df %>%
  ggplot(aes(datefin,mtrans)) +
  coord_cartesian(ylim = c(0,max(df$mtrans) + max(df$mtrans)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() 
# On voit une importante augmentation à partir de la période
df %>%
  ggplot(aes(mtrans)) +
  geom_density(show.legend = F) +
  theme_classic()
df %>%
  ggplot(aes(mtrans,col = cov)) +
  geom_density(show.legend = T) +
  theme_classic()

##3.4. Taux d'occupation ####

###3.4.1. Jours-présences ####
frq(df,jrpres)   # OK
# Le nombre jours de présence dépend de la durée de la période.
df$mjrpres <- df$jrpres/df$dureeper
df %>%
  ggplot(aes(datefin,mjrpres)) +
  coord_cartesian(ylim = c(0,max(df$mjrpres) + max(df$mjrpres)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() 
# Calculer le nombre de jours-présence moyen par jours règle le problème.  

###3.4.2. Moyenne de patients par jours ####
# Même chose que le nombre Moyens de Jours-Présence.
# moypat = jrpres/dureeper = mjrpres
frq(df,moypat)
df %>%
  ggplot(aes(datefin,moypat)) +
  coord_cartesian(ylim = c(0,max(df$moypat) + max(df$moypat)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() #OK
df %>%
  ggplot(aes(datefin,mjrpres)) +
  coord_cartesian(ylim = c(0,max(df$mjrpres) + max(df$mjrpres)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() #Pareil
# En comparant à la variable calculée au point 3.7 (mjrpres), on constate qu'on obtient le même résultat.

###3.4.3 Taux d'occupation ####
# Le taux d'occupation correspond au coefficient du nombre de jours-présence observés (jrpres) 
# divisé par le nombre maximum de jours-présence possibles durant la période (jrpresmax = litsdres*dureeper).
df$txoccr <- (df$jrpres)/(df$litsdres*df$dureeper)
frq(df,txocc)   
df %>%
  ggplot(aes(datefin,txoccr)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic()
df %>%
  ggplot(aes(datefin,txocc)) +
  coord_cartesian(ylim = c(0,100)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() # pas OK.
# Une erreur dans le document excel d'origine à la periode 4 de l'année financière 2021-2022.
# La durée de la période a été notée 25 au lieu de 28.
# Il semble y avoir une légère tendance à la baisse. 
df %>%
  ggplot(aes(txoccr)) +
  geom_density(show.legend = F) +
  theme_classic()
df %>%
  ggplot(aes(txoccr,col = cov)) +
  geom_density(show.legend = T) +
  theme_classic()

##3.5. Séjour moyen ####

###3.5.1. Jours d'hospitalisation ####
# Correspond à la somme des Jours-présence de tous les départs. 
df$jrhosr <- ifelse(df$dep == 0,NA,df$jrhos)
frq(df,jrhos)
df %>%
  ggplot(aes(datefin,jrhos)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() # OK
df %>%
  ggplot(aes(datefin,jrhosr)) + 
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() # Pareil

###3.5.2 Séjour moyen ####
# Correspond au nombre de départ diviser par le nombre de jours d'hospitalisation.
df$sejmoyr <- df$jrhos/df$dep
frq(df,sejmoy)
df %>%
  ggplot(aes(datefin,sejmoy)) + 
  coord_cartesian(ylim = c(0,max(df$sejmoy) + max(df$sejmoy)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() # OK
df %>%
  ggplot(aes(datefin,sejmoyr)) + 
  coord_cartesian(ylim = c(0,max(df$sejmoy) + max(df$sejmoy)/4)) +
  geom_vline(aes(xintercept = as.Date("2020-03-13")),linetype = 2) + 
  geom_line() + 
  theme_classic() # OK

# Il semble y avoir un outlier durant la période post-COVID.
df %>%
  ggplot(aes(sejmoy)) +
  geom_density(show.legend = F) +
  theme_classic()
df %>%
  ggplot(aes(sejmoy,col = cov)) +
  coord_cartesian(xlim = c(0,400)) +
  geom_density(show.legend = T) +
  theme_classic()
df$zsejmoy<-scale(df$sejmoy)
df$zsejmoy <-as.numeric(unlist(df$zsejmoy))
frq(df,zsejmoy,sejmoy)
df %>%
  select(per,datefin,cov,sejmoy) %>%
  View()
# sejmoy = 1196 (zscore = 5,89) durant la période Post-COVID.
df$sejmoyrr <- ifelse(df$sejmoyr<500, df$sejmoyr, NA)
df %>%
  select(sejmoy,sejmoyr,sejmoyrr) %>%
  View()

#4. Modélisation statistique ####
library(jmv)
library(ggplot2)

##4.1. Admissions ####
ttestIS(formula = madm ~ cov, data = df,
        ci = T,
        eqv = T,
        norm = T,
        welchs = T,
        mann = T,
        effectSize = T)
df %>%
  ggplot(aes(y = madm, x = cov, col = cov)) +
  coord_cartesian(ylim = c(0,max(df$madm))) +
  geom_jitter(width = .05, show.legend = F) + 
  geom_boxplot(width = .1, position = position_nudge(x = -.25),show.legend = F) +
  stat_summary(fun.data = "mean_cl_normal", position = position_nudge(x = .25), show.legend = F) +
  ylab("Moyenne d'admissions par jour") +
  xlab(NULL) +
  theme_classic()
ggsave(filename = "admplot.png" ,plot = last_plot(),width = 5, height = 5)


##4.2. Départs ####
ttestIS(formula = mdep ~ cov, data = df,
        eqv = T,
        norm = T,
        welchs = T,
        mann = T,
        effectSize = T)
df %>%
  ggplot(aes(y = mdep, x = cov, col = cov)) +
  coord_cartesian(ylim = c(0,max(df$mdep))) +
  geom_jitter(width = .05, show.legend = F) + 
  geom_boxplot(width = .1, position = position_nudge(x = -.25), show.legend = F) +
  stat_summary(fun.data = "mean_cl_normal", position = position_nudge(x = .25), show.legend = F) +
  ylab("Moyenne de départs par jour") +
  xlab(NULL) +
  theme_classic()
ggsave(filename = "depplot.png" ,plot = last_plot(),width = 5, height = 5)

##4.3. Transferts ####
ttestIS(formula = mtrans ~ cov, data = df,
        eqv = T,
        norm = T,
        welchs = T,
        mann = T,
        effectSize = T)
df %>%
  ggplot(aes(y = mtrans, x = cov, col = cov)) +
  coord_cartesian(ylim = c(0,max(df$mtrans))) +
  geom_jitter(width = .05, show.legend = F) + 
  geom_boxplot(width = .1, position = position_nudge(x = -.25), show.legend = F) +
  stat_summary(fun.data = "mean_cl_normal",position = position_nudge(x = .25), show.legend = F) +
  ylab("Moyenne de transferts par jour") +
  xlab(NULL) +
  theme_classic()
ggsave(filename = "transplot.png" ,plot = last_plot(),width = 5, height = 5)

##4.4. Taux d'occupation ####
ttestIS(formula = txoccr ~ cov, data = df,
        eqv = T,
        norm = T,
        welchs = T,
        mann = T,
        effectSize = T)
df %>%
  ggplot(aes(y = txoccr, x = cov, col = cov)) +
  geom_jitter(width = .05, show.legend = F) + 
  geom_boxplot(width = .1, position = position_nudge(x = -.25),show.legend = F) +
  stat_summary(fun.data = "mean_cl_normal", position = position_nudge(x = .25),show.legend = F) +
  ylab("Taux d'occupation moyen par jour") +
  xlab(NULL) +
  theme_classic()
ggsave(filename = "txoccplot.png" ,plot = last_plot(),width = 5, height = 5)

##4.5. Séjour moyen ####
ttestIS(formula = sejmoyr ~ cov, data = df,
        eqv = T,
        norm = T,
        welchs = T,
        mann = T,
        effectSize = T)
df %>%
  ggplot(aes(y = sejmoyr, x = cov, col = cov)) +
  geom_jitter(width = .05, show.legend = F) + 
  geom_boxplot(width = .1, position = position_nudge(x = -.25),show.legend = F) +
  stat_summary(fun.data = "mean_cl_normal", position = position_nudge(x = .25), show.legend = F) +
  ylab("Séjour moyen par période") +
  xlab(NULL) +
  theme_classic()
ggsave(filename = "sejmoyplot.png" ,plot = last_plot(),width = 5, height = 5)
