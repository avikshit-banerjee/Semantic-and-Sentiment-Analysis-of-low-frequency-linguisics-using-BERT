library(VGAM)
library('tidyverse')
library('tidytext')
library('textstem')
library('ggwordcloud')
library("afex")
library("emmeans")
library("cowplot") 
library("ggplot2") 
library('ggpubr')
library("factoextra")
library('lsa')
library("nnet")
library(mice)
library(VIM)


model.multi <- vglm(form ~ V.Mean.Sum + Conc.M + A.Mean.Sum+aoa_rating,family=multinomial,data=wbr)
summary(model.multi)

test <- multinom(form ~ Conc.M + aoa_rating + V.Mean.Sum + A.Mean.Sum + D.Mean.Sum, data = wbr)
summary(test)
#Effect of participles on Conc. is significant
a.1 <- aov_car(Conc.M ~ form + Error(index), wbr) 

e.1 <- emmeans(a.1, "form")

con.1 <- list(
  ed_vs_ing = c(1,-1,0),
  ed_vs_to = c(1,0,-1),
  ing_vs_to =c(0,1,-1))
contrast(e.1, con.1, adjust = "holm")

p1 <- afex_plot(a.1, "form", mapping = c( "shape", "fill"),
                data_geom = ggplot2::geom_violin, 
                data_arg = list(width = 0.5),
                point_arg = list(size = 3)) +
  geom_line(aes(group = 1), color = "black", size = 0.5)+
  ylab("Concreteness") + xlab("Participle") + theme(legend.position="bottom",
                                                    panel.grid.major.x = element_blank())

#Effect of participles on Conc. 
a.2 <- aov_car(aoa_rating ~ form + Error(index), wbr)

e.2 <- emmeans(a.2, "form")

con.2 <- list(
  ed_vs_ing = c(1,-1,0),
  ed_vs_to = c(1,0,-1),
  ing_vs_to =c(0,1,-1))
contrast(e.2, con.2, adjust = "holm")


p2 <- afex_plot(a.2, "form", mapping = c( "shape", "fill"),
                data_geom = ggplot2::geom_violin, 
                data_arg = list(width = 0.5),
                point_arg = list(size = 3)) +
  geom_line(aes(group = 1), color = "black", size = 0.5)+
  ylab("Age Of Acquisition") + xlab("Participle") + theme(legend.position="bottom",
                                                          panel.grid.major.x = element_blank())


#Effect of participles on Valence
a.3 <- aov_car(V.Mean.Sum ~ form + Error(index), wbr)

e.3 <- emmeans(a.3, "form")

con.3 <- list(
  ed_vs_ing = c(1,-1,0),
  ed_vs_to = c(1,0,-1),
  ing_vs_to =c(0,1,-1))
contrast(e.3, con.3, adjust = "holm")



p3 <-afex_plot(a.3, "form", mapping = c( "shape", "fill"),
               data_geom = ggplot2::geom_violin, 
               data_arg = list(width = 0.5),
               point_arg = list(size = 3)) +
  geom_line(aes(group = 1), color = "black", size = 0.5)+
  ylab("Valence") + xlab("Participle") + theme(legend.position="bottom",
                                               panel.grid.major.x = element_blank())


#Effect of participles on Arousal
a.4 <- aov_car(A.Mean.Sum ~ form + Error(index), wbr) 

e.4 <- emmeans(a.4, "form")

con.4 <- list(
  ed_vs_ing = c(1,-1,0),
  ed_vs_to = c(1,0,-1),
  ing_vs_to =c(0,1,-1))
contrast(e.4, con.4, adjust = "holm")


p4 <-afex_plot(a.4, "form", mapping = c( "shape", "fill"),
               data_geom = ggplot2::geom_violin, 
               data_arg = list(width = 0.5),
               point_arg = list(size = 3)) +
  geom_line(aes(group = 1), color = "black", size = 0.5)+
  ylab("Arousal Rating") + xlab("Participle") + theme(legend.position="bottom",
                                                      panel.grid.major.x = element_blank())


#Effect of participles on Dominance
a.5 <- aov_car(D.Mean.Sum ~ form + Error(tweetID), wbr) 

p5 <-afex_plot(a.5, "form", mapping = c( "shape", "fill"),
               data_geom = ggplot2::geom_violin, 
               data_arg = list(width = 0.5),
               point_arg = list(size = 3)) +
  geom_line(aes(group = 1), color = "black", size = 0.5)+
  ylab("Dominance Rating") + xlab("Participle") + theme(legend.position="bottom",
                                                        panel.grid.major.x = element_blank())

emmeans(a.5, "form")

plot_grid(p1,p2,p3,p4,p5)

#------------------------Inter Form - Norm distribution----------------

wbr_tidy <- wbr %>% group_by(variety) %>% 
  group_by(form, .add = TRUE) %>% 
  summarise(CONC =round(mean(Conc.M, na.rm = TRUE),2) ,
            AOA = round(mean(aoa_rating,na.rm = TRUE ),2),
            VAL = round(mean(V.Mean.Sum, na.rm = TRUE),2),
            ASL = round(mean(A.Mean.Sum, na.rm = TRUE),2))%>%
  pivot_longer(c(3:6), names_to = 'norm')

wbr_tidy.val <- filter(wbr_tidy, norm == 'VAL')
wbr_tidy.con <- filter(wbr_tidy, norm == 'CONC')
wbr_tidy.aoa <- filter(wbr_tidy, norm == 'AOA')
wbr_tidy.asl <- filter(wbr_tidy, norm == 'ASL')

x.1 <- ggplot(data = wbr_tidy.val, aes(x= form, y = value, fill = form))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  coord_cartesian(ylim = c(5.2, 5.7))+
  facet_wrap(~variety)+
  xlab("Forms") + ylab("Valence")


x.2 <- ggplot(data = wbr_tidy.con, aes(x= form, y = value, fill = form))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  coord_cartesian(ylim = c(3.2, 3.5))+
  facet_wrap(~variety)+
  xlab("Forms") + ylab("Concreteness")

x.3 <- ggplot(data = wbr_tidy.aoa, aes(x= form, y = value, fill = form))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  coord_cartesian(ylim = c(6.3,7.5))+
  facet_wrap(~variety)+
  xlab("Forms") + ylab("Age Of Acquisition")

x.4 <- ggplot(data = wbr_tidy.asl, aes(x= form, y = value, fill = form))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  coord_cartesian(ylim = c(4,4.3))+
  facet_wrap(~variety)+
  xlab("Forms") + ylab("Arousal")

plot_grid(x.1,x.2,x.3,x.4, nrow = 4)

#-----city wise valence mean------------************************************************************__________

wbr_UK <- wbr %>% group_by(variety) %>% group_by(City, .add = TRUE)%>%
  summarise(m.conc = mean(Conc.M, na.rm = TRUE))%>%
  filter(variety == 'UK')
wbr_US <- wbr %>% group_by(variety) %>% group_by(City, .add = TRUE)%>%
  summarise(m.conc = mean(Conc.M, na.rm = TRUE))%>%
  filter(variety == 'US')

wbr_UK <-  as.data.frame(wbr_UK) %>% select(2:3)
#wbr_UK$m.conc <- wbr_UK$m.conc/max(wbr_UK$m.conc)


wbr_US <-  as.data.frame(wbr_US) %>% select(2:3)

wbr_UK[2] <- as.data.frame(scale(wbr_UK[2]))
wbr_US[2] <- as.data.frame(scale(wbr_US[2]))

pos.val.UK <- wbr_UK
pos.val.US <- wbr_US

wbr_UK[2] <- wbr_UK[2]*-1 #multiplying by negative to invert the scale
wbr_US[2] <- wbr_US[2]*-1

write.csv(wbr_UK,"~/UK_citywise_Conc.csv", row.names = FALSE)

#--------------------------------Calculating ED proportion -------------------------------------

ED.count.UK <- wbr %>% group_by(variety)%>%
  filter(variety == 'UK')%>%filter(form == 'ED')%>% 
  summarise(count = n()) 
TO.count.UK <- wbr %>% group_by(variety)%>%
  filter(variety == 'UK')%>%filter(form == 'TO')%>% 
  summarise(count = n()) 
ED.count.US <- wbr %>% group_by(variety)%>%
  filter(variety == 'US')%>%filter(form == 'ED')%>% 
  summarise(count = n()) 

ED_prop_UK <- wbr %>% group_by(variety) %>% filter(variety == 'UK')%>%
  group_by(City, .add = TRUE)%>%filter(form == 'ED')%>% 
  summarise(count = n()/ED.count.UK$count) 
TO_prop_UK <- wbr %>% group_by(variety) %>% filter(variety == 'UK')%>%
  group_by(City, .add = TRUE)%>%filter(form == 'TO')%>% 
  summarise(count = n()/TO.count.UK$count) 

ED_prop_US <- wbr %>% group_by(variety) %>% filter(variety == 'US')%>%
  group_by(City, .add = TRUE)%>%filter(form == 'ED')%>% 
  summarise(count = n()/ED.count.US$count) 

ED_prop_UK <-  as.data.frame(ED_prop_UK) %>% select(2:3)
TO_prop_UK <-  as.data.frame(TO_prop_UK) %>% select(2:3)
ED_prop_US <-  as.data.frame(ED_prop_US) %>% select(2:3)

TO_prop_UK[2] <- TO_prop_UK[2]*100
ED_prop_UK[2] <- ED_prop_UK[2]*100
ED_prop_US[2] <- ED_prop_US[2]*100

#write.csv(TO_prop_UK,"/Users/Avit/Desktop/Dissertation/TO_prop_UK.csv", row.names = FALSE)
#write.csv(ED_prop_US,"/Users/Avit/Desktop/Dissertation/ED_prop_US.csv", row.names = FALSE)

#-------------------Correlation between ED prop and Valence------------------------


#General

ED.count <- wbr %>% filter(form == 'ED')%>% 
  summarise(count = n()) 

ED.prop.city <- wbr %>% 
  group_by(City)%>%filter(form == 'ED')%>% 
  summarise(prop = n()/ED.count$count*100) 

val_city <- wbr%>% group_by(City)%>%
  summarise(m.valence = mean(V.Mean.Sum, na.rm = TRUE))

ed.val.data <- right_join(ED.prop.city,val_city, by = 'City' )
ed.val.data[2 : 3] <- as.data.frame(scale(ed.val.data[2 : 3]))

ggscatter(ed.val.data, x = "prop", y = "m.valence", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ED Usage Proportion", ylab = "Mean Valence")


#UK



ed.val.UK <- right_join(wbr_UK,ED_prop_UK, by = 'City' )
ing.val.UK <- right_join(wbr_UK,ING_prop_UK, by = 'City' )

ggscatter(ing.val.UK, x = "count", y = "m.valence", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ED Usage Proportion", ylab = "Mean Valence")



#US
#world




#---------------------ING usage proportion----------------------------------

ING.count.UK <- wbr %>% group_by(variety)%>%
  filter(variety == 'UK')%>%filter(form == 'ING')%>% 
  summarise(count = n()) 
ING.count.US <- wbr %>% group_by(variety)%>%
  filter(variety == 'US')%>%filter(form == 'ING')%>% 
  summarise(count = n()) 

ING.count.UK <- wbr %>% group_by(variety)%>%
  filter(variety == 'UK')%>%filter(form == 'ING')%>% 
  summarise(count = n()) 
ING.count.US <- wbr %>% group_by(variety)%>%
  filter(variety == 'US')%>%filter(form == 'ING')%>% 
  summarise(count = n()) 


ING_prop_UK <- wbr %>% group_by(variety) %>% filter(variety == 'UK')%>%
  group_by(City, .add = TRUE)%>%filter(form == 'ING')%>% 
  summarise(count = n()/ING.count.UK$count) 
ING_prop_US <- wbr %>% group_by(variety) %>% filter(variety == 'US')%>%
  group_by(City, .add = TRUE)%>%filter(form == 'ING')%>% 
  summarise(count = n()/ING.count.US$count) 

ING_prop_UK <-  as.data.frame(ING_prop_UK) %>% select(2:3)
ING_prop_US <-  as.data.frame(ING_prop_US) %>% select(2:3)

ING_prop_UK[2] <- ING_prop_UK[2]*100
ING_prop_US[2] <- ING_prop_US[2]*100

# write.csv(ING_prop_UK,"/Users/Avit/Desktop/Dissertation/ING_prop_UK.csv", row.names = FALSE)
#---------------Inter-user participle Analysis----------------

#subsetting on the basis participles

wbr_ed<- wbr %>% filter(form=='ED')
wbr_to<- wbr %>% filter(form=='TO')
wbr_ing<- wbr %>% filter(form=='ING')

city_ed <- wbr_ed %>% group_by(City) %>% summarise(mean_conc = mean(Conc.M, na.rm = TRUE,),
                                                   mean_aoa = mean(aoa_rating, na.rm = TRUE),
                                                   mean_val = mean(V.Mean.Sum, na.rm = TRUE),
                                                   mean_ar = mean(A.Mean.Sum, na.rm = TRUE))

city_to <- wbr_to %>% group_by(City) %>% summarise(mean_conc = mean(Conc.M, na.rm = TRUE,),
                                                   mean_aoa = mean(aoa_rating, na.rm = TRUE),
                                                   mean_val = mean(V.Mean.Sum, na.rm = TRUE),
                                                   mean_ar = mean(A.Mean.Sum, na.rm = TRUE))
city_ing <- wbr_ing %>% group_by(City) %>% summarise(mean_conc = mean(Conc.M, na.rm = TRUE,),
                                                     mean_aoa = mean(aoa_rating, na.rm = TRUE),
                                                     mean_val = mean(V.Mean.Sum, na.rm = TRUE),
                                                     mean_ar = mean(A.Mean.Sum, na.rm = TRUE))

#Grouping the dataset by countries

city_data <- wbr %>% group_by(City) %>% summarise(mean_conc = mean(Conc.M, na.rm = TRUE,),
                                                        mean_aoa = mean(aoa_rating, na.rm = TRUE),
                                                        mean_val = mean(V.Mean.Sum, na.rm = TRUE),
                                                        mean_ar = mean(A.Mean.Sum, na.rm = TRUE))


#Creating a function to visualize norm vs city plots and clusters
city_norm_plot <- function(data, K = NULL, text = "Norms-City Plots (All Norms)"){
  data.long <- data[order( data[,4] ),]  %>% pivot_longer(2:5,names_to = 'norms')
  norm.plot<-ggplot(data.long, aes(fct_inorder(City), value, 
                                   color = norms, shape = norms, size = 0.5)) +
    geom_point() + geom_hline(aes(yintercept = 0))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
          legend.position="bottom")+
    xlab("City") + ylab("Norm Values")
  
  df <- as.data.frame(data)
  rownames(df) <- df[,1]
  df[,1]<-NULL
  cluster<- eclust(df, "kmeans", nboot = 500, k=K, hc_metric = 'euclidean') #Using K means to group the cities
  print(cluster$cluster)
  clust_df <- as.data.frame(cluster$cluster)%>% rename(cluster.name = 'cluster$cluster')
  
  p <- plot_grid(norm.plot)
  title <- ggdraw() + draw_label(text, fontface='bold')
  plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
}

#Plotting Norm vs City Cluster plots
city_norm_plot(city_data,3)
city_norm_plot(city_ed,3, "ED-Norm")
city_norm_plot(city_ing,3, "ING-Norm")
city_norm_plot(city_to,2, "TO-Norm")

cluster_similarity(as.numeric(ED_norm.cluster[,1]), as.numeric(ING_norm.cluster[,1]) )


################################################INTRA USER PARTICIPLE ANALYSIS########################


#Filtering for users having 3 or more tweets
intra_sp <- txtf %>% group_by(screenName)%>% mutate(n = n())%>% filter(n>=2)

#Creating a comparison table comprising the users and the freq of forms used by them
comp_tab <- as.data.frame(table(intra_sp$screenName, intra_sp$form))%>% 
  pivot_wider(names_from = 'Var2',values_from = 'Freq')%>%
  rename(screenName = Var1)

#Mutating another column to the table for assigning comparison score
comp_tab <- comp_tab %>% mutate(comp = case_when(
  ED != 0 & ING != 0 & TO == 0 ~ "ED/ING",
  ED != 0 & TO != 0 & ING == 0 ~ "ED/TO",
  ING != 0 & TO != 0 & ED == 0 ~ "ING/TO",
  ED != 0 & TO != 0 & ING !=0 ~ "ED/TO/ING",
  TRUE                        ~ ""))

comp_tab <- filter(comp_tab, comp != "")

#Percentage of users with intra-variation of forms
table(comp_tab$comp)/nrow(comp_tab)*100
# ED-ING = 4.16%
# ED-TO = 20.14%
# ING-TO = 73.5%
# ED-ING-TO = 2.19%

#Percentage of users having intra variation of forms
nrow(comp_tab)/nrow(txtf)*100
# 2.498313 %

txtf_intra <- right_join(txtf, comp_tab, by = "screenName" )

#Creating intra word-by-row by tokenising and lemmatising
intra_wbr <- txtf_intra %>% select(-c('ED','ING','TO'))%>%
  unnest_tokens(word, text) %>%
  mutate(word = textstem::lemmatize_words(word))

intra_wbr <- intra_wbr %>% left_join(cnorms, by = "word" )%>%
  left_join(aoanorms, by = "word")%>%
  left_join(vadnorms, by = "word")
intra_wbr <- intra_wbr %>%
  anti_join(stop_words, by = "word")%>%
  anti_join(add_stopw, by = "word")

temp.intra <- mice(intra_wbr[,8 : 12], m=3, maxit=50,meth='pmm',seed=500)
completedData <- complete(temp.intra,1)

#Imputed dataset
intra_wbr[8 : 12] <- as.data.frame(completedData)

#Filtering out ED/ING

ed.ing.intra <- intra_wbr %>% filter(comp == 'ED/ING')

ed.ing.intra$form <- as.factor(ed.ing.intra$form)



ed.ing.intra$form <- factor( ed.ing.intra$form , ordered = FALSE ) #Unordering factors as relevel only works for that
ed.ing.intra$form1 <- relevel(ed.ing.intra$form, ref = "ING")#Specifying the baseline form
test1 <- multinom(form1 ~ V.Mean.Sum + Conc.M + A.Mean.Sum, data = ed.ing.intra)
summary(test1)

#calculating p values
z <- summary(test1)$coefficients/summary(test1)$standard.errors # z scores
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


#Filtering out ED/TO

ed.to.intra <- intra_wbr %>% filter(comp == 'ED/TO')
ed.to.intra$form <- as.factor(ed.to.intra$form)
model <- glm(form ~ V.Mean.Sum + Conc.M + A.Mean.Sum,family=binomial,data=ed.to.intra)

ed.to.intra$form <- factor( ed.to.intra$form , ordered = FALSE ) #Unordering factors as relevel only works for that
ed.to.intra$form1 <- relevel(ed.to.intra$form, ref = "TO")#Specifying the baseline form
test2 <- multinom(form1 ~ V.Mean.Sum + Conc.M + A.Mean.Sum, data = ed.to.intra)
summary(test2)

#calculating p values
z <- summary(test2)$coefficients/summary(test2)$standard.errors # z scores
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p




#----------------Analysis of intra speaker form variance with norm values---

aa.1 <- aov_car(Conc.M ~ comp + Error(tweetID), intra_wbr) 

pp1 <- afex_plot(aa.1, "comp", mapping = c(  "fill"),
                 data_geom = ggplot2::geom_violin, 
                 data_arg = list(width = 0.5),
                 point_arg = list(size = 3)) +
  geom_line(aes(group = 1), color = "black", size = 0.5)+
  ylab("Concreteness") + xlab("Intra-Participle Variations") + theme(legend.position="bottom",
                                                                     panel.grid.major.x = element_blank())


aa.2 <- aov_car(aoa_rating ~ comp + Error(tweetID), intra_wbr) 

pp2 <- afex_plot(aa.2, "comp", mapping = c(  "fill"),
                 data_geom = ggplot2::geom_violin, 
                 data_arg = list(width = 0.5),
                 point_arg = list(size = 3)) +
  geom_line(aes(group = 1), color = "black", size = 0.5)+
  ylab("Age of Acquisition") + xlab("Intra-Participle Variations") + theme(legend.position="bottom",
                                                                           panel.grid.major.x = element_blank())


aa.3 <- aov_car(V.Mean.Sum ~ comp + Error(tweetID), intra_wbr) 

pp3 <- afex_plot(aa.3, "comp", mapping = c(  "fill"),
                 data_geom = ggplot2::geom_violin, 
                 data_arg = list(width = 0.5),
                 point_arg = list(size = 3)) +
  geom_line(aes(group = 1), color = "black", size = 0.5)+
  ylab("Valence") + xlab("Intra-Participle Variations") + theme(legend.position="bottom",
                                                                panel.grid.major.x = element_blank())


aa.4 <- aov_car(A.Mean.Sum ~ comp + Error(tweetID), intra_wbr) 

pp4 <- afex_plot(aa.4, "comp", mapping = c(  "fill"),
                 data_geom = ggplot2::geom_violin, 
                 data_arg = list(width = 0.5),
                 point_arg = list(size = 3)) +
  geom_line(aes(group = 1), color = "black", size = 0.5)+
  ylab("Arousal") + xlab("Intra-Participle Variations") + theme(legend.position="bottom",
                                                                panel.grid.major.x = element_blank())

p <- plot_grid(pp1,pp2,pp3,pp4)
title <- ggdraw() + draw_label('Norm variation across intra-participle usage' , fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

#-----------------------City wise intra particple variation viz

txtf_intra_viz<- txtf_intra %>% select('City','comp')%>%group_by(City) %>% 
  group_by(comp, .add = TRUE)%>%
  summarise(count = n())

# Stacked
ggplot(txtf_intra_viz, aes(fill=comp, y=count, x=City)) + 
  geom_bar(position="fill", stat="identity")+ scale_fill_brewer(palette="Pastel1")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Participle Variation Proportion")+ ggtitle("Participle Usage Graph")


