library(ggplot2)
library(dplyr)
library(ggrepel)
library(viridis)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(ggmap)
library(maps)
library(mapproj)
library(mapdata)
library(cowplot)
library('ggpubr')


# Get the world polygon and extract UK
library(maps)

MAP <-map_data("world")
UK <- map_data("world") %>% filter(region=="UK")
data <- world.cities %>% filter(country.etc=="UK")

df <- read_csv("/Users/Avit/Desktop/Dissertation/UK_citywise_Val.csv")
df1 <- read_csv("/Users/Avit/Desktop/Dissertation/UK_citywise_Sent.csv")
df.ed.UK <- read_csv("/Users/Avit/Desktop/Dissertation/ED_prop_UK.csv")
df.to.UK <- read_csv("/Users/Avit/Desktop/Dissertation/TO_prop_UK.csv")


df <-  df %>% rename(name = City,
                     Negative_Valence = m.valence)
df1 <-  df1 %>% rename(name = City,
                     Negative_Sentiment = m.sent)
df.ed.UK<-  df.ed.UK %>% rename(name = City,
                           Prop = count)
df.to.UK<-  df.to.UK %>% rename(name = City,
                                Prop = count)

df <- right_join(data, df, by = 'name')
df <-  df %>% select(-c(2,3,6))

df1 <- right_join(data, df1, by = 'name')
df1 <-  df1 %>% select(-c(2,3,6))

df.ed.UK <- right_join(data, df.ed.UK, by = 'name')
df.ed.UK <-  df.ed.UK %>% select(-c(2,3,6))

df.to.UK <- right_join(data, df.to.UK, by = 'name')
df.to.UK <-  df.to.UK %>% select(-c(2,3,6))

#####
df.UK <- right_join(df.ed.UK,df1, by = 'name')

ggplot(df.UK, aes(x=Prop, y=Negative_Sentiment)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  stat_cor(method = "pearson", label.x = 4, label.y = 20)


ed.UK <-  df.ed.UK %>% rename(City = name) %>% select(c(1,4))
ed.UK$City <- replace(ed.UK$City, ed.UK$City == 'Birmingham', 'Birmingham_UK')
ed.UK$City <- replace(ed.UK$City, ed.UK$City == 'Newcastle upon Tyne', 'Newcastle')


dff <- left_join(token_text%>%filter(variety == 'UK'), ed.UK, by = 'City')

sent.lvl <- dff
sent.num <- dff

ggscatter(sent.num, x = "Prop", y = "sentiment", aplha= 0.1,
          shape = 21, size = 0.5,
          add = "reg.line", add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

model.multi <- vglm(sentiment ~ Prop,family=multinomial,data=sent.lvl)
summary(model.multi)

sent.lvl$sentiment <- factor( sent.lvl$sentiment , ordered = FALSE ) #Unordering factors as relevel only works for that
sent.lvl$sentiment1 <- relevel(sent.lvl$sentiment, ref = "Neutral")#Specifying the baseline form
test1 <- multinom(sentiment1 ~ V.Mean.Sum, data = sent.lvl)
summary(test1)

#calculating p values
z <- summary(test1)$coefficients/summary(test1)$standard.errors # z scores
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p








########

mybreaks <- c(0.3, 0.4, 0.5, 0.6)

#------------------------------------------UK-------------------------------------------------
#UK_Sent_ED_Bubble

s.1 <- df %>%
  arrange(Negative_Valence) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=Negative_Valence, color= Negative_Valence, alpha=Negative_Valence)) +
  geom_text_repel( data=df , aes(x=long, y=lat, label=name), size=4) +
  scale_size_continuous(name="Negative Valence",range=c(1,30)) +
  scale_alpha_continuous(name="Negative Valence",range=c(.1,.9)) +
  scale_color_viridis(name="Negative Valence",option = 'magma') +
  theme_void() + ylim(50,59) + coord_map() +
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  guides( colour = guide_legend()) +
  ggtitle("Osgood’s Sentiment Analysis of UK cities ")+
  theme(
    legend.position = c(0.85, 0.75),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))




s.2 <- df1 %>%
  arrange(Negative_Sentiment) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=Negative_Sentiment, color= Negative_Sentiment, alpha=Negative_Sentiment)) +
  geom_text_repel( data=df , aes(x=long, y=lat, label=name), size=4) +
  scale_size_continuous(name="Negative Sentiment", breaks=mybreaks,range=c(1,30)) +
  scale_alpha_continuous(name="Negative Sentiment", breaks=mybreaks,range=c(.1,.9)) +
  scale_color_viridis(option = 'magma', breaks=mybreaks, name="Negative Sentiment") +
  theme_void() + ylim(50,59) + coord_map() +
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  guides( colour = guide_legend()) +
  ggtitle("'RoBERTa' Sentiment Analysis of UK cities ")+
  theme(
    legend.position = c(0.85, 0.75),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))



edbreak <- c(5, 20,35, 50)

s.3 <- df.ed.UK%>%
  arrange(Prop) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=Prop, color= Prop, alpha=Prop)) +
  geom_text_repel( data=df.ed.UK , aes(x=long, y=lat, label=name), size=4) +
  scale_size_continuous(name="Proportion %", breaks = edbreak,  range=c(1,30)) +
  scale_alpha_continuous(name="Proportion %",breaks = edbreak, range=c(.1,.9)) +
  scale_color_viridis(option = 'magma', breaks = edbreak,  name="Proportion %") +
  theme_void() + ylim(50,59) + coord_map() +
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  guides( colour = guide_legend()) +
  ggtitle("'ED' participle usage proportion (UK) ")+
  theme(
    legend.position = c(0.85, 0.75),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

plot_grid(s.2,s.3,s.1, nrow = 1,labels = "AUTO",label_size = 20)



#---------------------------------------------US-----------------------------------------------
US <- map_data('state')

df.val.US <- read_csv("/Users/Avit/Desktop/Dissertation/US_citywise_Val.csv")
df.sent.US <- read_csv("/Users/Avit/Desktop/Dissertation/US_citywise_Sent.csv")
df.ed.US <- read_csv("/Users/Avit/Desktop/Dissertation/ED_prop_US.csv")
df.ed.US <- df.ed.US %>% rename(name = City,
                                Prop = count)
df.sent.US <- df.sent.US %>% rename(name = City,
                                sent = m.sent)
df.val.US <- df.val.US %>% rename(name = City,
                                    val = m.valence)

df.ed.US <- right_join(us.cities, df.ed.US, by = 'name')
df.ed.US <-  df.ed.US %>% select(-c(2,3,6))

df.sent.US <- right_join(us.cities, df.sent.US, by = 'name')
df.sent.US <-  df.sent.US %>% select(-c(2,3,6))

df.val.US <- right_join(us.cities, df.val.US, by = 'name')
df.val.US <-  df.val.US %>% select(-c(2,3,6))


#ED usage

z.1 <- df.ed.US %>%
  arrange(Prop) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = US, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=Prop, color= Prop, alpha=Prop)) +
  geom_text_repel(data=df.ed.US , aes(x=long, y=lat, label=name), size=3) +
  scale_size_continuous(name="Proportion %",   range=c(1,30)) +
  scale_alpha_continuous(name="Proportion %", range=c(.1,.9)) +
  scale_color_viridis(option = 'magma',   name="Proportion %") +
  coord_map() + theme_void()+
  guides( colour = guide_legend()) +
  ggtitle("'ED' participle usage proportion (US) ")+
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

#Sentiment 

z.2 <- df.sent.US %>%
  arrange(sent) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = US, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=sent, color= sent, alpha=sent)) +
  geom_text_repel(data=df.sent.US , aes(x=long, y=lat, label=name), size=3) +
  scale_size_continuous(name="Negative Sentiment",   range=c(1,25)) +
  scale_alpha_continuous(name="Negative Sentiment", range=c(.1,.9)) +
  scale_color_viridis(option = 'magma',   name="Negative Sentiment") +
  coord_map() + theme_void()+
  guides( colour = guide_legend()) +
  ggtitle("'RoBERTa' Sentiment Analysis of US cities  ")+
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

#Valence

z.3 <- df.val.US %>%
  arrange(val) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = US, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=val, color= val, alpha=val)) +
  geom_text_repel(data=df.sent.US , aes(x=long, y=lat, label=name), size=3) +
  scale_size_continuous(name="Negative Valence",   range=c(1,25)) +
  scale_alpha_continuous(name="Negative Valence", range=c(.1,.9)) +
  scale_color_viridis(option = 'magma',   name="Negative Valence") +
  coord_map() + theme_void()+
  guides( colour = guide_legend()) +
  ggtitle("Osgood’s Sentiment Analysis of US cities ")+
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))


plot_grid(z.1,z.2,z.1,z.3, nrow = 2, labels = "AUTO",label_size = 20)

#------------------------------ING Usage-------------------------------------------------------
UK <- map_data("world") %>% filter(region=="UK")
data.UK <- world.cities %>% filter(country.etc=="UK")

df.pos.val.UK <- read_csv("/Users/Avit/Desktop/Dissertation/UK_citywise_Pos_Val.csv")
df.pos.val.US <- read_csv("/Users/Avit/Desktop/Dissertation/US_citywise_Pos_Val.csv")

df.pos.sent.UK <- read_csv("/Users/Avit/Desktop/Dissertation/UK_citywise_Pos_Sent.csv")
df.pos.sent.US <- read_csv("/Users/Avit/Desktop/Dissertation/US_citywise_Pos_Sent.csv")

df.ing.UK <- read_csv("/Users/Avit/Desktop/Dissertation/ING_prop_UK.csv")
df.ing.US <- read_csv("/Users/Avit/Desktop/Dissertation/ING_prop_US.csv")

#-------Pre-processing------

##Cleaning
df.ing.UK <- df.ing.UK %>% rename(name = City,
                                Prop = count)
df.pos.sent.UK <- df.pos.sent.UK %>% rename(name = City,
                                            sent = m.sent)
df.pos.val.UK <- df.pos.val.UK %>% rename(name = City,
                                          val = m.valence)


df.ing.US <- df.ing.US %>% rename(name = City,
                                  Prop = count)
df.pos.sent.US <- df.pos.sent.US %>% rename(name = City,
                                            sent = m.sent)
df.pos.val.US <- df.pos.val.US %>% rename(name = City,
                                          val = m.valence)
##Joining

df.ing.UK <- right_join(data.UK, df.ing.UK, by = 'name')
df.ing.UK <-  df.ing.UK %>% select(-c(2,3,6))
df.pos.sent.UK <- right_join(data.UK, df.pos.sent.UK, by = 'name')
df.pos.sent.UK <- df.pos.sent.UK %>% select(-c(2,3,6))
df.pos.val.UK <- right_join(data.UK, df.pos.val.UK, by = 'name')
df.pos.val.UK <-  df.pos.val.UK %>% select(-c(2,3,6))


df.ing.US <- right_join(us.cities, df.ing.US, by = 'name')
df.ing.US <-  df.ing.US %>% select(-c(2,3,6))
df.pos.sent.US <- right_join(us.cities, df.pos.sent.US, by = 'name')
df.pos.sent.US <- df.pos.sent.US %>% select(-c(2,3,6))
df.pos.val.US <- right_join(us.cities, df.pos.val.US, by = 'name')
df.pos.val.US <-  df.pos.val.US %>% select(-c(2,3,6))


#---------Visualisation

# UK

uk.plot.1 <- df.ing.UK %>%
  arrange(Prop) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=Prop, color= Prop, alpha=Prop)) +
  geom_text_repel(data=df.ing.UK , aes(x=long, y=lat, label=name), size=3) +
  scale_size_continuous(name="Proportion %",   range=c(1,25)) +
  scale_alpha_continuous(name="Proportion %", range=c(.1,.9)) +
  scale_color_viridis(option = 'magma',   name="Proportion %") +
  theme_void()+ coord_map() + ylim(50,59) +
  guides( colour = guide_legend()) +
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  ggtitle("'ING' participle usage proportion (UK) ")+
  theme(
    legend.position = c(0.85, 0.75),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))


#Sentiment 

uk.plot.2 <- df.pos.sent.UK %>%
  arrange(sent) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=sent, color= sent, alpha=sent)) +
  geom_text_repel(data=df.pos.sent.UK , aes(x=long, y=lat, label=name), size=3) +
  scale_size_continuous(name="Sentiment",   range=c(1,25)) +
  scale_alpha_continuous(name="Sentiment", range=c(.1,.9)) +
  scale_color_viridis(option = 'magma',   name="Sentiment") +
  coord_map() + theme_void()+  ylim(50,59) +
  guides( colour = guide_legend()) +
  ggtitle("'RoBERTa' Sentiment Analysis of UK cities  ")+
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  theme(
    legend.position = c(0.85, 0.75),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))



#Valence

uk.plot.3 <- df.pos.val.UK %>%
  arrange(val) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=val, color= val, alpha=val)) +
  geom_text_repel(data=df.pos.val.UK , aes(x=long, y=lat, label=name), size=3) +
  scale_size_continuous(name="Valence",   range=c(1,25)) +
  scale_alpha_continuous(name="Valence", range=c(.1,.9)) +
  scale_color_viridis(option = 'magma',   name="Valence") +
  coord_map() + theme_void()+ ylim(50,59) +
  guides( colour = guide_legend()) +
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  ggtitle("Osgood’s Sentiment Analysis of UK cities ")+
  theme(
    legend.position = c(0.85, 0.75),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))


plot_grid(uk.plot.2,uk.plot.1,uk.plot.3, nrow = 1,labels = "AUTO",label_size = 20)





# US
us.plot.1 <- df.ing.US %>%
  arrange(Prop) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = US, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=Prop, color= Prop, alpha=Prop)) +
  geom_text_repel(data=df.ing.US , aes(x=long, y=lat, label=name), size=3) +
  scale_size_continuous(name="Proportion %",   range=c(1,30)) +
  scale_alpha_continuous(name="Proportion %", range=c(.1,.9)) +
  scale_color_viridis(option = 'magma',   name="Proportion %") +
  coord_map() + theme_void()+
  guides( colour = guide_legend()) +
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  ggtitle("'ING' participle usage proportion (US) ")+
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))


#Sentiment 

us.plot.2 <- df.pos.sent.US %>%
  arrange(sent) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = US, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=sent, color= sent, alpha=sent)) +
  geom_text_repel(data=df.pos.sent.US , aes(x=long, y=lat, label=name), size=3) +
  scale_size_continuous(name="Sentiment",   range=c(1,25)) +
  scale_alpha_continuous(name="Sentiment", range=c(.1,.9)) +
  scale_color_viridis(option = 'magma',   name="Sentiment") +
  coord_map() + theme_void()+
  guides( colour = guide_legend()) +
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  ggtitle("'RoBERTa' Sentiment Analysis of US cities  ")+
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))



#Valence

us.plot.3 <- df.pos.val.US %>%
  arrange(val) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = US, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=val, color= val, alpha=val)) +
  geom_text_repel(data=df.pos.val.US , aes(x=long, y=lat, label=name), size=3) +
  scale_size_continuous(name="Valence",   range=c(1,25)) +
  scale_alpha_continuous(name="Valence", range=c(.1,.9)) +
  scale_color_viridis(option = 'magma',   name="Valence") +
  coord_map() + theme_void()+
  guides( colour = guide_legend()) +
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  ggtitle("Osgood’s Sentiment Analysis of US cities ")+
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))


plot_grid(us.plot.1,us.plot.2,us.plot.1,us.plot.3, nrow = 2,labels = "AUTO",label_size = 20)


########################################################################################

df.conc.UK <- read_csv("/Users/Avit/Desktop/Dissertation/UK_citywise_Conc.csv")
df.conc.US <- read_csv("/Users/Avit/Desktop/Dissertation/US_citywise_Conc.csv")

df.conc.UK <- df.conc.UK %>% rename(name = City,
                                          conc = m.conc)
df.conc.UK <- right_join(data.UK, df.conc.UK, by = 'name')
df.conc.UK <-  df.conc.UK %>% select(-c(2,3,6))


df.conc.US <- df.conc.US %>% rename(name = City,
                                    conc = m.conc)
df.conc.US <- right_join(us.cities, df.conc.US, by = 'name')
df.conc.US <-  df.conc.US %>% select(-c(2,3,6))




df.conc.UK %>%
  arrange(conc) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=conc, color= conc, alpha=conc)) +
  geom_text_repel(data=df.pos.val.UK , aes(x=long, y=lat, label=name), size=3) +
  scale_size_continuous(name="Concreteness",   range=c(1,20)) +
  scale_alpha_continuous(name="Concreteness", range=c(.1,.9)) +
  scale_color_viridis(option = 'magma',   name="Concreteness") +
  coord_map() + theme_void()+
  guides( colour = guide_legend()) +
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  ggtitle("Osgood’s Sentiment Analysis of UK cities ")+
  theme(
    legend.position = c(0.85, 0.8),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

###############################################################################################################

mybreaks = c(5, 10, 15, 20)

a <- df.to.UK%>%
  arrange(Prop) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=Prop, color= Prop, alpha=Prop)) +
  geom_text_repel( data=df.to.UK , aes(x=long, y=lat, label=name), size=4) +
  scale_size_continuous(name="Proportion %", breaks = mybreaks,  range=c(1,30)) +
  scale_alpha_continuous(name="Proportion %",breaks = mybreaks, range=c(.1,.9)) +
  scale_color_viridis(option = 'magma', breaks = mybreaks,  name="Proportion %") +
  theme_void() + ylim(50,59) + coord_map() +
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  guides( colour = guide_legend()) +
  ggtitle("'TO' participle usage proportion (UK) ")+
  theme(
    legend.position = c(0.85, 0.75),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))


b <- df.ing.UK %>%
  arrange(Prop) %>% 
  mutate( name=factor(name, unique(name))) %>% 
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.7) +
  geom_point( aes(x=long, y=lat, size=Prop, color= Prop, alpha=Prop)) +
  geom_text_repel(data=df.ing.UK , aes(x=long, y=lat, label=name), size=3) +
  scale_size_continuous(name="Proportion %",   range=c(1,25)) +
  scale_alpha_continuous(name="Proportion %", range=c(.1,.9)) +
  scale_color_viridis(option = 'magma',   name="Proportion %") +
  theme_void()+ coord_map() + ylim(50,59) +
  guides( colour = guide_legend()) +
  panel_border(color = "grey85", size = 2, linetype = 1, remove = FALSE)+
  ggtitle("'ING' participle usage proportion (UK) ")+
  theme(
    legend.position = c(0.85, 0.75),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
                              margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

c <- ggplot(df, aes(x=to, y=ing)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  stat_cor(method = "pearson", label.x = 4, label.y = 20)+
  theme_ipsum()+ xlab("NEED+TO Usage Proportion % - UK") + ylab("NEED+ING Usage Proportion % - UK")


plot_grid(a,c,b, nrow = 1,labels = "AUTO",label_size = 20)

df <- tibble( to = df.to.UK$Prop,
                  ing =  df.ing.UK$Prop)




