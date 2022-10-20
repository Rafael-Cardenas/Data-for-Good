library(ggplot2)
library(dplyr)
library(tidyr)

df_pop <- read.csv("population_mvan.csv", header=TRUE)
head(df_pop)
names(df_pop)[names(df_pop) == "Census_Subdivision"] <- "Subdivision"
names(df_pop)[names(df_pop) == "Age_Group"] <- "Age"
head(df_pop)

pop_ra <- df_pop %>% group_by(Year, Age, Subdivision) %>%
          summarise(n = sum(Count)) %>%
          mutate (percentage = n / sum(n)) %>% ungroup %>% 
          complete(Year, Age, fill = list(n = 0, percentage = 0)) 
pop_ra %>% print(n=300) # tibble and also a data frame

write.table(pop_ra, file="pop_ra.csv", sep = ",", row.names=FALSE) #write csv to current directory with 
                                                                   #file name pop_ra

pop <- ggplot(pop_ra, aes(x = factor(Year), y = percentage, fill = Age, group = Age)) +
  geom_area(position = "fill", colour = "black", size = .5, alpha = .7) +
  scale_y_continuous(labels = scales::percent, name="Relative Percentage", expand=c(0,0)) +
  scale_x_discrete(expand=c(0,0), 
                   limits=c('2016', '2021')) + 
  theme_bw() + theme(axis.title.x=element_blank(),
                     axis.text.x=element_text(angle=90),
                     text=element_text(size=13.5), 
                     legend.position='right', 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), 
                     panel.background = element_blank(),
                     panel.spacing.x = unit(4, "mm")) + facet_wrap(~Subdivision) 
                      
                
pop

city_pop <- ggplot(df_pop, aes(x = Subdivision, y = Count, fill = Age)) + geom_col(position="fill") +
       theme_bw() +
       theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + 
       scale_y_continuous(labels = scales::percent,
                          name="Relative Percentage", expand=c(0,0)) + facet_wrap(~Year, scale='free_x') 
city_pop
ggarrange(pop, city_pop + theme(axis.title.y=element_blank()),
          common.legend = TRUE, legend = "right", labels = "AUTO")

df_forecast <- read.csv("Forecast.csv", header=TRUE)
head(df_forecast)

library(ggpubr)

grp1.df <- df_forecast %>% filter(Population < 140000)
grp2.df <- df_forecast %>% filter(Population >= 140000 & Population < 500000) 
grp3.df <- df_forecast %>% filter(Population >= 500000 & Population < 1000000) 
grp4.df <- df_forecast %>% filter(Population >= 1000000) 

mvan_forecast <- ggscatter(df_forecast, x= "Housing", y = "Population", size = "Year", alpha=0.5, repel=TRUE,
                           add = "reg.line",
                           add.params = list(color = "blue", fill = "lightgray"),
                           conf.int = TRUE) + 
  stat_cor(
    aes(label = paste(..r.label.., sep = "~`,`~")),
    p.accuracy = 0.05, r.accuracy = 0.01, hjust=-2, vjust=2, size=6
  ) + 
  stat_regline_equation(label.x = 42, label.y = 3000000, hjust=-1)

mvan_forecast +
  stat_cor(aes(label = paste(..r.label.., sep = "~`,`~")), label.x = 750000, label.y = 10000, data = grp1.df) +
  stat_cor(aes(label = paste(..r.label.., sep = "~`,`~")), label.x = 750000, label.y = 300000, data = grp2.df) + 
  stat_cor(aes(label = paste(..r.label.., sep = "~`,`~")), label.x = 750000, label.y = 750000, data = grp3.df) + 
  stat_cor(aes(label = paste(..r.label.., sep = "~`,`~")), label.x = 750000, label.y = 1000000, data = grp4.df) 

grp1_forecast <- ggscatter(grp1.df, x= "Housing", y = "Population", size = "Year", alpha=0.5, repel=TRUE,
                             add = "reg.line",
                             add.params = list(color = "blue", fill = "lightgray"),
                             conf.int = TRUE, xlim=c(1500, 80000), ylim=c(0, 150000)) + 
                             scale_x_continuous(labels = scientific) + scale_y_continuous(labels = scientific) +
  stat_cor(
    aes(label = paste(..r.label.., sep = "~`,`~")),
    p.accuracy = 0.05, r.accuracy = 0.01, hjust=-0.2, vjust=1, size=6
  ) #+ 
  #stat_regline_equation(label.x = 1600, label.y = 5500, hjust=0, vjust=0)
grp1_forecast

grp2_forecast <- ggscatter(grp2.df, x= "Housing", y = "Population", size = "Year", alpha=0.5, repel=TRUE,
                           add = "reg.line",
                           add.params = list(color = "blue", fill = "lightgray"),
                           conf.int = TRUE, xlim=c(5000, 200000), ylim=c(10000, 500000)) + 
                           scale_x_continuous(labels = scientific) + scale_y_continuous(labels = scientific) +
  stat_cor(
    aes(label = paste(..r.label.., sep = "~`,`~")),
    p.accuracy = 0.05, r.accuracy = 0.01, hjust=1, vjust=-1, size=6
  ) #+ 
  #stat_regline_equation(label.x = 20000, label.y = 400000, hjust=-0.3, vjust=5)
grp2_forecast

require(scales)
grp3_forecast <- ggscatter(grp3.df, x= "Housing", y = "Population", size = "Year", alpha=0.5, repel=TRUE,
                           add = "reg.line",
                           add.params = list(color = "blue", fill = "lightgray"),
                           conf.int = TRUE, xlim=c(100000, 500000),ylim=c(300000, 1000000)) + 
                           scale_x_continuous(labels = scientific) + scale_y_continuous(labels = scientific) +
  stat_cor(
    aes(label = paste(..r.label.., sep = "~`,`~")),
    p.accuracy = 0.05, r.accuracy = 0.01, hjust=1.1, vjust=-1, size=6
  ) #+ 
  #stat_regline_equation(label.x = 20000, label.y = 400000, hjust=-2, vjust=-17)
grp3_forecast

grp4_forecast <- ggscatter(grp4.df, x= "Housing", y = "Population", size = "Year", alpha=0.5, repel=TRUE,
                           add = "reg.line",
                           add.params = list(color = "blue", fill = "lightgray"),
                           conf.int = TRUE, xlim=c(800000, 1600000),ylim=c(2500000, 4000000)) + 
                           scale_x_continuous(labels = scientific) + scale_y_continuous(labels = scientific) +
  stat_cor(
    aes(label = paste(..r.label.., sep = "~`,`~")),
    p.accuracy = 0.05, r.accuracy = 0.01, hjust=1.5, vjust=1, size=6
  ) #+ 
  #stat_regline_equation(label.x = 1000000, label.y = 3750000, hjust=0.5, vjust=0)
grp4_forecast

ggarrange(grp1_forecast + theme(axis.title.x=element_blank()), 
          grp2_forecast + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()), 
          grp3_forecast, grp4_forecast + theme(axis.title.y=element_blank()), 
          nrow=2, ncol=2, align='hv', common.legend = TRUE, legend = "top", labels = "AUTO")
