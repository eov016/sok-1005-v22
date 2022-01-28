setwd("~/Mappeinnlevering1")
library(readr)
library(data.table)
library(dplyr)
library(tidyr)
library(tidyselect)
library(magrittr)
library(tibble)
library(ggplot2)
library(zoo)
library(lubridate)
library(xts)
library(forecast)
library(reshape2)
library(scales)
library(tidyverse)
#Lastet in datasettene en etter en.
uahncdc_lt_6_0.txt <- fread("http://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt",
                            header = TRUE,
                            sep = "auto",
                            stringsAsFactors = FALSE,
                            fill = TRUE)

names(uahncdc_lt_6_0.txt)<-make.names(names(uahncdc_lt_6_0.txt),unique = TRUE)

uahncdc_mt_6_0.txt <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt",
                            header = TRUE,
                            sep = "auto",
                            stringsAsFactors = FALSE,
                            fill = TRUE)

names(uahncdc_mt_6_0.txt)<-make.names(names(uahncdc_mt_6_0.txt),unique = TRUE)

uahncdc_tp_6_0.txt <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt",
                            header = TRUE,
                            sep = "auto",
                            stringsAsFactors = FALSE,
                            fill = TRUE)

names(uahncdc_tp_6_0.txt)<-make.names(names(uahncdc_tp_6_0.txt),unique = TRUE)

uahncdc_ls_6_0.txt <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt",
                            header = TRUE,
                            sep = "auto",
                            stringsAsFactors = FALSE,
                            fill = TRUE)

names(uahncdc_ls_6_0.txt)<-make.names(names(uahncdc_ls_6_0.txt),unique = TRUE)

#kvitter meg med unødvendig kolonner og rader i alle datasettene
lt_raw <- select(uahncdc_lt_6_0.txt, -c(USA48, USA49, AUST))
str(lt_raw)

lt_raw1 <- lt_raw %>% slice(-c(518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529))

mt_raw <- select(uahncdc_mt_6_0.txt, -c(USA48, USA49, AUST))
str(mt_raw)

mt_raw1 <- mt_raw %>% slice(-c(518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529))

tp_raw <- select(uahncdc_tp_6_0.txt, -c(USA48, USA49, AUST))
str(tp_raw)

tp_raw1 <- tp_raw %>% slice(-c(518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529))

ls_raw <- select(uahncdc_ls_6_0.txt, -c(USA48, USA49, AUST))
str(ls_raw)

ls_raw1 <- ls_raw %>% slice(-c(518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529))

#Velger ønskelige kolloner

lt_<-lt_raw1 %>% 
  select(Year, Mo, NoPol)
mt_<-mt_raw1 %>% 
  select(NoPol)
tp_<-tp_raw1 %>% 
  select(NoPol)
ls_<-ls_raw1 %>% 
  select(NoPol)

#Samlet datasettene til ett ved hjelp av cbind()
#Deretter gjorde jeg om på navnene på kolonnene.
all_<- cbind(lt_, mt_,tp_,ls_)
head(all_)
str(all_)
view(all_)
names(all_)<-make.names(names(all_),unique = TRUE)
view(all_)

all_1<-rename(all_, NoPol_lt_ = NoPol,
       NoPol_mt_ = NoPol.1,
       NoPol_tp_ = NoPol.2,
       NoPol_ls_ = NoPol.3)

#Ville ha en tibble, deretter konverterte jeg de til numeric
as_tibble(all_1)

convert_to_numeric <- c("NoPol_lt_", "NoPol_mt_", "NoPol_tp_", "NoPol_ls_")

all_1[, convert_to_numeric] <- all_1[, lapply(.SD, as.numeric), .SDcols = convert_to_numeric]

#Utførte rowmeans() av alle NoPol kolonnene. Resultatet til ny kolonne
#kalt "Avg_NoPol".
#Deretter kjørte jeg rollmean av Avg_NoPol som jeg kalte for "thirteen_avg_NoPol"
all_2<-mutate(all_1, Avg_NoPol = rowMeans(select(all_1, ends_with("_")), na.rm = TRUE))

all_plot<- all_2 %>% mutate(thirteen_avg_NoPol = rollmean(Avg_NoPol, 13,
                                               align="right",
                                               fill=NA)) %>%
  relocate(thirteen_avg_NoPol)
str(all_plot)
view(all_plot)

                
#Jeg forsøkte forskjellige plot for å se hva som så mest oversiktelig ut.
#Endte opp med at geom_line() som for meg hvertfall var lettest å lese av.
#Prøvde å holde meg på samme linje visuelt som i Oppg1.
#NB:Denne grafen må sees på stor skjerm
all_plot %>% 
  ggplot(aes(x = 1:517, group = 1))+
  geom_line(aes(y = NoPol_lt_), linetype = 1, lwd = 0.3, color ="royalblue4", lineend = "round")+
  geom_line(aes(y = NoPol_mt_), linetype = 1, lwd = 0.3, color ="darkorchid4", lineend = "round")+
  geom_line(aes(y = NoPol_tp_), linetype = 1, lwd = 0.3, color ="darkseagreen4", lineend = "round")+
  geom_line(aes(y = NoPol_ls_), linetype = 1, lwd = 0.3, color ="skyblue3", lineend = "round")+
  geom_line(aes(y = thirteen_avg_NoPol),linetype = 1, lwd = 0.7, lineend ="butt", color = "mediumvioletred") +
  scale_y_continuous(breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-2.5,-1.5,-1.0,-0.5,0.0,
                                           0.5,1.0,1.5,2.0,2.5,3,4,5,6,7,8,9)) +
  scale_x_continuous(name = "", breaks=seq(1,517,12), labels = manual_years)+
  geom_hline(yintercept = 0, alpha = 0.60, color ="darkgray") +
  labs(x = "",
       y = "T Departure from 91`-20` Average\n Deg. Celcius",
       title ="Latest Average Temperatures in\nThe North Polar Temperate Zone")+
  theme(axis.text.x = element_text(angle = 90, size = 7),
        axis.text.y = element_text(size = 7),
        panel.background = element_rect(fill = "white", colour = "grey85"),
        panel.grid.major = element_line(colour = "gray92"),
        panel.grid.minor = element_line(colour = "gray98"))+
  annotate(geom="text", x=170, y=-6.0, label="Running Centered\n13 Month Average",
           color="Black", size = 2.5)+
  annotate(geom = "segment", x = 150, xend = 150, y = -5.0, yend = -1.0,  color ="mediumvioletred",
           arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  annotate(geom="text", x=190, y=8.0, 
           label="UAH Satellite-Based\nTemperature of the\nNorthern Polar Temperate Zone\n[Version 6.0]",
           color="slateblue4", size = 3.0)+
  annotate(geom="text", x=450, y=-6.5, 
           label="-Lower Trophospshere-",
           color="royalblue4", size = 2.5)+
  annotate(geom="text", x=441, y=-7.0, 
           label="-Mid Troposphere-",
           color="darkorchid4", size = 2.5)+
  annotate(geom="text", x=433, y=-7.4,
           label="-Tropopause-",
           color="darkseagreen4", size = 2.5)+
  annotate(geom="text", x=446, y=-7.8, 
           label="-Lower Stratosphere-",
           color="skyblue3", size = 2.5)
















