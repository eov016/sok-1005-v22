setwd("~/Mappeinnlevering1")
#har en "copy" av forskjellige pakker jeg bare laster opp med en gang så
#jeg slipper tenke på det underveis. Bruker nødvendigvis ikke all.
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

#laster inn med fread() funksjon. Deretter gjorde jeg navnene unique
#sjekket class og struktur.
uahncdc_lt_6_0.txt <- fread("http://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt",
                            header = TRUE,
                            sep = "auto",
                            stringsAsFactors = FALSE,
                            fill = TRUE)

names(uahncdc_lt_6_0.txt)<-make.names(names(uahncdc_lt_6_0.txt),unique = TRUE)

sapply(uahncdc_lt_6_0.txt, class)
str(uahncdc_lt_6_0.txt)


# valgte ønskede kolonner og kvittet meg med rader.
# (Viste seg etterhvert at jeg ikke fikk bruk for "Year, "Mo")
lt_txt <- uahncdc_lt_6_0.txt %>% 
  select(Year, Globe, Mo) %>% 
  slice(-c(518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529))
head(lt_txt)
str(lt_txt)

#Konverterer til numeric før jeg bruker rollmean
convert_to_double <- c("Globe", "Year", "Mo")

lt_txt[, convert_to_double] <- lt_txt[, lapply(.SD, as.double), .SDcols = convert_to_double]

str(lt_txt)

convert_to_integer <- c("Year")

lt_txt[, convert_to_integer] <- lt_txt[, lapply(.SD, as.integer), .SDcols = convert_to_integer]

str(lt_txt)


#Lager ny kolonne der jeg regner zoo::rollmean som jeg plasser først
lt_plot<- lt_txt %>%
  mutate(thirteen_avg = rollmean(Globe, 13,
                             align="right",
                             fill=NA)) %>%
  relocate(thirteen_avg)
  str(lt_plot)
##Prøvde unite for å få alle datapunktene med i grafen da Year hadde samme årstall
##12 ganger innafor et år. Konvertering av dette etterpå gav meg NA verdier og jeg løste 
##på en annen måte etter litt tenking og søke-hjelp.  
#lt_plot1<- lt_plot %>% unite("year_m", Year, Mo,  sep='-')
#lt_plot1

#Skrev årene inn til "labels" i scale_x_continuous
manual_years = c("1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985",
                 "1986", "1987", "1988", "1989","1990", "1991", "1992", "1993",
                 "1994", "1995", "1996", "1997", "1998", "1999", "2000","2001",
                 "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                 "2010", "2011","2012", "2013", "2014", "2015", "2016", "2017", 
                 "2018", "2019", "2020", "2021")

 

#Når jeg hadde "Year" på x aksen ble grafen plottet uten mange ønskede
#datapunkter fordi den inneholdt så mange like årstall. Forsøkte unite()
#unite("navn", Year, Mo, sep "-"), men det ville seg ikke. Etter mye tenking
#endte jeg opp med "x = 1:517", da fikk jeg alle verdiene med og grafen ble mer
#flytende. Da måtte jeg gjøre litt ekstra manuelt i scale_x_y_continous.

lt_plot1 %>% 
  ggplot(aes(x = 1:517, group = 1))+
  geom_line(aes(y = Globe), linetype = 1, color ="slateblue4", lineend = "round")+
  geom_point(aes(y = Globe),alpha = 0.30, color = "slateblue4")+
  geom_line(aes(y = thirteen_avg), lwd = 1, lineend ="butt", color = "mediumvioletred") +
  scale_y_continuous(breaks = round(seq(min(lt_plot1$Globe), max(lt_plot1$Globe), by = 0.1),1))+
  scale_x_continuous(name = "", breaks=seq(1,517,12), labels = manual_years)+
  geom_hline(yintercept = 0, alpha = 0.60, color ="darkgray") +
  labs(x = "",
       y = "T Departure from 91`-20` Average\n Deg. Celcius",
       title ="Latest Global Average Trophospheric Temperatures")+
  theme(axis.text.x = element_text(angle = 90, size = 8),
        panel.background = element_rect(fill = "white", colour = "grey85"),
        panel.grid.major = element_line(colour = "gray92"),
        panel.grid.minor = element_line(colour = "gray98"))+
  annotate(geom="text", x=300, y=0.5, label="Running Centered\n13 Month Average",
         color="Black", size = 2.5)+
  annotate(geom = "segment", x = 270, xend = 250, y = 0.4, yend = 0.3,  color ="mediumvioletred",
           arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  annotate(geom="text", x=80, y=0.5, 
           label="UAH Satellite-Based\nTemperature of the\nGlobal Lower Atmosphere\n [Version 6.0]",
           color="slateblue4", size = 3.5)


  geom_segment(aes(x = 3, y = 0.3, xend = 4 , yend = 25),
               arrow = arrow(length = unit(0.2, "cm")))






