rm(list=ls()) 
library(magrittr)
library(scales)
library(knitr)
library(lubridate)
library(ggrepel)
library(rvest)
library(rlist)
library(XML)
library(janitor)
library(repurrrsive)
library(tidyverse)

#Jeg begynte med å skrape timeplanen etter å ha valgt alle tre "Emnekodene".
#Forsøkte først med html_nodes('table-primary'), men endte opp med å kjøre url i _read_html()_,
#etterfulgt av html_table(). Brukte janitor::clean_names() og konverterte til vector da det oppstod
#problemer underveis når ting ikke var vectorized.
#Derretter var framgangsmåten min å bruke Purrr map() der jeg valgte listene som inneholdt informasjonen
#som var ønskelig. (Tid, Rom, Emnekode, Beskrivelse, Lærer)
#For å samle ukene som til slutt ble as.Date(Dato) gikk jeg gjennom en prosess som kanskje var i det meste laget der jeg også kopierte Øystein`s koder. 
#Når det er sagt, fikk jeg det ønsket resultat, nemlig as.Date(Dato).
#Brukte cbind til å samle alt, kalt Test_tp, der jeg også omgjorde variabelnavn.
#Innrømmer at oppgaven var for meg litt diffus, men fikk brukt purrr map, og input = url, ble output = timeplanen.

url1<-"https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&View=list&module[]=SOK-1005-1&module[]=SOK-1006-1#week-8"

##
min_timeplan <-function(url1){ 
tp <- read_html(url1) %>% 
  html_table() %>% 
  map(~ clean_names(.)) 
as.vector(tp, mode = "any")
##
Tid<-map(tp, ~.x[["x"]]) %>% unlist() %>% as_tibble()
Rom<-map(tp, ~.x[["x_2"]]) %>% unlist() %>% as_tibble()
Emnekode<-map(tp, ~.x[["x_3"]]) %>% unlist() %>% as_tibble()
Beskrivelse<-map(tp, ~.x[["x_4"]]) %>% unlist() %>% as_tibble()
Lærer<-map(tp, ~.x[["x_5"]]) %>% unlist() %>% as_tibble()
##
timeplan <- tibble(t_p = tp)

Timeplan<-timeplan %>% 
  unnest_wider(t_p) %>% 
  unnest_longer(starts_with("uke")) %>% 
  relocate("x","x_2","x_3","x_4","x_5") %>% 
  unite("Uke", uke_2:uke_23, na.rm = TRUE, remove = TRUE) %>%
  relocate("Uke") %>% 
  separate("Uke", into = c("Dag", "Dato"),
           sep = "(?<=[A-Za-z])(?=[0-9])",
           extra = "merge", fill = "right")  
Timeplan$Dato <- as.Date(Timeplan$Dato, format="%d.%m.%Y") 
##
Test_tp <- cbind(Timeplan$Dato,Tid,Rom,Emnekode,Beskrivelse,Lærer) %>% 
  clean_names(.) %>% 
  rename("Dato"= "timeplan_dato","Tid"="value","Rom"="value_2","Emnekode"="value_3",
         "Beskrivelse"="value_4","Lærer"="value_5")
##
TIMEPLANVÅR2022 <- as.tibble(Test_tp)#konverterer til tibble()
return(TIMEPLANVÅR2022) #siste tibble av timeplanen

}

min_timeplan(url1)



