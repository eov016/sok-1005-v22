library(rvest)
library(httr)
library(xml2)
library(magrittr)
library(ggplot2)
library(extrafont)
library(stringr)
library(tidyverse)
loadfonts(device = "pdf")

# Startet web-scraping ved å lese "adressen"
motor_scrap<- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

# Ved å "inspisere element" som jeg highlightet (test tallene), fant jeg fram til hvor datasettet begynte.
# Da kopierte jeg xpath ved å markerere og høyreklikke. 
motor_table<- motor_scrap %>% 
  html_nodes(xpath="/html/body/article/section/div[4]/div[4]/div[1]/table") %>% 
  html_table()
# Ville se hva jeg nå hadde.
head(motor_table)
str(motor_table)

# Etter å ha sett igjennom, valgte jeg [[1]], som jeg kalte motor_tabell.
motor_tabell <- motor_table[[1]]

head(motor_tabell)
# Endret kolonne navnene fra X1, X2, X3, X4 til ønsket kolonnenavn.
names(motor_tabell)[1] <- "Modell"
names(motor_tabell)[2] <- "WLTP"
names(motor_tabell)[3] <- "Stopp_Km"
names(motor_tabell)[4] <- "Avvik_pros"

# Fjernet første rad fra alle kollonnene.(Dette var kolonnenavnene)
motor_tabell1 <- motor_tabell[-(1:1),]

head(motor_tabell1)

# For å kunne bruke WLTP tallene delte jeg opp verdiene.
motor_tabell1$WLTP_Km <- sapply(strsplit(as.character(motor_tabell1$WLTP),"/"), "[", 1)

# Deretter fjernet jeg tekst fordi jeg ville bare så igjen med tallene, som da 
# ble gjort om til numeric
motor_tabell2 <- motor_tabell1 %>% 
  mutate_at("WLTP_Km", str_replace, "km", "") %>%
  mutate_at("Avvik_pros", str_replace, "%", "") %>%
  mutate_at("Stopp_Km", str_replace, "km", "")

motor_tabell2$WLTP_Km <- as.numeric(motor_tabell2$WLTP_Km)
motor_tabell2$Stopp_Km <- as.numeric(motor_tabell2$Stopp_Km)
head(motor_tabell2)

# Plottet grafen. Her forsøkte jeg å gjøre det oversiktelig og informasjonsrikt.
# Tok meg friheten til å markere "minste avvik", "største avvik", og "lengste rekkevidde".
p <- ggplot(motor_tabell2, aes(x = WLTP_Km, y = Stopp_Km, label = Modell))
p + geom_point(position = "identity", fill = "slateblue4", alpha= 0.60, color = "black", size = 1.5, shape = 21)+
  geom_point(data = motor_tabell2[motor_tabell2$Modell == "BYD Tang",],
             fill = "forestgreen", size=2.5, shape =21, color = "forestgreen") +
  geom_point(data = motor_tabell2[motor_tabell2$Modell == "Skoda Enyaq iV80",],
             fill = "red", size=2.5, shape =21, color = "red") +
  geom_point(data = motor_tabell2[motor_tabell2$Modell == "Tesla Model 3 LR Dual motor",],
             fill = "gold2", size=2.5, shape =21, color = "gold2") +
  geom_abline(intercept = 0, color = "red3") +
  annotate(geom="text", x=270, y=700, label="(Temp. Varierte fra 0° til -10°)",
           color="black", size = 3.0, family = "Times New Roman")+
  annotate(geom = "segment", x = 580, xend = 580, y = 620, yend = 590,  color ="red3",
           arrow = arrow(angle = 15, length = unit(.3,"cm")))+
  annotate(geom="text", x=560, y=675, label="Punktenes avstand fra linja\nillustrerer avvik mellom\nKm angitt og faktiske Km",
           color="red3", size = 3.0, family = "Times New Roman")+
  annotate(geom="text", x=650, y=350, label="Minst Avvik:\nBYD Tang",
           color="forestgreen", size = 3.0, family = "Times New Roman")+
  annotate(geom="text", x=650, y=250, label="Største Avvik:\nSkoda Enyaq iV80",
           color="red", size = 3.0, family = "Times New Roman")+
  annotate(geom="text", x=650, y=300, label="Lengste Rekkevidde:\n Tesla Model 3",
           color="gold2", size = 3.0, family = "Times New Roman")+
  scale_x_continuous(labels = scales::comma, breaks=seq(200,700,100), limits = c(200, 700))+
  scale_y_continuous(labels = scales::comma, breaks=seq(200,700,100), limits = c(200, 700))+
  labs(title = "Test Av Rekkevidde For El-Biler\n 2022 VINTER",
       x = "Worldwide Harmonised Light Vehicle Test Procedure (WLTP) \n Angitt avstand i Km",
       y = "Antall Km før Stopp")+
  theme(plot.title = element_text(face = "bold",hjust = 0.5, family = "Times New Roman"),
        axis.text.y = element_text(hjust = 0, margin = margin(0, -0.7, 0, 0, 'cm')),
        panel.background = element_rect(fill = "white", colour = "grey85"),
        panel.grid.major = element_line(colour = "gray92", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "gray94", linetype = "dotted"),
        panel.border = element_rect(fill = alpha("white", 0.10),color = "gray60", size = 1))
        

