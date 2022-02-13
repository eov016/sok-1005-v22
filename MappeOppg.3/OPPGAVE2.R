
#Brukte lm() og ga det navn. Dette fordi jeg ville se hva som inneholdt.
coeff_motor <- lm(Stopp_Km ~ WLTP_Km, data = motor_tabell2)

print(coeff_motor)
#Coefficients:
#  (Intercept)      WLTP_Km  
#-26.6450       0.8671 

#Tallene som er gitt fra lm(), tolker jeg som ett linært gjennomsnitt.
#Med disse tallene, kan jeg ved regning anta den egentlige rekkevidden av hvor langt en el-bil kan nå
#ved å i tilegg bruke gitt WLTP fra produsenten. Fordi så mange el-biler er med i denne testen, gir
#det et godt estimat på hva en kjøper kan forvente av rekkevidde med vinter temperaturer.
# Eksempelvis: Om en produsent oppgir til meg at en gitt el-bil
# har WLTP-rekkevidde på 550km, kan jeg nå regne ut hva jeg kan forvente ved å bruke tallene;
# -26.6450 + 0.8671 * 550 = 450.26 
#I dette eksempelet kan jeg regne med at ved vinter-temperaturer fra 0° til -10°, vil el-bilen
# ha en rekkevidde på ca 450.26.


# Modifiserte plottet fra oppgave1 og la til geom_smooth()
p <- ggplot(motor_tabell2, aes(x = WLTP_Km, y = Stopp_Km, label = Modell))
p + geom_point(position = "identity", fill = "slateblue4", alpha= 0.60, color = "black", size = 1.5, shape = 21)+
  geom_point(data = motor_tabell2[motor_tabell2$Modell == "BYD Tang",],
             fill = "forestgreen", size=2.5, shape =21, color = "forestgreen") +
  geom_point(data = motor_tabell2[motor_tabell2$Modell == "Skoda Enyaq iV80",],
             fill = "red", size=2.5, shape =21, color = "red") +
  geom_point(data = motor_tabell2[motor_tabell2$Modell == "Tesla Model 3 LR Dual motor",],
             fill = "gold2", size=2.5, shape =21, color = "gold2") +
  geom_abline(intercept = 0, color = "red3") +
  geom_smooth(method = lm, lwd = 0.6, color = alpha("lightseagreen", 0.80), fill = alpha("lightseagreen", 0.05))+
  annotate(geom="text", x=360, y=650, label="(Temp. Varierte fra 0° til -10°)",
           color="black", size = 3.0, family = "Times New Roman")+
  annotate(geom = "segment", x = 580, xend = 580, y = 620, yend = 590,  color ="red3",
           arrow = arrow(angle = 15, length = unit(.3,"cm")))+
  annotate(geom="text", x=527, y=635, label="Punktenes avstand fra linja\nillustrerer avvik mellom\nKm angitt og faktiske Km",
           color="red3", size = 3.0, family = "Times New Roman")+
  annotate(geom="text", x=620, y=400, label="Minst Avvik:\nBYD Tang",
           color="forestgreen", size = 3.0, family = "Times New Roman")+
  annotate(geom="text", x=620, y=340, label="Største Avvik:\nSkoda Enyaq iV80",
           color="red", size = 3.0, family = "Times New Roman")+
  annotate(geom="text", x=620, y=370, label="Lengste Rekkevidde:\n Tesla Model 3",
           color="gold2", size = 3.0, family = "Times New Roman")+
  scale_x_continuous(labels = scales::comma, breaks=seq(300,650,50), limits = c(300, 650))+
  scale_y_continuous(labels = scales::comma, breaks=seq(300,650,50), limits = c(300, 650))+
  labs(title = "Test Av Rekkevidde For El-Biler\n 2022 VINTER",
       x = "Worldwide Harmonised Light Vehicle Test Procedure (WLTP) \n Angitt avstand i Km",
       y = "Antall Km før Stopp")+
  theme(plot.title = element_text(face = "bold",hjust = 0.5, family = "Times New Roman"),
        axis.text.y = element_text(hjust = 0, margin = margin(0, -0.7, 0, 0, 'cm')),
        panel.background = element_rect(fill = "white", colour = "grey85"),
        panel.grid.major = element_line(colour = "gray92", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "gray94", linetype = "dotted"),
        panel.border = element_rect(fill = alpha("white", 0.10),color = "gray60", size = 1))
