#Utfyllende svar gjort i Rmarkdown, se pdf.

lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = NYT_cov_data)

#Coefficients:
#(Intercept)  fully_vaccinated_pct_of_pop  
#31.15                       -36.66  

p <- ggplot(NYT_cov_data, aes(x=fully_vaccinated_pct_of_pop, 
                              y = deaths_per_100k, label = name))
p + geom_point(position = "identity", fill = "mediumvioletred", alpha= 0.60, color = "black", size = 2.8, shape = 21)+
  geom_text(vjust = 0, nudge_y = 0.5, size = 2.5, color = "gray60")+
  geom_smooth(method = lm, lwd = 0.6, color = alpha("lightseagreen", 0.70), fill = alpha("lightseagreen", 0.05))+
  scale_x_continuous(labels = scales::percent, breaks=seq(0.45,1.00,0.05), limits = c(0.45, 0.80))+
  scale_y_continuous(name = "",limits = c(0, 20, 4))+
  labs(title = "Covid-19 deaths since universal adult vaccine eligibility compared with\n vaccineration rates",
       x = "Share of total population fully vaccinated",
       y = "")+
  annotate(geom="text", x=0.50, y=20, label="Avg. Monthly Deaths per 100.000",
           color="gray60", size = 3.5)+
  annotate(geom = "segment", x = 0.60, xend = 0.59, y = 17.5, yend = 18.5,  color ="gray50",
           arrow = arrow(angle = 10, length = unit(.3,"cm")))+
  annotate(geom="text", x=0.64, y=18, label="Lower Vaccination Rate\nHigher Death Rate",
           color="gray60", size = 3.5)+
  annotate(geom = "segment", x = 0.70, xend = 0.71, y = 10, yend = 8.9,  color ="gray50",
           arrow = arrow(angle = 10, length = unit(.3,"cm")))+
  annotate(geom="text", x=0.74, y=10, label="Higher Vaccination Rate\nLower Death Rate",
           color="gray60", size = 3.5)+
  annotate(geom="text", x=0.62, y=-0.0, label="Share of total population fully vaccinated",
           color="gray60", size = 3.5)+
  theme(plot.title = element_text(face = "bold",hjust = 0.5),
        axis.title.x = element_text(hjust = 0.9, size =5, color = "gray98"),
        axis.text = element_text(colour = "gray60"),
        axis.text.y = element_text(hjust = 0, margin = margin(0, -0.7, 0, 0, 'cm')),
        panel.background = element_rect(fill = "white", colour = "grey85"),
        panel.grid.major = element_line(colour = "gray92", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "gray94", linetype = NULL),
        panel.border = element_rect(fill = alpha("white", 0.10),color = "gray60", size = 1))
