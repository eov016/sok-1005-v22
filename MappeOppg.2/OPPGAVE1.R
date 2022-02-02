setwd("/Users/erlendoverli/sok-1005-v22")
library(jsonlite)
library(rjson)
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
library(RJSONIO)
library(tidyverse)

url <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")
cov_json_res <- read_json(url)
str(cov_json_res)

cov_json <- lapply(cov_json_res, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

NYT_cov_data <-as.data.frame(do.call("rbind", cov_json))
str(NYT_cov_data)
class(NYT_cov_data)
head(NYT_cov_data)

rename(NYT_cov_data, "ful_vax_pct_" = fully_vaccinated_pct_of_pop)

as_tibble(NYT_cov_data)
NYT_cov_data$fully_vaccinated_pct_of_pop <- 
  as.numeric(as.character(NYT_cov_data$fully_vaccinated_pct_of_pop))
NYT_cov_data$deaths_per_100k <- 
  as.numeric(as.character(NYT_cov_data$deaths_per_100k))
NYT_cov_data$deaths_before <- 
  as.numeric(as.character(NYT_cov_data$deaths_before))
NYT_cov_data$deaths <- 
  as.numeric(as.character(NYT_cov_data$deaths))
str(NYT_cov_data)
as_tibble(NYT_cov_data)


convert_to_numeric <- c("ful_vax_pct_", "deaths_per_100k", "deaths_before", "deaths")

NYT_cov_data[, convert_to_numeric] <- NYT_cov_data[, lapply(.SD, as.numeric), .SDcols = convert_to_numeric]

str(NYT_cov_data)

NYT_cov_data$name[NYT_cov_data$name == "Wyoming"] <- "Wyo."
NYT_cov_data$name[NYT_cov_data$name == "West Virginia"] <- "W.Vi."
NYT_cov_data$name[NYT_cov_data$name == "Montana"] <- "Mont."
NYT_cov_data$name[NYT_cov_data$name == "Idaho"] <- "Ida."
NYT_cov_data$name[NYT_cov_data$name == "Alabama"] <- "Ala."
NYT_cov_data$name[NYT_cov_data$name == "Mississippi"] <- "Miss."
NYT_cov_data$name[NYT_cov_data$name == "Louisiana"] <- "Lou."
NYT_cov_data$name[NYT_cov_data$name == "Tennessee"] <- "Tenn."
NYT_cov_data$name[NYT_cov_data$name == "Indiana"] <- "Ind."
NYT_cov_data$name[NYT_cov_data$name == "North Dakota"] <- "N.Da."
NYT_cov_data$name[NYT_cov_data$name == "North Carolina"] <- "N.Ca."
NYT_cov_data$name[NYT_cov_data$name == "Kansas"] <- "Kan."
NYT_cov_data$name[NYT_cov_data$name == "Hawaii"] <- "Haw."
NYT_cov_data$name[NYT_cov_data$name == "Illinois"] <- "Ill."
NYT_cov_data$name[NYT_cov_data$name == "Wisconsin"] <- "Wis."
NYT_cov_data$name[NYT_cov_data$name == "California"] <- "Calif."
NYT_cov_data$name[NYT_cov_data$name == "New Hampshire"] <- "N.H."
NYT_cov_data$name[NYT_cov_data$name == "Maryland"] <- "Mar."
NYT_cov_data$name[NYT_cov_data$name == "South Dakota"] <- "S.Da."
NYT_cov_data$name[NYT_cov_data$name == "Colerado"] <- "Col."
NYT_cov_data$name[NYT_cov_data$name == "Oregon"] <- "Ore."
NYT_cov_data$name[NYT_cov_data$name == "New Mexico"] <- "N.Me."
NYT_cov_data$name[NYT_cov_data$name == "Virginia"] <- "Vir."
NYT_cov_data$name[NYT_cov_data$name == "Maine"] <- "Mai."
NYT_cov_data$name[NYT_cov_data$name == "Florida"] <- "Flo."
NYT_cov_data$name[NYT_cov_data$name == "Oklahoma"] <- "Okl."
NYT_cov_data$name[NYT_cov_data$name == "Kentucky"] <- "Ken."
NYT_cov_data$name[NYT_cov_data$name == "Missouri"] <- "Miss."
NYT_cov_data$name[NYT_cov_data$name == "Michigan"] <- "Mich."
NYT_cov_data$name[NYT_cov_data$name == "Nevada"] <- "Nev."
NYT_cov_data$name[NYT_cov_data$name == "Arizona"] <- "Ari"
NYT_cov_data$name[NYT_cov_data$name == "Texas"] <- "Tex."
NYT_cov_data$name[NYT_cov_data$name == "Alaska"] <- "Ala."
NYT_cov_data$name[NYT_cov_data$name == "Minnesota"] <- "Min."
NYT_cov_data$name[NYT_cov_data$name == "Colorado"] <- "Col."
NYT_cov_data$name[NYT_cov_data$name == "Pennsylvania"] <- "Pen."
NYT_cov_data$name[NYT_cov_data$name == "Georgia"] <- "Geo."
NYT_cov_data$name[NYT_cov_data$name == "Arkansas"] <- "Ark."
NYT_cov_data$name[NYT_cov_data$name == "South Carolina"] <- "S.Ca."
NYT_cov_data$name[NYT_cov_data$name == "Washington"] <- "Was."
NYT_cov_data$name[NYT_cov_data$name == "Nebraska"] <- "Neb."
NYT_cov_data$name[NYT_cov_data$name == "Washington, D.C."] <- "W.D.C."
NYT_cov_data$name[NYT_cov_data$name == "New Jersey"] <- "N.J."
NYT_cov_data$name[NYT_cov_data$name == "New York"] <- "N.Y."
NYT_cov_data$name[NYT_cov_data$name == "Massachusetts"] <- "Mas."
NYT_cov_data$name[NYT_cov_data$name == "Connecticut"] <- "Conn."
NYT_cov_data$name[NYT_cov_data$name == "Rhode Island"] <- "R.Is."
NYT_cov_data$name[NYT_cov_data$name == "Vermont"] <- "Ver."



p <- ggplot(NYT_cov_data, aes(x=fully_vaccinated_pct_of_pop, 
                              y = deaths_per_100k, label = name))
p + geom_point(position = "identity", fill = "mediumvioletred", alpha= 0.60, color = "black", size = 2.8, shape = 21)+
  geom_text(vjust = 0, nudge_y = 0.5, size = 2.5, color = "gray60")+
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
        

