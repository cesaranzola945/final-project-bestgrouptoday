
# In case you need to install this to grade

# install.packages("readxl")
# install.packages("sf")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
#install.packages("viridis")

# Use the following libraries
library(tidyverse)
library(readxl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
#setwd("D:/Users/ASUS/Documents/GitHub/Data and programing for PP 2/Final Project")
setwd("/Users/Agha/Documents/GitHub/final-project-bestgrouptoday")


# read databases
data_energy <- read_xlsx("Data/all_public_energy.xlsx")

data_energy <- rename(data_energy,"country" = `Country of Headquarters`)

data_energy <-
  data_energy %>% 
  mutate(country = ifelse(country == "United States of America" ,"United States", country))


load("Data/data_refinitiv.Rda")

# group the info by country

count_en<-
data_energy %>% 
  group_by(country) %>% 
  summarise(ncomp = n())

count_en_esg<-
  data_fin %>% 
   filter(year == 2021) %>% 
    group_by(country) %>% 
    summarise(ncomp_esg = n())


# merge both database to show the penetration rate

all_count <- left_join(count_en_esg, count_en,by = "country")

all_count <-
all_count %>% 
  mutate(esg_pen = ncomp_esg /ncomp)


# call the world map
world <- ne_countries(scale = "medium", returnclass = "sf")


# merge both databases
str_match(world$formal_en, all_count$country)

world_info <- left_join(world, all_count, by = c("sovereignt" = "country"))


plot_dat<-
world_info %>% 
    mutate(esg_pen = ifelse(is.na(esg_pen),0,esg_pen)) %>% 
     arrange(sovereignt)

png(file = "images/esg_penetration_map.png", width = 1000, height = 900)
ggplot(data = plot_dat) +
  geom_sf(aes(fill = esg_pen*100)) +
  labs(title = "Figure 1: ESG Penetration Rate 2021 (%)", fill = "(%) of Companies",
       caption = "Calculated as the number of companies with ESG score over total
       number of companies in the database")+
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_viridis() +
  theme(plot.title = element_text(hjust = 0.5,size = 40),
        axis.title = element_text(size = 30),
        legend.text= element_text(size = 15),
        legend.title = element_text(size = 25),
        legend.key.height = unit(1, "cm"),
        plot.caption = element_text(size = 20))
dev.off()

  


