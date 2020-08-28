library(tidyverse)

NRI_data_2015 <- read_csv2("Hauptseminararbeit/Data/Raw/NRI/NRI_SH3.csv") %>% 
  drop_na()

EC_data_2015_raw <- read_csv2("Hauptseminararbeit/Data/Raw/UNSD_Data/07_2020_UNSD__production_trade_energy_supply.csv")%>%
  janitor::clean_names('snake') %>%
  separate(number_region_country_area_year_series_value_footnotes_source, c("number", "country", "year", "series", 
                                                                            "value", "value2", "footnotes", "source"), sep = ",") %>% 
  unite(value, value2, col = "value", sep = ",", , remove = TRUE, na.rm = FALSE) %>% 
  select(number, country, year, value, series) %>% 
  filter(year == 2015)

write.csv(EC_data_2015_raw, "Hauptseminararbeit/Data/Raw/EC/EC_2015_data_raw.csv")

#hier war Zwischenschritt in Excel

EC_data_2015 <- read_csv2("Hauptseminararbeit/Data/Raw/EC/EC_2015_data_raw.csv") %>% 
  filter(number >= 31) %>% 
  select( -number, -year) %>% 
  pivot_wider(names_from = series, values_from = value) %>%
  janitor::clean_names('snake') %>% 
  rename( total_supply = total_supply_petajoules, supply_per_capita = supply_per_capita_gigajoules) %>% 
  select(country, total_supply, supply_per_capita)

write.csv(EC_data_2015, "Hauptseminararbeit/Data/Wrangled/SH3/EC_2015_data.csv")

NRI_EC_merge <- left_join(NRI_data_2015, EC_data_2015)

write.csv(NRI_EC_merge, "Hauptseminararbeit/Data/Wrangled/SH3/NRI_EC_merge.csv")  

#total_supply

NRI_EC_scatterplot_1 <- ggplot(data = NRI_EC_merge) +
  geom_point(aes(x = NRI_value, y = total_supply))

NRI_EC_scatterplot_1 #--> mit Skalierung spielen

NRI_EC_scatterplot_2 <- ggplot(data = NRI_EC_merge, aes(x = NRI_value, y = total_supply)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

NRI_EC_scatterplot_2 

cor(x = NRI_EC_merge$NRI_value, y = NRI_EC_merge$total_supply,
    use = "pairwise.complete.obs")

bivar_reg_NRI_EC <- lm(total_supply ~ NRI_value, data = NRI_EC_merge)

summary(bivar_reg_NRI_EC)

# per_capita

NRI_EC_scatterplot_1 <- ggplot(data = NRI_EC_merge) +
  geom_point(aes(x = NRI_value, y = supply_per_capita))

NRI_EC_scatterplot_1 #--> mit Skalierung spielen

NRI_EC_scatterplot_2 <- ggplot(data = NRI_EC_merge, aes(x = NRI_value, y = supply_per_capita)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

NRI_EC_scatterplot_2 

cor(x = NRI_EC_merge$NRI_value, y = NRI_EC_merge$supply_per_capita,
    use = "pairwise.complete.obs")

bivar_reg_NRI_EC <- lm(supply_per_capita ~ NRI_value, data = NRI_EC_merge)

summary(bivar_reg_NRI_EC)

  

#leichte Ergebnisse in Richtung, dass hypo stimmen könnte
#per_capita immer ausagekräftiger

#BEI VERWENDEN UMRECHNUNG PETAJOULE DRANDENKEN
#Im Text auf total und capita supply eingehen und auch prüfen ob bei energieverbrauch der vllt ausländische 
#Verbrauch von Serverfarmen usw betroffen ist