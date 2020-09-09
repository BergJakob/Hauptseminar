library(tidyverse)

# DATA WRANGLING H1

EPI_data_2020 <- read_csv("Hauptseminararbeit/Data/Raw/EPI/epi2020_results.csv") %>% 
  rename(EPI_value = EPI.new) %>% 
  select(country, EPI_value)

NRI_data_2015 <- read_csv2("Hauptseminararbeit/Data/Raw/NRI/NRI_data_2015.csv")

NRI_EPI_merge <- left_join(NRI_data_2015, EPI_data_2020)

write.csv(EPI_data_2020, "Hauptseminararbeit/Data/Wrangled/H1/EPI_data_2020.csv")

write.csv(NRI_data_2015, "Hauptseminararbeit/Data/Wrangled/H1/NRI_data_2015.csv")

write.csv(NRI_EPI_merge, "Hauptseminararbeit/Data/Wrangled/H1/NRI_EPI_merge.csv")

# DATA WRANGLING SH1

NRI_data_2015 <- read_csv2("Hauptseminararbeit/Data/Raw/NRI/NRI_data_2015.csv")

ECI_data_2018 <- read_csv2("Hauptseminararbeit/Data/Raw/ECI/ECI_data_2018.csv") %>% 
  rename(ECI_value = value)

NRI_ECI_merge <- left_join(NRI_data_2015, ECI_data_2018)

write.csv(NRI_data_2015, "Hauptseminararbeit/Data/Wrangled/SH1/NRI_data_2015.csv")

write.csv(ECI_data_2018, "Hauptseminararbeit/Data/Wrangled/SH1/ECI_data_2018.csv")

write.csv(NRI_ECI_merge, "Hauptseminararbeit/Data/Wrangled/SH1/NRI_ECI_merge.csv")

# DATA WRANGLING SH2

ECI_data_2018 <- read_csv2("Hauptseminararbeit/Data/Raw/ECI/ECI_data_2018.csv") %>% 
  rename(ECI_value = value)

EPI_data_2020 <- read_csv("Hauptseminararbeit/Data/Raw/EPI/epi2020_results.csv") %>% 
  rename(EPI_value = EPI.new) %>% 
  select(country, EPI_value)

EPI_ECI_merge <- left_join(EPI_data_2020, ECI_data_2018)

write.csv(ECI_data_2018, "Hauptseminararbeit/Data/Wrangled/SH2/ECI_data_2018.csv")

write.csv(EPI_data_2020, "Hauptseminararbeit/Data/Wrangled/SH2/EPI_data_2020.csv")

write.csv(EPI_ECI_merge, "Hauptseminararbeit/Data/Wrangled/SH2/EPI_ECI_merge.csv")

# DATA WRANGLING SH3

EC_data_2015_raw <- read_csv2("Hauptseminararbeit/Data/Raw/UNSD_Data/07_2020_UNSD__production_trade_energy_supply.csv") %>%
  janitor::clean_names('snake') %>%
  separate(number_region_country_area_year_series_value_footnotes_source, c("number", "country", "year", "series", 
                                                                            "value", "value2", "footnotes", "source"), sep = ",") %>% 
  unite(value, value2, col = "value", sep = ",", , remove = TRUE, na.rm = FALSE) %>% 
  select(number, country, year, value, series) %>% 
  filter(year == 2015)

write.csv(EC_data_2015_raw, "Hauptseminararbeit/Data/Raw/EC/EC_2015_data_raw.csv")

## hier war Zwischenschritt in Excel

EC_data_2015 <- read_csv2("Hauptseminararbeit/Data/Raw/EC/EC_2015_data_raw.csv") %>% 
  filter(number >= 31) %>% 
  select( -number, -year) %>% 
  pivot_wider(names_from = series, values_from = value) %>%
  janitor::clean_names('snake') %>% 
  rename(total_supply = total_supply_petajoules, supply_per_capita = supply_per_capita_gigajoules) %>% 
  select(country, total_supply, supply_per_capita)

NRI_data_2015 <- read_csv2("Hauptseminararbeit/Data/Raw/NRI/NRI_data_2015.csv")

NRI_EC_merge <- left_join(NRI_data_2015, EC_data_2015)

write.csv(NRI_data_2015, "Hauptseminararbeit/Data/Wrangled/SH3/NRI_data_2015.csv")

write.csv(EC_data_2015, "Hauptseminararbeit/Data/Wrangled/SH3/EC_data_2015.csv")

write.csv(NRI_EC_merge, "Hauptseminararbeit/Data/Wrangled/SH3/NRI_EC_merge.csv")

# DATA WRANGLING SH4

EPI_data_2020 <- read_csv("Hauptseminararbeit/Data/Raw/EPI/epi2020_results.csv") %>% 
  rename(EPI_value = EPI.new) %>% 
  select(country, EPI_value)

EC_data_2015 <- read_csv2("Hauptseminararbeit/Data/Raw/EC/EC_2015_data_raw.csv") %>% 
  filter(number >= 31) %>% 
  select( -number, -year) %>% 
  pivot_wider(names_from = series, values_from = value) %>%
  janitor::clean_names('snake') %>% 
  rename(total_supply = total_supply_petajoules, supply_per_capita = supply_per_capita_gigajoules) %>% 
  select(country, total_supply, supply_per_capita)

EPI_EC_merge <- left_join(EPI_data_2020, EC_data_2015)

write.csv(EPI_data_2020, "Hauptseminararbeit/Data/Wrangled/SH4/EPI_data_2020.csv")

write.csv(EC_data_2015, "Hauptseminararbeit/Data/Wrangled/SH4/EC_data_2015.csv")

write.csv(EPI_EC_merge, "Hauptseminararbeit/Data/Wrangled/SH4/EPI_EC_merge.csv")

# DATA WRANGLING MAIN-DATASET

NRI_ECI_merge <- read_csv("Hauptseminararbeit/Data/Wrangled/SH1/NRI_ECI_merge.csv") %>% 
  select(country, NRI_value, ECI_value, rank) %>% 
  rename(ECI_rank = rank)

NRI_EC_merge <- read_csv("Hauptseminararbeit/Data/Wrangled/SH3/NRI_EC_merge.csv") %>% 
  select(-X1,-NRI_value) %>% 
  rename(EC_total_supply = total_supply,
         EC_supply_per_capita = supply_per_capita)

EPI_EC_merge <- read_csv("Hauptseminararbeit/Data/Wrangled/SH4/EPI_EC_merge.csv") %>% 
  select(country, EPI_value)

step_1 <- left_join(NRI_EC_merge, EPI_EC_merge)

data_hsa_2020_complete <- left_join(step_1, NRI_ECI_merge) %>% 
  select(country, NRI_value, EPI_value, ECI_value, ECI_rank, EC_total_supply, EC_supply_per_capita)

write.csv(data_hsa_2020_complete, "Hauptseminararbeit/Data/Wrangled/data_hsa_2020_complete.csv")

### ANALYSIS H1 ###

NRI_EPI_scatterplot_1 <- ggplot(data = data_hsa_2020_complete) +
  geom_point(aes(x = NRI_value, y = EPI_value))

NRI_EPI_scatterplot_1

NRI_EPI_scatterplot_2 <- ggplot(data = data_hsa_2020_complete, aes(x = NRI_value, y = EPI_value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

NRI_EPI_scatterplot_2 

cor(x = data_hsa_2020_complete$NRI_value, y = data_hsa_2020_complete$EPI_value,
    use = "pairwise.complete.obs")

bivar_reg_NRI_EPI <- lm(EPI_value ~ NRI_value, data = data_hsa_2020_complete)

summary(bivar_reg_NRI_EPI)

#alles deutet daraufhin, dass NRI EPI positiv beeinflusst (Pearson R, bi. Regression),
#was bedeutet, dass H1 eher abegelehnt werden muss

### ANALYSIS SH1 ###

## ECI_value

NRI_ECI_scatterplot_1 <- ggplot(data = data_hsa_2020_complete) +
  geom_point(aes(x = NRI_value, y = ECI_value))

NRI_ECI_scatterplot_1

NRI_ECI_scatterplot_2 <- ggplot(data = data_hsa_2020_complete, aes(x = NRI_value, y = ECI_value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

NRI_ECI_scatterplot_2 

cor(x = data_hsa_2020_complete$NRI_value, y = data_hsa_2020_complete$ECI_value,
    use = "pairwise.complete.obs")

bivar_reg_NRI_ECI <- lm(ECI_value ~ NRI_value, data = data_hsa_2020_complete)

summary(bivar_reg_NRI_ECI)

#hoher Zusammenhang ersichtlich

## ECI_rank

NRI_ECI_scatterplot_1 <- ggplot(data = data_hsa_2020_complete) +
  geom_point(aes(x = NRI_value, y = ECI_rank))

NRI_ECI_scatterplot_1

NRI_ECI_scatterplot_2 <- ggplot(data = data_hsa_2020_complete, aes(x = NRI_value, y = ECI_rank)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

NRI_ECI_scatterplot_2 

cor(x = data_hsa_2020_complete$NRI_value, y = data_hsa_2020_complete$ECI_rank,
    use = "pairwise.complete.obs")

bivar_reg_NRI_ECI <- lm(ECI_rank ~ NRI_value, data = data_hsa_2020_complete)

summary(bivar_reg_NRI_ECI)

### ANALYSIS SH2 ###

## ECI_value

EPI_ECI_scatterplot_1 <- ggplot(data = data_hsa_2020_complete) +
  geom_point(aes(x = EPI_value, y = ECI_value))

EPI_ECI_scatterplot_1

EPI_ECI_scatterplot_2 <- ggplot(data = data_hsa_2020_complete, aes(x = EPI_value, y = ECI_value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

EPI_ECI_scatterplot_2 

cor(x = data_hsa_2020_complete$EPI_value, y = data_hsa_2020_complete$ECI_value,
    use = "pairwise.complete.obs")

bivar_reg_EPI_ECI <- lm(EPI_value ~ ECI_value, data = data_hsa_2020_complete)

summary(bivar_reg_EPI_ECI)

#hoher negativer Zusammenhang ersichtlich

## ECI_rank

EPI_ECI_scatterplot_1 <- ggplot(data = data_hsa_2020_complete) +
  geom_point(aes(x = EPI_value, y = ECI_rank))

EPI_ECI_scatterplot_1

EPI_ECI_scatterplot_2 <- ggplot(data = data_hsa_2020_complete, aes(x = EPI_value, y = ECI_rank)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

EPI_ECI_scatterplot_2 

cor(x = data_hsa_2020_complete$EPI_value, y = data_hsa_2020_complete$ECI_rank,
    use = "pairwise.complete.obs")

bivar_reg_EPI_ECI <- lm(EPI_value ~ ECI_rank, data = data_hsa_2020_complete)

summary(bivar_reg_EPI_ECI)

### ANALYSIS SH3 ###

## EC_total_supply

NRI_EC_scatterplot_1 <- ggplot(data = data_hsa_2020_complete) +
  geom_point(aes(x = NRI_value, y = EC_total_supply))

NRI_EC_scatterplot_1 #--> mit Skalierung spielen

NRI_EC_scatterplot_2 <- ggplot(data = data_hsa_2020_complete, aes(x = NRI_value, y = EC_total_supply)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

NRI_EC_scatterplot_2 

cor(x = data_hsa_2020_complete$NRI_value, y = data_hsa_2020_complete$EC_total_supply,
    use = "pairwise.complete.obs")

bivar_reg_NRI_EC <- lm(EC_total_supply ~ NRI_value, data = data_hsa_2020_complete)

summary(bivar_reg_NRI_EC)

## EC_supply_per_capita

NRI_EC_scatterplot_1 <- ggplot(data = data_hsa_2020_complete) +
  geom_point(aes(x = NRI_value, y = EC_supply_per_capita))

NRI_EC_scatterplot_1 #--> mit Skalierung spielen

NRI_EC_scatterplot_2 <- ggplot(data = data_hsa_2020_complete, aes(x = NRI_value, y = EC_supply_per_capita)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

NRI_EC_scatterplot_2 

cor(x = data_hsa_2020_complete$NRI_value, y = data_hsa_2020_complete$EC_supply_per_capita,
    use = "pairwise.complete.obs")

bivar_reg_NRI_EC <- lm(EC_supply_per_capita ~ NRI_value, data = data_hsa_2020_complete)

summary(bivar_reg_NRI_EC)

#leichte Ergebnisse in Richtung, dass hypo stimmen könnte
#per_capita immer ausagekräftiger
#BEI VERWENDEN UMRECHNUNG PETAJOULE DRANDENKEN
#Im Text auf total und capita supply eingehen und auch prüfen ob bei Energieverbrauch der vllt ausländische 
#Verbrauch von Serverfarmen usw betroffen ist

### ANALYSIS SH4 ###

## EC_total_supply

EC_EPI_scatterplot_1 <- ggplot(data = data_hsa_2020_complete) +
  geom_point(aes(x = EPI_value, y = EC_total_supply))

EC_EPI_scatterplot_1 #--> mit Skalierung spielen

EC_EPI_scatterplot_2 <- ggplot(data = data_hsa_2020_complete, aes(x = EPI_value, y = EC_total_supply)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

EC_EPI_scatterplot_2 

cor(x = data_hsa_2020_complete$EPI_value, y = data_hsa_2020_complete$EC_total_supply,
    use = "pairwise.complete.obs")

bivar_reg_NRI_EC <- lm(EPI_value ~ EC_total_supply, data = data_hsa_2020_complete)

summary(bivar_reg_NRI_EC)

## EC_supply_per_capita

EC_EPI_scatterplot_1 <- ggplot(data = data_hsa_2020_complete) +
  geom_point(aes(x = EPI_value, y = EC_supply_per_capita))

EC_EPI_scatterplot_1 #--> mit Skalierung spielen

EC_EPI_scatterplot_2 <- ggplot(data = data_hsa_2020_complete, aes(x = EPI_value, y = EC_supply_per_capita)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

EC_EPI_scatterplot_2 

cor(x = data_hsa_2020_complete$EPI_value, y = data_hsa_2020_complete$EC_supply_per_capita,
    use = "pairwise.complete.obs")

bivar_reg_NRI_EC <- lm(EPI_value ~ EC_supply_per_capita, data = data_hsa_2020_complete)

summary(bivar_reg_NRI_EC)

#leichte Ergebnisse in Richtung, dass Hypo stimmen könnte


