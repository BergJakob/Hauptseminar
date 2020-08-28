library(tidyverse)

EPI_data_2020 <- read_csv("Hauptseminararbeit/Data/Raw/EPI/epi2020_results.csv") %>% 
  rename(EPI = EPI.new) %>% 
  select(country, EPI)

write.csv(EPI_data_2020, "Hauptseminararbeit/Data/Wrangled/SH4/EPI_data_2020.csv")

EC_data_2015 <- read_csv("Hauptseminararbeit/Data/Wrangled/SH3/EC_data_2015.csv") %>% 
  select(-X1)

write.csv(EC_data_2015, "Hauptseminararbeit/Data/Wrangled/SH4/EC_data_2015.csv")

EPI_EC_merge <- left_join(EPI_data_2020, EC_data_2015)

write.csv(EPI_EC_merge, "Hauptseminararbeit/Data/Wrangled/SH4/EPI_EC_merge.csv")

#total_supply

EC_EPI_scatterplot_1 <- ggplot(data = EPI_EC_merge) +
  geom_point(aes(x = EPI, y = total_supply))

EC_EPI_scatterplot_1 #--> mit Skalierung spielen

EC_EPI_scatterplot_2 <- ggplot(data = EPI_EC_merge, aes(x = EPI, y = total_supply)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

EC_EPI_scatterplot_2 

cor(x = EPI_EC_merge$EPI, y = EPI_EC_merge$total_supply,
    use = "pairwise.complete.obs")

bivar_reg_NRI_EC <- lm(total_supply ~ NRI_value, data = NRI_EC_merge)

summary(bivar_reg_NRI_EC)
