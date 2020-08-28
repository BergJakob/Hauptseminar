library(tidyverse)

EPI_data_2020 <- read_csv("Hauptseminararbeit/Data/Raw/EPI/epi2020_results.csv") %>% 
  select(iso, EPI.new) %>% 
  rename(ISO = iso, EPI = EPI.new)

NRI_data_2015 <- read_csv2("Hauptseminararbeit/Data/Raw/NRI.csv") %>% 
  select(ISO, NRI)

NRI_EPI_merge <- left_join(NRI_data_2015, EPI_data_2020)
  
write.csv(EPI_data_2020, "Hauptseminararbeit/Data/Wrangled/EPI_data_2020.csv")

write.csv(NRI_data_2015, "Hauptseminararbeit/Data/Wrangled/NRI_data_2015.csv")

write.csv(NRI_EPI_merge, "Hauptseminararbeit/Data/Wrangled/NRI_EPI_merge.csv")

NRI_EPI_scatterplot_1 <- ggplot(data = NRI_EPI_merge) +
  geom_point(aes(x = NRI, y = EPI))

NRI_EPI_scatterplot_1

NRI_EPI_scatterplot_2 <- ggplot(data = NRI_EPI_merge, aes(x = NRI, y = EPI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

NRI_EPI_scatterplot_2 

cor(x = NRI_EPI_merge$NRI, y = NRI_EPI_merge$EPI,
    use = "pairwise.complete.obs")

bivar_reg_NRI_EPI <- lm(EPI ~ NRI, data = NRI_EPI_merge)

summary(bivar_reg_NRI_EPI)


# alles deutet daraufhin, dass NRI EPI positiv beeinflusst (Pearson R, bi. Regression), was bedeutet, dass H1 eher abegelehnt werden muss
