library(tidyverse)
library(janitor)
library(scales)
library(factoextra)
library(tidymodels)
transfer <- read.csv("top250-00-19.csv")%>%
  janitor::clean_names()


transfer$name <- iconv(transfer$name,from="UTF-8",to="ASCII//TRANSLIT")
transfer$team_from <- iconv(transfer$team_from,from="UTF-8",to="ASCII//TRANSLIT")
transfer$team_to <- iconv(transfer$team_to,from="UTF-8",to="ASCII//TRANSLIT")
transfer$league_from <- iconv(transfer$league_from,from="UTF-8",to="ASCII//TRANSLIT")
transfer$league_to <- iconv(transfer$league_to,from="UTF-8",to="ASCII//TRANSLIT")


transfer %>%
  mutate(name = fct_reorder(name,transfer_fee))%>%
  arrange(desc(transfer_fee))%>%
  head(20)%>%
  group_by(position)%>%
  ggplot(aes(x = transfer_fee , y = name, color = position ))+
  geom_point()

transfer %>%
  group_by(team_to, league_to)%>%
  summarise(gasto = sum(transfer_fee))%>%
  mutate(team_to = fct_reorder(team_to,gasto))%>%
  arrange(desc(gasto))%>%
  head(100)%>%
  ungroup()%>%
  ggplot(aes( x = team_to, y = gasto , color = league_to))+
  geom_point()

transfer1 <- transfer %>%
  select(name, age,  team_to, league_to ,transfer_fee, season)%>%
  group_by(team_to, league_to, age)%>%
  summarise(transfer_fee = sum(transfer_fee))%>%
  arrange(desc(transfer_fee))%>%
  ungroup()%>%
  pivot_wider(names_from = team_to, values_from = transfer_fee, values_fill = 0 )

pca_rec<- recipe(~., data = transfer1)%>%
  update_role(league_to,age, new_role = "id")%>%
  step_normalize(all_predictors())%>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)
pca_prep

tidy_pca <-tidy(pca_prep, 2)

tidy_pca%>%
  filter(component %in% paste0("PC", 1:5))%>%
  mutate(component = fct_inorder(component))%>%
  ggplot(aes(value, terms, fill = terms))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~component, nrow = 1)+
  labs(y = NULL)

tidy_pca%>%
  filter(component %in% paste0("PC", 1:5))%>%
  mutate(component = fct_inorder(component))%>%
  ggplot(aes(value, terms, fill = terms))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~component, nrow = 1)+
  labs(y = NULL)

juice(pca_prep)%>%
  ggplot(aes(PC1, PC2))+
  geom_point(aes(color  = league_to))
  qbet
