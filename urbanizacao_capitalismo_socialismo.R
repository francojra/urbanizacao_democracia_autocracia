
# Urbanização em países capitalistas e comunistas ------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 13/10/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/urbanization --------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Mais da metade da população mundial agora vive em áreas urbanas - cada vez mais em cidades
### densas. Os cenários urbanos são um fenômeno relativamente novo na história da humanidade.
### Essa transição tem transformado a forma que vivemos, trabalhamos, viajamos e construimos
### interações.

### Nesse registro apresentamos uma visão geral da urbanização no mundo, estendendo-se 
### do passado distante, até ao presente, e projecções de tendências futuras.

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

urb <- read.csv("share-of-population-urban.csv")
view(urb)
names(urb)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

urb <- urb %>%
  select(-Code) %>%
  rename(por_urb = Urban.population....of.total.population.) %>%
  view()

urb1 <- urb %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(por_urb),
            sd = sd(por_urb), n = n(),
            se = sd/sqrt(n)) %>%
  view()

urb2 <- urb %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  view()

urb3 <- urb %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------










