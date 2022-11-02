
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
  filter(Entity %in% c("United States", "China", "Brazil")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(urb1, aes(x = fct_reorder(Entity, media), y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymax = media + se, ymin = media - se),
                width = 0.2, size = 0.8) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("China", "Coreia do Norte", "Cuba",
                              "Alemanha", "Estados Unidos", "Japão")) +
  labs(x = "Países", y = "Pessoas vivendo em áreas urbanas (%)") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplot(urb2, aes(x = Year, y = por_urb, 
                 color = Entity, group = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499"),
                     labels = c("China", "Cuba", "Alemanha",
                                "Japão", "Coreia do Norte", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Pessoas vivendo em áreas urbanas (%)",
       color = "Países") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(axis.text = element_text(color = "black"))

ggplot(urb3, aes(x = Year, y = por_urb, 
                  group = Entity, col = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Pessoas vivendo em áreas urbanas (%)", 
       color = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))





