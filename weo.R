library(tidyverse)
library(scales)
library(janitor)
library(RColorBrewer)
library(ggpubr)
library(ggrepel)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(DT)


# load and prepare data
weo1020group <- read.delim("WEOOct2020alla.txt", header = TRUE, sep = "\t", dec = ".") %>% 
  clean_names() %>% 
  select(-(estimates_start_after)) %>% 
  pivot_longer(x1980:x2025, names_to = "year" , values_to = "value") %>% 
  mutate(year = str_remove(year, "x")) %>% 
  mutate(subject_descriptor = str_remove(subject_descriptor, ", average consumer prices")) %>% 
  filter(!is.na(value))

weo1020group$year <- as.integer(weo1020group$year)
weo1020group$value <- as.numeric(weo1020group$value)
weo1020group$country_group_name <- as.factor(weo1020group$country_group_name)

weo1020 <- read.delim("WEOOct2020all.txt", header = TRUE, sep = "\t", dec = ".") %>% 
  clean_names() %>% 
  select(-(estimates_start_after)) %>% 
  pivot_longer(x1980:x2025, names_to = "year" , values_to = "value") %>% 
  mutate(year = str_remove(year, "x")) %>% 
  mutate(subject_descriptor = str_remove(subject_descriptor, ", average consumer prices")) %>% 
  filter(!is.na(value))

weo1020$year <- as.integer(weo1020$year)
weo1020$value <- as.numeric(weo1020$value)
weo1020$country <- as.factor(weo1020$country)

# explore data

# Series Plot
weo1020 %>% 
  filter(subject_descriptor == "Gross domestic product, constant prices" &
         units == "Percent change" &
           country %in% c("Indonesia" , "Thailand" , "Vietnam" , "Philippines" , 
                          "Malaysia" , "Korea" , "Japan" , "Singapore", "China" )) %>% 
  filter(year > 1989) %>%
  filter(!is.na(value)) %>% 
  ggplot() +
  geom_line(aes(year, value, color = country), size = 1, show.legend = TRUE) +
  geom_point(aes(year, value, color = country), size = 0.5, show.legend = FALSE) +
  #geom_smooth(aes(year, value), se = FALSE, color = "lightblue", linetype = 2, size = 0.6, 
  #            alpha = 0.2, show.legend = FALSE) +
  scale_color_brewer(palette = "Paired") +
  # scale_color_brewer(palette = "Paired", name='Countries',
  #                      guide=guide_legend(
  #                        direction='horizontal',
  #                        title.position='top',
  #                        title.hjust = .5,
  #                        label.hjust = .5,
  #                        label.position = 'bottom',
  #                        keywidth = 3,
  #                        keyheight = 1
  #                      ))+
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1998, 2009, 
                                2020, 2025)) +
  facet_wrap(~country, scales = "free") +
  theme_ipsum() +
  labs(title = "",
  #      subtitle = "",
  #      caption = paste("Source: World Economic Outlook, October 2020\n
  #      Note: The numbers for 2020 - 2025 are projections from the IMF") ,
         color = "Country",
        x = "",
        y = "Percent, yoy") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

# Boxplot

p1 <- weo1020 %>% 
  filter(subject_descriptor == "Gross domestic product, constant prices" &
           units == "Percent change" &
           country %in% c("Indonesia" , "Thailand" , "Vietnam" , "Philippines" , 
                          "Malaysia" , "Turkey" , "Brazil" , "Russia" , "Mexico",
                          "Argentina" , "China" , "South Africa")) %>% 
  filter(year < 2020) %>%
  filter(!is.na(value)) %>% 
  ggplot() +
  geom_boxplot(aes(country, value, color = country), size = 0.8, show.legend = FALSE) +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  labs(title = "Historical Growth Distribution, 1980 - 2019",
       color = "Country") +
  xlab("") +
  ylab("Percent, yoy")

p2 <- weo1020 %>% 
  filter(subject_descriptor == "Gross domestic product, constant prices" &
           units == "Percent change" &
           country %in% c("Indonesia" , "Thailand" , "Vietnam" , "Philippines" , 
                          "Malaysia" , "Turkey" , "Brazil" , "Russia" , "Mexico",
                          "Argentina" , "China" , "South Africa")) %>%  
  filter(year > 2019) %>%
  filter(!is.na(value)) %>% 
  ggplot() +
  geom_boxplot(aes(country, value, color = country), size = 0.8, show.legend = TRUE) +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  labs(title = "Growth Projection Distribution, 2020 - 2025",
       caption = "Source: World Economic Outlook, October 2020",
       color = "Country") +
  xlab("") +
  ylab("Perccent, yoy")

pbox <- ggarrange(p1,p2, legend = "bottom")

# General govt structural balance
weo1020 %>% filter(year > 2019) %>%
  filter(subject_descriptor == "General government structural balance" &
           units == "Percent of potential GDP" &
           country %in% c("Indonesia" , "Thailand" , "Vietnam" , "Philippines" , 
                          "Malaysia" , "Korea" , "Japan" , "Singapore", "China")) %>% 
  filter(!is.na(value)) %>%
  ggplot() +
  geom_col(aes(year, value, fill = country), size = 0.8, show.legend = TRUE) +
  # geom_smooth(aes(year, value), color = "blue", se = FALSE, 
  #             linetype = 2, alpha = 0.2, size = 0.5) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025)) +
  geom_text_repel(aes(year, value, label = value), size = 3) +
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  facet_wrap(~country, scales = "free", nrow = 2) +
  labs(title = "Government's budget balance, 2020 - 2025",
       subtitle = "The IMF's WEO, October 2020",
       caption = "",
       fill = "Country",
       x = "",
       y = "Percent") +
  theme(axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.title.position = "plot",
        legend.position = "bottom")


# General govt revenue
weo1020 %>% filter(year %in% c(2021:2023)) %>% 
  filter(subject_descriptor == "General government revenue" &
           units == "Percent of GDP" &
           country %in% c("Indonesia" , "Thailand" , "Vietnam" , "Philippines" , 
                          "Malaysia" , "Korea" , "Japan" , "Singapore", "China")) %>% 
  filter(!is.na(value)) %>%
  ggplot() +
  geom_line(aes(year, value, fill = country)) +
  scale_x_continuous(breaks = c(2000, 2005, 
                                2010, 2015, 2020, 2025)) +
  scale_color_brewer(palette = "Paired") +
  theme_ipsum() +
  facet_wrap(~country, scales = "free") +
  labs(title = "",
       subtitle = "",
       caption = "",
       color = "Country",
       x = "",
       y = "Percent") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())


# Debt to GDP

dtgdp <- weo1020 %>%  
  filter(year %in% c(2021:2023)) %>%
  filter(subject_descriptor == "General government gross debt") %>% 
  filter(units == "Percent of GDP") %>% 
  select(c(country, year, value)) %>% 
  filter(country %in% c("Indonesia" , "Thailand" , "Vietnam" , "Philippines" , 
                        "Malaysia" , "Korea" , "Singapore", "China")) %>% 
  as_tibble()

dtgdp$year <- as.factor(dtgdp$year)

dtgdp %>% ggplot() +
  geom_col(aes(reorder(country, value), value, fill = year), position = "dodge") +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 150, 200, 250, 270)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip() +
  labs(title = "Debt to GDP, 2021 - 2023",
       subtitle = "IMF's projection in WEO October 2020",
       caption = "",
       fill = "Year",
       x = "",
       y = "Percent") +
  theme_ipsum() +
  theme(axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.title.position = "plot")


dtgdp %>% ggplot() +
  geom_col(aes(year, value, fill = country), size = 0.8, show.legend = TRUE) +
  geom_smooth(aes(year, value), color = "blue", alpha = 0.2, size = 0.8, show.legend = FALSE) +
  scale_x_continuous(breaks = c(2019, 2020, 2021, 2022, 2023, 2024)) +
  scale_color_brewer(palette = "Paired") +
  theme_ipsum() +
  facet_wrap(~country, scales = "free", nrow = 3) +
  labs(title = "",
       subtitle = "",
       caption = "",
       fill = "Country",
       x = "",
       y = "Percent") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())



weo1020 %>% filter(year > 1979) %>% 
  filter(subject_descriptor == "Gross domestic product, constant prices" &
           country %in% c("Indonesia" , "United States" , "China",
                          "India", "Nigeria" , "Pakistan", "Vietnam" ,
                          "Thailand" , "Japan")) %>% 
  filter(!is.na(value)) %>%
  ggplot() +
  geom_line(aes(year, value, color = country), size = 0.2, alpha = 0.5) +
  geom_smooth(aes(year, value, color = country), size = 0.8, se = FALSE) +
  # scale_x_continuous(breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 
  #                               2010, 2015, 2020, 2025)) +
  scale_color_brewer(palette = "Set1") +
  theme_ipsum() +
  #facet_wrap(~country) +
  labs(title = "Grwoth, 1980 - 2025",
       subtitle = "2020 - 2025 based on the IMF projection",
       caption = "Source: WEO, October 2020",
       color = "Country",
       x = "",
       y = "Percent") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size =12),
        legend.text = element_text(size = 11))
