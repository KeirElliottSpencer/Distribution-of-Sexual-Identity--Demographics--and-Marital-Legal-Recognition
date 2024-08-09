#Data Visualisation Code

#Packages:
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)

census_dataset <- read.csv("data/TS079-2021-2.csv")
View(census_dataset)

grouped_data <- census_dataset %>%
  rename("category" = "Sexual.orientation..9.categories.") %>%
  group_by(category) %>%
  summarise(count = sum(Observation)) %>%
  arrange(desc(count)) %>%
  mutate(bar = "category 1") %>%
  filter(count != 0)
  
total_sum <- sum(grouped_data$count)
grouped_data$percentage <- (grouped_data$count / total_sum) * 100

View(grouped_data)

grouped_data$category <- factor(grouped_data$category, levels = 
                                  c("All other sexual orientations", "Queer", "Asexual", "Pansexual", "Bisexual",
                                    "Gay or Lesbian", "Straight or Heterosexual", "Not answered"))

library(RColorBrewer)

#set3_colors <- brewer.pal(n = 8, name = "Set3")
set3_colors <- c("#51ccd1","#f784e8","#fa938e","#85b0ff","#c6b346","#52c868","#78aad6","#bdbdbd")

#stacked bar chart of proportion of sexual orientations
ggplot(grouped_data, aes(x = bar, y = percentage, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  theme_minimal() +
  #scale_fill_brewer(palette = 4) +
  scale_fill_manual(values = set3_colors) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title='Proportion of Sexual Orientation Identification', 
    subtitle = 'UK 2021',
    caption='Source: ONS',
    tag='A')



#pie chart of lgbt proportions and sex
grouped_data_lgbt <- grouped_data %>%
  filter(category != "Straight or Heterosexual", category != "Not answered") %>%
  arrange(desc(count)) %>%
  mutate(lab.ypos = cumsum(percentage) - 0.5 * percentage)

total_sum_lgbt <- sum(grouped_data_lgbt$count)
grouped_data_lgbt$percentage <- (grouped_data_lgbt$count / total_sum_lgbt) * 100

View(grouped_data_lgbt)

grouped_data_lgbt$category <- factor(grouped_data_lgbt$category, levels = 
                                  c("All other sexual orientations", "Queer", "Asexual", "Pansexual", "Bisexual",
                                    "Gay or Lesbian"))

#pie chart??
ggplot(grouped_data_lgbt, aes(x = 2, y = percentage, fill = category)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_brewer(palette = 1) +
  theme_void() +
  xlim(0.5, 2.5)

#bar charts??
lgbt_col <- c("#732982", "#24408E", "#008026", "#FFED00", "#FF8C00", "#E40303")

ggplot(grouped_data_lgbt, aes(x = category, y = percentage, fill = category)) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +
  scale_fill_manual(values = lgbt_col) + 
  coord_flip() +
  theme_minimal() +
  geom_text(
    aes(label = sprintf("%.2f", round(percentage, 2))), size = 3, hjust = -0.4) +
  xlab("Sexual Orientation") +
  ylab("Percentage of LGBT+") +
  ylim(0,60) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank()) +
  labs(
    fill = "Sexual Orientation",
    title='Proportion of LGB+', 
    subtitle = 'UK 2021',
    caption='Source: ONS',
    tag='B')

#Doughnut charts for age and sex
library("readxl")
agesex_data2 <- read_excel("data/agesex.xlsx")
View(agesex_data2)

agesex_data <- agesex_data2 %>%
  rename("sexual_orientation" = "Sexual orientation (9 categories) label") %>%
  rename("sex" = "Sex label") %>%
  rename("age" = "Age label") %>%
  filter(sexual_orientation != "Straight or heterosexual", sexual_orientation != "Not answered") %>%
  mutate(Observation = as.numeric(Observation)) %>%
  replace(is.na(.), 0) %>%
  #
  mutate(sexual_orientation = replace(sexual_orientation, sexual_orientation == "Gay or lesbian", "Gay/Lesbian")) %>%
  mutate(sexual_orientation = replace(sexual_orientation, sexual_orientation == "All other sexual orientations", "Other")) %>%
  #
  mutate(age = replace(age, age == "Aged 16 to 24 years", "16-24")) %>%
  mutate(age = replace(age, age == "Aged 25 to 34 years", "26-34")) %>%
  mutate(age = replace(age, age == "Aged 35 to 44 years", "36-44")) %>%
  mutate(age = replace(age, age == "Aged 45 to 54 years", "46-54")) %>%
  mutate(age = replace(age, age == "Aged 55 to 64 years", "56-64")) %>%
  mutate(age = replace(age, age == "Aged 65 to 74 years", "66-74")) %>%
  mutate(age = replace(age, age == "Aged 75 years and over", "75+")) %>%
  #
  mutate(sexual_orientation = as.factor(sexual_orientation)) %>%
  mutate(sex = as.factor(sex)) %>%
  mutate(age = as.factor(age))
View(agesex_data)

grouped_sex <- agesex_data %>%
  group_by(sexual_orientation, sex) %>%
  summarise(total_count = sum(Observation))
View(grouped_sex)

grouped_age <- agesex_data %>%
  group_by(sexual_orientation, age) %>%
  summarise(total_count = sum(Observation))
View(grouped_age)

library(webr)


PieDonut(grouped_age, aes(sexual_orientation, age, count = total_count), title = "Sexual Orientation by Age",
         ratioByGroup = TRUE, r0 = 0.45, r1 = 0.9,
         explode = 3, explodeDonut=TRUE, labelposition = 1, showPieName = FALSE,
         pieLabelSize = 3.2, donutLabelSize = 2.5, start=pi/2, maxx = 1.6)

PieDonut(grouped_sex, aes(sexual_orientation, sex, count = total_count), title = "Sexual Orientation by Sex",
         ratioByGroup = TRUE, r0 = 0.45, r1 = 0.9,
         explode = 3, explodeDonut=FALSE, labelposition = 0, showPieName = FALSE,
         pieLabelSize = 3.2, donutLabelSize = 2.5, start=pi/2, maxx = 1.6)

#showRatioThreshold = 0.01



#Map of prevalence of lgbt
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

england_wales_shapefile <- st_read("data/LAD_MAY_2022_UK_BFE_V3.shp")

library(broom)
england_wales_shapefile <- tidy(england_wales_shapefile, region="lau115nm")

census_dataset_locations <- census_dataset %>%
  rename("category" = "Sexual.orientation..9.categories.") %>%
  rename("location_code" = "Lower.Tier.Local.Authorities.Code") %>%
  mutate(Sexual.orientation..9.categories..Code = NULL) %>%
  filter(!(category == "Does not apply")) %>%
  mutate(category = if_else(category %in% c("All other sexual orientations", "Queer", "Asexual", 
                                            "Pansexual", "Bisexual", "Gay or Lesbian"),
                                    "LGB+", category)) %>%
  group_by(location_code, category) %>%
  summarise(Observation = sum(Observation)) %>%
  ungroup() %>%
  group_by(location_code) %>%
  mutate(total = sum(Observation)) %>%
  mutate(percentage = (Observation / total) * 100) %>%
  ungroup() %>%
  filter(!(category == "Not answered")) %>%
  filter(!(category == "Straight or Heterosexual")) %>%
  mutate(Observation = NULL) %>%
  mutate(total = NULL)
View(census_dataset_locations)

merged_data <- england_wales_shapefile %>%
  left_join(census_dataset_locations, by = c("LAD22CD" = "location_code"))
View(merged_data)

merged_data_sf <- st_as_sf(merged_data)

library(viridis)
library()

ylgnbu_colors <- brewer.pal(n = 7, name = "YlGnBu")
ylgnbu_colors_reversed <- rev(ylgnbu_colors)

ggplot() +
  geom_sf(data = merged_data_sf, aes(fill = percentage)) +
  #scale_fill_viridis(option = "C", name = "Percentage of LGBT Population", breaks = seq(0, 100, 11), labels = seq(0, 100, 11)) +
  #scale_fill_gradientn(colors = ylgnbu_colors_reversed, name = "Value") +
  scale_fill_gradientn(colors = ylgnbu_colors_reversed, 
                       values = scales::rescale(c(0, 11), to = c(0, 1)),
                       name = "Value",
                       limits = c(0, 100)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 1)) +
  labs(title = "Proportion of LGB+ Population in England and Wales",
       subtitle = "By Local Authority District",
       caption = "Source: ONS")


#bar chart of top 5 and bottom 5 areas with LGB+ representation
census_dataset_locations2 <- census_dataset_locations %>%
  filter(percentage >= 7.2 | percentage <= 1.8) %>%
  arrange(desc(percentage)) #orders dataset by largest to smallest government transparency

census_dataset_locations2$group <- factor(ifelse(census_dataset_locations2$percentage <= median(census_dataset_locations2$percentage), "Bottom 8", "Top 8"), levels = c("Top 8", "Bottom 8"))
View(census_dataset_locations2)

temp <- census_dataset %>%
  filter(Sexual.orientation..9.categories..Code == 8) %>%
  select(Lower.Tier.Local.Authorities, Lower.Tier.Local.Authorities.Code)

census_dataset_locations3 <- census_dataset_locations2 %>%
  left_join(temp, by = c("location_code" = "Lower.Tier.Local.Authorities.Code")) %>%
  mutate(location_code = Lower.Tier.Local.Authorities)

View(temp)
View(census_dataset_locations3)

#Plots a bar graph of the top 8 and bottom 8 countries for government transparency
ggplot(census_dataset_locations3, aes(x = percentage, y = reorder(location_code, percentage), fill = group)) +
  geom_bar(stat="identity", width=0.7, color = "black", size = 0.3) +
  geom_text(
    aes(label = sprintf("%.2f", round(percentage, 2))), size = 3,
    hjust = ifelse(census_dataset_locations2$percentage >= 0, -0.2, 1.1)) +
  facet_wrap(~ group, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Top 8" = "#41ab5d", "Bottom 8" = "#d9f0a3"), name = NULL) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.major.y = element_blank()) + 
  xlim(0, 12) +
  labs(
    x="% LGB+", 
    y="Location",
    title='ABC', 
    subtitle = 'Top 8 & Bottom 8 locations',
    caption='Source: ONS',
    tag='')



#whole world map of legality of same-sex marriage
world_dataset2 <- read.csv("data/same-sex-marriage-recognition.csv")

world_dataset <- world_dataset2 %>%
  filter(Year == 2020) %>%
  mutate(Code = NULL) %>%
  rename("legal_ss_marriage" = "Same.sex.marriage.and.civil.unions.legal") %>%
  mutate(Year = NULL) %>%
  mutate(Entity = replace(Entity, Entity == "United Kingdom", "UK")) %>%
  mutate(Entity = replace(Entity, Entity == "United States", "USA"))
View(world_dataset)

world_map <- map_data("world")
View(world_map)

world_map_joined <- left_join(world_map,world_dataset, by = c("region" = "Entity"))

world_map_joined <- world_map_joined %>%
  mutate(legal_ss_marriage = replace_na(legal_ss_marriage, "Same-sex marriage not legally recognised")) %>%
  mutate(legal_ss_marriage = as_factor(legal_ss_marriage))

levels(world_map_joined$legal_ss_marriage)
world_map_joined$legal_ss_marriage <- factor(world_map_joined$legal_ss_marriage, 
                                             levels = c(
                                               "Same-sex marriage not legally recognised", 
                                               "Some rights to same-sex couples", 
                                               "Same-sex marriage legal in some jurisdictions", 
                                               "Same-sex marriage legal"
                                               ))

View(world_map_joined)

ggplot(world_map_joined) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = legal_ss_marriage)) +
  scale_fill_manual(values = c("Same-sex marriage not legally recognised" = "#d73027", 
                               "Some rights to same-sex couples" = "#fee090", 
                               "Same-sex marriage legal in some jurisdictions" = "#a6d96a", 
                               "Same-sex marriage legal" = "#1a9850"),
                    na.value = "gray", name = NULL) +
  theme_void() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow=2, byrow = TRUE)) +
  coord_fixed() +
  labs(
    title='World map of same-sex marriage legality', 
    subtitle = '2020',
    caption='Source: Our World in Data',
    tag='...')

