library(readr)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(stringi)
library(ggplot2)

df <- read_csv("datasets/space_missions.csv")
df_dict <- read_csv("datasets/space_missions_data_dictionary.csv")

### Showing columns specs
str(df)

min(df$Date)
max(df$Date)

### Counting the number of distinct values
sapply(df, function(x) n_distinct(x))

### Not much information
df %>% 
  filter(MissionStatus == "Prelaunch Failure")

df_lifetime <- df %>% 
  # filter(RocketStatus == "Retired") %>%
  select(Rocket, Date, MissionStatus) %>% 
  group_by(Rocket) %>% 
  summarise(first_launch = min(Date), last_launch = max(Date),
            first_launch_year = year(first_launch),
            lifetime = last_launch - first_launch,
            lifetime_years = time_length(lifetime, "years"),
            launch_group = ifelse(first_launch_year <= 1980, "le_1980", "gt_1980"),
            n_launches = n())

median_lifetime <- median(df_lifetime$lifetime_years)

df_lifetime %>% 
  ggplot(aes(x = lifetime_years)) +
  geom_histogram(bins = 23) +
  geom_vline(xintercept = median_lifetime)

df %>% 
  group_by(MissionStatus) %>% 
  summarise(qty = n())

### Check one time usage of rockets
df_lifetime %>% 
  filter(lifetime < 1) %>% 
  inner_join(df, by = "Rocket") %>% 
  group_by(MissionStatus) %>% 
  summarise(qty = n())


medians <- df_lifetime %>% 
  group_by(launch_group) %>% 
  summarise(grp.median = median(lifetime_years))
df_lifetime %>% 
  ggplot(aes(x = lifetime_years, fill = launch_group)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  geom_vline(data = medians, aes(xintercept = grp.median, color = launch_group),
              linetype="dashed")

### Getting the lauching countries
df_country <- df %>% 
  mutate(location_rev = stri_reverse(stri_enc_toutf8(df$Location, validate = TRUE))) %>% 
  separate(location_rev, c("country_rev", NA), sep = " ,", extra = "drop") %>% 
  mutate(country_temp = stri_reverse(country_rev),
         Country = ifelse(grepl("USSR", Company), "Russia + USSR", country_temp)) %>% 
  select(Rocket, Country)

### Countries with most launches
df_country_launches <- df_country %>% 
  group_by(Country) %>% 
  summarise(launches = n())
top_countries <- df_country_launches[order(df_country_launches$launches, decreasing = TRUE),]$Country[1:2]


df_lifetime_top_countries <- df_lifetime %>% 
  inner_join(distinct(df_country), by = "Rocket") %>% 
  filter(Country %in% top_countries)

stats_country <- df %>%
  inner_join(distinct(df_country), by = "Rocket") %>%
  filter(Country %in% top_countries) %>%
  mutate(launch_year = year(Date),
         launch_group = ifelse(launch_year <= 1980, "le_1980", "gt_1980")) %>%
  group_by(Country, launch_group) %>%
  summarise(success_rate = sum(MissionStatus == "Success") / n(),
            launches = n()) %>% 
  inner_join(
    df_lifetime_top_countries %>% 
      group_by(Country, launch_group) %>% 
      summarise(grp.median = median(lifetime_years),
                grp.rockets = n()),
    by = c("Country", "launch_group")
  )

df_lifetime_top_countries %>% 
  ggplot(aes(x = lifetime_years, fill = Country)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  geom_vline(data = medians_country, aes(xintercept = grp.median, color = Country),
             linetype="dashed") +
  facet_wrap(~Country, nrow = 2)

my_theme <- theme_classic() +
  theme(plot.background = element_rect(fill = alpha("white", 0)),
        panel.background = element_rect(fill = alpha("black", 0.9)),
        axis.text = element_text(colour = "#b0bec5", size = rel(1.2)),
        axis.line = element_line(colour = "#b0bec5"),
        axis.title = element_text(color = "#b0bec5", size=14),
        plot.title = element_text(color = "black", size = 16),
        plot.margin = margin(20,20,20,20))

plot_rockets_lifetime <- function(dataset = df_lifetime_top_countries,
                                  country_name, launch_group_name,
                                  bar_color = "#9292d1", 
                                  medians = medians_country,
                                  title = "") {
  y_max <- 40
  x_max <- 50
  median_lifetime <- filter(medians, Country == country_name, launch_group == launch_group_name)$grp.median
  
  dataset %>% 
    filter(Country == country_name & launch_group == launch_group_name) %>% 
    ggplot(aes(x = lifetime_years)) +
    geom_histogram(position = "identity", fill = bar_color, color = bar_color) +
    ylim(NA, y_max) + xlim(NA, x_max) +
    geom_vline(xintercept = median_lifetime, color = "#fbc02d", linetype = "dashed") +
    annotate("text", x = median_lifetime+4, y = y_max/2,
             label = paste(round(median_lifetime, 2),"years"),
             angle = 0, color = "#fbc02d") +
    ggtitle(title) +
    xlab("Rocket lifetime (in years)") +
    ylab("Number of Rockets") +
    my_theme
}

plot_rockets_lifetime(country_name = "Russia + USSR", launch_group_name = "gt_1980",
                      title = "Russia + USSR")

?formatC
