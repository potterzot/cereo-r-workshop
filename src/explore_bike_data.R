# Explore bike data to see if there is a relationship between weather and ridership

library(tidyverse)


#### Load the data to work with ----
df <- read_csv("data/daily_bike_data.csv")

df


#### Exploration of data relationships ----

# Time trend of ridership
p <- ggplot(data = df) +
  geom_line(aes(x = dteday, y = cnt))
p

# Relationship between ridership and temperature
ggplot(data = df, aes(x = temp, y = cnt)) +
  geom_point() +
  geom_smooth()

# What is weathersit?
summary(df$weathersit)
unique(df$weathersit)  

# Dplyr verbs (some of them):
# mutate: adds new columns to your data frame (adds new variables to your dataset)
# transmute: keeps only new the columns
# select: selects columns from the dataset
# filter: filters the rows according to some logical specification

df2 <- df %>%
  dplyr::mutate(
    weather_fac = factor(weathersit,
                         levels = c(1,2,3,4),
                         labels = c("Clear", "Cloudy", "Rainy", "Heavy Rain"))
  )

df2 %>% dplyr::select(dteday, weathersit, weather_fac)

df2 %>% 
  dplyr::filter(weather_fac == "Rainy") %>%
  ggplot(aes(x = temp, y = cnt)) +
    geom_point() +
    geom_smooth()

# dplyr::select, you can drop variables
df3 <- df2 %>%
  dplyr::select(-weathersit)

# can also use character lists
keep_vars <- c("dteday", "weather_fac", "temp", "cnt")
df4 <- df2 %>% select( all_of( keep_vars ) ) 

# Other ways of filtering
weather_factors_we_like <- c("Rainy", "Cloudy")
df2 %>% dplyr::filter(weather_fac == "Rainy" | weather_fac == "Cloudy")
df2 %>% dplyr::filter(weather_fac %in% weather_factors_we_like)
df2 %>% dplyr::filter(weather_fac != "Rainy")
df2 %>% dplyr::filter( !(weather_fac %in% weather_factors_we_like) )

## dplyr::summarize
df2 %>% 
  dplyr::group_by(season, weather_fac) %>%
  dplyr::summarize(
    cnt_mean = mean(cnt)
  )

### Tranforming data format from long to wide or vice-versa

#Transform to create separate temp variables for each month
months <- c("January", "February", "March", "April", "May", "June", 
            "July", "September", "October", "November", "December")
df_wide <- df2 %>%
  dplyr::mutate(mnth = factor(mnth, levels = months, labels = months)) %>%
  dplyr::rename(year = yr) %>%
  dplyr::select(year, mnth, temp) %>%
  dplyr::group_by(year, mnth) %>%
  dplyr::summarize(temp_mean = mean(temp)) %>%
  tidyr::pivot_wider(names_prefix = "temp_", names_from = mnth, values_from = temp_mean) %>%
  dplyr::rename_with(tolower)

# Alternative syntax to:
# tidyr::pivot_wider(names_prefix = "temp_", names_from = mnth, values_from = temp_mean) %>%
# tidyr::spread(key = mnth, values = temp_mean)  

## Pivoting longer
df_long <- df2 %>%
  tidyr::pivot_longer(cols = c(temp, atemp, hum, windspeed), 
                      values_to = "value", names_to = "variable")

## Pivoting wider
df_wide2 <- df_long %>%
  tidyr::pivot_wider(names_prefix = "v_", 
                     names_from = variable, values_from = value)



## Facetting
p <- ggplot(data = df2, aes(x = temp, y = cnt)) +
  geom_point(shape = 21, color = "orangered") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "steelblue", se = FALSE) +
  facet_wrap(~ weather_fac, scales = "free_y") +
  labs(x = "Temperature", y = "Ridership County") +
  ggtitle("Relationship between temperature and ridership") +
  theme_linedraw() +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(color = "black"))
ggsave(plot = p, filename = "temp_county_scatter.png")

## PLotting with a longer data frame
p <- ggplot(data = df2, aes(x = temp, y = cnt)) +
  geom_point(shape = 21, color = "orangered") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "steelblue", se = FALSE) +
  facet_wrap(~ weather_fac, scales = "free_y") +
  labs(x = "Temperature", y = "Ridership County") +
  ggtitle("Relationship between temperature and ridership") +
  theme_linedraw() +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(color = "black"))
ggsave(plot = p, filename = "temp_county_scatter.png")

ggplot(data = df_long, aes(x = value, y = cnt, color = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ weather_fac)


nicholas.a.potter@wsu.edu



