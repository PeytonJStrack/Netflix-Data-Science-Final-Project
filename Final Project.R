library(tidyverse)
library(showtext)

font_add(family = "sunset_boulevard", regular = "C:/Users/Peyto/AppData/Local/Microsoft/Windows/Fonts/Sunset Boulevard.otf")
showtext_auto()

n_df <- read.csv("netflix_titles.csv")

n_df %>%
  filter(release_year >= 2000) -> n_df

n_df$date_added <- mdy(n_df$date_added)
n_df$year_added <- year(n_df$date_added)


#Graph 1: Genre Popularity Across the Years (2000 to 2021) (Heatmap)
n_df %>%
  separate_rows(listed_in, sep = ", ") %>%
  count(release_year, listed_in) %>%
  group_by(listed_in) %>%
  filter(sum(n) > 50) %>%   #Keep genres with more than 50 titles
  filter(listed_in != "Movies") %>%
  arrange(n) -> g1

g1$listed_in <- reorder(g1$listed_in, g1$n) #Orders by popularity

ggplot(g1, aes(x = release_year, y = listed_in, fill = n)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("#5b3b3b", "#b20710", "#ff6b6b")) +
  scale_x_continuous(breaks = seq(min(g1$release_year), max(g1$release_year), by = 3)) +
  labs(title = "Popularity of Genres Over the Years", subtitle = "From 2000 to 2021", x = "Movie Release Year", y = "Genres", fill = "Number of Titles") +
  theme(text = element_text(color = "white"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "grey"), axis.text.x = element_text(color = "white", size = 12, margin = margin(t = 0)), axis.text.y.left = element_text(color = "white", size = 10), plot.title = element_text(size = 22, family = "sunset_boulevard", margin = margin(b = 10, t = -2)), plot.subtitle = element_text(size = 12, family = "sans", face = "bold", margin = margin(b = 6, t = -4)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.title = element_text(size = 10, family = "sunset_boulevard", color = "black"), legend.text = element_text(color = "black"), legend.background = element_rect(fill = "lightgrey"))

#Graph 2: International vs US movies over time (2000 to 2021)
n_df %>%
  separate_rows(country, sep = ", ") %>%
  filter(country != "") %>%
  mutate(country = ifelse(country == "United States", "United States", "International")) %>%
  count(release_year, country) %>%
  group_by(release_year) %>%
  mutate(Percent = n / sum(n) * 100) -> us_df

ggplot(us_df, aes(x = release_year, y = Percent, fill = country)) +
  geom_col(position = "stack", width = .5) +
  scale_x_continuous(breaks = seq(min(us_df$release_year), max(us_df$release_year), by = 3)) +
  scale_fill_manual(values = c("United States" = "red", "International" = "#2ca9d0")) +
  labs(title = "Percent of United States vs Internationally Produced Content", subtitle = "Released From 2000 to 2021", x = "Release Year", y = "Percent of Content") +
  theme(text = element_text(color = "white"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "#3c3c3c"), axis.text.x = element_text(color = "white", size = 12, margin = margin(t = 0)), axis.text.y.left = element_text(color = "white", size = 10), plot.title = element_text(size = 22, family = "sunset_boulevard", margin = margin(b = 10, t = -2)), plot.subtitle = element_text(size = 12, family = "sans", face = "bold", margin = margin(b = 6, t = -4)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.title = element_text(size = 10, family = "sunset_boulevard", color = "black"), legend.text = element_text(color = "black"), legend.background = element_rect(fill = "lightgrey"))

  

#Graph 3: Popularity of shows vs movies across the years (2000 to 2021)
n_df %>%
  count(year_added, type) -> sh_df
sh_df$year_added[sh_df$year_added == ""] <- NA
sh_df %>%
  na.omit() -> csh_df

ggplot(csh_df, aes(x= year_added, y = n, group = type)) +
  geom_line(aes(color = type), linewidth = 1, position = position_dodge(width = 0.3)) +
  geom_point(aes(color = type), size = 2, position = position_dodge(width = 0.3)) + 
  scale_x_continuous(breaks = seq(min(csh_df$year_added), max(csh_df$year_added), by = 1)) +
  scale_y_continuous(breaks = seq(0, 1500, by = 250)) +
  scale_color_manual(values = c("Movie" = "red", "TV Show" = "#4cc9f0")) +
  labs(title = "Popularity by Content Type", subtitle = "Added From 2000 to 2021", x = "Year Added to Netflix", y = "Number of Titles") +
  theme(text = element_text(color = "white"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "#303030"), axis.text.x = element_text(color = "white", size = 12, margin = margin(t = 0)), axis.text.y.left = element_text(color = "white", size = 10), plot.title = element_text(size = 22, family = "sunset_boulevard", margin = margin(b = 10, t = -2)), plot.subtitle = element_text(size = 12, family = "sans", face = "bold", margin = margin(b = 6, t = -4)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.title = element_text(size = 10, family = "sunset_boulevard", color = "black"), legend.text = element_text(color = "black"), legend.background = element_rect(fill = "lightgrey"))


#Graph 4: Genre Dominance by Country
n_df %>%
  separate_rows(listed_in, sep = ", ") %>%
  separate_rows(country, sep = ", ") %>%
  filter(listed_in != "") %>%
  mutate(country = ifelse(country == "United States", "United States", "International")) %>%
  count(country, listed_in) %>%
  group_by(country) %>%
  mutate(Percent = n / sum(n) * 100) %>%
  filter(Percent >= 2.5) -> usg_df

ggplot(usg_df, aes(x = listed_in, y = Percent, fill = listed_in)) +
  geom_col() +
  coord_polar() +
  facet_wrap(~country) +
  geom_text(aes(label = paste(round(Percent,1), "%"), y = Percent + 2), color = "white", size = 3, family = "sans", hjust = 0.5, fontface = "bold") +
  labs(title = "Genre Distribution in the United States vs Internationally", x = "", y = "Percent of Titles") +
  theme(strip.text = element_text(size = 14), text = element_text(color = "white", family = "sunset_boulevard"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "#222"), panel.grid.major = element_line(color = "grey"), axis.text.x = element_text(color = "white", size = 10, hjust = 1, family = "sans", face = "bold"), axis.text.y = element_blank(), plot.title = element_text(size = 22, margin = margin(b = 10, t = -2)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.position = "none")

#Graph 5: Movie Duration over the years
n_df %>%
  filter(type == "Movie", duration != "") %>%
  mutate(duration = parse_number(duration)) %>%
  group_by(release_year, duration) -> md_df

ggplot(md_df, aes(x = as.factor(release_year), y = duration)) +
  geom_violin(fill = "#b20710", color = "white", alpha = 0.7) +
  stat_summary(fun = median, geom = "point", color = "white", size = 2) +
  scale_x_discrete(breaks = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021)) +
  labs(title = "Distribution of Movie Durations", subtitle = "Released From 2000 to 2021", x = "Release Year", y = "Duration in minutes") +
  theme(text = element_text(color = "white"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "#303030"), axis.text.x = element_text(color = "white", size = 12), axis.text.y.left = element_text(color = "white", size = 10), plot.title = element_text(size = 22, family = "sunset_boulevard", margin = margin(b = 10, t = -2)), plot.subtitle = element_text(size = 12, family = "sans", face = "bold", margin = margin(b = 6, t = -4)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.title = element_text(size = 10, family = "sunset_boulevard", color = "black"), legend.text = element_text(color = "black"), legend.background = element_rect(fill = "lightgrey"))

#Graph 6: 

#Graph 7: 

#Graph 8: