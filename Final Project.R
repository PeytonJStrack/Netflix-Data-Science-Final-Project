library(tidyverse)
library(showtext)

font_add(family = "sunset_boulevard", regular = "C:/Users/Peyto/AppData/Local/Microsoft/Windows/Fonts/Sunset Boulevard.otf")
font_add(family = "sunset_boulevard", regular = "C:/Users/pstrack/AppData/Local/Microsoft/Windows/Fonts/Sunset Boulevard.otf")
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
  filter(sum(n) >= 50) %>%   #Keep genres with more than 50 titles
  filter(listed_in != "Movies") %>%
  arrange(n) -> g_df

ggplot(g_df, aes(x = release_year, y = reorder(listed_in, n), fill = n)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("#5b3b3b", "#b20710", "#ff6b6b")) +
  scale_x_continuous(breaks = seq(min(g_df$release_year), max(g_df$release_year), by = 3)) +
  labs(title = "Popularity of Genres Over the Years", subtitle = "For Genres with 50 or more Titles", x = "Movie Release Year", y = "Genres", fill = "Number of Titles") +
  theme(text = element_text(color = "white"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "grey"), axis.text.x = element_text(color = "white", size = 12, margin = margin(t = 0)), axis.text.y.left = element_text(color = "white", size = 10), plot.title = element_text(size = 22, family = "sunset_boulevard", margin = margin(b = 7.5, t = -4)), plot.subtitle = element_text(size = 12, family = "sans", face = "bold", margin = margin(b = 6, t = -4)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.title = element_text(size = 10, family = "sunset_boulevard", color = "black"), legend.text = element_text(color = "black"), legend.background = element_rect(fill = "lightgrey"))

#Graph 2: International vs US movies over time (2000 to 2021) (Stacked bar chart)
n_df %>%
  separate_rows(country, sep = ", ") %>%
  filter(country != "") %>%
  mutate(country = ifelse(country == "United States", "United States", "International")) %>%
  count(release_year, country) %>%
  group_by(release_year) %>%
  mutate(Percent = n / sum(n) * 100) -> us_df

ggplot(us_df, aes(x = release_year, y = Percent, fill = country)) +
  geom_area(position = "fill", alpha = 0.85, color = "white", linewidth = 0.2) +
  scale_x_continuous(breaks = seq(min(us_df$release_year), max(us_df$release_year), by = 3)) +
  scale_fill_manual(values = c("United States" = "red", "International" = "#2ca9d0")) +
  labs(title = "Percent of United States vs Internationally Produced Content", subtitle = "Released From 2000 to 2021", x = "Release Year", y = "Percent of Content") +
  theme(text = element_text(color = "white"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "#4c4c4c"), axis.text.x = element_text(color = "white", size = 12, margin = margin(t = 0)), axis.text.y.left = element_text(color = "white", size = 10), plot.title = element_text(size = 22, family = "sunset_boulevard", margin = margin(b = 7.5, t = -4)), plot.subtitle = element_text(size = 12, family = "sans", face = "bold", margin = margin(b = 6, t = -4)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.title = element_text(size = 14, family = "sunset_boulevard", color = "black"), legend.text = element_text(color = "black"), legend.background = element_rect(fill = "lightgrey"))

  

#Graph 3: Director Netflix Debut (2000 to 2021) (Density graph)
n_df %>%
  filter(director != "", country == "United States") %>%
  separate_rows(director, sep = ", ") %>%
  count(director) %>%
  slice_max(n, n = 15, with_ties = FALSE) -> dir_df

ggplot(dir_df, aes(x = reorder(director, n), y = n)) +
  geom_segment(aes(xend = director, y = 0, yend = n), color = "#b20710", linewidth = 1.5) +
  geom_point(size = 7.5, color = "#b20710") +
  geom_text(aes(label = n), color = "white", fontface = "bold") +
  labs(title = "Top Directors of US Produced Media on Netflix", subtitle = "From 2000 to 2021",x = "Director", y = "Number of Titles") +
  theme(panel.grid.major = element_line(color = "#888"), panel.grid.minor = element_line(color = "#888"), text = element_text(color = "white"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "#303030"), axis.text.x = element_text(color = "white", size = 10, angle = 45, margin = margin(t = 12)), axis.text.y.left = element_text(color = "white", size = 10), plot.title = element_text(size = 22, family = "sunset_boulevard", margin = margin(b = 7.5, t = -4)), plot.subtitle = element_text(size = 12, family = "sans", face = "bold", margin = margin(b = 6, t = -4)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.title = element_text(size = 10, family = "sunset_boulevard", color = "black"), legend.text = element_text(color = "black"), legend.background = element_rect(fill = "lightgrey"))


#Graph 4: Genre Dominance by Country (polar bar chart)
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
  theme(strip.text = element_text(size = 14), text = element_text(color = "white", family = "sunset_boulevard"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "#222"), panel.grid.major = element_line(color = "grey"), axis.text.x = element_text(color = "white", size = 9, hjust = 1, family = "sans", face = "bold"), axis.text.y = element_blank(), plot.title = element_text(size = 22, margin = margin(b = 7.5, t = -4)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.position = "none")

#Graph 5: Movie Duration over the years (Violin plot)
n_df %>%
  filter(type == "Movie", duration != "") %>%
  mutate(duration = parse_number(duration)) %>%
  group_by(release_year, duration) -> md_df

ggplot(md_df, aes(x = as.factor(release_year), y = duration)) +
  geom_violin(fill = "#b20710", color = "white", alpha = 0.7) +
  stat_summary(fun = median, geom = "point", color = "white", size = 2) +
  scale_x_discrete(breaks = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021)) +
  labs(title = "Distribution of Movie Durations", subtitle = "Released From 2000 to 2021", x = "Release Year", y = "Duration in minutes") +
  theme(panel.grid.major = element_line(color = "#888"), panel.grid.minor = element_line(color = "#888"), text = element_text(color = "white"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "#303030"), axis.text.x = element_text(color = "white", size = 12), axis.text.y.left = element_text(color = "white", size = 10), plot.title = element_text(size = 22, family = "sunset_boulevard", margin = margin(b = 7.5, t = -4)), plot.subtitle = element_text(size = 12, family = "sans", face = "bold", margin = margin(b = 6, t = -4)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.title = element_text(size = 10, family = "sunset_boulevard", color = "black"), legend.text = element_text(color = "black"), legend.background = element_rect(fill = "lightgrey"))

#Graph 6: Jitterplot of number of seasons based on age rating     (**NOTE: Jitterplot for categorical data, scatterplot for continuous numerical data)
n_df %>%
  filter(type == "TV Show", rating != "", rating != "NR", duration != "") %>%
  mutate(duration = parse_number(duration)) %>%
  group_by(rating) %>%
  filter(n() >= 10) %>%
  mutate(rating = factor(rating, levels = c("TV-Y", "TV-Y7", "TV-G", "TV-PG", "TV-14", "TV-MA"))) -> mr_df

ggplot(mr_df, aes(x = rating, y = duration, color = rating)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3.5, color = "white") +
  scale_y_continuous(breaks = seq(0, max(mr_df$duration), by = 4)) +
  labs(title = "Number of Seasons per Show by Age Rating", subtitle = "For ratings with 10 or more occurances", x = "Age Rating", y = "Number of Seasons") +
  theme(panel.grid.major = element_line(color = "#888"), panel.grid.minor = element_line(color = "#888"), text = element_text(color = "white"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "#303030"), axis.text.x = element_text(color = "white", size = 12), axis.text.y.left = element_text(color = "white", size = 10), plot.title = element_text(size = 22, family = "sunset_boulevard", margin = margin(b = 7.5, t = -4)), plot.subtitle = element_text(size = 12, family = "sans", face = "bold", margin = margin(b = 6, t = -4)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.title = element_text(size = 10, family = "sunset_boulevard", color = "black"), legend.text = element_text(color = "black"), legend.background = element_rect(fill = "lightgrey"))

#Graph 7: Top 20 Actors in US media (Flipped Bar Graph)
n_df %>%
  filter(cast != "", country == "United States") %>% 
  separate_rows(cast, sep = ", ") %>% 
  count(cast) %>% 
  group_by(cast) %>%
  mutate(tot = sum(n)) %>% 
  ungroup() %>%
  slice_max(tot, n = 20) -> c_df

ggplot(c_df, aes(x = reorder(cast, tot), y = tot)) +
  geom_col(fill = "#b20710", alpha = 0.95) +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.2, color = "white", size = 3.5, family = "sans", fontface = "bold") +
  labs(title = "Top Actors in US Produced Media on Netflix", subtitle = "From 2000 to 2021", x = "Actors/Actresses", y = "Number of Titles") +
  theme(panel.grid.major = element_line(color = "#888"), panel.grid.minor = element_line(color = "#888"), text = element_text(color = "white"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "#303030"), axis.text.x = element_text(color = "white", size = 12), axis.text.y.left = element_text(color = "white", size = 10), plot.title = element_text(size = 22, family = "sunset_boulevard", margin = margin(b = 7.5, t = -4)), plot.subtitle = element_text(size = 12, family = "sans", face = "bold", margin = margin(b = 6, t = -4)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.title = element_text(size = 10, family = "sunset_boulevard", color = "black"), legend.text = element_text(color = "black"), legend.background = element_rect(fill = "lightgrey"))

#Graph 8: Delay between Title release and addition to Netflix (bubbleplot)
n_df %>%
  filter(year_added != "NA") %>%
  mutate(delay = year_added - release_year) %>%
  count(release_year, delay) -> d_df

ggplot(d_df, aes(x = release_year, y = delay)) +
  geom_point(aes(size = n), color = "#c20710", alpha = 0.8) +
  scale_size(range = c(1, 8)) +
  scale_x_continuous(breaks = seq(min(d_df$release_year), max(d_df$release_year), by = 3)) +
  labs(title = "Delay Between Release Date and Netflix Addition Date", subtitle = "Larger Bubbles Correlate to More Titles", x = "Release Year", y = "Years Until Added", size = "Number of Titles") +
  theme(panel.grid.major = element_line(color = "#888"), panel.grid.minor = element_line(color = "#888"), text = element_text(color = "white"), plot.background = element_rect(fill = "#0b0b0b", color = "#e50914", linewidth = 16), panel.background = element_rect(fill = "#303030"), axis.text.x = element_text(color = "white", size = 12), axis.text.y.left = element_text(color = "white", size = 10), plot.title = element_text(size = 22, family = "sunset_boulevard", margin = margin(b = 7.5, t = -4)), plot.subtitle = element_text(size = 12, family = "sans", face = "bold", margin = margin(b = 6, t = -4)), axis.title.x = element_text(size = 16, family = "sunset_boulevard", margin = margin(t = 8, b = -1)), axis.title.y = element_text(size = 16, family = "sunset_boulevard"), legend.title = element_text(size = 10, family = "sunset_boulevard", color = "black"), legend.text = element_text(color = "black"), legend.background = element_rect(fill = "lightgrey"))
