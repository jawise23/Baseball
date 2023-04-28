library(dplyr)
library(ggplot2)
library(Lahman)

as_tibble(People)

batting_data <- as_tibble(Batting)
player_data <- as_tibble(People)

merged_data <- batting_data %>% 
  left_join(player_data, by = join_by(playerID)) %>% 
  mutate(age = yearID - birthYear)

min_pa = 500

merged_data <- merged_data %>%
  mutate(SLG = (H + X2B + 2 * X3B + 3 * HR) / AB) %>%
  mutate(OBP = (H + BB + HBP) / (AB + BB + HBP + SF)) %>%
  mutate(OPS = OBP + SLG) %>%
  group_by(yearID) %>%
  mutate(PA = AB + BB + HBP + SH + SF) %>%
  filter(PA >= min_pa) %>%
  ungroup()

age_curve_data <- merged_data %>%
  mutate(decade = 10 * floor(yearID / 10)) %>%
  group_by(decade, age) %>%
  summarize(mean_OPS = mean(OPS, na.rm = TRUE),
            n = n()) %>%
  filter(!is.na(age))

ggplot(age_curve_data, aes(x = age, y = mean_OPS, size = n)) +
  geom_line() +
  geom_point() +
  labs(x = "Age", y = "OPS", title = "Age Curve for OPS by Decade") +
  facet_wrap(~decade, nrow = 2) +
  scale_size_continuous(range = c(1, 6), breaks = seq(0, 200, 50), name = "Number of Observations") +
  theme_minimal()


selected_players <- c("bondsba01", "jeterde01", "griffke02", "pujolal01", "rodrial01")
filtered_data <- merged_data %>% filter(playerID %in% selected_players)

filtered_data <- filtered_data %>%
  mutate(age = yearID - birthYear)

ggplot(filtered_data, aes(x = age, y = OPS)) +
  geom_line(aes(group = playerID), color = "blue") +
  geom_point() +
  labs(x = "Age", y = "OPS", title = "OPS Progression as Players Age") +
  facet_wrap(~nameGiven, ncol = 2) +
  theme_minimal()

average_ops_by_age <- merged_data %>%
  group_by(age) %>%
  summarize(mean_OPS = mean(OPS, na.rm = TRUE),
            player_count = n()) %>%
  filter(!is.na(age))

ggplot(average_ops_by_age, aes(x = age, y = mean_OPS)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_text(aes(label = player_count), vjust = -0.5, size = 3.5) +
  labs(x = "Age", y = "Average Expected OPS", title = "Average Expected OPS as Players Age with Player Count Labels") +
  theme_minimal()

peak_age <- average_ops_by_age %>%
  top_n(1, wt = mean_OPS) %>%
  pull(age)

ggplot(average_ops_by_age, aes(x = age, y = mean_OPS)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_text(aes(label = player_count), vjust = -0.5, size = 3.5) +
  geom_vline(xintercept = peak_age, linetype = "dashed", color = "red") +
  annotate("text", x = peak_age, y = max(average_ops_by_age$mean_OPS), label = paste("Peak Age:", peak_age), hjust = -0.1, color = "red") +
  labs(x = "Age", y = "Average Expected OPS", title = "Average Expected OPS as Players Age with Peak Age Indicated") +
  theme_minimal()

player_count_by_age <- merged_data %>%
  group_by(age) %>%
  summarize(player_count = n()) %>%
  filter(!is.na(age))

ggplot(player_count_by_age, aes(x = age, y = player_count)) +
  geom_bar(stat = "identity") +
  labs(x = "Age", y = "Number of Players", title = "Number of Players at Each Age") +
  theme_minimal()

player_years_data <- merged_data %>%
  group_by(playerID, nameGiven) %>%
  summarize(start_year = min(yearID),
            end_year = max(yearID),
            num_years_played = end_year - start_year + 1) %>%
  ungroup()

player_years_data <- player_years_data %>%
  mutate(start_decade = 10 * floor(start_year / 10))

average_years_by_decade <- player_years_data %>%
  group_by(start_decade) %>%
  summarize(average_years_played = mean(num_years_played))

ggplot(average_years_by_decade, aes(x = factor(start_decade), y = average_years_played)) +
  geom_bar(stat = "identity") +
  labs(x = "Starting Decade", y = "Average Number of Years Played", title = "Average Number of Years a Player Played by Starting Decade") +
  theme_minimal()

player_start_data <- merged_data %>%
  group_by(playerID, nameGiven, birthYear) %>%
  summarize(start_year = min(yearID)) %>%
  ungroup()

player_start_data <- player_start_data %>%
  mutate(start_age = start_year - birthYear)

average_start_age <- player_start_data %>%
  summarize(average_start_age = mean(start_age, na.rm = TRUE)) %>%
  pull(average_start_age)

print(average_start_age)

player_start_data <- player_start_data %>%
  left_join(player_years_data %>%
              select(playerID, num_years_played), by = "playerID")

ggplot(player_start_data, aes(x = start_age, y = num_years_played)) +
  geom_point(alpha = 0.3) +
  labs(x = "Starting Age", y = "Number of Years Played", title = "Relationship Between Starting Age and Career Length") +
  theme_minimal()

correlation <- cor(player_start_data$start_age, player_start_data$num_years_played, use = "complete.obs")

correlation

players_started_at_20 <- player_start_data %>%
  filter(start_age == 22) 

ggplot(players_started_at_20, aes(x = num_years_played)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Number of Years Played", y = "Count", title = "Distribution of Years Played for Players Who Started at Age 20") +
  theme_minimal()

player_start_data_bins <- player_start_data %>%
  mutate(age_bin = cut(start_age, breaks = seq(from = floor(min(start_age)), to = ceiling(max(start_age)), by = 3), include.lowest = TRUE, right = FALSE))

average_years_by_age_bin <- player_start_data_bins %>%
  group_by(age_bin) %>%
  summarize(average_years_played = mean(num_years_played, na.rm = TRUE))

ggplot(average_years_by_age_bin, aes(x = age_bin, y = average_years_played)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Starting Age Group", y = "Average Number of Years Played", title = "Average Number of Years Played by Starting Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

starting_ages <- player_start_data$start_age
n <- length(starting_ages)
iqr_starting_ages <- IQR(starting_ages, na.rm = TRUE)
h <- 2 * iqr_starting_ages / (n^(1/3))

range_starting_ages <- range(starting_ages, na.rm = TRUE)
num_bins <- ceiling((range_starting_ages[2] - range_starting_ages[1]) / h)

player_start_data_bins_FD <- player_start_data %>%
  mutate(age_bin = cut(start_age, breaks = seq(from = floor(min(start_age)), to = ceiling(max(start_age)), by = h), include.lowest = TRUE, right = FALSE))

average_years_by_age_bin_FD <- player_start_data_bins_FD %>%
  group_by(age_bin) %>%
  summarize(average_years_played = mean(num_years_played, na.rm = TRUE))

ggplot(average_years_by_age_bin_FD, aes(x = age_bin, y = average_years_played)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Starting Age Group", y = "Average Number of Years Played", title = "Average Number of Years Played by Starting Age Group (Freedman-Diaconis)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add necessary columns to the batting dataset
batting <- batting %>%
  mutate(Singles = H - X2B - X3B - HR,
         BB = BB,
         HBP = ifelse(is.na(HBP), 0, HBP),
         PA = AB + BB + HBP + SF)

# Define weights (using 2021 weights from FanGraphs as an example)
w_single <- 0.881
w_double <- 1.242
w_triple <- 1.569
w_hr <- 2.065
w_walk <- 0.693
w_hbp <- 0.723
w_pa <- sum(batting$PA, na.rm = TRUE)

# Calculate wOBA
batting <- batting %>%
  mutate(wOBA = (w_single * Singles + w_double * X2B + w_triple * X3B + w_hr * HR + w_walk * BB + w_hbp * HBP) / PA)

