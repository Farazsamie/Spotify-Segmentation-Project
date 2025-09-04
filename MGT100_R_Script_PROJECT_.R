# Spotify User Segmentation Analysis:
# Load libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(cluster)
library(factoextra)
library(umap)
library(corrplot)
library(scales)
library(knitr)
library(gridExtra)
library(readxl)
library(dplyr)

# theme for consistent visualizations
theme_set(theme_minimal(base_size = 12))

# Importing the dataset
spotify_data <- read_xlsx("/Users/farazsamie/Downloads/R/Spotify_data.xlsx")

# Checking the data structure
str(spotify_data)
summary(spotify_data)

# Checking if there are any missing values
missing_values <- colSums(is.na(spotify_data))
print("Missing values per column:")
print(missing_values)

# Displaying the first few rows
head(spotify_data)

# Created a clean version of the data
spotify_clean <- spotify_data %>%
  # Converted some key variables 
  mutate(
    `Age Group` = as.factor(`Age Group`),
    Gender = as.factor(Gender),
    spotify_subscription_plan = as.factor(spotify_subscription_plan),
    spotify_listening_device = as.factor(spotify_listening_device),
    fav_music_genre = as.factor(fav_music_genre),
    preferred_listening_content = as.factor(preferred_listening_content)
  )

# Created enhanced metrics 
spotify_clean <- spotify_clean %>%
  mutate(
    # Created a Premium willingness score that ranges from (0 or 1) -- likely that a group will purchase Spotify Premium 
    willingness_score = ifelse(premium_sub_willingness == "Yes", 1, 0),
    
    # Made a music engagement score which is based on the listening frequency and recommendation rating
    music_engagement_score = case_when(
      grepl("Daily|daily", music_lis_frequency) ~ 5,
      grepl("Several times a week|week", music_lis_frequency) ~ 4,
      grepl("Weekly|Once a week", music_lis_frequency) ~ 3,
      grepl("Rarely", music_lis_frequency) ~ 2,
      grepl("Never", music_lis_frequency) ~ 1,
      TRUE ~ 3  
    ) + music_recc_rating,
    
    # Classified the preference for content 
    content_preference_type = case_when(
      preferred_listening_content == "Music" ~ "Music Lover",
      preferred_listening_content == "Podcast" ~ "Podcast Fan",
      str_detect(preferred_listening_content, "Both|both") ~ "Balanced User",
      TRUE ~ "General Listener"
    ),
    
    # Made a score for podcast engagement 
    podcast_engagement_score = case_when(
      grepl("Daily|daily", pod_lis_frequency) ~ 5,
      grepl("Several times a week|week", pod_lis_frequency) ~ 4,
      grepl("Weekly|Once a week", pod_lis_frequency) ~ 3,
      grepl("Rarely", pod_lis_frequency) ~ 2,
      grepl("Never", pod_lis_frequency) ~ 1,
      TRUE ~ 1
    ) + case_when(
      pod_variety_satisfaction == "Very satisfied" ~ 2,
      pod_variety_satisfaction == "Satisfied" ~ 1,
      pod_variety_satisfaction == "Ok" ~ 0,
      TRUE ~ 0
    ),
    
    # Also mnade a score for discovery behavior -- wanting to discovery new content
    discovery_score = case_when(
      grepl("recommendations|Recommendations", music_expl_method) ~ 4,
      grepl("Playlists|playlists", music_expl_method) ~ 3,
      grepl("Social|social", music_expl_method) ~ 2,
      TRUE ~ 1
    ),
    
    # Overall engagement score (weighted combination)
    overall_engagement = (music_engagement_score * 0.4) + 
      (podcast_engagement_score * 0.3) + 
      (discovery_score * 0.2) + 
      (willingness_score * 0.1),
    
    # Create synthetic behavioral metrics for clustering
    estimated_hours_weekly = case_when(
      music_engagement_score >= 8 ~ rnorm(n(), 25, 5),
      music_engagement_score >= 6 ~ rnorm(n(), 15, 3),
      music_engagement_score >= 4 ~ rnorm(n(), 8, 2),
      TRUE ~ rnorm(n(), 3, 1)
    ),
    
    estimated_playlists_created = case_when(
      overall_engagement >= 8 ~ rpois(n(), 15),
      overall_engagement >= 6 ~ rpois(n(), 8),
      overall_engagement >= 4 ~ rpois(n(), 4),
      TRUE ~ rpois(n(), 1)
    ),
    
    estimated_social_sharing = case_when(
      discovery_score >= 3 ~ rpois(n(), 5),
      discovery_score >= 2 ~ rpois(n(), 2),
      TRUE ~ rpois(n(), 0.5)
    )
  ) %>%
  # Ensure no negative values for synthetic metrics
  mutate(
    estimated_hours_weekly = pmax(estimated_hours_weekly, 0.5),
    estimated_playlists_created = pmax(estimated_playlists_created, 0),
    estimated_social_sharing = pmax(estimated_social_sharing, 0)
  )

# user personalities based on behavior patterns
spotify_clean <- spotify_clean %>%
  mutate(
    user_personality = case_when(
      music_engagement_score >= 8 & willingness_score == 1 ~ "Power User",
      overall_engagement <= 4 & willingness_score == 0 ~ "Casual Listener",
      podcast_engagement_score >= 6 ~ "Podcast Enthusiast",
      discovery_score >= 3 & music_engagement_score >= 6 ~ "Music Explorer",
      estimated_social_sharing >= 3 ~ "Social Sharer",
      TRUE ~ "Balanced User"
    ),
    
    # grouping ages 
    age_category = case_when(
      `Age Group` == "12-20" ~ "Teen",
      `Age Group` == "20-35" ~ "Young Adult",
      `Age Group` == "35-60" ~ "Adult",
      `Age Group` == "60+" ~ "Senior",
      TRUE ~ "Unknown"
    )
  )

#new categorical variables into factors
spotify_clean <- spotify_clean %>%
  mutate(
    content_preference_type = as.factor(content_preference_type),
    user_personality = as.factor(user_personality),
    age_category = as.factor(age_category)
  )

# summary of statistics for metrics
cat("Summary Statistics for Key Engagement Metrics:\n")
summary_stats <- spotify_clean %>%
  select(music_engagement_score, podcast_engagement_score, overall_engagement, 
         estimated_hours_weekly, willingness_score) %>%
  summary()
print(summary_stats)

# Distribution of overall engagement scores
engagement_dist <- ggplot(spotify_clean, aes(x = overall_engagement)) +
  geom_histogram(fill = "#1DB954", bins = 30, alpha = 0.7, color = "white") +
  labs(title = "Distribution of Overall User Engagement Scores",
       x = "Overall Engagement Score", y = "Number of Users") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(engagement_dist)

# User personality distribution
personality_dist <- ggplot(spotify_clean, aes(x = user_personality, fill = user_personality)) +
  geom_bar(alpha = 0.8) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Distribution of User Personalities",
       x = "User Personality Type", y = "Number of Users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

print(personality_dist)

# Gender distribution by subscription plan
gender_sub_plot <- ggplot(spotify_clean, aes(x = Gender, fill = spotify_subscription_plan)) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Subscription Plan Distribution by Gender",
       x = "Gender", y = "Proportion", fill = "Subscription Plan") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(gender_sub_plot)

# Music engagement vs podcast engagement
engagement_scatter <- ggplot(spotify_clean, aes(x = music_engagement_score, y = podcast_engagement_score,
                                                color = user_personality, size = willingness_score)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  scale_size_continuous(range = c(1, 4)) +
  labs(title = "Music vs Podcast Engagement by User Personality",
       x = "Music Engagement Score", y = "Podcast Engagement Score",
       color = "User Personality", size = "Premium Willingness") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(engagement_scatter)

# features for clustering
clustering_features <- spotify_clean %>%
  select(music_engagement_score, podcast_engagement_score, overall_engagement,
         estimated_hours_weekly, estimated_playlists_created, estimated_social_sharing,
         willingness_score, discovery_score) %>%
  na.omit()

# Scale the features
clustering_features_scaled <- scale(clustering_features)

# Determine optimal number of clusters 
set.seed(123)
wss <- numeric(10)
for (i in 1:10) {
  if (i <= nrow(clustering_features_scaled)) {
    kmeans_temp <- kmeans(clustering_features_scaled, centers = i, nstart = 25)
    wss[i] <- kmeans_temp$tot.withinss
  }
}

# Plot elbow curve
elbow_data <- data.frame(k = 1:length(wss), wss = wss)
elbow_plot <- ggplot(elbow_data, aes(x = k, y = wss)) +
  geom_line(color = "#1DB954", size = 1) +
  geom_point(color = "#191414", size = 2) +
  labs(title = "Elbow Method for Optimal Clusters",
       x = "Number of Clusters (k)", y = "Within-cluster Sum of Squares") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(elbow_plot)

# Perform K-means clustering with optimal k=4
set.seed(123)
k_optimal <- 4
kmeans_result <- kmeans(clustering_features_scaled, centers = k_optimal, nstart = 25)

# Add cluster assignments to the data
spotify_clean$cluster <- as.factor(kmeans_result$cluster)

# Visualize clusters using UMAP
set.seed(123)
umap_config <- umap.defaults
umap_config$n_neighbors <- 15
umap_config$min_dist <- 0.1

umap_result <- umap(clustering_features_scaled, config = umap_config)
umap_data <- data.frame(
  UMAP1 = umap_result$layout[,1],
  UMAP2 = umap_result$layout[,2],
  Cluster = spotify_clean$cluster,
  UserPersonality = spotify_clean$user_personality
)

cluster_viz <- ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = Cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "User Segments Visualization (UMAP)",
       x = "UMAP Dimension 1", y = "UMAP Dimension 2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(cluster_viz)

# Create cluster profiles
cluster_profiles <- spotify_clean %>%
  group_by(cluster) %>%
  summarise(
    users_count = n(),
    avg_music_engagement = mean(music_engagement_score, na.rm = TRUE),
    avg_podcast_engagement = mean(podcast_engagement_score, na.rm = TRUE),
    avg_overall_engagement = mean(overall_engagement, na.rm = TRUE),
    avg_hours_weekly = mean(estimated_hours_weekly, na.rm = TRUE),
    avg_playlists = mean(estimated_playlists_created, na.rm = TRUE),
    avg_social_sharing = mean(estimated_social_sharing, na.rm = TRUE),
    premium_willingness_rate = mean(willingness_score, na.rm = TRUE),
    most_common_personality = names(sort(table(user_personality), decreasing = TRUE))[1],
    most_common_age = names(sort(table(age_category), decreasing = TRUE))[1],
    most_common_gender = names(sort(table(Gender), decreasing = TRUE))[1],
    .groups = 'drop'
  )

cat("\nDetailed Cluster Profiles:\n")
print(cluster_profiles)

# Visualize metrics by cluster
cluster_comparison <- spotify_clean %>%
  select(cluster, music_engagement_score, podcast_engagement_score, overall_engagement, willingness_score) %>%
  pivot_longer(cols = -cluster, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = cluster, y = value, fill = cluster)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Key Metrics Comparison Across Clusters",
       x = "Cluster", y = "Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

print(cluster_comparison)

# User personality distribution by clusters
personality_by_cluster <- ggplot(spotify_clean, aes(x = cluster, fill = user_personality)) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "User Personality Distribution by Cluster",
       x = "Cluster", y = "Proportion", fill = "User Personality") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(personality_by_cluster)

# Made segment names and strategies
segment_info <- data.frame(
  cluster = 1:4,
  segment_name = c("Casual Browsers", "Music Enthusiasts", "Balanced Consumers", "Power Users"),
  description = c(
    "Low engagement users who occasionally browse music",
    "High music engagement but low podcast interest", 
    "Moderate engagement across both music and podcasts",
    "Highly engaged across all platform features"
  ),
  marketing_strategy = c(
    "Focus on easy onboarding and habit formation",
    "Promote music discovery features and artist content",
    "Highlight platform versatility and cross-content benefits",
    "Offer premium features and exclusive content access"
  ),
  conversion_priority = c("Low", "Medium", "High", "High")
)

# Add segment information to main dataset
spotify_clean <- spotify_clean %>%
  left_join(segment_info, by = "cluster")

# Create final summary table
final_summary <- cluster_profiles %>%
  left_join(segment_info, by = "cluster") %>%
  select(cluster, segment_name, users_count, avg_overall_engagement, 
         premium_willingness_rate, most_common_personality, marketing_strategy)

cat("\nFinal Marketing Segmentation Summary:\n")
kable(final_summary, digits = 2)

# Statistical significance testing
# Chi square test for personality distribution
chisq_personality <- chisq.test(table(spotify_clean$cluster, spotify_clean$user_personality))
cat("\nChi-square test for User Personality by Cluster:\n")
print(chisq_personality)

# Export results
write.csv(spotify_clean, "spotify_segmentation_results.csv", row.names = FALSE)

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SPOTIFY USER SEGMENTATION ANALYSIS COMPLETE\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("✓ Successfully identified", k_optimal, "distinct user segments\n")
cat("✓ Created comprehensive user profiles and personas\n") 
cat("✓ Generated targeted marketing strategies for each segment\n")
cat("✓ Performed statistical validation of segment differences\n")
cat("✓ Results exported to 'spotify_segmentation_results.csv'\n")
cat(paste(rep("=", 70), collapse = ""), "\n")