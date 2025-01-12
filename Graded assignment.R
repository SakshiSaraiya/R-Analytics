#load packages
library(tidyverse)
library(dplyr)
library(ggplot2)

#load dataset
spotify<-read.csv("C:/Users/Sakshi saraiya/OneDrive/Desktop/spotify-2023.csv")
View(spotify)

#view first 10 rows
View(head(spotify,10))

#view last 10 rows
view(tail(spotify,10))

#view structure 
str(spotify)

#summary statistics of dataset
print(summary(spotify))

#check missing values
colSums(is.na(spotify))

#remove duplicates
spotify_clean<-spotify%>%distinct()
view(spotify_clean)

#1.Overall insights into streams, playlists, and charts.
spotify_data<-spotify_clean %>%
  summarise(
    total_streams = sum(streams, na.rm = TRUE),
    avg_streams = mean(streams, na.rm = TRUE),
    max_streams = max(streams, na.rm = TRUE),
    avg_playlists = mean(in_spotify_playlists, na.rm = TRUE) )
view(spotify_data)

#2.Top Artists by Streams
top_artists <- spotify_clean%>%
  group_by(artist.s._name) %>%
  summarise(total_streams = sum(streams, na.rm = TRUE)) %>%
  arrange(desc(total_streams)) 
view(head(top_artists))

#3.Top Tracks by Spotify Charts Appearances
top_charted_tracks <- spotify_clean %>%
  arrange(desc(in_spotify_charts)) %>%
  select(track_name, artist.s._name, in_spotify_charts) 
view(head(top_charted_tracks,10))

#4.Yearly Streams Growth
streams_trend <- spotify_clean %>%
  group_by(released_year) %>%
  summarise(
    total_streams = sum(streams, na.rm = TRUE),
    track_count = n() ) %>%
  arrange(released_year)
view(head(spotify_clean,10))

#5.Identify artists with the highest number of tracks.
most_productive_artists <- spotify_clean%>%
  group_by(artist.s._name) %>%
  summarise(track_count = n()) %>%
  arrange(desc(track_count))
view(head(most_productive_artists))

#6.Track Lifespan Analysis
#Analyze how long after release a track appeared on Spotify playlists or charts
spotify_clean <- spotify_clean %>%
  mutate(release_lifespan = 2024 - released_year) %>%
  arrange(desc(release_lifespan))

long_lifespan_tracks <- spotify_clean%>%
  select(track_name,artist.s._name, release_lifespan)
view(head(long_lifespan_tracks))

#7.Most Streamed Tracks
most_streamed_tracks <- spotify_clean %>%
  arrange(desc(streams)) %>%
  select(track_name, artist.s._name, streams) 
view(head(most_streamed_tracks,10))

#8.Calculate monthly release trends
monthly_releases <- spotify_clean %>%
  group_by(released_month) %>%
  summarise(track_count = n())
View(head(monthly_releases,10))

#9.Playlist Performance
playlist_performance <- spotify_clean %>%
  mutate(playlists_per_stream = in_spotify_playlists / streams) %>%
  arrange(desc(playlists_per_stream)) %>%
  select(track_name, artist.s._name, playlists_per_stream) 
View(head(playlist_performance,10))

#10.Artist Collaboration Impact
# Calculate average streams by artist count
collab_streams <- spotify_clean %>%
  group_by(artist_count) %>%
  summarise(avg_streams = mean(streams, na.rm = TRUE))
View(head(collab_streams,10))

#11.Average Streams by Artist Count
streams_by_artist_count <- spotify_clean%>%
  group_by(artist_count) %>%
  summarise( avg_streams = mean(streams, na.rm = TRUE),
    track_count = n() ) %>%
  arrange(desc(avg_streams))
View(head(streams_by_artist_count,10))

#VISUALIZATIONS
#12.Stream Trends by Year
ggplot(data = streams_trend, aes(x = released_year, y = total_streams)) +
  geom_line(color = "magenta") +
  geom_point() +
  ggtitle("Total Streams by Release Year")

#13.Visualize the distribution of streams using a histogram.
ggplot(spotify_clean, aes(x = streams)) +
  geom_histogram(binwidth = 50000000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Streams",x="Streams",y="Count")

#14.Line plot for monthly releases
ggplot(monthly_releases, aes(x = released_month, y = track_count)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(size = 3, color = "darkblue") +
  labs(title="Tracks Released by Month",x="Month",y="Number of Tracks")

#15.Relationship Between Playlists and Streams
# Scatter plot for playlists vs. streams
ggplot(spotify_clean, aes(x = in_spotify_playlists, y = streams)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "pink") +
  labs(title="Correlation Between Playlists and Streams",x="Number of Playlists",
       y="Streams") +
  theme_minimal()

#16.Bar chart for collaborations
ggplot(collab_streams, aes(x = artist_count, y = avg_streams,fill = artist_count)) +
  geom_bar(stat = "identity") +
  labs(title="Average Streams by Number of Artists",
       x="Number of Artists",y="Average Streams") +
  theme_minimal()

#17.Pie Chart: Proportion of Tracks by Artist Count
artist_count_data <- spotify_clean %>%
  group_by(artist_count) %>%
  summarise(track_count = n())
view(head(artist_count_data,10))

# Summarize the data by artist count
artist_count_data <- artist_count_data %>%
  mutate(percentage = track_count / sum(track_count) * 100)

# Create a pie chart
ggplot(artist_count_data, aes(x = "", y = percentage, fill = factor(artist_count))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage, 0), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of Tracks by Artist Count", fill = "Artist Count") +
  theme_void()

#18.Box Plot: Streams by Artist Count
ggplot(spotify_clean, aes(x = factor(artist_count), y = streams)) +
  geom_boxplot(fill = "yellow", outlier.color = "turquoise", outlier.size = 2) +
  labs(title="Distribution of Streams by Artist Count",x="Number of Artists",
       y="Streams") +
  theme_minimal()

#19.Heatmap: Aggregate average streams by `released_year` and `artist_count`
heatmap_data <- spotify_clean %>%
  group_by(released_year, artist_count) %>%
  summarise(avg_streams = mean(streams, na.rm = TRUE))

ggplot(heatmap_data,aes(x = factor(released_year),y =factor(artist_count),fill=avg_streams)) +
  geom_tile(color="white")+ 
  scale_fill_gradient(low="blue",high="red",na.value ="gray90")+ 
  labs(title="Heatmap of Average Streams", 
       x="Release Year", 
       y="Artist Count", 
       fill = "Avg Streams") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#20.Density plot faceted by artist count
ggplot(spotify_clean, aes(x = streams, fill = factor(artist_count))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Streams by Artist Count",
    x = "Streams",
    y = "Density",
    fill = "Artist Count"
  ) +
  theme_minimal()

