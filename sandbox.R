library(tidyverse)
library(dplyr)
library(ggplot2)
library(gganimate)
library(sportyR)
library(magick)
library(httr)
library(ggforce)
library(cowplot)
library(gifski)

options(gganimate.dev_args = list(width = 10, height = 6, units = 'in', res = 320))



olympic_data <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/pxp_womens_oly_2022_v2.csv")
pp_info <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/pp_info.csv")
# Load the data directly from github
# To get this URL go to our GitHub repository then click "TrackingData/<game name>/<powerplay name.csv>", click "Raw", then copy the url once the raw data loads
tracking_data = read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Finland%20at%20USA/2022-02-14%20Finland%20at%20USA%20P3%20PP6.csv")


olympic_data%>%head()
pp_info%>%head()


## Lets explore the olympic data more. I think this is our event level data

# Let's clean the team name and then create a game ID, game clock id, score state,home team and away team
olym


# quick exploration of the types of data we have in play by play
unique(olympic_data$game_date) # This is 4 days
unique(olympic_data$season_year) # This is just 1 season

situation_type_num<-olympic_data%>%group_by(situation_type)%>%summarise(n=n())
event_type_num<-olympic_data%>%group_by(event_type)%>%summarise(n=n())
event_num<-olympic_data%>%group_by(event)%>%summarise(n=n())
event_event_type_num<-olympic_data%>%group_by(event,event_type)%>%summarise(n=n())


situation_type_num%>%view()
event_type_num%>%view()
event_num%>%view()
event_event_type_num%>%view()



# Let's see if we can plot zone entries for a game

# plotting zone entries
zone_entries<-olympic_data%>%filter(event=="Zone Entry" & game_date=="12/2/2022")%>%select(x_coord,y_coord)

plot_rink(ggplot(zone_entries)) +
  geom_point(aes(x = x_coord, y = y_coord))


# plotting shots
shot<-olympic_data%>%filter(event=="Shot" & game_date=="12/2/2022")%>%select(x_coord,y_coord,event,event_type,event_detail_1)

plot_rink(ggplot(shot)) +
  geom_point(aes(x = x_coord, y = y_coord,color=event_detail_1))

plot_rink(ggplot(shot)) +
  geom_point(aes(x = x_coord, y = y_coord,color=event_type))


## Notes
### Let's see if we can get the data animated today
### Jersey num 100 means that they couldn't tag it. Likely need to figure out how to guess who that is. based on distances



tracking_data_nested = tracking_data %>%
  group_by(frame_id) %>%
  nest()


# We can perform transformations on the nested data using mutate and map
tracking_data_player_count = tracking_data_nested %>%
  mutate(num_players = map(.x = data, .f = ~nrow(.x)))




## ANIMATED PLOTS ##

# Set the specs for the gif we want to create (lower res to make it run quicker)
options(gganimate.dev_args = list(width = 10, height = 6, units = 'in', res = 320))


# Create a gif of this play
p = plot_rink(ggplot(tracking_data)) +
  geom_point(aes(x = x_ft, y = y_ft, fill = team_name), shape = 21, size = 6) +
  geom_text(aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
  scale_colour_manual(values = c("USA" = "white", "Finland" = "navy")) +
  scale_fill_manual(values = c("USA" = "blue", "Finland" = "grey90")) +
  transition_time(frame_id) +
  labs(fill = "Team") +
  guides(colour = "none")


# Get the maximum and minimum frame number (so we know how long the gif should be)
max_frame = max(tracking_data$frame_id)
min_frame = min(tracking_data$frame_id)


# Render the animation
p2 = animate(p, renderer = gifski_renderer(), fps = 30, duration = (max_frame - min_frame)/30 + 1)


# Save as an mp4
anim_save("demo.gif", p2)



