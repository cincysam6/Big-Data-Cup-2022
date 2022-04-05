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


USA_CANADA_020822<-dir(path = "TrackingData/2022-02-08 Canada at USA", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
ROC_FINLAND_020822<-dir(path = "TrackingData/2022-02-08 ROC at Finland", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
SWISS_ROC_021222<-dir(path = "TrackingData/2022-02-12 Switzerland at ROC", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
FINLAND_USA_021422<-dir(path = "TrackingData/2022-02-14 Finland at USA", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
SWISS_CANADA_021422<-dir(path = "TrackingData/2022-02-14 Switzerland at Canada", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
SWISS_FINLAND_021622<-dir(path = "TrackingData/2022-02-16 Switzerland at Finland", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)



readDatFile <- function(f) {
  dat.fl <- read.csv(f) # You may have to change read.csv to match your data type
}



USA_CANADA_020822_data <- sapply(USA_CANADA_020822, readDatFile)
ROC_FINLAND_020822_data <- sapply(ROC_FINLAND_020822, readDatFile)
SWISS_ROC_021222_data <- sapply(SWISS_ROC_021222, readDatFile)
FINLAND_USA_021422_data <- sapply(FINLAND_USA_021422, readDatFile)
SWISS_CANADA_021422_data <- sapply(SWISS_CANADA_021422, readDatFile)
SWISS_FINLAND_021622_data <- sapply(SWISS_FINLAND_021622, readDatFile)




names(SWISS_FINLAND_021622_data)

SWISS_FINLAND_021622_pp_data<-data.frame()


for(i in 1:8){
  pp_df<-as.data.frame(SWISS_FINLAND_021622_data[i])
  colnames(pp_df)<-c("frame_id","period","track_id","team_id","team_name","jersey_number","x_ft","y_ft")
  pp_df$pp_number<-i
  SWISS_FINLAND_021622_pp_data<-rbind(SWISS_FINLAND_021622_pp_data,pp_df)
}



pp_info <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/pp_info.csv")
# Load the data directly from github
# To get this URL go to our GitHub repository then click "TrackingData/<game name>/<powerplay name.csv>", click "Raw", then copy the url once the raw data loads


olympic_data%>%head()



## Lets explore the olympic data more. I think this is our event level data
olympic_data <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/pxp_womens_oly_2022_v2.csv")
# Let's clean the team name and then create a game ID, game clock id, score state,home team and away team

olympic_data<-olympic_data%>%separate(team_name,c("competition","team_name"),"-")%>%separate(opp_team_name,c("opp_competition","opp_team_name"),"-")
olympic_data<-olympic_data%>%mutate(game_id=paste0(game_date,"_",season_year,"_",home_team,"_",away_team),
                                    score_state=goals_for-goals_against,
                                    minutes = (clock_seconds/60)|> floor(),
                                    seconds_left = clock_seconds %% 60,
                                    home_team= case_when(venue == "home"  ~ team_name, TRUE ~ opp_team_name),
                                    away_team=case_when(venue == "home"~ opp_team_name,TRUE ~ team_name),
                                    xcoord_new = case_when(venue=="away" ~ (200L -x_coord), TRUE ~ x_coord), ## FUN fact, you need to put L next to integer so R interprets the data type correctly
                                    ycoord_new = case_when(venue=="away" ~ (85L-y_coord), TRUE ~ y_coord))

### RINK IS 0-200 on the X axis and 0-85 on the Y axis

olympic_data%>%view()

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
zone_entries<-olympic_data%>%filter(event=="Zone Entry" & game_id=="8/2/2022_2021_ United States_ Canada")%>%select(x_coord,y_coord)

plot_rink(ggplot(zone_entries)) +
  geom_point(aes(x = x_coord, y = y_coord))


# plotting shots
# need to split these out by teams and group this data into game data

shot<-olympic_data%>%filter(event=="Shot" & game_id=="8/2/2022_2021_ United States_ Canada")%>%select(xcoord_new,ycoord_new,event,event_type,event_detail_1)

plot_rink(ggplot(shot)) +
  geom_point(aes(x = xcoord_new, y = ycoord_new,color=event_detail_1))

plot_rink(ggplot(shot)) +
  geom_point(aes(x = x_coord, y = y_coord,color=event_type))


pp_info%>%head()


## Notes
### Let's see if we can get the data animated today
### Jersey num 100 means that they couldn't tag it. Likely need to figure out how to guess who that is. based on distances

tracking_data = read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/TrackingData/2022-02-14%20Finland%20at%20USA/2022-02-14%20Finland%20at%20USA%20P3%20PP6.csv")


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



