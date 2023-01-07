##packages
install.packages("devtools")
devtools::install_github("danmorse314/hockeyR")
install.packages("hockeyR")
install.packages("gtExtras")


library(tidyverse)
library(janitor)
library(hockeyR)
library(sportyR)
library(gt)
library(gtExtras)
library(ggtext)


##As you can see in the Tableau viz linked below, the Oilers have an outstanding
#   offensive profile as of 1/7/23. They are first in the league in powerplay rate 
#   and they have the sublime Connor McDavid as an evergreen threat.
#   Darnell Nurse is paired with Cody Ceci and McD is playing primarily with
#   Hyman and Draisaitl (forward-wise).
#   So why is a team with the #1 PP and a starting goalie sporting a .914 all
#   the way down at 5th in their own division (see bubble-plot in Tableau viz)?

#   (For reference, Stuart Skinner has a .914 and the Oilers are below VGK, LAK, SEA AND CGY).

https://public.tableau.com/app/profile/john.a.2177/viz/Oilersdeep-dive/Shotplot



##    What I'd like to do here is identify a few important games from the Oilers'
##    schedule; McDavid's two hat-tricks so far and the Oilers' 1 or 2 worst games
##    so far in terms of penalty-killing efficiency. We can use pre-existing code
##    to initiate sportyR and view a 'heatplot' of where these events occurred on the 
##    ice.


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##playbyplay data

season <- load_pbp('2022-23')
##Plotting McDavid's first hat trick vs. VAN Oct. 12.


game <- season %>%
  filter(game_date == "2022-10-12" & home_abbreviation == "EDM")

team_logos <- hockeyR::team_logos_colors %>%
  filter(team_abbr == unique(game$home_abbreviation) | team_abbr == unique(game$away_abbreviation)) %>%
  # add in dummy variables to put logos on the ice
  mutate(x = ifelse(full_team_name == unique(game$home_name), 50, -50),
         y = 0)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.3*a", channel = "alpha")
}

fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")
shots <- game %>% filter(event_type %in% fenwick_events) %>%
  # adding team colors
  left_join(team_logos, by = c("event_team_abbr" = "team_abbr"))


geom_hockey("nhl") +
  ggimage::geom_image(
    data = team_logos,
    aes(x = x, y = y, image = team_logo_espn),
    image_fun = transparent, size = 0.22, asp = 2.35
  ) +
  geom_point(
    data = shots,
    aes(x_fixed, y_fixed),
    size = 6,
    color = shots$team_color1,
    shape = ifelse(shots$event_type == "GOAL", 19, 1)
  ) +
  labs(
    title = glue::glue("{unique(game$away_name)} @ {unique(game$home_name)}"),
    subtitle = glue::glue(
      "{unique(game$game_date)}\n
    {unique(shots$away_abbreviation)} {unique(shots$away_final)} - {unique(shots$home_final)} {unique(shots$home_abbreviation)}"
    ),
    caption = "data from hockeyR | plot made with sportyR"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = .9)
  )


#_______________________________________________________________________________



##  Now that we have that, let's examine his other HT on Oct. 27 @chi.

game <- season %>%
  filter(game_date == "2022-10-27" & home_abbreviation == "CHI")

team_logos <- hockeyR::team_logos_colors %>%
  filter(team_abbr == unique(game$home_abbreviation) | team_abbr == unique(game$away_abbreviation)) %>%
  # add in dummy variables to put logos on the ice
  mutate(x = ifelse(full_team_name == unique(game$home_name), 50, -50),
         y = 0)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.3*a", channel = "alpha")
}

fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")
shots <- game %>% filter(event_type %in% fenwick_events) %>%
  # adding team colors
  left_join(team_logos, by = c("event_team_abbr" = "team_abbr"))


geom_hockey("nhl") +
  ggimage::geom_image(
    data = team_logos,
    aes(x = x, y = y, image = team_logo_espn),
    image_fun = transparent, size = 0.22, asp = 2.35
  ) +
  geom_point(
    data = shots,
    aes(x_fixed, y_fixed),
    size = 6,
    color = shots$team_color1,
    shape = ifelse(shots$event_type == "GOAL", 19, 1)
  ) +
  labs(
    title = glue::glue("{unique(game$away_name)} @ {unique(game$home_name)}"),
    subtitle = glue::glue(
      "{unique(game$game_date)}\n
    {unique(shots$away_abbreviation)} {unique(shots$away_final)} - {unique(shots$home_final)} {unique(shots$home_abbreviation)}"
    ),
    caption = "data from hockeyR | plot made with sportyR"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = .9)
  )



##______________________________________________________________________________



##    Finally, let's look at the Oilers worst game so far in terms of pen-killing.
##    November 7 @washington.

game <- season %>%
  filter(game_date == "2022-11-07" & home_abbreviation == "WSH")

team_logos <- hockeyR::team_logos_colors %>%
  filter(team_abbr == unique(game$home_abbreviation) | team_abbr == unique(game$away_abbreviation)) %>%
  # add in dummy variables to put logos on the ice
  mutate(x = ifelse(full_team_name == unique(game$home_name), 50, -50),
         y = 0)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.3*a", channel = "alpha")
}

fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")
shots <- game %>% filter(event_type %in% fenwick_events) %>%
  # adding team colors
  left_join(team_logos, by = c("event_team_abbr" = "team_abbr"))


geom_hockey("nhl") +
  ggimage::geom_image(
    data = team_logos,
    aes(x = x, y = y, image = team_logo_espn),
    image_fun = transparent, size = 0.22, asp = 2.35
  ) +
  geom_point(
    data = shots,
    aes(x_fixed, y_fixed),
    size = 6,
    color = shots$team_color1,
    shape = ifelse(shots$event_type == "GOAL", 19, 1)
  ) +
  labs(
    title = glue::glue("{unique(game$away_name)} @ {unique(game$home_name)}"),
    subtitle = glue::glue(
      "{unique(game$game_date)}\n
    {unique(shots$away_abbreviation)} {unique(shots$away_final)} - {unique(shots$home_final)} {unique(shots$home_abbreviation)}"
    ),
    caption = "data from hockeyR | plot made with sportyR"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = .9)
  )



##thank you to https://sportyr.sportsdataverse.org/ and hockey-reference for 
##    important stats.