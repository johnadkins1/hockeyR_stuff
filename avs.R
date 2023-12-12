##    Looking at some data to improve the Avalanche on a budget

##    The Avalanche are -- no doubt -- still in win-now mode. At the time of
#     writing this, they are tied-1st in the Central (.592). However, with Landeskog
#     out of the lineup for the time being and an early exit last year, 
#     I wanted to propose a potential move that won't break the Avs' bank and 
#     is attainable for a team who is missing some draft picks. 
#     They selected twice in the first round of 2023, 
#     so you could likely afford to get into this business. Project began on 12.11.23




##    Per the Athletic, the Avalanche ranked 25th in the NHL in faceoffs (47.6%)
##    in the first couple days of October (Baugh, 2023). In the Flyers game, 
#     they surrendered multiple odd-man rushes in the 3rd to ice the game away.
#     So, again, we want to find a player who is (ideally) 1. Good at FO 2. Expiring 
#     3. Not completely inept offensively and 4. Cheap

##    Let us begin importing data. 


teammates <- read.csv("Avs1.csv", header = T)

Comparison <- read.csv("Avs2.csv", header = T)

Player <- read.csv("Avs3.csv", header = T)


#   Entity          Number (mi.)   Percentile   Strength
#   1  Avalanche        81.04         93       PK
#   2 S. Monahan        81.96         90      All
# 


#   I have simply pasted the 'Comparison' sheet for easy display. We have 
#   some numbers of the Avalanche as a team and Sean Monahan individually. I have found a 
#   striking similarity; allow me to explain. The Avalanche -- per NHL EDGE -- 
#   skate a great distance while on the penalty-kill. Monahan, individually, 
#   skates a great distance overall. I feel as though he would not feel out of 
#   place on their team as a result. (Not the sexiest 1to1 comparison, but still).

##  So what kind of year is the former 6th overall pick having?


Monahan <- Player[11, ]


Monahan_select <- Monahan[, c("GP", "Shots.Blocked", "Faceoffs..")]

# (12/28)*82 = ~35.14
Monahan_select$ShotBlockPace <- 35.14

barplot(as.matrix(Monahan_select), beside = TRUE, 
        col = c("violet"), 
        main = "Monahan Misc. Stats", 
        names.arg = c("GP", "Shots.Blocked", "Faceoffs..", "ShotBlockPace"))


##  Here, you can see how a few of Monahan's stats stack up. Please notice 
#   not only his shots-blocked pace of over 35.00 but his positive faceoff rate!



#   Lastly, let's look at the three teammates he's spent the most time with.



selected_players <- subset(teammates, Player %in% c("Tanner Pearson", "Brendan Gallagher", "Josh Anderson"))

# Display the selected data
print(selected_players)

selected_players <- subset(teammates, Player %in% c("Tanner Pearson", "Brendan Gallagher", "Josh Anderson"))

# Bar plot for TOI.With values
barplot(selected_players$TOI.With, names.arg = selected_players$Player, col = "blue",
        main = "TOI.With Values for Selected Players", xlab = "Player", ylab = "TOI.With",
        width = 0.8)


# I provided his frequent teammates because should you want his performance
#   to continue, you would likely need to mold his new line around it.


##  Finally, per NHL EDGE, Monahan is shooting 14.3%. That puts him in the 
#   81st percentile. Then, per capfriendly, his contract is $1.985M (expiring).
#   So let's re-visit the requirements laid out in the intro:


# 1. Good at FO: Check
# 2. Expiring:    Check
# 3. Not completely inept offensively: Check
# 4. Cheap:       Check


# There would likely need to be some cap management in order for the league to
#   approve the deal. Further, I don't think this move would propel Colorado
#   into a championship (be the difference). However, for a team without their
#   2nd, 3rd & 5th for summer 2024 and no 2nd for 2025, perhaps this could be a 
#   value-buy to solidify depth.



#______________________________________________________________________________


##After some basic browsing, it was proven that Monahan’s highest goal output so far was on
##December 4 vs. SEA. Thanks to pre-existing code, we can visualize this game on an 
##ice-hockey sheet model.



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(hockeyR)
library(tidyverse)
library(janitor)
library(sportyR)
library(gt)
library(gtExtras)
library(ggtext)

##playbyplay data
season <- load_pbp('2023-24')

##Plotting Monahan’s two-goal game vs. SEA
game <- season %>%
  filter(game_date == "2023-12-04" & home_abbreviation == "MTL")

team_logos <- hockeyR::team_logos_colors %>%
  filter(team_abbr == unique(game$home_abbreviation) | team_abbr == unique(game$away_abbreviation)) %>%
  # add in dummy variables to put logos on the ice
  mutate(x = ifelse(full_team_name == unique(game$home_name), 50, -50),
         y = 0)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.3*a", channel = "alpha")
}

fenwick_events <- c("MISSED_SHOT", "SHOT", "GOAL")
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
    aes(x = x, y = y),
    size = 6,
    color = shots$team_color1,
    shape = ifelse(shots$event_type == "GOAL", 19, 1)
  ) +
  labs(
    title = glue::glue("{unique(game$away_name)} @ {unique(game$home_name)}"),
    subtitle = glue::glue(
      "{unique(game$game_date)}\n\
      {unique(shots$away_abbreviation)} {unique(shots$away_final)} - {unique(shots$home_final)} {unique(shots$home_abbreviation)}"
    ),
    caption = "data from hockeyR | plot made with sportyR"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = .9)
  )

