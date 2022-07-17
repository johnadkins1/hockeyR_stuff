##packages
install.packages("devtools")
devtools::install_github("danmorse314/hockeyR")
install.packages("hockeyR")
install.packages("gtExtras")

##With JT Miller in free agency rumors, let's run through some of his numbers/stats.




library(tidyverse)
library(janitor)
library(hockeyR)
library(sportyR)
library(gt)
library(gtExtras)
library(ggtext)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##read in the data from nhl.com/stats.

canucks22 <- read.csv(file = "vancouver.csv", header = T, sep = ",")


##read in the data from Natural stat Trick (NST).

nstmiller <- read.csv(file = "nstdata.csv", header = T, sep = ",")


##Which skaters did JT Miller spend the most time with in 2021-22?

head(nstmiller)
head(nstmiller$TOI.With)
head(nstmiller$With)
##Answer: Boeser (552:34), Hughes (489:27) & Pearson (454:21).

##______________________________________________________________________________


install.packages("plotly")
library(plotly)


##place the nst data into a data frame.
name <- c("Boeser", "Hughes", "Pearson")
time <- c(552.34, 489.27, 454.21)
df_players <- data.frame(name, time)
df_players


##plot it.

v <- ggplot(data = df_players, aes(x = name, y = time)) +
  geom_bar(color = "blue", stat = "identity")

ggplotly(v)


##______________________________________________________________________________


##read in Miller's data for his entire #Canucks career [RS] (2019-20 to present)


datacanucks <- read.csv(file = "canucks1922.csv", header = T, sep = ",")

##Clean the data up a bit.

datacanucks$FOW. <- NULL
datacanucks$S.C <- NULL


datacanucks <- datacanucks %>%
  mutate(evpGM = EVP/GP)


##calculate the GP avg for Boeser, Miller and Hughes since 2019-20.

datacanucks <- datacanucks %>%
  mutate(game = GP/3)


datacanucks <- datacanucks %>%
  mutate(pred2023 = evpGM*game)

datacanucks

dfpredict <- datacanucks %>%
  select(Player, pred2023) %>%
filter(pred2023 >1)


ggplot(data = dfpredict, aes(x = dfpredict$pred2023, y = dfpredict$Player)) +
  geom_bar(color = "blue", stat = "identity")



##______________________________________________________________________________


##Let's create a scatterplot that joins Shots and shooting % -- min. ~100 shots.


dfshots <- datacanucks %>%
  select(Player, S, S.) %>%
  filter(S >= 98)


dfshots$spx <- c(15.5, 16.6, 14.3, 12.3, 10.4, 9.4, 9.2, 11.5, 11.9, 10.6, 12.1, 12.2, 13.3)

dfshots$S. <- NULL
dfshots


##now plot!

update_geom_defaults("point",list(size=5))

ggplot(data = dfshots, mapping = aes(x = S, y = spx)) +
         geom_point(mapping = NULL, data = dfshots, stat = "identity", na.rm = FALSE, col = "red")+
  geom_smooth() +
  ggtitle(label = "SOG plotted against shooting %, 2019-20") 
  

##______________________________________________________________________________


##After some basic browsing, it was proven that JT Miller's highest goal output was on
##January 27 @wpg. Thanks to pre-existing code, we can visualize this game on an 
##ice-hockey sheet model.

       

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##playbyplay data

season <- load_pbp('2021-22')
##Plotting JT Miller's hat-trick, 1.27.22 @wpg


game <- season %>%
  filter(game_date == "2022-01-27" & home_abbreviation == "WPG")

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




       

       
       
       
       
       