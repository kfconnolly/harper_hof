# install.packages("tidyverse")
# install.packages("Lahman")
# install_github("BIllPetti/baseballr")



library(tidyverse)
library(Lahman)
library(ggplot2)
library(stringr)
library(devtools)
library(baseballr)



#-------------------------------------#
#--------- GAME BY GAME DATA ---------#
#-------------------------------------#




####---- IMPORT DATA & BIND ----####

# clone Chadwick repository to local machine 
# repository at: https://github.com/chadwickbureau/retrosplits


# create list / character vector of all the names of the files in the pre-1947 folder
retro1901.csv.list <- list.files(path="~/Downloads/retrosplits/daybyday_playing_pre1947", 
                             pattern=".csv$", 
                             full.names=TRUE)

# create list / character vector of all the names of the files in the post-1947 folder
# note that this folder goes back to 1938 because some players who hit ~5000PA post-1947 starteed playing before 1947
retro1947.csv.list <- list.files(path="~/Downloads/retrosplits/daybyday_playing_post1947", 
                                 pattern=".csv$", 
                                 full.names=TRUE)



# combine all files pre-1947 files into single dataframe
daybyday_playing_1901 <- bind_rows(map(retro1901.csv.list, read.csv))

# combine all files post-1947 files into single dataframe
daybyday_playing_1947 <- bind_rows(map(retro1947.csv.list, read.csv))







####---- CALC CUMULATIVE TOTALS BY PA & OTHER STATS ----####


# set variables for each stat that needs to be summarized
retro_vars <- c("B_G", "B_PA", "B_AB", "B_R", "B_H", "B_TB", "B_2B", "B_3B", 
                 "B_HR", "B_RBI", "B_BB", "B_SO", "B_SB", "B_HP", "B_SF")



# only select batting stats from daybyday_playing_1947
# replace all NAs with 0 or else calculations will not work
# use 'evt' game.source (includes PA, and not filtering will duplicate data for players with more than one game.source available)
daybyday_playing_1947 <- daybyday_playing_1947 %>%
  filter(game.source == "evt") %>%
  select(game.key:B_G_PR) %>%
  mutate_if(is.integer, replace_na, 0)



# get cumulative stats for each plate appearance
# for now, listed all manually (opportunity to re-write later with more dynamic method)
daybyday_1947_cumsum <- daybyday_playing_1947 %>%
  select(game.key:B_G_PR) %>%
  arrange(person.key, game.date) %>%
  group_by(person.key) %>%
  mutate(Cumulative.B_G = cumsum(B_G),
         Cumulative.B_PA = cumsum(B_PA),
         Cumulative.B_AB = cumsum(B_AB),
         Cumulative.B_R = cumsum(B_R),
         Cumulative.B_H = cumsum(B_H),
         Cumulative.B_TB = cumsum(B_TB),
         Cumulative.B_2B = cumsum(B_2B),
         Cumulative.B_3B = cumsum(B_3B),
         Cumulative.B_HR = cumsum(B_HR),
         Cumulative.B_RBI = cumsum(B_RBI),
         Cumulative.B_BB = cumsum(B_BB),
         Cumulative.B_SO = cumsum(B_SO),
         Cumulative.B_SB = cumsum(B_SB),
         Cumulative.B_HP = cumsum(B_HP),
         Cumulative.B_SF = cumsum(B_SF))







####---- FILTER TO PLAYERS WITH ~5000PAs ----####


# since each player won't have exactly 50000 PAs after a game, can't just filter B_PA == 5000
# will get whatever game is closest to 5000 (within 10 PAs)
# create B_PA_min to calculate the difference between current game PA & 5000 PAs

daybyday_1947_5000PAs <- daybyday_1947_cumsum %>%
  mutate(B_PA_min = abs(5000 - Cumulative.B_PA),
         game_year = as.integer(str_sub(game.key, 4, 7))
  ) %>%
  
  # find season that is closest to 5150 PAs
  # if a hitter has a tie, select only one row
  group_by(person.key) %>%
  slice(which.min(B_PA_min)) %>%
  
  # remove players who didn't have ~5000PAs
  filter(B_PA_min < 10) %>%

  # only select players who reached ~5000PAs in 1947 or after
  filter(game_year >= 1947)

  





####---- ADD HARPER ----####


# create Harper dataframe
daybyday_harper <- daybyday_playing_1947 %>%
  filter(person.key == "harpb003")



# daybyday_1947_5000PAs only includes Harper's 2020 data
# manually add Harper's 2021 numbers to to csv file
# download dataframe
write.csv(daybyday_harper,"~/Downloads/daybyday_harper.csv", row.names = TRUE)



# re-import Harper stats after manual update
daybyday_harper <- read.csv("~/Downloads/daybyday_harper_updated.csv") 
  
  

# select only the game closes to 5000 PA, like for other players
daybyday_harper_5000PAs <-  daybyday_harper %>%
  mutate(B_PA_min = abs(5000 - Cumulative.B_PA)) %>%
  game_year = as.integer(str_sub(game.key, 4, 7)) %>%
  
  # find season that is closest to 5150 PAs
  # if a hitter has a tie, select only one row
  group_by(person.key) %>%
  slice(which.min(B_PA_min)) %>%
  


# add Harper's stats to daybyday_1947_5000PAs
daybyday_1947_5000PAs <- rbind(daybyday_1947_5000PAs, daybyday_harper_5000PAs)







####---- GET PLAYER INFO ----####


# get_retrosheet_data() acquires & parses all event & roster files for selected years
# set local file save location in path_to_directory
get_retrosheet_data(path_to_directory = "~/Downloads/retrosheet",
                    years_to_acquire = c(1947,2020),
                    sequence_years = T)


# only need roster data
# manually remove "all-" files from local "~/retrosheet/download.folder/unzipped" directory

# create list / character vector of all the names of the files in the unzipped folder
roster.csv.list <- list.files(path="~/Downloads/retrosheet/download.folder/unzipped", 
                                 pattern=".csv$", 
                                 full.names=TRUE)



# combine all roster files into single dataframe
roster_1947 <- bind_rows(map(roster.csv.list, read.csv))



# join name & position info with PA dataframe
# select only 1 row per player
daybyday_1947_5000PAs <- 
  merge(x = daybyday_1947_5000PAs, 
        y = roster_1947[ , c("player_id", "year", "last_name", 
                             "first_name", "x7")], 
        by.x = c("person.key", "game_year"), 
        by.y = c("player_id", "year"),
        all.x=TRUE) %>%
  group_by(person.key) %>%
  slice(1)



# update Harper's roster info (missing because roster_1947 only goes up to 2020)
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "harpb003", 
                      "last_name"] <- "Harper"
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "harpb003", 
                      "first_name"] <- "Bryce"
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "harpb003", 
                      "x7"] <- "OF"



# clean up names where there are duplicate last_name + first_name combos
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "aloms101", 
                      "last_name"] <- "Alomar Sr."
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "aloms001", 
                      "last_name"] <- "Alomar Jr."
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "gonza001", 
                      "first_name"] <- "Alex S."
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "gonza002", 
                      "first_name"] <- "Alex L."
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "grifk001", 
                      "last_name"] <- "Griffey Sr."
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "grifk002", 
                      "last_name"] <- "Griffey Jr."
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "thomf103", 
                      "first_name"] <- "Frank J."
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "thomf001", 
                      "first_name"] <- "Frank E."
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "uptob001", 
                      "first_name"] <- "B.J."
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "youne001", 
                      "last_name"] <- "Young Sr."



# concatenate first_name + last_name fields together
daybyday_1947_5000PAs <- daybyday_1947_5000PAs %>%
  unite("full_name", 
        first_name:last_name, 
        sep = " ", remove = FALSE)



# get debut year & last year of player's career in people dataframe
# then join to daybyday_1947_5000PAs
people <- People %>%
  group_by(playerID) %>%
  mutate(final_year = as.integer(str_sub(finalGame, 1, 4)),
         debut_year = as.integer(str_sub(debut, 1, 4))
  )


daybyday_1947_5000PAs <- 
  merge(x = daybyday_1947_5000PAs,
        y = people[ ,c("retroID", "debut_year", "final_year")],
        by.x = "person.key",
        by.y = "retroID",
        all.x = TRUE)



#save full name values
# names_5000_all <- daybyday_1947_5000PAs$full_name







####---- ADD AGE ----####


# concatenate first_name + last_name fields together in People dataframe (from Lahman)
people <- people %>%
  unite("full_name", 
        nameFirst:nameLast, 
        sep = " ", remove = FALSE)



# join people dataframe to daybyday_1947_5000PAs
# retroID (not playerID) matches up to person.key 
daybyday_1947_5000PAs <-  
  merge(x = daybyday_1947_5000PAs, 
        y = people[, c("retroID", "birthYear", "birthMonth")],
        by.x = "person.key",
        by.y = "retroID",
        all.x = TRUE)


# calc age at 5000th PA
# using age code from Analyzing Baseball Data with R (MLB defines age as age on June 30th of season)
daybyday_1947_5000PAs <- daybyday_1947_5000PAs %>%
  mutate(birthyear = ifelse(birthMonth >= 7,
                            birthYear + 1, birthYear),
         Age = game_year - birthyear
  )







####---- JOIN HOFERS ----####


# select hof inductees from HallOfFame dataframe (from Lahman)
# only interested in players elected by BBWAA
# also include Clemente & Gehrig via "Special Election"

inductees <- HallOfFame %>%
  group_by(playerID) %>%
  filter(votedBy %in% c("BBWAA", "Special Election") & category == "Player") %>%
  summarize(yearsOnBallot = n(), 
            inducted = sum(inducted == "Y"), 
            best = max(votes/ballots)) %>%
  arrange(desc(best))

# add names by merging with People dataframe (Lahman)
inductees <-
  merge(x = inductees, 
        y = people[ , c("playerID", "nameFirst", "nameLast", "full_name")], 
        by = "playerID", 
        all.x = TRUE) 



# merge hof inductees with daybyday_1947_5150PAs to get inducted status
daybyday_1947_5000PAs <- 
  merge(x = daybyday_1947_5000PAs, 
        y = inductees[ , c("full_name", "inducted")], 
        by = "full_name",
        all.x = TRUE)



# manually update Griffey's inducted status 
# (listed with "Jr." in daybyday_1947_5000PAs, without "Jr." in inductees)
daybyday_1947_5000PAs[daybyday_1947_5000PAs$person.key == "grifk002", 
                      "inducted"] <- 1



# exclude current players (2021) — list does not include Harper
# local file with names from MLB.com
current_players <- read.csv("~/Downloads/current_mlb_players.csv")

daybyday_1947_5000PAs <-
  anti_join(daybyday_1947_5000PAs, current_players, by="full_name")



# save name values
# names_5000_noncurrent <- daybyday_1947_5000PAs$full_name







####---- CALC BASIC RATE STATS ----####


daybyday_1947_5000PAs <- daybyday_1947_5000PAs %>%
  mutate(AVG = Cumulative.B_H / Cumulative.B_AB,
         SLG = (Cumulative.B_H - Cumulative.B_2B - Cumulative.B_3B - Cumulative.B_HR + 
                  2 * Cumulative.B_2B + 3 * Cumulative.B_3B + 4 * Cumulative.B_HR) / Cumulative.B_AB,
         OBP = (Cumulative.B_H + Cumulative.B_BB + Cumulative.B_HP) / 
                (Cumulative.B_AB + Cumulative.B_BB + Cumulative.B_HP + Cumulative.B_SF),
         OPS = SLG + OBP,
         BB_pct = Cumulative.B_BB / Cumulative.B_PA,
         K_pct = Cumulative.B_SO / Cumulative.B_PA,
         ISO = SLG - AVG)







####---- ADD WAR & wRC+ ----####


# read WAR data into dataframe
# will include data up to yesterday
war <- read.csv("https://raw.githubusercontent.com/NeilPaine538/MLB-WAR-data-historical/master/mlb-war-data-historical.csv")



# calculate cumulative WAR & cumulative average wRC+
war <- war %>%
  arrange(player_ID, year_ID) %>%
  group_by(player_ID) %>%
  mutate_if(is.integer, replace_na, 0) %>%
  mutate(Career.WAR = cumsum(WAR),
         Career.wRCp_avg = cumsum(wRCp) / seq_along(wRCp)
  )



# updated some names in war dataframe to match those in daybyday_1947_5000PAs
war[war$player_ID == "aaronha01", 
    "name_common"] <- "Hank Aaron"
war[war$player_ID == "alomasa01", 
    "name_common"] <- "Sandy Alomar Sr."
war[war$player_ID == "dejesiv01", 
    "name_common"] <- "Ivan De Jesus"
war[war$player_ID == "gonzaal01", 
    "name_common"] <- "Alex S. Gonzalez"
war[war$player_ID == "gonzaal02", 
    "name_common"] <- "Alex L. Gonzalez"
war[war$player_ID == "griffke01", 
    "name_common"] <- "Ken Griffey Sr."
war[war$player_ID == "ramiral03", 
    "name_common"] <- "Alexei Ramirez"
war[war$player_ID == "ripkeca01", 
    "name_common"] <- "Cal Ripken"
war[war$player_ID == "thomafr03", 
    "name_common"] <- "Frank J. Thomas"
war[war$player_ID == "thomafr04", 
    "name_common"] <- "Frank E. Thomas"



# merge war with daybyday_1947_5000PAs
calcs_1947_5000PAs <- 
  merge(x = daybyday_1947_5000PAs, 
        y = war[, c("name_common", "age", "year_ID", "bWAR", "WAR", 
                    "WAR162", "Career.WAR", "wRCp", "Career.wRCp_avg")],
        by.x = c("full_name", "game_year"),
        by.y = c("name_common", "year_ID"),
        all.x = TRUE)



# summarize metrics that get fragmented for individual players (usually ones on multiple teams in one season)
calcs_1947_5000PAs <- calcs_1947_5000PAs %>%
  group_by(full_name) %>%
  summarize(bWAR = sum(bWAR, na.rm = TRUE),
            WAR = sum(WAR, na.rm = TRUE),
            WAR162 = sum(WAR162, na.rm = TRUE),
            Career.WAR = sum(Career.WAR, na.rm = TRUE),
            wRCp = mean(wRCp, na.rm = TRUE),
            Career.wRCp_avg = mean(Career.wRCp_avg, na.rm = TRUE)
  )



# join calcs to daybyday_1947_5150PAs dataframe
daybyday_1947_5000PAs <- daybyday_1947_5000PAs %>%
  left_join(calcs_1947_5000PAs, by = "full_name")



# save to csv to share with group / add to Google Sheeet
write.csv(daybyday_1947_5000PAs,"~/Downloads/daybyday_1947_5000PAs.csv", row.names = TRUE)







####---- HOW MANY ~5000PAs REACH HOF? ----####


HOFeligible_5150PAs <- daybyday_1947_5150PAs %>%
  filter(final_year < 2014)
  

HOFeligible_5150PAs %>% filter(is.na(inducted))

HOFeligible_5150PAs %>% filter(inducted == 0)

HOFeligible_5150PAs %>% filter(inducted == 1)
