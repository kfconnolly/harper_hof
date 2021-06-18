####--- SIMILARITY SCORES ---####


# calculate players with close similiarity scores to Harper at his current age & PA milestone (5000)

# concept (developed by Bill James) is to compare players on basis of career stats
#### being more point-in-time in this use case since Harper's career is still in progress

# uses method from Analyzing Baseball Data with R
##### github --> https://github.com/beanumber/baseball_R/blob/master/chapter_code/trajectories.R

# use daybyday_1947_5000PAs data frame from 5000_PAs.r file in this repo




daybyday_1947_5000PAs_sim <- daybyday_1947_5000PAs %>%
  # get players that were close to Harper's age at time of 5000 PA
  filter(Age >= 25, Age <= 30) %>%
  # update column names for readability
  rename(G = Cumulative.B_G,
         AB = Cumulative.B_AB,
         R = Cumulative.B_R,
         H = Cumulative.B_H,
         X2B = Cumulative.B_2B,
         X3B = Cumulative.B_3B,
         HR = Cumulative.B_HR,
         RBI = Cumulative.B_RBI,
         BB = Cumulative.B_BB,
         SO = Cumulative.B_SO,
         SB = Cumulative.B_SB,
         HBP = Cumulative.B_HP,
         SF = Cumulative.B_SF)
  
  

# career stats calcs
vars_cumulative <- c("G", "AB", "R", "H", "X2B", "X3B", 
                     "HR", "RBI", "BB", "SO", "SB")

totals.5000 <- daybyday_1947_5000PAs_sim %>%
  group_by(person.key) %>%
  summarize_at(vars_cumulative, sum, na.rm = TRUE)



# calc each player's career batting average & career slugging percentage
totals.5000 <- totals.5000 %>%
  mutate(AVG = H / AB,
         SLG = (H - X2B - X3B - HR + 2 * X2B + 
                  3 * X3B + 4 * HR) / AB
  )



# merge career stats df with fielding positions 
# each fielding position has an associated value for final similarity score calc
totals.5000 <- totals.5000 %>%
  inner_join(roster_1947, 
             by = c("person.key" = "player_id")) %>%
  mutate(Value.POS = case_when(
    x7 == "C" ~ 240,
    x7 == "SS" ~ 168,
    x7 == "2B" ~132,
    x7 == "3B" ~ 84,
    x7 == "OF" ~ 48,
    x7 == "1B" ~ 12,
    TRUE ~ 0))



# function to find players most similar to a given player
similar <- function(p, number = 10) {
  totals.5000 %>% filter(person.key == p) -> P
  totals.5000 %>%
    mutate(sim_score = 1000 -
             floor(abs(G - P$G) / 20) -
             floor(abs(AB - P$AB) / 75) -
             floor(abs(R - P$R) / 10) - 
             floor(abs(H - P$H) / 15) -
             floor(abs(X2B - P$X2B) / 5) -
             floor(abs(X3B - P$X3B) / 4) -
             floor(abs(HR - P$HR) / 2) -
             floor(abs(RBI - P$RBI) / 10) -
             floor(abs(BB - P$BB) / 25) -
             floor(abs(SO - P$SO) / 150) -
             floor(abs(SB - P$SB) / 20) -
             floor(abs(AVG - P$AVG) / 0.001) -
             floor(abs(SLG - P$SLG) / 0.002) -
             abs(Value.POS - P$Value.POS)) %>%
    arrange(desc(sim_score)) %>%
    head(number)
}



# define Harper's id since we will be comparing other players to him
harper_id <- daybyday_1947_5000PAs %>% 
  filter(person.key == "harpb003") %>%
  pull(person.key)



# output data frame of player statistics orders in decreasing order by simiilarity scores
harper_similar <- similar(harper_id,50) %>%
  select(-year, -X, -team) %>%
  distinct()
