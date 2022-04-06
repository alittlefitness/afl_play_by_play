library(tidyverse)
library(fitzRoy)

location_data <- tibble(season = NULL)

for(i in c("01", "02", "03", "04", "05",
           "06", "07", "08", "09", "10",
           "11", "12", "13", "14", "15",
           "16", "17", "18", "19", "20",
           "21", "22", "23", "24", "25",
           "26", "27")) {
  
  df <- read.csv(paste0("https://raw.githubusercontent.com/DataByJosh/AFL-Data/main/AFLM_Match_Chains/csvs/match_chains_2022_", i, ".csv"), stringsAsFactors = F)
  location_data <- bind_rows(location_data, df)
  
}

player_stats <- fetch_player_stats(season = 2022)

chain_starts <- player_stats %>%
  select(roundNumber = round.name, home = home.team.club.name, away = away.team.club.name, team = team.name, 
         clearances = clearances.totalClearances, intercepts, kickins = extendedStats.kickins) %>%
  group_by(roundNumber, home, away, team) %>%
  summarise(clearances = sum(clearances, na.rm = T),
            intercepts = sum(intercepts, na.rm = T),
            kickins = sum(kickins, na.rm = T)) %>%
  na.omit() %>%
  mutate(roundNumber = as.numeric(str_remove(roundNumber, "Round "))) %>%
  pivot_longer(cols = c("clearances", "intercepts", "kickins"), names_to = "source", values_to = "chains")

scores <- location_data %>%
  select(roundNumber, home = homeTeam.teamName, away = awayTeam.teamName, chain_number, teamName = team.teamName, initialState, result = finalState, description) %>%
  filter(result %in% c("goal", "behind", "rushed", "rushedOpp")) %>%
  group_by(roundNumber, home, away, chain_number) %>%
  mutate(lastDisposal = if_else(!initialState %in% c("kickIn", "possGain") & description %in% c("Ground Kick", "Handball", "Kick"), teamName, NULL)) %>%
  fill(lastDisposal) %>%
  mutate(team = if_else(initialState %in% c("kickIn", "possGain"), teamName[which.min(row_number())], lastDisposal[which.max(row_number())])) %>%
  fill(team, .direction = "downup") %>%
  filter(row_number() == max(row_number())) %>%
  mutate(source = case_when(initialState %in% c("ballUp", "throwIn", "centreBounce") ~ "clearances",
                           initialState == "possGain" ~ "intercepts",
                           initialState == "kickIn" ~"kickins"),
         team = case_when(result == "rushedOpp" & team == home ~ away,
                          result == "rushedOpp" & team == away ~ home,
                          TRUE ~ team),
         result = case_when(result %in% c("rushed", "rushedOpp") ~ "behind",
                            TRUE ~ result)) %>%
  group_by(roundNumber, home, away, team, source, result) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = result, values_from = n)

score_sources <- left_join(chain_starts, scores) %>%
  select(roundNumber:chains, goal, behind)
