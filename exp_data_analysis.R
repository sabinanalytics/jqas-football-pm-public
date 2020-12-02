#exploratory data analysis

library(Matrix)
library(espnanalytics)
library(stringr)
library(tidyselect)
library(xtable)

options(tibble.width = Inf)
options("scipen"=100, "digits"=8)
Sys.setenv("TZ" = "America/New_York")
Sys.setenv("ORA_SDTZ" = "America/New_York")
#Connect to DB
SDWconnection <- OpenOracleConnection("SDWPRD")
SDRconnection <- OpenOracleConnection("SDRPRD")
SDWReadOnly   <- dbConnect(dbDriver("Oracle"), "readonly", "gamerecap", dbname="SDWPRD")

# new_run <- FALSE
# prior_plays <- 25
# qualifying_plays <- 15 #(per team game)
# qualifying_plays_season <- 75 ## qualifying plays for a whole season regardless of team games (for run plays and pass plays)
# plays_game_constant <- 35 # constant to take per play stats to possession neutral per game stats (for each of runs and passes plays)
# pm_replacement = -0.0773 ## replacement level plus-minus on a play 
# pm_quantile <- 0.20 ## quantile for a replacement player (about 20th)


# NFL Data ----------------------------------------------------------------


### nfl position dictionary
nfl_pos_dict <- read_csv("nfl/data/updated_pos.csv")
positions_translate <- nfl_pos_dict %>% select(Position, Group)
unique_pos <- positions_translate$Group %>% unique()


nfl_seasons <- QueryOracleTable(SDWconnection, query_text = "
                                select season_id,  substr(description, 1, 4) as season_year, substr(description, 5, 7) as season_type, 
                                start_date, end_date from SDROLTP.SEASON t
                                where league_id = 324
                                and (description LIKE ('%REG') OR description like ('%PLY') )
                                order by end_date desc",lowercase_col = TRUE) %>% as_tibble %>% 
  filter(season_year >= 2007)

## NFL position play counts
position_play_counts_fromQuery <- QueryOracleTable(SDWconnection, query_text =  paste0(
  "select substr(season.description,1,4) as year, tn.team_id, tn.team_name, r.person_id, r.first_name, r.last_name,
  pp.position,
  count(pp.position) as plays
  from presch.pff_participation pp
  join sdroltp.schedule s on s.eventid = pp.eventid
  join sdroltp.season season on season.season_id = s.season_id
  join sdroltp.person r on r.person_id = pp.sdr_id
  join presch.pff_positions o on o.position = pp.position
  join sdroltp.team_name@presch_sdr_link tn on tn.team_id = pp.team+0 and tn.requestor_id = 0
  where s.season_id IN (",  paste(nfl_seasons$season_id, collapse = ','), ") ",  
  "group by substr(season.description,1,4), tn.team_id, tn.team_name, r.person_id, r.first_name, r.last_name, pp.position
  order by count(pp.position) desc, max(pp.eventid) desc"),
  lowercase_col = TRUE) %>% as_tibble


position_play_counts_fromQuery <- position_play_counts_fromQuery %>% 
  left_join(positions_translate, by = c("position" = "Position") ) %>% 
  rename(position_group = Group)

nfl_events <- QueryOracleTable(SDRconnection, query_text = "select substr(season.description, 1,4) as season, substr(season.description, 5, 7) as season_type,
conf_game_ind as conf_game, div_game_ind as div_game, week_no as week,
                           event.start_dt as game_date_time, event.event_id, event.schedule_id, 
                           tm.playerteam_id as tm_id, tm_pregame_rating, tm_days_since_prev_game, 
                           (case when event.neutral_site_ind = 'Y' then 'Neutral' when tm.home_visitor_ind  = 'H' then 'Home' else 'Road' end) as tm_site, 
                           opp.playerteam_id as opp_id, opp_pregame_rating, opp_days_since_prev_game,  
                           tm_chance_win, tm_chance_lose, tm_chance_tie
                           
                           from SDROLTP.EVENT event
                           left join sdroltp.season season on season.season_id = event.season_id
                           left join sdroltp.competitor tm on tm.event_id = event.event_id
                           left join sdroltp.competitor opp on opp.event_id = event.event_id
                           left join presch.NFL_GAME_PREDICTIONS game_pred on game_pred.event_id = event.event_id and game_pred.tm_id = tm.playerteam_id
                           
                           where event.league_id = 3369
                           and tm.playerteam_id != opp.playerteam_id",lowercase_col = TRUE  ) %>% as_tibble


participation_dat <- QueryOracleTable(SDWconnection, query_text = paste0(
  "select s.schedule_id, s.eventid as event_id, s.week_no, pp.playcnt, tn.team_id, tn.team_name, r.person_id, r.first_name, r.last_name,
                                            pp.position,
                                            o.off_def,
                                            ep.epa,
                                            ep.posstm_wpa,
                                            (case when collapse.play_type = 'LOS' then collapse.play_subtype else collapse.play_type end) as play_type
                                            from presch.pff_participation pp
                                            join sdroltp.schedule s on s.eventid = pp.eventid
                                            join sdroltp.person r on r.person_id = pp.sdr_id
                                            join presch.pff_positions o on o.position = pp.position
                                            join sdroltp.team_name@presch_sdr_link tn on tn.team_id = pp.team+0 and tn.requestor_id = 0
                                            join presch.nfl_play_ep_wp@presch_sdr_link ep on ep.schedule_id = s.schedule_id and ep.unqply_group = pp.playcnt
                                            join presch.nfl_collapse@presch_sdr_link collapse on collapse.schedule_id = ep.schedule_id and collapse.unqply_group = ep.unqply_group
                                            where s.season_id IN (",  paste(nfl_seasons$season_id, collapse = ','), ") ",
  "order by s.schedule_id asc, pp.playcnt asc"), 
  lowercase_col = TRUE
) %>%   as_tibble %>% 
  mutate(offense = if_else(tolower(off_def) == "off", 1, -1)) %>% 
  filter(!(play_type %in% c("ExtraPoint", "FieldGoal", "Punt", "TwoPoint", "Penalty") )) %>% #omit special teams and penalty designated plays
  mutate(play_type = if_else(play_type %in% c("Aborted", "Rush", "Penalty-Rush"), "Run", "Pass") ) %>% 
  left_join(nfl_events %>% 
              select(schedule_id, event_id, season, game_date_time) %>% 
              rename(sdr_event_id = event_id) %>% 
              unique(), 
            by = c("schedule_id"))


#### Broad Position Categories
participation_dat <- participation_dat %>%
  left_join(positions_translate, by = c("position" = "Position") ) %>% 
  rename(position_group = Group)

### position priors
# position_priors <- participation_dat %>% 
#   left_join(select(players_by_season, person_id, team_id, position_group) %>% 
#               rename(main_position = position_group), by = c("person_id", "team_id")) %>% 
#   group_by(main_position, position_group) %>% 
#   summarize(count = n()) %>% ## how many plays at a certain position based on roster position
#   group_by(main_position) %>% 
#   mutate(fraction = count / sum(count),
#          prior = fraction*prior_plays) %>% ungrou

### turnover epa replacements
#interception and rush fumble epa adjustments
nfl_interception_epa_adj <- readRDS(paste0("nfl/data/interception_epa_adj_2019.rds"))
nfl_rush_fumble_epa_adj <- readRDS(paste0("nfl/data/rush_fumble_epa_adj_2019.rds"))
nfl_pass_fumble_epa_adj <- readRDS(paste0("nfl/data/pass_fumble_epa_adj_2019.rds"))



participation_dat_2019 <- participation_dat %>% 
  filter(season == 2019) %>% 
  left_join(nfl_interception_epa_adj %>% select(espnEventId, playId, contains("epa")), by = c("event_id" = "espnEventId", "playcnt" = "playId") ) %>% 
  left_join(nfl_rush_fumble_epa_adj %>% select(espnEventId, playId, contains("epa")), by = c("event_id" = "espnEventId", "playcnt" = "playId") ) %>% 
  left_join(nfl_pass_fumble_epa_adj %>% select(espnEventId, playId, contains("epa")), by = c("event_id" = "espnEventId", "playcnt" = "playId") ) %>%
  mutate(adj_epa = case_when(!is.na(int_epa_reg) ~ int_epa_reg, 
                         !is.na(fumble_rush_epa_reg) ~ fumble_rush_epa_reg, 
                         !is.na(fumble_pass_epa_reg) ~ fumble_pass_epa_reg, 
                         TRUE ~ epa )) %>% 
  select(-int_epa_reg, -fumble_rush_epa_reg, -fumble_pass_epa_reg)
#pct of plays using the adj epa
(participation_dat_2019 %>% filter(adj_epa != epa) %>% nrow()) / nrow(participation_dat_2019)


participation_dat_2019$epa %>% mean()
participation_dat_2019$adj_epa %>% mean()

#max sigma of 2019 epa plays
max(abs(scale(participation_dat_2019$epa)))

pnorm(-3)*2
mean(abs(scale(participation_dat_2019$epa)) > 3)
mean(abs(scale(participation_dat_2019$adj_epa)) > 3)

pnorm(-5)*2
mean(abs(scale(participation_dat_2019$epa)) > 5)
mean(abs(scale(participation_dat_2019$adj_epa)) > 5)

##highest average epa/play
extreme_avg_epa_nfl <- participation_dat_2019 %>% 
  group_by(person_id, team_id, team_name, first_name, last_name) %>% 
  summarize(avg_epa = mean(epa), 
            avg_adj_epa = mean(adj_epa), 
            plays = n()) %>% 
  ungroup() %>% 
  mutate(regressed_avg_epa = (avg_epa*plays + (50*weighted.mean(avg_epa, w = plays))) /(plays + 50) ) %>% 
  arrange(desc(avg_epa))

extreme_avg_epa_nfl %>% filter(person_id %in% filter(participation_dat_2019, event_id == 4251935, playcnt == 2657)$person_id)

participation_dat %>% 
  ggplot(aes(x = epa)) + 
  geom_histogram() + 
  theme_bw()
set.seed(5)
nfl_epa_sample <- participation_dat_2019$epa[sample(nrow(participation_dat_2019), 5000)]
shapiro.test(nfl_epa_sample)#W = 0.906187, p-value < 0.000000000000000222
nfl_turn_adj_epa_sample <- participation_dat_2019$adj_epa[sample(nrow(participation_dat_2019), 5000)]
shapiro.test(nfl_turn_adj_epa_sample)#W = 0.906187, p-value < 0.000000000000000222
# College Data ------------------------------------------------------------





### looking at how often players play together, 
# take most snaps player at each position, 
# then look at matrix plot of how often they were on the field with the most at the next position, on average
participation_dat <- participation_dat %>% 
  mutate(position_group = fct_relevel(position_group, "QB", "RB", "SWR", "WR", "TE", "OT", "OG", "C", "DT", "DE", "ILB", "OLB", "SCB" ,"CB", "S"))

pos_plays_by_season <- participation_dat %>% 
  group_by(season, team_id, team_name, off_def, position_group, person_id, first_name, last_name) %>% 
  summarize(pos_plays = n()) %>% 
  ungroup()

tm_plays_by_season <- participation_dat %>% 
  group_by(season, team_id, team_name, off_def, event_id, playcnt) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(season, team_id, team_name, off_def) %>% 
  group_by(season, team_id, team_name, off_def) %>% 
  summarize(tm_plays = n()) %>% 
  ungroup()

plays_by_season <- participation_dat %>% 
  group_by(season, team_id, team_name, person_id, first_name, last_name) %>% 
  summarize(plays = n()) %>% 
  ungroup()

### top player at each position for each team each season
tm_top_plyr_by_pos <- pos_plays_by_season %>% 
  group_by(season, person_id) %>% 
  mutate(tot_plays = sum(pos_plays)) %>% 
  group_by(season, team_id, team_name, off_def, position_group) %>% 
  filter(pos_plays == max(pos_plays)) %>% 
  filter(tot_plays == max(tot_plays)) %>% #for tiebreaking two players that played both max plays at position
  slice(1) %>% #take first entry if still tied
  left_join(tm_plays_by_season %>% 
              select(season, team_id, off_def, tm_plays), 
            by = c("season", "team_id" , "off_def")) %>% 
  ungroup() %>% 
  mutate(plyr_play_pct = tot_plays / tm_plays)

tm_by_seas <- tm_top_plyr_by_pos %>% 
  select(season,team_id, team_name) %>% 
  unique()
n_tm_by_seas <- nrow(tm_by_seas)

top_plyr_pos_participation_dat <- participation_dat %>% 
  select(event_id, playcnt, epa, first_name, last_name, off_def, team_name, 
         season, team_id, position_group, person_id) %>% 
  inner_join(tm_top_plyr_by_pos %>% 
               select(season, team_id, position_group, person_id), 
             by = c("season", "team_id", "position_group", "person_id")
             ) 
  
team_season_pos_matrix <- NULL
pos_vec <- c("QB", "RB", "SWR", "WR", "TE", "OT", "OG", "C", "DT", "DE", "ILB", "OLB", "SCB", "CB", "S")
for(j in 1:n_tm_by_seas){
  temp_tm_seas <- tm_by_seas %>% slice(j)
  temp_top_plyr_by_pos <- tm_top_plyr_by_pos %>% 
    filter(season %in% temp_tm_seas$season,
           team_id %in% temp_tm_seas$team_id)
  
  temp <- top_plyr_pos_participation_dat %>% 
    inner_join(temp_top_plyr_by_pos %>% 
                 select(season, team_id, person_id, position_group), 
               by = c("season", "team_id", "person_id", "position_group")) %>% 
    left_join(plays_by_season %>% 
                select(season, team_id, person_id, plays) %>% 
                rename(seas_plays = plays), 
              by = c("season", "team_id", "person_id")) %>% 
    mutate(ind = 1)
  
  team_season_pos_matrix <- temp %>% 
    pivot_wider(id_cols = c("event_id", "playcnt", "season", "team_id", "team_name"), 
                names_from = "position_group", 
                values_from = "ind", 
                values_fill = list(ind = 0)) %>% 
    right_join(temp, by = c("event_id", "playcnt", "season", "team_id", "team_name")) %>% 
    group_by(season, team_id, team_name, position_group) %>% 
    summarize_at(vars(any_of(pos_vec)),
                 ~sum(.)/max(seas_plays)) %>% 
    ungroup() %>% 
    select(season, team_name, team_id, position_group,
           any_of(pos_vec)) %>% 
    bind_rows(team_season_pos_matrix, . )
  
  cat("j = ", j, " out of ", n_tm_by_seas, "\r")
}

## calculate and output avg, min and max


## max on-field pct by position
max_pos_matrix <- team_season_pos_matrix %>% 
  group_by(position_group) %>% 
  summarize_at(vars(any_of(pos_vec)), ~max(., na.rm = T))

## mean on-field pct by position
mean_pos_matrix <- team_season_pos_matrix %>% 
  group_by(position_group) %>% 
  summarize_at(vars(any_of(pos_vec)), ~mean(., na.rm = T))

# insert diagonal to have the pct of times a position is on the field
mean_pos_pct_on_field <- tm_top_plyr_by_pos %>% 
  group_by(position_group) %>% 
  summarize(on_field_pct_mean = mean(as.numeric(plyr_play_pct)))

for(i in 1:nrow(mean_pos_pct_on_field)){
  mean_pos_matrix[,-1][i,i] <- mean_pos_pct_on_field$on_field_pct_mean[i]
}



#print for latex table
mean_pos_matrix %>% 
  select(position_group, QB, RB, SWR, WR, TE, OT, OG , C) %>% 
  xtable(digits = 3)
mean_pos_matrix %>% 
  select(position_group,  "DT", "DE", "ILB", "OLB", "SCB", "CB", "S") %>% 
  xtable(digits = 3)

## min on-field pct by position
min_pos_matrix <- team_season_pos_matrix %>% 
  group_by(position_group) %>% 
  summarize_at(vars(any_of(pos_vec)), ~min(., na.rm = T))


