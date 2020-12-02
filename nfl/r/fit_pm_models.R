###nfl playER Participation Data Pulls###
#Load Libraries
# library(beepr)
library(randomForest)
library(stringr)
require(curl)
require(httr)
library(MASS)
library(ggridges)
library(Matrix)
library(lubridate)
library(glue)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(rstan)
library(rstanarm)
library(tidyverse)
library(espnanalytics)


lowercaseCols <- function(data){
  return(rename_all(data, function(x)tolower(x))  )
}
source("prepPlusMinusDataForModel.R")
source("fit_rapm.R")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())### run on all cores
options(tibble.width = Inf)
options("scipen"=100, "digits"=8)
Sys.setenv("TZ" = "America/New_York")
Sys.setenv("ORA_SDTZ" = "America/New_York")
#Connect to DB
SDWconnection <- OpenOracleConnection("SDWPRD")
SDRconnection <- OpenOracleConnection("SDRPRD")
SDWReadOnly   <- dbConnect(dbDriver("Oracle"), "readonly", "gamerecap", dbname="SDWPRD")


### read in ratings from previous seasons from SDW

### edit variance to be a compilation of a players different positions
new_run <- FALSE
prior_plays <- 25
qualifying_plays <- 15 #(per team game)
qualifying_plays_season <- 75 ## qualifying plays for a whole season regardless of team games (for run plays and pass plays)
plays_game_constant <- 35 # constant to take per play stats to possession neutral per game stats (for each of runs and passes plays)
pm_replacement = -0.0773 ## replacement level plus-minus on a play 
pm_quantile <- 0.20 ## quantile for a replacement player (about 20th)


### position dictionary
positions.dict <- read_csv("../data/updated_pos.csv")
positions_translate <- positions.dict %>% select(Position, Group)
unique_pos <- positions_translate$Group %>% unique()

#### runs
nfl_runs <- QueryOracleTable(SDRconnection, query_text = "select * from NFL_METRICS_RUNS t
where weekly_run_of_record = 'Y'
                                    and week_of_run > 1
                                    and season >= 2007
                                    order by run_datetime_key", lowercase_col = TRUE) %>% as_tibble 


seasons_fromQuery <- QueryOracleTable(SDWconnection, query_text = "
                                      select season_id,  substr(description, 1, 4) as season_year, substr(description, 5, 7) as season_type, 
                                      start_date, end_date from SDROLTP.SEASON t
                                      where league_id = 324
                                      and (description LIKE ('%REG') OR description like ('%PLY') )
                                      order by end_date desc",lowercase_col = TRUE) %>% as_tibble

events <- QueryOracleTable(SDRconnection, query_text = "select substr(season.description, 1,4) as season, substr(season.description, 5, 7) as season_type,
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


nfl_rosters <- QueryOracleTable(SDRconnection, 
  query_text = "select 
                  roster.person_id, roster.team_id, roster.jersey_number,
                  roster.primary_position_id, pos.position_abbr as position,
                  teams.team,
                  experience, 
                  roster.modified_dt
                from SDROLTP.MOT_LOG roster
                left join sdroltp.person_position pos on pos.position_id = roster.primary_position_id
                inner join (select t.team_id, tn.team_name as team
                                  from SDROLTP.TEAM_LEAGUE t
                                  left join sdroltp.team_name tn on tn.team_id = t.team_id and tn.requestor_id = 0
                                  where league_id IN (select league_id from SDROLTP.LEAGUE t
                                                      where sport_id = 51
                                                      and (league_id = 3369 or parent = 3369 or parent IN (3462, 3463) ))
                                                      and tn.team_name NOT IN ('AFC', 'NFC')
                                                      ) teams
                            on teams.team_id = roster.team_id
                where pos.role_cd = 'P'
                order by person_id, modified_dt desc", lowercase_col = TRUE) %>% as_tibble



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
  where s.season_id IN (",  paste(seasons_fromQuery$season_id, collapse = ','), ") ",  
  "group by substr(season.description,1,4), tn.team_id, tn.team_name, r.person_id, r.first_name, r.last_name, pp.position
  order by count(pp.position) desc, max(pp.eventid) desc"),
  lowercase_col = TRUE) %>% as_tibble


position_play_counts_fromQuery <- position_play_counts_fromQuery %>% 
  left_join(positions_translate, by = c("position" = "Position") ) %>% 
  rename(position_group = Group)

nfl_rosters <- nfl_rosters %>%
  left_join(positions_translate, by = c("position" = "Position")) %>%
  rename(Group1 = Group) %>% 
  left_join(positions_translate, by = c("position" = "Group")) %>%
  rename(Group2 = Position) %>% 
  mutate(position_group = if_else(position %in% unique_pos, position,coalesce(Group1,Group2 )) ) %>% 
  select(-Group1, -Group2)

### denote playoff run or not
reg_season_dates <- seasons_fromQuery %>% 
  mutate(season_year = as.numeric(season_year)) %>% 
  filter(season_type == "REG")
ply_season_dates <- seasons_fromQuery %>% 
  mutate(season_year = as.numeric(season_year)) %>% 
  filter(season_type == "PLY")
nfl_runs <- nfl_runs%>% 
  left_join(ply_season_dates %>% 
              select(season_year, start_date) %>% 
              rename(ply_start_date = start_date),
            by = c("season" = "season_year")) %>% 
  mutate(season_type = if_else(run_cutoff_datetime < ply_start_date, "REG", "PLY"))
## only keep runs regular season . The last run of each season will serve as a playoff test run for cross-validation
nfl_runs <- nfl_runs %>% 
  filter(season_type == "REG") %>% 
  group_by(season) %>% 
  mutate(season_type = if_else(week_of_run == max(week_of_run), "PLY", season_type)) %>% 
  ungroup




if(new_run){
  run_start <- run_end <- nrow(nfl_runs)
}else{
  # stop("Manually put in the row of nfl_week_runs you want to run below")
  run_start <- 1
  run_end <- nrow(nfl_runs)
}

#See which positions are most important to NFL evaluators
nflDraft <- read_csv("../data/NFLDraftPlayers.csv", col_types = c('icccicccciccciiiiiiicidddd'))
## filter out seasons (Data mostly complete since 2000)
nflDraft <- nflDraft %>% filter(DraftYear >= 2000)
## see what the average position is of the first spot each player is taken
position_draft_importance <- nflDraft %>% arrange(DraftYear, DraftNumOverall) %>% 
  group_by(DraftYear, primaryPosition) %>% 
  mutate(num_selected = n()) %>% 
  filter(row_number(primaryPosition) == 1) %>% 
  group_by(primaryPosition) %>% 
  summarize(min_overall = min(DraftNumOverall) , 
            max_overall = max(DraftNumOverall) , 
            mean_overall = mean(DraftNumOverall) , 
            sd_overall = sd(DraftNumOverall) , 
            median_overall = median(DraftNumOverall),
            mean_num_selected = mean(num_selected),
            num_selected = sum(num_selected)
  ) %>% 
  arrange(median_overall)


teams <- nfl_rosters %>% 
  dplyr::select(team_id, team) %>% 
  unique()

### save turnover models for  each season 
nfl_seasons <- unique(nfl_runs$season)
for(current_season in nfl_seasons){
  if(!(
      current_season < max(nfl_seasons) & 
      file.exists(paste0("../data/interception_epa_adj_", current_season,".rds")) &
      file.exists(paste0("../data/rush_fumble_epa_adj_", current_season, ".rds")) &
      file.exists(paste0("../data/pass_fumble_epa_adj_", current_season, ".rds")) 
      ) 
     ){
    source("turnover_epa_modeling.R")

  }
  cat("Season: ", current_season, "\n")
}


### Begin Runs Loop #############################################
## initialize objects to save off model information
model_stat <- model_param <- NULL

# i_run = 4;run_start = 2
for(i_run in run_start:run_end){
  start_time <- Sys.time()

  current_run <- nfl_runs %>% slice(i_run) %>%
    rename(week = week_of_run)
  next_run <- nfl_runs %>% slice(i_run + 1)

  current_season <- current_run$season

  print(paste0("Starting run: ", i_run, " which is for day/time: ", current_run$run_cutoff_datetime))
  # if(current_run$season_type == "PLY")next

  ### set up the tables in sdr for the nfl player rater
  seasons.ref <- filter(seasons_fromQuery, season_year %in% current_season)


  #interception and rush fumble epa adjustments
  interception_epa_adj <- readRDS(paste0("../data/interception_epa_adj_", current_season,".rds"))
  rush_fumble_epa_adj <- readRDS(paste0("../data/rush_fumble_epa_adj_", current_season,".rds"))
  pass_fumble_epa_adj <- readRDS(paste0("../data/pass_fumble_epa_adj_", current_season, ".rds"))


  ## run prior means and variances for each player
  # source("BPM_Priors.R")


  players_by_season <- position_play_counts_fromQuery %>%
    group_by_at(vars(year, contains("_id"), contains("_name"), position_group)) %>%
    summarize(plays = sum(plays)) %>%
    arrange(desc(plays)) %>%
    group_by(person_id) %>%
    mutate(plays = sum(plays)) %>%
    slice(1) %>%
    ungroup() 
  
  
  players_this_season <- players_by_season %>%
    filter(year == current_season)

  teamsBySeason <- position_play_counts_fromQuery %>%
    filter(year == current_season) %>%
    select(team_id, team_name) %>%
    unique()


  participation_fromQuery <- QueryOracleTable(SDWconnection, query_text = paste0(
    "select s.schedule_id, s.eventid as event_id, s.week_no, pp.playcnt, tn.team_id, tn.team_name, r.person_id, r.first_name, r.last_name,
                                            pp.position,
                                            o.off_def,
                                            ep.epa,
                                            ep.posstm_wpa,
                                            ep.lev_index,
                                            (case when collapse.play_type = 'LOS' then collapse.play_subtype else collapse.play_type end) as play_type
                                            from presch.pff_participation pp
                                            join sdroltp.schedule s on s.eventid = pp.eventid
                                            join sdroltp.person r on r.person_id = pp.sdr_id
                                            join presch.pff_positions o on o.position = pp.position
                                            join sdroltp.team_name@presch_sdr_link tn on tn.team_id = pp.team+0 and tn.requestor_id = 0
                                            join presch.nfl_play_ep_wp@presch_sdr_link ep on ep.schedule_id = s.schedule_id and ep.unqply_group = pp.playcnt
                                            join presch.nfl_collapse@presch_sdr_link collapse on collapse.schedule_id = ep.schedule_id and collapse.unqply_group = ep.unqply_group
                                            where s.season_id IN (",  paste(seasons.ref$season_id, collapse = ','), ") ",
    "order by s.schedule_id asc, pp.playcnt asc"),
    lowercase_col = TRUE
  ) %>%   as_tibble %>%
    mutate(offense = if_else(tolower(off_def) == "off", 1, -1))
  # write_rds("//etsig01/SIG/nfl/Player Impact/Data/nfl_participation_2019.rds")

  ###games per team
  team_games <- participation_fromQuery %>%
    group_by(team_id)  %>%
    summarize(team_plays = n_distinct(playcnt),
              team_games = n_distinct(schedule_id)) %>%
    left_join(teamsBySeason, by = c("team_id"))



  participation_dat <- participation_fromQuery %>%
    filter(!(play_type %in% c("ExtraPoint", "FieldGoal", "Punt", "TwoPoint", "Penalty") )) %>% #omit special teams and penalty designated plays
    mutate(play_type = if_else(play_type %in% c("Aborted", "Rush", "Penalty-Rush"), "Run", "Pass") )


  ### temp id's should be given before splitting into test/training.
  player_plays <- participation_dat %>%
    arrange(desc(schedule_id)) %>%
    group_by(person_id, team_id, offense) %>%
    summarize(plays = n(),
              mean_epa_pos =  mean(epa) ) %>%
    left_join(team_games, by = c("team_id") ) %>%
    ungroup %>%
    mutate(qualified = ((plays >= 2*qualifying_plays_season) | (plays >= (team_games*2*qualifying_plays))) ,
           temp_person_id = case_when(qualified ~ person_id,
                                      TRUE ~ if_else(offense ==1 , -1, -2) %>% paste0(., team_id) %>%  as.numeric() )
    )

  season_players <- player_plays %>%
    arrange(temp_person_id) %>%
    pull(temp_person_id) %>%
    unique()
  unique_player_teams <- team_games$team_id %>% unique()

  # #### Replacement player id swap  ######
  participation_dat <- participation_dat %>%
    left_join(player_plays %>%
                select(contains("id"), offense), by = c("person_id", "team_id", "offense")) %>%
    mutate(person_id = temp_person_id) %>%
    select(-starts_with("temp_") )



  #### Broad Position Categories
  participation_dat <- participation_dat %>%
    left_join(positions_translate, by = c("position" = "Position") ) %>%
    rename(position_group = Group)

  ### position priors
  position_priors <- participation_dat %>%
    left_join(select(players_this_season, person_id, team_id, position_group) %>%
                rename(main_position = position_group), by = c("person_id", "team_id")) %>%
    group_by(main_position, position_group) %>%
    summarize(count = n()) %>% ## how many plays at a certain position based on roster position
    group_by(main_position) %>%
    mutate(fraction = count / sum(count),
           prior = fraction*prior_plays) %>% ungroup


  ### Split Training and Test for CV
  last_time_cutoff <- current_run$run_lastgame_datetime + hours(1)
  next_time_cutoff <- if_else(nrow(next_run) == 0 , Sys.time() + years(1), next_run$run_lastgame_datetime[1])

  participation_train <- participation_dat %>%
    mutate(game_dt = substr(schedule_id, 1, 10) %>% ymd() ) %>%
    filter( !between(game_dt, last_time_cutoff,  next_time_cutoff) )
  participation_test <- participation_dat %>%
    mutate(game_dt = substr(schedule_id, 1, 10) %>% ymd() ) %>%
    filter( between(game_dt, last_time_cutoff,  next_time_cutoff)   )
  #only need test x matrix and test y values and then remove participation_test

  if(nrow(participation_train) == 0 | nrow(participation_test) == 0)next

  train <- prepPlusMinusDataForModel(participation_train, player_plays, team_games)
  test <- prepPlusMinusDataForModel(participation_test, player_plays, team_games)
  # teamLeague, events, players_this_season,
  # interception_epa_adj, rush_fumble_epa_adj, pass_fumble_epa_adj,
  # teamsBySeason, current_run,
  #
  # qualifying_plays_season, qualifying_plays,
  # nfl_recruiting, recruiting_grade_cutoff, pm_replacement,
  # player_bpm_priors


  # APM/RAPM Models ---------------------------------------------------------------


  ### hfa only and team only model


  rapm_hfa_obj <- fit_rapm(y = train$y,
                           xtx = (sum(train$stanDat$home^2)) %>% as.matrix(),
                           xty = rbind(sum(train$stanDat$home*train$y)),
                           x = cbind(train$stanDat$home),
                           k = 0 ,
                           diag_ridge = diag(1),
                           test_y = test$y,
                           test_x = cbind(test$stanDat$home),
                           cv = TRUE)

  gc()

  model_stat <- tibble(model = "hfa",
                       k = 0,
                       mse = rapm_hfa_obj$mse_rapm,
                       mae = rapm_hfa_obj$mae_rapm,
                       cv_mse = rapm_hfa_obj$cv_mse_rapm,
                       cv_mae = rapm_hfa_obj$cv_mae_rapm,
                       league = 'nfl',
                       season = current_run$season,
                       week = current_run$week
  ) %>%
    bind_rows(model_stat, . )
  model_param <- tibble(model = "hfa",
                        k = 0,
                        league = 'nfl',
                        season = current_run$season,
                        week = current_run$week,
                        values = as.vector(rapm_hfa_obj$mod_coef),
                        var = rapm_hfa_obj$mod_coef_var,
                        coef = "hfa"
  ) %>%
    bind_rows(model_param, . )


  # kvalues = c(100, 200, 500, 1000, 10000)##k = 0 needs to be added back in
  kvalues <- c(0,10,100,500, 1000, 10000)



  for(r in 1:length(kvalues)){
    if(kvalues[r] == 0){
      k <- 0.00001
    }else{
      k <- kvalues[r]
    }

    rapm_player_obj <- fit_rapm(y = train$y,
                                xtx = train$xtx,
                                xty = train$xty,
                                x = train$x,
                                k = k ,
                                diag_ridge = train$diag_ridge,
                                test_y = test$y,
                                test_x = test$x,
                                cv = TRUE)

    gc()

    model_stat <- tibble(model = if_else(kvalues[r] == 0, "apm", "rapm"),
                         k = kvalues[r],
                         mse = rapm_player_obj$mse_rapm,
                         mae = rapm_player_obj$mae_rapm,
                         cv_mse = rapm_player_obj$cv_mse_rapm,
                         cv_mae = rapm_player_obj$cv_mae_rapm,
                         league = 'nfl',
                         season = current_run$season,
                         week = current_run$week
    ) %>%
      bind_rows(model_stat, . )
    model_param <- tibble(model = if_else(kvalues[r] == 0, "apm", "rapm"),
                          k = kvalues[r],
                          league = 'nfl',
                          season = current_run$season,
                          week = current_run$week,
                          values = as.vector(rapm_player_obj$mod_coef),
                          var = rapm_player_obj$mod_coef_var,
                          param = c(rep('intercept',2), 
                                    rep("player", length(season_players))#, 
                                    # rep("tm_off", length(unique_player_teams) ),
                                    # rep("tm_def", length(unique_player_teams) )
                          ),
                          sub_param = c("home", 
                                        "pass",
                                        season_players#,
                                        # unique_player_teams, 
                                        # unique_player_teams
                          )
    ) %>%
      bind_rows(model_param, . )

    cat("Finished k = ", kvalues[r], " \n")
  }



  # stan model --------------------------------------------------------------



  #
  #   #### need to inform priors around the position variance by the average position of the fist pick of that position in the nfl draft
  #   gc(verbose= TRUE)
  #     #remove sparse matrix for memory purposes
  #     rm(X_model)

  ####### agnostic/draft position/last season rapm PRIOR MODEL ####################
  stan_start_time <- Sys.time()
  nfl_rep_prior_fit <- stan(file = "../stan/nfl_rep_prior.stan", data = train$stanDat,
                             control = list(adapt_delta = 0.801,
                                            #stepsize_jitter = 0.925,
                                            max_treedepth = 14#,
                                            # adapt_init_buffer = 70, adapt_term_buffer = 45, adapt_window = 25
                             ),
                             pars = c("eta"),include = FALSE,
                             init = function(){list(b =train$stanDat$bpmMean, u = c(0.02),
                                                    s2model = c(0.98898),
                                                    phi = c( 2.78414, #QB
                                                             0.56643, #RB
                                                             1.06, 0.65, #SWR/WR
                                                             1, #TE
                                                             1.3, 1.3, 1.5, #OT/OG/C
                                                             1.3, 0.9, #DT/DE
                                                             0.9, 0.8, #OLB/ILB
                                                             1.09, 1.61,  #SCB/CB
                                                             1.2 ), #S ???
                                                    # s2player = c(0.0035),
                                                    positionVarCoef = rep(1, train$stanDat$nPlayers)
                             )}, seed = 6,#s2player = c(0.0015)
                             iter = 550, warmup =150, chains = 6, thin = 1,
                             save_warmup = FALSE)
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("Recruiting Stan Model took: ",
      time_length(stan_end_time - stan_start_time, unit = "minutes"),
      " minutes \n")

  # traceplot(nfl_rep_prior_fit, pars = 'phi')
  # print(nfl_rep_prior_fit, pars = c('u', 'g', 's2model', 's2player', 's2playType', 'phi'),
  #       probs = c(0.025, 0.5, 0.975), digits = 5)
  # posterior_means <- get_posterior_mean(nfl_rep_prior_fit)[,"mean-all chains"]
  # plyr_effect_ind <- str_detect(names(nfl_rep_prior_fit), "b\\[([[:digit:]]+)\\]" )


  #player effects
  plyr_summary <- summary(nfl_rep_prior_fit, pars = "b")$summary
  plyr_effect <- tibble(param = "player",
                        sub_param = paste0("player_", season_players),
                        effect = plyr_summary[,"mean"],
                        var = (plyr_summary[,"sd"])^2
  )

  ### home field and pass play effects
  home_effect <- rstan::extract(nfl_rep_prior_fit, "u")$u %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  pass_effect <- rstan::extract(nfl_rep_prior_fit, "g")$g %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))

  s2model_est <- rstan::extract(nfl_rep_prior_fit, "s2model")$s2model %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  s2player_est <- rstan::extract(nfl_rep_prior_fit, "s2player")$s2player %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  s2playType_est <- rstan::extract(nfl_rep_prior_fit, "s2playType")$s2playType %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))


  ##position effects
  phi.post <- rstan::extract(nfl_rep_prior_fit, "phi")$phi
  phi.post.tbl <- phi.post %>% as.data.frame %>% as_tibble %>%
    rename(QB = V1, RB = V2, SWR = V3, WR = V4, TE = V5, OT = V6, OG = V7, C = V8,
           DT = V9, DE = V10, OLB = V11, ILB = V12, SCB = V13, CB = V14, S = V15 ) %>%
    gather(key = "Position", value = "posteriorDraw")
  phi.post.tbl.med <- phi.post.tbl %>%
    group_by(Position) %>%
    summarize(effect = median(posteriorDraw),
              var = var(posteriorDraw)) %>%
    rename(sub_param = Position) %>%
    mutate(param = 'position') %>%
    arrange(effect)

  temp_model_param <- bind_rows(home_effect, pass_effect, s2model_est, s2player_est, s2playType_est) %>%
    rename(effect = est) %>%
    mutate(param = c(rep('intercept', 2), rep("variance", 3)),
           sub_param = c("home", "pass", "model", "player", "play_type")
    ) %>%
    rbind(., plyr_effect, phi.post.tbl.med)


  ## look at season_players -- in both this script and the prepPlusMinusDataForModel.R function

  # "x_i"
  # "x_p"
  # "x_x"
  # "x_dims"

  #recreate sparse matrix for test/training dataset
  train_x <- sparseMatrix(i =  train$x_i,
                          p = train$x_p,
                          x = train$x_x,
                          dims = train$x_dims,
                          index1 = FALSE
  ) %>%
    cbind(train$stanDat$home, train$stanDat$pass, .)

  test_x <- sparseMatrix(i =  test$x_i,
                         p = test$x_p,
                         x = test$x_x,
                         dims = test$x_dims,
                         index1 = FALSE
  ) %>%
    cbind(test$stanDat$home, test$stanDat$pass, .)

  parameter_vec <- c(home_effect$est, pass_effect$est, plyr_effect$effect)
  # ## to see if index1 needs to be true or false
  # if(ncol(test_x) != length(parameter_vec)){
  #   #recreate sparse matrix for test/training dataset
  #   train_x <- sparseMatrix(j =  train$stanDat$vX,
  #                           p = (train$stanDat$uX - 1),
  #                           x = train$stanDat$wX,
  #                           index1 = FALSE,
  #                           dims = c(train$stanDat$N, train$stanDat$nPlayers)
  #   ) %>%
  #     cbind(train$stanDat$home, train$stanDat$pass, .)
  #
  #   test_x <- sparseMatrix(j =  test$stanDat$vX,
  #                          p = (test$stanDat$uX - 1 ),
  #                          x = test$stanDat$wX,
  #                          index1 = FALSE,
  #                          dims = c(test$stanDat$N, test$stanDat$nPlayers)
  #   ) %>%
  #     cbind(test$stanDat$home, test$stanDat$pass, .)
  # }

  ## calculated estimated y's for training and test with model parameters
  train_y_hat <- train_x%*%parameter_vec
  test_y_hat <- test_x%*%parameter_vec



  model_stat <- tibble(model = "rpm_recruiting_prior",
                       k = NA_real_,
                       mse = mean((train_y_hat - train$y)^2),
                       mae = mean( abs(train_y_hat - train$y) ),
                       cv_mse = mean((test_y_hat - test$y)^2),
                       cv_mae = mean( abs(test_y_hat - test$y) ),
                       league = 'nfl',
                       season = current_run$season,
                       week = current_run$week
  ) %>%
    bind_rows(model_stat, . )
  model_param <- temp_model_param %>%
    rename(values = effect) %>%
    mutate(
      model = "rpm_replacement_prior",
      k = NA_real_,
      league = 'nfl',
      season = current_run$season,
      week = current_run$week
    ) %>%
    bind_rows(model_param, . )




  rm("train_x", "test_x", "train_y_hat", "test_y_hat")
  #
  #
  # #### BPM priors mod (not split between run and pass) ##################
  # stan_start_time <- Sys.time()
  # nfl_bpm_Fit <- stan(file = "../stan/nfl_bayes_bpm_prior.stan", data = train$stanDat,
  #                     control = list(adapt_delta = 0.801,
  #                                    #stepsize_jitter = 0.925,
  #                                    max_treedepth = 14#,
  #                                    # adapt_init_buffer = 70, adapt_term_buffer = 45, adapt_window = 25
  #                     ),
  #                     pars = c("eta"),include = FALSE,
  #                     init = function(){list(b =train$stanDat$bpmMean, u = c(0.02),
  #                                            s2model = c(0.98898),
  #                                            phi = c( 2.78414, #QB
  #                                                     0.56643, #RB
  #                                                     1.06, 0.65, #SWR/WR
  #                                                     1, #TE
  #                                                     1.3, 1.3, 1.5, #OT/OG/C
  #                                                     1.3, 0.9, #DT/DE
  #                                                     0.9, 0.8, #OLB/ILB
  #                                                     1.09, 1.61,  #SCB/CB
  #                                                     1.2 ), #S ???
  #                                            # s2player = c(0.0035),
  #                                            positionVarCoef = rep(1, train$stanDat$nPlayers)
  #                     )}, seed = 6,#s2player = c(0.0015)
  #                     iter = 420, warmup =150, chains = 6, thin = 1,
  #                     save_warmup = FALSE)
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("BPM Prior Stan Model took: ",
      time_length(stan_end_time - stan_start_time, unit = "minutes"),
      " minutes \n")
  # traceplot(nfl_bpm_Fit, pars = 'phi')
  # print(nfl_bpm_Fit, pars = c('u', 'g', 's2model', 's2player', 's2playType', 'phi'),
  #       probs = c(0.025, 0.5, 0.975), digits = 5)
  # posterior_means <- get_posterior_mean(nfl_bpm_Fit)[,"mean-all chains"]
  # plyr_effect_ind <- str_detect(names(nfl_bpm_Fit), "b\\[([[:digit:]]+)\\]" )
  #
  #
  # #player effects
  # plyr_summary <- summary(nfl_bpm_Fit, pars = "b")$summary
  # plyr_effect <- tibble(param = "player",
  #                       sub_param = paste0("player_", season_players),
  #                       effect = plyr_summary[,"mean"],
  #                       var = (plyr_summary[,"sd"])^2
  # )
  #
  # ### home field and pass play effects
  # home_effect <- rstan::extract(nfl_bpm_Fit, "u")$u %>% as_tibble() %>%
  #   summarize(est = median(value), var = var(value))
  # pass_effect <- rstan::extract(nfl_bpm_Fit, "g")$g %>% as_tibble() %>%
  #   summarize(est = median(value), var = var(value))
  #
  # s2model_est <- rstan::extract(nfl_bpm_Fit, "s2model")$s2model %>% as_tibble() %>%
  #   summarize(est = median(value), var = var(value))
  # # s2player_est <- rstan::extract(nfl_bpm_Fit, "s2player")$s2player %>% as_tibble() %>%
  # #   summarize(est = median(value), var = var(value))
  # s2playType_est <- rstan::extract(nfl_bpm_Fit, "s2playType")$s2playType %>% as_tibble() %>%
  #   summarize(est = median(value), var = var(value))
  #
  #
  # ##position effects
  # phi.post <- rstan::extract(nfl_bpm_Fit, "phi")$phi
  # phi.post.tbl <- phi.post %>% as.data.frame %>% as_tibble %>%
  #   rename(QB = V1, RB = V2, SWR = V3, WR = V4, TE = V5, OT = V6, OG = V7, C = V8,
  #          DT = V9, DE = V10, OLB = V11, ILB = V12, SCB = V13, CB = V14, S = V15 ) %>%
  #   gather(key = "Position", value = "posteriorDraw")
  # phi.post.tbl.med <- phi.post.tbl %>%
  #   group_by(Position) %>%
  #   summarize(effect = median(posteriorDraw),
  #             var = var(posteriorDraw)) %>%
  #   rename(sub_param = Position) %>%
  #   mutate(param = 'position') %>%
  #   arrange(effect)
  #
  # temp_model_param <- bind_rows(home_effect, pass_effect, s2model_est, s2playType_est) %>%
  #   rename(effect = est) %>%
  #   mutate(param = c(rep('intercept', 2), rep("variance", 2)),
  #          sub_param = c("home", "pass", "model", "play_type")
  #   ) %>%
  #   rbind(., plyr_effect, phi.post.tbl.med)
  #
  #
  # #recreate sparse matrix for test/training datast
  # train_x <- sparseMatrix(j =  train$stanDat$vX,
  #                         p = (train$stanDat$uX - 1),
  #                         x = train$stanDat$wX,
  #                         index1 = FALSE
  # ) %>%
  #   cbind(train$stanDat$home, train$stanDat$pass, .)
  #
  # test_x <- sparseMatrix(j =  test$stanDat$vX,
  #                        p = (test$stanDat$uX - 1),
  #                        x = test$stanDat$wX,
  #                        index1 = FALSE
  # ) %>%
  #   cbind(test$stanDat$home, test$stanDat$pass, .)
  #
  #
  # ## calculated estimated y's for training and test with model parameters
  # train_y_hat <- train_x%*%c(home_effect$est, pass_effect$est, plyr_effect$effect)
  # test_y_hat <- test_x%*%c(home_effect$est, pass_effect$est, plyr_effect$effect)
  #
  #
  #
  # model_stat <- tibble(model = "rpm_bpm_prior",
  #                      k = NA_real_,
  #                      mse = mean((train_y_hat - train$y)^2),
  #                      mae = mean( abs(train_y_hat - train$y) ),
  #                      cv_mse = mean((test_y_hat - test$y)^2),
  #                      cv_mae = mean( abs(test_y_hat - test$y) ),
  #                      league = 'nfl',
  #                      season = current_run$season,
  #                      week = current_run$week
  # ) %>%
  #   bind_rows(model_stat, . )
  # model_param <- temp_model_param %>%
  #   rename(values = effect) %>%
  #   mutate(
  #     model = "rpm_bpm_prior",
  #     k = NA_real_,
  #     league = 'nfl',
  #     season = current_run$season,
  #     week = current_run$week
  #   ) %>%
  #   bind_rows(model_param, . )
  #
  #
  #
  #
  # rm("train_x", "test_x", "train_y_hat", "test_y_hat")

  cat("Finished run = ", i_run, " ", current_run$run_datetime_key, " \n")


}

write_rds(model_stat, "../results/cv_leave_one_week_model_stats.rds")
write_rds(model_param, "../results/cv_leave_one_week_model_params.rds")


# end of season CV leave one season out -----------------------------------
nfl_season_runs <- nfl_runs %>% 
  group_by(season) %>% 
  filter(run_cutoff_datetime == max(run_cutoff_datetime)) %>% 
  ungroup()

model_stat_season <- model_param_season <- NULL

# i_run = 4;run_start = 2
run_start_season <- 1
run_end_season <- nrow(nfl_season_runs)
for(i_run in run_start_season:run_end_season){
  start_time <- Sys.time()
  
  current_run <- nfl_season_runs %>% slice(i_run) %>%
    rename(week = week_of_run)
  next_run <- nfl_season_runs %>% slice(i_run + 1)
  
  current_season <- current_run$season
  
  print(paste0("Starting run: ", i_run, " which is for day/time: ", current_run$run_cutoff_datetime))
  # if(current_run$season_type == "PLY")next
  
  ### set up the tables in sdr for the nfl player rater & next two seasons
  seasons.ref <- filter(seasons_fromQuery, season_year %in% c(current_season:(current_season+2)))
  
  
  #interception and rush fumble epa adjustments
  interception_epa_adj <- readRDS(paste0("../data/interception_epa_adj_", current_season,".rds"))
  rush_fumble_epa_adj <- readRDS(paste0("../data/rush_fumble_epa_adj_", current_season,".rds"))
  pass_fumble_epa_adj <- readRDS(paste0("../data/pass_fumble_epa_adj_", current_season, ".rds"))
  
  
  ## run prior means and variances for each player
  # source("BPM_Priors.R")
  
  
  players_by_season <- position_play_counts_fromQuery %>%
    group_by_at(vars(year, contains("_id"), contains("_name"), position_group)) %>%
    summarize(plays = sum(plays)) %>%
    arrange(desc(plays)) %>%
    group_by(person_id) %>%
    mutate(plays = sum(plays)) %>%
    slice(1) %>%
    ungroup() 
  

  players_this_season <- players_by_season %>%
    filter(year == current_season)
  
  teamsBySeason <- position_play_counts_fromQuery %>%
    filter(year == current_season) %>%
    select(team_id, team_name) %>%
    unique()
  
  
  participation_fromQuery <- QueryOracleTable(SDWconnection, query_text = paste0(
    "select s.schedule_id, s.eventid as event_id, s.week_no, pp.playcnt, tn.team_id, tn.team_name, r.person_id, r.first_name, r.last_name,
                                            pp.position,
                                            o.off_def,
                                            ep.epa,
                                            ep.posstm_wpa,
                                            ep.lev_index,
                                            (case when collapse.play_type = 'LOS' then collapse.play_subtype else collapse.play_type end) as play_type
                                            from presch.pff_participation pp
                                            join sdroltp.schedule s on s.eventid = pp.eventid
                                            join sdroltp.person r on r.person_id = pp.sdr_id
                                            join presch.pff_positions o on o.position = pp.position
                                            join sdroltp.team_name@presch_sdr_link tn on tn.team_id = pp.team+0 and tn.requestor_id = 0
                                            join presch.nfl_play_ep_wp@presch_sdr_link ep on ep.schedule_id = s.schedule_id and ep.unqply_group = pp.playcnt
                                            join presch.nfl_collapse@presch_sdr_link collapse on collapse.schedule_id = ep.schedule_id and collapse.unqply_group = ep.unqply_group
                                            where s.season_id IN (",  paste(seasons.ref$season_id, collapse = ','), ") ",
    "order by s.schedule_id asc, pp.playcnt asc"),
    lowercase_col = TRUE
  ) %>%   as_tibble %>%
    mutate(offense = if_else(tolower(off_def) == "off", 1, -1))
  # write_rds("//etsig01/SIG/nfl/Player Impact/Data/nfl_participation_2019.rds")
  
  ###games per team
  team_games <- participation_fromQuery %>%
    group_by(team_id)  %>%
    summarize(team_plays = n_distinct(playcnt),
              team_games = n_distinct(schedule_id)) %>%
    left_join(teamsBySeason, by = c("team_id"))
  
  
  
  participation_dat <- participation_fromQuery %>%
    filter(!(play_type %in% c("ExtraPoint", "FieldGoal", "Punt", "TwoPoint", "Penalty") )) %>% #omit special teams and penalty designated plays
    mutate(play_type = if_else(play_type %in% c("Aborted", "Rush", "Penalty-Rush"), "Run", "Pass") )
  
  
  ### temp id's should be given before splitting into test/training.
  player_plays <- participation_dat %>%
    arrange(desc(schedule_id)) %>%
    group_by(person_id, team_id, offense) %>%
    summarize(plays = n(),
              mean_epa_pos =  mean(epa) ) %>%
    left_join(team_games, by = c("team_id") ) %>%
    ungroup %>%
    mutate(qualified = ((plays >= 2*qualifying_plays_season) | (plays >= (team_games*2*qualifying_plays))) ,
           temp_person_id = case_when(qualified ~ person_id,
                                      TRUE ~ if_else(offense ==1 , -1, -2) %>% paste0(., team_id) %>%  as.numeric() )
    )
  
  season_players <- player_plays %>%
    arrange(temp_person_id) %>%
    pull(temp_person_id) %>%
    unique()
  unique_player_teams <- team_games$team_id %>% unique()
  
  # #### Replacement player id swap  ######
  participation_dat <- participation_dat %>%
    left_join(player_plays %>%
                select(contains("id"), offense), by = c("person_id", "team_id", "offense")) %>%
    mutate(person_id = temp_person_id) %>%
    select(-starts_with("temp_") )
  
  
  
  #### Broad Position Categories
  participation_dat <- participation_dat %>%
    left_join(positions_translate, by = c("position" = "Position") ) %>%
    rename(position_group = Group)
  
  ### position priors
  position_priors <- participation_dat %>%
    left_join(select(players_this_season, person_id, team_id, position_group) %>%
                rename(main_position = position_group), by = c("person_id", "team_id")) %>%
    group_by(main_position, position_group) %>%
    summarize(count = n()) %>% ## how many plays at a certain position based on roster position
    group_by(main_position) %>%
    mutate(fraction = count / sum(count),
           prior = fraction*prior_plays) %>% ungroup
  
  
  ### Split Training and Test for CV
  last_time_cutoff <- current_run$run_lastgame_datetime + hours(1)
  next_time_cutoff <- if_else(nrow(next_run) == 0 , Sys.time() + years(1), next_run$run_lastgame_datetime[1])
  
  participation_train <- participation_dat %>%
    mutate(game_dt = substr(schedule_id, 1, 10) %>% ymd() ) %>%
    filter( !between(game_dt, last_time_cutoff,  next_time_cutoff) )
  participation_test <- participation_dat %>%
    mutate(game_dt = substr(schedule_id, 1, 10) %>% ymd() ) %>%
    filter( between(game_dt, last_time_cutoff,  next_time_cutoff)   )
  #only need test x matrix and test y values and then remove participation_test
  
  if(nrow(participation_train) == 0 | nrow(participation_test) == 0)next
  
  train <- prepPlusMinusDataForModel(participation_train, player_plays, team_games)
  test <- prepPlusMinusDataForModel(participation_test, player_plays, team_games)
  # teamLeague, events, players_this_season,
  # interception_epa_adj, rush_fumble_epa_adj, pass_fumble_epa_adj,
  # teamsBySeason, current_run,
  #
  # qualifying_plays_season, qualifying_plays,
  # nfl_recruiting, recruiting_grade_cutoff, pm_replacement,
  # player_bpm_priors
  
  
  # APM/RAPM Models ---------------------------------------------------------------
  
  
  ### hfa only and team only model
  
  
  rapm_hfa_obj <- fit_rapm(y = train$y,
                           xtx = (sum(train$stanDat$home^2)) %>% as.matrix(),
                           xty = rbind(sum(train$stanDat$home*train$y)),
                           x = cbind(train$stanDat$home),
                           k = 0 ,
                           diag_ridge = diag(1),
                           test_y = test$y,
                           test_x = cbind(test$stanDat$home),
                           cv = TRUE)
  
  gc()
  
  model_stat_season <- tibble(model = "hfa",
                       k = 0,
                       mse = rapm_hfa_obj$mse_rapm,
                       mae = rapm_hfa_obj$mae_rapm,
                       cv_mse = rapm_hfa_obj$cv_mse_rapm,
                       cv_mae = rapm_hfa_obj$cv_mae_rapm,
                       league = 'nfl',
                       season = current_run$season,
                       week = current_run$week
  ) %>%
    bind_rows(model_stat_season, . )
  model_param_season <- tibble(model = "hfa",
                        k = 0,
                        league = 'nfl',
                        season = current_run$season,
                        week = current_run$week,
                        values = as.vector(rapm_hfa_obj$mod_coef),
                        var = rapm_hfa_obj$mod_coef_var,
                        coef = "hfa"
  ) %>%
    bind_rows(model_param_season, . )
  
  
  # kvalues = c(100, 200, 500, 1000, 10000)##k = 0 needs to be added back in
  kvalues <- c(0,10,100,500, 1000, 10000)
  
  
  
  for(r in 1:length(kvalues)){
    if(kvalues[r] == 0){
      k <- 0.00001
    }else{
      k <- kvalues[r]
    }
    
    rapm_player_obj <- fit_rapm(y = train$y,
                                xtx = train$xtx,
                                xty = train$xty,
                                x = train$x,
                                k = k ,
                                diag_ridge = train$diag_ridge,
                                test_y = test$y,
                                test_x = test$x,
                                cv = TRUE)
    
    gc()
    
    model_stat_season <- tibble(model = if_else(kvalues[r] == 0, "apm", "rapm"),
                         k = kvalues[r],
                         mse = rapm_player_obj$mse_rapm,
                         mae = rapm_player_obj$mae_rapm,
                         cv_mse = rapm_player_obj$cv_mse_rapm,
                         cv_mae = rapm_player_obj$cv_mae_rapm,
                         league = 'nfl',
                         season = current_run$season,
                         week = current_run$week
    ) %>%
      bind_rows(model_stat_season, . )
    model_param_season <- tibble(model = if_else(kvalues[r] == 0, "apm", "rapm"),
                          k = kvalues[r],
                          league = 'nfl',
                          season = current_run$season,
                          week = current_run$week,
                          values = as.vector(rapm_player_obj$mod_coef),
                          var = rapm_player_obj$mod_coef_var,
                          param = c(rep('intercept',2), 
                                    rep("player", length(season_players))#, 
                                    # rep("tm_off", length(unique_player_teams) ),
                                    # rep("tm_def", length(unique_player_teams) )
                          ),
                          sub_param = c("home", 
                                        "pass",
                                        season_players#,
                                        # unique_player_teams, 
                                        # unique_player_teams
                          )
    ) %>%
      bind_rows(model_param_season, . )
    
    cat("Finished k = ", kvalues[r], " \n")
  }
  
  
  
  # stan model --------------------------------------------------------------
  
  
  
  #
  #   #### need to inform priors around the position variance by the average position of the fist pick of that position in the nfl draft
  #   gc(verbose= TRUE)
  #     #remove sparse matrix for memory purposes
  #     rm(X_model)
  
  ####### agnostic/draft position/last season rapm PRIOR MODEL ####################
  stan_start_time <- Sys.time()
  nfl_rep_prior_fit <- stan(file = "../stan/nfl_rep_prior.stan", data = train$stanDat,
                            control = list(adapt_delta = 0.801,
                                           #stepsize_jitter = 0.925,
                                           max_treedepth = 14#,
                                           # adapt_init_buffer = 70, adapt_term_buffer = 45, adapt_window = 25
                            ),
                            pars = c("eta"),include = FALSE,
                            init = function(){list(b =train$stanDat$bpmMean, u = c(0.02),
                                                   s2model = c(0.98898),
                                                   phi = c( 2.78414, #QB
                                                            0.56643, #RB
                                                            1.06, 0.65, #SWR/WR
                                                            1, #TE
                                                            1.3, 1.3, 1.5, #OT/OG/C
                                                            1.3, 0.9, #DT/DE
                                                            0.9, 0.8, #OLB/ILB
                                                            1.09, 1.61,  #SCB/CB
                                                            1.2 ), #S ???
                                                   # s2player = c(0.0035),
                                                   positionVarCoef = rep(1, train$stanDat$nPlayers)
                            )}, seed = 6,#s2player = c(0.0015)
                            iter = 550, warmup =150, chains = 6, thin = 1,
                            save_warmup = FALSE)
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("Recruiting Stan Model took: ",
      time_length(stan_end_time - stan_start_time, unit = "minutes"),
      " minutes \n")
  
  # traceplot(nfl_rep_prior_fit, pars = 'phi')
  # print(nfl_rep_prior_fit, pars = c('u', 'g', 's2model', 's2player', 's2playType', 'phi'),
  #       probs = c(0.025, 0.5, 0.975), digits = 5)
  # posterior_means <- get_posterior_mean(nfl_rep_prior_fit)[,"mean-all chains"]
  # plyr_effect_ind <- str_detect(names(nfl_rep_prior_fit), "b\\[([[:digit:]]+)\\]" )
  
  
  #player effects
  plyr_summary <- summary(nfl_rep_prior_fit, pars = "b")$summary
  plyr_effect <- tibble(param = "player",
                        sub_param = paste0("player_", season_players),
                        effect = plyr_summary[,"mean"],
                        var = (plyr_summary[,"sd"])^2
  )
  
  ### home field and pass play effects
  home_effect <- rstan::extract(nfl_rep_prior_fit, "u")$u %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  pass_effect <- rstan::extract(nfl_rep_prior_fit, "g")$g %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  
  s2model_est <- rstan::extract(nfl_rep_prior_fit, "s2model")$s2model %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  s2player_est <- rstan::extract(nfl_rep_prior_fit, "s2player")$s2player %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  s2playType_est <- rstan::extract(nfl_rep_prior_fit, "s2playType")$s2playType %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  
  
  ##position effects
  phi.post <- rstan::extract(nfl_rep_prior_fit, "phi")$phi
  phi.post.tbl <- phi.post %>% as.data.frame %>% as_tibble %>%
    rename(QB = V1, RB = V2, SWR = V3, WR = V4, TE = V5, OT = V6, OG = V7, C = V8,
           DT = V9, DE = V10, OLB = V11, ILB = V12, SCB = V13, CB = V14, S = V15 ) %>%
    gather(key = "Position", value = "posteriorDraw")
  phi.post.tbl.med <- phi.post.tbl %>%
    group_by(Position) %>%
    summarize(effect = median(posteriorDraw),
              var = var(posteriorDraw)) %>%
    rename(sub_param = Position) %>%
    mutate(param = 'position') %>%
    arrange(effect)
  
  temp_model_param <- bind_rows(home_effect, pass_effect, s2model_est, s2player_est, s2playType_est) %>%
    rename(effect = est) %>%
    mutate(param = c(rep('intercept', 2), rep("variance", 3)),
           sub_param = c("home", "pass", "model", "player", "play_type")
    ) %>%
    rbind(., plyr_effect, phi.post.tbl.med)
  
  
  ## look at season_players -- in both this script and the prepPlusMinusDataForModel.R function
  
  # "x_i"
  # "x_p"
  # "x_x"
  # "x_dims"
  
  #recreate sparse matrix for test/training dataset
  train_x <- sparseMatrix(i =  train$x_i,
                          p = train$x_p,
                          x = train$x_x,
                          dims = train$x_dims,
                          index1 = FALSE
  ) %>%
    cbind(train$stanDat$home, train$stanDat$pass, .)
  
  test_x <- sparseMatrix(i =  test$x_i,
                         p = test$x_p,
                         x = test$x_x,
                         dims = test$x_dims,
                         index1 = FALSE
  ) %>%
    cbind(test$stanDat$home, test$stanDat$pass, .)
  
  parameter_vec <- c(home_effect$est, pass_effect$est, plyr_effect$effect)
 
  
  ## calculated estimated y's for training and test with model parameters
  train_y_hat <- train_x%*%parameter_vec
  test_y_hat <- test_x%*%parameter_vec
  
  
  
  model_stat_season <- tibble(model = "rpm_recruiting_prior",
                       k = NA_real_,
                       mse = mean((train_y_hat - train$y)^2),
                       mae = mean( abs(train_y_hat - train$y) ),
                       cv_mse = mean((test_y_hat - test$y)^2),
                       cv_mae = mean( abs(test_y_hat - test$y) ),
                       league = 'nfl',
                       season = current_run$season,
                       week = current_run$week
  ) %>%
    bind_rows(model_stat_season, . )
  model_param_season <- temp_model_param %>%
    rename(values = effect) %>%
    mutate(
      model = "rpm_replacement_prior",
      k = NA_real_,
      league = 'nfl',
      season = current_run$season,
      week = current_run$week
    ) %>%
    bind_rows(model_param_season, . )
  
  
  
  
  rm("train_x", "test_x", "train_y_hat", "test_y_hat")
  
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("BPM Prior Stan Model took: ",
      time_length(stan_end_time - stan_start_time, unit = "minutes"),
      " minutes \n")
 
  cat("Finished run = ", i_run, " ", current_run$run_datetime_key, " \n")
  
  
}

write_rds(model_stat_season, "../results/cv_leave_one_season_model_stats.rds")
write_rds(model_param_season, "../results/cv_leave_one_season_model_params.rds")



### end of season runs with no cross-validating
## initialize objects to save off model information


# Full season models ------------------------------------------------------



run_start <- 1
run_end <- nrow(nfl_season_runs)

model_stat_full_season <- model_param_full_season <- NULL
for(i_run in run_start:run_end){
  start_time <- Sys.time()
  
  current_run <- nfl_season_runs %>% slice(i_run) %>% 
    rename(week = week_of_run)
  next_run <- nfl_season_runs %>% slice(i_run + 1)
  
  current_season <- current_run$season
  
  print(paste0("Starting run: ", i_run, " which is for day/time: ", current_run$run_cutoff_datetime))
  # if(current_run$season_type == "PLY")next
  
  ### set up the tables in sdr for the nfl player rater
  seasons.ref <- filter(seasons_fromQuery, season_year %in% current_season)  
  
  
  #interception and rush fumble epa adjustments
  interception_epa_adj <- readRDS(paste0("../data/interception_epa_adj_", current_season,".rds"))
  rush_fumble_epa_adj <- readRDS(paste0("../data/rush_fumble_epa_adj_", current_season,".rds"))
  pass_fumble_epa_adj <- readRDS(paste0("../data/pass_fumble_epa_adj_", current_season, ".rds"))
  
  
  ## run prior means and variances for each player
  # source("BPM_Priors.R")
  
  
  players_by_season <- position_play_counts_fromQuery %>%
    group_by_at(vars(year, contains("_id"), contains("_name"), position_group)) %>%
    summarize(plays = sum(plays)) %>%
    arrange(desc(plays)) %>%
    group_by(person_id) %>%
    mutate(plays = sum(plays)) %>%
    slice(1) %>%
    ungroup() 
  
  
  players_this_season <- players_by_season %>%
    filter(year == current_season)
  
  teamsBySeason <- position_play_counts_fromQuery %>% 
    filter(year == current_season) %>% 
    select(team_id, team_name) %>% 
    unique()
  
  
  participation_fromQuery <- QueryOracleTable(SDWconnection, query_text = paste0(
    "select s.schedule_id, s.eventid as event_id, s.week_no, pp.playcnt, tn.team_id, tn.team_name, r.person_id, r.first_name, r.last_name,
                                            pp.position,
                                            o.off_def,
                                            ep.epa,
                                            ep.posstm_wpa,
                                            ep.lev_index, 
                                            (case when collapse.play_type = 'LOS' then collapse.play_subtype else collapse.play_type end) as play_type
                                            from presch.pff_participation pp
                                            join sdroltp.schedule s on s.eventid = pp.eventid
                                            join sdroltp.person r on r.person_id = pp.sdr_id
                                            join presch.pff_positions o on o.position = pp.position
                                            join sdroltp.team_name@presch_sdr_link tn on tn.team_id = pp.team+0 and tn.requestor_id = 0
                                            join presch.nfl_play_ep_wp@presch_sdr_link ep on ep.schedule_id = s.schedule_id and ep.unqply_group = pp.playcnt
                                            join presch.nfl_collapse@presch_sdr_link collapse on collapse.schedule_id = ep.schedule_id and collapse.unqply_group = ep.unqply_group
                                            where s.season_id IN (",  paste(seasons.ref$season_id, collapse = ','), ") ",
    "order by s.schedule_id asc, pp.playcnt asc"), 
    lowercase_col = TRUE
  ) %>%   as_tibble %>% 
    mutate(offense = if_else(tolower(off_def) == "off", 1, -1))
  # write_rds("//etsig01/SIG/nfl/Player Impact/Data/nfl_participation_2019.rds")
  
  ###games per team
  team_games <- participation_fromQuery %>% 
    group_by(team_id)  %>% 
    summarize(team_plays = n_distinct(playcnt),
              team_games = n_distinct(schedule_id)) %>% 
    left_join(teamsBySeason, by = c("team_id"))
  
  
  
  participation_dat <- participation_fromQuery %>%
    filter(!(play_type %in% c("ExtraPoint", "FieldGoal", "Punt", "TwoPoint", "Penalty") )) %>% #omit special teams and penalty designated plays
    mutate(play_type = if_else(play_type %in% c("Aborted", "Rush", "Penalty-Rush"), "Run", "Pass") )
  
  
  ### temp id's should be given before splitting into test/training.
  player_plays <- participation_dat %>% 
    arrange(desc(schedule_id)) %>% 
    group_by(person_id, team_id, offense) %>% 
    summarize(plays = n(), 
              mean_epa_pos =  mean(epa) ) %>%
    left_join(team_games, by = c("team_id") ) %>% 
    ungroup %>% 
    mutate(qualified = ((plays >= 2*qualifying_plays_season) | (plays >= (team_games*2*qualifying_plays))) ,
           temp_person_id = case_when(qualified ~ person_id,
                                      TRUE ~ if_else(offense ==1 , -1, -2) %>% paste0(., team_id) %>%  as.numeric() )
    ) 
  
  season_players <- player_plays %>% 
    arrange(temp_person_id) %>% 
    pull(temp_person_id) %>% 
    unique()
  unique_player_teams <- team_games$team_id %>% unique()
  
  # #### Replacement player id swap  ######
  participation_dat <- participation_dat %>%
    left_join(player_plays %>% 
                select(contains("id"), offense), by = c("person_id", "team_id", "offense")) %>%
    mutate(person_id = temp_person_id) %>% 
    select(-starts_with("temp_") )
  
  
  
  #### Broad Position Categories
  participation_dat <- participation_dat %>%
    left_join(positions_translate, by = c("position" = "Position") ) %>% 
    rename(position_group = Group)
  
  ### position priors
  position_priors <- participation_dat %>% 
    left_join(select(players_this_season, person_id, team_id, position_group) %>% 
                rename(main_position = position_group), by = c("person_id", "team_id")) %>% 
    group_by(main_position, position_group) %>% 
    summarize(count = n()) %>% ## how many plays at a certain position based on roster position
    group_by(main_position) %>% 
    mutate(fraction = count / sum(count),
           prior = fraction*prior_plays) %>% ungroup
  
  
  ### Split Training and Test for CV 

  participation_train <- participation_test <- participation_dat %>%
    mutate(game_dt = substr(schedule_id, 1, 10) %>% ymd() )
  #only need test x matrix and test y values and then remove participation_test
  
  if(nrow(participation_train) == 0 | nrow(participation_test) == 0)next
  
  train <- test <- prepPlusMinusDataForModel(participation_train, player_plays, team_games)
  # test <- prepPlusMinusDataForModel(participation_test, player_plays, team_games)
  # teamLeague, events, players_this_season, 
  # interception_epa_adj, rush_fumble_epa_adj, pass_fumble_epa_adj, 
  # teamsBySeason, current_run, 
  # 
  # qualifying_plays_season, qualifying_plays, 
  # nfl_recruiting, recruiting_grade_cutoff, pm_replacement,
  # player_bpm_priors
  
  
  # APM/RAPM Models ---------------------------------------------------------------
  
  
  ### hfa only and team only model
  
  
  rapm_hfa_obj <- fit_rapm(y = train$y, 
                           xtx = (sum(train$stanDat$home^2)) %>% as.matrix(), 
                           xty = rbind(sum(train$stanDat$home*train$y)), 
                           x = cbind(train$stanDat$home), 
                           k = 0 , 
                           diag_ridge = diag(1),
                           test_y = test$y, 
                           test_x = cbind(test$stanDat$home), 
                           cv = TRUE)
  
  gc()
  
  model_stat_full_season <- tibble(model = "hfa", 
                       k = 0, 
                       mse = rapm_hfa_obj$mse_rapm,
                       mae = rapm_hfa_obj$mae_rapm,
                       cv_mse = rapm_hfa_obj$cv_mse_rapm,
                       cv_mae = rapm_hfa_obj$cv_mae_rapm,
                       league = 'nfl', 
                       season = current_run$season, 
                       week = current_run$week
  ) %>% 
    bind_rows(model_stat_full_season, . )
  model_param_full_season <- tibble(model = "hfa", 
                        k = 0, 
                        league = 'nfl', 
                        season = current_run$season, 
                        week = current_run$week,
                        values = as.vector(rapm_hfa_obj$mod_coef), 
                        var = rapm_hfa_obj$mod_coef_var,
                        coef = "hfa"
  ) %>% 
    bind_rows(model_param_full_season, . )
  
  
  # kvalues = c(100, 200, 500, 1000, 10000)##k = 0 needs to be added back in
  kvalues <- c(0,10,100,500, 1000, 10000)
  
  
  
  for(r in 1:length(kvalues)){
    if(kvalues[r] == 0){
      k <- 0.00001
    }else{
      k <- kvalues[r]
    }
    
    rapm_player_obj <- fit_rapm(y = train$y, 
                                xtx = train$xtx, 
                                xty = train$xty, 
                                x = train$x, 
                                k = k , 
                                diag_ridge = train$diag_ridge,
                                test_y = test$y, 
                                test_x = test$x, 
                                cv = TRUE)
    
    gc()
    
    model_stat_full_season <- tibble(model = if_else(kvalues[r] == 0, "apm", "rapm"), 
                         k = kvalues[r], 
                         mse = rapm_player_obj$mse_rapm,
                         mae = rapm_player_obj$mae_rapm,
                         cv_mse = rapm_player_obj$cv_mse_rapm,
                         cv_mae = rapm_player_obj$cv_mae_rapm,
                         league = 'nfl', 
                         season = current_run$season, 
                         week = current_run$week
    ) %>% 
      bind_rows(model_stat_full_season, . )
    model_param_full_season <- tibble(model = if_else(kvalues[r] == 0, "apm", "rapm"), 
                          k = kvalues[r], 
                          league = 'nfl', 
                          season = current_run$season, 
                          week = current_run$week,
                          values = as.vector(rapm_player_obj$mod_coef), 
                          var = rapm_player_obj$mod_coef_var,
                          param = c(rep('intercept',2), 
                                    rep("player", length(season_players))#, 
                                    # rep("tm_off", length(unique_player_teams) ),
                                    # rep("tm_def", length(unique_player_teams) )
                          ),
                          sub_param = c("home", 
                                        "pass",
                                        season_players#,
                                        # unique_player_teams, 
                                        # unique_player_teams
                          )
    ) %>% 
      bind_rows(model_param_full_season, . )
    
    cat("Finished k = ", kvalues[r], " \n")
  }
  
  
  
  # stan model --------------------------------------------------------------
  
  
  
  #
  #   #### need to inform priors around the position variance by the average position of the fist pick of that position in the nfl draft
  #   gc(verbose= TRUE)
  #     #remove sparse matrix for memory purposes
  #     rm(X_model)
  
  ####### agnostic/draft position/last season rapm PRIOR MODEL ####################
  stan_start_time <- Sys.time()
  nfl_rep_prior_fit <- stan(file = "../stan/nfl_rep_prior.stan", data = train$stanDat,
                            control = list(adapt_delta = 0.801,
                                           #stepsize_jitter = 0.925,
                                           max_treedepth = 14#,
                                           # adapt_init_buffer = 70, adapt_term_buffer = 45, adapt_window = 25
                            ),
                            pars = c("eta"),include = FALSE,
                            init = function(){list(b =train$stanDat$bpmMean, u = c(0.02),
                                                   s2model = c(0.98898),
                                                   phi = c( 2.78414, #QB
                                                            0.56643, #RB
                                                            1.06, 0.65, #SWR/WR
                                                            1, #TE
                                                            1.3, 1.3, 1.5, #OT/OG/C
                                                            1.3, 0.9, #DT/DE
                                                            0.9, 0.8, #OLB/ILB
                                                            1.09, 1.61,  #SCB/CB
                                                            1.2 ), #S ???
                                                   # s2player = c(0.0035),
                                                   positionVarCoef = rep(1, train$stanDat$nPlayers)
                            )}, seed = 6,#s2player = c(0.0015)
                            iter = 550, warmup =150, chains = 6, thin = 1,
                            save_warmup = FALSE)
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("Recruiting Stan Model took: ",
      time_length(stan_end_time - stan_start_time, unit = "minutes"),
      " minutes \n")
  
  write_rds(nfl_rep_prior_fit, path = paste0("../stan/nfl_rep_prior_stanfit_", current_season, ".rds"))
  
  # traceplot(nfl_rep_prior_fit, pars = 'phi')
  # print(nfl_rep_prior_fit, pars = c('u', 'g', 's2model', 's2player', 's2playType', 'phi'),
  #       probs = c(0.025, 0.5, 0.975), digits = 5)
  # posterior_means <- get_posterior_mean(nfl_rep_prior_fit)[,"mean-all chains"]
  # plyr_effect_ind <- str_detect(names(nfl_rep_prior_fit), "b\\[([[:digit:]]+)\\]" )
  
  
  #player effects
  plyr_summary <- summary(nfl_rep_prior_fit, pars = "b")$summary
  plyr_effect <- tibble(param = "player",
                        sub_param = paste0("player_", season_players),
                        effect = plyr_summary[,"mean"],
                        var = (plyr_summary[,"sd"])^2
  )
  
  ### home field and pass play effects
  home_effect <- rstan::extract(nfl_rep_prior_fit, "u")$u %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  pass_effect <- rstan::extract(nfl_rep_prior_fit, "g")$g %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  
  s2model_est <- rstan::extract(nfl_rep_prior_fit, "s2model")$s2model %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  s2player_est <- rstan::extract(nfl_rep_prior_fit, "s2player")$s2player %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  s2playType_est <- rstan::extract(nfl_rep_prior_fit, "s2playType")$s2playType %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  
  
  ##position effects
  phi.post <- rstan::extract(nfl_rep_prior_fit, "phi")$phi
  phi.post.tbl <- phi.post %>% as.data.frame %>% as_tibble %>%
    rename(QB = V1, RB = V2, SWR = V3, WR = V4, TE = V5, OT = V6, OG = V7, C = V8,
           DT = V9, DE = V10, OLB = V11, ILB = V12, SCB = V13, CB = V14, S = V15 ) %>%
    gather(key = "Position", value = "posteriorDraw")
  phi.post.tbl.med <- phi.post.tbl %>%
    group_by(Position) %>%
    summarize(effect = median(posteriorDraw),
              var = var(posteriorDraw)) %>%
    rename(sub_param = Position) %>%
    mutate(param = 'position') %>%
    arrange(effect)
  
  temp_model_param <- bind_rows(home_effect, pass_effect, s2model_est, s2player_est, s2playType_est) %>%
    rename(effect = est) %>%
    mutate(param = c(rep('intercept', 2), rep("variance", 3)),
           sub_param = c("home", "pass", "model", "player", "play_type")
    ) %>%
    rbind(., plyr_effect, phi.post.tbl.med)
  
  
  ## look at season_players -- in both this script and the prepPlusMinusDataForModel.R function
  
  # "x_i"
  # "x_p"
  # "x_x"
  # "x_dims"
  
  #recreate sparse matrix for test/training dataset
  train_x <- sparseMatrix(i =  train$x_i,
                          p = train$x_p,
                          x = train$x_x,
                          dims = train$x_dims,
                          index1 = FALSE
  ) %>%
    cbind(train$stanDat$home, train$stanDat$pass, .)
  
  test_x <- sparseMatrix(i =  test$x_i,
                         p = test$x_p,
                         x = test$x_x,
                         dims = test$x_dims,
                         index1 = FALSE
  ) %>%
    cbind(test$stanDat$home, test$stanDat$pass, .)
  
  parameter_vec <- c(home_effect$est, pass_effect$est, plyr_effect$effect)
  
  
  ## calculated estimated y's for training and test with model parameters
  train_y_hat <- train_x%*%parameter_vec
  test_y_hat <- test_x%*%parameter_vec
  
  
  
  model_stat_full_season <- tibble(model = "rpm_recruiting_prior",
                       k = NA_real_,
                       mse = mean((train_y_hat - train$y)^2),
                       mae = mean( abs(train_y_hat - train$y) ),
                       cv_mse = mean((test_y_hat - test$y)^2),
                       cv_mae = mean( abs(test_y_hat - test$y) ),
                       league = 'nfl',
                       season = current_run$season,
                       week = current_run$week
  ) %>%
    bind_rows(model_stat_full_season, . )
  model_param_full_season <- temp_model_param %>%
    rename(values = effect) %>%
    mutate(
      model = "rpm_replacement_prior",
      k = NA_real_,
      league = 'nfl',
      season = current_run$season,
      week = current_run$week
    ) %>%
    bind_rows(model_param_full_season, . )
  
  
  
  
  rm("train_x", "test_x", "train_y_hat", "test_y_hat")
 
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("BPM Prior Stan Model took: ", 
      time_length(stan_end_time - stan_start_time, unit = "minutes"), 
      " minutes \n")      
  
  cat("Finished run = ", i_run, " ", current_run$run_datetime_key, " \n")
  
  
} 

write_rds(model_stat_full_season, "../results/full_year_model_stats.rds")
write_rds(model_param_full_season, "../results/full_year_model_params.rds")




# Multi-season Models -----------------------------------------------------


run_start <- 3
run_end <- nrow(nfl_season_runs)

model_stat_multi_season <- model_param_multi_season <- NULL
for(i_run in run_start:run_end){
  start_time <- Sys.time()
  
  current_run <- nfl_season_runs %>% slice((i_run-2):i_run) %>% 
    rename(week = week_of_run)
  next_run <- nfl_season_runs %>% slice(i_run + 1)
  
  current_season <- current_run$season
  
  print(paste0("Starting run: ", i_run, " which is for day/time: ", current_run$run_cutoff_datetime))
  # if(current_run$season_type == "PLY")next
  
  ### set up the tables in sdr for the nfl player rater
  seasons.ref <- filter(seasons_fromQuery, season_year %in% current_season)  
  
  
  #interception and rush fumble epa adjustments
  interception_epa_adj <- rush_fumble_epa_adj <- pass_fumble_epa_adj <- NULL
  for(s in current_season){
    interception_epa_adj <- readRDS(paste0("../data/interception_epa_adj_", s,".rds")) %>% 
      bind_rows(interception_epa_adj, .)
    rush_fumble_epa_adj <- readRDS(paste0("../data/rush_fumble_epa_adj_", s,".rds")) %>% 
      bind_rows(rush_fumble_epa_adj, .)
    pass_fumble_epa_adj <- readRDS(paste0("../data/pass_fumble_epa_adj_", s, ".rds")) %>% 
      bind_rows(pass_fumble_epa_adj, .)
  }

  
  
  ## run prior means and variances for each player
  # source("BPM_Priors.R")
  
  
  players_by_season <- position_play_counts_fromQuery %>%
    group_by_at(vars(year, contains("_id"), contains("_name"), position_group)) %>%
    summarize(plays = sum(plays)) %>%
    arrange(desc(plays)) %>%
    group_by(person_id) %>%
    mutate(plays = sum(plays)) %>%
    slice(1) %>%
    ungroup() 
  
  
  players_this_season <- players_by_season %>%
    filter(year %in% current_season)
  
  teamsBySeason <- position_play_counts_fromQuery %>% 
    filter(year %in% current_season) %>% 
    select(team_id, team_name) %>% 
    unique()
  
  
  participation_fromQuery <- QueryOracleTable(SDWconnection, query_text = paste0(
    "select s.schedule_id, s.eventid as event_id, s.week_no, pp.playcnt, tn.team_id, tn.team_name, r.person_id, r.first_name, r.last_name,
                                            pp.position,
                                            o.off_def,
                                            ep.epa,
                                            ep.posstm_wpa,
                                            ep.lev_index, 
                                            (case when collapse.play_type = 'LOS' then collapse.play_subtype else collapse.play_type end) as play_type
                                            from presch.pff_participation pp
                                            join sdroltp.schedule s on s.eventid = pp.eventid
                                            join sdroltp.person r on r.person_id = pp.sdr_id
                                            join presch.pff_positions o on o.position = pp.position
                                            join sdroltp.team_name@presch_sdr_link tn on tn.team_id = pp.team+0 and tn.requestor_id = 0
                                            join presch.nfl_play_ep_wp@presch_sdr_link ep on ep.schedule_id = s.schedule_id and ep.unqply_group = pp.playcnt
                                            join presch.nfl_collapse@presch_sdr_link collapse on collapse.schedule_id = ep.schedule_id and collapse.unqply_group = ep.unqply_group
                                            where s.season_id IN (",  paste(seasons.ref$season_id, collapse = ','), ") ",
    "order by s.schedule_id asc, pp.playcnt asc"), 
    lowercase_col = TRUE
  ) %>%   as_tibble %>% 
    mutate(offense = if_else(tolower(off_def) == "off", 1, -1))
  # write_rds("//etsig01/SIG/nfl/Player Impact/Data/nfl_participation_2019.rds")
  
  ###games per team
  team_games <- participation_fromQuery %>% 
    group_by(team_id)  %>% 
    summarize(team_plays = n_distinct(playcnt),
              team_games = n_distinct(schedule_id)) %>% 
    left_join(teamsBySeason, by = c("team_id"))
  
  
  
  participation_dat <- participation_fromQuery %>%
    filter(!(play_type %in% c("ExtraPoint", "FieldGoal", "Punt", "TwoPoint", "Penalty") )) %>% #omit special teams and penalty designated plays
    mutate(play_type = if_else(play_type %in% c("Aborted", "Rush", "Penalty-Rush"), "Run", "Pass") )
  
  
  ### temp id's should be given before splitting into test/training.
  player_plays <- participation_dat %>% 
    arrange(desc(schedule_id)) %>% 
    group_by(person_id, team_id, offense) %>% 
    summarize(plays = n(), 
              mean_epa_pos =  mean(epa) ) %>%
    left_join(team_games, by = c("team_id") ) %>% 
    ungroup %>% 
    mutate(qualified = ((plays >= 2*qualifying_plays_season) | (plays >= (team_games*2*qualifying_plays))) ,
           temp_person_id = case_when(qualified ~ person_id,
                                      TRUE ~ if_else(offense ==1 , -1, -2) %>% paste0(., team_id) %>%  as.numeric() )
    ) 
  
  season_players <- player_plays %>% 
    arrange(temp_person_id) %>% 
    pull(temp_person_id) %>% 
    unique()
  unique_player_teams <- team_games$team_id %>% unique()
  
  # #### Replacement player id swap  ######
  participation_dat <- participation_dat %>%
    left_join(player_plays %>% 
                select(contains("id"), offense), by = c("person_id", "team_id", "offense")) %>%
    mutate(person_id = temp_person_id) %>% 
    select(-starts_with("temp_") )
  
  
  
  #### Broad Position Categories
  participation_dat <- participation_dat %>%
    left_join(positions_translate, by = c("position" = "Position") ) %>% 
    rename(position_group = Group)
  
  ### position priors
  position_priors <- participation_dat %>% 
    left_join(select(players_this_season, person_id, team_id, position_group) %>% 
                rename(main_position = position_group), by = c("person_id", "team_id")) %>% 
    group_by(main_position, position_group) %>% 
    summarize(count = n()) %>% ## how many plays at a certain position based on roster position
    group_by(main_position) %>% 
    mutate(fraction = count / sum(count),
           prior = fraction*prior_plays) %>% ungroup
  
  
  ### Split Training and Test for CV 
  
  participation_train <- participation_test <- participation_dat %>%
    mutate(game_dt = substr(schedule_id, 1, 10) %>% ymd() )
  #only need test x matrix and test y values and then remove participation_test
  
  if(nrow(participation_train) == 0 | nrow(participation_test) == 0)next
  
  train <- test <- prepPlusMinusDataForModel(participation_train, player_plays, team_games)
  # test <- prepPlusMinusDataForModel(participation_test, player_plays, team_games)
  # teamLeague, events, players_this_season, 
  # interception_epa_adj, rush_fumble_epa_adj, pass_fumble_epa_adj, 
  # teamsBySeason, current_run, 
  # 
  # qualifying_plays_season, qualifying_plays, 
  # nfl_recruiting, recruiting_grade_cutoff, pm_replacement,
  # player_bpm_priors
  
  
  # APM/RAPM Models ---------------------------------------------------------------
  
  
  ### hfa only and team only model
  
  
  rapm_hfa_obj <- fit_rapm(y = train$y, 
                           xtx = (sum(train$stanDat$home^2)) %>% as.matrix(), 
                           xty = rbind(sum(train$stanDat$home*train$y)), 
                           x = cbind(train$stanDat$home), 
                           k = 0 , 
                           diag_ridge = diag(1),
                           test_y = test$y, 
                           test_x = cbind(test$stanDat$home), 
                           cv = TRUE)
  
  gc()
  
  model_stat_multi_season <- tibble(model = "hfa", 
                                   k = 0, 
                                   mse = rapm_hfa_obj$mse_rapm,
                                   mae = rapm_hfa_obj$mae_rapm,
                                   cv_mse = rapm_hfa_obj$cv_mse_rapm,
                                   cv_mae = rapm_hfa_obj$cv_mae_rapm,
                                   league = 'nfl', 
                                   season = max(current_run$season), 
                                   week = max(current_run$week)
  ) %>% 
    bind_rows(model_stat_multi_season, . )
  model_param_multi_season <- tibble(model = "hfa", 
                                    k = 0, 
                                    league = 'nfl', 
                                    season = max(current_run$season), 
                                    week = max(current_run$week),
                                    values = as.vector(rapm_hfa_obj$mod_coef), 
                                    var = rapm_hfa_obj$mod_coef_var,
                                    coef = "hfa"
  ) %>% 
    bind_rows(model_param_multi_season, . )
  
  
  # kvalues = c(100, 200, 500, 1000, 10000)##k = 0 needs to be added back in
  kvalues <- c(0,10,100,500, 1000, 10000)
  
  
  
  for(r in 1:length(kvalues)){
    if(kvalues[r] == 0){
      k <- 0.00001
    }else{
      k <- kvalues[r]
    }
    
    rapm_player_obj <- fit_rapm(y = train$y, 
                                xtx = train$xtx, 
                                xty = train$xty, 
                                x = train$x, 
                                k = k , 
                                diag_ridge = train$diag_ridge,
                                test_y = test$y, 
                                test_x = test$x, 
                                cv = TRUE)
    
    gc()
    
    model_stat_multi_season <- tibble(model = if_else(kvalues[r] == 0, "apm", "rapm"), 
                                     k = kvalues[r], 
                                     mse = rapm_player_obj$mse_rapm,
                                     mae = rapm_player_obj$mae_rapm,
                                     cv_mse = rapm_player_obj$cv_mse_rapm,
                                     cv_mae = rapm_player_obj$cv_mae_rapm,
                                     league = 'nfl', 
                                     season = max(current_run$season), 
                                     week = max(current_run$week)
    ) %>% 
      bind_rows(model_stat_multi_season, . )
    model_param_multi_season <- tibble(model = if_else(kvalues[r] == 0, "apm", "rapm"), 
                                      k = kvalues[r], 
                                      league = 'nfl', 
                                      season = max(current_run$season), 
                                      week = max(current_run$week),
                                      values = as.vector(rapm_player_obj$mod_coef), 
                                      var = rapm_player_obj$mod_coef_var,
                                      param = c(rep('intercept',2), 
                                                rep("player", length(season_players))#, 
                                                # rep("tm_off", length(unique_player_teams) ),
                                                # rep("tm_def", length(unique_player_teams) )
                                      ),
                                      sub_param = c("home", 
                                                    "pass",
                                                    season_players#,
                                                    # unique_player_teams, 
                                                    # unique_player_teams
                                                    )
    ) %>% 
      bind_rows(model_param_multi_season, . )
    
    cat("Finished k = ", kvalues[r], " \n")
  }
  
  
  
  # stan model --------------------------------------------------------------
  
  
  
  #
  #   #### need to inform priors around the position variance by the average position of the fist pick of that position in the nfl draft
  #   gc(verbose= TRUE)
  #     #remove sparse matrix for memory purposes
  #     rm(X_model)
  
  ####### agnostic/draft position/last season rapm PRIOR MODEL ####################
  stan_start_time <- Sys.time()
  nfl_rep_prior_fit <- stan(file = "../stan/nfl_rep_prior.stan", data = train$stanDat,
                            control = list(adapt_delta = 0.801,
                                           #stepsize_jitter = 0.925,
                                           max_treedepth = 14#,
                                           # adapt_init_buffer = 70, adapt_term_buffer = 45, adapt_window = 25
                            ),
                            pars = c("eta"),include = FALSE,
                            init = function(){list(b =train$stanDat$bpmMean, u = c(0.02),
                                                   s2model = c(0.98898),
                                                   phi = c( 2.78414, #QB
                                                            0.56643, #RB
                                                            1.06, 0.65, #SWR/WR
                                                            1, #TE
                                                            1.3, 1.3, 1.5, #OT/OG/C
                                                            1.3, 0.9, #DT/DE
                                                            0.9, 0.8, #OLB/ILB
                                                            1.09, 1.61,  #SCB/CB
                                                            1.2 ), #S ???
                                                   # s2player = c(0.0035),
                                                   positionVarCoef = rep(1, train$stanDat$nPlayers)
                            )}, seed = 6,#s2player = c(0.0015)
                            iter = 550, warmup =150, chains = 6, thin = 1,
                            save_warmup = FALSE)
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("Recruiting Stan Model took: ",
      time_length(stan_end_time - stan_start_time, unit = "minutes"),
      " minutes \n")
  
  write_rds(nfl_rep_prior_fit, path = paste0("../stan/nfl_rep_prior_stanfit_multiseason_", max(current_season), ".rds"))
  
  # traceplot(nfl_rep_prior_fit, pars = 'phi')
  # print(nfl_rep_prior_fit, pars = c('u', 'g', 's2model', 's2player', 's2playType', 'phi'),
  #       probs = c(0.025, 0.5, 0.975), digits = 5)
  # posterior_means <- get_posterior_mean(nfl_rep_prior_fit)[,"mean-all chains"]
  # plyr_effect_ind <- str_detect(names(nfl_rep_prior_fit), "b\\[([[:digit:]]+)\\]" )
  
  
  #player effects
  plyr_summary <- summary(nfl_rep_prior_fit, pars = "b")$summary
  plyr_effect <- tibble(param = "player",
                        sub_param = paste0("player_", season_players),
                        effect = plyr_summary[,"mean"],
                        var = (plyr_summary[,"sd"])^2
  )
  
  ### home field and pass play effects
  home_effect <- rstan::extract(nfl_rep_prior_fit, "u")$u %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  pass_effect <- rstan::extract(nfl_rep_prior_fit, "g")$g %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  
  s2model_est <- rstan::extract(nfl_rep_prior_fit, "s2model")$s2model %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  s2player_est <- rstan::extract(nfl_rep_prior_fit, "s2player")$s2player %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  s2playType_est <- rstan::extract(nfl_rep_prior_fit, "s2playType")$s2playType %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  
  
  ##position effects
  phi.post <- rstan::extract(nfl_rep_prior_fit, "phi")$phi
  phi.post.tbl <- phi.post %>% as.data.frame %>% as_tibble %>%
    rename(QB = V1, RB = V2, SWR = V3, WR = V4, TE = V5, OT = V6, OG = V7, C = V8,
           DT = V9, DE = V10, OLB = V11, ILB = V12, SCB = V13, CB = V14, S = V15 ) %>%
    gather(key = "Position", value = "posteriorDraw")
  phi.post.tbl.med <- phi.post.tbl %>%
    group_by(Position) %>%
    summarize(effect = median(posteriorDraw),
              var = var(posteriorDraw)) %>%
    rename(sub_param = Position) %>%
    mutate(param = 'position') %>%
    arrange(effect)
  
  temp_model_param <- bind_rows(home_effect, pass_effect, s2model_est, s2player_est, s2playType_est) %>%
    rename(effect = est) %>%
    mutate(param = c(rep('intercept', 2), rep("variance", 3)),
           sub_param = c("home", "pass", "model", "player", "play_type")
    ) %>%
    rbind(., plyr_effect, phi.post.tbl.med)
  
  
  ## look at season_players -- in both this script and the prepPlusMinusDataForModel.R function
  
  # "x_i"
  # "x_p"
  # "x_x"
  # "x_dims"
  
  #recreate sparse matrix for test/training dataset
  train_x <- sparseMatrix(i =  train$x_i,
                          p = train$x_p,
                          x = train$x_x,
                          dims = train$x_dims,
                          index1 = FALSE
  ) %>%
    cbind(train$stanDat$home, train$stanDat$pass, .)
  
  test_x <- sparseMatrix(i =  test$x_i,
                         p = test$x_p,
                         x = test$x_x,
                         dims = test$x_dims,
                         index1 = FALSE
  ) %>%
    cbind(test$stanDat$home, test$stanDat$pass, .)
  
  parameter_vec <- c(home_effect$est, pass_effect$est, plyr_effect$effect)
  
  
  ## calculated estimated y's for training and test with model parameters
  train_y_hat <- train_x%*%parameter_vec
  test_y_hat <- test_x%*%parameter_vec
  
  
  
  model_stat_multi_season <- tibble(model = "rpm_recruiting_prior",
                                   k = NA_real_,
                                   mse = mean((train_y_hat - train$y)^2),
                                   mae = mean( abs(train_y_hat - train$y) ),
                                   cv_mse = mean((test_y_hat - test$y)^2),
                                   cv_mae = mean( abs(test_y_hat - test$y) ),
                                   league = 'nfl',
                                   season = max(current_run$season),
                                   week = max(current_run$week)
  ) %>%
    bind_rows(model_stat_multi_season, . )
  model_param_multi_season <- temp_model_param %>%
    rename(values = effect) %>%
    mutate(
      model = "rpm_replacement_prior",
      k = NA_real_,
      league = 'nfl',
      season = max(current_run$season),
      week = max(current_run$week)
    ) %>%
    bind_rows(model_param_multi_season, . )
  
  
  
  
  rm("train_x", "test_x", "train_y_hat", "test_y_hat")
  
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("BPM Prior Stan Model took: ", 
      time_length(stan_end_time - stan_start_time, unit = "minutes"), 
      " minutes \n")      
  
  cat("Finished run = ", i_run, " ", current_run$run_datetime_key, " \n")
  
  
} 

write_rds(model_stat_multi_season, "../results/multiseason_model_stats.rds")
write_rds(model_param_multi_season, "../results/multiseason_model_params.rds")


