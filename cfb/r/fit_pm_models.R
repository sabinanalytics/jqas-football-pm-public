###CFB playER Participation Data Pulls###
#Load Libraries
# library(beepr)
library(MASS)
library(dbplyr)
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
Sys.setenv("TZ" = "America/New_York")
Sys.setenv("ORA_SDTZ" = "America/New_York")
#Connect to DB
QA <- FALSE
SDWconnection <- OpenOracleConnection("SDWPRD")
SDRconnection <- OpenOracleConnection("SDRPRD")
if(QA){
  SDRconnection <- OpenOracleConnection("SDRQA")
}
SDWReadOnly   <- dbConnect(dbDriver("Oracle"), "readonly", "gamerecap", dbname="SDWPRD")
options("scipen"=100, "digits"=8)

recruiting_grade_cutoff <- 60###anything less is treated as unevaluated

### read in ratings from previous seasons from SDW

### edit variance to be a compilation of a players different positions
new_run <- FALSE
prior_plays <- 25
qualifying_plays <- 15 #(per team game)
qualifying_plays_season <- 75 ## qualifying plays for a whole season regardless of team games (for run plays and pass plays)
plays_game_constant <- 35 # constant to take per play stats to possession neutral per game stats (for each of runs and passes plays)
pm_replacement = -0.0773 ## replacement level plus-minus on a play 
pm_quantile <- 0.20 ## quantile for a replacement player (about 20th)


#interception and rush fumble epa adjustments
interception_epa_adj <- readRDS("../models/interception_epa_adj.rds")
rush_fumble_epa_adj <- readRDS("../models/rush_fumble_epa_adj.rds")
pass_fumble_epa_adj <- readRDS("../models/pass_fumble_epa_adj.rds")


#### runs
cfb_runs <- read_rds("../data/cfb_runs.rds")


if(new_run){
  run_start <- run_end <- nrow(cfb_runs)
}else{
  # stop("Manually put in the row of cfb_week_runs you want to run below")
  run_start <- 1
  run_end <- nrow(cfb_runs)
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

teamLeague <- read_rds("../data/teamLeague.rds") %>% lowercaseCols()
current_rosters <- read_rds("../data/current_rosters.rds")
cfb_recruiting <- read_rds("../data/cfb_recruiting.rds")

teams <- read_rds("../data/teams.rds")
events <- read_rds(paste0("../data/events.rds"))

### Begin Runs Loop #############################################
## initialize objects to save off model information
# model_stat <- model_param <- NULL

model_stat <- read_rds("../results/model_stats.rds")
model_param <- read_rds("../results/model_params.rds")

# i_run = 4;run_start = 2#start week 19 here
for(i_run in run_start:run_end){
  start_time <- Sys.time()

  current_run <- cfb_runs %>% slice(i_run)
  next_run <- cfb_runs %>% slice(i_run + 1)

  current_season <- current_run$season


  ## run prior means and variances for each player
  source("BPM_Priors.R")


  players_by_season <- read_rds(paste0("../data/players_by_season_", current_run$season,".rds")) %>% lowercaseCols()
  teamsBySeason <- read_rds(paste0("../data/teamsBySeason_", current_run$season,".rds")) %>% lowercaseCols()


  participation_fromQuery <- read_rds(paste0("../data/cfb_participation_", current_run$season,".rds")) %>%
    mutate(offense = if_else(type == "O", 1, -1)) %>%
    mutate(play_type = if_else(play_type == "SK", "P", play_type)) %>% ## include sacks in passes
    filter(play_type != "PEN") %>% #for now get rid of "PEN" play_type -- only 67 in 2018 season
    filter(!(play_type %in% c("U", "FG", "EP", "2P", "FMBRT")) ) %>% ## for now, ignore special teams: play type "U"-Punt, "FG", "EP", "2P"
    filter(!is.na(espn_player_id))## if there isn't an espn Player id then don't keep in datase
  # write_rds("//etsig01/SIG/CFB/Player Impact/Data/cfb_participation_2019.rds")

  ###games per team
  team_games <- participation_fromQuery %>%
    group_by(espn_team_id)  %>%
    summarize(team_plays = n_distinct(record_id),
              team_games = n_distinct(espn_game_id)) %>%
    left_join(teamsBySeason %>%
                filter(season == current_run$season) %>%
                dplyr::select(team_id, class),
              by = c("espn_team_id" = "team_id")) %>%
    mutate(temp_espn_team_id = if_else(class == "1AA", -1, espn_team_id))

  ### temp id's should be given before splitting into test/training.
  player_plays <- participation_fromQuery %>%
    group_by(espn_player_id, espn_team_id, offense) %>%
    summarize(plays = n()) %>% ungroup %>%
    left_join(team_games, by = c("espn_team_id") ) %>%
    mutate(qualified = ((plays >= 2*qualifying_plays_season) | (plays >= (team_games*2*qualifying_plays))) ,### add in FBS requirement
           temp_espn_player_id = case_when(qualified & class == "1A" ~ espn_player_id,
                                           class == "1A"             ~ paste0(if_else(offense ==1 , -1, -2), espn_team_id) %>% as.numeric() ,
                                           TRUE                      ~ if_else(offense ==1 , -1, -2) %>% paste0(., espn_team_id) %>%  as.numeric() )
    ) %>%
    dplyr::select(-class)

  season_players <- player_plays %>%
    arrange(temp_espn_player_id) %>%
    pull(temp_espn_player_id) %>%
    unique()
  unique_player_teams <- team_games$temp_espn_team_id %>% unique()

  # #### Replacement player id swap  ######
  participation_fromQuery <- participation_fromQuery %>%
    left_join(player_plays %>%
                dplyr::select(contains("id"), offense), by = c("espn_player_id", "espn_team_id", "offense")) %>%
    mutate(espn_player_id = temp_espn_player_id,
           espn_team_id = temp_espn_team_id) %>%
    dplyr::select(-starts_with("temp_") )





  ### Split Training and Test for CV
  last_time_cutoff <- current_run$run_last_game_datetime + hours(1)
  next_time_cutoff <- if_else(nrow(next_run) == 0 , Sys.time() + years(1), next_run$run_last_game_datetime[1])

  participation_train <- participation_fromQuery %>%
    mutate(game_dt = substr(espn_schedule_id, 1, 10) %>% ymd() ) %>%
    filter( !between(game_dt, last_time_cutoff,  next_time_cutoff) )
  participation_test <- participation_fromQuery %>%
    mutate(game_dt = substr(espn_schedule_id, 1, 10) %>% ymd() ) %>%
    filter( between(game_dt, last_time_cutoff,  next_time_cutoff)   )
  #only need test x matrix and test y values and then remove participation_test


  train <- prepPlusMinusDataForModel(participation_train, player_plays, team_games)
  test <- prepPlusMinusDataForModel(participation_test, player_plays, team_games)
  # teamLeague, events, players_by_season,
  # interception_epa_adj, rush_fumble_epa_adj, pass_fumble_epa_adj,
  # teamsBySeason, current_run,
  #
  # qualifying_plays_season, qualifying_plays,
  # cfb_recruiting, recruiting_grade_cutoff, pm_replacement,
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
                       league = 'cfb',
                       season = current_run$season,
                       week = current_run$week
  ) %>%
    bind_rows(model_stat, . )
  model_param <- tibble(model = "hfa",
                        k = 0,
                        league = 'cfb',
                        season = current_run$season,
                        week = current_run$week,
                        values = as.vector(rapm_hfa_obj$mod_coef),
                        var = rapm_hfa_obj$mod_coef_var,
                        coef = "hfa"
  ) %>%
    bind_rows(model_param, . )


  # kvalues = c(100, 200, 500, 1000, 10000)##k = 0 needs to be added back in
  kvalues <- c(0, 10, 100, 500, 1000, 10000)



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
                         league = 'cfb',
                         season = current_run$season,
                         week = current_run$week
    ) %>%
      bind_rows(model_stat, . )
    model_param <- tibble(model = if_else(kvalues[r] == 0, "apm", "rapm"),
                          k = kvalues[r],
                          league = 'cfb',
                          season = current_run$season,
                          week = current_run$week,
                          values = as.vector(rapm_player_obj$mod_coef),
                          var = rapm_player_obj$mod_coef_var,
                          param = c('intercept', rep("player", length(season_players)),
                                    rep("tm_off", length(unique_player_teams) ),
                                    rep("tm_def", length(unique_player_teams) )
                          ),
                          sub_param = c("home",
                                        season_players,
                                        unique_player_teams,
                                        unique_player_teams)
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

  ####### RECRUITING PRIOR MODEL ####################
  stan_start_time <- Sys.time()
  cfb_recruiting_Fit <- stan(file = "../stan/cfb_bayes_recruiting_prior.stan", data = train$stanDat,
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
                             iter = 800, warmup =150, chains = 6, thin = 1,
                             save_warmup = FALSE)
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("Recruiting Stan Model took: ",
      time_length(stan_end_time - stan_start_time, unit = "minutes"),
      " minutes \n")

  # traceplot(cfb_recruiting_Fit, pars = 'phi')
  # print(cfb_recruiting_Fit, pars = c('u', 'g', 's2model', 's2player', 's2playType', 'phi'),
  #       probs = c(0.025, 0.5, 0.975), digits = 5)
  # posterior_means <- get_posterior_mean(cfb_recruiting_Fit)[,"mean-all chains"]
  # plyr_effect_ind <- str_detect(names(cfb_recruiting_Fit), "b\\[([[:digit:]]+)\\]" )


  #player effects
  plyr_summary <- summary(cfb_recruiting_Fit, pars = "b")$summary
  plyr_effect <- tibble(param = "player",
                        sub_param = paste0("player_", season_players),
                        effect = plyr_summary[,"mean"],
                        var = (plyr_summary[,"sd"])^2
  )

  ### home field and pass play effects
  home_effect <- rstan::extract(cfb_recruiting_Fit, "u")$u %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  pass_effect <- rstan::extract(cfb_recruiting_Fit, "g")$g %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))

  s2model_est <- rstan::extract(cfb_recruiting_Fit, "s2model")$s2model %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  s2player_est <- rstan::extract(cfb_recruiting_Fit, "s2player")$s2player %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  s2playType_est <- rstan::extract(cfb_recruiting_Fit, "s2playType")$s2playType %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))


  ##position effects
  phi.post <- rstan::extract(cfb_recruiting_Fit, "phi")$phi
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


  #recreate sparse matrix for test/training datast
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

  ## calculated estimated y's for training and test with model parameters
  train_y_hat <- train_x%*%c(home_effect$est, pass_effect$est, plyr_effect$effect)
  test_y_hat <- test_x%*%c(home_effect$est, pass_effect$est, plyr_effect$effect)



  model_stat <- tibble(model = "rpm_recruiting_prior",
                       k = NA_real_,
                       mse = mean((train_y_hat - train$y)^2),
                       mae = mean( abs(train_y_hat - train$y) ),
                       cv_mse = mean((test_y_hat - test$y)^2),
                       cv_mae = mean( abs(test_y_hat - test$y) ),
                       league = 'cfb',
                       season = current_run$season,
                       week = current_run$week
  ) %>%
    bind_rows(model_stat, . )
  model_param <- temp_model_param %>%
    rename(values = effect) %>%
    mutate(
      model = "rpm_recruiting_prior",
      k = NA_real_,
      league = 'cfb',
      season = current_run$season,
      week = current_run$week
    ) %>%
    bind_rows(model_param, . )




  rm("train_x", "test_x", "train_y_hat", "test_y_hat")


  #### BPM priors mod (not split between run and pass) ##################
  stan_start_time <- Sys.time()
  cfb_bpm_Fit <- stan(file = "../stan/cfb_bayes_bpm_prior.stan", data = train$stanDat,
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
                      iter = 800, warmup =150, chains = 6, thin = 1,
                      save_warmup = FALSE)
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("BPM Prior Stan Model took: ",
      time_length(stan_end_time - stan_start_time, unit = "minutes"),
      " minutes \n")
  # traceplot(cfb_bpm_Fit, pars = 'phi')
  # print(cfb_bpm_Fit, pars = c('u', 'g', 's2model', 's2player', 's2playType', 'phi'),
  #       probs = c(0.025, 0.5, 0.975), digits = 5)
  # posterior_means <- get_posterior_mean(cfb_bpm_Fit)[,"mean-all chains"]
  # plyr_effect_ind <- str_detect(names(cfb_bpm_Fit), "b\\[([[:digit:]]+)\\]" )


  #player effects
  plyr_summary <- summary(cfb_bpm_Fit, pars = "b")$summary
  plyr_effect <- tibble(param = "player",
                        sub_param = paste0("player_", season_players),
                        effect = plyr_summary[,"mean"],
                        var = (plyr_summary[,"sd"])^2
  )

  ### home field and pass play effects
  home_effect <- rstan::extract(cfb_bpm_Fit, "u")$u %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  pass_effect <- rstan::extract(cfb_bpm_Fit, "g")$g %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))

  s2model_est <- rstan::extract(cfb_bpm_Fit, "s2model")$s2model %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  # s2player_est <- rstan::extract(cfb_bpm_Fit, "s2player")$s2player %>% as_tibble() %>%
  #   summarize(est = median(value), var = var(value))
  s2playType_est <- rstan::extract(cfb_bpm_Fit, "s2playType")$s2playType %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))


  ##position effects
  phi.post <- rstan::extract(cfb_bpm_Fit, "phi")$phi
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

  temp_model_param <- bind_rows(home_effect, pass_effect, s2model_est, s2playType_est) %>%
    rename(effect = est) %>%
    mutate(param = c(rep('intercept', 2), rep("variance", 2)),
           sub_param = c("home", "pass", "model", "play_type")
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


  model_stat <- tibble(model = "rpm_bpm_prior",
                       k = NA_real_,
                       mse = mean((train_y_hat - train$y)^2),
                       mae = mean( abs(train_y_hat - train$y) ),
                       cv_mse = mean((test_y_hat - test$y)^2),
                       cv_mae = mean( abs(test_y_hat - test$y) ),
                       league = 'cfb',
                       season = current_run$season,
                       week = current_run$week
  ) %>%
    bind_rows(model_stat, . )
  model_param <- temp_model_param %>%
    rename(values = effect) %>%
    mutate(
      model = "rpm_bpm_prior",
      k = NA_real_,
      league = 'cfb',
      season = current_run$season,
      week = current_run$week
    ) %>%
    bind_rows(model_param, . )




  rm("train_x", "test_x", "train_y_hat", "test_y_hat")

  #### rep priors mod (not split between run and pass) ##################
  stan_start_time <- Sys.time()
  cfb_rep_Fit <- stan(file = "../stan/cfb_bayes_rep_prior.stan", data = train$stanDat,
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
                      iter = 800, warmup =150, chains = 6, thin = 1,
                      save_warmup = FALSE)
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("rep Prior Stan Model took: ",
      time_length(stan_end_time - stan_start_time, unit = "minutes"),
      " minutes \n")
  # traceplot(cfb_rep_Fit, pars = 'phi')
  # print(cfb_rep_Fit, pars = c('u', 'g', 's2model', 's2player', 's2playType', 'phi'),
  #       probs = c(0.025, 0.5, 0.975), digits = 5)
  # posterior_means <- get_posterior_mean(cfb_rep_Fit)[,"mean-all chains"]
  # plyr_effect_ind <- str_detect(names(cfb_rep_Fit), "b\\[([[:digit:]]+)\\]" )
  
  
  #player effects
  plyr_summary <- summary(cfb_rep_Fit, pars = "b")$summary
  plyr_effect <- tibble(param = "player",
                        sub_param = paste0("player_", season_players),
                        effect = plyr_summary[,"mean"],
                        var = (plyr_summary[,"sd"])^2
  )
  
  ### home field and pass play effects
  home_effect <- rstan::extract(cfb_rep_Fit, "u")$u %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  pass_effect <- rstan::extract(cfb_rep_Fit, "g")$g %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  
  s2model_est <- rstan::extract(cfb_rep_Fit, "s2model")$s2model %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  # s2player_est <- rstan::extract(cfb_rep_Fit, "s2player")$s2player %>% as_tibble() %>%
  #   summarize(est = median(value), var = var(value))
  s2playType_est <- rstan::extract(cfb_rep_Fit, "s2playType")$s2playType %>% as_tibble() %>%
    summarize(est = median(value), var = var(value))
  
  
  ##position effects
  phi.post <- rstan::extract(cfb_rep_Fit, "phi")$phi
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
  
  temp_model_param <- bind_rows(home_effect, pass_effect, s2model_est, s2playType_est) %>%
    rename(effect = est) %>%
    mutate(param = c(rep('intercept', 2), rep("variance", 2)),
           sub_param = c("home", "pass", "model", "play_type")
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
  
  
  model_stat <- tibble(model = "rpm_rep_prior",
                       k = NA_real_,
                       mse = mean((train_y_hat - train$y)^2),
                       mae = mean( abs(train_y_hat - train$y) ),
                       cv_mse = mean((test_y_hat - test$y)^2),
                       cv_mae = mean( abs(test_y_hat - test$y) ),
                       league = 'cfb',
                       season = current_run$season,
                       week = current_run$week
  ) %>%
    bind_rows(model_stat, . )
  model_param <- temp_model_param %>%
    rename(values = effect) %>%
    mutate(
      model = "rpm_rep_prior",
      k = NA_real_,
      league = 'cfb',
      season = current_run$season,
      week = current_run$week
    ) %>%
    bind_rows(model_param, . )
  
  
  
  
  rm("train_x", "test_x", "train_y_hat", "test_y_hat")
  
  cat("Finished run = ", i_run, " ", current_run$run_datetime_key, " \n")


}

write_rds(model_stat, "../results/model_stats.rds")
write_rds(model_param, "../results/model_params.rds")



### Run for full seasons
# i_run = 4;run_start = 2
model_stat_full_year <- model_param_full_year <- NULL
cfb_season_runs <- cfb_runs %>% 
  group_by(season_id) %>% 
  filter(run_datetime == max(run_datetime)) %>% 
  ungroup()
run_start <- 1
run_end <- nrow(cfb_season_runs)
for(i_run in run_start:run_end){
  start_time <- Sys.time()
  
  current_run <- cfb_season_runs %>% slice(i_run)
  next_run <- cfb_season_runs %>% slice(i_run + 1)
  
  current_season <- current_run$season
  
  
  ## run prior means and variances for each player
  source("BPM_Priors.R")
  
  
  players_by_season <- read_rds(paste0("../data/players_by_season_", current_run$season,".rds")) %>% lowercaseCols()
  teamsBySeason <- read_rds(paste0("../data/teamsBySeason_", current_run$season,".rds")) %>% lowercaseCols()
  
  
  participation_fromQuery <- read_rds(paste0("../data/cfb_participation_", current_run$season,".rds")) %>% 
    mutate(offense = if_else(type == "O", 1, -1)) %>% 
    mutate(play_type = if_else(play_type == "SK", "P", play_type)) %>% ## include sacks in passes
    filter(play_type != "PEN") %>% #for now get rid of "PEN" play_type -- only 67 in 2018 season
    filter(!(play_type %in% c("U", "FG", "EP", "2P", "FMBRT")) ) %>% ## for now, ignore special teams: play type "U"-Punt, "FG", "EP", "2P"
    filter(!is.na(espn_player_id))## if there isn't an espn Player id then don't keep in datase
  # write_rds("//etsig01/SIG/CFB/Player Impact/Data/cfb_participation_2019.rds")
  
  ###games per team
  team_games <- participation_fromQuery %>% 
    group_by(espn_team_id)  %>% 
    summarize(team_plays = n_distinct(record_id),
              team_games = n_distinct(espn_game_id)) %>% 
    left_join(teamsBySeason %>% 
                filter(season == current_run$season) %>% 
                dplyr::select(team_id, class), 
              by = c("espn_team_id" = "team_id")) %>% 
    mutate(temp_espn_team_id = if_else(class == "1AA", -1, espn_team_id)) 
  
  ### temp id's should be given before splitting into test/training.
  player_plays <- participation_fromQuery %>% 
    group_by(espn_player_id, espn_team_id, offense) %>% 
    summarize(plays = n()) %>% ungroup %>% 
    left_join(team_games, by = c("espn_team_id") ) %>% 
    mutate(qualified = ((plays >= 2*qualifying_plays_season) | (plays >= (team_games*2*qualifying_plays))) ,### add in FBS requirement
           temp_espn_player_id = case_when(qualified & class == "1A" ~ espn_player_id,
                                           class == "1A"             ~ paste0(if_else(offense ==1 , -1, -2), espn_team_id) %>% as.numeric() ,
                                           TRUE                      ~ if_else(offense ==1 , -1, -2) %>% paste0(., espn_team_id) %>%  as.numeric() )
    ) %>% 
    dplyr::select(-class)
  
  season_players <- player_plays %>% 
    arrange(temp_espn_player_id) %>% 
    pull(temp_espn_player_id) %>% 
    unique() 
  unique_player_teams <- team_games$temp_espn_team_id %>% unique()
  
  # #### Replacement player id swap  ######
  participation_fromQuery <- participation_fromQuery %>%
    left_join(player_plays %>% 
                dplyr::select(contains("id"), offense), by = c("espn_player_id", "espn_team_id", "offense")) %>%
    mutate(espn_player_id = temp_espn_player_id,
           espn_team_id = temp_espn_team_id) %>% 
    dplyr::select(-starts_with("temp_") )
  
  
  participation_train <- participation_fromQuery %>%
    mutate(game_dt = substr(espn_schedule_id, 1, 10) %>% ymd() ) 
  #only need test x matrix and test y values and then remove participation_test
  
  
  train <- test <- prepPlusMinusDataForModel(participation_train, player_plays, team_games)
  # teamLeague, events, players_by_season, 
  # interception_epa_adj, rush_fumble_epa_adj, pass_fumble_epa_adj, 
  # teamsBySeason, current_run, 
  # 
  # qualifying_plays_season, qualifying_plays, 
  # cfb_recruiting, recruiting_grade_cutoff, pm_replacement,
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
  
  model_stat_full_year <- tibble(model = "hfa", 
                       k = 0, 
                       mse = rapm_hfa_obj$mse_rapm,
                       mae = rapm_hfa_obj$mae_rapm,
                       cv_mse = rapm_hfa_obj$cv_mse_rapm,
                       cv_mae = rapm_hfa_obj$cv_mae_rapm,
                       league = 'cfb', 
                       season = current_run$season, 
                       week = current_run$week
  ) %>% 
    bind_rows(model_stat_full_year, . )
  model_param_full_year <- tibble(model = "hfa", 
                        k = 0, 
                        league = 'cfb', 
                        season = current_run$season, 
                        week = current_run$week,
                        values = as.vector(rapm_hfa_obj$mod_coef), 
                        var = rapm_hfa_obj$mod_coef_var,
                        coef = "hfa"
  ) %>% 
    bind_rows(model_param_full_year, . )
  
  
  # kvalues = c(100, 200, 500, 1000, 10000)##k = 0 needs to be added back in
  kvalues <- c(0, 1000)
  
  
  
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
    
    model_stat_full_year <- tibble(model = if_else(kvalues[r] == 0, "apm", "rapm"), 
                         k = kvalues[r], 
                         mse = rapm_player_obj$mse_rapm,
                         mae = rapm_player_obj$mae_rapm,
                         cv_mse = rapm_player_obj$cv_mse_rapm,
                         cv_mae = rapm_player_obj$cv_mae_rapm,
                         league = 'cfb', 
                         season = current_run$season, 
                         week = current_run$week
    ) %>% 
      bind_rows(model_stat_full_year, . )
    model_param_full_year <- tibble(model = if_else(kvalues[r] == 0, "apm", "rapm"), 
                          k = kvalues[r], 
                          league = 'cfb', 
                          season = current_run$season, 
                          week = current_run$week,
                          values = as.vector(rapm_player_obj$mod_coef), 
                          var = rapm_player_obj$mod_coef_var,
                          param = c('intercept', rep("player", length(season_players)), 
                                    rep("tm_off", length(unique_player_teams) ),
                                    rep("tm_def", length(unique_player_teams) )
                          ),
                          sub_param = c("home", 
                                        season_players,
                                        unique_player_teams, 
                                        unique_player_teams)
    ) %>% 
      bind_rows(model_param_full_year, . )
    
    cat("Finished k = ", kvalues[r], " \n")
  }
  
  
  
  # stan model --------------------------------------------------------------
  
  
  
  #   
  #   #### need to inform priors around the position variance by the average position of the fist pick of that position in the nfl draft
  #   gc(verbose= TRUE)
  #     #remove sparse matrix for memory purposes
  #     rm(X_model)
  
  ####### RECRUITING PRIOR MODEL ####################
  stan_start_time <- Sys.time()
  cfb_recruiting_Fit <- stan(file = "../stan/cfb_bayes_recruiting_prior.stan", data = train$stanDat,
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
                             iter = 420, warmup =150, chains = 6, thin = 1,
                             save_warmup = FALSE)
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("Recruiting Stan Model took: ", 
      time_length(stan_end_time - stan_start_time, unit = "minutes"), 
      " minutes \n")
  
  # traceplot(cfb_recruiting_Fit, pars = 'phi')
  # print(cfb_recruiting_Fit, pars = c('u', 'g', 's2model', 's2player', 's2playType', 'phi'),
  #       probs = c(0.025, 0.5, 0.975), digits = 5)
  # posterior_means <- get_posterior_mean(cfb_recruiting_Fit)[,"mean-all chains"]
  # plyr_effect_ind <- str_detect(names(cfb_recruiting_Fit), "b\\[([[:digit:]]+)\\]" )
  
  
  #player effects
  plyr_summary <- summary(cfb_recruiting_Fit, pars = "b")$summary
  plyr_effect <- tibble(param = "player", 
                        sub_param = paste0("player_", season_players), 
                        effect = plyr_summary[,"mean"], 
                        var = (plyr_summary[,"sd"])^2
  )
  
  ### home field and pass play effects
  home_effect <- rstan::extract(cfb_recruiting_Fit, "u")$u %>% as_tibble() %>% 
    summarize(est = median(value), var = var(value))
  pass_effect <- rstan::extract(cfb_recruiting_Fit, "g")$g %>% as_tibble() %>% 
    summarize(est = median(value), var = var(value))
  
  s2model_est <- rstan::extract(cfb_recruiting_Fit, "s2model")$s2model %>% as_tibble() %>% 
    summarize(est = median(value), var = var(value))
  s2player_est <- rstan::extract(cfb_recruiting_Fit, "s2player")$s2player %>% as_tibble() %>% 
    summarize(est = median(value), var = var(value))
  s2playType_est <- rstan::extract(cfb_recruiting_Fit, "s2playType")$s2playType %>% as_tibble() %>% 
    summarize(est = median(value), var = var(value))
  
  
  ##position effects
  phi.post <- rstan::extract(cfb_recruiting_Fit, "phi")$phi
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
  
  
  #recreate sparse matrix for test/training datast
  train_x <- sparseMatrix(i =  train$x_i,
                          p = train$x_p,
                          x = train$x_x,
                          dims = train$x_dims,
                          index1 = FALSE
  ) %>% 
    cbind(train$stanDat$home, train$stanDat$pass, .)

  
  ## calculated estimated y's for training and test with model parameters
  train_y_hat <- train_x%*%c(home_effect$est, pass_effect$est, plyr_effect$effect)

  
  model_stat_full_year <- tibble(model = "rpm_recruiting_prior", 
                       k = NA_real_, 
                       mse = mean((train_y_hat - train$y)^2),
                       mae = mean( abs(train_y_hat - train$y) ),
                       # cv_mse = mean((test_y_hat - test$y)^2),
                       # cv_mae = mean( abs(test_y_hat - test$y) ),
                       league = 'cfb', 
                       season = current_run$season, 
                       week = current_run$week
  ) %>% 
    bind_rows(model_stat_full_year, . )
  model_param_full_year <- temp_model_param %>% 
    rename(values = effect) %>% 
    mutate(
      model = "rpm_recruiting_prior", 
      k = NA_real_, 
      league = 'cfb', 
      season = current_run$season, 
      week = current_run$week
    ) %>% 
    bind_rows(model_param_full_year, . )  
  
  
  
  
  rm("train_x", "test_x", "train_y_hat", "test_y_hat")
  
  
  #### BPM priors mod (not split between run and pass) ##################
  stan_start_time <- Sys.time()
  cfb_bpm_Fit <- stan(file = "../stan/cfb_bayes_bpm_prior.stan", data = train$stanDat,
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
                      iter = 420, warmup =150, chains = 6, thin = 1,
                      save_warmup = FALSE)
  gc(verbose = TRUE)
  stan_end_time <- Sys.time()
  cat("BPM Prior Stan Model took: ", 
      time_length(stan_end_time - stan_start_time, unit = "minutes"), 
      " minutes \n")      
  # traceplot(cfb_bpm_Fit, pars = 'phi')
  # print(cfb_bpm_Fit, pars = c('u', 'g', 's2model', 's2player', 's2playType', 'phi'),
  #       probs = c(0.025, 0.5, 0.975), digits = 5)
  # posterior_means <- get_posterior_mean(cfb_bpm_Fit)[,"mean-all chains"]
  # plyr_effect_ind <- str_detect(names(cfb_bpm_Fit), "b\\[([[:digit:]]+)\\]" )
  
  
  #player effects
  plyr_summary <- summary(cfb_bpm_Fit, pars = "b")$summary
  plyr_effect <- tibble(param = "player", 
                        sub_param = paste0("player_", season_players), 
                        effect = plyr_summary[,"mean"], 
                        var = (plyr_summary[,"sd"])^2
  )
  
  ### home field and pass play effects
  home_effect <- rstan::extract(cfb_bpm_Fit, "u")$u %>% as_tibble() %>% 
    summarize(est = median(value), var = var(value))
  pass_effect <- rstan::extract(cfb_bpm_Fit, "g")$g %>% as_tibble() %>% 
    summarize(est = median(value), var = var(value))
  
  s2model_est <- rstan::extract(cfb_bpm_Fit, "s2model")$s2model %>% as_tibble() %>% 
    summarize(est = median(value), var = var(value))
  # s2player_est <- rstan::extract(cfb_bpm_Fit, "s2player")$s2player %>% as_tibble() %>% 
  #   summarize(est = median(value), var = var(value))
  s2playType_est <- rstan::extract(cfb_bpm_Fit, "s2playType")$s2playType %>% as_tibble() %>% 
    summarize(est = median(value), var = var(value))
  
  
  ##position effects
  phi.post <- rstan::extract(cfb_bpm_Fit, "phi")$phi
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
  
  temp_model_param <- bind_rows(home_effect, pass_effect, s2model_est, s2playType_est) %>% 
    rename(effect = est) %>% 
    mutate(param = c(rep('intercept', 2), rep("variance", 2)),
           sub_param = c("home", "pass", "model", "play_type")
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
  
  
  model_stat_full_year <- tibble(model = "rpm_bpm_prior", 
                       k = NA_real_, 
                       mse = mean((train_y_hat - train$y)^2),
                       mae = mean( abs(train_y_hat - train$y) ),
                       cv_mse = mean((test_y_hat - test$y)^2),
                       cv_mae = mean( abs(test_y_hat - test$y) ),
                       league = 'cfb', 
                       season = current_run$season, 
                       week = current_run$week
  ) %>% 
    bind_rows(model_stat_full_year, . )
  model_param_full_year <- temp_model_param %>% 
    rename(values = effect) %>% 
    mutate(
      model = "rpm_bpm_prior", 
      k = NA_real_, 
      league = 'cfb', 
      season = current_run$season, 
      week = current_run$week
    ) %>% 
    bind_rows(model_param_full_year, . )  
  
  
  
  
  rm("train_x", "test_x", "train_y_hat", "test_y_hat")
  
  cat("Finished run = ", i_run, " ", current_run$run_datetime_key, " \n")
  
  
} 

write_rds(model_stat_full_year, "../results/full_year_model_stats.rds")
write_rds(model_param_full_year, "../results/full_year_model_params.rds")





### code for splitting run v pass

# # Split Run and Pass ------------------------------------------------------
# 
#   
#   participation_model_run <- participation_model %>% 
#     filter(play_type == "R")
#   
#   participation_model_pass <- participation_model %>% 
#     filter(play_type == "P")
#   # 
#   # rm(list = c("participation", "participation_fromQuery", "participation_model"))#, 
#   # rm(list = c("qb_db", "rushes_data", "prior_data", "player_penalty_data", "player_drawn_penalty_data", "team_plays_data", "passes_data", 
#   #             "def_prior_data", "skill_player_data"))# data from priors
#   gc(verbose = TRUE)
# 
#   for(model_play_type in c('P', 'R')){
#     
#     if(model_play_type == 'P'){
#       temp_participation_model <- participation_model_pass
#     }else if(model_play_type == 'R'){
#       temp_participation_model <- participation_model_run
#     }
#     
#     ####players to group into replacement -- each team will have a differenet replacement value on both sides of the ball
#     team_games <- temp_participation_model %>% 
#       group_by(espn_schedule_id, espn_team_id) %>% 
#       slice(1) %>% 
#       group_by(espn_team_id) %>% 
#       summarize(games = n()) %>% ungroup
#     
#     player_plays <- temp_participation_model %>% 
#       group_by(espn_player_id, espn_team_id, offense) %>% 
#       summarize(plays = n()) %>% ungroup %>% 
#       left_join(team_games, by = "espn_team_id") %>% 
#       left_join(teamsBySeason %>% 
#                   filter(season == current_run$season) %>% 
#                   dplyr::select(team_id, class), 
#                 by = c("espn_team_id" = "team_id")) %>% 
#       mutate(qualified = ((plays >= qualifying_plays_season) | (plays >= (games*qualifying_plays))) & class == "1A",### add in FBS requirement
#              temp_espn_player_id = if_else(qualified, espn_player_id, paste0(if_else(offense ==1 , -1, -2), espn_team_id) %>% as.numeric ) 
#       ) %>% 
#       dplyr::select(-class)
#     
#     ## starts with -1 is a replacement offensive player for team and starts with -2 is defensive player for team_id of the following digits
#     
#     # #### Replacement player id swap  ######
#     temp_participation_model <- temp_participation_model %>%
#       left_join(player_plays %>% dplyr::select(contains("id"), offense), by = c("espn_player_id", "espn_team_id", "offense")) %>%
#       mutate(espn_player_id = temp_espn_player_id)
#     
#     
#     
#     
#     ###get unique players w/ their most played position
#     player_tbl <- temp_participation_model %>%
#       group_by(espn_player_id, espn_team_id, position_group) %>%
#       summarize(plays_position = n(), mean_epa_pos = mean(team_epa)) %>%
#       group_by(espn_player_id) %>%
#       arrange(desc(plays_position)) %>% 
#       mutate(plays = sum(plays_position)) %>%  
#       ungroup() %>% 
#       arrange(espn_player_id, desc(plays_position)) 
#     
#     
#     
#     ### add in recruiting grade
#     ### get rid of recruiting grades below the cutoff (these aren't real grades)
#     ### then only take most recent recruit with sdr mapping
#     cfb_recruiting_rm_duplicates <- cfb_recruiting %>% 
#       filter(grade >= recruiting_grade_cutoff) %>% 
#       arrange(person_id, desc(YEAR), recruiting_id) %>% 
#       group_by(person_id) %>% 
#       filter(row_number(person_id) == 1) %>% 
#       ungroup %>% 
#       dplyr::select(person_id, grade) %>% 
#       rename(recruiting_grade = grade)
#     
#     
#     player_tbl <- player_tbl %>% 
#       left_join(cfb_recruiting_rm_duplicates, by = c("espn_player_id" = "person_id")) %>% 
#       mutate(grade_use = if_else(!is.na(recruiting_grade) & recruiting_grade >= recruiting_grade_cutoff , 1, 0),
#              recruiting_grade = recruiting_grade %>% coalesce(0), 
#              QB_ind = if_else(position_group  == "QB", 1, 0))
#     
#     ### list all percentages a player has played a position -- plus priors on plays in each position
#     #### add in roster position for each player and get a posterior plays_position_pct
#     # player_pos_pct <- sis_rosters %>% dplyr::select(espn_player_id, position_group_roster) %>% 
#     #   left_join(player_plays %>% dplyr::select(espn_player_id, temp_espn_player_id), 
#     #             by = "espn_player_id") %>% 
#     #   mutate(espn_player_id = temp_espn_player_id) %>%
#     #   filter(!is.na(espn_player_id)) %>% 
#     #   dplyr::select(-temp_espn_player_id) %>% 
#     #   group_by(espn_player_id, position_group_roster) %>% unique %>% ungroup %>% 
#     #   inner_join(position_priors, by = c("position_group_roster")) %>% 
#     #   left_join(player_tbl %>% dplyr::select(espn_player_id, position_group, plays_position), 
#     #             by = c("espn_player_id", "position_group")) %>% 
#     #   mutate(plays_position = coalesce(plays_position, as.integer(0)) ) %>% 
#     #   group_by(espn_player_id, position_group) %>% 
#     #   summarize(prior = sum(prior), 
#     #             plays_position = sum(plays_position)) %>% 
#     #   ungroup %>% 
#     #   mutate(post_plays = prior + plays_position) %>% 
#     #   group_by(espn_player_id) %>% 
#     #   mutate(post_frac = post_plays / sum(post_plays)) %>% ungroup
#     
#     
#     #### add in roster position for each player and get a posterior plays_position_pct
#     player_pos_pct <- player_tbl %>% dplyr::select(espn_player_id, position_group, plays_position) %>% 
#       inner_join(position_priors, by = c("position_group")) %>% 
#       mutate(plays_position = coalesce(plays_position, as.integer(0)) ) %>% 
#       group_by(espn_player_id, position_group) %>% 
#       summarize(prior = sum(prior), 
#                 plays_position = sum(plays_position)) %>% 
#       ungroup %>% 
#       mutate(post_plays = prior + plays_position) %>% 
#       group_by(espn_player_id) %>% 
#       mutate(post_frac = post_plays / sum(post_plays)) %>% ungroup
#     
#     ### save one line per player 
#     player_tbl <- player_tbl %>%
#       group_by(espn_player_id) %>% 
#       slice(1) %>% ungroup %>% unique %>% 
#       mutate(player_index = row_number(espn_player_id)) %>% ungroup
#     
#     
#     player_pos_pct <- player_pos_pct %>% 
#       left_join(player_tbl %>% dplyr::select(espn_player_id, player_index), by = c("espn_player_id")) %>% 
#       ungroup %>% 
#       arrange(player_index, position_group)
#     
#     
#     temp_participation_model <- temp_participation_model %>% ungroup %>% 
#       arrange(espn_schedule_id, unqply_group, record_id, espn_team_id, espn_player_id) %>%
#       mutate(play_index = dense_rank(record_id)) %>%
#       arrange(play_index, espn_team_id, espn_player_id)
#     
#     #add player_index to temp_participation_model
#     temp_participation_model <- temp_participation_model %>% 
#       left_join(dplyr::select(player_tbl, espn_player_id, player_index), by = "espn_player_id")
#     
#     Nplays <- max(temp_participation_model$play_index)
#     
#     
#     # Non clutch-weighted epa
#     play_epa <- temp_participation_model %>% arrange(play_index) %>%
#       dplyr::select(play_index, team_epa) %>% unique %>% dplyr::select(team_epa) %>% unlist
#     
#     
#     ### home/away/neutral each play
#     play_home_ind <- temp_participation_model %>% filter(offense == 1) %>% 
#       arrange(play_index) %>%
#       dplyr::select(play_index, team_site_num) %>% unique %>% dplyr::select(team_site_num) %>% unlist
#     
#     ## play type on each play
#     play_type <- temp_participation_model %>% 
#       arrange(play_index, play_type) %>% 
#       dplyr::select(play_index, play_type) %>% unique %>% 
#       dplyr::select(play_type) %>% unlist
#     play_type %>% unique %>% as.factor
#     
#     ### look up the factor levels and which are which
#     player_pos <- factor(player_tbl$position_group) %>% fct_relevel(c("QB", "RB", "SWR", "WR", "TE", "OT", "OG", "C", 
#                                                                       "DT", "DE", "OLB", "ILB", "SCB", "CB", "S"))
#     
#     player_pos_pct_mat <- player_pos_pct %>% 
#       dplyr::select(player_index, position_group, post_frac) %>% 
#       spread(key = position_group, value = post_frac, fill = 0) %>% 
#       dplyr::select(QB, RB, SWR, WR, TE, OT, OG, C, 
#              DT, DE, OLB, ILB, SCB, CB, S)
#     
#     
#     ## will need to rearrange columns to match the order of the factors
#     
#     ## Also will need to add uncertainty to the position of each player. 
#     #Each player's count of positions ~ Multinomial(p) where p ~ Dirichlet(\alpha)
#     
#     tibble(pos = unique(player_pos) %>% sort, integer = 1:length(unique(player_pos)))
#     
#     
#     # pm_last_yr_mean <- pir_LastSeason$PLUS_MINUS_play %>% mean
#     # pm_last_yr_sd <- pir_LastSeason$PLUS_MINUS_play %>% sd
#     
#     
#     # ### add last seasons plus-minus and plays to player table
#     # player_tbl <- pir_LastSeason %>% 
#     #   dplyr::select(playER_id, PLYR_playS, PLUS_MINUS_play, position) %>% 
#     #   rename(plys_last_yr = PLYR_playS, 
#     #          pm_last_yr = PLUS_MINUS_play, 
#     #          pos_last_yr = position) %>% 
#     #   group_by(pos_last_yr) %>% 
#     #   mutate(pm_last_yr_z_score = scale(pm_last_yr) ) %>% ungroup %>% 
#     #   right_join(player_tbl, by = c("playER_id" = "espn_player_id") ) %>% 
#     #   mutate(plys_last_yr = coalesce(plys_last_yr, 0),
#     #          pm_last_yr = coalesce(pm_last_yr, pm_replacement),
#     #          #### account for people who change from QB to a new position to have a smaller prior (unusual but gives funky results)
#     #          pm_last_yr = case_when(is.na(pos_last_yr) ~ pm_last_yr, 
#     #                                 pos_last_yr == "QB" & position_group != "QB" ~ pm_last_yr_z_score*pm_last_yr_sd + pm_last_yr_mean, 
#     #                                 TRUE ~ pm_last_yr )) %>% 
#     #   dplyr::select(-pos_last_yr, -pm_last_yr_z_score)
#     
#     ### Add Box Plus-Minus (BPM) to player table
#     if(model_play_type == "P"){
#       player_bpm_priors <- player_bpm_priors %>% 
#         mutate(bpm_mean = pass_bpm, 
#                bpm_var = pass_bpm_var)
#     }else if(model_play_type == "R"){
#       player_bpm_priors <- player_bpm_priors %>% 
#         mutate(bpm_mean = rush_bpm, 
#                bpm_var = rush_bpm_var)
#     }
#     ### add BPM priors to player_tbl
#     player_tbl <- player_tbl %>% 
#       left_join(player_bpm_priors %>% 
#                   dplyr::select(espn_player_id, espn_team_id, bpm_mean, bpm_var), 
#                 by = c("espn_player_id" = "espn_player_id", "espn_team_id" = "espn_team_id")
#       ) %>% 
#       group_by(position_group) %>% 
#       mutate(bpm_mean = coalesce(bpm_mean, quantile(bpm_mean, 0.2, na.rm = T)), 
#              bpm_var = coalesce(bpm_var, quantile(bpm_var, 0.2, na.rm = T)) 
#       ) %>% ungroup()
#     
#     
#     ## To help in prior dplyr::selection, look at variance of positional epa's the adjust based on prior belief
#     # player_tbl %>% filter(plays >= 100) %>% 
#     #   group_by(position_group) %>% 
#     #   summarize(avg_epa_pos = weighted.mean(bpm_mean, w = plays), se_mean_epa_pos = weighted.mean(bpm_var, w = plays)) %>% 
#     #   mutate(empirical_a = (avg_epa_pos^2) / (se_mean_epa_pos^2), 
#     #          empirical_b =  avg_epa_pos    / (se_mean_epa_pos^2))
#     
#     # #last year's weight based on how many plays you had last year. everyone that didn't play last year starts with weight of 1
#     # player_tbl <- player_tbl %>% 
#     #   mutate(lastYrWeight = 1  + sqrt(plys_last_yr*0_0025) )#was 0.05
#     # 
#     
#     mean_epa <- mean(play_epa)
#     sd_epa <- sd(play_epa)
#     y <- (play_epa - mean_epa)/sd_epa
#     
#     X_model <- sparseMatrix(i = temp_participation_model$play_index, j = temp_participation_model$player_index, x = temp_participation_model$offense)
#     
#     ### weight X matrix based on garbage time
#     # ## weights based on the win probability at the start of the play -- if game greater than 99% for a team then it gets 12% weight
#     # weights <- temp_participation_model %>% group_by(play_index) %>% 
#     #   slice(1) %>% 
#     #   mutate(garbage_time_weight = if_else(team_WPS >= 0.015 & team_WPS <= 0.985, 1, 0.12)
#     #   ) %>% 
#     #   pull(garbage_time_weight)
#     # 
#     # X_model <- (1/weights^2) * X_model
#     # #weight y response based on garbage time
#     # y <- (1/weights^2) * ((play_epa - mean_epa)/sd_epa)
#     
#     sparse_parts <- extract_sparse_parts(X_model)
#     
#     ### weight of each position to soft sum constraint variances to 1
#     position_weights <- player_pos_pct %>% 
#       group_by(position_group) %>% 
#       summarize(
#         count = sum(plays_position)
#       ) %>% 
#       mutate(weight = count / sum(count),
#              position_group = fct_relevel(as.factor(position_group), levels(player_pos))) %>% 
#       arrange(position_group)
#     
#     
#     
#     
#     stanDat <- list(Playerid = as.integer(player_tbl$player_index),
#                     playType = as.integer(as_factor(play_type)),
#                     positions = as.integer(player_pos),
#                     home = as.integer(play_home_ind), 
#                     
#                     y = y, ### takes the centered at 0 epa for hopefully easier estimation
#                     N = Nplays,
#                     pmReplace = pm_replacement,
#                     nPlayers = max(player_tbl$player_index),
#                     nPlayType = nlevels(factor(play_type)),
#                     nPosition = nlevels(factor(player_tbl$position_group)),
#                     nPlayerPlays = length(sparse_parts$w),
#                     
#                     recgrade = player_tbl$recruiting_grade, 
#                     recgradeUse = player_tbl$grade_use, ### indicator whether or not to use the grade
#                     QBind = player_tbl$QB_ind, ### indicator if the player is listed as a QB
#                     
#                     bpmMean = player_tbl$bpm_mean,
#                     bpmVar = player_tbl$bpm_var,
#                     bpmSD = sqrt(player_tbl$bpm_var),
#                     
#                     posMat = player_pos_pct_mat  %>% as.matrix,
#                     
#                     #weight player plays to have mean 0 player effects
#                     playerPlayWeights = player_tbl$plays,
#                     
#                     posWeights = position_weights$weight,
#                     sdSumWeights = 0.0001,
#                     # plysLastYr = player_tbl$plys_last_yr, 
#                     # pmLastYr = player_tbl$pm_last_yr,
#                     # lastYrWeight = player_tbl$lastYrWeight,
#                     ### should be equivalent to making last year worth 50 plays for a player who played 1000 snaps last season
#                     
#                     wX = sparse_parts$w, ## values of sparse matrix
#                     vX = sparse_parts$v, # column indicators of sparse matrix
#                     uX = sparse_parts$u #row indicators of sparse matrix
#                     
#     )
#     
#     
#     #### need to inform priors around the position variance by the average position of the fist pick of that position in the nfl draft
#     gc(verbose= TRUE)
#     if(current_run$in_season_ind == "N"){
#       #remove sparse matrix for memory purposes
#       rm(X_model)
#       ########### STAN MODELS ######################
#       model_start_time <- Sys.time()
#       if(model_play_type == "P"){
#         cfb_pm_Fit <- stan(file = "../STAN/cfbpm_pass.stan", data = stanDat, 
#                            control = list(adapt_delta = 0.801, 
#                                           #stepsize_jitter = 0.925,
#                                           max_treedepth = 14#,
#                                           # adapt_init_buffer = 70, adapt_term_buffer = 45, adapt_window = 25
#                            ),
#                            pars = c("eta"),include = FALSE,
#                            init = function(){list(b =player_tbl$bpm_mean, u = c(0.02),
#                                                   s2model = c(0.98898),
#                                                   phi = c( 2.78414, #QB
#                                                            0.56643, #RB
#                                                            1.06, 0.65, #SWR/WR
#                                                            1, #TE
#                                                            1.3, 1.3, 1.5, #OT/OG/C 
#                                                            1.3, 0.9, #DT/DE 
#                                                            0.9, 0.8, #OLB/ILB 
#                                                            1.09, 1.61,  #SCB/CB
#                                                            1.2 ), #S ???
#                                                   # s2player = c(0.0035),
#                                                   positionVarCoef = rep(1, stanDat$nPlayers)
#                            )}, seed = 5,#s2player = c(0.0015)
#                            iter = 420, warmup =150, chains = 5, thin = 1, 
#                            save_warmup = FALSE)
#         gc(verbose = TRUE)
#       }else if(model_play_type == "R"){
#         cfb_pm_Fit <- stan(file = "../STAN/cfbpm_run.stan", data = stanDat, 
#                            control = list(adapt_delta = 0.801, 
#                                           #stepsize_jitter = 0.925,
#                                           max_treedepth = 14#,
#                                           # adapt_init_buffer = 70, adapt_term_buffer = 45, adapt_window = 25
#                            ),
#                            pars = c("eta"),include = FALSE,
#                            init = function(){list(b = player_tbl$bpm_mean, u = c(0.02),
#                                                   s2model = c(0.98898),
#                                                   phi = c( 2.78414, #QB
#                                                            1.56643, #RB
#                                                            1.06, 0.65, #SWR/WR
#                                                            1.12, #TE
#                                                            1.3, 1.3, 1.5, #OT/OG/C 
#                                                            1.3, 0.9, #DT/DE 
#                                                            0.9, 0.8, #OLB/ILB 
#                                                            1.09, 1.61,  #SCB/CB
#                                                            1.2 ), #S ???
#                                                   # s2player = c(0.0035),
#                                                   positionVarCoef = rep(1, stanDat$nPlayers)
#                            )}, seed = 5,#s2player = c(0.0015)
#                            iter = 420, warmup =150, chains = 5, thin = 1, 
#                            save_warmup = FALSE)
#         
#         #### Add in home field advantage to test, then add in fpi differences at the very least to test to see if there is an effect
#         gc(verbose = TRUE)
#       }
#       
#       cat(model_play_type, " Model Fit time: ", time_length(Sys.time() - model_start_time , unit = "hour") , " hours \n")
#       
#       save(cfb_pm_Fit, file = paste0("../STAN/season_model_outputs/", current_run$season,"_", model_play_type, "_stan_fit.RData") )
#       # tryCatch(save(cfb_pm_Fit, file = paste0("//etsig01/SIG/CFB/Player Impact/Research Analysis/current_run_model_cfb.RData") ) )
#       print(cfb_pm_Fit, pars = c( "s2model", "u", "phi"), #
#             probs = c(0.025, 0.5, 0.975), digits = 5)
#       # traceplot(cfb_pm_Fit, pars = "phi")
#       
#       #### Write out Parameters
#       posterior_param_summary <- summary(cfb_pm_Fit)$summary
#       posterior_means <- get_posterior_mean(cfb_pm_Fit)[,"mean-all chains"]
#       phi_post <- rstan::extract(cfb_pm_Fit, "phi")$phi
#       
#       write_rds(phi_post, path = paste0("../STAN/season_model_outputs/", current_run$season,"_", model_play_type, "_phi.rds"))
#       write_rds(posterior_param_summary, path = paste0("../STAN/season_model_outputs/", current_run$season,"_", model_play_type, "_posterior_summary.rds"))
#       write_rds(posterior_means, path = paste0("../STAN/season_model_outputs/", current_run$season,"_", model_play_type, "_posterior_means.rds"))
#       
#       write_rds(phi_post, path = paste0("../STAN/season_model_outputs/current_", model_play_type, "_phi.rds"))
#       write_rds(posterior_param_summary, path = paste0("../STAN/season_model_outputs/current_", model_play_type, "_posterior_summary.rds"))
#       write_rds(posterior_means, path = paste0("../STAN/season_model_outputs/current_", model_play_type, "_posterior_means.rds"))
#       
#       phi_post_tbl <- phi_post %>% as.data.frame %>% as_tibble %>% 
#         rename(QB = V1, RB = V2, SWR = V3, WR = V4, TE = V5, OT = V6, OG = V7, C = V8,
#                DT = V9, DE = V10, OLB = V11, ILB = V12, SCB = V13, CB = V14, S = V15 ) %>% 
#         gather(key = "Position", value = "posteriorDraw") 
#       
#       phi_post_tbl_med <- phi_post_tbl %>% 
#         group_by(Position) %>% 
#         summarize(median_effect = median(posteriorDraw)) %>% 
#         arrange(median_effect)
#       
#       
#       ###### Plot Position Impact for end of season runs
#       max_posteriorDraw <- max(phi_post)
#       position_var_plot <- ggplot(phi_post_tbl %>% mutate(Position = fct_relevel(Position, (phi_post_tbl_med$Position))),
#                                   aes(x = posteriorDraw, y = Position,
#                                       fill = Position, height = ..density..)) +
#         stat_density_ridges(alpha = 0.5, quantile_lines = TRUE, quantiles = 4, show.legend = FALSE) +
#         ylab("") + xlab("Penalization Divisor") + xlim(-0.1,max_posteriorDraw) +
#         theme_bw() + theme(text = element_text(size = 20))
#       
#       if(model_play_type == "P"){
#         ggsave(position_var_plot + ggtitle("Passing Plays"), filename = paste0("../Plots/passing_phi_position_",current_run$season ,".pdf") )
#       }else if(model_play_type == "R"){
#         ggsave(position_var_plot + ggtitle("Running Plays"), filename = paste0("../Plots/rushing_phi_position_",current_run$season ,".pdf") )
#       }
#       
#       
#     }
#     
#     ### POST STAN MODELS #####################
#     if(current_run$season == 2018){### first year of model has to take its's end of season parameter estimates
#       file_prefix <- paste0("../STAN/season_model_outputs/2018_", model_play_type)
#     }else{
#       
#       if(current_run$in_season_ind == "Y"){### if not end of season run, take last year's parameter estimates
#         file_prefix <- paste0("../STAN/season_model_outputs/", (current_run$season-1),"_", model_play_type)
#       }else if(current_run$in_season_ind == "N"){## at end of season, use this season's parameter estimates
#         file_prefix <- paste0("../STAN/season_model_outputs/", current_run$season,"_", model_play_type)
#       }
#     }
#     
#     ### read in data 
#     phi_post <- read_rds(path = paste0(file_prefix, "_phi.rds"))
#     posterior_param_summary <- read_rds(paste0(file_prefix, "_posterior_summary.rds"))
#     posterior_means <- read_rds(path = paste0(file_prefix, "_posterior_means.rds"))
#     
#     
#     
#     ### Plot the Variance in Positions
#     phi_post_tbl <- phi_post %>% as.data.frame %>% as_tibble %>% 
#       rename(QB = V1, RB = V2, SWR = V3, WR = V4, TE = V5, OT = V6, OG = V7, C = V8,
#              DT = V9, DE = V10, OLB = V11, ILB = V12, SCB = V13, CB = V14, S = V15 ) %>% 
#       gather(key = "Position", value = "posteriorDraw")
#     rm(phi_post)
#     phi_post_tbl_med <- phi_post_tbl %>% 
#       group_by(Position) %>% 
#       summarize(median_effect = median(posteriorDraw)) %>%
#       arrange(median_effect)
#     phi_post_summary <- phi_post_tbl %>% 
#       group_by(Position) %>% 
#       summarize(mean_effect = mean(posteriorDraw), 
#                 med_effect = median(posteriorDraw),
#                 var_effect = var(posteriorDraw)) %>% 
#       arrange(med_effect) %>% 
#       mutate(play_type = model_play_type, 
#              run_datetime_key = current_run$run_datetime_key, 
#              season = current_run$season, 
#              season_type = current_run$SEASON_type, 
#              week = current_run$run_ASSOCIATED_WEEK_NUM,
#              Position = fct_relevel(Position, (phi_post_tbl_med$Position))
#       ) %>% 
#       arrange(desc(Position))
#     
#     hfa <- posterior_means[(names(posterior_means) == 'u' )]
#     s2model <- posterior_means[(names(posterior_means) == 's2model' )]
#     beta_prior_var <- c(stanDat$posMat%*%phi_post_summary$mean_effect)^2*stanDat$bpmVar
#     beta_prior_mean <- stanDat$bpmMean
#     
#     y_star <- y - stanDat$home*hfa
#     X_model <- sparseMatrix(i = temp_participation_model$play_index, j = temp_participation_model$player_index, x = temp_participation_model$offense)
#     
#     beta_post_var <- 1/((1/s2model)*diag(t(X_model)%*%X_model) + (1/beta_prior_var))
#     beta_post_mean <- beta_post_var*((1/s2model)*as.vector(t(X_model)%*%y_star) + (1/beta_prior_var)*beta_prior_mean )
#     
#     player_effect <- beta_post_mean
#     player_effect_sd <- sqrt(beta_post_var)
#     
#     # player_effect_sd <- posterior_param_summary[ (rownames(posterior_param_summary) %>% regexec("b\\[", .)) > 0 ,"sd"]
#     player_effect_tbl <- tibble(player_index = 1:max(player_tbl$player_index), 
#                                 PM = (player_effect * sd_epa) + mean_epa, 
#                                 PM_sd = player_effect_sd * sd_epa, 
#                                 PM_var = PM_sd^2,
#                                 PMaR = PM - pm_replacement, 
#                                 PMaR_game = PMaR*plays_game_constant) %>%
#       left_join(player_tbl, by = "player_index") %>%
#       left_join(dplyr::select(teams, -VENUE_id), by = c("espn_team_id" = "team_id")) %>% 
#       arrange(desc(PM))
#     
#     ### add prior values to player_effect_tbl
#     
#     #### evaluate each player on a 0-100 scale overall and for his position:
#     
#     #### add the sqrt(p*s2player) for each position group and use that for the model standard deviation
#     pos_var_effect <- posterior_means[regexec("phi\\[", names(posterior_means)) > 0 | regexec("s2player", names(posterior_means)) > 0]
#     n_Positions <- nlevels(factor(player_tbl$position_group))
#     names(pos_var_effect)[1:n_Positions] <- c("QB", "RB", "SWR", "WR", "TE", "OT", "OG", "C", "DT", "DE", "OLB", "ILB", "SCB", "CB", "S")
#     pos_phi_tbl <- tibble(position = names(pos_var_effect)[1:n_Positions], phi = pos_var_effect[1:n_Positions])
#     
#     
#     
#     
#     player_effect_tbl <- player_effect_tbl %>% 
#       left_join(team_games, by = "espn_team_id") %>% 
#       left_join(dplyr::select(teamsBySeason, team_id, class), by = c("espn_team_id" = "team_id")) %>% 
#       ungroup() %>% 
#       mutate(qualified = (plays >= pmin(team_games*qualifying_plays, qualifying_plays_season) ) & (class == "1A") & (espn_player_id > 0) )
#     
#     temp_pos_moments <- player_effect_tbl %>% 
#       filter(qualified) %>% 
#       group_by(position_group) %>% 
#       summarize(pm_median = median(PM), 
#                 pm_sd = (quantile(PM, pnorm(1.5)) - median(PM))/1.5) %>% 
#       ungroup()
#     
#     player_effect_tbl <- player_effect_tbl %>% 
#       left_join(temp_pos_moments, by = 'position_group') %>% 
#       mutate(Rating_PosAdj = pnorm(PM, pm_median, pm_sd )*100) %>% 
#       group_by(qualified, position_group) %>% 
#       mutate(Position_Rank = if_else(qualified == FALSE, NA_integer_, min_rank(desc(PM))) ) %>% 
#       group_by(qualified) %>% 
#       mutate(Rating_PosAdj_Rank = if_else(qualified == FALSE, NA_integer_, min_rank(desc(Rating_PosAdj)) )
#       ) %>% ungroup %>% 
#       dplyr::select(-pm_median, -pm_sd) %>% 
#       arrange(desc(PMaR)) 
#     
#     player_effect_tbl <- player_effect_tbl %>% 
#       dplyr::select(-contains("bpm"), -ends_with("player")) %>% 
#       left_join(player_bpm_priors %>% 
#                   dplyr::select(espn_player_id, contains("bpm")), by = c("espn_player_id" = "espn_player_id")
#       )
#     
#     #### further adjust scales here so that the median for each position is around 40
#     player_effect_tbl %>% filter(qualified) %>% 
#       arrange(desc(Rating_PosAdj)) %>% group_by(position_group) %>% summarize(max_num = max(Rating_PosAdj), min_num = min(Rating_PosAdj), median_num = median(Rating_PosAdj), mean_num = mean(Rating_PosAdj))
#     
#     
#     
#     #loop through and get each players on/off splits
#     
#     
#     ### order players by teams so we can shuffle through teams (more efficient) then players within each team
#     player_effect_tbl <- player_effect_tbl %>% 
#       arrange(espn_team_id, desc(PM))
#     unique_teams <- player_effect_tbl %>% 
#       dplyr::select(espn_team_id) %>% unique %>% 
#       unlist
#     
#     # write("", file = "epa_splits.txt")### ovewrites 
#     cl <- makeCluster(8, outfile= "epa_splits.txt")
#     registerDoParallel(cl)
#     
#     playersepaSplits <- foreach(tm_i = icount(length(unique_teams)), .combine=rbind, 
#                                 .packages = c("tidyverse")) %dopar% 
#       {
#         
#         temp_tm_plyrs <- player_effect_tbl %>% 
#           filter(espn_team_id == unique_teams[tm_i])
#         
#         tm_plays <- temp_participation_model %>% 
#           filter(espn_team_id == unique_teams[tm_i])
#         
#         temp_pm_output <- NULL
#         
#         for(plyr in 1:nrow(temp_tm_plyrs)){
#           temp_plyr <- temp_tm_plyrs %>% slice(plyr)
#           
#           temp_count_plays <- tm_plays %>% 
#             group_by(espn_schedule_id, espn_team_id, tm) %>% 
#             summarize(plyr_plays = sum(espn_player_id  %in% temp_plyr$espn_player_id ), 
#                       tm_plays = n()/11) %>% ungroup 
#           
#           
#           temp_plyr_plays <- temp_count_plays %>% 
#             filter(plyr_plays > 0) %>% 
#             arrange(espn_schedule_id)
#           
#           n_plyr_games_plyd <- nrow(temp_plyr_plays)
#           
#           if(n_plyr_games_plyd > 0){
#             plyr_tm_plays <- tm_plays %>% 
#               group_by(espn_schedule_id, record_id ) %>% 
#               mutate(ON_OFF = if_else(temp_plyr$espn_player_id %in% espn_player_id, 1, 0)) %>% 
#               filter(row_number(espn_schedule_id) == 1) %>% ungroup
#             
#             n_tm_games_plyd <- nrow(temp_count_plays)
#             
#             plyr_offdef <- if_else(temp_plyr$position_group[1] %in% c("QB", "OG", "OT", "RB", "SWR", "TE", "WR", "C"), "O", "D")
#             
#             plyr_onOff_splits <-  plyr_tm_plays %>% 
#               filter(type == plyr_offdef) %>% 
#               group_by(ON_OFF) %>% 
#               summarize(epa_avg = mean(team_epa),
#                         wpa_avg = mean(team_WPA))
#             
#             temp_pm_output <- temp_plyr %>% 
#               rename(player_id = espn_player_id,
#                      plyr_plays = plays, 
#                      plyr_plays_qualified = qualified,
#                      plus_minus_play = PM, 
#                      plus_minus_play_var = PM_var,
#                      pm_above_rep_play = PMaR, 
#                      pm_above_rep_game = PMaR_game, 
#                      plyr_rank_position = Position_Rank, 
#                      plyr_pos_adj_rank_ovr = Rating_PosAdj_Rank, 
#                      position = position_group) %>% 
#               mutate(run_datetime_key = current_run$run_datetime_key, 
#                      season = current_season,
#                      plyr_gms_played = n_plyr_games_plyd, 
#                      tm_gms_played = n_tm_games_plyd, 
#                      tm_plays = nrow(plyr_tm_plays), 
#                      impact_rating = Rating_PosAdj %>% round(., digits = 4), 
#                      plyr_on_field_epa_avg = filter(plyr_onOff_splits, ON_OFF == 1)$epa_avg %>% ifelse(is_empty(.), NA_real_, .),
#                      plyr_off_field_epa_avg = filter(plyr_onOff_splits, ON_OFF == 0)$epa_avg %>% ifelse(is_empty(.), NA_real_, .),
#                      plyr_on_field_wpa_avg = filter(plyr_onOff_splits, ON_OFF == 1)$wpa_avg %>% ifelse(is_empty(.), NA_real_, .),
#                      plyr_off_field_wpa_avg = filter(plyr_onOff_splits, ON_OFF == 0)$wpa_avg %>% ifelse(is_empty(.), NA_real_, .),
#                      modified_dt = Sys.time(), 
#                      modified_by = "Player Impact Script"
#               ) %>% 
#               dplyr::select(run_datetime_key         ,
#                      season                   ,
#                      player_id                ,
#                      plyr_gms_played          ,
#                      tm_gms_played            ,
#                      plyr_plays               ,
#                      tm_plays                 ,
#                      plyr_plays_qualified     ,
#                      plyr_on_field_epa_avg    ,
#                      plyr_off_field_epa_avg   ,
#                      plyr_on_field_wpa_avg    ,
#                      plyr_off_field_wpa_avg   ,
#                      plus_minus_play          ,
#                      plus_minus_play_var      ,
#                      pm_above_rep_play        ,
#                      pm_above_rep_game        ,
#                      impact_rating            ,
#                      plyr_rank_position       ,
#                      plyr_pos_adj_rank_ovr    ,
#                      modified_dt              ,
#                      modified_by              ,
#                      position                 
#               ) %>% 
#               bind_rows(temp_pm_output, .)
#           }
#         }
#         cat("Team: ", tm_i, " of ", length(unique_teams), "\n")
#         
#         return(temp_pm_output) ## save object off of parallel
#       }
#     
#     stopCluster(cl)
#     
#     unregister <- function() {
#       ### close the backend
#       env <- foreach:::.foreachGlobals
#       rm(list=ls(name=env), pos=env)
#     }
#     unregister()
#     
#     
#     playersepaSplits <- playersepaSplits %>% 
#       left_join(player_effect_tbl %>% 
#                   dplyr::select(espn_player_id, espn_team_id, contains("BPM"), NAME) %>% 
#                   rename(team = NAME),
#                 by = c("player_id" = "espn_player_id")) %>% 
#       rename(team_id = espn_team_id)
#     
#     
#     
#     
#     ###get mean and sd based on players with more than 200 plays at that position
#     qualified_pos_moments <- playersepaSplits %>% 
#       filter(plyr_plays_qualified) %>% 
#       group_by(position) %>% 
#       summarize(pos_pm_mean = mean(plus_minus_play), 
#                 pos_pm_sd = sd(plus_minus_play),
#                 pos_pm_twenty_percentile = quantile(plus_minus_play, pm_quantile),
#                 pos_pm_w_mean =weighted.mean(plus_minus_play, w = plyr_plays)) %>% 
#       ungroup
#     
#     #### adjust plus-minus to be averaged at 0 and 20th percentile replacement to be 0
#     pm_output <- playersepaSplits %>% 
#       rename_all(function(x)tolower(x)) %>% 
#       left_join(qualified_pos_moments, by = "position") %>% 
#       group_by(position) %>% 
#       mutate(pm_above_rep_play = plus_minus_play - pos_pm_twenty_percentile, 
#              plus_minus_play = plus_minus_play - pos_pm_w_mean,
#              pm_above_rep_game = pm_above_rep_play*plays_game_constant,
#              play_type = model_play_type
#       ) %>% 
#       dplyr::select(-starts_with("pos_pm"))
#     
#     #### Unit rankings
#     ### Following Units (separate object for each):
#     #RB (2 RB)
#     #OL (2 OT, 2 OG, 1 C)
#     #WR (2 WR, 1 SWR, 1 TE) i.e. pass-catchers
#     #DL (2 DE 1-2 DT)
#     #LB (2 OLB, 1-2 ILB)
#     #DB (2 CB, 1 SCB, 2 S)
#     
#     
#     #### Offensive Line pm_output
#     ol_starters_team <- pm_output %>% 
#       filter(position %in% c("OT", "OG", "C"), plyr_plays_qualified == TRUE) %>% 
#       group_by(team_id) %>% 
#       mutate(plays_rank = min_rank(desc(plyr_plays)),
#              unit_size = 5
#       ) %>% ungroup()
#     
#     ol_team_ratings <- ol_starters_team %>% group_by(team_id, unit_size) %>% 
#       summarize_at(vars(starts_with("plus_minus"), contains("bpm"), plyr_plays, pm_above_rep_play),sum ) %>% ungroup %>% 
#       mutate(Unit_Rating = pnorm(plus_minus_play, median(plus_minus_play), (1/3)*(quantile(plus_minus_play, pnorm(3)) - median(plus_minus_play)))*100
#       ) %>% 
#       arrange(desc(plus_minus_play))  %>% 
#       mutate(unit = "OL")
#     
#     #### Quarterbacks (so its all in the unit table)
#     qb_starters_team <- pm_output %>% 
#       filter(position %in% c("QB"), plyr_plays_qualified == TRUE) %>% 
#       group_by(team_id) %>% 
#       mutate(plays_rank = min_rank(desc(plyr_plays)),
#              unit_size = 1
#       )  %>% ungroup()
#     qb_team_ratings <- qb_starters_team %>% group_by(team_id, unit_size) %>% 
#       summarize_at(vars(starts_with("plus_minus"), contains("bpm"), plyr_plays, pm_above_rep_play),sum ) %>% ungroup %>% 
#       mutate(Unit_Rating = pnorm(plus_minus_play, median(plus_minus_play), (1/3)*(quantile(plus_minus_play, pnorm(3)) - median(plus_minus_play)))*100
#       ) %>% 
#       arrange(desc(plus_minus_play)) %>% 
#       mutate(unit = "QB")
#     
#     
#     #### Running Backs Line
#     rb_starters_team <- pm_output %>% 
#       filter(position %in% c("RB"), plyr_plays_qualified == TRUE) %>% 
#       group_by(team_id) %>% 
#       mutate(plays_rank = min_rank(desc(plyr_plays)),
#              unit_size = 2
#       )  %>% ungroup
#     
#     
#     rb_team_ratings <- rb_starters_team %>% group_by(team_id, unit_size) %>% 
#       summarize_at(vars(starts_with("plus_minus"), contains("bpm"), plyr_plays, pm_above_rep_play),sum ) %>% ungroup %>% 
#       mutate(Unit_Rating = pnorm(plus_minus_play, median(plus_minus_play), (1/3)*(quantile(plus_minus_play, pnorm(3)) - median(plus_minus_play)))*100
#       ) %>% 
#       arrange(desc(plus_minus_play))  %>% 
#       mutate(unit = "RB")
#     
#     ## Receivers
#     rec_starters_team <- pm_output %>% 
#       filter(position %in% c("WR", "SWR", "TE"), plyr_plays_qualified == TRUE) %>% 
#       group_by(team_id) %>% 
#       mutate(plays_rank = min_rank(desc(plyr_plays)),
#              unit_size = 4
#       ) %>% ungroup
#     
#     
#     
#     rec_team_ratings <- rec_starters_team %>% group_by(team_id, unit_size) %>% 
#       summarize_at(vars(starts_with("plus_minus"), contains("bpm"), plyr_plays, pm_above_rep_play),sum ) %>% ungroup %>% 
#       mutate(Unit_Rating = pnorm(plus_minus_play, median(plus_minus_play), (1/3)*(quantile(plus_minus_play, pnorm(3)) - median(plus_minus_play)))*100
#       ) %>% 
#       arrange(desc(plus_minus_play)) %>% 
#       mutate(unit = "REC")
#     
#     ### Defensive Line
#     dl_starters_team <- pm_output %>% 
#       filter(position %in% c("DT", "DE"), plyr_plays_qualified == TRUE) %>% 
#       group_by(team_id) %>% 
#       mutate(plays_rank = min_rank(desc(plyr_plays)),
#              unit_size = 4
#       ) %>% ungroup
#     
#     
#     
#     dl_team_ratings <- dl_starters_team %>% group_by(team_id, unit_size) %>% 
#       summarize_at(vars(starts_with("plus_minus"), contains("bpm"), plyr_plays, pm_above_rep_play),sum ) %>% ungroup %>% 
#       mutate(Unit_Rating = pnorm(plus_minus_play, median(plus_minus_play), (1/3)*(quantile(plus_minus_play, pnorm(3)) - median(plus_minus_play)))*100
#       ) %>% 
#       arrange(desc(plus_minus_play)) %>% 
#       mutate(unit = "DL")
#     
#     #### Linebackers
#     lb_starters_team <- pm_output %>% 
#       filter(position %in% c("OLB", "ILB"), plyr_plays_qualified == TRUE) %>% 
#       group_by(team_id) %>% 
#       mutate(plays_rank = min_rank(desc(plyr_plays)),
#              unit_size = 4
#       ) %>% ungroup
#     
#     lb_team_ratings <- lb_starters_team %>% group_by(team_id, unit_size) %>% 
#       summarize_at(vars(starts_with("plus_minus"), contains("bpm"), plyr_plays, pm_above_rep_play),sum ) %>% ungroup %>% 
#       mutate(Unit_Rating = pnorm(plus_minus_play, median(plus_minus_play), (1/3)*(quantile(plus_minus_play, pnorm(3)) - median(plus_minus_play)))*100
#       ) %>% 
#       arrange(desc(plus_minus_play)) %>% 
#       mutate(unit = "LB")
#     
#     
#     
#     ### Defensive Backs
#     db_starters_team <- pm_output %>% 
#       filter(position %in% c("SCB", "CB", "S"), plyr_plays_qualified == TRUE) %>% 
#       group_by(team_id) %>% 
#       mutate(plays_rank = min_rank(desc(plyr_plays)),
#              unit_size = 5
#       ) %>% ungroup
#     
#     db_team_ratings <- db_starters_team %>% group_by(team_id, unit_size) %>% 
#       summarize_at(vars(starts_with("plus_minus"), contains("bpm"), plyr_plays, pm_above_rep_play),sum ) %>% ungroup %>% 
#       mutate(Unit_Rating = pnorm(plus_minus_play, median(plus_minus_play), (1/3)*(quantile(plus_minus_play, pnorm(3)) - median(plus_minus_play)))*100
#       ) %>% 
#       arrange(desc(plus_minus_play)) %>% 
#       mutate(unit = "DB")
#     
#     unit_write_table <- bind_rows(qb_team_ratings,
#                                   ol_team_ratings,
#                                   rb_team_ratings,
#                                   rec_team_ratings,
#                                   dl_team_ratings,
#                                   lb_team_ratings,
#                                   db_team_ratings) %>% 
#       mutate(run_datetime_key = current_run$run_datetime_key, 
#              season = current_run$season, 
#              impact_rating = Unit_Rating %>% round(., digits = 4)
#       ) %>% 
#       left_join(team_games, by = c("team_id" = "espn_team_id") ) %>% 
#       rename(tm_plays = team_plays, 
#              tm_gms_played = team_games,
#              unit_plays = plyr_plays,
#              rpm = plus_minus_play, 
#              rpm_var = plus_minus_play_var,
#              par_play = pm_above_rep_play) %>% 
#       dplyr::select(run_datetime_key      ,
#              season                ,
#              team_id               ,
#              unit                  ,
#              unit_size             ,
#              tm_gms_played         ,
#              unit_plays            ,
#              tm_plays              ,
#              contains("bpm")       ,
#              rpm                   ,
#              rpm_var               ,
#              par_play              ,
#              impact_rating         
#       )
#     
#     
#     
#     ### temporary write out results
#     write_rds(pm_output, 
#               paste0("../Data/Runs/player_output_with_prior_", "_", current_run$run_datetime_key, "_", model_play_type, ".rds"))
#     
#     write_rds(unit_write_table, 
#               paste0("../Data/Runs/unit_output_with_prior_","_", current_run$run_datetime_key, "_",model_play_type, ".rds"))
#     
#     rm(list = c("stanDat", "cfb_pm_Fit"))
#     
#     cat("Runtime: ", Sys.time() - start_time , " for play type:", model_play_type, " \n")
#   }
#   
#   ##### overall run vs pass plays and for each team
#   tm_plays <- participation_model_pass %>% 
#     group_by(espn_game_id, unqply_group, record_id, espn_team_id) %>% 
#     slice(1) %>% 
#     bind_rows(.,  participation_model_run %>% 
#                 group_by(espn_game_id, unqply_group, record_id, espn_team_id) %>% 
#                 slice(1)) %>% 
#     group_by(espn_team_id, tm, play_type, offense) %>% 
#     summarize(plays = n(), 
#               epa = sum(team_epa), 
#               weighted_epa = sum(team_epa*if_else(abs(team_WPS - 0.5) >= 0.49 ,0.12 , 1)) ## down-weight garbage time
#     ) %>% 
#     mutate(playtype = if_else(play_type == "P", "pass", "run"),
#            unit = if_else(offense == 1, "Off", "Def")) %>% 
#     ungroup() %>% 
#     dplyr::select(-offense, -play_type) %>% 
#     pivot_wider(names_from = c("playtype"), values_from = c("plays", "epa", "weighted_epa")) %>% 
#     mutate(tm_pass_plays_pct = plays_pass / (plays_pass + plays_run), 
#            tm_pass_epa_pct = epa_pass / (epa_pass + epa_run),
#            tm_pass_wepa_pct = weighted_epa_pass / (weighted_epa_pass + weighted_epa_run)) %>% 
#     group_by(unit) %>% 
#     mutate(all_pass_plays_pct =  sum(plays_pass) / (sum(plays_pass) + sum(plays_run)),
#            all_pass_epa_pct = sum(epa_pass) / (sum(epa_pass) + sum(epa_run)),
#            all_pass_wepa_pct = sum(weighted_epa_pass) / (sum(weighted_epa_pass) + sum(weighted_epa_run))
#     ) %>% 
#     ungroup()
#   
#   fbs_pass_epa_pct <- tm_plays %>% 
#     summarize(fbs_pass_epa_pct = sum(epa_pass) / sum(epa_pass + epa_run) ) %>% 
#     pull(fbs_pass_epa_pct)
#   
#   
#   ### combine run and passes data
#   plyr_pm_pass <- read_rds(  paste0("../Data/Runs/player_output_with_prior_", "_", current_run$run_datetime_key, "_P.rds"))
#   unit_pm_pass <- read_rds(  paste0("../Data/Runs/unit_output_with_prior_","_", current_run$run_datetime_key,  "_P.rds"))
#   # phi_pass <- read_rds(  paste0("../Data/Runs/phi_summary_", current_run$run_datetime_key,  "_P.rds"))
#   plyr_pm_run <- read_rds(  paste0("../Data/Runs/player_output_with_prior_", "_", current_run$run_datetime_key, "_R.rds"))
#   unit_pm_run <- read_rds(  paste0("../Data/Runs/unit_output_with_prior_","_", current_run$run_datetime_key,  "_R.rds"))
#   # phi_run <- read_rds(  paste0("../Data/Runs/phi_summary_","_", current_run$run_datetime_key, "_R.rds"))
#   
#   
#   player_effect_final <- plyr_pm_pass %>% 
#     ungroup() %>% 
#     dplyr::select(-impact_rating, -contains("rank"), -contains("bpm"), -contains("modified"), -play_type, -team_id, -team) %>% 
#     rename_at(vars(contains("plyr"), contains("tm"), contains("_play"), contains("_game"), position), ~paste0("pass_", .)) %>% 
#     full_join(plyr_pm_run %>% 
#                 ungroup() %>% 
#                 dplyr::select(-impact_rating, -contains("rank"), -contains("modified"), -play_type) %>% 
#                 rename_at(vars(contains("plyr"), contains("tm"), contains("_play"), contains("_game"), position), ~paste0("run_", .)),
#               by = c("run_datetime_key", "season", "player_id")) %>% 
#     left_join(dplyr::select(teamsBySeason, team_id, class), by = c("team_id" = "team_id")) %>% 
#     mutate(unit = if_else(pass_position %in% c("C", "OG", "OT", "QB", "RB", "SWR", "TE", "WR") | run_position %in% c("C", "OG", "OT", "QB", "RB", "SWR", "TE", "WR"), "Off", "Def"),
#            tm_gms_played = pmax(pass_tm_gms_played, run_tm_gms_played), 
#            plyr_gms_played = pmax(pass_plyr_gms_played, run_plyr_gms_played),
#            pass_plyr_plays = coalesce(as.numeric(pass_plyr_plays),0),
#            run_plyr_plays = coalesce(as.numeric(run_plyr_plays),0),
#            plyr_plays = pass_plyr_plays + run_plyr_plays,
#            
#            plyr_plays_qualified = (pass_plyr_plays_qualified & run_plyr_plays_qualified) & (class == "1A"),
#     ) %>% 
#     dplyr::select(-pass_tm_gms_played, -run_tm_gms_played, -pass_plyr_gms_played, -run_plyr_gms_played
#     ) %>% 
#     left_join(tm_plays, by = c("team_id" = "espn_team_id", "unit")) %>% 
#     mutate(position = if_else( (tm_pass_plays_pct*pass_plyr_plays) > ((1-tm_pass_plays_pct)*run_plyr_plays), pass_position, run_position)) %>% 
#     group_by(position, plyr_plays_qualified) %>% 
#     mutate(pass_impact_rating = pt(df=25,(pass_plus_minus_play -  mean(pass_plus_minus_play))/sd(pass_plus_minus_play) )*100,
#            run_impact_rating = pt(df=25,(run_plus_minus_play -  mean(run_plus_minus_play))/sd(run_plus_minus_play) )*100,
#            pass_impact_rating = if_else(plyr_plays_qualified, pass_impact_rating, NA_real_),
#            run_impact_rating = if_else(plyr_plays_qualified, run_impact_rating, NA_real_),
#            
#            pass_pm_above_rep = pass_pm_above_rep_play*pass_plyr_plays,
#            run_pm_above_rep = run_pm_above_rep_play*run_plyr_plays,
#     ) %>% 
#     mutate(bpm_mean = fbs_pass_epa_pct*pass_bpm + (1-fbs_pass_epa_pct)*rush_bpm, 
#            bpm_mean_var = fbs_pass_epa_pct*pass_bpm_var + (1-fbs_pass_epa_pct)*rush_bpm_var, 
#            
#            
#            ### PMaR - purely based on a player's individual plays
#            pm_above_rep_play = ((pass_plyr_plays)*coalesce(pass_pm_above_rep_play,0) + (run_plyr_plays)*coalesce(run_pm_above_rep_play,0)) / (plyr_plays), 
#            pm_above_rep_game = 2*plays_game_constant*pm_above_rep_play, 
#            pm_above_rep = pm_above_rep_play*(plyr_plays),
#            
#            ### Weighted PMaR - fbs epa pass rates
#            weighted_pm_above_rep_play = (fbs_pass_epa_pct)*pass_pm_above_rep_play + (1-fbs_pass_epa_pct)*run_pm_above_rep_play, 
#            weighted_pm_above_rep_game = 2*((fbs_pass_epa_pct)*pass_pm_above_rep_game + (1-fbs_pass_epa_pct)*run_pm_above_rep_game), 
#            weighted_pm_above_rep = weighted_pm_above_rep_play*(plyr_plays),
#            
#            rpm = (fbs_pass_epa_pct)*pass_plus_minus_play + (1-fbs_pass_epa_pct)*run_plus_minus_play, 
#            rpm_var = (fbs_pass_epa_pct^2)*pass_plus_minus_play + ((1-fbs_pass_epa_pct)^2)*run_plus_minus_play_var, 
#            
#            
#            ### run/pass player_impact
#            ### playER_IMPACT - fbs epa pass rate
#            impact_rating = pt(df=25,(weighted_pm_above_rep_play -  mean(weighted_pm_above_rep_play))/sd(weighted_pm_above_rep_play) )*100, 
#            impact_rating = if_else(plyr_plays_qualified, impact_rating, NA_real_)
#            
#     ) %>% 
#     rename(pass_rpm     = pass_plus_minus_play,
#            pass_rpm_var = pass_plus_minus_play_var,
#            run_rpm      = run_plus_minus_play,
#            run_rpm_var  = run_plus_minus_play_var ) %>% 
#     ungroup()
#   
#   #### Calculate WAR
#   
#   source("model_war.R")
#   
#   player_effect_final <- player_effect_final %>%
#     left_join(season_results_par %>% 
#                 dplyr::select(team_id, season, home_ind, team_par, sum_par_per_gm_diff, games) %>% 
#                 rename(net_home_games = home_ind, 
#                        tm_net_par_per_game = sum_par_per_gm_diff), 
#               by = c("team_id", "season")) %>% 
#     mutate(tm_adj_wins = calculate_tm_adj_wins(tm_net_par_per_game, net_home_games, tm_gms_played),
#            plyr_war = calculate_war(pm_above_rep, team_par, tm_net_par_per_game, net_home_games, tm_gms_played),
#            plyr_war_pass = (pass_pm_above_rep/pm_above_rep)*plyr_war,
#            plyr_war_run  = (run_pm_above_rep/pm_above_rep)*plyr_war,
#            
#            plyr_war_12_gm = calculate_war_per_12_gms(pm_above_rep, team_par, tm_net_par_per_game, net_home_games, tm_gms_played),
#     )
#   
#   
#   
#   
#   # player_effect_final %>% 
#   #   filter(plyr_plays_qualified, player_id >= 0) %>% 
#   #   ggplot(aes(x = run_impact_rating, y = position,
#   #              fill = position, height = ..density..)) +
#   #   stat_density_ridges(alpha = 0.5, quantile_lines = TRUE, quantiles = 4, show.legend = FALSE) +
#   #   ylab("") + xlab("Penalization Divisor") + xlim(0,100) +
#   #   theme_bw() + theme(text = element_text(size = 20))
#   
#   ### maybe we add the impact ratings
#   
#   ### write out for example:
#   player_effect_final_write <- player_effect_final %>% 
#     ungroup() %>% 
#     mutate(fbs_pass_epa_pct = fbs_pass_epa_pct,
#            season_id = current_run$sdr_season_id, 
#            sdw_season_id = current_run$season_id) %>% 
#     rename(fbs_pass_pct = all_pass_plays_pct, 
#            tm_passes = plays_pass, 
#            tm_runs = plays_run,
#            tm_epa_pass = epa_pass, 
#            tm_epa_run = epa_run) %>% 
#     rename_at(vars(contains("pm_above_rep")), ~gsub("pm_above_rep", "par", .)) %>% 
#     dplyr::select(run_datetime_key, season_id, sdw_season_id , player_id, team_id,  position, #tm,
#            plyr_plays, pass_plyr_plays, run_plyr_plays,
#            plyr_gms_played, tm_gms_played,
#            plyr_plays_qualified,
#            
#            rpm, rpm_var, pass_rpm, pass_rpm_var, run_rpm, run_rpm_var, 
#            
#            par, pass_par, run_par,
#            tm_adj_wins,
#            plyr_war, plyr_war_pass, plyr_war_run, plyr_war_12_gm,
#            
#            par_play,
#            pass_par_play,
#            run_par_play,
#            
#            weighted_par, weighted_par_play,
#            
#            impact_rating, pass_impact_rating, run_impact_rating,
#            
#            bpm_mean, bpm_mean_var, pass_bpm, pass_bpm_var, rush_bpm, rush_bpm_var, 
#            
#            tm_passes, tm_runs,
#            tm_epa_pass, tm_epa_run,
#            fbs_pass_epa_pct ) %>% 
#     arrange(desc(impact_rating)) 
#   
#   
#   
#   ### combine unit_write_table pass and run and war for the units
#   
#   unit_write_pass <- read_rds(paste0("../Data/Runs/unit_output_with_prior_","_", current_run$run_datetime_key, "_P.rds"))
#   unit_write_run <- read_rds(paste0("../Data/Runs/unit_output_with_prior_","_", current_run$run_datetime_key, "_R.rds"))
#   
#   unit_write <- unit_write_pass %>% 
#     dplyr::select(-run_datetime_key, -season, -tm_gms_played, -tm_plays, -contains("rush_bpm") ) %>% 
#     rename_at(vars(-unit,-unit_size, -team_id, -contains("bpm")), ~paste0("pass_", .)) %>%
#     ungroup() %>% 
#     left_join(unit_write_run %>% 
#                 dplyr::select(-contains("pass_bpm"), -starts_with("bpm"), -unit_size, -tm_plays) %>% 
#                 rename_at(vars(-unit, -team_id, -run_datetime_key, -season, -tm_gms_played, -contains("bpm")), ~paste0("run_", .)) %>% 
#                 ungroup() %>% 
#                 rename(run_bpm = rush_bpm, 
#                        run_bpm_var = rush_bpm_var),
#               by = c("team_id" , "unit")) %>% 
#     left_join(player_effect_final %>% 
#                 mutate(unit = case_when(position %in% c("OT", "C", "OG") ~ "OL", 
#                                         position %in% c("WR", "SWR", "TE") ~ "REC",
#                                         position %in% c("CB", "SCB", "S") ~ "DB",
#                                         position %in% c("DE", "DT") ~ "DL",
#                                         position %in% c("OLB", "ILB") ~ "LB", 
#                                         TRUE ~ position)) %>% 
#                 group_by(team_id, unit) %>% 
#                 summarize(tm_adj_wins = median(tm_adj_wins, na.rm = T), 
#                           unit_war = sum(plyr_war, na.rm = T), 
#                           unit_war_12_gm = sum(plyr_war_12_gm, na.rm = T)
#                 ) %>% ungroup() , 
#               by = c("team_id", "unit")) %>% 
#     mutate(OffDef = if_else(unit %in% c("OL", "REC", "RB", "QB"), "Off", "Def")) %>% 
#     left_join(tm_plays %>% 
#                 dplyr::select(-tm) %>% 
#                 rename(tm_pass_epa = epa_pass, 
#                        tm_run_epa = epa_run,
#                        tm_weighted_pass_epa = weighted_epa_pass,
#                        tm_weighted_run_epa = weighted_epa_run,
#                        tm_passes = plays_pass, 
#                        tm_runs = plays_run), by = c("team_id" = "espn_team_id", "OffDef" = "unit")) %>% 
#     mutate(plays = (pass_unit_plays + run_unit_plays), 
#            rpm = (fbs_pass_epa_pct)*pass_rpm + (1-fbs_pass_epa_pct)*run_rpm, 
#            rpm_var = (fbs_pass_epa_pct^2)*pass_rpm_var + ((1-fbs_pass_epa_pct)^2)*run_rpm_var, 
#            
#            pass_par = pass_par_play*pass_unit_plays, 
#            run_par = run_par_play*run_unit_plays, 
#            par = pass_par + run_par,
#            
#            par_play = par / plays,
#            
#            pass_unit_war = (pass_par/par)*unit_war, 
#            run_unit_war = (run_par/par)*unit_war, 
#            
#            weighted_par_play = (fbs_pass_epa_pct)*pass_par_play + (1-fbs_pass_epa_pct)*run_par_play, 
#            weighted_par = weighted_par_play*(plays),
#            
#            ### run/pass player_impact
#            ### playER_IMPACT - fbs epa pass rate
#            impact_rating = pt(df=25,(weighted_par_play -  mean(weighted_par_play))/sd(weighted_par_play) )*100,
#            
#            season_id = current_run$sdr_season_id, 
#            sdw_season_id = current_run$season_id
#     ) %>% 
#     rename_at(vars(contains("pm_above_rep")), ~gsub("pm_above_rep", "par", .)) %>% 
#     dplyr::select(run_datetime_key, season_id, sdw_season_id, team_id, unit, unit_size, tm_gms_played, 
#            plays, pass_unit_plays, run_unit_plays,
#            tm_passes, tm_runs, 
#            
#            rpm, rpm_var, pass_rpm, pass_rpm_var, run_rpm, run_rpm_var,
#            par, pass_par, run_par, 
#            tm_adj_wins,
#            unit_war, pass_unit_war, run_unit_war, unit_war_12_gm,
#            
#            par_play, pass_par_play, run_par_play,
#            weighted_par, weighted_par_play,
#            impact_rating, pass_impact_rating, run_impact_rating, 
#            
#            
#            bpm_mean, bpm_var, pass_bpm, pass_bpm_var, run_bpm, run_bpm_var, 
#            
#            tm_pass_epa, tm_run_epa, all_pass_epa_pct
#     )
#   
#   
#   
#   ### Write final data out 
#   player_effect_final_write %>% 
#     write_rds( paste0("../Data/Runs/pir_write_", "_", current_run$run_datetime_key, ".rds"))
#   unit_write %>% 
#     write_rds( paste0("../Data/Runs/pir_unit_write_", "_", current_run$run_datetime_key, ".rds"))
#   
#   
#   if(current_run$in_season_ind == "N"){
#     player_effect_final_write  %>% 
#       filter(plyr_plays_qualified, player_id > 0) %>% 
#       write_csv(paste0("../Data/",current_run$season ," Player Impact.csv") )
#   }
#   
#   
#   cat("Finished run_datetime: ", current_run$run_datetime_key, " and run number: ", i.run, " \n")
#   
#   


## save off initial cv results of finding the right ridge parameter
# write_rds(model_stat, "../results/ridge_cv_model_stat.rds")
# write_rds(model_param, "../results/ridge_cv_model_param.rds")

# cfb_ridge_cv <- ggplot(model_stat, aes(y = cv_mse, x = week, col = factor(season))) + 
#   geom_point() + 
#   geom_line() + 
#   scale_color_discrete("Season") + 
#   facet_wrap(~k, nrow = 1) + 
#   theme_bw() + 
#   xlab("Week (hold out)") + 
#   ylab("Mean-squared Error") + 
#   ggtitle("Ridge Parameter Cross Validation") +
#   theme(legend.position = 'bottom', 
#         axis.text = element_text(size = 12),
#         axis.title = element_text(size = 12),
#         title = element_text(size = 18)
#         )
# ggsave("../results/cfb_ridge_cv.png", cfb_ridge_cv, width = 8, height = 8)
