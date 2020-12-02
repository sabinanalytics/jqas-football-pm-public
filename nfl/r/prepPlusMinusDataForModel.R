prepPlusMinusDataForModel <- function(participation, player_plays,  team_games,...) {
  
  
  participation <- participation %>%
    left_join(events %>% 
                select(schedule_id, tm_id, tm_site), 
              by = c("schedule_id", "team_id" = "tm_id")) %>% 
    group_by(event_id) %>% 
    fill(tm_site, .direction = "downup") %>% ## fill in missing tm_site information (home/away/neutral)
    ungroup()
  
  
  participation <- participation %>%
    mutate(team_site_num = if_else(str_to_lower(tm_site) == "home", 1, if_else( str_to_lower(tm_site) == "road", -1, 0)))
  

  
  ## if  missing a position then add position listed in player participation
  participation <- participation %>% 
    left_join(players_this_season %>% filter(year %in% current_season) %>% 
                select(person_id, position_group) %>% 
                rename(position_name = position_group),
              by = c("person_id")) %>%
    mutate(position_group = coalesce(position_group, position_name)) %>% 
    select(-position_name)
  
  
  
  ## With no special teams or if missing a position then make it "TE"
  participation <- participation %>% 
    mutate(position_group = coalesce(position_group, "TE"))
  
  ###update current roster information to categories -- these won't always match participation data but correlation matrices can then be created
  season_rosters <- players_this_season %>% 
    filter(year %in% current_season)
  
  
  ############################################################################
  
  ########### MODEL FITTING
  participation_model <- participation %>%
    group_by(schedule_id, playcnt) %>% 
    mutate(pass = as.integer(str_to_lower(play_type) == "pass") , 
           run = as.integer(str_to_lower(play_type) == "run")
    ) %>% ungroup
  
  #88/11008 games in our database have a winner with a negative net-epa
  
  
  ## filter out any play with not exactly 22 players
  participation_model <- participation_model %>% 
    filter(!is.na(epa)) %>% 
    group_by(schedule_id, playcnt, offense) %>% 
    filter(n() == 11) %>% 
    group_by(schedule_id, playcnt) %>% 
    filter(n() == 22) %>% 
    ungroup 
  
  
  ### replace turnover epa values with modeled regressed epa turnover values
  participation_model <- participation_model %>% 
    left_join(interception_epa_adj %>% select(espnEventId, playId, contains("epa")), by = c("event_id" = "espnEventId", "playcnt" = "playId") ) %>% 
    left_join(rush_fumble_epa_adj %>% select(espnEventId, playId, contains("epa")), by = c("event_id" = "espnEventId", "playcnt" = "playId") ) %>% 
    left_join(pass_fumble_epa_adj %>% select(espnEventId, playId, contains("epa")), by = c("event_id" = "espnEventId", "playcnt" = "playId") ) %>%
    mutate(epa = case_when(!is.na(int_epa_reg) ~ int_epa_reg, 
                                !is.na(fumble_rush_epa_reg) ~ fumble_rush_epa_reg, 
                                !is.na(fumble_pass_epa_reg) ~ fumble_pass_epa_reg, 
                                TRUE ~ epa )) %>% 
    select(-int_epa_reg, -fumble_rush_epa_reg, -fumble_pass_epa_reg)
  
  ### filter out plays without a corresponding epa listed
  participation_model <- participation_model %>% 
    filter(!is.na(epa))
  

  ###avg plays per game
  avg_poss <- participation_model %>% group_by(event_id) %>% 
    summarize(plays = n()/22) %>% 
    summarize(avg_plays = mean(plays)/2)#average offensive plays per team
  
  
  ###get unique players w/ all of their played positions
  player_tbl <- participation_model %>%
    group_by(team_id, person_id, position_group) %>%
    summarize(plays_position = n(), mean_epa_pos = mean(epa)) %>%
    group_by(team_id, person_id) %>%
    arrange(desc(plays_position)) %>% 
    mutate(plays = sum(plays_position)) %>%  
    ungroup() %>% 
    arrange(person_id, desc(plays_position)) 
  
  ### add testing cv data to player_tbl
  
  player_tbl <- player_plays %>% 
    select(temp_person_id, team_id) %>% 
  anti_join( player_tbl, by = c("temp_person_id" = "person_id", "team_id")) %>% 
  rename(person_id = temp_person_id) %>% 
  mutate(position_group = NA_character_, 
         plays_position = 0, 
         mean_epa_pos = NA_real_, 
         plays = 0) %>% 
    bind_rows(player_tbl, .) %>% 
    arrange(person_id)
    
  
  ### list all percentages a player has played a position -- plus priors on plays in each position
  #### add in roster position for each player and get a posterior plays_position_pct
  # player_pos_pct <- sis_rosters %>% select(espn_player_id, position_group_roster) %>%
  #   left_join(player_plays %>% select(espn_player_id, temp_espn_player_id),
  #             by = "espn_player_id") %>%
  #   mutate(espn_player_id = temp_espn_player_id) %>%
  #   filter(!is.na(espn_player_id)) %>%
  #   select(-temp_espn_player_id) %>%
  #   group_by(espn_player_id, position_group_roster) %>% unique %>% ungroup %>%
  #   inner_join(position_priors, by = c("position_group_roster")) %>%
  #   left_join(player_tbl %>% select(espn_player_id, position_group, plays_position),
  #             by = c("espn_player_id", "position_group")) %>%
  #   mutate(plays_position = coalesce(plays_position, as.integer(0)) ) %>%
  #   group_by(espn_player_id, position_group) %>%
  #   summarize(prior = sum(prior),
  #             plays_position = sum(plays_position)) %>%
  #   ungroup %>%
  #   mutate(post_plays = prior + plays_position) %>%
  #   group_by(espn_player_id) %>%
  #   mutate(post_frac = post_plays / sum(post_plays)) %>% ungroup
  # 
  
  #### add in roster position for each player and get a posterior plays_position_pct
  
  
  ### save one line per player 
  player_tbl <- player_tbl %>%
    group_by(person_id) %>% 
    slice(1) %>% ungroup %>% 
    arrange(person_id) %>% 
    unique %>% 
    mutate(player_index = row_number(person_id)) %>% ungroup
  
  ##get players all-time position preference based on all seasons in span (helps if someone only appears in test and not training or vice-versa)
  ## and replace missing positions from player table due to this issue
  
  players_top_position_all_seasons <- players_by_season %>% 
    group_by(person_id, position_group) %>% 
    summarize(plays = sum(plays)) %>% 
    group_by(person_id) %>% 
    filter(plays == max(plays)) %>% 
    ungroup()
  
  player_tbl <- player_tbl %>% 
    left_join(players_top_position_all_seasons %>% 
                select(person_id, position_group) %>% 
                rename(top_position = position_group), 
              by = "person_id") %>% 
    mutate(position_group = coalesce(position_group, top_position)) 
  
  
  player_pos_pct <- player_tbl %>% 
    left_join(players_this_season %>% 
                select(person_id, position_group) %>%
                rename(roster_pos = position_group), 
              by = c("person_id")) %>% 
    select(person_id, position_group, plays_position, roster_pos) %>% 
    left_join(players_top_position_all_seasons %>% 
                select(person_id, position_group) %>% 
                rename(top_position = position_group), 
              by = "person_id") %>% 
    mutate(position_group = coalesce(position_group, top_position)) %>% 
    inner_join(position_priors, by = c("roster_pos" = "main_position", "position_group")) %>% 
    mutate(plays_position = coalesce(plays_position, as.double(0)) ) %>% 
    mutate(post_plays = prior + plays_position) %>% 
    group_by(person_id) %>% 
    mutate(post_frac = post_plays / sum(post_plays)) %>% 
    ungroup
  
  player_pos_pct <- player_pos_pct %>% 
    left_join(player_tbl %>% select(person_id, player_index), by = c("person_id")) %>% 
    ungroup %>% 
    arrange(player_index, position_group)
  
  if(exists("cur_group_id")){
    participation_model <- participation_model %>% ungroup %>% 
      arrange(schedule_id, playcnt) %>%
      group_by(schedule_id, playcnt) %>% 
      mutate(play_index = cur_group_id()) %>%
      ungroup() %>% 
      arrange(play_index, team_id, person_id)
  }else if(exists("group_indices")){
    participation_model <- participation_model %>% ungroup %>% 
      arrange(schedule_id, playcnt) %>%
      group_by(schedule_id, playcnt) %>% 
      mutate(play_index = group_indices()) %>%
      ungroup() %>% 
      arrange(play_index, team_id, person_id)
  }

  
  #add player_index to participation_model
  participation_model <- participation_model %>% 
    left_join(select(player_tbl, person_id, player_index), by = "person_id")
  
  Nplays <- max(participation_model$play_index)
  
  
  # Non clutch-weighted epa
  play_epa <- participation_model %>% arrange(play_index) %>%
    select(play_index, epa) %>% unique %>% select(epa) %>% unlist
  
  
  ### home/away/neutral each play
  play_home_ind <- participation_model %>% filter(offense == 1) %>% 
    arrange(play_index) %>%
    group_by(play_index) %>% 
    slice(1) %>% 
    ungroup() %>% 
    pull(team_site_num)
  
  ## play type on each play
  play_type <- participation_model %>% 
    arrange(play_index, play_type) %>% 
    select(play_index, play_type) %>% unique %>% 
    select(play_type) %>% unlist
  play_type %>% unique %>% as.factor
  
  play_pass <- participation_model %>% 
    arrange(play_index, play_type) %>% 
    select(play_index, pass) %>% unique %>% 
    pull(pass)
  ### look up the factor levels and which are which
  player_pos <- factor(player_tbl$position_group) %>% 
    fct_relevel(c("QB", "RB", "SWR", "WR", "TE", "OT", "OG", "C", 
                  "DT", "DE", "OLB", "ILB", "SCB", "CB", "S"))
  
  player_pos_pct_mat <- player_pos_pct %>% 
    select(player_index, position_group, post_frac) %>% 
    spread(key = position_group, value = post_frac, fill = 0) %>% 
    select(QB, RB, SWR, WR, TE, OT, OG, C, 
           DT, DE, OLB, ILB, SCB, CB, S)
  
  
  ## will need to rearrange columns to match the order of the factors
  # 
  # ### Add Box Plus-Minus (BPM) to player table
  # ### add BPM priors to player_tbl
  # player_tbl <- player_tbl %>% 
  #   left_join(player_bpm_priors %>% 
  #               mutate(bpm_mean = (rush_bpm*run_plays + pass_bpm*pass_plays) / (pass_plays + run_plays),
  #                      bpm_var = (rush_bpm_var*run_plays + pass_bpm_var*pass_plays) / (pass_plays + run_plays)
  #               ) %>% 
  #               select(espn_player_id, espn_team_id, bpm_mean, bpm_var), 
  #             by = c("espn_player_id" = "espn_player_id", "espn_team_id" = "espn_team_id")
  #   ) %>% 
  #   group_by(position_group) %>% 
  #   mutate(bpm_mean = coalesce(bpm_mean, quantile(bpm_mean, 0.2, na.rm = T)), 
  #          bpm_var = coalesce(bpm_var, quantile(bpm_var, 0.2, na.rm = T)) 
  #   ) %>% ungroup()
  
  
  ## To help in prior selection, look at variance of positional epa's the adjust based on prior belief
  # player_tbl %>% filter(plays >= 100) %>% 
  #   group_by(position_group) %>% 
  #   summarize(avg_epa_pos = weighted.mean(bpm_mean, w = plays), se_mean_epa_pos = weighted.mean(bpm_var, w = plays)) %>% 
  #   mutate(empirical_a = (avg_epa_pos^2) / (se_mean_epa_pos^2), 
  #          empirical_b =  avg_epa_pos    / (se_mean_epa_pos^2))
  
  
  mean_epa <- mean(play_epa)
  sd_epa <- sd(play_epa)
  y <- (play_epa - mean_epa)/sd_epa
  
  season_player_length <- player_plays$temp_person_id %>% unique() %>% length()
  
  X_model <- sparseMatrix(i = participation_model$play_index, j = participation_model$player_index, x = participation_model$offense, 
                          giveCsparse = TRUE, 
                          dims = c(length(y), season_player_length))
  
  ### weight X matrix based on garbage time
  # ## weights based on the win probability at the start of the play -- if game greater than 99% for a team then it gets 12% weight
  # weights <- participation_model %>% group_by(play_index) %>%
  #   slice(1) %>%
  #   mutate(garbage_time_weight = if_else(team_WPS >= 0.015 & team_WPS <= 0.985, 1, 0.12)
  #   ) %>%
  #   pull(garbage_time_weight)
  # 
  # X_model <- (1/weights^2) * X_model
  # #weight y response based on garbage time
  # y <- (1/weights^2) * ((play_epa - mean_epa)/sd_epa)
  
  sparse_parts <- extract_sparse_parts(X_model)
  
  
  ### weight of each position to soft sum constraint variances to 1
  position_weights <- player_pos_pct %>% 
    group_by(position_group) %>% 
    summarize(
      count = sum(plays_position)
    ) %>% 
    mutate(weight = count / sum(count),
           position_group = fct_relevel(as.factor(position_group), levels(player_pos))) %>% 
    arrange(position_group)
  
  
  ## designate offensive or defensive player in player_tbl
  player_tbl <- player_tbl %>% 
    mutate(Off = if_else(position_group %in% c("C", "OG", "OT", "QB", "RB", "SWR", "TE", "WR"), 1, 0), 
           Def = if_else(Off == 1, 0, 1))
  
  ## sparse matrix for team offense, defensive effects
  unique_player_teams <- team_games$team_id %>% unique()
  team_ind_match <- match(participation_model$team_id, unique_player_teams)
  
  team_off_ind_mat <- sparseMatrix(i = participation_model$play_index[participation_model$offense == 1], 
                                   j = team_ind_match[participation_model$offense == 1], 
                                   x = 1, 
                                   dims = c(length(y), length(unique_player_teams))
                                   )
  team_def_ind_mat <- sparseMatrix(i = participation_model$play_index[participation_model$offense == -1], 
                                   j = team_ind_match[participation_model$offense == -1], 
                                   x = -1, 
                                   dims = c(length(y), length(unique_player_teams))
                                   )
  ## just player effects for now for direct comparison
  x <- cbind( play_home_ind, play_pass, X_model)
  # x <- cbind( play_home_ind, play_pass,X_model, team_off_ind_mat, team_def_ind_mat) 
  diag_ridge <- sparseMatrix(i = 1:ncol(x), j = 1:ncol(x), x = 1, giveCsparse = TRUE) 
  
  
  
  ### filter out data from this week and create test/training datasets
  
  ### stan data object
  
  stanDat <- list(PlayerID = as.integer(player_tbl$player_index),
                  playType = as.integer(as_factor(play_type)),
                  pass = play_pass,
                  
                  
                  positions = as.integer(player_pos),
                  home = as.integer(play_home_ind), 
                  
                  y = y, ### takes the centered at 0 epa for hopefully easier estimation
                  N = Nplays,
                  pmReplace = pm_replacement,
                  nPlayers = max(player_tbl$player_index),
                  nPlayType = nlevels(factor(play_type)),
                  nPosition = nlevels(factor(player_tbl$position_group)),
                  nPlayerPlays = length(sparse_parts$w),
                  
                  # recGrade = player_tbl$recruiting_grade, 
                  # recGradeUse = player_tbl$grade_use, ### indicator whether or not to use the grade
                  # QBind = player_tbl$QB_ind, ### indicator if the player is listed as a QB
                  # 
                  # bpmMean = player_tbl$bpm_mean,
                  # bpmVar = player_tbl$bpm_var,
                  # bpmSD = sqrt(player_tbl$bpm_var),
                  # 
                  posMat = player_pos_pct_mat  %>% as.matrix,
                  
                  #weight player plays to have mean 0 player effects
                  playerPlayWeights = player_tbl$plays,
                  
                  posWeights = position_weights$weight,
                  sdSumWeights = 0.0001,
                  # plysLastYr = player_tbl$plys_last_yr, 
                  # pmLastYr = player_tbl$pm_last_yr,
                  # lastYrWeight = player_tbl$lastYrWeight,
                  ### should be equivalent to making last year worth 50 plays for a player who played 1000 snaps last season
                  
                  wX = sparse_parts$w, ## values of sparse matrix
                  vX = sparse_parts$v, # column indicators of sparse matrix
                  uX = sparse_parts$u #row indicators of sparse matrix
  )

  xtx <- as.matrix(t(x)%*%x)
  xty <- as.matrix(t(x)%*%y)
  
  
  
  return_list <- list("stanDat" = stanDat, 
                      "y" = y, 
                      "x" = x, 
                      "xtx" = xtx, 
                      "xty" = xty, 
                      "diag_ridge" = diag_ridge,
                      "mean_epa" = mean_epa,
                      "sd_epa" = sd_epa, 
                      "x_i" = X_model@i,
                      "x_p" = X_model@p,
                      "x_x" = X_model@x,
                      "x_dims" = X_model@Dim
                      
  )
  
  return(return_list)

}