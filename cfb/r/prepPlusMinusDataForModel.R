prepPlusMinusDataForModel <- function(participation, player_plays,  team_games, ...) {
  
  
  participation <- participation %>%
    left_join(teamLeague , by = c("espn_team_id" = "team_id")) %>%
    rename(tm = team_name, tm_conf = conf, tm_league_id = league_id, tm_class = class) %>%
    left_join(teamLeague, by = c("opp_id" = "team_id")) %>% 
    rename(opp = team_name, opp_conf = conf, opp_league_id = league_id, opp_class = class) %>% 
    left_join(events %>% 
                select(schedule_id, neutral_site_ind), 
              by = c("espn_schedule_id" = "schedule_id")) %>% 
    mutate(team_site = if_else(neutral_site_ind == "N", if_else(home_away == "Visitor", "V", "H"), "N"), 
           team_site_num = if_else(team_site == "H", 1, if_else(team_site == "V", -1, 0))) %>% 
    select(-home_away, -neutral_site_ind)
  
  
  
  #### Broad Position Categories
  participation <- participation %>% 
    mutate(position_group = if_else(position_category %in% c("DB", "S"), "S", #safeties
                                    if_else(position_category == "CB", if_else(regexpr("SCB", position_sub_category) >0, "SCB", "CB"), #slot corners and corners
                                            if_else(position_category == "LB", if_else(regexpr("ILB", position_sub_category) > 0, "ILB", #LINEBACKERS
                                                                                       if_else(regexpr("MLB", position_sub_category) > 0, "ILB", "OLB")), 
                                                    if_else(position_category == "DL", if_else(regexpr("DE", position_sub_category) > 0, "DE", #DEFENSIVE LINE
                                                                                               if_else(regexpr("NT", position_sub_category) > 0, "DT", "DT")),
                                                            if_else(position_category == "B", "RB", ## runNING BACKS
                                                                    if_else(position_category == "QB", "QB", ## QUARTERBACKS
                                                                            if_else(position_category == "OL", if_else(regexpr("G", position_sub_category) > 0, "OG", "OT"), #OFFENSIVE LINE
                                                                                    if_else(position_category == "C", "C", # OFFENSIVE CENTER
                                                                                            if_else(position_category == "TE", "TE", # TIGHT ENDS
                                                                                                    if_else(position_category == "SWR", "SWR", #SLOT WidE RECEIVER
                                                                                                            if_else(position_category == "WR", "WR", #WidE RECEIVER
                                                                                                                    if_else(position_category == "ST", "ST", #SPECIAL teamS ONLY
                                                                                                                            position_category)
                                                                                                            )
                                                                                                    )
                                                                                            )
                                                                                    )
                                                                            )
                                                                    )
                                                            )
                                                    )
                                            )
                                    )
    )
    )
  
  
  
  
  
  ## if  missing a position then add position listed in player participation
  participation <- participation %>% 
    left_join(players_by_season %>% filter(season == current_season) %>% 
                select(player_id, position_name),
              by = c("player_id")) %>%
    mutate(position_group = coalesce(position_group, position_name)) %>% 
    select(-position_name)
  
  
  
  #### If still only a general category or unit -- replace position group with most common of that unit (only 48 examples in 2018)
  participation <- participation %>% 
    mutate(position_group = case_when(position_group =="DB" ~ "S",
                                      position_group == "LB" ~ "OLB", 
                                      position_group == "DL" ~ "DT", 
                                      position_group %in% c("HB", "FB", "RB") ~ "RB",
                                      position_group == "OL" ~ "OG", 
                                      TRUE ~ position_group)
    )
  
  
  
  ## With no special teams or if missing a position then make it "TE"
  participation <- participation %>% 
    mutate(position_group = coalesce(position_group, "TE"))
  
  ###update current roster information to categories -- these won't always match participation data but correlation matrices can then be created
  sis_rosters <- players_by_season %>% 
    mutate(position_group_roster = case_when(position_name %in% c("C", "G", "OL", "T", "LS") ~ "OL", 
                                             position_name %in% c("DB", "S", "CB") ~ "DB", 
                                             position_name %in% c("DE", "DT", "DL", "NT") ~ "DL", 
                                             position_name %in% c("ATH", "FB", "RB") ~ "RB", 
                                             TRUE ~ position_name))
  
  
  position_priors <- participation %>% 
    left_join(select(sis_rosters, espn_player_id, position_group_roster), by = c("espn_player_id")) %>% 
    group_by(position_group_roster, position_group) %>% 
    summarize(count = n()) %>% 
    group_by(position_group_roster) %>% 
    mutate(fraction = count / sum(count),
           prior = fraction*prior_plays) %>% ungroup
  
  ## Deducing the Play type (Run, Pass, Kickoff, Punt, FG, Extra Point)
  
  ### When a change of possession there seems to be duplicates rows of each player. Some players are missing
  # where unqply_group = 910
  # and schedule_id = '20180901NCAAFOKLAHOMA--0'
  
  participation <- participation %>%
    group_by(espn_schedule_id, unqply_group) %>% 
    mutate(offense = if_else(type == "O", 1, -1)
    ) %>%  ungroup
  ############################################################################
  #### Derive everything I need to know on an OFFENSIVE play
  
  
  
  ########### MODEL FITTING
  participation_model <- participation %>%
    group_by(espn_schedule_id, unqply_group) %>% 
    mutate(pass = as.integer(play_type == "P") , 
           run = as.integer(play_type == "R")
    ) %>% ungroup
  
  #88/11008 games in our database have a winner with a negative net-epa
  
  
  ## filter out any play with not exactly 22 players
  participation_model <- participation_model %>% 
    group_by(espn_schedule_id, record_id) %>% 
    filter(n() == 22) %>% ungroup
  
  ## and exactly 11 players on each team in each play
  ## filter out any play with not exactly 22 players
  participation_model <- participation_model %>% 
    group_by(espn_schedule_id, record_id, espn_team_id, type) %>% 
    filter(n() == 11) %>% ungroup
  
  ### replace turnover epa values with modeled regressed epa turnover values
  participation_model <- participation_model %>% 
    left_join(interception_epa_adj, by = c("espn_game_id", "event_id" = "unqply_group", "record_id") ) %>% 
    left_join(rush_fumble_epa_adj, by = c("espn_game_id", "event_id" = "unqply_group", "record_id") ) %>% 
    left_join(pass_fumble_epa_adj, by = c("espn_game_id", "event_id" = "unqply_group", "record_id") ) %>%
    mutate(team_epa = case_when(!is.na(int_epa_reg) ~ int_epa_reg, 
                                !is.na(fumble_rush_epa_reg) ~ fumble_rush_epa_reg, 
                                !is.na(fumble_pass_epa_reg) ~ fumble_pass_epa_reg, 
                                TRUE ~ team_epa )) %>% 
    select(-int_epa_reg, -fumble_rush_epa_reg, -fumble_pass_epa_reg)
  
  ### filter out plays without a corresponding epa listed
  participation_model <- participation_model %>% 
    filter(!is.na(team_epa))
  

  ###avg plays per game
  avg_poss <- participation_model %>% group_by(espn_game_id) %>% 
    summarize(plays = n()/22) %>% 
    summarize(avg_plays = mean(plays)/2)#average offensive plays per team
  
  
  ###get unique players w/ their most played position
  player_tbl <- participation_model %>%
    group_by(espn_player_id, espn_team_id, position_group) %>%
    summarize(plays_position = n(), mean_epa_pos = mean(team_epa)) %>%
    group_by(espn_player_id) %>%
    arrange(desc(plays_position)) %>% 
    mutate(plays = sum(plays_position)) %>%  
    ungroup() %>% 
    arrange(espn_player_id, desc(plays_position)) 
  
  ### add testing cv data to player_tbl
  
  player_tbl <- player_plays %>% 
    select(temp_espn_player_id, espn_team_id) %>% 
  anti_join( player_tbl, by = c("temp_espn_player_id" = "espn_player_id")) %>% 
  rename(espn_player_id = temp_espn_player_id) %>% 
  mutate(position_group = if_else(substr(espn_player_id,1,1) == 2, "OLB", "TE"), 
         plays_position = 0, 
         mean_epa_pos = NA_real_, 
         plays = 0) %>% 
    bind_rows(player_tbl, .) %>% 
    arrange(espn_player_id)
    
  

  ### add in recruiting grade
  ### get rid of recruiting grades below the cutoff (these aren't real grades)
  ### then only take most recent recruit with sdr mapping
  cfb_recruiting_rm_duplicates <- cfb_recruiting %>% 
    filter(grade >= recruiting_grade_cutoff) %>% 
    arrange(person_id, desc(year), recruiting_id) %>% 
    group_by(person_id) %>% 
    filter(row_number(person_id) == 1) %>% 
    ungroup %>% 
    select(person_id, grade) %>% 
    rename(recruiting_grade = grade)
  
  
  player_tbl <- player_tbl %>% 
    left_join(cfb_recruiting_rm_duplicates, by = c("espn_player_id" = "person_id")) %>% 
    mutate(grade_use = if_else(!is.na(recruiting_grade) & recruiting_grade >= recruiting_grade_cutoff , 1, 0),
           recruiting_grade = recruiting_grade %>% coalesce(0), 
           QB_ind = if_else(position_group  == "QB", 1, 0))
  
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
  #   mutate(plays_position = coalesce(plays_position, as.double(0)) ) %>%
  #   group_by(espn_player_id, position_group) %>%
  #   summarize(prior = sum(prior),
  #             plays_position = sum(plays_position)) %>%
  #   ungroup %>%
  #   mutate(post_plays = prior + plays_position) %>%
  #   group_by(espn_player_id) %>%
  #   mutate(post_frac = post_plays / sum(post_plays)) %>% ungroup
  # 
  
  #### add in roster position for each player and get a posterior plays_position_pct
  player_pos_pct <- player_tbl %>% 
    select(espn_player_id, position_group, plays_position) %>% 
    inner_join(position_priors, by = c("position_group")) %>% 
    mutate(plays_position = coalesce(plays_position, as.double(0)) ) %>% 
    group_by(espn_player_id, position_group) %>% 
    summarize(prior = sum(prior), 
              plays_position = sum(plays_position)) %>% 
    ungroup %>% 
    mutate(post_plays = prior + plays_position) %>% 
    group_by(espn_player_id) %>% 
    mutate(post_frac = post_plays / sum(post_plays)) %>% ungroup
  
  ### save one line per player 
  player_tbl <- player_tbl %>%
    group_by(espn_player_id) %>% 
    slice(1) %>% ungroup %>% 
    arrange(espn_player_id) %>% 
    unique %>% 
    mutate(player_index = row_number(espn_player_id)) %>% ungroup
  
  
  player_pos_pct <- player_pos_pct %>% 
    left_join(player_tbl %>% select(espn_player_id, player_index), by = c("espn_player_id")) %>% 
    ungroup %>% 
    arrange(player_index, position_group)
  
  
  
  
  if(exists("cur_group_id")){
    participation_model <- participation_model %>% 
      ungroup %>% 
      arrange(espn_schedule_id, unqply_group, record_id, espn_team_id, espn_player_id) %>%
      group_by(record_id) %>% 
      mutate(play_index = cur_group_id()) %>%
      ungroup() %>% 
      arrange(play_index, espn_team_id, espn_player_id)
  }else if(exists("group_indices")){
    participation_model <- participation_model %>% 
      ungroup %>% 
      arrange(espn_schedule_id, unqply_group, record_id, espn_team_id, espn_player_id) %>%
      group_by(record_id) %>% 
      mutate(play_index = group_indices()) %>%
      ungroup() %>% 
      arrange(play_index, espn_team_id, espn_player_id)
    
  }
  
  
  
  #add player_index to participation_model
  participation_model <- participation_model %>% 
    left_join(select(player_tbl, espn_player_id, player_index), by = "espn_player_id")
  
  Nplays <- max(participation_model$play_index)
  
  
  # Non clutch-weighted epa
  play_epa <- participation_model %>% arrange(play_index) %>%
    select(play_index, team_epa) %>% unique %>% select(team_epa) %>% unlist
  
  
  ### home/away/neutral each play
  play_home_ind <- participation_model %>% filter(offense == 1) %>% 
    arrange(play_index) %>%
    select(play_index, team_site_num) %>% unique %>% select(team_site_num) %>% unlist
  
  ## play type on each play
  play_type <- participation_model %>% 
    arrange(play_index, play_type) %>% 
    select(play_index, play_type) %>% unique %>% 
    select(play_type) %>% unlist
  play_type %>% unique %>% as.factor
  
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
  
  ### Add Box Plus-Minus (BPM) to player table
  ### add BPM priors to player_tbl
  player_tbl <- player_tbl %>% 
    left_join(player_bpm_priors %>% 
                mutate(bpm_mean = (rush_bpm*run_plays + pass_bpm*pass_plays) / (pass_plays + run_plays),
                       bpm_var = (rush_bpm_var*run_plays + pass_bpm_var*pass_plays) / (pass_plays + run_plays)
                ) %>% 
                select(espn_player_id, espn_team_id, bpm_mean, bpm_var), 
              by = c("espn_player_id" = "espn_player_id", "espn_team_id" = "espn_team_id")
    ) %>% 
    group_by(position_group) %>% 
    mutate(bpm_mean = coalesce(bpm_mean, quantile(bpm_mean, 0.2, na.rm = T)), 
           bpm_var = coalesce(bpm_var, quantile(bpm_var, 0.2, na.rm = T)) 
    ) %>% ungroup()
  
  
  ## To help in prior selection, look at variance of positional epa's the adjust based on prior belief
  # player_tbl %>% filter(plays >= 100) %>% 
  #   group_by(position_group) %>% 
  #   summarize(avg_epa_pos = weighted.mean(bpm_mean, w = plays), se_mean_epa_pos = weighted.mean(bpm_var, w = plays)) %>% 
  #   mutate(empirical_a = (avg_epa_pos^2) / (se_mean_epa_pos^2), 
  #          empirical_b =  avg_epa_pos    / (se_mean_epa_pos^2))
  
  
  mean_epa <- mean(play_epa)
  sd_epa <- sd(play_epa)
  y <- (play_epa - mean_epa)/sd_epa
  
  season_players <- player_plays$temp_espn_player_id %>% unique() %>% length()
  
  X_model <- sparseMatrix(i = participation_model$play_index, j = participation_model$player_index, x = participation_model$offense, 
                          giveCsparse = TRUE, 
                          dims = c(length(y), season_players))
  
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
  unique_player_teams <- team_games$temp_espn_team_id %>% unique()
  team_ind_match <- match(participation_model$espn_team_id, unique_player_teams)
  
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
  
  
  x <- cbind( play_home_ind, X_model, team_off_ind_mat, team_def_ind_mat) 
  diag_ridge <- sparseMatrix(i = 1:ncol(x), j = 1:ncol(x), x = 1, giveCsparse = TRUE) 
  
  
  
  ### filter out data from this week and create test/training datasets
  
  ### stan data object
  
  stanDat <- list(PlayerID = as.integer(player_tbl$player_index),
                  playType = as.integer(as_factor(play_type)),
                  pass = if_else(play_type == "P", 1, 0),
                  
                  
                  positions = as.integer(player_pos),
                  home = as.integer(play_home_ind), 
                  
                  y = y, ### takes the centered at 0 epa for hopefully easier estimation
                  N = Nplays,
                  pmReplace = pm_replacement,
                  nPlayers = max(player_tbl$player_index),
                  nPlayType = nlevels(factor(play_type)),
                  nPosition = nlevels(factor(player_tbl$position_group)),
                  nPlayerPlays = length(sparse_parts$w),
                  
                  recGrade = player_tbl$recruiting_grade, 
                  recGradeUse = player_tbl$grade_use, ### indicator whether or not to use the grade
                  QBind = player_tbl$QB_ind, ### indicator if the player is listed as a QB
                  
                  bpmMean = player_tbl$bpm_mean,
                  bpmVar = player_tbl$bpm_var,
                  bpmSD = sqrt(player_tbl$bpm_var),
                  
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