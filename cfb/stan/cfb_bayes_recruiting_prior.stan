
  data {
  int<lower=1> N; //number of data points
  real y[N]; //response variable, y is a set of integers
  real pmReplace;//replacement level plus-minus for all players
  
  //int<lower=0, upper = 1> pass[N]; //variable denoting if play was a pass  then 1 for offense else 0
  //int<lower=0, upper = 1> run[N]; //variable denoting if play was a run  then 1 for offense else 0
  vector[N] home; // variable denoting home/away/neutral (-1 for away, 0 for neutral, 1 for home)
  vector[N] pass; // variable denoting if it was a pass play

  int<lower=1> nPlayers; //number of Players
  int<lower=1, upper=nPlayers> PlayerID[nPlayers]; //player id
  
  int<lower=1> nPlayType; //number of different play types
  int<lower=1, upper=nPlayType> playType[N]; //variable denoting what type of play it was
  
  
  int<lower=1> nPosition; //number of unique position groups
  int<lower=1, upper=nPosition> positions[nPlayers]; //the main position for each player
  
  int<lower=1> nPlayerPlays; //total number of plays by all players (should be about nPlayers*N)
  
  vector[nPlayers] recGrade     ; //recruiting grade -1 is missing -2 is less than cutofff-- only above 50 is a real grade
  vector[nPlayers] recGradeUse  ; // grade is useable (greater than 60 and not missing)
  vector[nPlayers] QBind        ; // Whether or not player is listed as a QB
  // vector[nPlayers] plysLastYr ; //Plays from the player last year
  // vector[nPlayers] pmLastYr   ; //plus minus per play for the player last year
  // vector[nPlayers] lastYrWeight; //variable to decrease variance/sd based on how many plays they played last year
  
  vector<lower = 0, upper=1>[nPosition] posWeights; // weights for each position to help with the SOFT sum constraint to 1 of p
  real sdSumWeights; //standard deviation of sum of weights 

  vector<lower = 0> [nPlayers] playerPlayWeights; // weighting the player effects to soft constraint sum 0

  
  vector [nPlayerPlays] wX; // values of sparse design matrix
  int <lower = 1> vX[nPlayerPlays]; // column indicators of sparse matrix
  int <lower = 1> uX[N+1]; // row indicators of sparse matrix
  
  matrix<lower =0, upper = 1>[nPlayers, nPosition] posMat;//Matrix of fraction of time each player spends at each position
  
  }
  
  
  
  transformed data {
  
  }
  
  parameters {
  vector[nPlayers] b; //player intercepts
  
  real u; // effect for home field on a per play basis 
  real g; // effects for a pass play each play type (can't have intercept or else model may not be identifiable)
  
  vector<lower=0>[nPosition] phi; // multiplicative adjustment to variance of player mean by position (different one for each main position - serves as different shrinkage parameters)
  
  //vector[3] playerB; // coefficients for player priors 
  
  real<lower=0> s2model; //model variance
  real<lower=0> s2player; //across player variance
  real<lower=0> s2playType; //across play type variance
  
  }
  
  transformed parameters {
  vector[N] eta;  // linear predictor
  real sumWeightedPhi;
  real sumWeightedPlayerEffect;
  // compute linear predictor
  eta = csr_matrix_times_vector(N, nPlayers, wX, vX, uX, b) + home*u + pass*g;
  //transformed phi
  sumWeightedPhi = sum(posWeights .*phi );
  sumWeightedPlayerEffect = sum(playerPlayWeights .*b );

  }
  
  model{
  real mu[N]; //temporary regression variable
  
  vector[nPlayers] positionVarCoef; // variance multiplier for each player
  vector[nPlayers] playerPriorMean; // prior mean for each player
  positionVarCoef = posMat*phi;//
  
  //Priors
  
  //priors and hyperpriors on players

  playerPriorMean = (-0.0733 + QBind*0.02422 + recGrade*0.000524 - recGradeUse*0.036944);  //recruiting information

  b ~ student_t(8, playerPriorMean, sqrt(s2player)*positionVarCoef) ;

// soft constraint to make positions weights sum to approximately 1
  sumWeightedPhi ~ normal(1, sdSumWeights*nPosition);

  s2player ~ gamma(0.5 , 90); // in a later version I may be able to change 0.03 and 10 to their own parameters


  //Different variances per position (each player gets 1 dominant position)
  phi[1] ~  gamma(2, 2) ; //QB 0.1,25
  phi[2] ~  gamma(2, 2) ; //RB (Runnning back and fullback)
  phi[3] ~  gamma(2, 2) ; //SWR (slot receivers)
  phi[4] ~  gamma(2, 2) ; //WR (non-slot wide receivers)
  phi[5] ~  gamma(2, 2) ; //TE not quite converged
  phi[6] ~  gamma(2, 2) ; //OT (tackle)
  phi[7] ~  gamma(2, 2) ; //OG (guard)
  phi[8] ~  gamma(2, 2) ; //C (center)
  phi[9] ~  gamma(2, 2) ; //DT (defensive tackle)
  phi[10] ~ gamma(2, 2) ; //DE (defensive end)
  phi[11] ~ gamma(2, 2) ; //OLB (outside linebacker)
  phi[12] ~ gamma(2, 2) ; //ILB (inside linebacker)
  phi[13] ~ gamma(2, 2) ; //SCB (slot cornerback)
  phi[14] ~ gamma(2, 2) ; //CB (cornerback)
  phi[15] ~ gamma(2, 2) ; //S (safety)
  
  
  // prior for Home Field Advantage (per play) 
  u ~ normal(0.1, 1); 
  
  // prior for play type
  g ~ normal(0, s2playType);
  
  s2playType ~ gamma(1,1);
  
  s2model ~ gamma(30,15);
  // likelihood contribution
  y ~ normal(eta, sqrt(s2model));
  
  
  
  }
  
