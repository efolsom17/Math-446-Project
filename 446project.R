library(dplyr)
library(baseballr)
library(glmnet)

#comment this out if you aren't shea, or substitute whatever folder you've got the data in
setwd("/Users/ericfolsom/Desktop/446 Project")

#initializing data
pre <- read.csv("pre_ban_min100pa.csv", header = TRUE, stringsAsFactors = FALSE)
post <- read.csv("post_ban_min100pa.csv", header = TRUE, stringsAsFactors = FALSE)
#filtering our data to pitchers who reached the prerequisite pitching threshold
#both before and after the policy change
overlap <- intersect(pre$player_id, post$player_id)
# the %>% character is indicating running a function on a dataset, so in this case we're filtering
# the pre d.f for player_id's *in* the overlap vector, then storing that new d.f in true_pre
true_pre <- pre %>% filter(player_id %in% overlap) %>% 
  mutate(whiff_rate = whiffs/swings)
true_post <- post %>% filter(player_id %in% overlap) %>% 
  mutate(whiff_rate = whiffs/swings)
#doing rank sum test on difference in spin rate as it's a paired sample.
#p = 0.008974, indicates a significant difference in spin rates following the ban
sr_test <- true_pre$spin_rate - true_post$spin_rate
wilcox.test(sr_test, alternative = "greater")
#pre-avg = 2335.85
#mean(true_pre$spin_rate)
#post-avg = 2290.661
#mean(true_post$spin_rate)
#going off the dip in averages as well as the significant p-value, we can say that the sticky
#stuff ban resulted in a statistically significant dip in spin rate for the average mlb starter

woba_test <- true_pre$woba - true_post$woba
wilcox.test(woba_test, alternative = "less")
#p-value of 0.0004066, indicates a significant difference in woba pre and post ban
#mean(true_pre$woba)
#mean(true_post$woba)
#higher avg. woba post ban indicates that there's a significantly lower woba pre ban, 
#so we can conclude that pitchers were gaining a significant advantage pre ban.

#getting bref data from baseballr
bref_pre <- bref_daily_pitcher("2021-04-01", "2021-06-21")
bref_post <- bref_daily_pitcher("2021-06-21", "2021-10-03")
bref_pre <- fip_plus(bref_pre)
bref_post <- fip_plus(bref_post)
#making our map
map <- read.csv("id_map.csv", header = TRUE, stringsAsFactors = FALSE)
#map1 <- map
#note that when making the map the name column selected impacts how many NA's show 
#up in the final product. Current name selection has 6 na's in final df.
colnames(map)[39] = "Name"
f <- select(bref_pre, bbref_id, Name)
map <- left_join(map, f, by = "Name")
map <- map %>% filter(!is.na(bbref_id)) %>% 
  select(MLBID, Name, bbref_id)
colnames(map)[1] = "player_id"
#using the map to combine the baseball savant data with the bref data
true_pre <- left_join(true_pre, map, by = "player_id")
true_pre <- left_join(true_pre, bref_pre, by = "bbref_id")
true_pre <- true_pre[!duplicated(true_pre), ]
true_post <- left_join(true_post, map, by = "player_id")
true_post <- left_join(true_post, bref_pre, by = "bbref_id")
true_post <- true_post[!duplicated(true_post), ]


#######################
# LASSO REGRESSION EXPERIMENTATION
#######################

# once we're able to get our data from baseball savant we can work this out.

full_season <- read.csv("full_season_data.csv", header = TRUE, stringsAsFactors = FALSE)
full_season <- left_join(full_season, map, by = "player_id")
full_season <- left_join(full_season, bref_pre, by = "bbref_id")
full_season <- full_season[!duplicated(full_season), ]
full_season <- full_season %>% mutate(whiff_rate = whiffs/swings)
#getting full data from statcast
atcast_data <- read.csv("full_season_statcast.csv.part", header = TRUE)
# we want average pfx_x and pfx_z for each individual pitcher for fastballs and breaking balls
pitches <- c("FA", "FT", "FC", "SI", "SL", "CU", "KC", "KN", "EP")
statcast_data <- statcast_data %>%
  filter(pitcher %in% full_season$player_id) %>%
  filter(pitch_type %in% pitches) %>% 
  group_by(pitcher) %>% 
  mutate(avg_x = mean(pfx_x), avg_z = mean(pfx_z))
colnames(statcast_data)[9] <- "player_id"
statcast_data <- select(statcast_data, player_id, avg_x, avg_z)
statcast_data <- statcast_data %>% distinct(player_id, .keep_all = TRUE)
full_season <- left_join(full_season, statcast_data, by = "player_id")

#everything we're interested in
baseball_trim <- full_season %>% select("woba", "spin_rate", "velocity", "whiff_rate", "WHIP", "FIP", "SO9", "release_extension", "launch_angle")
baseball_trim <- baseball_trim[complete.cases(baseball_trim), ]
baseball_trim <- baseball_trim %>% select("WHIP", "spin_rate", "velocity", "release_extension", "launch_angle")

baseball2 <- data.frame(scale(as.matrix(baseball_trim)))
B <- 100

#Matrices and vector storing results
lasso01mat<-c()
lassobetamat<-c()
mse.lasso<-c()

for(b in 1:B)
{
  #set.seed(b)
  print(paste("Iteration #", b, sep=""))
  train = sample(1:dim(baseball2)[1], dim(baseball2)[1]/2)
  test <- -train
  baseball2.train <- baseball2[train, ]
  baseball2.test <- baseball2[test, ]
  
  train.mat <- model.matrix(WHIP ~ ., data = baseball2.train)[,-1]
  test.mat <- model.matrix(WHIP ~ ., data = baseball2.test)[,-1]
  
  #alpha = 1 corresponds to the lasso regression
  fit.lasso <- glmnet(train.mat, baseball2.train$WHIP, alpha = 1)
  cv.lasso <- cv.glmnet(train.mat, baseball2.train$WHIP, alpha = 1)
  bestlam.lasso <- cv.lasso$lambda.min
  bestlam.lasso
  
  pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
  mse.lasso[b] <- mean((pred.lasso - baseball2.test$WHIP)^2)
  
  #List of selected/dropped variables with their respective beta estimates
  beta.lasso <- predict(fit.lasso, s = bestlam.lasso, type = "coefficients")
  lassobetamat <- rbind(lassobetamat, beta.lasso[-1,1])
  
  #Convert the variables to 0 (not selected) or 1 (selected)
  lasso01 <- as.integer(abs(beta.lasso[-1,1]) > 0)
  lasso01mat <- rbind(lasso01mat, lasso01)
}
colnames(lasso01mat) <- colnames(baseball2)[-1]
colnames(lassobetamat) <- colnames(baseball2)[-1]
#Showing the probability of selecting each predictor for the Lasso
colMeans(lasso01mat)
#Showing the mean of the beta coefficients for each predictor for the Lasso
colMeans(lassobetamat)
#Mean of the MSE values
mean(mse.lasso)

#plots the residuals for ONE (the last) of the above simulated lasso regressions
plot((pred.lasso-baseball2.test$WHIP), main = "Residuals vs. Fits (Lasso Regression)",
     ylab = "Fit", xlab = "Residuals")



