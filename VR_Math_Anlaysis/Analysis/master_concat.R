#######################################
# CONCATENATE DATA FILES
#######################################

rm(list = ls())

########## LOAD IN LIBRARIES ########## 

source("~/Dropbox/VR_TREADMILL/Helper_Functions/r_functions.R")
setwd("~/Dropbox/VR_TREADMILL/CODE/Sensorimotor/Figures_Tables")
call_libraries()

########## AESTHETICS ########## 

text_size <- 20
axis_text <- 10
th <- theme_pubclean(base_family = "Helvetica")  + theme(panel.grid.major = element_blank(), 
                                                         panel.grid.minor = element_blank(), 
                                                         strip.background = element_blank(), 
                                                         panel.spacing.x=unit(1.5, "lines"),
                                                         axis.line = element_line(colour = "black", size = 1), 
                                                         legend.position = "right", 
                                                         text=element_text(size= text_size, family="Helvetica"), 
                                                         strip.text.x = element_text(size=text_size, face="bold"), 
                                                         axis.text.x = element_text(size = axis_text), 
                                                         axis.text.y = element_text(size = axis_text), 
                                                         axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)), 
                                                         axis.title.x = element_text(vjust=-0.3), 
                                                         axis.ticks.length=unit(0.25,"cm"))

#######################################
# SM BATTERY 
#######################################

########## LOAD DATA ########## 

Total_Game <- c('The_Pentagram_Randomized', 'The_Pentagram5_perfected_ext', 'Tracking', 'Tracking_no_back')
pilot <- read.delim('~/Dropbox/VR_TREADMILL/DATA/Second_Pilot_Compiled/sm_data_10232019.csv', header=FALSE, sep=","  )
names(pilot)[1:3] <- c('SN', 'Session', 'Game')

########## SUBSET DATA ########## 

aiming <- short_sm_clean( pilot %>% filter(Game %in% Total_Game[1:2])  )
tracking <- short_sm_clean(pilot %>% filter(Game %in% Total_Game[3:4] ))

########## AIMING (PENTAGON) ########## 

aiming_processed <- aiming %>% select(SN, Session, Game, starts_with('RT'), -V4, -V5) %>% gather(cond, value, RT_1:RT_70) %>% 
  arrange(SN, Session) %>% 
  separate(cond, c("move_var", "trial"), sep = "_") %>%
  mutate(trial, as.numeric(trial) ) %>% 
  mutate(task = ifelse(Game == Total_Game[1], "Random", "Predict") ) %>% 
  mutate(value = ifelse(trial <= 5, NA, value * 1000)) # remove first trace 

aiming_ind_sum <- data_summary(aiming_processed, varname = "value", groupnames = c("SN", "Session", "task"))

# Now to reshape the data
library(stats)
aiming_ind_sum <- reshape(aiming_ind_sum, 
        timevar="task",
        idvar=c("SN", "Session"),
        direction = "wide")


#######################################
# COG BATTERY
#######################################

###### LOAD IN LIBRARIES ###### 
game_dir <- '~/Dropbox/VR_TREADMILL/DATA/Second_Pilot_Compiled/'
call_libraries()

###### INPUTS ###### 

game<- c("boxed", "filter", "trace", "BRT")
Total_SN <- sprintf('2yp%s', 1:10)
practice_trials <- c(3, 3, 3, 3)
num_cond <- c(4, 5, 2, 4)
remove_practice <- 1
num_session <- 2
make_ind_plot <- 1

###### GATHER GAME FILES ###### 

boxed_data <- read_vr_data("Boxed", game_dir) %>% select(-(FPS.Mean:Times.Finished.Game))
filter_data <- read_vr_data("filter", game_dir) %>% select(-(FPS.Mean:Times.Finished.Game))
trace_data <- read_vr_data("Triangle_Trace", game_dir) %>% select(-(FPS.Mean:Times.Finished.Game))
BRT_data <- read_vr_data("BRT", game_dir) %>% select(-(FPS.Mean:Times.Finished.Game))

###### RCS ###### 

SN_trials <- set_names(data.frame(matrix(NaN, length(Total_SN) * num_session, length(game) + 2)), c('SN', 'Session', game))
SN_trials$SN <- rep(Total_SN, each = num_session)
SN_trials$Session <- rep(c(seq(1, num_session, 1)), length(Total_SN) )

boxed_rcs <- set_names(data.frame(matrix(NaN, length(Total_SN) * num_session, num_cond[1] + 2)), c('SN', 'Session', as.character(unique(boxed_data$Condition) )))
filter_rcs <- set_names(data.frame(matrix(NaN, length(Total_SN) * num_session, num_cond[2] + 2)), c('SN', 'Session', as.character(unique(filter_data$Condition) )))
trace_rcs <- set_names(data.frame(matrix(NaN, length(Total_SN) * num_session, num_cond[3] + 2)), c('SN', 'Session', as.character(unique(trace_data$Condition) )))
BRT_rcs <- set_names(data.frame(matrix(NaN, length(Total_SN) * num_session, num_cond[4] + 2)), c('SN', 'Session', as.character(unique(BRT_data$Condition) )))

counter <- 1

for( si in 1:length(Total_SN)){
  for(ki in 1:2){
    for(ci in 1:num_cond[1]){
      temp_boxed_level <- unique(boxed_data$Condition)
      temp_boxed <- boxed_data[boxed_data$SN == Total_SN[si] & boxed_data$Session == ki & boxed_data$Condition == temp_boxed_level[ci], ]
      boxed_data$TN[boxed_data$SN == Total_SN[si] & boxed_data$Session == ki &boxed_data$Condition == temp_boxed_level[ci]] <- seq(1, nrow(temp_boxed))
      boxed_count_correct=sum(temp_boxed$Correct.Button)
      boxed_mean_rt= mean(temp_boxed$Response.Time/1000)
      boxed_length= length(temp_boxed$Response.Time)
      
      boxed_rcs$SN[counter] <- Total_SN[si]
      boxed_rcs$Session[counter] <- ki
      boxed_rcs[counter, ci + 2] <- boxed_count_correct/(boxed_mean_rt*boxed_length)
    }
    
    for(ci in 1:num_cond[2]){
      temp_filter_level <- unique(filter_data$Condition)
      temp_filter <- filter_data[filter_data$SN == Total_SN[si] & filter_data$Session == ki & filter_data$Condition == temp_filter_level[ci], ]
      filter_data$TN[filter_data$SN == Total_SN[si] & filter_data$Session == ki &filter_data$Condition == temp_filter_level[ci]] <- seq(1, nrow(temp_filter))
      filter_count_correct=sum(temp_filter$Correct.Button)
      filter_mean_rt= mean(temp_filter$Response.Time/1000)
      filter_length= length(temp_filter$Response.Time)
      
      filter_rcs$SN[counter] <- Total_SN[si]
      filter_rcs$Session[counter] <- ki
      filter_rcs[counter, ci + 2] <- filter_count_correct/(filter_mean_rt*filter_length)
      
    }
    
    for(ci in 1:num_cond[3]){
      temp_trace_level <- unique(trace_data$Condition)
      temp_trace <- trace_data[trace_data$SN == Total_SN[si] & trace_data$Session == ki & trace_data$Condition == temp_trace_level[ci], ]
      trace_data$TN[trace_data$SN == Total_SN[si] & trace_data$Session == ki &trace_data$Condition == temp_trace_level[ci]] <- seq(1, nrow(temp_trace))
      trace_count_correct=sum(temp_trace$Correct.Button)
      trace_mean_rt= mean(temp_trace$Response.Time/1000)
      trace_length=length(temp_trace$Response.Time)
      
      trace_rcs$SN[counter] <- Total_SN[si]
      trace_rcs$Session[counter] <- ki
      trace_rcs[counter, ci + 2] <- trace_count_correct/(trace_mean_rt*trace_length)
    }
    
    for(ci in 1:num_cond[4]){
      temp_BRT_level <- unique(BRT_data$Condition)
      temp_BRT <- BRT_data[BRT_data$SN == Total_SN[si] & BRT_data$Session == ki & BRT_data$Condition == temp_BRT_level[ci], ]
      BRT_data$TN[BRT_data$SN == Total_SN[si] & BRT_data$Session == ki &BRT_data$Condition == temp_BRT_level[ci]] <- seq(1, nrow(temp_BRT))
      BRT_count_correct=sum(temp_BRT$Correct.Button)
      BRT_mean_rt= mean(temp_BRT$Response.Time/1000)
      BRT_length= length(temp_BRT$Response.Time)
      
      BRT_rcs$SN[counter] <- Total_SN[si]
      BRT_rcs$Session[counter] <- ki
      BRT_rcs[counter, ci + 2] <- BRT_count_correct/(BRT_mean_rt*BRT_length)
      
      
    }
    counter <- counter + 1
  }
}


###### REMOVE PRACTICE & ADD TN ###### 

if(remove_practice == 1){
  for( si in 1:length(Total_SN)){
    for(ki in 1:2){
      boxed_data[boxed_data$SN == Total_SN[si] & boxed_data$Session == ki & boxed_data$TN %in% seq(1, practice_trials[1]), ] <- NA
      filter_data[filter_data$SN == Total_SN[si] & filter_data$Session == ki & filter_data$TN %in% seq(1, practice_trials[2]), ] <- NA
      trace_data[trace_data$SN == Total_SN[si] & trace_data$Session == ki & trace_data$TN %in% seq(1, practice_trials[3]), ] <- NA
      BRT_data[BRT_data$SN == Total_SN[si] & BRT_data$Session == ki & BRT_data$TN %in% seq(1, practice_trials[4]), ] <- NA
    }
  }
  boxed_data <- na.omit(boxed_data)
  filter_data <- na.omit(filter_data)
  trace_data <- na.omit(trace_data)
  BRT_data <- na.omit(BRT_data)
}

#######################################
# TUG
#######################################

########## LOAD DATA ########## 

tug <- read.delim('~/Dropbox/VR_TREADMILL/DATA/Second_Pilot_Compiled/tug_data_10242019.csv', header=TRUE, sep=","  )

########## CLEAN DATA ########## 

tug_summary <- tug %>% gather(cond, value, cup_1:nocup_2) %>%
  separate(cond, into = c("game", "TN"), sep = "_")
tug_summary[, c('Session', 'game')] <- lapply(tug_summary[, c('Session', 'game')] , factor)
tug_summary$game <- relevel(tug_summary$game, ref = 'nocup')
tug_summary <- tug_summary[1:20,]
tug_summary <- tug_summary[order(tug_summary$SN),]

#######################################
# BALANCE
#######################################

########## LOAD TRIAL DATA ########## 

posture_trials <- list.files(
  path = "~/Dropbox/VR_TREADMILL/DATA/Second_Pilot(Oct_Nov)/Balance",
  pattern = "trial_results.csv",
  full.names = TRUE,
  recursive = TRUE
) %>%
  map_dfr(read_csv)


posture_movement <- posture_trials %>% 
  mutate(center_eye_movement = map(file.path("~/Dropbox/VR_TREADMILL/DATA/Second_Pilot(Oct_Nov)/Balance", directory, center_eye_movement_filename), read_csv)) %>% 
  unnest()

#write.csv(posture_movement, file = 'balance_10232019.csv')

########## SUMMARIZE ########## 

calculate_path_length <- function(x, y, z) {
  sum(
    (diff(x) ^ 2 +
       diff(y) ^ 2 +
       diff(z) ^ 2) ^ 0.5
  )
}

new_session <- function(x){
  browser()
  new <- sprintf('Session %s', x)
  return(new)
}

posture_movement[, c('assessment_type', 'ppid', 'session_num')] <- sapply(posture_movement[, c('assessment_type', 'ppid', 'session_num')] , factor)
posture_movement$assessment_type  <- fct_relevel(posture_movement$assessment_type, c('Vision', 'NoVision', 'RoomOscillate') )

# posture_summary <- posture_movement %>% 
#   group_by(experiment, ppid, session_num, assessment_type) %>% 
#   summarise(path_length = calculate_path_length(pos_x, pos_y, pos_z)) %>%
#   separate(ppid, into = c('game', 'SN'), by = '_')


# Goal: SN, Session, Conditions, Value (path length)
# Create empty dataframe with these headers. 
# loop through subjects using for loop

# take out your subjects data from data frame 
# using index.
# calculate path length using function 
# store in new dataframe 

counter <- 1
posture_summary <- set_names(data.frame(matrix(NaN, 10 * 2 * 6, 5)), c('SN', 'Session', "Type", "PL", "TN"))

posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp1"] <- 1
posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp2"] <- 2
posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp3"] <- 3
posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp4"] <- 4
posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp5"] <- 5
posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp6"] <- 6
posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp7"] <- 7
posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp8"] <- 8
posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp9"] <- 9
posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp10"] <- 10

for( si in 1:10){
  for(ki in 1:2){
    for(ci in 1:6){
      idx <- as.numeric(posture_movement$ppid) == si & as.numeric(posture_movement$session_num) == ki & as.numeric(posture_movement$trial_num) == ci
      if(sum(idx) > 1){
        x <- posture_movement$pos_x[idx]
        y <- posture_movement$pos_y[idx]
        z <- posture_movement$pos_z[idx]
        path_length = calculate_path_length(x, y, z)
        
        posture_summary$SN[counter] <- si
        posture_summary$Session[counter] <- ki
        posture_summary$TN[counter] <- ci
        posture_summary$Type[counter] <-  unique(posture_movement$assessment_type[idx])
        posture_summary$PL[counter] <- path_length
        counter <- counter + 1
      }
    }
  }
}

posture_summary <- posture_summary[1:114,]
library(dplyr)
posture_summary %>% separate(posture_summary$Type, c("vision", "no_vision", "oscillate"))


#######################################
# CONCATENATE FILES 
#######################################

# add columns from TUG (need to fix subj 10 row positions)
aiming_ind_sum$age <- tug_summary$Age
aiming_ind_sum$sex <- tug_summary$Sex
aiming_ind_sum$tug_value <- tug_summary$value

# add columns from COG battery 
boxed_rcs <- boxed_rcs[order(boxed_rcs$SN),]
BRT_rcs <- BRT_rcs[order(BRT_rcs$SN),]
filter_rcs <- filter_rcs[order(filter_rcs),]
filter_rcs <- filter_rcs[116:135,]

aiming_ind_sum$boxed_conjunction_4 <- boxed_rcs$Conjunction_4
aiming_ind_sum$boxed_feature_4 <- boxed_rcs$Feature_4
aiming_ind_sum$boxed_conjunction_12 <- boxed_rcs$Conjunction_12
aiming_ind_sum$boxed_feature_12 <- boxed_rcs$Feature_12

aiming_ind_sum$BRT_left <- BRT_rcs$Left
aiming_ind_sum$BRT_left_thumb <- BRT_rcs$LeftThumb
aiming_ind_sum$BRT_right <- BRT_rcs$Right
aiming_ind_sum$BRT_right_thumb <- BRT_rcs$RightThumb

aiming_ind_sum$filter_R2B0 <- filter_rcs$R2B0
aiming_ind_sum$filter_R2B2 <- filter_rcs$R2B2
aiming_ind_sum$filter_R2B4 <- filter_rcs$R2B4
aiming_ind_sum$filter_R4B0 <- filter_rcs$R4B0
aiming_ind_sum$filter_R4B2 <- filter_rcs$R4B2

master <- read.csv("/Users/newxjy/Desktop/master_concat_data.csv")

# add posture balance data into the dataframe 
## average values across rows 
posture_ind_sum <- data_summary_med(data = posture_summary, varname = "PL", groupnames = c("SN", "Session", "Type"))
posture_ind_sum <- subset(posture_ind_sum, select = -c(sd))
posture_ind_sum <- posture_ind_sum %>% spread(Type, PL)
posture_ind_sum <- rbind(posture_ind_sum,list(9,2,0.23515343,0.26366032, 0.35013079))
posture_ind_sum %>% arrange(SN)

master$balance_vision <- posture_ind_sum$`1`
master$balance_novision <- posture_ind_sum$`2`
master$balance_oscillate <- posture_ind_sum$`3`

# derive the right calculation for each performances
master$box_score <- (master$boxed_conjunction_4 - master$boxed_feature_4)/(master$boxed_conjunction_12 - master$boxed_feature_12)
master$BRT_score <- 1/(master$BRT_left_thumb - master$BRT_right_thumb + master$BRT_left - master$BRT_right)
master$filter_score <- 1/(master$filter_R2B0 - master$filter_R4B0 + master$filter_R2B2 - master$filter_R4B2)

write.csv(master, "master_concat_data.csv")

# step 2: 
# box performance


# step 3: calculate the mean for differences 
# within battery 
  
# between battery

#######################################
# ANALYSIS 
#######################################

scatterplotMatrix(~ income + education + prestige | type, data=Duncan)
scatterplotMatrix(~ income + education + prestige, 
                  transform=TRUE, data=Duncan, smoother=loessLine)
scatterplotMatrix(~ income + education + prestige | type, smoother=FALSE, 
                  by.group=TRUE, transform=TRUE, data=Duncan)



