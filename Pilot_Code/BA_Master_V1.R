rm(list = ls())

########## LOAD IN LIBRARIES ########## 

source("~/Dropbox/VR_TREADMILL/Helper_Functions/r_functions.R")
setwd("~/Dropbox/VR_TREADMILL/CODE/Sensorimotor/Figures_Tables")
library(tidyverse)
library(ggpubr)


########## AESTHETICS ########## 

text_size <- 20
axis_text <- 15
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

all_cond <- unique(posture_movement[, c('ppid', 'session_num')])
all_SN <- c("vrtreadmill_yp1", "vrtreadmill_yp2", "vrtreadmill_yp3", "vrtreadmill_yp4", "vrtreadmill_yp5", "vrtreadmill_yp6", "vrtreadmill_yp7", 
             "vrtreadmill_yp8", "vrtreadmill_yp9", "vrtreadmill_yp10","vrtreadmill_yp15_s1", "vrtreadmill_2yp16", "vrtreadmill_session1_2yp17", "vrtreadmill_session1_a1",
            "vrtreadmill_session1_a2","vrtreadmill_session1_a5", "vrtreadmill_session1_a6", "vrtreadmill_session1_a7","vrtreadmill_session1_a8")
num_cond <- length(all_cond$ppid)
num_subj <- length(all_SN)

# create new column called condition and new column called SN

for(ci in 1:num_cond){
  # assign ci incrementally to new cond 
  posture_movement$condition[posture_movement$ppid==all_cond$ppid[ci]&posture_movement$session_num==all_cond$session_num[ci]]<-ci
  }
# assign position in all_SN as subj number
for(si in 1:num_subj){
  posture_movement$SN[posture_movement$ppid==all_SN[si]]<- si
}

#posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp1"] <- 1
#posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp2"] <- 2
#posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp3"] <- 3
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp4"] <- 4
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp5"] <- 5
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp6"] <- 6
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp7"] <- 7
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp8"] <- 8
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp9"] <- 9
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp10"] <- 10
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_yp15_s1"] <- 15
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_session1_2yp16"] <- 16
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_session1_yp17"] <- 17
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_session1_a1"] <- 18
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_session1_a2"] <- 19
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_a5"] <- 20
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_a6"] <- 21
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_a7"] <- 22
# posture_movement$ppid[posture_movement$ppid == "vrtreadmill_a8"] <- 23


  for(ci in 1:num_cond){
    for(ti in 1:6){
      idx <- as.numeric(posture_movement$condition) == ci & as.numeric(posture_movement$trial_num) == ti
      if(sum(idx) > 1){
        x <- posture_movement$pos_x[idx]
        y <- posture_movement$pos_y[idx]
        z <- posture_movement$pos_z[idx]
        path_length = calculate_path_length(x, y, z)
        
        posture_summary$condition[counter] <- ci
        posture_summary$TN[counter] <- ti
        posture_summary$Type[counter] <-  unique(posture_movement$assessment_type[idx])
        posture_summary$PL[counter] <- path_length
        counter <- counter + 1
      }
    }
  }






# we can also plot with dot plot
posture_summary <- posture_summary[1:114,]
posture_summary[c('SN', 'Session', 'Type')] <- lapply(posture_summary[c('SN', 'Session', 'Type')], factor)

posture_sum <- data_summary_med(data = posture_summary, varname = "PL", groupnames = c("SN", "Session", "Type"))
ggplot(posture_sum, aes(x =Type, y = PL *  1000)) + 
  geom_point(alpha = 0.2, size = 2, aes(color = SN)) +
  geom_line(alpha = 1, size = 0.5, aes(group = SN, color = SN)) +
  stat_summary(fun.data= 'mean_se', geom="pointrange", color="black") + 
  facet_wrap(.~Session) + 
  xlab("Vision conditions") + 
  ylab("Head path length (mm)") + th


