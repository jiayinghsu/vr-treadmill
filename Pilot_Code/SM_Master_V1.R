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

########## LOAD DATA ########## 

Total_Game <- c('The_Pentagram_Randomized', 'The_Pentagram5_perfected_ext', 'Tracking', 'Tracking_no_back')
pilot <- read.delim('~/Dropbox/VR_TREADMILL/DATA/Second_Pilot_Compiled/sm_data_2122020.csv', header=FALSE, sep=","  )
names(pilot)[1:4] <- c('SN', 'Session','Pilot_Num', 'Game')

########## SUBSET DATA ########## 

aiming <- short_sm_clean( pilot %>% filter(Game %in% Total_Game[1:2])  )
tracking <- short_sm_clean(pilot %>% filter(Game %in% Total_Game[3:4] ))

########## AIMING (PENTAGON) ########## 

aiming_processed <- aiming %>% select(SN, Session,Pilot_Num, Game, starts_with('RT'), -V5, -V6) %>% gather(cond, value, RT_1:RT_70) %>% 
  arrange(SN, Session) %>% 
  separate(cond, c("move_var", "trial"), sep = "_") %>%
  mutate(trial, as.numeric(trial) ) %>% 
  mutate(task = ifelse(Game == Total_Game[1], "Random", "Predict") ) %>% 
  mutate(value = ifelse(trial <= 5, NA, value * 1000)) # remove first trace 

aiming_ind_sum <- data_summary(aiming_processed, varname = "value", groupnames = c("SN", "Session","Pilot_Num", "task"))

aim_plot <- ggplot(aiming_ind_sum, aes(x = factor(task), y = value) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange") +
  stat_summary(fun.y = "mean", geom = "line", alpha  = 0.3, aes(group = factor (SN), color = factor(SN))) +
  stat_summary(fun.y = "mean", geom = "point", alpha  = 0.3, aes(group = factor (SN), color = factor(SN))) +
  labs(x = "Condition", y = "RT (ms)") + facet_wrap(Pilot_Num~Session) + th
print(aim_plot)


########## TRACKING (FIG 8) ########## 

# calcuate 1 over root mean square: everyone draws 3 slow, 3 med, 3 fast (x with and without guidelines)
tracking_processed <- tracking %>%
  mutate(speed1 = (F_track_error_RMS_1 + F_track_error_RMS_1.1 + F_track_error_RMS_1.2)/3,
         speed2 =  (F_track_error_RMS_2 + F_track_error_RMS_2.1 + F_track_error_RMS_2.2)/3, 
         speed3 = (F_track_error_RMS_3 + F_track_error_RMS_3.1 + F_track_error_RMS_3.2)/3, 
         speed4 = (F_track_error_RMS_4 + F_track_error_RMS_4.1 + F_track_error_RMS_4.2)/3) %>%
  select(SN, Session, Game,Pilot_Num, speed1, speed2, speed3, speed4) %>%
  mutate(Game = ifelse(Game == Total_Game[3], "No Line", "Line")) %>% 
  gather(cond, value, speed1:speed4) 

tracking3_plot <- ggplot(tracking_processed, aes(x = Game, y = value)) + 
  stat_summary(fun.data = 'mean_se', geom = 'pointrange', aes(group = Session, color = factor(Session) ), position = position_dodge(0.1) ) + 
  stat_summary(fun.y = 'mean', geom = 'line', aes(group = Session, color  = factor(Session))) + 
  facet_wrap(Pilot_Num ~ cond, ncol = 4) + th + labs(x = 'Condition', y = 'RMSE (mm)', color = 'Session')
print(tracking3_plot)








