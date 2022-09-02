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

pilot <- read.delim('~/Dropbox/VR_TREADMILL/DATA/Second_Pilot_Compiled/tug_data_28Feb2020.csv', header=TRUE, sep=","  )%>%
          mutate(Pilot_Num = ifelse(SN %in% sprintf('2yp%s', 1:14), 1, 
                                    ifelse(SN %in% sprintf('2yp%s', 15:19), 2, 3) ) )

########## CLEAN DATA ########## 

pilot_summary <- pilot %>% gather(cond, value, cup_1:nocup_2) %>%
  separate(cond, into = c("game", "TN"), sep = "_")
pilot_summary[, c('Session', 'game')] <- lapply(pilot_summary[, c('Session', 'game')] , factor)
pilot_summary$game <- relevel(pilot_summary$game, ref = 'nocup')

########## PLOT DATA ########## 

tug_plot <- ggplot(pilot_summary, aes(factor(Session), value)) + 
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') + 
  facet_grid(game~Pilot_Num) + 
  stat_summary(fun.y = 'mean', geom = 'line', alpha = 0.5, aes(group = SN, color = SN) ) + 
  labs(x = 'Session', y = 'Time (s)') + 
  th
print(tug_plot)

tug2_plot <- ggplot(pilot_summary, aes(factor(game), value)) + 
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') + 
  facet_grid(Session ~ Pilot_Num) + 
  stat_summary(fun.y = 'mean', geom = 'line', alpha = 0.5, aes(group = SN, color = SN) ) + 
  labs(x = 'Condition', y = 'Time (s)') + 
  th
print(tug2_plot)





