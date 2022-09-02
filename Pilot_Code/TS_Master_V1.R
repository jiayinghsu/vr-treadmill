rm(list = ls())

############ LOAD LIBRARIES ############ 

setwd("~/Dropbox/VR_TREADMILL/CODE/Cognitive/Figures_Tables")
source('~/Dropbox/VR_TREADMILL/Helper_Functions/r_functions.R')
call_libraries()

###### AESTHETICS ###### 

text_size <- 22

th <- theme_pubclean(base_family = "Helvetica")  + theme(axis.line = element_line(size = 1.25), 
                                                         legend.position = 'right', 
                                                         legend.text = element_text(size= text_size, family="Helvetica"),
                                                         text=element_text(size= text_size, family="Helvetica"), 
                                                         strip.text.x = element_text(size = rel(0.97)), 
                                                         strip.text.y = element_text(size = rel(0.97)), 
                                                         axis.text.x = element_text(size = rel(0.97)),
                                                         axis.text.y = element_text(size = rel(0.97)),
                                                         axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)), 
                                                         axis.title.x = element_text(vjust=-0.3), 
                                                         plot.title = element_text(hjust = 0.5, vjust = -7), 
                                                         axis.ticks.length=unit(0.25, "cm"), 
                                                         axis.ticks = element_line(size = 0.75))

############ LOAD DATA ############ 

pilot.data <- read.table('~/Dropbox/VR_TREADMILL/DATA/Second_Pilot_Compiled/ts_data_2212020.csv', header=TRUE, sep=",")%>%
mutate(Pilot_Num = ifelse(SN %in% sprintf('yp%s', 1:14), 1,
        ifelse(SN %in% sprintf('yp%s', 15:19), 2,3)))

############ CLEAN DATA ############

num.sub <- length( unique(pilot.data$SN) ) 
subj.list <- unique(pilot.data$SN)
pilot.data[c("trans", "trial_type")] <- lapply(pilot.data[c("trans", "trial_type")] , factor)
pilot.data$trans <- factor(pilot.data$trans, levels = c( "1", "3", "2"))
levels(pilot.data$trans) <- c("No Switch","Switch Diff","Switch Same")

############ CLEAN DATA ############ 

pilot.data$rt <- pilot.data$rt  *  1000

for(si in 1:num.sub){
  for(ti in 1:2){
    idx <- pilot.data$SN == subj.list[si] & pilot.data$Session == ti
    pilot.data$TN [idx] <- 1:sum(idx)
    mean_rt <- mean(pilot.data$rt[idx])
    sd_rt <- sd(pilot.data$rt[idx])
    idx_remove <- (pilot.data$rt > mean_rt + 3 * sd_rt) | (pilot.data$rt < mean_rt - 3 * sd_rt) #| (pilot.data$rt > 5000)
    pilot.data$rt[idx &  idx_remove] <- NaN
  }
}

pilot.acc <- pilot.data [ pilot.data$correct == 1  &  pilot.data$TN >= 5, ]
pilot.acc <- pilot.acc [pilot.data$rt > 250, ]
pilot.acc <- na.omit(pilot.acc)

############ LEARNING PLOT ############ 

ggplot (pilot.data[pilot.data$SN == subj.list[20], ], aes(x = TN, y = rt)) + 
  geom_line() +
  facet_wrap(.~Session)

############ PLOT MAIN RT PLOT ############ 

ggplot(pilot.acc, aes(x = Session, y = rt, group = SN, color = SN)) + 
  stat_summary(fun.y = 'mean', geom = 'point') +
  stat_summary(fun.y = 'mean', geom = 'line') +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.05) +
  facet_grid(trans~Pilot_Num)

ggplot(pilot.acc, aes(x =  trans, y = rt, group = SN, color = factor(SN))) + 
  stat_summary(fun.y = 'mean', geom = 'point') +
  stat_summary(fun.y = 'mean', geom = 'line') +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.05) +
  facet_wrap(Session~Pilot_Num)

ggplot(pilot.acc, aes(x = trans, y = rt)) + 
  stat_summary(fun.y = 'mean', geom = 'line', alpha = 0.30, size = 0.5, aes(group = SN, color = factor(SN)) ) +
  stat_summary(fun.y = 'mean', geom = 'point', alpha = 0.30, size = 2, aes(group = SN, color = factor(SN)) ) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  facet_wrap(Session~Pilot_Num) + 
  xlab("Conditions") + 
  ylab("RT (ms)") + th






