###### LOAD IN LIBRARIES ###### 

rm(list = ls())
source("~/Dropbox/VR_TREADMILL/Helper_Functions/r_functions.R")
setwd("~/Dropbox/VR_TREADMILL/CODE/Cognitive/Figures_Tables")
game_dir <- '~/Dropbox/VR_TREADMILL/DATA/Second_Pilot_Compiled/'
todaysdate <- '21Feb2020'
call_libraries()

###### INPUTS ###### 

game<- c("boxed", "filter", "trace", "BRT")
Total_SN_young <- sprintf('2yp%s', 1:19)
Total_SN_adult<- sprintf('a%s', 1:4)
Total_SN <- c(Total_SN_young, Total_SN_adult)
practice_trials <- c(3, 3, 3, 3)
num_cond <- c(4, 5, 2, 4)
remove_practice <- 1
num_session <- 2
make_ind_plot <- 1

###### AESTHETICS ###### 

text_size <- 22

th <- theme_pubclean(base_family = "Helvetica")  + theme(axis.line = element_line(size = 1.25), 
                                                         legend.position = 'none', 
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

###### GATHER GAME FILES ###### 

boxed_data <- read_vr_data("Boxed", game_dir) %>% select(-(FPS.Mean:Times.Finished.Game))%>%
  mutate(Pilot_Num = ifelse((SN %in% sprintf('2yp%s', 1:19)&Session==1), 'Y1', 
  ifelse ((SN %in% sprintf('2yp%s', 1:19)& Session==2), 'Y2','A1' ) ))
filter_data <- read_vr_data("filter", game_dir) %>% select(-(FPS.Mean:Times.Finished.Game))%>%
  mutate(Pilot_Num = ifelse((SN %in% sprintf('2yp%s', 1:19)&Session==1), 'Y1', 
  ifelse ((SN %in% sprintf('2yp%s', 1:19)& Session==2), 'Y2','A1' ) ))%>%
mutate(Group = ifelse(SN %in% sprintf('2yp%s', 1:19), 'Y','A'))
trace_data <- read_vr_data("Triangle_Trace", game_dir) %>% select(-(FPS.Mean:Times.Finished.Game))%>%
  mutate(Pilot_Num = ifelse((SN %in% sprintf('2yp%s', 1:19)&Session==1), 'Y1', 
  ifelse ((SN %in% sprintf('2yp%s', 1:19)& Session==2), 'Y2','A1' ) ))
BRT_data <- read_vr_data("BRT", game_dir) %>% select(-(FPS.Mean:Times.Finished.Game)) %>%
  mutate(Pilot_Num = ifelse((SN %in% sprintf('2yp%s', 1:19)&Session==1), 'Y1', 
  ifelse ((SN %in% sprintf('2yp%s', 1:19)& Session==2), 'Y2','A1' ) ))

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



###### FILTER ###### 

filter_data <- filter_data %>% mutate(k = ifelse( Cue.Rotated == 0 & Button.Pressed == "Same", "Hit", 
                                                  ifelse(Cue.Rotated == 0 & Button.Pressed == "Different", "FA", 
                                                         ifelse(Cue.Rotated == 1 & Button.Pressed == "Same", "Miss", "CR") ) )   ) %>%
  mutate(num_tar = ifelse(Condition == "R2B0" | Condition == "R2B2" | Condition == "R2B4", 2, 4)) %>%
  mutate(num_dist = ifelse(Condition == "R2B0" | Condition == "R4B0", 0, 
                           ifelse(Condition == "R2B2" | Condition == "R4B2", 2, 4) ) ) %>%
  mutate(count = 1)%>%
arrange(desc(SN))%>%
  mutate(tally=1)

filter_SN_Check <- filter_data %>%
  data_summary_count(varname = "tally", groupnames=c("SN", "Session"))

filter_count <- data_summary_count(filter_data, varname = 'count', groupnames = c("SN", "Session", "num_tar", "num_dist", "k", "Condition", "Pilot_Num", "Group") ) %>%
  spread(k, count) 
filter_count[is.na(filter_count)] <- 0

filter_count$total <- rowSums(filter_count[ , c('CR', 'FA', 'Hit', 'Miss') ], na.rm = TRUE)
filter_count$dprime <- dprime(filter_count$Hit, filter_count$FA, filter_count$Miss, filter_count$CR)$dprime 
  

# plot d prime 
filter_plot <- ggplot(data = filter_count, aes (x = num_dist, y = dprime, color = num_tar, linetype=Session) ) + 
  facet_grid(.~Group) +
  geom_point()+
  #stat_summary(fun.y = "mean", geom = "point", size = 2) + 
  stat_summary(fun.data = 'mean_se', geom = "pointrange", inherit.aes = FALSE, aes (x = num_dist, y = dprime)) + 
  stat_summary(fun.y = "mean", geom = "line", inherit.aes = FALSE, aes(x = num_dist, y = dprime, color = num_tar, linetype=Session)) + 
  ggtitle("Filter dprime") + th

# make individual plots 
if(make_ind_plot == 1) {
  filter_plot <- filter_plot + geom_line ( aes(group = SN, color = SN), alpha = 0.5)
  print(filter_plot)
}      


###### TRACE ###### 

###### TRIANGLE TRACE ###### 

trace_data <- trace_data %>% mutate(RT = Response.Time) %>% select(-Response.Time) %>%
  mutate(count = 1) %>%
  mutate(accuracy = ifelse(Correct.Button == 1, 1, 0), RT = ifelse(accuracy == 1, RT, NaN))  %>%
  # remove correct rejection from RTs
  mutate(RT = ifelse(Trial.Accuracy == "Correct Rejection", NaN, RT)) %>%
  mutate(Condition = ifelse(Condition == "Tap Only", "Tap", "TT")) %>% 
  mutate(Trial.Accuracy = ifelse (Trial.Accuracy == "Correct Rejection", "CR",
                                  ifelse(Trial.Accuracy == "False Alarm", "FA", 
                                         ifelse(Trial.Accuracy == "Hit", "Hit", "Miss" )))) 


# summarise raw RT data
trace_RT_sum <- data_summary_med(data = trace_data, varname = "RT", groupnames = c("Condition", "Session", "SN", "Pilot_Num"))

trace_RT_plot <- ggplot(data = trace_RT_sum, aes(x = Condition, y = RT)) + 
  stat_summary(fun.y = "mean", geom = "point", size =  2) + stat_summary(fun.y = "mean", geom = "line") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2)+
  ggtitle("Dual Task RT") + 
  facet_wrap(Pilot_Num~Session) + th

# summarise cost data 
trace_cost_sum <- data_summary_med(data = trace_data, varname = "RT", groupnames = c("Condition", "Session", "SN", "Pilot_Num")) %>%
  select(-sd) %>% spread(Condition, RT) %>%
  mutate(Cost = TT - Tap) %>%
  subset(!SN %in% '2yp3') #SHOULD WE REMOVE THIS OUTLIER.

trace_cost_plot <- ggplot(data = trace_cost_sum, aes(x = Pilot_Num, y = Cost, color= SN)) + 
  geom_point()+
  stat_summary(fun.data = 'mean_se', geom = "pointrange", inherit.aes = FALSE, aes (x = Pilot_Num, y = Cost)) + 
  stat_summary(fun.y = "mean", geom = "line", inherit.aes = FALSE, aes(x = Pilot_Num, y = Cost)) + 
  stat_summary(fun.y = "mean", geom = "point", size = 2) + 
  stat_summary(fun.y = "mean", geom = "line") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2)+ 
  ggtitle("Dual Task Cost")

# plot individual data 
if(make_ind_plot == 1) {
  trace_RT_plot <- trace_RT_plot + geom_line ( aes(group = SN, color = SN), alpha = 0.5)
  trace_cost_plot <- trace_cost_plot+ stat_summary(fun.y = "mean", geom = "line", aes(group = SN, color = SN), alpha = 0.5) + th

  print(trace_RT_plot)
  print(trace_cost_plot)
}      


###### BOXED: VISUAL SEARCH ###### 

boxed_data <- boxed_data %>% 
  mutate(num_obj = ifelse(Condition == "Conjunction_12" | Condition == "Feature_12", "12", "4") ) %>%
  mutate(type= ifelse(Condition == "Conjunction_12" | Condition == "Conjunction_4", "Con", "Feat") ) %>%
  mutate(RT = Response.Time) %>% select(-Response.Time) %>%
  # only look at accurate trials
  mutate(accuracy = ifelse(Correct.Button == 1, 1, 0), RT = ifelse(accuracy == 1, RT, NaN)) %>%
  arrange(desc(SN))%>%
  mutate(tally=1)

boxed_SN_Check <- boxed_data %>%
  data_summary_count(varname = "tally", groupnames=c("SN", "Session"))

# define factors 
boxed_data[c("Condition", "num_obj", "type")] <- lapply(boxed_data[c("Condition", "num_obj", "type")], factor)
boxed_data$num_obj <- factor(boxed_data$num_obj, levels = c( "4", "12"))

# summarise raw RT data 
boxed_RT_sum <- data_summary_med(data = boxed_data, varname = "RT", groupnames = c("num_obj", "type", "Session", "SN", "Condition", "Pilot_Num"))

boxed_RT_plot <- ggplot(data = boxed_RT_sum, aes(x = num_obj, y = RT, color = type)) + facet_grid(type+Pilot_Num~Session)+
  stat_summary(fun.y = "mean", geom = "point", size = 2) + stat_summary(fun.y = "mean", geom = "line") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2)+ ggtitle("Visual search RT") +th

# summarise cost data 
boxed_cost_sum <- data_summary_med(data = boxed_data, varname = "RT", groupnames = c("Condition", "Session", "SN", "Pilot_Num")) %>%
  select(-sd) %>% spread(Condition, RT) %>%
  mutate(CD = Conjunction_12 - Conjunction_4, FD = Feature_12 - Feature_4) %>% 
  gather(Diff, Cost, CD:FD)

boxed_cost_plot <- ggplot(data = boxed_cost_sum, aes(x = Pilot_Num, y = Cost, group = Diff, Color=SN)) + facet_grid(.~Diff)+
  geom_point()+
  stat_summary(fun.data = 'mean_se', geom = "pointrange", inherit.aes = FALSE, aes (x = Pilot_Num, y = Cost)) + 
  stat_summary(fun.y = "mean", geom = "line", inherit.aes = FALSE, aes(x = Pilot_Num, y = Cost)) + 
  stat_summary(fun.y = "mean", geom = "point", size = 2) + stat_summary(fun.y = "mean", geom = "line") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2)+ggtitle("Visual Search Cost")

# summarise accuracy data 
boxed_acc_sum <- data_summary(data = boxed_data, varname = "accuracy", groupnames = c("num_obj", "type", "Session", "SN", "Pilot_Num"))

boxed_acc_plot <- ggplot(data = boxed_acc_sum, aes(x = num_obj, y = accuracy, color = type,)) + facet_grid(type+Pilot_Num~Session)+
  stat_summary(fun.y = "mean", geom = "point", size = 2) + stat_summary(fun.y = "mean", geom = "line", aes(linetype = Session)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2)+ ggtitle("Visual Search Accuracy") + th

# plot individual data 
if(make_ind_plot == 1) {
  boxed_RT_plot <- boxed_RT_plot  + geom_line ( aes(group = interaction(SN, Session, type), color  = SN), alpha = 0.5) + facet_wrap(type~Session)
  boxed_cost_plot <- boxed_cost_plot +  geom_line ( aes(group = interaction(SN,Diff), linetype = Diff, color = SN), alpha = 0.5) + th
  boxed_acc_plot <- boxed_acc_plot+ geom_line ( aes(group = interaction(SN, Session, type), linetype = Session, color=type), alpha = 0.5) + facet_wrap(type~Session)
  
  print(boxed_RT_plot)
  print(boxed_cost_plot)
  print(boxed_acc_plot)
} 

###### BRT ###### 

names(BRT_data)[names(BRT_data) == 'Response.Time'] <- 'RT'
BRT_data <- BRT_data %>% 
  mutate(accuracy = ifelse(Correct.Button == 1, 1, 0), RT = ifelse(accuracy == 1, RT, NaN)) %>%
  arrange(desc(SN))%>%
  mutate(tally=1)

BRT_SN_Check <- BRT_data %>%
  data_summary_count(varname = "tally", groupnames=c("SN", "Session"))

# summarize raw RT data 
BRT_sum <- data_summary_med(data = BRT_data, varname = "RT", groupnames = c( "Condition", "Session", "SN", "Pilot_Num"))

BRT_plot <- ggplot(data = BRT_sum, aes(x = Session, y = RT, group= interaction(Condition, Pilot_Num), color=SN))  + 
  geom_point()+
  stat_summary(fun.data = 'mean_se', geom = "pointrange", inherit.aes = FALSE, aes (x = Session, y = RT)) + 
  stat_summary(fun.y = "mean", geom = "line", inherit.aes = FALSE, aes(x = Session, y = RT, linetype = Session)) + 
  stat_summary(fun.y = "mean", geom = "point", size = 2) + stat_summary(fun.y = "mean", geom = "line") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2)+ ggtitle("Response Time")+
  facet_grid(Condition~Pilot_Num)+ th

# plot individual data 
if(make_ind_plot == 1) {
  BRT_plot <- BRT_plot +  geom_line ( aes(group = interaction(SN, Condition), color = SN), alpha = 0.5)
} 












