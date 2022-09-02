library(tidyverse)

# our data is stored in a folder called `Balance`.
# within that, is each participant folder.
# within those, is the session folder for each participant.

# first, we load all the trial_results files.
# we do this by recursively searching all files that match a pattern.
# then reading all the files we find (with `map`) and binding them row-wise (`_dfr`)

posture_trials <- list.files(
  path = "~/Dropbox/VR_TREADMILL/DATA/Second_Pilot(Oct_Nov)/Balance",
  pattern = "trial_results.csv",
  full.names = TRUE,
  recursive = TRUE
) %>%
  map_dfr(read_csv)

# now we want the head movement data for every trial.
# we can do this by reading each filename given in the `*_movement_filename` column.
# then we can use `unnest` to give 1 row per timestep. 

posture_movement <- posture_trials %>% 
  mutate(center_eye_movement = map(file.path("~/Dropbox/VR_TREADMILL/DATA/Second_Pilot(Oct_Nov)/Balance", directory, center_eye_movement_filename), read_csv)) %>% 
  unnest()

# we can now write a function which calculates path length and summarise by it.
# grouping will give us 1 row per trial again

calculate_path_length <- function(x, y, z) {
  # calculates sum of point-to-point distances.
  sum(
    (diff(x) ^ 2 +
       diff(y) ^ 2 +
       diff(z) ^ 2) ^ 0.5
  )
}

posture_summary <- posture_movement %>% 
  group_by(experiment, ppid, session_num, trial_num, assessment_type) %>% 
  summarise(path_length = calculate_path_length(pos_x, pos_y, pos_z))

# we can plot these data on a graph

ggplot(posture_summary, aes(x = assessment_type, y = path_length, color = assessment_type)) + 
  geom_point() +
  xlab("Vision conditions") +
  ylab("Head path length (m)") 

# Note: Head path length (where higher values indicate worse posturla stability) as a function of vision condition. 

# we can also plot with dot plot
p <- ggplot(posture_summary, aes(x = assessment_type, y = path_length, color = assessment_type, fill = assessment_type)) + 
  geom_dotplot(binaxis='y', stackdir='center') +
  xlab("Vision conditions") +
  ylab("Head path length (m)") 
# Use geom_pointrange()
p + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                 geom="pointrange", color="black")

