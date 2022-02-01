# Randomization check ==========================================================

basedir <- "/Users/bernhardclemm/Dropbox/Mac/Documents/Academia/EXPO/repositories/ERCEXPO_OUTPUT/null-effects-news-experiments/"
datadir = paste0(basedir, "Data/")
tabdir = paste0(basedir, "Output/Tables/")
figdir = paste0(basedir, "Output/Figures/")

path_base <- "/Users/bernhardclemm/Dropbox/Mac/Documents/Academia/EXPO/repositories/EXPO2.0/"
path_data <- paste0(path_base, "Projects/Other-98/data/")
path_tools <- paste0(path_base, "Tools/Survey recoding joining/")

# Read in data like in other project

source(paste0(path_tools, "survey-recoding-functions_PL.R"))
pl_w1 <- read_sav(paste0(path_data, "surveys_pl/PL_survey_w1_raw_filtered.sav"))
pl_w2 <- read_sav(paste0(path_data, "surveys_pl/PL_survey_w2_raw_filtered.sav")) 
pl_w3 <- read_sav(paste0(path_data, "surveys_pl/PL_survey_w3_raw_filtered.sav")) 
pl_survey_00 <- recode_join_PL(pl_w1, pl_w2, pl_w3) %>%
  mutate(country = "PL")

# trace data
visits_pl = read.csv(paste0(datadir, "pl_trace_visits_NEW.csv")) %>%
  filter(more_news_rel %in% c("before_more_news", "during_more_news", "after_more_news")) 

visits_pl_wide <- visits_pl %>%
  group_by(person_id, more_news_rel) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = "more_news_rel", values_from = "count")

# join data
data_pl <- pl_survey_00 %>%
  select(person_id, wave2kontrolna,
         wave2kontrolna1,
         wave2eksperymentalna,
         wave2eksperymentalna1) %>%
  mutate(condition = case_when(
    !is.na(wave2kontrolna) ~ "control",
    !is.na(wave2eksperymentalna) ~ "treatment")) %>%
  filter(!is.na(condition)) %>%
  unite(wave2kontrolna, wave2eksperymentalna, col = "participation_accept1") %>%
  unite(wave2kontrolna1, wave2eksperymentalna1, col = "participation_accept2") %>%
  mutate(across(starts_with("participation_accept"), 
                ~ gsub("_NA|NA_|NA", "", .))) %>%
  left_join(., visits_pl_wide) %>%
  mutate(across(ends_with("more_news"),
                ~ ifelse(is.na(.), 0, .)))

data_pl_long <- data_pl %>%
  select(person_id, condition, before_more_news, during_more_news) %>%
  pivot_longer(before_more_news:during_more_news)
  
summary_assignment <- data_pl_long %>%
  group_by(condition, name) %>%
  summarise(n = n(), 
            visits_mean = mean(value, na.rm = T),
            visits_sum = sum(value, na.rm = T)) 

ggplot(data_pl_long %>% filter(name == "during_more_news"),
       aes(y = value, x = condition)) +
  geom_point()
  

summary_accept1 <- data_pl %>%
  filter(participation_accept1 == 1) %>%
  group_by(condition) %>%
  summarise(n = n(), 
            visits_before = sum(before_more_news, na.rm = T),
            visits_during = sum(during_more_news, na.rm = T))%>%
  mutate(visits_before_pp = visits_before/n,
         visits_during_pp = visits_during/n)

summary_accept2 <- data_pl %>%
  filter(participation_accept2 == 1) %>%
  group_by(condition) %>%
  summarise(n = n(), 
            visits_before = sum(before_more_news, na.rm = T),
            visits_during = sum(during_more_news, na.rm = T))%>%
  mutate(visits_before_pp = visits_before/n,
         visits_during_pp = visits_during/n)
    
  