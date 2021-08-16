# DIRECTORIES 

basedir = paste0(dirname(dirname(
  rstudioapi::getSourceEditorContext()$path)), "/")
datadir = paste0(basedir, "Data/")
tabdir = paste0(basedir, "Output/Tables/")
figdir = paste0(basedir, "Output/Figures/")

# LIBRARIES

library(readxl)
library(haven)
library(dplyr)
library(writexl)
library(stringr)
library(panelr)
library(Hmisc)

# PARTICIPANT IDS 
# participant ids
pl_ids = left_join(read_sav(paste0(datadir, "pl_survey_post.sav")),
                   read.csv(paste0(datadir, "person_ids.csv")), 
                   by = "respondent_id")$person_id

us_ids = read.csv(paste0(datadir, "us_survey_post.csv"))$person_id

# FUNCTIONS

# function to add empty cells to dataframe
emptycell = function(n){x = rep("", n); return(x)}

# negation %in% function
`%notin%` = Negate(`%in%`)

# returns percentage for particular value of variable
getperc = function(var, value){
  
  out = data.frame(prop.table(table(var)))
  out = out[out$var == value, 2] * 100
  
  return(out)
}

# functions to read file and add suffix to column names
read_rename_sav = function(filename, suffix){
  
  df = read_sav(paste0(datadir, filename)) %>%
    setNames(., paste0(names(.), suffix))
  
  return(df)
}

read_rename_csv = function(filename, suffix){
  
  df = read.csv(paste0(datadir, filename)) %>%
    setNames(., paste0(names(.), suffix))
  
  return(df)
}

# function to find variable names in data frame
catchvar = function(df, string){
  out = list()
  for (var in string){
    out[[length(out) + 1]] = 
      names(df)[grepl(var, names(df), ignore.case = T)]
  }
  out = do.call(c, out)
  return(out)
}

# function to invert scales
invert = function(var){
  var = max(var, na.rm = T) - var
  return(var)
}

# function to rescale variables
rescale_100 = function(x){
  x = x - min(x, na.rm = T)
  x = (x / max(x, na.rm = T))*100
  return(x)
}

# function to calculate CIs
get_ci = function(mean, se, n, ci){
  cis = data.frame(
    lb = mean - (qnorm(ci)* se / sqrt(n)),
    ub = mean - (qnorm(ci)* se / sqrt(n))
  )
  
  return(cis)
}

# function to translate labels
translate_label = function(x){
  
  lab = attr(x, "label")
  lang = "unknown"
  
  if (!is.null(lab) & length(lab) == 1){lang = textcat::textcat(lab)} 
  if (lang == "polish"){
    
    apikey = "AIzaSyAzCGqsbh9LQN8AMCxePQcsMFlNWBZGqco"
    newlab = translateR::translate(content.vec = lab,
                                   google.api.key = apikey,
                                   source.lang = 'pl',
                                   target.lang = 'en')  
    
    attributes(x) = list(label = newlab)}
  return(x)}

# function to get label
get_label = function(x){
  lab = attr(x, "label")
  return(lab)
}

# function to create variable list 
get_varlist = function(dataframe){
  
  labs = lapply(dataframe, get_label)
  
  temp = list()
  for (i in 1:length(labs)){
    name = names(labs)[[i]]
    lab = labs[[i]]
    
    if (is.null(lab)){lab = "NA"}
    temp[[i]] = data.frame(name = name, lab = lab)
  }
  
  labs = do.call(rbind, temp)
  
  return(labs)
}

# function to find matching labels
match_labs = function(pattern, varlist){
  x = varlist[grepl(pattern, varlist[,2]),]
  return(x)
}

# function to turn lm models into tables 
model_to_table = function(model){
  x = summary(model)$coefficients %>% 
    data.frame() %>%
    mutate(variable = row.names(.)) %>%
    setNames(c("est", "se", "t", "p", "coeff")) %>% 
    select(., c("coeff", "est", "se", "p")) %>% 
    mutate(lb = est - (1.96*se)) %>%
    mutate(ub = est + (1.96*se))
  
  return(x)
}


