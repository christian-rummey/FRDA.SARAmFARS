# -------------------------------------------------------------------------
#' ---
#' title: "0.DM.R"
#' author: "Christian Rummey"
#' date: "`r format(Sys.time(), '%d %B, %Y, %H:%M')`"
#' output:
#'    html_document:
#'      toc: true
#'      code_folding: show
#'      number_sections: no
#'      toc_float: no
#'      highlight: tango
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(collapse = T)
knitr::opts_chunk$set(echo     = T)
knitr::opts_chunk$set(include  = T)
knitr::opts_chunk$set(warning  = F)

#' ### Data 

# packages ----------------------------------------------------------------

rm(list = ls())

scales.list <- c('FARSn',
                 'mFARS'   ,'SARA'      ,'ICARS',
                 'FARS.E'  ,'SARA.ax'   ,'ICARS.ax',
                 'FARS.BC' ,'SARA.ki'   ,'ICARS.ki',
                 'FARS.Am' ,'s4.speech' ,'ICARS.sp',
                 'ICARS.od')

mx.    <- .rt('../DATA other/scales.txt') %>% 
  filter(paramcd %in% scales.list) %>% select(paramcd, maxscore) %>% 
  mutate(paramcd = factor(paramcd, scales.list))

steps. <- .dd.FA('steps')

# data for progression ----------------------------------------------------

dt. <- bind_rows(
  .dd.FA('icars.slope.chg'), 
  .dd.FA('sara.slope.chg'),  
  .dd.FA('fars.slope.chg')
  ) %>% 
  filter(study == 'FACOMS') %>% 
  mutate(paramcd = factor(paramcd, scales.list)) %>% 
  filter(!is.na(paramcd)) %>% 
  select(-c('fpf', 'hpf', 'bl', 'bl.age','adt', 'neuro.score', 'dev.y')) %>% 
  droplevels() 

dt. <- .dd.FA('icars') %>% filter(paramcd == 'ICARS') %>% select(study, sjid, avisitn) %>% 
  left_join(dt.)

dt. %<>% 
  left_join(mx.)

dt.pct <- dt. %>% 
  mutate(aval = 100*aval/maxscore, chg = 100*chg/maxscore) %>% 
  mutate(type = 'pct')

dt. %<>% 
  mutate(type = 'val') %>% 
  bind_rows(dt.pct)

rm(dt.pct)

dt. %<>%
  filter(!is.na(age))

# dt. %<>% 
#   mutate( age.grp = cut(age, c(0,8,12,16,25,40,100), labels = c('<8y', '8-11y', '12-15y', '16-24y',           '25-40y', '>40y' ), right = T))

with(dt., table(int, forslope, type, exclude = F))

# adjust differential staring times to this subset ----------------------------------------

dt. %<>%
  group_by(sjid, paramcd, amb, type) %>%
  arrange(sjid, avisitn, paramcd, type) %>% 
  mutate(time. = time. - min(time.))

# complete it -------------------------------------------------------------

dt. %<>% 
  left_join(steps. %>% select(sjid, avisitn, fds = fds.act, step))

# write -------------------------------------------------------------------

dt. %<>%
  mutate(fds = ifelse(is.na(fds), 2, fds)) 

dt. %>% # 4362, avis 6
  .wds('DATA derived/long.data')

dt. %>% 
  ungroup %>% filter(forslope == 1) %>% 
  select(paramcd) %>% 
  table()
