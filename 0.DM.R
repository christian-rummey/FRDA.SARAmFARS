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
  filter( forslope == 1 ) %>% #basta
  select(-c(int, chg, forslope)) %>% 
  group_by(sjid, avisitn, paramcd) %>% 
  droplevels() 

dt. <- .dd.FA('icars') %>% filter(study == 'FACOMS', paramcd == 'ICARS') %>% select(study, sjid, avisitn) %>% 
  left_join(dt.) %>% 
  group_by(sjid, avisitn)

dt. %<>% 
  left_join(mx.)

dt.pct <- dt. %>% 
  mutate(aval = 100*aval/maxscore) %>% 
  mutate(type = 'pct')

dt. %<>% 
  mutate(type = 'val') %>% 
  bind_rows(dt.pct)

rm(dt.pct)

dt. %<>%
  filter(!is.na(age))

# dt. %<>% 
#   mutate( age.grp = cut(age, c(0,8,12,16,25,40,100), labels = c('<8y', '8-11y', '12-15y', '16-24y',           '25-40y', '>40y' ), right = T))

with(dt., table(type, paramcd, exclude = F))

# adjust differential staring times to this subset ------------------------

dt. %<>%
  group_by(sjid, paramcd, amb, type) %>%
  arrange(sjid, avisitn, paramcd, type) %>% 
  mutate(time. = time. - min(time.))

# first.visit -------------------------------------------------------------

dt. %<>% 
  mutate(type2 = 'all') %>% 
  bind_rows(
    dt. %>%
      group_by(sjid, paramcd) %>% 
      filter(avisitn == min(avisitn)) %>% 
      mutate(type2 = 'first')
  )

# complete it -------------------------------------------------------------

dt. %<>% 
  left_join(steps. %>% select(sjid, avisitn, fds = fds.act))

# write -------------------------------------------------------------------

dt. %<>%
  mutate(fds = ifelse(is.na(fds) & sjid == 4362, 2, fds)) 

# dt. %>% # 4362, avis 6
#   .wds('DATA derived/long.data')


# correlated variables ----------------------------------------------------

scales.list <- c('FARSn',
                 'mFARS'   ,'SARA'      ,'ICARS',
                 'FARS.E'  ,'SARA.ax'   ,'ICARS.ax',
                 'FARS.BC' ,'SARA.ki'   ,'ICARS.ki',
                 'FARS.Am' ,'s4.speech' ,'ICARS.sp',
                 'ICARS.od')

mx.    <- .rt('../DATA other/scales.txt') %>% 
  filter(paramcd %in% scales.list) %>% select(paramcd, maxscore) %>% 
  mutate(paramcd = factor(paramcd, scales.list))

dt.w <- dt. %>%
  spread(paramcd, aval)

scores. <- bind_rows(
  dt.w %>% mutate(score = mFARS, dep = 'mFARS') %>% select(-scales.list[!(scales.list %in% scales.list[c(1,3:4)] )]) %>% gather(indep, value, scales.list[c(1,3:4)]),
  dt.w %>% mutate(score = SARA , dep = 'SARA' ) %>% select(-scales.list[!(scales.list %in% scales.list[c(2,4)]   )]) %>% gather(indep, value, scales.list[c(2,4)]),
  dt.w %>% mutate(score = ICARS, dep = 'ICARS') %>% select(-scales.list[!(scales.list %in% scales.list[c(2,3)]   )]) %>% gather(indep, value, scales.list[c(2,3)]),
  dt.w %>% mutate(score = FARS.E  , dep = 'FARS.E'  ) %>% select(-scales.list[!(scales.list %in% scales.list[c(6,7)] )]) %>% gather(indep, value, scales.list[c(6,7)]),
  dt.w %>% mutate(score = SARA.ax , dep = 'SARA.ax' ) %>% select(-scales.list[!(scales.list %in% scales.list[c(5,7)] )]) %>% gather(indep, value, scales.list[c(5,7)]),
  dt.w %>% mutate(score = ICARS.ax, dep = 'ICARS.ax') %>% select(-scales.list[!(scales.list %in% scales.list[c(5,6)] )]) %>% gather(indep, value, scales.list[c(5,6)]),
  dt.w %>% mutate(score = FARS.BC , dep = 'FARS.BC' ) %>% select(-scales.list[!(scales.list %in% scales.list[c( 9,10)] )]) %>% gather(indep, value, scales.list[c(9,10)]),
  dt.w %>% mutate(score = SARA.ki , dep = 'SARA.ki' ) %>% select(-scales.list[!(scales.list %in% scales.list[c( 8,10)] )]) %>% gather(indep, value, scales.list[c(8,10)]),
  dt.w %>% mutate(score = ICARS.ki, dep = 'ICARS.ki') %>% select(-scales.list[!(scales.list %in% scales.list[c( 8, 9)] )]) %>% gather(indep, value, scales.list[c(8,9)]),
  dt.w %>% mutate(score = mFARS, dep = 'FARS.Am'  ) %>% select(-scales.list[!(scales.list %in% scales.list[c(12,13)] )]) %>% gather(indep, value, scales.list[c(12,13)]),
  dt.w %>% mutate(score = mFARS, dep = 's4.speech') %>% select(-scales.list[!(scales.list %in% scales.list[c(11,13)] )]) %>% gather(indep, value, scales.list[c(11,13)]),
  dt.w %>% mutate(score = mFARS, dep = 'ICARS.sp' ) %>% select(-scales.list[!(scales.list %in% scales.list[c(11,12)] )]) %>% gather(indep, value, scales.list[c(11,12)])
) 

# add a split

scores.amb <- scores. %>% 
  filter( amb == 'ambulatory' ) %>% 
  group_by( dep, indep ) %>% 
  filter(  dep %in% scales.list[c(2,3,4,5,6,7)]) %>% 
  filter(indep %in% scales.list[c(2,3,4,5,6,7)]) %>% 
  # left_join(mx. %>% mutate(maxscore = maxscore*.5) %>% rename(indep = paramcd, split.val = maxscore)) %>%
  # mutate ( split.val = case_when (
  #   indep == 'SARA' ~ 9,
  #   indep == 'ICARS', 23, 
  #   indep == 'mFARS', 21,
  #   indep == 'FARSn', 24, 
  #   NA)) %>%
  filter ( !is.na(value) ) %>% 
  mutate ( split.val = median(value) ) %>% 
  mutate ( split = ifelse(value < split.val, 'low','hig') )

scores.    %>% saveRDS('DATA derived/scores.rds')
scores.amb %>% saveRDS('DATA derived/scores.amb.rds')

