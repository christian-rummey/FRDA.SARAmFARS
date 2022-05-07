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

steps. <- .dd.FA('steps')

# once and for all --------------------------------------------------------

scales.list <- c('FARSn',
                 'mFARS'   ,'SARA'      ,'ICARS',
                 'FARS.E'  ,'SARA.ax'   ,'ICARS.ax',
                 'FARS.BC' ,'SARA.ki'   ,'ICARS.ki',
                 'FARS.Am' ,'s4.speech' ,'ICARS.sp',
                 'ICARS.od')

scales <- .rt('../DATA other/scales.txt') %>% 
  select(-pl2) %>% 
  filter(paramcd %in% scales.list) %>% 
  mutate(score = case_when(
    paramcd %in% scales.list[c(1,2,5,8,11)]      ~ 'mFARS',
    paramcd %in% scales.list[c(3,6,9,12)]        ~ 'SARA',
    paramcd %in% scales.list[c(4,7,10,13,14)] ~ 'ICARS'
  )) %>%
  mutate(score.max = case_when(
    score == 'mFARS' ~  93,
    score == 'SARA'  ~  40,
    score == 'ICARS' ~ 100
  )) %>% 
  mutate(score.type = case_when(
    paramcd %in% scales.list[c(  5, 6, 7 )]  ~ 'Axial Function',
    paramcd %in% scales.list[c(  8, 9, 10)]  ~ 'Kinetic Function',
    paramcd %in% scales.list[c( 11,12, 13)]  ~ 'Speech Disorder',
    paramcd %in% scales.list[c( 14       )]  ~ 'Oculomotor Disorder',
    TRUE ~ 'Total Score'
  )) %>% 
  mutate(paramcd = factor(paramcd, scales.list))

scales %>% 
  saveRDS('DATA derived/scales.rds')

rm(scales.list)

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
  left_join(scales %>% select(paramcd, maxscore))

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

# correct data ------------------------------------------------------------

dt. %<>%
  mutate(fds = ifelse(is.na(fds) & sjid == 4362, 2, fds)) 

# correlated variables ----------------------------------------------------

mx.    <- .rt('../DATA other/scales.txt') %>% 
  filter(paramcd %in% scales.list) %>% select(paramcd, maxscore) %>% 
  mutate(paramcd = factor(paramcd, scales.list))

dt. %>% 
  # filter ( type == 'pct', type2 == 'all' ) %>% 
  select ( type, type2, study, sjid, avisitn, age, paramcd, aval, amb, fds ) %>% 
  saveRDS('DATA derived/dt.rds')

dt.w <- dt. %>%
  select(-maxscore, -time.) %>% 
  spread(paramcd, aval) 

scores. <- bind_rows(
  dt.w %>% mutate(dep.val = mFARS   , dep = 'mFARS' )    %>% select(-scales.list[!(scales.list %in% scales.list[c(1,3:4)] )]) %>% gather(ind, ind.val, scales.list[c(1,3:4)]),
  dt.w %>% mutate(dep.val = SARA    , dep = 'SARA'  )    %>% select(-scales.list[!(scales.list %in% scales.list[c(2,4)]   )]) %>% gather(ind, ind.val, scales.list[c(2,4)]  ),
  dt.w %>% mutate(dep.val = ICARS   , dep = 'ICARS' )    %>% select(-scales.list[!(scales.list %in% scales.list[c(2,3)]   )]) %>% gather(ind, ind.val, scales.list[c(2,3)]  ),
  dt.w %>% mutate(dep.val = FARS.E  , dep = 'FARS.E'  )  %>% select(-scales.list[!(scales.list %in% scales.list[c(6,7)] )])   %>% gather(ind, ind.val, scales.list[c(6,7)]  ),
  dt.w %>% mutate(dep.val = SARA.ax , dep = 'SARA.ax' )  %>% select(-scales.list[!(scales.list %in% scales.list[c(5,7)] )])   %>% gather(ind, ind.val, scales.list[c(5,7)]  ),
  dt.w %>% mutate(dep.val = ICARS.ax, dep = 'ICARS.ax')  %>% select(-scales.list[!(scales.list %in% scales.list[c(5,6)] )])   %>% gather(ind, ind.val, scales.list[c(5,6)]  ),
  dt.w %>% mutate(dep.val = FARS.BC , dep = 'FARS.BC' )  %>% select(-scales.list[!(scales.list %in% scales.list[c( 9,10)] )]) %>% gather(ind, ind.val, scales.list[c(9,10)] ),
  dt.w %>% mutate(dep.val = SARA.ki , dep = 'SARA.ki' )  %>% select(-scales.list[!(scales.list %in% scales.list[c( 8,10)] )]) %>% gather(ind, ind.val, scales.list[c(8,10)] ),
  dt.w %>% mutate(dep.val = ICARS.ki, dep = 'ICARS.ki')  %>% select(-scales.list[!(scales.list %in% scales.list[c( 8, 9)] )]) %>% gather(ind, ind.val, scales.list[c(8,9)]  ),
  dt.w %>% mutate(dep.val = mFARS   , dep = 'FARS.Am'  ) %>% select(-scales.list[!(scales.list %in% scales.list[c(12,13)] )]) %>% gather(ind, ind.val, scales.list[c(12,13)]),
  dt.w %>% mutate(dep.val = mFARS   , dep = 's4.speech') %>% select(-scales.list[!(scales.list %in% scales.list[c(11,13)] )]) %>% gather(ind, ind.val, scales.list[c(11,13)]),
  dt.w %>% mutate(dep.val = mFARS   , dep = 'ICARS.sp' ) %>% select(-scales.list[!(scales.list %in% scales.list[c(11,12)] )]) %>% gather(ind, ind.val, scales.list[c(11,12)])
) 

scores. %<>% 
  ungroup %>% 
  select( type, type2, study, sjid, avisitn, age, amb, fds, dep, ind, dep.val, ind.val)

# model and add predicted values ----------------------------------------------------

lm.mod <- scores. %>% 
  filter(type == 'pct', type2 == 'all') %>% 
  # filter(dep == 'SARA.ax', ind == 'FARS.E') %>% 
  group_by(type, type2, dep, ind) %>% 
  filter (!is.na(ind.val), !is.na(dep.val)) %>% 
  nest   () %>% 
  mutate ( mod.lm = map( data  , ~ lm ( dep.val ~ ind.val, data = .))) %>% 
  mutate ( mod.pl = map( data  , ~ lm ( dep.val ~ poly(ind.val,2, raw=TRUE), data = .))) %>% 
  mutate ( pred.lm = map2( mod.lm, data, predict )) %>% 
  mutate ( pred.pl = map2( mod.pl, data, predict ))

lm.mod %>% 
  saveRDS('DATA derived/models.predictions.rds')
