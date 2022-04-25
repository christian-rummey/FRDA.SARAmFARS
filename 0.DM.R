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

# data for progression ----------------------------------------------------

dt. <- bind_rows(
  .dd.FA('icars.slope.chg') %>% filter(paramcd %in% c('ICARS','ICARS.ax','ICARS.ki','ICARS.od', .l.icars.ax)),
  .dd.FA('sara.slope.chg')  %>% filter(paramcd %in% c('SARA','SARA.ax','SARA.ki','s4.speech', .l.sara.ax)),
  .dd.FA('fars.slope.chg')  %>% filter(paramcd %in% c('FARSn','mFARS','FARS.E','FARS.BC','FARS.B','FARS.Am', .l.FARS.E))
) %>% 
  droplevels() %>% 
  select(-c('fpf', 'hpf', 'bl', 'bl.age')) %>% 
  filter(study == 'FACOMS')

dt. <- .dd.FA('icars') %>% filter(paramcd == 'ICARS') %>% select(study, sjid, avisitn) %>% 
  left_join(dt.)

with(dt., table(int, forslope, exclude = F))

dt. %>% 
  filter(sjid == 4383) %>% 
  # filter(sjid == 4201, paramcd == 'mFARS') %>% 
  # filter(!is.na(int))
  mutate()

dt. %<>% 
    filter(!is.na(age))

dt. %<>% 
  mutate( age.grp = cut(age, c(0,8,12,16,25,40,100), labels = c('<8y', '8-11y', '12-15y', '16-24y',           '25-40y', '>40y' ), right = T))


# adjust differential staring times to this subset ----------------------------------------

dt. %<>%
  group_by(sjid, paramcd, amb) %>%
  mutate(time. = time. - min(time.))


# initial version ---------------------------------------------------------

# pars       <- c(
#   'SARA',  'SARA.ax', 'SARA.ki',
#   'mFARS', 'FARS.E' , 'FARS.B',
#   'ICARS', 'ICARS.gp', 'ICARS.ki'
# )
# 
# dt. <- bind_rows(
#   .dd.FA('fars')  %>% filter(study == 'FACOMS', paramcd %in% pars),
#   .dd.FA('icars') %>% filter(study == 'FACOMS', paramcd %in% pars),
#   .dd.FA('sara')  %>% filter(study == 'FACOMS', paramcd %in% pars)
#   ) %>% 
#   select(-fpf, -hpf) %>% 
#   spread( paramcd, aval ) %>% 
#   filter( !is.na(SARA) | !is.na(ICARS)) %>% 
#   gather( paramcd, aval, pars) %>% 
#   .add.time() %>% 
#   .add.demo(l = 3 ) %>% 
#   left_join( .dd.FA('steps') %>% select(sjid, avisitn, amb)  )
# 
# write -------------------------------------------------------------------

dt. %>%
  .wds('DATA derived/long.data')

  
dt. %>% 
  ungroup %>% filter(forslope == 1) %>% 
  select(paramcd) %>% 
  table()

levels( dt.$age.grp )
range ( dt.$age )

dt. %>% group_by(sjid, avisitn) %>% 
  filter(paramcd == 'SARA')

dt. %>% filter(avisitn == 11, sjid == 4208, forslope == 1) %>% ungroup 