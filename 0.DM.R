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

# data --------------------------------------------------------------------

pars       <- c(
  'SARA',  'SARA.ax', 'SARA.ki',
  'mFARS', 'FARS.E' , 'FARS.B',
  'ICARS', 'ICARS.gp', 'ICARS.ki'
)


dt. <- bind_rows(
  .dd.FA('fars')  %>% filter(study == 'FACOMS', paramcd %in% pars),
  .dd.FA('icars') %>% filter(study == 'FACOMS', paramcd %in% pars),
  .dd.FA('sara')  %>% filter(study == 'FACOMS', paramcd %in% pars)
  ) %>% 
  select(-fpf, -hpf) %>% 
  spread( paramcd, aval ) %>% 
  filter( !is.na(SARA) | !is.na(ICARS)) %>% 
  gather( paramcd, aval, pars) %>% 
  .add.time() %>% 
  .add.demo(l = 3 ) %>% 
  left_join( .dd.FA('steps') %>% select(sjid, avisitn, amb)  )

dt. %>% 
  .wds('DATA derived/long.data')

# data for progression ----------------------------------------------------

dt. <- bind_rows(
  .dd.FA('icars.slope.chg') %>% filter(paramcd == 'ICARS'),
  .dd.FA('sara.slope.chg') %>% filter(paramcd == 'SARA'),
  .dd.FA('fars.slope.chg') %>% filter(paramcd == 'mFARS')
) %>% 
  droplevels() %>% 
  select(-fpf, -hpf)

dt. <- .dd.FA('icars') %>% filter(paramcd == 'ICARS') %>% select(study, sjid, avisitn) %>% 
  left_join(dt.)

with(dt., table(int, forslope, exclude = F))

dt. %>% 
  filter(sjid == 4201, paramcd == 'mFARS') %>% 
  # filter(!is.na(int))
  mutate()

levels( dt.$paramcd )
levels( dt.$age.grp )
range ( dt.$age )

dt. %<>% 
  mutate( age.grp = cut(age, c(0,8,12,16,25,40,100), labels = c('<8y', '8-11y', '12-15y', '16-24y',           '25-40y', '>40y' ), right = T))
  

# demo. <- dt. %>% 
#   group_by( sjid ) %>% 
#   filter  ( avisitn == min(avisitn) ) %>% 
#   select  ( study, sjid, avisitn, paramcd, aval) %>% 
#   spread  ( paramcd, aval)


# dt.  <- read_excel('../DATA/Melbourne/Copy of FACOMS SARA ICARS.xlsx') %>% 
#   mutate( adt = as.Date(`Assessment date`)) %>% 
#   rename( sjid = `CCRN Subject ID`) %>% 
#   select(-`Assessment date`) %>% 
#   mutate_at('sjid', as.character) %>% 
#   select(sjid, adt, everything())
# 
# fars. <- .dd.FA('FARS') %>% 
#   filter( paramcd %in% c('mFARS','FARSn','FARS.E')) %>% 
#   spread( paramcd, aval ) %>% 
#   select( sjid, adt, avisitn, mFARS, FARSn, FARS.E )
# 
# dt. %<>% 
#   select(sjid, adt, SARA_Total, ICARS_Total) %>% 
#   left_join(fars.)
# 
# dt.missing <- dt. %>% 
#   filter(is.na(avisitn))
# 
# dt.missing %>% 
#   ggplot()+geom_histogram()+
#   aes(x = adt)+
#   scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10))
# 
# dt.missing %<>% 
#   mutate(yov = lubridate::year(adt)) %>% 
#   rename(adt.melbourne = adt ) %>% 
#   select(-avisitn)
# 
# .ds.FACOMS('faneuroe') %>% 
#   select(sjid, avisitn, adt) %>% 
#   mutate(yov = lubridate::year(adt)) %>% 
#   rename(adt.facoms = adt ) %>% 
#   right_join( dt.missing ) %>% 
#   select(sjid, avisitn, yov, adt.facoms, adt.melbourne) %>% 
#   mutate(adt.facoms - adt.melbourne) %>% 
#   arrange(sjid, adt.facoms) %>% 
#   as.data.frame()
# 
# 
# dt. %<>% 
#   filter(!is.na(mFARS)) %>% 
#   rename(SARA = SARA_Total, ICARS = ICARS_Total)
# 
# 
# # . -----------------------------------------------------------------------
# 
# dt. %>% 
#   left_join(.dd.FA('steps') %>% select(sjid, avisitn, amb)) %>% 
#   ggplot()+geom_jitter()+
#   aes(x = FARS.E)+
#   aes(y = SARA)+
#   aes(color = amb)+
#   geom_smooth(method = 'lm', se = F)+
#   ggpubr::stat_cor()+
#   geom_line(aes(group = sjid))
# 
# 
# 
