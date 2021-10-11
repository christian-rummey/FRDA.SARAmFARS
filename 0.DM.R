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

# pars       <- c(
#   'SARA',  .l.sara, 'SARA.gp', 'SARA.ki',
#   'ADL' ,  .l.adl,
#   'mFARS'  .l.mFARS, 'FARS.Ex', 'FARS.rc1',
#   'hpt.i', 'w25.i',
#   'hpt.iu', 'w25.iu',
#   'ccfs.f', 'ccfs.h',
#   'bva','od','os','ou'
# )

dt.  <- read_excel('../DATA/Melbourne/Copy of FACOMS SARA ICARS.xlsx') %>% 
  mutate( adt = as.Date(`Assessment date`)) %>% 
  rename( sjid = `CCRN Subject ID`) %>% 
  select(-`Assessment date`) %>% 
  mutate_at('sjid', as.character) %>% 
  select(sjid, adt, everything())

fars. <- .dd.FA('FARS') %>% 
  filter( paramcd %in% c('mFARS','FARSn','FARS.E')) %>% 
  spread( paramcd, aval ) %>% 
  select( sjid, adt, avisitn, mFARS, FARSn, FARS.E )

dt. %<>% 
  select(sjid, adt, SARA_Total, ICARS_Total) %>% 
  left_join(fars.)

dt.missing <- dt. %>% 
  filter(is.na(avisitn))

dt.missing %>% 
  ggplot()+geom_histogram()+
  aes(x = adt)+
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10))

dt.missing %<>% 
  mutate(yov = lubridate::year(adt)) %>% 
  rename(adt.melbourne = adt ) %>% 
  select(-avisitn)

.ds.FACOMS('faneuroe') %>% 
  select(sjid, avisitn, adt) %>% 
  mutate(yov = lubridate::year(adt)) %>% 
  rename(adt.facoms = adt ) %>% 
  right_join( dt.missing ) %>% 
  select(sjid, avisitn, yov, adt.facoms, adt.melbourne) %>% 
  mutate(adt.facoms - adt.melbourne) %>% 
  arrange(sjid, adt.facoms) %>% 
  as.data.frame()


dt. %<>% 
  filter(!is.na(mFARS)) %>% 
  rename(SARA = SARA_Total, ICARS = ICARS_Total)


# . -----------------------------------------------------------------------

dt. %>% 
  left_join(.dd.FA('steps') %>% select(sjid, avisitn, amb)) %>% 
  ggplot()+geom_jitter()+
  aes(x = FARS.E)+
  aes(y = SARA)+
  aes(color = amb)+
  geom_smooth(method = 'lm', se = F)+
  ggpubr::stat_cor()+
  geom_line(aes(group = sjid))



