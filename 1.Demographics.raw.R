#' ---
#' title: "Demographics & Structure"
#' author: "Christian Rummey"
#' date: "`r format(Sys.time(), '%d %B, %Y, %H:%M')`"
#' output:
#'    word_document:
#'      number_sections: yes
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(cache    = F)
knitr::opts_chunk$set(echo     = F)
knitr::opts_chunk$set(include  = T)
knitr::opts_chunk$set(warning  = F)
knitr::opts_chunk$set(message  = F)
knitr::opts_chunk$set(fig.width=6, fig.height=4) 

rm(list=ls())

require(tableone)
require(labelled)
require(jstable)

# Data Baseline Table & Structure -----------------------------------------

dt. <- readRDS('DATA derived/long.data.rds')

dt. %<>% 
  ungroup %>%
  # filter( forslope == 1) %>%
  select( study, sjid, avisitn, age, amb) %>% 
  unique() %>% 
  ungroup 

#' # Demographics 

# fullow-up time ----------------------------------------------------------

dt.fu <- dt. %>%
  # filter ( sjid == 4208 ) %>%
  mutate ( amb.n = as.numeric((factor(amb)))) %>% 
  select ( study, sjid, avisitn, age, amb, amb.n ) %>% 
  unique %>% mutate(paramcd = 'dummy') %>% 
  .duplicate_phase_visits() %>%
  group_by(sjid) %>% 
  mutate ( fu       = max(age)-min(age) ) %>% 
  group_by(sjid, phase.n) %>% 
  mutate ( fu.phase = max(age)-min(age) ) %>%
  filter ( dupline != T) %>%
  group_by(sjid) %>% 
  mutate ( age.first = min(age), age.last = max(age) ) %>%
  mutate ( status = min(amb.n), status.last = max(amb.n)) %>% 
  select ( study, sjid, age.first, age.last, amb, fu, fu.phase, status, status.last) %>% 
  unique %>% 
  spread ( amb, fu.phase) %>% 
  rename ( fu.amb = ambulatory, fu.namb = `non-amb.`)

dt.fu %<>% 
  left_join(.dd.FA('demo') %>% select(sjid, site, sex, symp, diag, sev.o, gaa1, gaa2, pm, pm.grp)) %>% 
  mutate( since.d = age.first-diag) %>% 
  mutate( since.o = age.first-symp)  
  
dt.fu %<>% 
  mutate(no.fu = ifelse(fu==0, 1, 0)) %>% 
  mutate(pm = ifelse(pm == 0, 0, 1)) %>% 
  arrange(status)

dt.fu %<>% # this is new Apr 27 
  # select(-c(age, since.d)) %>% 
  unique %>% 
  group_by(sjid) %>% 
  mutate(
    fu.amb  = mean(fu.amb, na.rm=T),
    fu.namb = mean(fu.namb, na.rm=T)
    ) %>% 
  unique

# Demo Table by type----------------------------------------------------------------

var_label(dt.fu) <- list(sex    = 'Sex',
                      age.first = 'Age (BL)', 
                      symp      = 'Age of Onset', 
                      gaa1      = 'GAA1',
                      gaa2      = 'GAA2', 
                      pm        = 'PMs (%)',
                      sev.o     = 'Severity Group', #-------------------
                      since.d   = 'Time since Diagnosis',
                      since.o   = 'Disease Duration',
                      status    = 'Amb at Enrol.',
                      status.last  = 'Amb at Last Visit',
                      fu        = 'FU (years)',
                      no.fu     = 'No Follow-up (%)',
                      fu.amb    = 'FU (amb., y)',
                      fu.namb   = 'FU (non-amb., y)',
                      age.last  = 'Age (FU)'
)

tb <- jstable::CreateTableOne2(
  vars       = c( 'sex','age.first','symp','gaa1','gaa2','pm','since.d','since.o','status','status.last','fu','no.fu','fu.amb','fu.namb','age.last'),
  factorVars = c( 'pm', 'no.fu','status','status.last' ),
  nonnormal  = c( 'symp','fu','fu.amb','fu.namb' ),
  strata     = c( 'sev.o' ),
  data       = dt.fu 
  # addOverall = F
  # test = F
)    %>% 
  .ct

tb <- tableone::CreateTableOne(
  vars       = c( 'sex','age.first','symp','gaa1','gaa2','pm','since.d','since.o','status','status.last','fu','no.fu','fu.amb','fu.namb','age.last'),
  factorVars = c( 'pm', 'no.fu','status','status.last' ),
  strata     = c( 'sev.o' ),
  data       = dt.fu, 
  addOverall = T,
  test = F
  )    

tb %>% print(varLabels = T, 
             nonnormal  = c( 'symp','gaa1' ,'gaa2','age,first','age.last','sinced','fu','fu.amb','fu.namb' , 'pm', 'since.d','since.o'),
             contDigits = 1, 
             catDigits = 0,
             missing = F,
             explain = F, 
             dropEqual= F, 
             add.rownames = T,
             format = 'p',
             showAllLevels = TRUE
) %>% kableone()


#' # Structure

#' ## Visits

dt. %<>% 
  left_join(.dd.FA('demo') %>% select(sjid, sev.o))

dt. %<>%
  mutate( age.grp = cut(age, c(0,8,12,16,25,40,100), labels = c('<8y', '8-11y', '12-15y', '16-24y',           '25-40y', '>40y' ), right = T))

pct <- dt. %>% 
  group_by(age.grp) %>% 
  mutate(N = n()) %>% 
  group_by(age.grp, N, amb) %>% 
  summarise(n = n(), age = median(age)) %>% 
  mutate(pct = 100*n/N) %>% 
  filter(amb == 'ambulatory') %>%
  mutate(sev.o = factor('8-14y', levels(dt.$sev.o)))

dt. %>% 
  group_by(sjid) %>% 
  filter(avisitn == min(avisitn)) %>%
  group_by(sjid, amb) %>% 
  mutate(split = '1 recruited') %>% 
  bind_rows(dt. %>% mutate(split = '2 all visit')) %>% 
  # filter(split == '1 recruited') %>%
  filter(split == '2 all visit') %>%
  ggplot()+geom_bar(width=1)+
  aes(x = round(age))+
  scale_x_continuous(
    breaks = c( 8, 12, 16,25,40,50,60,70,80 )+.5,
    labels = c( 8, 12, 16,25,40,50,60,70,80 ),
  )+
  aes(fill = sev.o)+.sfbs1+
  facet_grid(amb~.)+
  .theme()+
  geom_vline(xintercept = c( 8, 12, 16, 25, 40 )+.5, linetype = 'dotted')+
  geom_text(aes(label = round(pct,0), y = -1), data = pct, size = 3)+
  coord_cartesian(xlim = c(5, 83))+
  .leg_tr+
  xlab('Age')+
  ylab('Visit Count')

# .sp()


#' ## Subjects/Visit %

dt <- dt. %>% 
  group_by(age.grp, sev.o) %>%
  summarise(n = n()) %>% 
  mutate(N = sum(n)) %>% 
  mutate(pct = round(100*n/N)) %>%
  select(-n, -N) %>% 
  spread(age.grp, pct, fill = '')

dt <- dt. %>% 
  group_by(sjid) %>% filter(avisitn == min(avisitn)) %>% 
  group_by(age.grp, sev.o) %>%
  summarise(n = n()) %>% 
  mutate(N = sum(n)) %>% 
  mutate(pct = round(100*n/N)) %>%
  select(-n, -N) %>% 
  spread(age.grp, pct, fill = '') %>% 
  left_join(dt, by = 'sev.o')

dt %>% 
  mutate(   '<8y' = paste(   `<8y.x`, '\n(' ,   `<8y.y`, ')',sep = '')) %>% 
  mutate( '8-11y' = paste( `8-11y.x`, '\n(' , `8-11y.y`, ')',sep = '')) %>% 
  mutate('12-15y' = paste(`12-15y.x`, '\n(' ,`12-15y.y`, ')',sep = '')) %>% 
  mutate('16-24y' = paste(`16-24y.x`, '\n(' ,`16-24y.y`, ')',sep = '')) %>% 
  mutate('25-40y' = paste(`25-40y.x`, '\n(' ,`25-40y.y`, ')',sep = '')) %>% 
  mutate(  '>40y' = paste(  `>40y.x`, '\n(' ,  `>40y.y`, ')',sep = '')) %>% 
  select(-contains('.x'), -contains('.y')) %>% 
  # .ct()
  flextable


