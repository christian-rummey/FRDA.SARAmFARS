
# Graph that compares 3 Scales, by items and type of function -------------

scales <- .rt('../DATA other/scales.txt') %>% 
  filter(paramcd %in% c('SARA.ax', 'SARA.ki','s4.speech', 'ICARS.ax', 'ICARS.ki', 'ICARS.od','ICARS.sp', 'FARS.E' ,'FARS.BC', 'FARS.Am')) %>% 
  mutate(score = case_when(
    paramcd %in% c('FARS.E' ,'FARS.BC', 'FARS.Am')     ~ 'mFARS',
    paramcd %in% c('SARA.ax', 'SARA.ki','s4.speech')   ~ 'SARA' ,
    paramcd %in% c('ICARS.ax', 'ICARS.ki', 'ICARS.od', 'ICARS.sp') ~ 'ICARS'
  )) %>% 
  mutate(score.max = case_when(
    paramcd %in% c('FARS.E' ,'FARS.BC', 'FARS.Am')     ~ 93,
    paramcd %in% c('SARA.ax', 'SARA.ki','s4.speech')   ~ 40,
    paramcd %in% c('ICARS.ax', 'ICARS.ki', 'ICARS.od', 'ICARS.sp') ~ 100
  )) %>% 
  mutate(score.type = case_when(
    paramcd %in% c('SARA.ax', 'ICARS.ax', 'FARS.E')             ~ 'axial function',
    paramcd %in% c('SARA.ki', 'ICARS.ki', 'FARS.BC')  ~ 'kinetic function',
    TRUE ~ 'speech, bulbar function'
  )) %>%
  mutate( score = factor(score, c('mFARS','SARA','ICARS'))) %>% 
  mutate( paramcd = factor(paramcd, c(
    'FARS.Am','FARS.BC','FARS.E','ICARS.ax','ICARS.ki','ICARS.od','ICARS.sp','SARA.ax','s4.speech','SARA.ki'))) %>% 
  arrange(score, paramcd) %>% 
  group_by ( score) %>% 
  mutate   ( ranks = cumsum(maxscore) ) %>% 
  mutate   ( ranks.pct = 100*(ranks/score.max)) %>% 
  mutate   ( maxscore = 100*maxscore/score.max) %>% 
  select(score, paramcd, maxscore, ranks, ranks.pct, score.type)

# subscores graph ---------------------------------------------------------

scales %>% 
  mutate(ranks.end = lag(ranks.pct)) %>% 
  mutate(ranks.end = ifelse(is.na(ranks.end), 0, ranks.end)) %>% 
  mutate(text.y.value = (ranks.pct+ranks.end)/2) %>% 
  ggplot()+geom_rect(colour = 'black')+
  # aes(x = score)+
  aes(ymin = -ranks.pct, ymax = -ranks.end)+
  aes(xmin = -.5, xmax = .5)+
  aes(fill = score.type)+
  geom_text(aes(label = paramcd, y = -text.y.value, x = 0))+
  facet_wrap(~score)+
  theme_void()+
  scale_fill_brewer(palette = 'Set1', direction = -1)+
  theme(legend.position = 'top')

# item graph --------------------------------------------------------------

scales <- .rt('../DATA other/scales.txt') %>% 
  filter(paramcd %in% c(.l.sara, .l.mFARS, .l.icars)) %>% 
  mutate(score = case_when(
    paramcd %in% .l.mFARS ~ 'mFARS',
    paramcd %in% .l.sara  ~ 'SARA',
    paramcd %in% .l.icars ~ 'ICARS'
  )) %>% 
  mutate(score.max = case_when(
    paramcd %in% .l.mFARS ~ 93,
    paramcd %in% .l.sara  ~ 40,
    paramcd %in% .l.icars ~ 100
  )) %>% 
  mutate(score.type = case_when(
    paramcd %in% c(.l.sara.ax, .l.icars.ax, .l.FARS.E)             ~ 'axial function',
    paramcd %in% c(.l.sara.ki, .l.icars.ki, .l.FARS.B, .l.FARS.C)  ~ 'kinetic function',
    TRUE ~ 'speech, bulbar function'
  )) %>%
  group_by ( score) %>% 
  mutate   ( ranks = cumsum(maxscore) ) %>% 
  mutate   ( ranks.pct = 100*(ranks/score.max)) %>% 
  mutate   ( maxscore = 100*maxscore/score.max) %>% 
  select(score, paramcd, maxscore, ranks, ranks.pct, score.type)

scales %>% 
  mutate(score= factor(score, c('mFARS','SARA','ICARS'))) %>% 
  mutate(ranks.end = lag(ranks.pct)) %>% 
  mutate(ranks.end = ifelse(is.na(ranks.end), 0, ranks.end)) %>% 
  mutate(text.y.value = (ranks.pct+ranks.end)/2) %>% 
  ggplot()+geom_rect(colour = 'black')+
  # aes(x = score)+
  aes(ymin = -ranks.pct, ymax = -ranks.end)+
  aes(xmin = -.5, xmax = .5)+
  aes(fill = score.type)+
  geom_text(aes(label = paramcd, y = -text.y.value, x = 0))+
  facet_wrap(~score)+
  theme_void()+
  theme(legend.position = 'top')

