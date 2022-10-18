
source('0.DM.R')

# Graph that compares 3 Scales, by items and type of function -------------

scales <- .rt('../DATA other/scales.txt') %>% 
  filter(paramcd %in% scales.list) %>% 
  mutate(score = case_when(
    paramcd %in% scales.list[c(1,4,7,10)]     ~ 'mFARS',
    paramcd %in% scales.list[c(2,5,8,11)]     ~ 'SARA',
    paramcd %in% scales.list[c(3,6,9,12,13)]  ~ 'ICARS'
    )) %>%
  mutate(score.max = case_when(
    score == 'mFARS' ~  93,
    score == 'SARA'  ~  40,
    score == 'ICARS' ~ 100
    )) %>% 
  mutate(score.type = case_when(
    paramcd %in% scales.list[c(  4, 5, 6 )]  ~ 'axial function',
    paramcd %in% scales.list[c(  7, 8, 9 )]  ~ 'kinetic function',
    paramcd %in% scales.list[c( 10,11,12 )]  ~ 'speech disorder',
    paramcd %in% scales.list[c( 13       )]  ~ 'oculomotor disorder',
    TRUE ~ 'Total Score'
  ))

# subscores graph ---------------------------------------------------------

scales  %>%
  mutate  ( pct = paste( round(100*maxscore/score.max,0), '%', sep = '' ) ) %>% 
  mutate  ( paramcd = factor(paramcd, c('FARS.Am','FARS.BC','FARS.E','SARA.ax','s4.speech','SARA.ki','ICARS.ax','ICARS.ki','ICARS.sp','ICARS.od'))) %>% 
  mutate  ( score = factor(score, scales.list[c(1,2,3)])) %>% 
  filter  ( !is.na(paramcd)) %>% 
  arrange ( score, paramcd ) %>% 
  group_by( score ) %>% 
  mutate  ( ranks     = cumsum( maxscore ) ) %>% 
  mutate  ( ranks.pct = 100*(ranks/score.max )) %>% 
  mutate  ( maxscore  = 100*maxscore/score.max ) %>% 
  select  ( score, paramcd, maxscore, ranks, ranks.pct, score.type, pct) %>%
  mutate  ( ranks.end = lag(ranks.pct) ) %>% 
  mutate  ( ranks.end = ifelse(is.na(ranks.end), 0, ranks.end)) %>% 
  mutate  ( text.y.value = (ranks.pct+ranks.end)/2) %>% 
  ggplot()+geom_rect()+
  aes     ( ymin = -ranks.pct, ymax = -ranks.end)+
  aes     ( xmin = -.5, xmax = .5)+
  aes     ( fill = score.type)+
  geom_text(aes(label = pct, y = -text.y.value, x = 0))+
  facet_wrap(~score)+
  theme_void()+
  scale_fill_manual(values = c('#377EB8','#4DAF4A','#F0F0F0','#FFC000'))+
  theme(legend.position = 'top')

# .sp(ti = 'Figure 1')

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

