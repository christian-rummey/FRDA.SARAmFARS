
rm(list = ls())

scales.list <- c('FARSn',
                 'mFARS'   ,'SARA'      ,'ICARS',
                 'FARS.E'  ,'SARA.ax'   ,'ICARS.ax',
                 'FARS.BC' ,'SARA.ki'   ,'ICARS.ki',
                 'FARS.Am' ,'s4.speech' ,'ICARS.sp',
                 'ICARS.od')

dt. <- readRDS('DATA derived/dt.rds') %>% 
  ungroup %>% 
  select( study, sjid, avisitn, age, paramcd, aval, amb, fds ) %>% 
  filter( paramcd %in% c('mFARS','SARA','ICARS','FARS.E', 'SARA.ax','ICARS.ax')) %>% 
  droplevels()

with(dt., table(paramcd))

scales <- readRDS('DATA derived/scales.txt')

# . -----------------------------------------------------------------------

dt. %>% 
  left_join(
    scales %>% select(paramcd, score, score.type)
  ) %>% 
  mutate( score      = factor(score     , c('mFARS','SARA','ICARS'))) %>%
  mutate( score.type = factor(score.type, c('Total Score','Axial Function'))) %>%
  mutate( paramcd    = factor(paramcd, c('mFARS','SARA','ICARS','FARS.E', 'SARA.ax','ICARS.ax'), labels = c('mFARS','SARA','ICARS','FARS E', 'SARA-AX','ICARS-AX'))) %>%
  filter( fds>0, fds<6) %>%
  ggplot()+geom_boxplot()+
  aes( x    = factor(fds), y = aval )+
  aes( fill = score )+
  scale_fill_manual(values = c("#2ca25f", "#E7B800", "#FC4E07"))+
  facet_wrap(~score.type, ncol = 2)+
  geom_hline(yintercept = c(0,50, 100), linetype = 'dotted')+
  ggpubr::theme_pubclean(base_size = 22)+
  coord_cartesian(ylim = c(0,100))+
  xlab('Functional Disease Stage')+
  ylab('Percentualized Score')+
  theme(legend.title = element_blank())

# .sp(l = 'F', ti ='Figure 2' )
