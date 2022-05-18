
rm(list = ls())

scales <- readRDS('DATA derived/scales.rds')

scales.list <- scales$paramcd

dt. <- readRDS('DATA derived/dt.rds') %>% 
  filter(type == 'pct', type2 == 'all') %>% 
  ungroup %>% 
  select( study, sjid, avisitn, age, paramcd, aval, amb, fds ) %>% 
  filter( paramcd %in% c('mFARS','SARA','ICARS','FARS.E', 'SARA.ax','ICARS.ax', 'FARS.BC', 'SARA.ki', 'ICARS.ki')) %>% 
  droplevels()

# . -----------------------------------------------------------------------

base.size <- 10 

tmp <- dt. %>% 
  left_join(
    scales %>% select(paramcd, score, score.type)
  ) %>% 
  mutate( score      = factor(score     , c('mFARS','SARA','ICARS'))) %>%
  mutate( score.type = factor(score.type, c('Total Score','Axial Function','Appendicular Function'))) %>%
  mutate( paramcd    = factor(paramcd, c('mFARS','SARA','ICARS','FARS.E', 'SARA.ax','ICARS.ax'), labels = c('mFARS','SARA','ICARS','FARS E', 'SARA-AX','ICARS-AX'))) %>%
  filter( fds>0, fds<6)

A <- tmp %>%
  filter ( score.type == 'Total Score') %>% 
  ggplot()+geom_boxplot()+
  aes( x    = factor(fds), y = aval )+
  aes( fill = score )+
  scale_fill_manual(values = c("#2ca25f", "#E7B800", "#FC4E07"))+
  facet_wrap(~score.type, ncol = 2)+
  geom_hline(yintercept = c(0,50, 100), linetype = 'dotted')+
  ggpubr::theme_pubclean(base_size = base.size)+
  coord_cartesian(ylim = c(0,100))+
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

B <- tmp %>%
  filter ( score.type == 'Axial Function') %>% 
  ggplot()+geom_boxplot()+
  aes( x    = factor(fds), y = aval )+
  aes( fill = score )+
  scale_fill_manual(values = c("#2ca25f", "#E7B800", "#FC4E07"))+
  facet_wrap(~score.type, ncol = 2)+
  geom_hline(yintercept = c(0,50, 100), linetype = 'dotted')+
  ggpubr::theme_pubclean(base_size = base.size)+
  coord_cartesian(ylim = c(0,100))+
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

C <- tmp %>%
  filter ( score.type == 'Appendicular Function') %>% 
  ggplot()+geom_boxplot()+
  aes( x    = factor(fds), y = aval )+
  aes( fill = score )+
  scale_fill_manual(values = c("#2ca25f", "#E7B800", "#FC4E07"))+
  facet_wrap(~score.type, ncol = 2)+
  geom_hline(yintercept = c(0,50, 100), linetype = 'dotted')+
  ggpubr::theme_pubclean(base_size = base.size)+
  coord_cartesian(ylim = c(0,100))+
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())


p <- ggarrange( A, B, C, labels = c('A','B','C' ), ncol = 2, nrow = 2, common.legend = T)+.leg_lr
p <- annotate_figure(p, left = 'Percentualized Score', bottom = 'Functional Disease Stage')

# .sp(l = 'TTE', ti ='Figure 2 - Boxplots' )

