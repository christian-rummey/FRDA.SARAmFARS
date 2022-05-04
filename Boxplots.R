
rm(list = ls())

scales.list <- c('FARSn',
                 'mFARS'   ,'SARA'      ,'ICARS',
                 'FARS.E'  ,'SARA.ax'   ,'ICARS.ax',
                 'FARS.BC' ,'SARA.ki'   ,'ICARS.ki',
                 'FARS.Am' ,'s4.speech' ,'ICARS.sp',
                 'ICARS.od')

lm.mod <- readRDS('DATA derived/models.predictions.rds') %>% 
  select( -mod.lm, -mod.pl ) %>% 
  unnest( c(data, pred.lm, pred.pl )) %>% 
  mutate( dep = factor(dep, scales.list)) %>%
  mutate( ind = factor(ind, scales.list)) %>%
  droplevels
  
with(lm.mod, table(dep, ind))

scales <- .rt('../DATA other/scales.txt') %>% 
  select(-pl2) %>% 
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



# correlate predictions ---------------------------------------------------

lm.mod %>% 
  ungroup %>% 
  filter ( dep %in% c('FARS.E')) %>% 
  filter ( ind %in% c('SARA.ax')) %>% 
  ggplot()+geom_point()+
  aes(x = pred.lm, y = ind.val)+
  facet_grid( ind ~ dep )+
  geom_abline (slope = 1)+
  coord_cartesian(ylim = c(0,100), xlim = c(0,100))


# . -----------------------------------------------------------------------

scales.list[c(5:7)]

lm.mod %<>% 
  mutate(pred = pred.lm)

A <- lm.mod %>% 
  filter( dep %in% scales.list[c(5:7)]) %>% 
  filter( ind %in% scales.list[c(5:7)]) %>% 
  bind_rows(
    lm.mod %>%
      filter( dep %in% scales.list[c(5:7)]) %>%
      mutate( dep.val = pred) %>%
      mutate( ind = dep )
  ) %>%
  filter(fds>0, fds<5) %>% 
  ggplot()+geom_boxplot()+
  aes( x    = factor(fds), y = pred )+
  aes( fill = ind)+
  # scale_x_continuous('dep')+scale_y_continuous('indep')+
  facet_wrap(~dep)+
  geom_hline(yintercept = 0, linetype = 'dotted')+
  ggpubr::theme_pubclean()+
  coord_cartesian(ylim = c(0,100))

ggpubr::ggarrange( A, B, ncol = 2 )
.sp(l = 'F')
# predicted values --------------------------------------------------------

tmp %>% 
  group_by(paramcd, )

# boxplot -----------------------------------------------------------------

p <- tmp %>% 
  filter(score.type != 'kinetic function') %>%
  ggplot()+
  geom_boxplot(outlier.shape = 21, outlier.fill = NA, outlier.alpha = .5, width = .5)+
  aes(fill = factor(score))+scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  facet_wrap(  ~ score.type, ncol = 1, scales = 'free_x' )+
  aes(x = factor(fds), y = aval)+    
  geom_vline(xintercept = seq(.5, 5.5, 1), linetype = 'solid', color = 'grey')+
  scale_x_discrete(labels = c(1, 2, 3, 4, 'non-\nambulatory'))+
  theme_pubclean(base_size = 14)+
  theme(axis.title.x = element_blank())+
  ylab('Percentage of Total')+
  .leg_tl

# ggsave('Figure 2 - Boxplots.png', plot = p, height = 13.5*.6, width = 13.5*.6)

tmp %>% 
  filter(score.type != 'kinetic function') %>%
  ggplot()+
  geom_boxplot(outlier.shape = 21, outlier.fill = NA, outlier.alpha = .5, width = .5)+
  aes(fill = factor(score))+scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  facet_grid(  score.type ~ , scales = 'free_x' )+
  aes(x = fds, y = aval)+    theme_pubclean()+
  # aes(alpha = factor(score))+
  # geom_vline(xintercept = seq(.5, 5.5, 1), linetype = 'solid', color = 'grey')+
  # scale_x_discrete(labels = c(1, 2, 3, 4, 'non-\nambulatory'))+
  .leg_tl

# .sp(l = 'F')

# median values -----------------------------------------------------------

tmp %>% 
  group_by(score, score.type, fds) %>% 
  summarise( med = median (aval)) %>% 
  spread(score, med) %>% 
  mutate(mFARS/SARA)



# lineplot -----------------------------------------------------------------

tmp %>% 
  # filter(score.type == 'axial function') %>% 
  group_by( fds, score, score.type, paramcd ) %>% 
  # summarise(
  #   m = median(aval),
  #   CI = .ci(aval)
  #   ) %>% 
  summarise(
    m = median(aval),
    lower = .ci(aval),
    upper = .ci(aval)
    # lower = quantile(aval, probs = seq(0, 1, 0.25))[2],
    # upper = quantile(aval)[4]
  ) %>% 
  ggplot()+
  geom_pointrange(position = .dodge)+
  aes(ymin = lower, ymax = upper)+
  aes(y = m, factor(fds)) +
  aes(color = factor(score))+scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  facet_wrap(  ~ score.type, ncol = 3 )+
  geom_vline(xintercept = seq(.5, 5.5, 1), linetype = 'dotted', color = 'grey')+
  theme_pubclean()+
  .leg_tl

# .sp(l = 1)
