
# . -----------------------------------------------------------------------

rm(list = ls())

scales <- readRDS('DATA derived/scales.rds')

scales.list <- scales$paramcd

dt.  <- readRDS('DATA derived/models.predictions.rds') %>% 
  filter ( type == 'pct', type2 == 'all') %>% 
  ungroup %>% 
  select ( dep, ind, data, pred.lm, pred.pl  ) %>% 
  unnest( c(data, starts_with('pred.')))

# Distributions.R ---------------------------------------------------------

df.fds <- dt. %>% ungroup %>% select(sjid, avisitn, fds) %>% unique

# scaled --------------------------------------------------------------

dt <- dt. %>% 
  # mutate( dep = factor(dep, c( 'mFARS' ,'SARA'   , 'FARS.E','SARA.ax'    ) ) ) %>%
  # mutate( ind = factor(ind, c( 'mFARS' ,'SARA','ICARS', 'FARS.E','SARA.ax','ICARS.ax' ) ) ) %>%
  mutate( dep = factor(dep, c( 'FARS.E','SARA.ax'    ) ) ) %>%
  mutate( ind = factor(ind, c( 'FARS.E','SARA.ax','ICARS.ax' ) ) ) %>%
  filter(!is.na( dep ), !is.na(ind))

dt %<>% 
  rename( measured = dep.val ) %>% 
  select( dep, ind, sjid, avisitn, amb, measured, pred.lm, pred.pl ) %>% 
  gather( type, value, measured, pred.lm, pred.pl)

# median <- dt %>% 
#   group_by(dep, amb) %>% summarise(m = median(dep.val))

dt %>% 
  filter( amb == 'ambulatory') %>% 
  # filter(sjid == 4212, avisitn == 4) %>%
  left_join(scales %>% select(ind = paramcd, score.ind = score, score.type.ind = score.type)) %>% 
  left_join(scales %>% select(dep = paramcd, score.dep = score, score.type.dep = score.type)) %>% 
  select(dep, sjid, avisitn, amb, type, value, ind, score.dep, score.ind, score.type.dep, score.type.ind) %>%
  mutate_at('score.type.dep', factor, c('Total Score','axial function')) %>% 
  mutate_at('score.type.ind', factor, c('Total Score','axial function')) %>% 
  filter   ( !(score.type.dep == 'axial function' & amb == 'non-amb.')  ) %>% 
  unique %>% 
  mutate( score.ind  = factor(score.ind , c( 'mFARS','SARA','ICARS' ) ) ) %>%
  mutate( score.dep  = factor(score.dep , c( 'mFARS','SARA','ICARS' ) ) ) %>%
  # filter( type != 'pred.lm') %>%
  # filter( dep %in% c('mFARS','FARS.E') ) %>%
  ggplot( )+
  geom_density(alpha = .50)+
  # geom_histogram(alpha = .5, position = position_identity(), binwidth = 20)+
  # aes (linetype = score.ind)+
  aes (x = value)+
  aes (fill = type)+scale_fill_manual(values = c('#111111', '#f03b20', '#ffffff'), labels = c('measured','linear model','polynom. model'))+ #, labels = bquote( c('FARS E', SARA^ax, ICARS^ax )))+
  xlab('Percentualized Score')+
  # coord_cartesian(xlim = c(0,100))+
  # geom_vline(data = median, aes(xintercept = m), aes(color = dep))+
  ggpubr::theme_pubclean()+
  # facet_grid( paste(score.dep, 'from', score.ind, sep=' ')~score.type.dep, scale = 'free_x' )+
  facet_wrap( ~paste(score.dep, 'from', score.ind, sep=' ') , scale = 'free_x' )+
  theme(legend.title = element_blank())
# .leg_lr+


# .sp(ti = 'density plots - scaled ')

# non-scaled --------------------------------------------------------------

dt <- dt. %>% 
  # mutate( dep = factor(dep, c( 'FARS.E','SARA.ax','ICARS.ax' ) ) ) %>% 
  mutate( dep = factor(dep, c( 'mFARS' ,'SARA'   ,'ICARS', 'FARS.E','SARA.ax','ICARS.ax'    ) ) ) %>%
  filter(!is.na( dep ))

median <- dt %>% 
  group_by(dep, amb) %>% summarise(m = median(dep.val))

dt %>% 
  left_join(scales %>% select(dep = paramcd, score, score.type)) %>% 
  select(dep, study, sjid, avisitn, fds, age, amb, dep.val, score, score.type) %>%
  mutate_at('score.type', factor, c('Total Score','axial function')) %>% 
  filter   ( !(score.type == 'axial function' & amb == 'non-amb.')  ) %>% 
  unique %>% 
  mutate( score  = factor(score , c( 'mFARS','SARA','ICARS' ) ) ) %>%
  ggplot( )+
  geom_density(alpha = .50)+
  # geom_histogram(alpha = .5, position = position_identity(), binwidth = 20)+
  # aes (linetype = amb)+
  aes (x = dep.val)+
  aes (fill = score)+scale_fill_brewer(palette = 'Set1')+ #, labels = bquote( c('FARS E', SARA^ax, ICARS^ax )))+
  xlab('Percentualized Score')+
  # coord_cartesian(xlim = c(0,100))+
  # geom_vline(data = median, aes(xintercept = m), aes(color = dep))+
  ggpubr::theme_pubclean()+
  facet_grid( amb~score.type, scale = 'free_x' )+
  .leg_lr

# .sp(ti = 'density plots')














