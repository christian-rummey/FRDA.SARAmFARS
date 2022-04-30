
rm(list = ls())

scales.list <- c('mFARS'   ,'SARA'      ,'ICARS',
                 'FARS.E'  ,'SARA.ax'   ,'ICARS.ax',
                 'FARS.BC' ,'SARA.ki'   ,'ICARS.ki',
                 'FARS.Am' ,'s4.speech' ,'ICARS.sp',
                 'ICARS.od')

dt.  <- readRDS('DATA derived/long.data.rds') %>% 
  # filter(amb == 'ambulatory') %>%
  filter( forslope == 1 )

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

# . -----------------------------------------------------------------------

dt <- dt. %>%
  filter ( type == 'pct') %>% 
  select ( study, sjid, avisitn, age, amb, paramcd, aval, fds = fds.act )

# dt %>% 
#   filter(paramcd %in% c('FARS.E', 'SARA.ax', 'ICARS.ax')) %>% 
#   # filter(fds.act == 6)
#   # ggplot()+geom_violin()+geom_jitter(width = .2)+
#   ggplot()+geom_boxplot()+#geom_jitter(width = .2)+
#   aes(x = factor(fds), y = aval)+
#   aes(fill = paramcd)+
#   facet_wrap(  ~ paramcd, scales = 'free_y' )

tmp <- dt %>% 
  left_join(scales %>% select(paramcd, score, score.type)) %>% 
  # filter(score.type != 'kinetic function') %>% 
  mutate(fds = ifelse(fds>5, 5, fds)) %>%
  mutate(fds = ifelse(fds<1, 1, fds)) %>%
  filter(fds > 0, fds < 6) %>% 
  mutate(score.type = factor(score.type, c('kinetic function','axial function','Total Score'))) %>%
  filter(!is.na(score.type)) %>%
  mutate(score = factor(score, c('mFARS','SARA','ICARS'))) %>% 
  filter(!is.na(paramcd))

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

ggsave('Figure 2 - Boxplots.png', plot = p, height = 13.5*.6, width = 13.5*.6)

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
