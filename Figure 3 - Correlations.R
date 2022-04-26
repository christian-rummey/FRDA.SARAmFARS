

# # A tibble: 12 x 6
# paramcd  param             maxscore paramlong              pl2   ptype   
# <chr>    <chr>                <int> <chr>                  <chr> <chr>   
#  1 FARSn    FARSn                  125 FARSn                  NA    total   
#  2 mFARS    mFARS                   93 mFARS                  NA    total   
#  3 FARS.BC  FARS.append             52 BC.FARS.append         NA    subscore
#  4 FARS.B   UPPER.LIMB              36 B.UPPER.LIMB           NA    subscore
#  5 FARS.E   UPRIGHT.STABILITY       36 E.UPRIGHT.STABILITY    NA    subscore
#  6 SARA.ki  SARA.ki                 16 SARA.kinetic           NA    subscore
#  7 SARA.ku  SARA.ku                 12 SARA.kupper            NA    subscore
#  8 SARA.ax  SARA.ax                 18 SARA.gaitposture       NA    subscore
#  9 SARA     sarasum                 40 SARA                   NA    total   
# 10 ICARS    ICARS_Total            100 ICARS_Total            NA    total   
# 11 ICARS.ax ICARS.ax                34 ICARS_Posture.and.Gait NA    subscore
# 12 ICARS.ki ICARS.ki                52 ICARS_Kinetic.Function NA    subscore

steps. <- .dd.FA('steps')

dt. <- readRDS('DATA derived/long.data.rds')

dt <- dt. %>%
  filter   ( forslope == 1 ) %>%
  left_join(.rt('../DATA other/scales.txt') %>% select(paramcd, maxscore)) %>%
  mutate   ( aval.pct = 100*aval/maxscore ) %>%
  select   ( study, sjid, avisitn, age, amb, paramcd, aval, aval.pct ) %>%
  gather   ( type, aval, aval, aval.pct) %>% 
  left_join(
    steps. %>% 
      select(sjid, avisitn, fds)
    ) %>% 
  filter(type == 'aval')

# dt %>% filter(type == 'aval.pct') %>% spread(paramcd, aval) %>% 
#   # lm( ICARS ~ mFARS, data = . ) %>% 
#   # lm( mFARS ~ ICARS, data = . ) %>% 
#   lm( mFARS ~ SARA, data = . ) %>%
#   # lm( SARA ~ mFARS, data = . ) %>% 
#   broom::tidy()

# normal ones -------------------------------------------------------------

list.scales <- c('mFARS','SARA','ICARS','FARS.E','SARA.ax','ICARS.ax','FARS.BC','SARA.ki','ICARS.ki','ICARS.od','s4.speech','FARS.Am')

mx. <- .rt('../DATA other/scales.txt') %>% filter(paramcd %in% list.scales) %>% select(paramcd, maxscore)

my.cor.facet <- function(ds, x.par, x.max, y.par, y.max) {
  
  x.par <- ensym(x.par)
  y.par <- ensym(y.par)
  
  dt.1 <- dt %>% 
    # group_by(sjid) %>% filter(avisitn == min(avisitn)) %>% 
    # filter(amb != 'ambulatory') %>%
    # left_join(mx.) %>%
    filter(paramcd %in% c(x.par, y.par)) %>% 
    # pivot_wider(values_from = c(aval, maxscore), names_from = paramcd) %>%
    spread(paramcd, aval) %>%
    filter(!is.na(!!x.par) & !is.na(!!y.par)) %>% 
    ungroup %>% select(-c(study, sjid, avisitn, age, amb, type, fds))
  
  dt.2 <- dt %>% 
    group_by(sjid) %>% filter(avisitn == min(avisitn)) %>%
    # filter(amb != 'ambulatory') %>%
    # left_join(mx.) %>%
    filter(paramcd %in% c(x.par, y.par)) %>% 
    # pivot_wider(values_from = c(aval, maxscore), names_from = paramcd) %>%
    spread(paramcd, aval) %>%
    filter(!is.na(!!x.par) & !is.na(!!y.par)) %>% 
    ungroup %>% select(-c(study, sjid, avisitn, age, amb, type, fds))
  
  dt.1 %>% 
    ggplot()+geom_point(shape = 21)+
    aes( x = !!x.par, y = !!y.par )+
    stat_regline_equation(label.y = y.max*.95)+
    # stat_regline_equation(label.y = y.max*.80, data = dt.2)+
    stat_cor(label.y = y.max*.90, aes(label = paste(..rr.label..)))+
    # stat_cor(label.y = y.max*.75, aes(label = paste(..rr.label..)), data = dt.2)+
    geom_smooth()+
    geom_smooth(method = lm, color = 'red')+
    # geom_smooth(method = lm, color = 'red', data = dt.2, linetype = 'dashed')+
    geom_abline(slope = y.max/x.max, linetype = 'dotted') +
    coord_cartesian( xlim = c(0,x.max), ylim = c(0,y.max) )+
    theme_pubclean()+
    .leg_none
}

# dt %>% my.cor.facet('ICARS', 100, 'mFARS',  93 )
# dt %>% my.cor.facet('ICARS.od', 6, 's4.speech',  6 )
# dt %>% my.cor.facet('ICARS.od', 6, 'FARS.Am',  5 )
# dt %>% my.cor.facet('s4.speech', 6, 'FARS.Am',  5 )

G <- dt %>% my.cor.facet('ICARS', 100, 'mFARS',  93 )
H <- dt %>% my.cor.facet('SARA' ,  40, 'mFARS',  93 )
I <- dt %>% my.cor.facet('SARA' ,  40, 'ICARS', 100 )
D <- dt %>% my.cor.facet('ICARS.ax',  34, 'FARS.E'   , 36 )
E <- dt %>% my.cor.facet('SARA.ax' ,  20, 'FARS.E'   , 36 )
F <- dt %>% my.cor.facet('SARA.ax' ,  20, 'ICARS.ax' , 34 )
A <- dt %>% my.cor.facet('ICARS.ki',  52, 'FARS.BC'  , 52 )
B <- dt %>% my.cor.facet('SARA.ki' ,  16, 'FARS.BC'  , 52 )
C <- dt %>% my.cor.facet('SARA.ki' ,  16, 'ICARS.ki' , 52 )

p <- ggarrange(A, B, C, D, E, F, G, H, I, labels = c('A','B','C','D', 'E', 'F', 'G', 'H', 'I'), ncol = 3, nrow = 3)
p

# ggsave('Figure 3 - Correlations.png', plot = p, height = 13.5*.9, width = 13.5*.9)
