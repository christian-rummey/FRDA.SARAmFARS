
# . -----------------------------------------------------------------------



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

dt.  <- readRDS('DATA derived/long.data.rds')
steps. <- .dd.FA('steps')

# . -----------------------------------------------------------------------

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
  # filter(amb == 'ambulatory') %>%
  filter(type == 'aval') %>%
  # filter(type == 'aval.pct') %>% 
  spread(paramcd, aval)

# normal ones -------------------------------------------------------------

my.hist.facet <- function(ds, x.par, x.max) {
  
  x.par <- ensym(x.par)
  
  ds %>%
    # mutate( E.stage = cut(FARS.E, c(0, 8, 30, 36))) %>% 
    mutate( E.stage = cut(FARS.E, c(0, 20, 80, 100))) %>%
    ggplot()+ 
    geom_histogram(position = position_stack(reverse = T), aes(group = amb))+
    aes(x = !!x.par)+
    aes(alpha = amb)+scale_alpha_manual(values = c(.75, .4))+
    # scale_y_discrete(name="", )
    geom_vline(xintercept = c(0, x.max), linetype = 'dotted')+
    theme_pubclean()+
    .leg_none
}

A <- dt %>% my.hist.facet('mFARS',  93 )
B <- dt %>% my.hist.facet('SARA' ,  40 )
C <- dt %>% my.hist.facet('ICARS', 100 ) + .leg_tr
D <- dt %>% my.hist.facet('FARS.E'   , 36 )
E <- dt %>% my.hist.facet('SARA.ax' ,  20 )
F <- dt %>% my.hist.facet('ICARS.ax',  34 )
G <- dt %>% my.hist.facet('FARS.BC'  , 52 )
H <- dt %>% my.hist.facet('SARA.ki' ,  16 )
I <- dt %>% my.hist.facet('ICARS.ki',  52 )

p <- ggarrange(A, B, C, D, E, F, G, H, I, labels = c('A','B','C','D', 'E', 'F', 'G', 'H', 'I'), ncol = 3, nrow = 3)
p

# ggsave('Figure 2 - Histogram.png', plot = p, height = 13.5*.9, width = 13.5*.9)

# . -----------------------------------------------------------------------

rm(A, B, C, D, E, F, G, H, I, p)
