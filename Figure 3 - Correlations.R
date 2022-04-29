
# . -----------------------------------------------------------------------

dt. <- readRDS('DATA derived/long.data.rds') %>%
  filter ( type2 == 'all') %>% 
  select ( study, sjid, avisitn, age, amb, paramcd, aval, fds )

# normal ones -------------------------------------------------------------

my.cor.facet <- function(ds, x.par, x.max, y.par, y.max) {
  
  x.par <- ensym(x.par)
  y.par <- ensym(y.par)
  
  dt.. <- ds %>% 
    filter(type == 'val') %>% 
    filter(paramcd %in% c(x.par, y.par)) %>% 
    spread(paramcd, aval) %>%
    filter(!is.na(!!x.par) & !is.na(!!y.par)) %>% 
    ungroup 
  
  dt..first <- ds %>% 
    filter(type == 'val') %>% 
    group_by(sjid) %>% filter(avisitn == min(avisitn)) %>%
    filter(paramcd %in% c(x.par, y.par)) %>% 
    spread(paramcd, aval) %>%
    filter(!is.na(!!x.par) & !is.na(!!y.par)) %>% 
    ungroup 
  
  dt..pct <- ds %>% 
    filter(type == 'pct') %>% 
    filter(paramcd %in% c(x.par, y.par)) %>% 
    spread(paramcd, aval) %>%
    filter(!is.na(!!x.par) & !is.na(!!y.par)) %>% 
    ungroup 
  
  dt.. %>% 
    ggplot()+geom_point(shape = 21)+
    aes( x = !!x.par, y = !!y.par )+
    # stat_regline_equation(label.y = y.max*.80)+
    # stat_cor(label.y = y.max*.75, aes(label = paste(..rr.label..)))+
    geom_smooth()+
    geom_smooth(method = lm, color = 'red')+
    stat_regline_equation(label.y = y.max*.95, data = dt..pct)+
    stat_cor(label.y = y.max*.90, aes(label = paste(..rr.label..)), data = dt..pct)+
    # geom_smooth(method = lm, color = 'green', linetype = 'dashed', data = dt..pct)+
    # stat_regline_equation(label.y = y.max*.80, data = dt..first)+
    # stat_cor(label.y = y.max*.75, aes(label = paste(..rr.label..)), data = dt..first)+
    # geom_smooth(method = lm, color = 'red', linetype = 'dashed', data = dt..first)+
    geom_abline(slope = y.max/x.max, linetype = 'dotted') +
    coord_cartesian( xlim = c(0,x.max), ylim = c(0,y.max) )+
    theme_pubclean()+
    .leg_none
}

# dt. %>% my.cor.facet('ICARS', 100, 'mFARS',  93 )
# dt %>% my.cor.facet('ICARS.ax', 34, 'FARS.E',  36 )
# dt %>% my.cor.facet('ICARS.ax', 34/100, 'FARS.E',  36/100 )
# dt %>% my.cor.facet('ICARS.od', 6, 's4.speech',  6 )
# dt %>% my.cor.facet('ICARS.od', 6, 'FARS.Am',  5 )
# dt %>% my.cor.facet('s4.speech', 6, 'FARS.Am',  5 )

G <- dt. %>% my.cor.facet('ICARS', 100, 'mFARS',  93 )
H <- dt. %>% my.cor.facet('SARA' ,  40, 'mFARS',  93 )
I <- dt. %>% my.cor.facet('SARA' ,  40, 'ICARS', 100 )
D <- dt. %>% my.cor.facet('ICARS.ax',  34, 'FARS.E'   , 36 )
E <- dt. %>% my.cor.facet('SARA.ax' ,  20, 'FARS.E'   , 36 )
F <- dt. %>% my.cor.facet('SARA.ax' ,  20, 'ICARS.ax' , 34 )
A <- dt. %>% my.cor.facet('ICARS.ki',  52, 'FARS.BC'  , 52 )
B <- dt. %>% my.cor.facet('SARA.ki' ,  16, 'FARS.BC'  , 52 )
C <- dt. %>% my.cor.facet('SARA.ki' ,  16, 'ICARS.ki' , 52 )

p <- ggarrange(A, B, C, D, E, F, G, H, I, labels = c('A','B','C','D', 'E', 'F', 'G', 'H', 'I'), ncol = 3, nrow = 3)
p

# ggsave('Figure 3 - Correlations.png', plot = p, height = 13.5*.9, width = 13.5*.9)

# . -----------------------------------------------------------------------

rm(A, B, C, D, E, F, G, H, I, p)
