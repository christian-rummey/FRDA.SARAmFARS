
# . -----------------------------------------------------------------------

dt. <- readRDS('DATA derived/dt.rds') %>%
  filter(type2 == 'all') %>% 
  ungroup %>% 
  select ( type, study, sjid, avisitn, age, amb, paramcd, aval)

# normal ones -------------------------------------------------------------

my.cor.facet <- function(ds, x.par, x.max, y.par, y.max) {
  
  x.par <- ensym(x.par)
  y.par <- ensym(y.par)
  
  x.max <- 100
  y.max <- 100
  
  dt.. <- ds %>% 
    filter(type == 'pct') %>% 
    filter(paramcd %in% c(x.par, y.par)) %>% 
    spread(paramcd, aval) %>%
    filter(!is.na(!!x.par) & !is.na(!!y.par)) %>% 
    ungroup 
  
  dt..first <- ds %>% 
    filter(type == 'pct') %>% 
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
    geom_smooth( formula = y ~ poly(x, 2), method = 'lm', color = 'blue')+
    geom_smooth( method = lm, color = 'red')+
    stat_regline_equation(label.x = x.max*.03, label.y = y.max*.95, data = dt..pct)+
    stat_cor             (label.x = x.max*.03, label.y = y.max*.90, aes(label = paste(..rr.label..)), data = dt..pct, r.digits = 2)+
    # geom_smooth(method = lm, color = 'green', linetype = 'dashed', data = dt..pct)+
    # stat_regline_equation(label.y = y.max*.80, data = dt..first)+
    # stat_cor(label.y = y.max*.75, aes(label = paste(..rr.label..)), data = dt..first)+
    # geom_smooth(method = lm, color = 'red', linetype = 'dashed', data = dt..first)+
    geom_abline(slope = y.max/x.max, linetype = 'dotted') +
    coord_cartesian( xlim = c(0,x.max), ylim = c(0,y.max) )+
    geom_hline(yintercept = c(0,y.max), linetype = 'dotted')+
    geom_vline(xintercept = c(0,x.max), linetype = 'dotted')+
    theme_pubclean()+
    .leg_none
}

# dt. %>% my.cor.facet('ICARS.od', 6, 's4.speech',  6 )
# dt. %>% my.cor.facet('ICARS.od', 6, 'FARS.Am',  5 )
# dt. %>% my.cor.facet('s4.speech', 6, 'FARS.Am',  5 )

G <- dt. %>% my.cor.facet('ICARS', 100, 'mFARS',  93 )
H <- dt. %>% my.cor.facet('SARA' ,  40, 'mFARS',  93 )
I <- dt. %>% my.cor.facet('SARA' ,  40, 'ICARS', 100 )
D <- dt. %>% my.cor.facet('ICARS.ax',  34, 'FARS.E'   , 36 ) + xlab('ICARS, axial') + ylab('mFARS, axial')
E <- dt. %>% my.cor.facet('SARA.ax' ,  18, 'FARS.E'   , 36 ) + xlab('SARA, axial') + ylab('mFARS, axial')
F <- dt. %>% my.cor.facet('SARA.ax' ,  18, 'ICARS.ax' , 34 ) + xlab('SARA, axial') + ylab('ICARS, axial')
A <- dt. %>% my.cor.facet('ICARS.ki',  52, 'FARS.BC'  , 52 ) + xlab('ICARS, appendicular') + ylab('mFARS, appendicular')
B <- dt. %>% my.cor.facet('SARA.ki' ,  16, 'FARS.BC'  , 52 ) + xlab('SARA, appendicular') + ylab('mFARS, appendicular')
C <- dt. %>% my.cor.facet('SARA.ki' ,  16, 'ICARS.ki' , 52 ) + xlab('SARA, appendicular') + ylab('ICARS, appendicular')

p <- ggarrange(A, B, C, D, E, F, G, H, I, labels = c('A','B','C','D', 'E', 'F', 'G', 'H', 'I'), ncol = 3, nrow = 3)
p

# ggsave('Figure 3 - Correlations.png', plot = p, height = 13.5*.9, width = 13.5*.9)

# . -----------------------------------------------------------------------

rm(A, B, C, D, E, F, G, H, I, p)
