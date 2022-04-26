
dt.  <- readRDS('DATA derived/long.data.rds') %>% 
  # filter(amb == 'ambulatory') %>%
  filter( forslope == 1 )

# . -----------------------------------------------------------------------

dt <- dt. %>%
  filter ( type == 'val' ) %>% 
  select ( study, sjid, avisitn, age, amb, paramcd, aval ) %>% 
  spread ( paramcd, aval)

# normal ones -------------------------------------------------------------

my.hist.facet <- function(ds, x.par, x.max) {
  
  x.par <- ensym(x.par)
  
  ds %>%
    ggplot()+ 
    geom_histogram(position = position_stack(reverse = T), aes(group = amb))+
    aes(x = !!x.par)+
    aes(alpha = amb)+scale_alpha_manual(values = c(.75, .4))+
    geom_vline(xintercept = c(0, x.max), linetype = 'dotted')+
    theme_pubclean()+
    .leg_none
}

G <- dt %>% my.hist.facet('mFARS'   ,  93 )
H <- dt %>% my.hist.facet('SARA'    ,  40 )
I <- dt %>% my.hist.facet('ICARS'   , 100 ) + .leg_tr
D <- dt %>% my.hist.facet('FARS.E'  ,  36 )
E <- dt %>% my.hist.facet('SARA.ax' ,  20 )
F <- dt %>% my.hist.facet('ICARS.ax',  34 )
A <- dt %>% my.hist.facet('FARS.BC' ,  52 )
B <- dt %>% my.hist.facet('SARA.ki' ,  16 )
C <- dt %>% my.hist.facet('ICARS.ki',  52 )

p <- ggarrange(A, B, C, D, E, F, G, H, I, labels = c('A','B','C','D', 'E', 'F', 'G', 'H', 'I'), ncol = 3, nrow = 3)
p

# ggsave('Figure 2 - Histogram.png', plot = p, height = 13.5*.9, width = 13.5*.9)

# . -----------------------------------------------------------------------

rm(A, B, C, D, E, F, G, H, I, p)
