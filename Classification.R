
rm(list = ls())

scales.list <- c('mFARS'   ,'SARA'      ,'ICARS',
                 'FARS.E'  ,'SARA.ax'   ,'ICARS.ax',
                 'FARS.BC' ,'SARA.ki'   ,'ICARS.ki',
                 'FARS.Am' ,'s4.speech' ,'ICARS.sp',
                 'ICARS.od')

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

dt.  <- readRDS('DATA derived/models.predictions.rds') %>% 
  filter ( type == 'pct', type2 == 'all') %>% 
  ungroup %>% 
  select ( dep, ind, data, pred.lm, pred.pl  ) %>% 
  unnest( c(data, starts_with('pred.')))

# . -----------------------------------------------------------------------

df.fds <- dt. %>% ungroup %>% select(sjid, avisitn, fds) %>% unique

edges.   <- c(  -40,      20,      40,      60, 80, 150)
edges.l  <- c('<20', '21-40', '41-60', '61-80', '>80')
edges.ln <- seq(1,5,1)

dt. %>% 
  filter( ind %in% c( 'FARS.E','SARA.ax','ICARS.ax' ), dep %in% c( 'FARS.E','SARA.ax' ) ) %>% 
  # select( -c( dep.val, ind.val, pred.lm, pred.pl )) %>% 
  select(-ind.val) %>% 
  gather( type, score, pred.lm, pred.pl, dep.val ) %>% 
  left_join(df.fds) %>% filter(fds > 0, fds<5) %>% 
  ggplot( )+
  aes (x = factor(fds), y = score)+
  aes (fill = type)+
  geom_boxplot()+
  facet_grid(ind~dep)



# classification table (doesn't work :-P) ---------------------------------

df.fds <- dt. %>% ungroup %>% select(sjid, avisitn, fds) %>% unique

edges.   <- c(  -40,      20,      40,      60, 80, 150)
edges.l  <- c('<20', '21-40', '41-60', '61-80', '>80')
edges.ln <- seq(1,5,1)

dt. %>% 
  filter( ind %in% c( 'FARS.E','SARA.ax','ICARS.ax' ), dep %in% c( 'FARS.E','SARA.ax','ICARS.ax' ) )%>% 
  mutate( status.is = cut(dep.val, edges., edges.l) ) %>% 
  mutate( status.lm = cut(pred.lm, edges., edges.l) ) %>% 
  mutate( status.pl = cut(pred.pl, edges., edges.l) ) %>% 
  select( -c( dep.val, ind.val, pred.lm, pred.pl )) %>% 
  gather( type, status.pred, status.lm, status.pl) %>% 
  mutate( status.pred = factor(status.pred, edges.l)) %>% 
  group_by( dep, ind, type, status.is ) %>% 
  mutate( N = n()) %>% 
  mutate( 
    prediction = case_when (
      as.numeric( status.is ) == as.numeric( status.pred ) ~ '2 ok' ,
      as.numeric( status.is ) >  as.numeric( status.pred ) ~ '1 low',
      as.numeric( status.is ) <  as.numeric( status.pred ) ~ '3 high'
      )
  ) %>% 
  group_by( dep, ind, type, status.is, prediction, N ) %>% 
  summarise( pct = n() ) %>%
  mutate   ( pct = round(pct*100/N,0 ) ) %>% 
  arrange(type, status.is) %>% 
  # ungroup %>% select(-status.pred, -N) %>%
  spread(prediction, pct) %>% 
  as.data.frame() %>% .ct
  


coeffs. <- readRDS('DATA derived/coefficients.rds') %>% 
  filter( !is.na(split) ) %>% 
  filter( type == 'pct', type2 == 'all') %>% 
  select( dep, paramcd = indep, inter, slope, split)

dt.pred <- dt. %>% 
  select (-fds) %>% 
  filter ( paramcd %in% c('mFARS','SARA','ICARS', 'FARS.E','SARA.ax','ICARS.ax')) %>% 
  left_join(coeffs.) %>% 
  filter( dep %in% c('mFARS','SARA','ICARS', 'FARS.E','SARA.ax','ICARS.ax')) %>% 
  mutate( dep.pred = (aval * slope) + inter)

dt.pred %>%
  left_join(df.fds) %>% 
  filter(paramcd %in% c('FARS.E','SARA.ax','ICARS.ax') ) %>% 
  ggplot()+
  # geom_boxplot(outlier.shape = 21, outlier.fill = NA, outlier.alpha = .5, width = .5)+
  geom_boxplot()+
  aes( x = factor(fds), y = dep.pred)+    
  aes( fill = dep )+
  facet_wrap( ~ split, ncol = 3, scales = 'free_x' )+
  geom_vline(xintercept = seq(.5, 5.5, 1), linetype = 'solid', color = 'grey')+
  theme_pubclean(base_size = 16)+
  theme(axis.title.x = element_blank())+
  # ylab('Percentage of Total')+
  .leg_tl

# . -----------------------------------------------------------------------

bind_rows(
  dt.pred %>% select(study, sjid, avisitn, paramcd = dep, aval = dep.pred, indep = paramcd, split) %>% filter(paramcd %in% c('FARS.E','ICARS.ax')),
  dt.     %>% select(study, sjid, avisitn, paramcd      , aval                            ) %>% filter(paramcd %in% c('SARA.ax'))
) %>% #droplevels() %>% select(paramcd, indep) %>% table(exclude = F)
  rename (split. = split) %>% 
  left_join( df.fds ) %>% 
  filter(fds < 5) %>%
  # filter(paramcd %in% c('FARS.E','SARA.ax','ICARS.ax') ) %>% 
  ggplot()+
  # geom_boxplot(outlier.shape = 21, outlier.fill = NA, outlier.alpha = .5, width = .5)+
  geom_boxplot()+
  aes( x = factor(fds), y = aval)+    
  aes( fill = split. )+
  facet_wrap( ~ paramcd, ncol = 6, scales = 'free_x' )+
  geom_vline(xintercept = seq(.5, 5.5, 1), linetype = 'solid', color = 'grey')+
  theme_pubclean(base_size = 16)+
  theme(axis.title.x = element_blank())+
  # ylab('Percentage of Total')+
  .leg_tl

# percantage buckets (correct thing to do??) ------------------------------

dt <- dt. %>%
  filter ( type == 'pct') %>% 
  select ( study, sjid, avisitn, age, amb, paramcd, aval, fds = fds.act )

dt %>% 
  group_by(sjid, avisitn) %>% 
  filter ( paramcd %in% scales.list[c(1,2)]) %>% 
  filter ( n()==2) %>% 
  filter ( avisitn == min(avisitn) ) %>%
  mutate ( aval.cat = cut(aval, seq(0,100,20))) %>% 
  # filter ( is.na(aval.cat))
  ungroup %>% select(-aval) %>% 
  spread ( paramcd, aval.cat) %>% 
  mutate ( N = n()) %>% 
  group_by( mFARS, SARA, N ) %>% 
  summarise( n = n()) %>% 
  mutate(ok = ifelse(mFARS == SARA, 'ok','false')) %>% 
  ggplot()+geom_tile()+
  aes(y = mFARS, x = SARA, fill = ok )+
  geom_text(aes(label = n))


parlist <- scales.list[c(4,5)]
parlist <- scales.list[c(4,5)]

dt %>% 
  group_by(sjid, avisitn) %>% 
  filter ( paramcd %in% parlist ) %>% 
  filter ( n()==2) %>% 
  group_by(paramcd) %>% 
  # filter ( avisitn == min(avisitn) ) %>%
  mutate ( aval.cat = cut(aval, seq(0,100,20))) %>%
  # mutate ( aval.cat = cut_interval(aval, 5)) %>%
  # mutate ( aval.cat = rank(aval.cat, ties.method = 'average')) %>% 
  # filter ( is.na(aval.cat))
  ungroup %>% select(-aval) %>% 
  spread ( paramcd, aval.cat) %>% 
  mutate ( N = n()) %>% 
  group_by( FARS.E, SARA.ax, N) %>%
  # group_by( mFARS, SARA, N) %>% 
  summarise( n = n()) %>% 
  mutate ( class = ifelse(FARS.E == SARA.ax, 'ok','false')) %>% 
  mutate ( pct = round(100*n/N,0) ) %>% 
  group_by(class) %>% 
  summarise(sum(pct))

