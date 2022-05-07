
# . -----------------------------------------------------------------------

rm(list=ls())

scales <- readRDS('DATA derived/scales.rds')
scales.list <- scales$paramcd

lm.mod <- readRDS('DATA derived/models.predictions.rds')

# model criteria ----------------------------------------------------------

lm.mod %>% 
  select(-data, -pred.lm, -pred.pl) %>% 
  gather(type, model, mod.lm, mod.pl) %>% 
  mutate( glance = map (model, ~ broom::glance(.))) %>% 
  unnest( glance ) %>% 
  # filter( dep %in% c('FARS.E','SARA.ax','ICARS.ax')) %>% 
  filter( dep %in% c('mFARS','SARA' ,        'FARS.E','SARA.ax' )) %>% 
  filter( ind %in% c('mFARS','SARA' ,'ICARS','FARS.E','SARA.ax','ICARS.ax')) %>% 
  ungroup %>% 
  select(dep, ind, type, r.squared, AIC, BIC, p.value ) %>% 
  # select(dep, ind, type, r.squared, adj.r.squared, AIC, BIC, p.value ) %>% 
  mutate(p.value = '<0.0001') %>% 
  pivot_wider(id_cols = c(dep, ind), values_from = c(r.squared, AIC, BIC, p.value), names_from = type) %>% 
  # pivot_wider(id_cols = c(dep, ind), values_from = c(r.squared, adj.r.squared, AIC, BIC, p.value), names_from = type) %>% 
  select(dep, ind, ends_with('mod.lm'), everything()) %>% 
  mutate(dep = gsub('\\.',' ',dep)) %>% 
  mutate(ind = gsub('\\.ax','\\-AX',ind)) %>% 
  qflextable() %>% 
  colformat_double(digits = 2) %>%
  colformat_double(digits = 0, j = c(4,5,8,9), big.mark = '') %>%
  # colformat_double(digits = 0, j = c(5,6,10,11), big.mark = '') %>%
  set_header_labels(
    values = c(dep = 'Dep. Var.',ind = 'Indep. Var',
               r.squared_mod.lm='R^2',AIC_mod.lm='AIC',BIC_mod.lm='BIC',p.value_mod.lm = 'p.value',
               r.squared_mod.pl='R^2',AIC_mod.pl='AIC',BIC_mod.pl='BIC',p.value_mod.pl = 'p.value')
  ) %>%
  # set_header_labels(
  #   values = c(dep = 'Dep. Var.',ind = 'Indep. Var',
  #              r.squared_mod.lm='R^2',adj.r.squared_mod.lm='R^2 adj',AIC_mod.lm='AIC',BIC_mod.lm='BIC',p.value_mod.lm = 'p.value',
  #              r.squared_mod.pl='R^2',adj.r.squared_mod.pl='R^2 adj',AIC_mod.pl='AIC',BIC_mod.pl='BIC',p.value_mod.pl = 'p.value')
  # ) %>%
  set_table_properties(layout = "autofit", width = .8) %>% 
  add_header_row(top = T, values = c('', 'linear model','2nd order poly. model'), colwidths = c(2,4,4)) %>% 
  align(align = 'center', part = 'all') %>% 
  autofit()
  
