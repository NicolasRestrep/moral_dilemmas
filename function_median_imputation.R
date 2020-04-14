smaller <- which(d_long$reaction_time_imm < 1)


replace_small_imm <- function(x) {
  
ss <- d_long[x, 2] %>% 
  as.vector() %>% 
  as.character()

gdi_scenario <- gdi %>% 
  filter(str_detect(scenario, ss)) %>% 
  select(med) %>% 
  as.numeric() %>% 
  as.vector()

d_long[x, 5] <<- gdi_scenario

}


map(.x = smaller, .f = replace_small_imm)
