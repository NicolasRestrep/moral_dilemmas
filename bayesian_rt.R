b1 <- brm(formula = reaction_time_harm ~ 1 + distances_prot + length + (1 | ids) + (1 | scenario), 
          iter = 5000, warmup = 1000, chains = 4, cores = 4,  
          control = list(adapt_delta = 0.95), 
          family = gaussian, 
          data = d_long)

b2 <- update(b1, 
             formula = reaction_time_harm ~ 1 + distances_prot + I(distances_prot^2) + length + (1 | ids) + (1 | scenario), 
             newdata = d_long)
b3 <- update(b1, 
             formula = reaction_time_harm ~ 1 + log(distances_prot) + length + (1 | ids) + (1 | scenario), 
             newdata = d_long)

b1 <- add_criterion(b1, c("waic", "loo"))
b2 <- add_criterion(b2, c("waic", "loo"))
b3 <- add_criterion(b3, c("waic", "loo"))

loo_compare(b1,b2,b3, criterion = "loo")
model_weights(b1,b2,b3, weights = "loo")

marginal_effects(b2)
marg<-marginal_effects(b2)

new_data <- marg$distances_prot
outliers <- d_long %>% 
  group_by(scenario) %>% 
  arrange(desc(reaction_time_harm)) %>% 
  slice(1) %>% 
  ungroup()

ggplot() + 
  geom_line(data = new_data, aes(x = distances_prot, y = estimate__), size = 1.3, color = "blue") + 
  geom_ribbon(data = new_data, aes(x = distances_prot, y = estimate__, ymax = upper__, ymin = lower__), fill="skyblue4",alpha=0.3) + 
  geom_point(data = d_long, aes(x=distances_prot, y=reaction_time_harm, col = scenario),alpha=0.08) + 
  geom_text_repel(data = dsl, aes(x = distances_prot, y = mean_harm, label = factor(scenario))) + 
  labs(x = "Distance from the Prototype", 
       y = "Reaction Time", 
       caption = "Labels located at the average reaction time", 
       title = "Reaction Time by Distance from Prototype", 
       subtitle = "Raw data and fitted model") + 
  theme(legend.position = "none")

ggsave("quadratic_model.png")

