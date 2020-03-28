
p <- plot_ly(events_def, x = ~be, y = ~bp, z = ~dop, color = ~type, 
             marker = list(symbol = 'circle'), 
             text = ~paste('Scenario: ', scenario)) %>% 
            layout(title = "Semantic Structures of Moral Wrongs", 
                   scene = list(xaxis = list(title = 'Behavior Evaluation'),
                                yaxis = list(title = 'Behavior Potency'),
                                zaxis = list(title = 'Change in Object Potency')))
p
  
chart_link <-  api_create(p, filename="scenarios-semantics")
chart_link
