testdata <- CPIdata %>% filter(Product=="General Index")

plot1 = plot_ly(testdata, x = ~Date, y = ~Index, type = 'scatter', mode = 'lines', color=~Source)%>%
  layout(title = "",
         xaxis = list(title = ""),
         yaxis = list (title = "Index, February 2014 = 100"),
         legend = list(orientation='h', xanchor = "center", x = 0.45))

plot1

plot2 <- ggplot(testdata)+
          geom_line(aes(x=Date, y=Index, colour=Source))+
          theme_classic()+
          labs(x="",
               y="Index, February 2014 = 100",
               colour="Location",
               title = "CPI for General Index")+
          theme(legend.position="bottom")


plot2 <- ggplotly(plot2)

plot2
