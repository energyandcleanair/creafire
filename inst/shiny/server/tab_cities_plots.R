



output$plots <- renderPlotly({

  # req(plot_fire_count())
  # req(plot_fire_contribution())
  # req(plot_fire_frp())
  # req(plot_poll())
  
  # req(input$plots)

  plots <- list(
    plot_poll(),
    plot_fire_contribution(),
    plot_fire_count(),
    plot_fire_frp()
    )
  
  plots <- plots[!is.na(plots)]
  plots <- plots[!unlist(lapply(plots, is.null))]
  

  plotly::subplot(plots,
                  nrows = length(plots),
                  shareX = TRUE,
                  titleX = FALSE,
                  titleY = T,
                  # shareY = T,
                  margin = 0.05
  ) %>%
    plotly::layout(hovermode='x',
                   xaxis = list(
                     title="",
                     # showspikes = T,
                     spikemode  = 'across+toaxis',
                     spikesnap = 'cursor',
                     # spikedash = 'solid',
                     showline=T,
                     showgrid=T),
                   margin=list(t=60)
    )
})




plot_fire_count <- reactive({
  
  req(weather())
  req(input$running_width)
  # req(input$firesource)
  
  if(input$firesource=="gfas"){
    fire_value="pm25_emission"
    fire_name="PM25 emission from fires"
  }else{
    fire_value="fire_count"
    fire_name="Fire count"
  }
  
  fires <- weather() %>%
    dplyr::select_at(c("date", "value"=fire_value)) %>%
    rcrea::utils.running_average(input$running_width)
  
  fires %>% 
    mutate(year = year(date)) %>% 
    group_by(year) %>% 
    plot_ly(x = ~ lubridate::`year<-`(date, 2000),
            showlegend=T) %>% 
    add_lines(y = ~ value, 
              color = ~ factor(year),
              colors="Reds"
              # legendgroup="group1"
    ) %>%
    layout(
      
      hovermode  = 'x unified',
      xaxis=list(
        tickformat= '%d %B',
        title=""
      ),
      yaxis=list(
        title=fire_name,
        rangemode = 'tozero'
      )
    )
})


plot_fire_frp <- reactive({
  
  req(weather())
  req(input$running_width)
  req(selected_metadata())
  
  m <- selected_metadata()
  
  if(m$firesource=="gfas"){
    fire_value="pm25_emission"
    fire_name="PM25 emission from fires"
  }else{
    fire_value="fire_frp"
    fire_name="Fire Radiative Power (MW)"
  }
  
  fires <- weather() %>%
    dplyr::select_at(c("date", "value"=fire_value)) %>%
    rcrea::utils.running_average(input$running_width)
  
  fires %>% 
    mutate(year = year(date)) %>% 
    group_by(year) %>% 
    plot_ly(x = ~ lubridate::`year<-`(date, 2000),
            showlegend=F) %>% 
    add_lines(y = ~ value, 
              color = ~ factor(year),
              colors="Reds"
              # legendgroup="group1"
              
    ) %>%
    layout(
      hovermode  = 'x unified',
      xaxis=list(
        tickformat= '%d %B',
        title=""
      ),
      yaxis=list(
        title=fire_name,
        rangemode = 'tozero'
      )
    )
})




plot_fire_contribution <- reactive({
  
  req(meas())
  poll <- input$poll
  req(input$running_width, poll)
  
  m <- meas() %>%
    filter(poll==!!poll)
  
  if(nrow(m)==0){
    return(NULL)
  }
  
  poll_name <- rcrea::poll_str(poll)
  unit <- unique(m$unit)
  hovertemplate <- paste('%{y:.0f}',unit)
  
  m <- m %>%
    mutate(value=predicted-predicted_nofire) %>%
    select(date, value) %>%
    rcrea::utils.running_average(input$running_width) %>%
    mutate(date0000=lubridate::`year<-`(date, 2000))
  
  m %>% 
    mutate(year = year(date)) %>% 
    group_by(year) %>% 
    plot_ly(x = ~ lubridate::`year<-`(date, 2000),
            showlegend = F) %>% 
    add_lines(y = ~ value, 
              color = ~ factor(year),
              colors="Reds"
              # legendgroup="group1"
    )  %>%
    layout(
      hovermode  = 'x unified',
      xaxis=list(
        tickformat= '%d %B',
        title=""
      ),
      yaxis=list(
        title=sprintf("Fire contribution to %s (%s)", poll_name, unit),
        rangemode = 'tozero'
      )
    )
  
})


plot_poll <- reactive({
  
  req(meas())
  poll <- input$poll
  req(input$running_width, poll)
  
  m <- meas() %>%
    filter(poll==!!poll)
  
  if(nrow(m)==0){
    return(NULL)
  }
  
  poll_name <- rcrea::poll_str(poll)
  unit <- unique(m$unit)
  hovertemplate <- paste('%{y:.0f}',unit)
  
  m <- m %>%
    select(date, value=observed) %>%
    rcrea::utils.running_average(input$running_width) %>%
    mutate(date0000=lubridate::`year<-`(date, 2000))
  
  m %>% 
    mutate(year = year(date)) %>% 
    group_by(year) %>% 
    plot_ly(x = ~ lubridate::`year<-`(date, 2000),
            showlegend = F) %>% 
    add_lines(y = ~ value, 
              color = ~ factor(year),
              colors="Reds"
              # legendgroup="group1"
              )  %>%
    layout(
      hovermode  = 'x unified',
      xaxis=list(
        tickformat= '%d %B',
        title=""
      ),
      yaxis=list(
        title=sprintf("%s (%s)", poll_name, unit),
        rangemode = 'tozero'
      )
    )

})

