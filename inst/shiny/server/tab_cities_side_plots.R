
output$selectInputSidePlots <- renderUI({
  plots <- list(
    "Pollutant"="pollutant",
    "Fire contribution"="fire_contrib",
    "Fire count"="fire_count",
    "Precipitation"="precip")

  pickerInput("side_plots", "Charts", choices=plots, selected=c("pollutant","fire_contrib","fire_count"),
              options = list(`actions-box` = TRUE, `selected-text-format`= "count"), multiple = T)
})


output$sidePlots <- renderPlotly({

  # req(side_plot_poll())
  # req(side_plot_fire())
  # req(side_plot_firecontribution())
  req(input$side_plots)

  plots <- list(
    "fire_count"=side_plot_fire(),
    "precip"=side_plot_precip(),
    "pollutant"=side_plot_poll(),
    "fire_contrib"=side_plot_firecontribution()
  )[input$side_plots]
  
  plots <- plots[!is.na(plots)]
  plots <- plots[!unlist(lapply(plots, is.null))]

  plotly::subplot(plots,
                  nrows = length(plots),
                  shareX = TRUE,
                  titleX = FALSE,
                  titleY = FALSE,
                  shareY = T,
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


side_plot_poll <- reactive({
  
  if(is.null(meas())) return(NA)
  
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
  
  data <- m %>%
    filter(variable %in% c('observed', 'predicted', 'predicted_nofire')) %>%
    select(date, variable, value) %>%
    rcrea::utils.running_average(as.numeric(input$running_width)) %>%
    tidyr::spread(variable, value) 
  

  
  result <- data %>%
    plot_ly(
      type="scatter",
      mode="lines"
    )
  
  if('observed' %in% names(data)){
    result <- result %>%
      plotly::add_lines(x=~date,
                        y=~observed,
                        name="Observed",
                        opacity=0.4,
                        hovertemplate = hovertemplate,
                        line = list(
                          color = 'rgb(0, 0, 0)',
                          width = 1
                        ))
  }
  
  if('predicted' %in% names(data)){
    result <- result %>%
      plotly::add_lines(x=~date,
                        y=~predicted,
                        name="Predicted with fire",
                        hovertemplate = hovertemplate,
                        line = list(
                          color = 'red',
                          width = 2
                        ))
  }
  
  if('predicted_nofire' %in% names(data)){
    result <- result %>%
      plotly::add_lines(x=~date,
                        y=~predicted_nofire,
                        name="Predicted without fire",
                        hovertemplate = hovertemplate,
                        line = list(
                          color = 'orange',
                          width = 2
                        ))
  }
  
  result %>%
    plotly::layout(
      margin=list(t=200),
      showlegend = F,
      hovermode  = 'x unified',
      # title=list(
      #     text=sprintf("%s [%s]",poll, unit),
      #     x=0.1,
      #     font=list(size=10)
      # ),
      yaxis = list(
        # title="", #sprintf("%s [%s]",poll, unit),
        rangemode = 'tozero'
      ),
      xaxis = list(
        title="",
        # showspikes = T,
        spikemode  = 'across+toaxis',
        spikesnap = 'cursor',
        # spikedash = 'solid',
        showline=T,
        showgrid=T
      ),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
      # fig_bgcolor   = "rgba(0, 0, 0, 0)"
    ) %>%
    plotly::add_annotations(
      text = sprintf("%s [%s]", poll_name, unit),
      x = -0.05,
      y = 1.19,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 12)
    ) %>%
    plotly::event_register('plotly_click')
})

side_plot_fire <- reactive({
  
  req(weather())
  req(selected_metadata())
  req(input$running_width)
  
  if(selected_metadata()$fire_source=="gfas"){
    fire_value_regex="^pm25_emission$"
    fire_value="pm25_emission"
    fire_name="PM25 emission from fires"
  }else{
    fire_value_regex="^fire_count.*$"
    fire_value="fire_count"
    fire_name="Fire count"
  }


  f <- weather()
  f[fire_value] <- rowSums(f[,grep(fire_value_regex, names(f))], na.rm=T)
  f <- f %>%
    dplyr::select_at(c("date", "value"=fire_value))
  
  f.rolled <- rcrea::utils.running_average(f, as.numeric(input$running_width))
  
  # selected <- which(trajs_meas_obs()$date==trajs_date())
  f.rolled %>%
    plot_ly(
      x = ~date,
      y = ~value
      # selectedpoints=as.list(selected),
    ) %>%
    plotly::add_lines(name=fire_name) %>%
    plotly::layout(
      # title=list(
      #     text="Fire count (within 10km of trajectories)",
      #     x=0.1,
      #     font=list(size=10)
      # ),
      showlegend = F,
      hovermode  = 'x unified',
      yaxis = list(
        title="", #Fire count (within 10km of trajectories)",
        rangemode = 'tozero'
      ),
      xaxis = list(title="")) %>%
    plotly::add_annotations(
      text = fire_name,
      x = -0.05,
      y = 1.19,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 12)
    )
})

side_plot_precip <- reactive({
  
  req(weather())
  req(input$running_width)
  
  f <- weather() %>%
    dplyr::select(date, value=precip) %>%
    mutate(value=value/10) #NOAA ISD has a 10 scaling factor
  
  f.rolled <- rcrea::utils.running_average(f, as.numeric(input$running_width))
  
  f.rolled %>%
    plot_ly(
      x = ~date,
      y = ~value
    ) %>%
    plotly::add_lines(name="Precipitation") %>%
    plotly::layout(
      showlegend = F,
      hovermode  = 'x unified',
      yaxis = list(
        title="",
        rangemode = 'tozero'
      ),
      xaxis = list(title="")) %>%
    plotly::add_annotations(
      text = "Precipitation (mm/day)",
      x = -0.05,
      y = 1.19,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 12)
    )
})

side_plot_firecontribution <- reactive({
  
  if(is.null(meas())) return(NA)
  req(input$running_width)
  poll <- input$poll
  req(poll)
  
  m <- meas() %>%
    filter(poll==!!poll)
  
  if(nrow(m)==0){
    return(NULL)
  }
  
  poll_name <- rcrea::poll_str(poll)
  unit <- unique(m$unit)
  
  hovertemplate <- paste('%{y:.0f}',unit)
  
  data <- m %>%
    filter(variable %in% c('observed', 'predicted', 'predicted_nofire')) %>%
    select(date, variable, value) %>%
    rcrea::utils.running_average(as.numeric(input$running_width)) %>%
    tidyr::spread(variable, value) %>%
    mutate(value=predicted-predicted_nofire) %>%
    select(date, value)
  

  
  # selected <- which(trajs_meas_obs()$date==trajs_date())
  data %>%
    plot_ly(
      type="scatter",
      mode="lines"
    ) %>%
    plotly::add_lines(x=~date,
                      y=~value,
                      hovertemplate = hovertemplate,
                      line = list(
                        width = 2
                      )) %>%
    plotly::layout(
      # title=list(
      #     text=sprintf("Fire contribution to %s [%s]",poll, unit),
      #     x=0.1,
      #     font=list(size=10)
      #     ),
      showlegend = F,
      hovermode  = 'x unified',
      yaxis = list(
        # title=sprintf("Fire contribution to %s [%s]",poll, unit),
        rangemode = 'tozero'
      ),
      xaxis = list(
        title="",
        # showspikes = T,
        spikemode  = 'across+toaxis',
        spikesnap = 'cursor',
        # spikedash = 'solid',
        showline=T,
        showgrid=T
      ),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    ) %>%
    plotly::add_annotations(
      text = sprintf("Fire contribution to %s [%s]",poll_name, unit),
      x = -0.05,
      y = 1.19,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 12)
    )
})