
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

  req(side_plot_poll())
  req(side_plot_fire())
  req(side_plot_firecontribution())
  req(input$side_plots)

  plots <- list(
    "pollutant"=side_plot_poll(),
    "fire_contrib"=side_plot_firecontribution(),
    "fire_count"=side_plot_fire(),
    "precip"=side_plot_precip()
  )[input$side_plots]

  plots <- plots[!is.na(plots)]

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
  
  req(trajs_meas_all())
  poll <- input$poll
  req(input$running_width, poll)
  
  m <- trajs_meas_all() %>%
    filter(poll==!!poll)
  
  if(nrow(m)==0){
    return(NULL)
  }
  
  poll_name <- rcrea::poll_str(poll)
  unit <- unique(m$unit)
  hovertemplate <- paste('%{y:.0f}',unit)
  
  m <- m %>%
    select(date, observed, predicted, predicted_nofire)
  
  m.rolled <- rcrea::utils.running_average(m, input$running_width, vars_to_avg = c("observed","predicted","predicted_nofire"))
  
  
  # selected <- which(trajs_meas_obs()$date==trajs_date())
  m.rolled %>%
    plot_ly(
      type="scatter",
      mode="lines"
    ) %>%
    plotly::add_lines(x=~date,
                      y=~observed,
                      name="Observed",
                      opacity=0.4,
                      hovertemplate = hovertemplate,
                      line = list(
                        color = 'rgb(0, 0, 0)',
                        width = 1
                      )) %>%
    plotly::add_lines(x=~date,
                      y=~predicted,
                      name="Predicted with fire",
                      hovertemplate = hovertemplate,
                      line = list(
                        color = 'red',
                        width = 2
                      )) %>%
    plotly::add_lines(x=~date,
                      y=~predicted_nofire,
                      name="Predicted without fire",
                      hovertemplate = hovertemplate,
                      line = list(
                        color = 'orange',
                        width = 2
                      )) %>%
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
  req(input$running_width)
  req(input$firesource)
  
  if(input$firesource=="gfas"){
    fire_value="pm25_emission"
    fire_name="PM25 emission from fires"
  }else{
    fire_value="fire_count"
    fire_name="Fire count"
  }
  f <- weather() %>%
    dplyr::select_at(c("date", "value"=fire_value))
  
  f.rolled <- rcrea::utils.running_average(f, input$running_width)
  
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
  
  f.rolled <- rcrea::utils.running_average(f, input$running_width)
  
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
  
  req(trajs_meas_all())
  req(input$running_width)
  poll <- input$poll
  req(poll)
  
  m <- trajs_meas_all() %>%
    filter(poll==!!poll)
  
  if(nrow(m)==0){
    return(NULL)
  }
  
  poll_name <- rcrea::poll_str(poll)
  unit <- unique(m$unit)
  
  hovertemplate <- paste('%{y:.0f}',unit)
  m <- m %>%
    select(date, observed, predicted, predicted_nofire) %>%
    mutate(value=predicted-predicted_nofire) %>%
    select(date, value)
  
  m.rolled <- rcrea::utils.running_average(m, input$running_width)
  
  
  # selected <- which(trajs_meas_obs()$date==trajs_date())
  m.rolled %>%
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