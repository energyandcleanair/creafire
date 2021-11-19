tabPanel("Trajectories",
         value="trajectories",
         class = "no-padding-tab",
         sidebarLayout(
           mainPanel(
             width=8,
             conditionalPanel(
               condition = "input.cities_mode == 'map'",
               leafletOutput("maptrajs", height = "calc(100%)"),
               absolutePanel(left=25,
                             top=10,
                             width=160,
                             htmlOutput("trajsInfos", height="120px")
               ),
               absolutePanel(bottom = 10, right = "10%", width="80%",
                             uiOutput("selectInputTrajsDates", height = "30px")
               ),
               style="height: 100%"
             ),
             conditionalPanel(
               condition = "input.cities_mode == 'charts'",
               plotlyOutput("plots", height="calc(100%)"),
               style="height: 100%"
             ),
             
           ),
           sidebarPanel(
             width = 4,
             div(
               class="row-inline",
               uiOutput("radioMode"),
               style="height: 30px",
               id="mode"
             ),
             div(
               class="row-inline",
               height=50,
               uiOutput("selectInputCountry") %>% withSpinner(color="#8cc9D0"),
               uiOutput("selectInputCity")
             ),
             conditionalPanel(
               condition = "input.cities_mode == 'map'",
               div(
                 class="row-inline",
                 height=50,
                 uiOutput("selectInputDuration"),
                 uiOutput("selectInputBuffer"),
                 uiOutput("selectInputFireSource"),
                 uiOutput("selectInputPoll")
               ),
             ),
             
             div(
               class="row-inline",
               height=50,
               uiOutput("selectInputRunning"),
               conditionalPanel(
                 condition = "input.cities_mode == 'map'",
                 uiOutput("selectInputSidePlots")
               )
             ),
             
             conditionalPanel(
               condition = "input.cities_mode == 'map'",
               plotlyOutput("sidePlots", height="calc(100%)"),
               style = "height: calc(100% - 280px)"
             )

           )

         )
)
