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
               plotlyOutput("citiesPlots", height="calc(100%)"),
               style="height: 100%"
             ),
             
           ),
           sidebarPanel(
             width = 4,
             div(
               uiOutput("radioMode") %>% withSpinner(color="#8cc9D0"),
             ),
             conditionalPanel(
               condition = "input.cities_mode == 'map'",
               div(
                 class="row-inline",
                 height=50,
                 uiOutput("selectInputTrajsCountry"),
                 uiOutput("selectInputTrajsCity")
               ),
               div(
                 class="row-inline",
                 height=50,
                 uiOutput("selectInputTrajsDuration"),
                 uiOutput("selectInputTrajsBuffer"),
                 uiOutput("selectInputTrajsFireSource"),
                 uiOutput("selectInputTrajsPoll")
               ),
             ),
             
             div(
               class="row-inline",
               height=50,
               uiOutput("selectInputTrajsRunning"),
               conditionalPanel(
                 condition = "input.cities_mode == 'map'",
                 uiOutput("selectInputTrajsPlots")
               )
             ),
             
             conditionalPanel(
               condition = "input.cities_mode == 'map'",
               plotlyOutput("trajsPlots", height="calc(100% - 300px)") #"calc(100% - 300px)")
             )

           )

         )
)
