library(shiny)
if(!grepl('Artist_song_app',getwd())){setwd('Artist_song_app')}

source("Artist_song_app.R")
server <- function(input, output) {
  artist_id_holder=reactive({
    req(input$artist1)
    get_artist_id(artist = input$artist1)
  })
  
  output$Run_button=renderUI({
    req(artist_id_holder())
    verticalLayout(
    actionButton("load", "Click to Load")
    )})
  
  artist_id_holder2=reactive({
    req(input$artist2)
    get_artist_id(artist = input$artist2)
  })
  
  output$Run_button2=renderUI({
    req(artist_id_holder2())
    verticalLayout(
      actionButton("load2", "Click to Load")
    )})
  
  
  song_holder<-eventReactive(input$load, {
    req(input$artist1)
    req(artist_id_holder())
    get_track_list_and_lengths(
      artist = input$artist1,
      artist_id = artist_id_holder()
    )
    })
  
  
  song_holder2<-eventReactive(input$load2, {
    req(input$artist2)
    req(artist_id_holder2())
    get_track_list_and_lengths(
      artist = input$artist2,
      artist_id = artist_id_holder2()
    )
  })
  
  song_holder2_dummy<-reactive({
    if(nrow(song_holder2())>0){
      song_holder2()
    }else{NA}
  })
  
  output$MainPanel=renderUI({
    req(artist_id_holder())
    if(is.na(artist_id_holder())){
      verticalLayout(
     h2('Artist not Recognised') 
      )
    }else{
      verticalLayout(
        fluidRow(
        column(6,
      renderDT({
        req(song_holder())
        make_DT_output(df=song_holder())
      })
      ),
      column(6,
             renderDT({
               req(song_holder2())
               make_DT_output(df=song_holder2())
             })
      )
      ),
      br(),
      renderPlotly({
        req(song_holder())
        make_Plot_output(df_1=song_holder(),
                         df_2=song_holder2_dummy())
      })
      )
    }
    })
  
}

ui <- fluidPage(theme = shinytheme("cosmo"),
  titlePanel("Song Lengths by Artist"),
  sidebarPanel(
        textInput('artist1', 'Input First Artist Name', value = "", width = NULL,
            placeholder = NULL),
        htmlOutput("Run_button"),
        br(),
        textInput('artist2', 'Input Second Artist Name', value = "", width = NULL,
                  placeholder = NULL),
        htmlOutput("Run_button2")
              ),
  mainPanel(
    verticalLayout(
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage")
    ),
    htmlOutput("MainPanel")
  )
  )
)
shinyApp(ui, server,options=list("launch.browser"=T))