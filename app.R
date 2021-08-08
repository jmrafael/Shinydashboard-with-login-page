library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)


# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("ENTRADA", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Utilizador...", label = tagList(icon("user"), "Utilizador")),
                   passwordInput("passwd", placeholder="Senha de acesso...", label = tagList(icon("unlock-alt"), "Senha de acesso")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "ENTRAR", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Utilizador ou Senha de acesso invalidos!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Utilizador: utilizador1 | Senha de acesso: senha1"),
                     br(),
                     tags$code("Utilizador: utilizador2 | Senha de acesso: senha2"),
                     
                     br(),
                     br(),
                     #tags$code("Credits to https://www.listendata.com/2019/06/how-to-add-login-page-in-shiny-r.html"),
                     #tags$code(a(icon("fa fa-sign-out"), "Sair", href="javascript:window.location.reload(true)"),
                     
                     tags$code(a(icon("fa fa-globe"), "Credits", 
                               href="https://www.listendata.com/2019/06/how-to-add-login-page-in-shiny-r.html"),
                               target="_blank",
                             class = "dropdown", 
                             style = "background-color: #eee !important; border: 0;
                              font-weight: bold; margin:5px; padding: 10px;")
                     
                   ))
)

credentials = data.frame(
  username_id = c("utilizador1", "utilizador2"),
  passod   = sapply(c("senha1", "senha1"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

header <- dashboardHeader( title = "R ShinyDashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Sair", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebarMenu(
        menuItem("Menu 1", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Menu 2", tabName = "second", icon = icon("th"))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        
        # First tab
        tabItem(tabName ="dashboard", class = "active",
                fluidRow(
                  box(width = 12, dataTableOutput('results'))
                )),
        
        # Second tab
        tabItem(tabName = "second",
                fluidRow(
                  box(width = 12, dataTableOutput('results2'))
                )
        ))
      
    }
    else {
      loginpage
    }
  })
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
  output$results2 <-  DT::renderDataTable({
    datatable(mtcars, options = list(autoWidth = TRUE,
                                     searching = FALSE))
  })
  
  
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)