library(shiny)
library(shinydashboard)
library(ggplot2)

#3 different tabs to work with

body <- dashboardBody(
  fluidRow(tabBox(
    width = 12,
    
    ####tab ctv####
    tabPanel(
      "ctv",
      box(
        title = "filter",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        selectizeInput(
          'ctvs_select',
          'ctvs to select',
          choices = ctvs,
          multiple = TRUE,
          options = list(maxItems = 40),
          selected = "Psychometrics"
        ),
        conditionalPanel(
          condition = "input.ctvs_select == 'Psychometrics'",
          selectizeInput(
            'subcategory_select',
            'subcategory_select to select',
            choices = psy_subs,
            multiple = TRUE,
            options = list(maxItems = 11),
            selected = "Item Response Theory (IRT)"
          )
        )
      ),
    
      
      
      fluidRow(
        box(
          title = "download statistics of CTV",
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          plotlyOutput("ctv_plot")
        ),
        conditionalPanel(
          condition = "input.ctvs_select == 'Psychometrics'",
          box(
            title = "Comparison of sub categories",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("sub_plot")
          )
        ),
        
        conditionalPanel(
          condition = "input.ctvs_select != 'Psychometrics'",
          box(
            title = "average download statistics of CTV (relative to package count)",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("downloads_per_ctv")
          )
        )
        
      ),
      
      fluidRow(
        box(
          title = "date range",
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          sliderInput(
            'year',
            "time span",
            min = as.Date("2012-10-01"),
            max = as.Date("2019-07-31"),
            value = c(as.Date("2018-07-31"), as.Date("2019-07-31")),
            timeFormat = "%Y-%m"
          )
          
        ),
        
        box(
          title = "number of packages per ctv",
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          tableOutput("pkg_per_ctv")
          
        )
      )
    ),
    
    
    ####tab package#####
    tabPanel(
      "package",
      box(
        title = "filter",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        selectizeInput(
          'packages_select',
          'packages to select',
          choices = packages,
          multiple = TRUE,
          options = list(maxItems = 100),
          selected = "dplyr"
        )
      ),
    
      
      #box(
        #title = "filter",
        #width = 12,
        #solidHeader = TRUE,
        #status = "primary",
        #selectizeInput(
         # 'packages_select',
          #'packages to select',
          #choices = list(Psychometrics = c('ade4', 'lavaan' ),
           #              Bayesian = c('ku', 'x')),
          
          #multiple = TRUE,
          #options = list(maxItems = 100),
          #selected = "dplyr"
          #)
        #),
      fluidRow(
        box(
          title = "plot: dependencies",
          width = 6,
          solidHeader = TRUE,
          status = "primary"
          ,visNetworkOutput("dep_plot", width = "100%")
        ),
        
        box(
          title = "plot: linechart",
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          plotOutput("pkg_plot")
        )
        
      ),
      
      fluidRow(
        box(
          title = "dependencies",
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          checkboxGroupInput(
            'dep_select',
            label = "specify type of dependencies",
            choices = list(
              "depends" = 1,
              "imports" = 2,
              "suggests" = 3
            ),
            selected = c(1,2,3)
          )
        ),
        box(
          title = "date range",
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          sliderInput(
            'date_selection_package',
            "time span",
            min = as.Date("2012-10-01"),
            max = as.Date("2019-07-31"),
            value = c(as.Date("2018-07-31"), as.Date("2019-07-31")),
            timeFormat = "%Y-%m"
            
          )
          
        )
      ))
  )))


# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(dashboardHeader(title = "CRAN"),
                    dashboardSidebar(collapsed = TRUE),
                    body)

