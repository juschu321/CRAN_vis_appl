library(shiny)
library(shinydashboard)
library(ggplot2)

#3 different tabs to work with

body <- dashboardBody(fluidRow(tabBox(
  width = 12,
  
  ####TAB CTV####
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
          max = as.Date("2019-08-31"),
          value = c(as.Date("2018-07-31"), as.Date("2019-08-31")),
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
    fluidRow(
      box(
        title = "importance",
        width = 3,
        solidHeader = TRUE,
        status = "primary",
        # Input: Specification of range within an interval ----
        sliderInput(
          "importance_range",
          "Range:",
          min = 0,
          max = 1100,
          value = c(0, 1100)
        )
      ),
      box(
        title = "dependencies",
        width = 3,
        solidHeader = TRUE,
        status = "primary",
        checkboxGroupInput(
          'dep_select',
          label = "specify type of dependencies",
          choices = list("depends",
                         "imports",
                         "suggests"),
          selected = c("depends",
                       "imports",
                       "suggests")
        )
      ),
      box(
        title = "filter",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        uiOutput("package_selection")
      )
      
    ),
    fluidRow(
      box(
        title = "plot: importance histgram",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        plotlyOutput("importance_hist")
      ),
      conditionalPanel(
        condition = "input.packages_select",
        box(
          title = "plot: dep",
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          visNetworkOutput("dep_plot")
        )
        
      )
    ),
    fluidRow(conditionalPanel(condition = "input.packages_select",
                              box(
                                width = 6,
                                box(
                                  title = "plot: count downloads",
                                  width = 12,
                                  solidHeader = TRUE,
                                  status = "primary",
                                  plotlyOutput("pkg_plot", width = "100%")
                                ),
                                box(
                                  title = "date range",
                                  width = 12,
                                  solidHeader = TRUE,
                                  status = "primary",
                                  sliderInput(
                                    'date_selection_package',
                                    "time span",
                                    min = as.Date("2012-10-01"),
                                    max = as.Date("2019-08-31"),
                                    value = c(as.Date("2018-08-31"), as.Date("2019-08-31")),
                                    timeFormat = "%Y-%m"
                                    
                                  )
                                  
                                )
                              )))
  )
)))


# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(dashboardHeader(title = "CRAN"),
                    dashboardSidebar(collapsed = TRUE),
                    body)
