source("setup.R")

ui <- page_sidebar(
  title = h4("Eligibility and event rates - SwedeHF"),
  theme = mytheme,
  sidebar = sidebar(
    width = "30%",
    fillable = FALSE,
    card(
      card_header(h5("Ejection fraction (%)"), class = "bg-primary"),
      selectInput(
        inputId = "ef",
        label = NULL,
        choices = appvar_values$shf_ef,
        selected = appvar_values$shf_ef,
        multiple = TRUE
      )
    ),
    card(
      card_header(h5("Eligibility criteria"), class = "bg-primary"),
      selectInput(
        inputId = "prevhfh6mo",
        label = "HF hospitalization < 6 months",
        choices = appvar_values$shf_sos_prevhfh6mo,
        selected = appvar_values$shf_sos_prevhfh6mo,
        multiple = TRUE
      ),
      selectInput(
        inputId = "nyha",
        label = "NYHA class",
        choices = appvar_values$shf_nyha,
        selected = appvar_values$shf_nyha,
        multiple = TRUE
      ),
      selectInput(
        inputId = "gfrckdepi",
        label = "eGFR (mL/min/1.73 m²)",
        choices = appvar_values$shf_gfrckdepi,
        selected = appvar_values$shf_gfrckdepi,
        multiple = TRUE
      ),
      checkboxInput("byaf", "Separate NT-proBNP by atrial fibrillation"),
      conditionalPanel(
        condition = "input.byaf == true",
        selectInput(
          inputId = "ntprobnp_af",
          label = "NT-proBNP (pg/mL) with atrial fibrillation",
          choices = appvar_values$shf_ntprobnp_af,
          selected = appvar_values$shf_ntprobnp_af[2],
          multiple = TRUE
        ),
        selectInput(
          inputId = "ntprobnpnoaf_af",
          label = "NT-proBNP (pg/mL) without atrial fibrillation",
          choices = appvar_values$shf_ntprobnp_noaf,
          selected = appvar_values$shf_ntprobnp_noaf[2],
          multiple = TRUE
        )),
      conditionalPanel(
        condition = "input.byaf == false",
        selectInput(
          inputId = "ntprobnp_noaf",
          label = "NT-proBNP (pg/mL)",
          choices = appvar_values$shf_ntprobnp,
          selected = appvar_values$shf_ntprobnp,
          multiple = TRUE
        )),
      selectInput(
        inputId = "bpsys",
        label = "Systolic blood pressure (mmHg)",
        choices = appvar_values$shf_bpsys,
        selected = appvar_values$shf_bpsys,
        multiple = TRUE
      )
    ), card(
      card_header(h5("Event type"), class = "bg-primary"),
      selectInput(
        inputId = "out",
        label = NULL,
        choices = outvars$name,
        selected = outvars$name,
        multiple = TRUE
      )
    )
  ),
  navset_card_underline(
    nav_panel(
      title = "Table",
      icon = icon("table"),
      #             page_fillable(
      # br(),
      uiOutput("error"),
      # width = "250px",
      # fill = FALSE,
      value_box(
        title = NULL,
        uiOutput("eligibilty"),
        showcase = bsicons::bs_icon("people-fill"),
        theme = "bg-primary"
      ),
      # card(
      DT::dataTableOutput(outputId = "base"),
      padding = 0
      # )
      ,
      # card(
      DT::dataTableOutput(outputId = "event")
      # )
      # )
    ),
    nav_panel(
      title = "Graph",
      icon = icon("chart-simple"),
      page_fillable(
        uiOutput("error2"),
        plotOutput("plot")
      )
    ),
    nav_panel("Information",
              value = "Information",
              icon = icon("info"),
              p("The displayed information is the last registration / patient in the Swedish Heart Failure Registry (SwedeHF) 2016-2023 with recorded Ejection fraction and discharged alive from hospital (N = 46,636)."),
              p("The outcomes were derived from the National Patient register (hospitalizations) and The Cause of Death Register (deaths). HF was defined as ICD-10 I110, I130, I132, I255, I420, I423, I425-9, I43, I50, J81, K761, R570 and CV as I, J81, K761, R570, G45."),
              p("The displayed information is the last registration / patient in the Swedish Heart Failure Registry (SwedeHF) 2016-2023 and discharged alive from hospital."),
              p("Missing values are imputed with a multivariate imputation algorithm based on random forests (Mayer M (2024). _missRanger: Fast Imputation of Missing Values_. doi:10.32614/CRAN.package.missRanger")
    )
  )
)


server <- function(input, output) {
  show_modal_spinner()
  Sys.sleep(1.5)
  remove_modal_spinner()
  select_func2 <- reactive({
    select_func(
      ef = input$ef,
      prevhfh6mo = input$prevhfh6mo,
      nyha = input$nyha,
      gfrckdepi = input$gfrckdepi,
      ntprobnp = input$ntprobnp,
      bpsys = input$bpsys
    )
  })
  
}


# Create Shiny app ----
shinyApp(ui, server)
