library(shiny)
library(tidyverse)
library(readr)
library(shinythemes)
library(leaflet)
library(shinycssloaders)
library(pointblank)
library(readxl)
library(cpp11)
library(progress)
library(lubridate)
library(pointblank)
library(ggplot2)
library(patchwork)
library(DT)

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Intervention Effect Analysis",
  theme = shinytheme('flatly'),
  tabPanel(
    "Overview",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          id = "sidebar_panel", 
          style = "background-color: inherit;", 
          tags$br(), 
          tags$br(), 
          img(src = "CGH_logo.png", style = "width: 100%;"),
          h2(strong("Intervention Effect Analysis")),
          h2("By H.A.T - Incubator A Team", style = "font-size:150%"),
          width = 3,
          uiOutput("custom_css")
        ),
        mainPanel(
          h2(strong("Problem Statement")),
          hr(),
          span("Proving that infection prevention measures work is difficult when infections are already rare. 
                In these low-risk settings, even effective interventions may only prevent a small number of additional infections. 
                This makes it hard to show clear improvements with numbers alone. 
                For example, reducing infections from 2% to 1% represents a meaningful achievement, but the actual difference of just one percentage point can seem insignificant. 
                As a result, hospitals and researchers struggle to demonstrate the value of their infection control efforts, even when these measures are genuinely helping patients. 
                The small numerical changes don't always reflect the true importance of the improvements being made.",
               style = "font-size:150%"
          ),
          h2(strong("App Functions")),
          hr(),
          tags$ol(
            tags$li(
              style = "font-size: 150%",
              strong("Data import"),
              tags$ul(
                tags$li("Users are to put their files into here to check their dataset before proceeding to the analysis")
                )
            ),
            tags$li(
              style = "font-size: 150%",
              strong("Boot-strapping and analytical framework"),
              tags$ul(
                tags$li("Bootstraps the data to expand the data size"),
                tags$li("Confidence interval analysis")
                )
            )
          ),
         h2(strong("Requirements")),
         hr(),
         span("Do note that the datafile has to follow this format",bstyle = "font-size:150%"),
         br(),
         img(src = "data_format.png", style = "width: 80%; max-width: 600px;"),
         br(),
         br(),
         br(),
         br(),
         br()
        )
      )
    )
  ),
  tabPanel(
    "Data Import", 
    fluid = TRUE, 
    icon = icon("upload"),
    sidebarLayout(
      position = 'left',
      sidebarPanel(
        fluid = TRUE, 
        width = 4, 
        id = "sidebar_panel", 
        style = "background-color: inherit;", 
        tags$strong("Data Import (Intervention Data):"), 
        tags$br(), 
        tags$hr(), 
        fileInput(
          "data_file", 
          "Upload csv/xlsx file of intervention data", 
          accept = c(".csv", ".xlsx")
        ), 
        actionButton("submit", "Submit")
      ),
      mainPanel(
        h4("Preview of Uploaded Data:"),
        DT::dataTableOutput("data_preview"),
        tabsetPanel(
          tabPanel(
            "User Guide", 
            column(
              12, 
              h4("This page previews the table for the user to check through the table. This is to ensure that the table is formatted properly")
            )
          )
        )
      )
    )
  ),
  tabPanel(
    "Boot Strapping & Analytical Framework", 
    fluid = TRUE, 
    icon = icon("glyphicon-play"),
    sidebarLayout(
      position = 'left',
      sidebarPanel(
        fluid = TRUE, 
        width = 4,
        id = "sidebar_panel",
        style = "background-color: inherit;",
        tags$strong("Select Parameters:"),
        tags$br(),
        tags$hr(),
        numericInput(
          inputId = "boot_n",
          label = "Select iterations",
          min = 0,
          max = 500,
          step =50,
          value = 100
        ),
        actionButton("Boot_run", "Submit")
      ),
      mainPanel(
        h4("Confidence Interval Graph:"),
        hr(),
        plotOutput("plot1", width = "100%", height = "600px"),
        hr(),
        # plotOutput("plot2"),
        tabsetPanel(
          tabPanel(
            "User Guide",
            column(
              12,
              h4("The violin graph shows the distribution of each group. The centre line represents the 95% confidence interval."),
              h4("For the intervention to be effective, this confidence interval must fall below the value 0.")
            )
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Reactive value to store uploaded data
  uploaded_data <- reactiveVal(NULL)
  
  # When Submit button is clicked
  observeEvent(
    input$submit, 
    {
      req(input$data_file)
      file_ext <- tools::file_ext(input$data_file$name)
    
      # Read the data based on file type
      data <- switch(
        file_ext,
        csv = read_csv(input$data_file$datapath, show_col_types = FALSE),
        xlsx = read_xlsx(input$data_file$datapath),
        {
          showNotification("Unsupported file type. Please upload .csv or .xlsx", type = "error")
          return(NULL)
        }
      )
      
      data_columns = names(data)
      required_columns <- c(
      "Date", "Intervention_count", "Intervention_exposure", 
       "Control_count", "Control_exposure", "Intervention"
      )
    
      # Check if all required columns are present
      if (all(required_columns %in% data_columns)) {
        data <- data |>
          mutate(Date = ymd(Date))
        uploaded_data(data)
      } else {
        # Find which columns are missing
        missing_columns <- setdiff(required_columns, data_columns)
        showNotification(
          paste(
            "Invalid column names - Missing columns:", 
            paste(missing_columns, collapse = ", "), 
            "\nPlease amend and resubmit data"
          ),
          type = "error",
          duration = 10
        )
      }
    }
  )
  
  # Render table preview
  output$data_preview <- DT::renderDataTable(
    {
      req(uploaded_data())
    }
  )
  
  # Second Tab
  observeEvent(
    input$Boot_run, 
    {
      req(uploaded_data())
      n_iterations = input$boot_n
      raw_data <- uploaded_data()
      
      samplemean <- function(x,d) {
        return(mean(x[d])*1000)
      }
      
      # Data visualisation segment
      output$plot1 <- renderPlot(
      {
        req(uploaded_data())
        raw_data <- uploaded_data()
        n_iterations = input$boot_n
        
        # ---- 2. Get 2018 and 2019 data ----
        filter_data <- raw_data |> 
          dplyr::filter(
            !is.na(.data[["Intervention"]])
          ) |> 
          dplyr::mutate(
            Date = lubridate::ymd(.data[["Date"]]),
            Years = lubridate::year(.data[["Date"]]),
            Intervention = dplyr::case_when(
              .data[["Intervention"]] <= 0.5 ~ 0,
              .data[["Intervention"]] > 0.5 & .data[["Intervention"]] <= 1 ~ 1,
              .default = .data[["Intervention"]]
            )
          ) |> 
          pointblank::col_vals_in_set(
            columns = c("Intervention"),
            set = c(0, 1)
          ) |> 
          dplyr::filter(
            .data[["Years"]] %in% c("2018", "2019")
          )
        
        # ---- 3. Get control data ----
        control <- filter_data |> 
          dplyr::select(
            c("Control_count", "Control_exposure", "Intervention")
          ) |> 
          dplyr::group_by(
            .data[["Intervention"]]
          ) |> 
          dplyr::summarise(
            Count = sum(.data[["Control_count"]], na.rm = TRUE),
            Exposure = sum(.data[["Control_exposure"]], na.rm = TRUE),
            NoCount = .data[["Exposure"]] - .data[["Count"]]
          )
        
        ## ---- 3a Get statistics from control data ----
        
        pre_control_vector <- c(
          rep(1, control$Count[1]),
          rep(0, control$NoCount[1])
        )
        
        post_control_vector <- c(
          rep(1, control$Count[2]),
          rep(0, control$NoCount[2])
        )
        
        pre_control_bootstrap_dist <- pre_control_vector |> 
          boot::boot(statistic = samplemean, R = n_iterations)
        post_control_bootstrap_dist <- post_control_vector |> 
          boot::boot(statistic = samplemean, R = n_iterations)
        
        # Distribution
        pre_control_bootstrap_t <- pre_control_bootstrap_dist$t
        post_control_bootstrap_t <- post_control_bootstrap_dist$t
        
        # Mean point
        pre_control_bootstrap_t0 <- pre_control_bootstrap_dist$t0
        post_control_bootstrap_t0 <- post_control_bootstrap_dist$t0
        
        # 95% CI
        pre_control_bootstrap_ci <- pre_control_bootstrap_t |> 
          quantile(probs = c(0.05, 0.95), type = 7)
        
        post_control_bootstrap_ci <- post_control_bootstrap_t |> 
          quantile(probs = c(0.05, 0.95), type = 7)
        
        # Differences
        diff_control_bootstrap_t <- post_control_bootstrap_t - pre_control_bootstrap_t 
        diff_control_bootstrap_t0 <- mean(diff_control_bootstrap_t, na.rm = TRUE)
        diff_control_bootstrap_ci <- quantile(diff_control_bootstrap_t, probs = c(0.05, 0.95), type = 7)
        
        control_report <- glue::glue("Pre-Control Rate: {round(pre_control_bootstrap_t0, 3)}, 95CI: {round(pre_control_bootstrap_ci[1], 3)} — {round(pre_control_bootstrap_ci[2], 3)}
Post-Control Rate: {round(post_control_bootstrap_t0, 3)}, 95CI: {round(post_control_bootstrap_ci[1], 3)} — {round(post_control_bootstrap_ci[2], 3)}
Post-Control Difference: {round(diff_control_bootstrap_t0, 3)}, 95CI: {round(diff_control_bootstrap_ci[1], 3)} — {round(diff_control_bootstrap_ci[2], 3)}")
        
        ## ---- 3b Get table from control data for plotting ----
        pre_control <- data.frame(group = "control", time = "pre", mean_rate = pre_control_bootstrap_t)
        post_control <- data.frame(group = "control", time = "post", mean_rate = post_control_bootstrap_t)
        control_df <- dplyr::bind_rows(pre_control, post_control)
        
        control_ci_df <- data.frame(
          group = "control",
          time = c("pre", "post"),
          lower = c(pre_control_bootstrap_ci[1], post_control_bootstrap_ci[1]),
          upper = c(pre_control_bootstrap_ci[2], post_control_bootstrap_ci[2]),
          mean = pre_control_bootstrap_t0, post_control_bootstrap_t0
        )
        
        # ---- 4. Get intervention data ----
        intervention <- filter_data |> 
          dplyr::select(
            c("Intervention_count", "Intervention_exposure", "Intervention")
          ) |> 
          dplyr::group_by(
            .data[["Intervention"]]
          ) |> 
          dplyr::summarise(
            Count = sum(.data[["Intervention_count"]], na.rm = TRUE),
            Exposure = sum(.data[["Intervention_exposure"]], na.rm = TRUE),
            NoCount = .data[["Exposure"]] - .data[["Count"]]
          )
        
        ## ---- 4a Get statistics from intervention data ----
        
        pre_intervention_vector <- c(
          rep(1, intervention$Count[1]),
          rep(0, intervention$NoCount[1])
        )
        
        post_intervention_vector <- c(
          rep(1, intervention$Count[2]),
          rep(0, intervention$NoCount[2])
        )
        
        pre_intervention_bootstrap_dist <- pre_intervention_vector |> 
          boot::boot(statistic = samplemean, R = n_iterations)
        post_intervention_bootstrap_dist <- post_intervention_vector |> 
          boot::boot(statistic = samplemean, R = n_iterations)
        
        # Distribution
        pre_intervention_bootstrap_t <- pre_intervention_bootstrap_dist$t
        post_intervention_bootstrap_t <- post_intervention_bootstrap_dist$t
        
        # Mean point
        pre_intervention_bootstrap_t0 <- pre_intervention_bootstrap_dist$t0
        post_intervention_bootstrap_t0 <- post_intervention_bootstrap_dist$t0
        
        # 95% CI
        pre_intervention_bootstrap_ci <- pre_intervention_bootstrap_t |> 
          quantile(probs = c(0.05, 0.95), type = 7)
        
        post_intervention_bootstrap_ci <- post_intervention_bootstrap_t |> 
          quantile(probs = c(0.05, 0.95), type = 7)
        
        # Differences
        diff_intervention_bootstrap_t <- post_intervention_bootstrap_t - pre_intervention_bootstrap_t 
        diff_intervention_bootstrap_t0 <- mean(diff_intervention_bootstrap_t, na.rm = TRUE)
        diff_intervention_bootstrap_ci <- quantile(diff_intervention_bootstrap_t, probs = c(0.05, 0.95), type = 7)
        
        ## ---- 4b Get table from intervention data for plotting ----
        pre_intervention <- data.frame(group = "intervention", time = "pre", mean_rate = pre_intervention_bootstrap_t)
        post_intervention <- data.frame(group = "intervention", time = "post", mean_rate = post_intervention_bootstrap_t)
        intervention_df <- dplyr::bind_rows(pre_intervention, post_intervention)
        
        intervention_report <- glue::glue("Pre-Intervention Rate: {round(pre_intervention_bootstrap_t0, 3)}, 95CI: {round(pre_intervention_bootstrap_ci[1], 3)} — {round(pre_intervention_bootstrap_ci[2], 3)}
Post-Intervention Rate: {round(post_intervention_bootstrap_t0, 3)}, 95CI: {round(post_intervention_bootstrap_ci[1], 3)} — {round(post_intervention_bootstrap_ci[2], 3)}
Post-Intervention Difference: {round(diff_intervention_bootstrap_t0, 3)}, 95CI: {round(diff_intervention_bootstrap_ci[1], 3)} — {round(diff_intervention_bootstrap_ci[2], 3)}")
        
        intervention_ci_df <- data.frame(
          group = "intervention",
          time = c("pre", "post"),
          lower = c(pre_intervention_bootstrap_ci[1], post_intervention_bootstrap_ci[1]),
          upper = c(pre_intervention_bootstrap_ci[2], post_intervention_bootstrap_ci[2]),
          mean = c(pre_intervention_bootstrap_t0, post_intervention_bootstrap_t0)
        )
        
        # ---- 5 Difference In Difference calculation
        
        diff_in_diff_bootstrap_t <- diff_intervention_bootstrap_t - diff_control_bootstrap_t
        diff_in_diff_bootstrap_t0 <- mean(diff_in_diff_bootstrap_t, na.rm = TRUE)
        diff_in_diff_bootstrap_ci <- quantile(diff_in_diff_bootstrap_t, probs = c(0.05, 0.95), type = 7)
        
        diff_in_diff_report <- glue::glue("Difference In Difference: {round(diff_in_diff_bootstrap_t0, 3)}, 95CI: {round(diff_in_diff_bootstrap_ci[1], 3)} — {round(diff_in_diff_bootstrap_ci[2], 3)}")
        
        # ---- 6 Combine data for plotting ----
        
        infection_df <- dplyr::bind_rows(control_df, intervention_df) |> 
          dplyr::mutate(
            time = forcats::fct_relevel(
              .data[["time"]],
              c("pre", "post")
            ),
            group = forcats::fct_relevel(
              .data[["group"]],
              c("control", "intervention")
            )
          )
        
        infection_ci_df <- dplyr::bind_rows(control_ci_df, intervention_ci_df) |> 
          dplyr::mutate(
            time = forcats::fct_relevel(
              .data[["time"]],
              c("pre", "post")
            ),
            group = forcats::fct_relevel(
              .data[["group"]],
              c("control", "intervention")
            )
          )
        
        diff_in_diff_df <- data.frame(
          time = "diff",
          mean_rate = diff_in_diff_bootstrap_t
        )
        
        diff_in_diff_ci_df <- data.frame(
          time = "diff",
          lower = c(diff_in_diff_bootstrap_ci[1]),
          upper = c(diff_in_diff_bootstrap_ci[2]),
          mean = c(diff_in_diff_bootstrap_t0)
        )
        
        # ---- 7 Plot Infection Plot (Control vs Intervention)----
        
        infection_plot <- infection_df |>   
          ggplot2::ggplot(
            mapping = ggplot2::aes(
              x = .data[["time"]],
              y = .data[["mean_rate"]],
              fill = .data[["group"]]) 
          ) +
          gghalves::geom_half_violin(
            mapping = ggplot2::aes(
              split = .data[["group"]]
            ),
            position = "identity",
            nudge = 0.1
          ) +
          geom_pointrange(
            data = infection_ci_df,
            mapping = ggplot2::aes(
              x = .data[["time"]], 
              y = .data[["mean"]], 
              ymin = .data[["lower"]], 
              ymax = .data[["upper"]],
              colour = .data[["group"]]
            ),
            size = 0.8,
          ) +
          geom_line(
            data = infection_ci_df,
            mapping = ggplot2::aes(
              x = .data[["time"]], 
              y = .data[["mean"]],       
              group = .data[["group"]],
              colour = .data[["group"]]
              
            )
          ) +
          ggplot2::scale_colour_manual(
            values = c("#FF8886", "#2baeff"),
            guide = ggplot2::guide_legend(
              title = "Ward"
            )
          ) +
          ggplot2::scale_fill_manual(
            values = c("#FFCCCB", "#90D5FF"),
            guide = ggplot2::guide_legend(
              title = "Ward",
              override.aes = list(
                shape = NA, 
                linetype = NA
              )
            )
          ) +
          ggplot2::labs(
            title = "Infection Rate Results",
            caption = glue::glue(
              control_report, "\n\n",
              intervention_report
            ),
            x = "Intervention Period",
            y = "Mean\nInfection\nRate"
          ) +
          ggplot2::theme_minimal() + 
          ggplot2::theme(
            axis.title.y = ggplot2::element_text(angle = 0),
            legend.position = "top",
            plot.caption = ggplot2::element_text(
              hjust = 0,
              size = 10
            )
          )
        
        # ---- 8 Plot Diff in Diff ----
        
        diff_plot <- diff_in_diff_df |> 
          ggplot2::ggplot(
            mapping = ggplot2::aes(
              x = .data[["time"]],
              y = .data[["mean_rate"]]
            ) 
          ) +  
          gghalves::geom_half_violin(
            position = "identity",
            nudge = 0.1,
            fill = "#BEBEBE"
          ) +
          geom_pointrange(
            data = diff_in_diff_ci_df,
            mapping = ggplot2::aes(
              x = .data[["time"]], 
              y = .data[["mean"]], 
              ymin = .data[["lower"]], 
              ymax = .data[["upper"]]
            ),
            size = 0.8,
            colour = "#808080"
          ) +
          ggplot2::geom_hline(
            yintercept = 0, 
            linetype = "solid", 
            color = "#ff38ce"
          ) +
          ggplot2::labs(
            title = "Difference in Difference Results",
            caption = glue::glue(
              diff_in_diff_report
            ),
            y = "Mean\nInfection\nRate"
          ) +
          ggplot2::theme_minimal() + 
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(), 
            axis.ticks.x = ggplot2::element_blank(),   
            axis.title.y = ggplot2::element_text(angle = 0),
            plot.caption = ggplot2::element_text(
              hjust = 0,
              size = 10
            )
          )
        
        # ---- 9 Combine Plots ----
        
        patchwork::wrap_plots(
          infection_plot,
          diff_plot,
          ncol = 1,
          nrow = 2
        )
  })
  #   # Render text output
  #   output$results_text <- renderUI({
  #     req(bootstrap_results())
  #     
  #     results <- bootstrap_results()
  #     
  #     HTML(glue::glue("
  #   <h4><strong>Bootstrap Results:</strong></h4>
  #   <p style='font-size: 110%;'>
  #   <strong>Pre-Intervention Rate:</strong> {round(results$pre_t0, 3)}, 
  #   <strong>95% CI:</strong> {round(results$pre_ci[1], 3)} — {round(results$pre_ci[2], 3)}<br>
  #   <strong>Post-Intervention Rate:</strong> {round(results$post_t0, 3)}, 
  #   <strong>95% CI:</strong> {round(results$post_ci[1], 3)} — {round(results$post_ci[2], 3)}<br>
  #   <strong>Difference:</strong> {round(results$diff_t0, 3)}, 
  #   <strong>95% CI:</strong> {round(results$diff_ci[1], 3)} — {round(results$diff_ci[2], 3)}
  #   </p>
  # "))
  #   })
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
