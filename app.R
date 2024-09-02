library(shiny)
library(shinydashboard)
library(TrialEmulation)
library(DT)
library(data.table)
library(shinyjs)

ui <- dashboardPage(
        dashboardHeader(title = "Trial𓅦"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Data", tabName = "data", icon = icon("database")),
                        menuItem("Data Preparation", tabName = "prep", icon = icon("cogs")),
                        menuItem("Model Fitting", tabName = "model", icon = icon("chart-line")),
                        menuItem("Plot", tabName = "results", icon = icon("bar-chart")),
                        menuItem("About", tabName = "about", icon = icon("info-circle"))
                )
        ),
        dashboardBody(
                useShinyjs(),
                tabItems(
                        # Data tab
                        tabItem(tabName = "data",
                                fluidRow(
                                        box(
                                                width = 12,
                                                title = "Data Source",
                                                radioButtons("data_source", "Choose data source:",
                                                             choices = c("Use trial_example data" = "example",
                                                                         "Generate random dataset" = "random")),
                                                conditionalPanel(
                                                        condition = "input.data_source == 'random'",
                                                        numericInput("n_patients", "Number of Patients", value = 1000, min = 100),
                                                        numericInput("n_periods", "Number of Periods per Patient", value = 20, min = 1)
                                                ),
                                                actionButton("load_data", "Load Data"),
                                                actionButton("save_data", "Save Data")
                                        )
                                ),
                                fluidRow(
                                        box(
                                                width = 12,
                                                title = "Data Preview",
                                                DTOutput("data_preview")
                                        )
                                )
                        ),
                        
                        # Data Preparation tab
                        tabItem(tabName = "prep",
                                fluidRow(
                                        box(
                                                width = 12,
                                                title = "Data Preparation Settings",
                                                selectInput("id", "ID Column", ""),
                                                selectInput("period", "Period Column", ""),
                                                selectInput("treatment", "Treatment Column", ""),
                                                selectInput("outcome", "Outcome Column", ""),
                                                selectInput("eligible", "Eligible Column", ""),
                                                selectInput("outcome_cov", "Outcome Covariates", 
                                                            choices = NULL,
                                                            multiple = TRUE),
                                                selectInput("switch_n_cov", "Switch Covariates", 
                                                            choices = NULL,
                                                            multiple = TRUE),
                                                numericInput("first_period", "First Period", value = 1),
                                                numericInput("last_period", "Last Period", value = 20),
                                                selectInput("estimand_type", "Estimand Type", 
                                                            choices = c("ITT", "PP", "As-Treated"),
                                                            selected = "ITT"),
                                                checkboxInput("use_censor_weights", "Use Censor Weights", value = FALSE),
                                                actionButton("prep_data", "Prepare Data", class = "btn-primary")
                                        )
                                ),
                                fluidRow(
                                        box(
                                                width = 12,
                                                title = "Preparation Summary",
                                                verbatimTextOutput("prep_summary")
                                        )
                                )
                        ),
                        
                        # Model Fitting tab
                        tabItem(tabName = "model",
                                fluidRow(
                                        box(
                                                width = 12,
                                                title = "Model Fitting Settings",
                                                selectInput("model_var", "Model Variable", 
                                                            choices = c("assigned_treatment", "dose"),
                                                            selected = "assigned_treatment"),
                                                actionButton("fit_model", "Fit Model", class = "btn-primary")
                                        )
                                ),
                                fluidRow(
                                        box(
                                                width = 12,
                                                title = "Model Summary",
                                                verbatimTextOutput("model_summary")
                                        )
                                )
                        ),
                        
                        # Results tab
                        tabItem(tabName = "results",
                                fluidRow(
                                        box(
                                                width = 12,
                                                title = "Cumulative Incidence Plot",
                                                plotOutput("cumulative_incidence_plot")
                                        )
                                )
                        ),
                        # About tab
                        tabItem(tabName = "about",
                                fluidRow(
                                        box(
                                                title = "About the Creator",
                                                width = 12,
                                                p("Creator: Ahmad Sofi-Mahmudi"),
                                                br(),
                                                p("Social Media:"),
                                                tags$div(
                                                        tags$a("Blog", href = "https://choxos.com", target = "_blank"),
                                                        tags$span(" | "),
                                                        tags$a("GitHub", href = "https://github.com/choxos", target = "_blank"),
                                                        tags$span(" | "),
                                                        tags$a("Twitter", href = "https://twitter.com/ASofiMahmudi", target = "_blank"),
                                                        tags$span(" | "),
                                                        tags$a("LinkedIn", href = "https://www.linkedin.com/in/asofimahmudi/", target = "_blank")
                                                )
                                        )
                                )
                        )
                )
        )
)

server <- function(input, output, session) {
        
        # Reactive values
        rv <- reactiveValues(
                data = NULL,
                prep_data = NULL,
                model_result = NULL
        )
        
        # Load Data
        observeEvent(input$load_data, {
                if (input$data_source == "example") {
                        rv$data <- TrialEmulation::trial_example
                } else {
                        # Generate random data based on trial_example characteristics
                        n_patients <- input$n_patients
                        n_periods <- input$n_periods
                        
                        rv$data <- data.frame(
                                id = rep(1:n_patients, each = n_periods),
                                eligible = sample(0:1, n_patients * n_periods, replace = TRUE),
                                period = rep(1:n_periods, times = n_patients),
                                outcome = sample(0:1, n_patients * n_periods, replace = TRUE, prob = c(0.9, 0.1)),
                                treatment = sample(0:1, n_patients * n_periods, replace = TRUE),
                                catvarA = factor(sample(0:4, n_patients * n_periods, replace = TRUE)),
                                catvarB = factor(sample(0:4, n_patients * n_periods, replace = TRUE)),
                                catvarC = sample(0:1, n_patients * n_periods, replace = TRUE),
                                nvarA = sample(0:10, n_patients * n_periods, replace = TRUE),
                                nvarB = sample(30:60, n_patients * n_periods, replace = TRUE),
                                nvarC = sample(70:80, n_patients * n_periods, replace = TRUE)
                        )
                }
                
                # Update column choices
                updateSelectInput(session, "id", choices = names(rv$data), selected = "id")
                updateSelectInput(session, "period", choices = names(rv$data), selected = "period")
                updateSelectInput(session, "treatment", choices = names(rv$data), selected = "treatment")
                updateSelectInput(session, "outcome", choices = names(rv$data), selected = "outcome")
                updateSelectInput(session, "eligible", choices = names(rv$data), selected = "eligible")
                updateSelectInput(session, "outcome_cov", choices = names(rv$data), 
                                  selected = intersect(c("catvarA", "catvarB", "nvarA", "nvarB", "nvarC"), names(rv$data)))
                updateSelectInput(session, "switch_n_cov", choices = names(rv$data), 
                                  selected = intersect(c("nvarA", "nvarB"), names(rv$data)))
                
                # Update period range
                updateNumericInput(session, "first_period", value = min(rv$data$period))
                updateNumericInput(session, "last_period", value = max(rv$data$period))
                
                # Show data preview
                output$data_preview <- renderDT({
                        datatable(head(rv$data, 100), options = list(scrollX = TRUE, scrollY = "300px"))
                })
                
                showNotification("Data loaded successfully", type = "message")
        })
        
        # Save Data
        observeEvent(input$save_data, {
                if (!is.null(rv$data)) {
                        saveRDS(rv$data, "saved_data.rds")
                        showNotification("Data saved successfully", type = "message")
                } else {
                        showNotification("No data to save. Please load data first.", type = "warning")
                }
        })
        
        # Data Preparation
        observeEvent(input$prep_data, {
                req(rv$data)
                
                rv$working_dir = file.path(tempdir(TRUE), "trial_emu")
                if (dir.exists(rv$working_dir)) {
                        unlink(rv$working_dir, recursive = TRUE)
                }
                dir.create(rv$working_dir)
                
                withProgress(message = 'Preparing data', value = 0, {
                        tryCatch({
                                rv$prep_data = data_preparation(
                                        data = rv$data,
                                        id = input$id,
                                        period = input$period,
                                        eligible = input$eligible,
                                        treatment = input$treatment,
                                        outcome = input$outcome,
                                        outcome_cov = as.formula(paste("~ ", paste(input$outcome_cov, collapse = " + "))),
                                        data_dir = rv$working_dir,
                                        save_weight_models = TRUE,
                                        estimand_type = input$estimand_type,
                                        pool_cense = "none",
                                        use_censor_weights = input$use_censor_weights,
                                        chunk_size = 500,
                                        separate_files = TRUE,
                                        switch_n_cov = as.formula(paste("~ ", paste(input$switch_n_cov, collapse = " + "))),
                                        quiet = TRUE
                                )
                                
                                incProgress(1)
                                showNotification("Data preparation complete", type = "message")
                        }, error = function(e) {
                                rv$prep_data <- NULL
                                showNotification(paste("Error in data preparation:", e$message), type = "error")
                        })
                })
                
                # Update preparation summary
                output$prep_summary <- renderPrint({
                        req(rv$prep_data, rv$working_dir)
                        cat("Model Preparation Summary:\n")
                        cat("----------------------\n")
                        summary(rv$prep_data)
                        cat("Weighting Models:\n")
                        cat("----------------------\n")
                        print(as.data.frame(rv$prep_data$switch_models$switch_n0$summary))
                        cat("----------------------\n")
                        cat("Weighting Models Fitting Information:\n")
                        cat("----------------------\n")
                        print(as.data.frame(rv$prep_data$switch_models$switch_n0$fit_summary))
                        
                        # list.files(rv$working_dir, "*.rds")
                        #switch_n0 = readRDS(prep_data$switch_models$switch_n0$path)
                        #summary(switch_n0)
                        
                })
        })
        
        # Model Fitting
        observeEvent(input$fit_model, {
                req(rv$prep_data)
                
                withProgress(message = 'Fitting model', value = 0, {
                        tryCatch({
                                # Sample data for model fitting
                                sampled_data <- case_control_sampling_trials(rv$prep_data, p_control = 0.1)
                                incProgress(0.3)
                                
                                rv$model_result <- trial_msm(
                                        data = sampled_data,
                                        outcome_cov = input$outcome_cov,
                                        model_var = input$model_var,
                                        use_sample_weights = TRUE
                                )
                                incProgress(0.7)
                                showNotification("Model fitting complete", type = "message")
                        }, error = function(e) {
                                rv$model_result <- NULL
                                showNotification(paste("Error in model fitting:", e$message), type = "error")
                        })
                })
                
                # Update model summary
                output$model_summary <- renderPrint({
                        req(rv$model_result)
                        cat("Model Fitting Summary:\n")
                        cat("----------------------\n")
                        summary(rv$model_result$robust)
                })
        })
        
        # Prediction Summary and Plot
        output$cumulative_incidence_plot <- renderPlot({
                req(rv$model_result, rv$prep_data, rv$working_dir)
                
                tryCatch({
                        # Load the data
                        new_data <- data.table::fread(file.path(rv$working_dir, "trial_1.csv"))
                        
                        # Add the data template
                        new_data <- rbind(data.table::as.data.table(rv$prep_data$data_template), new_data)
                        
                        # Make predictions
                        model_preds <- predict(rv$model_result, predict_times = 0:40, newdata = new_data, type = "cum_inc")
                        
                        # Plot
                        plot(
                                model_preds$difference$followup_time,
                                model_preds$difference$cum_inc_diff,
                                type = "l",
                                ylab = "Cumulative Incidence Difference",
                                xlab = "Follow-up Time",
                                ylim = c(-0.15, 0.05),
                                main = "Cumulative Incidence Difference Over Time"
                        )
                        lines(model_preds$difference$followup_time, model_preds$difference$`2.5%`, lty = 2, col = "blue")
                        lines(model_preds$difference$followup_time, model_preds$difference$`97.5%`, lty = 2, col = "blue")
                        abline(h = 0, lty = 3, col = "red")
                        legend("bottomleft", legend = c("Difference", "95% CI"), lty = c(1, 2), col = c("black", "blue"))
                }, error = function(e) {
                        # If an error occurs, print the error message on the plot
                        plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Error in Plotting")
                        text(0, 0, paste("Error:", e$message), cex = 1.2, col = "red")
                })
        })
        
}

shinyApp(ui, server)