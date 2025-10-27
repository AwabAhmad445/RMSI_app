library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)

# ---- helper functions ----

compute_area_above_threshold <- function(time_vec, value_vec, threshold) {
  n <- length(time_vec)
  total_area <- 0
  if (n < 2) return(0)
  for (i in 1:(n - 1)) {
    t1 <- time_vec[i]; t2 <- time_vec[i + 1]; dt <- t2 - t1
    x1 <- value_vec[i]; x2 <- value_vec[i + 1]
    if (is.na(x1) || is.na(x2)) next
    if (x1 > threshold && x2 > threshold) {
      total_area <- total_area + dt * ((x1 - threshold + x2 - threshold) / 2)
    } else if (x1 > threshold && x2 <= threshold) {
      t_cross <- t1 + (x1 - threshold) / (x1 - x2) * dt
      area <- (t_cross - t1) * (x1 - threshold) / 2
      total_area <- total_area + area
    } else if (x1 <= threshold && x2 > threshold) {
      t_cross <- t1 + (threshold - x1) / (x2 - x1) * dt
      area <- (t2 - t_cross) * (x2 - threshold) / 2
      total_area <- total_area + area
    }
  }
  total_area
}

compute_time_above_threshold <- function(time_vec, value_vec, threshold) {
  n <- length(time_vec)
  total_time <- 0
  if (n < 2) return(0)
  for (i in 1:(n - 1)) {
    t1 <- time_vec[i]; t2 <- time_vec[i + 1]; dt <- t2 - t1
    x1 <- value_vec[i]; x2 <- value_vec[i + 1]
    if (is.na(x1) || is.na(x2)) next
    if (x1 > threshold && x2 > threshold) {
      total_time <- total_time + dt
    } else if (x1 > threshold && x2 <= threshold) {
      t_cross <- t1 + (x1 - threshold) / (x1 - x2) * dt
      total_time <- total_time + (t_cross - t1)
    } else if (x1 <= threshold && x2 > threshold) {
      t_cross <- t1 + (threshold - x1) / (x2 - x1) * dt
      total_time <- total_time + (t2 - t_cross)
    }
  }
  total_time
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("deltaMSI Calculator for Myocardial Stress (DCD)"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "input_mode", "Select Input Method:",
        choices = c("Upload CSV", "Manual Entry"),
        selected = "Upload CSV"
      ),
      conditionalPanel(
        condition = "input.input_mode == 'Upload CSV'",
        fileInput("file", "Upload CSV File", accept = ".csv"),
        helpText("CSV must include: time, HR, SBP, DBP, SaO2")
      ),
      conditionalPanel(
        condition = "input.input_mode == 'Manual Entry'",
        uiOutput("manual_ui")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", tableOutput("calculated_table")),
        tabPanel("deltaMSI Curve", plotOutput("rmsi_plot")),
        tabPanel("Summary Metrics", verbatimTextOutput("metrics"))
      )
    )
  )
)

# ---- server ----
server <- function(input, output, session) {
  
  manual_data <- reactiveVal(
    data.frame(
      time = numeric(),
      HR   = numeric(),
      SBP  = numeric(),
      DBP  = numeric(),
      SaO2 = numeric()
    )
  )
  
  timer    <- reactiveVal(0)
  declared <- reactiveVal(FALSE)
  
  observeEvent(input$declare, {
    declared(TRUE)
  })
  
  output$manual_ui <- renderUI({
    if (!declared()) {
      tagList(
        numericInput("new_hr",   "HR",   value = 60),
        numericInput("new_sbp",  "SBP",  value = 100),
        numericInput("new_dbp",  "DBP",  value = 60),
        numericInput("new_sao2", "SaO2", value = 100),
        actionButton("add_row",  "Add Minute"),
        actionButton("declare",  "Declare")
      )
    } else {
      tags$p(
        style = "color: green; font-weight: bold;",
        "Withdrawal Phase Complete"
      )
    }
  })
  
  observeEvent(input$add_row, {
    current_time <- timer()
    HR   <- input$new_hr
    SBP  <- input$new_sbp
    DBP  <- input$new_dbp
    SaO2 <- input$new_sao2
    
    # physiology
    RPP <- HR * SBP
    rr_sec <- 60 / HR
    tsyst  <- 0.35 - 0.001 * (HR - 60)
    tdiast <- max(rr_sec - tsyst, 0)
    diastolic_frac <- ifelse(HR > 0, tdiast / rr_sec, 0)
    
    Perfusion <- DBP * (SaO2 / 100) * diastolic_frac
    
    RPP_z  <- ifelse(RPP == 0, 0, (RPP - 8831.1) / 6501.861)
    Perf_z <- ifelse(Perfusion == 0, 0, (Perfusion - 9.94) / 12.36)
    
    MSI <- ifelse(RPP == 0 | Perfusion == 0, NA, RPP_z - Perf_z)
    
    existing_df  <- manual_data()
    baseline_msi <- ifelse(nrow(existing_df) == 0, MSI, existing_df$MSI[1])
    
    deltaMSI <- ifelse(!is.na(MSI), MSI - baseline_msi, NA)
    
    new_row <- data.frame(
      time       = current_time,
      HR         = HR,
      SBP        = SBP,
      DBP        = DBP,
      SaO2       = SaO2,
      RPP        = RPP,
      Perfusion  = Perfusion,
      MSI        = MSI,
      deltaMSI   = deltaMSI
    )
    
    manual_data(rbind(existing_df, new_row))
    timer(current_time + 1)
  })
  
  csv_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  manual_processed_data <- reactive({
    df <- manual_data()
    req(nrow(df) > 0)
    
    df <- df %>%
      mutate(
        RPP   = HR * SBP,
        rr_sec = 60 / HR,
        tsyst  = 0.35 - 0.001 * (HR - 60),
        tdiast = pmax(rr_sec - tsyst, 0),
        diastolic_frac = if_else(HR > 0, tdiast / rr_sec, 0),
        Perfusion = DBP * (SaO2 / 100) * diastolic_frac,
        RPP_z  = if_else(RPP == 0, 0, (RPP - 8831.1) / 6501.861),
        Perf_z = if_else(Perfusion == 0, 0, (Perfusion - 9.94) / 12.36),
        MSI    = if_else(RPP == 0 | Perfusion == 0, NA_real_, RPP_z - Perf_z)
      )
    
    baseline_msi <- df$MSI[1]
    
    df <- df %>%
      mutate(
        deltaMSI = if_else(!is.na(MSI), MSI - baseline_msi, NA_real_)
      ) %>%
      select(
        time, HR, SBP, DBP, SaO2,
        RPP, Perfusion, MSI, deltaMSI
      )
    
    df
  })
  
  processed_data <- reactive({
    if (input$input_mode == "Upload CSV") {
      df <- csv_data()
      req(nrow(df) > 1)
      
      df <- df %>%
        mutate(
          RPP   = HR * SBP,
          rr_sec = 60 / HR,
          tsyst  = 0.35 - 0.001 * (HR - 60),
          tdiast = pmax(rr_sec - tsyst, 0),
          diastolic_frac = if_else(HR > 0, tdiast / rr_sec, 0),
          Perfusion = DBP * (SaO2 / 100) * diastolic_frac,
          RPP_z  = if_else(RPP == 0, 0, (RPP - 8831.1) / 6501.861),
          Perf_z = if_else(Perfusion == 0, 0, (Perfusion - 9.94) / 12.36),
          MSI    = if_else(RPP == 0 | Perfusion == 0, NA_real_, RPP_z - Perf_z)
        )
      
      baseline_msi <- df$MSI[1]
      
      df <- df %>%
        mutate(
          deltaMSI = if_else(!is.na(MSI), MSI - baseline_msi, NA_real_)
        ) %>%
        select(
          time, HR, SBP, DBP, SaO2,
          RPP, Perfusion, MSI, deltaMSI
        )
      
      df
    } else {
      manual_processed_data()
    }
  })
  
  # ---- outputs ----
  
  output$calculated_table <- renderTable({
    processed_data()
  })
  
  output$rmsi_plot <- renderPlot({
    df <- processed_data() %>%
      arrange(time) %>%
      mutate(
        deltaMSI_interp = zoo::na.approx(
          deltaMSI,
          x = time,
          na.rm = FALSE,
          rule = 2
        )
      )
    
    ggplot(df, aes(x = time, y = deltaMSI_interp)) +
      geom_line() +
      geom_hline(
        yintercept = c(0.5, 1.0),
        linetype   = "dashed"
      ) +
      labs(
        title = "deltaMSI Over Time",
        x = "Time (minutes)",
        y = "deltaMSI (MSI - baseline)"
      ) +
      theme_minimal()
  })
  
  output$metrics <- renderPrint({
    df <- processed_data()
    
    df_filtered <- df %>%
      filter(
        !is.na(deltaMSI),
        is.finite(deltaMSI),
        !is.na(time)
      )
    
    avg_delta <- mean(df_filtered$deltaMSI, na.rm = TRUE)
    
    auc_delta <- sum(
      diff(df_filtered$time) *
        zoo::rollmean(df_filtered$deltaMSI, 2, fill = NA),
      na.rm = TRUE
    )
    
    time_above_0_5 <- compute_time_above_threshold(
      df_filtered$time,
      df_filtered$deltaMSI,
      0.5
    )
    
    area_above_0_5 <- compute_area_above_threshold(
      df_filtered$time,
      df_filtered$deltaMSI,
      0.5
    )
    
    time_above_1_0 <- compute_time_above_threshold(
      df_filtered$time,
      df_filtered$deltaMSI,
      1.0
    )
    
    area_above_1_0 <- compute_area_above_threshold(
      df_filtered$time,
      df_filtered$deltaMSI,
      1.0
    )
    
    slope <- if (nrow(df_filtered[df_filtered$time <= 10, ]) >= 2) {
      coef(lm(deltaMSI ~ time,
              data = df_filtered[df_filtered$time <= 10, ]))[2]
    } else {
      NA
    }
    
    withdrawal_to_declaration <- max(df$time, na.rm = TRUE)
    
    cat("Average deltaMSI:", round(avg_delta, 3), "\n")
    cat("Cumulative AUC (approx):", round(auc_delta, 3), "\n")
    cat("Time above deltaMSI = 0.5:", round(time_above_0_5, 2), "min\n")
    cat("Area above deltaMSI = 0.5:", round(area_above_0_5, 3), "\n")
    cat("Time above deltaMSI = 1.0:", round(time_above_1_0, 2), "min\n")
    cat("Area above deltaMSI = 1.0:", round(area_above_1_0, 3), "\n")
    cat("Slope in first 10 min:", round(slope, 4), "\n")
    cat("Withdrawal to Declaration Time:",
        withdrawal_to_declaration,
        "min\n")
  })
}

shinyApp(ui = ui, server = server)
