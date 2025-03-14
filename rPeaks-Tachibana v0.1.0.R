app_name = "rPeaks-Tachibana v0.1.0"
#20250314
#Hongrong Yin

library(shiny)
library(minpack.lm)
library(DT)
library(dplyr)
library(shinyBS)
library(ggplot2)
library(plotly)
library(signal)
library(pracma)

#############################
# UI
#############################

ui <- fluidPage(
  titlePanel("Peak Analysis and Fitting"),
  sidebarLayout(
    sidebarPanel(
      h4("Data file"),
      fileInput("file", "Upload Original CSV file", accept = ".csv"),
      uiOutput("choose_columns"),
      hr(),
      
      h4("Blank Correction"),
      fileInput("blank_file", "Upload Blank CSV file (optional)", accept = ".csv"),
      uiOutput("choose_blank_columns"),
      numericInput("A_factor", "a (for original)", value = 1, step = 0.1),
      selectInput("op", "Operation", choices = c("-", "+", "*", "/")),
      numericInput("B_factor", "b (for blank)", value = 1, step = 0.1),
      actionButton("apply_blank", "Apply Blank Correction"),
      helpText("Corrected data: Y = a*Yo (op) b*Yb."),
      helpText("Example: If a=1, op='-', b=1, then Y = Yo - Yb."),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 bsCollapsePanel("Baseline & Noise Settings",
                                 h4("Fitting Range"),
                                 fluidRow(
                                   column(width = 5, numericInput("fit_xmin", "x min", value = NA)),
                                   column(width = 5, numericInput("fit_xmax", "x max", value = NA))
                                 ),
                                 fluidRow(
                                   column(width = 5,
                                          selectInput(
                                            "baseline_type", 
                                            "Baseline Type", 
                                            choices = c("constant", "linear", "quadratic", "cubic", "manual")
                                          )
                                   ),
                                   column(width = 5, uiOutput("baseline_params_ui"))
                                 ),
                                 numericInput("noise_level", "Approx. Noise Level (for info)", value = 0.1, step = 0.1),
                                 hr(),
                                 conditionalPanel(
                                   condition = "input.baseline_type == 'manual'",
                                   checkboxInput("manual_baseline_pick", "Enable Manual Baseline Picking", FALSE),
                                   helpText("Check to start picking baseline points and uncheck to stop."),
                                   DTOutput("baseline_points_table"),
                                   fluidRow(
                                     column(6, actionButton("clear_baseline_points", "Clear Baseline Points")),
                                     column(6, actionButton("remove_baseline_point", "Remove Selected Point"))
                                   )
                                 ),
                                 collapsed = FALSE
                 ),
                 
                 bsCollapsePanel("Peak Parameter Limits",
                                 h4("Set Limits (Optional)"),
                                 helpText("Define min and max limits for peak center, height, width(HWHM). Blank => no limits."),
                                 fluidRow(
                                   column(width = 6, numericInput("min_peak_center", "Min Peak Center", value = NA, step = 0.1)),
                                   column(width = 6, numericInput("max_peak_center", "Max Peak Center", value = NA, step = 0.1))
                                 ),
                                 fluidRow(
                                   column(width = 6, numericInput("min_peak_height", "Min Peak Height", value = NA, step = 0.1)),
                                   column(width = 6, numericInput("max_peak_height", "Max Peak Height", value = NA, step = 0.1))
                                 ),
                                 fluidRow(
                                   column(width = 6, numericInput("min_peak_width", "Min Peak Width (HWHM)", value = NA, step = 0.1)),
                                   column(width = 6, numericInput("max_peak_width", "Max Peak Width (HWHM)", value = NA, step = 0.1))
                                 ),
                                 helpText("Ensure min < max."),
                                 collapsed = TRUE
                 ),
                 
                 bsCollapsePanel("Peak Model Settings",
                                 style = "info",
                                 fluidRow(
                                   column(width = 5,
                                          selectInput("peak_shape", "Peak Shape",
                                                      choices = c("gauss", "lorentz", "voigt", "mixed", "lognormal"),
                                                      selected = "gauss")
                                   ),
                                   column(width = 5,
                                          selectInput("peak_sign", "Peak Sign:",
                                                      choices = c("positive", "negative", "both"),
                                                      selected = "positive")
                                   )
                                 ),
                                 hr(),
                                 radioButtons("mode_select", "Choose Peak Picking Mode:",
                                              choices = c("Manual" = "manual", "Automatic" = "auto"),
                                              selected = "manual"),
                                 conditionalPanel(
                                   condition = "input.mode_select == 'auto'",
                                   h4("Automatic Mode Settings"),
                                   hr(),
                                   h4("Peak Picking Sensitivity"),
                                   selectInput("sensitivity_level", "Sensitivity Level",
                                               choices = c("Low (Second Derivative)" = "low",
                                                           "Mid (Third Derivative)" = "mid",
                                                           "High (Fourth Derivative)" = "high"),
                                               selected = "low"),
                                   hr(),
                                   h4("Savitzky-Golay Parameters"),
                                   fluidRow(
                                     column(width = 5,
                                            numericInput("sg_window", "SG Window Size (odd #)", value = 11, min = 5, step = 2)
                                     ),
                                     column(width = 5,
                                            numericInput("sg_polyorder", "SG Polynomial Order", value = 3, min = 1, step = 1)
                                     )
                                   ),
                                   helpText("Window Size > Polynomial Order, and must be odd."),
                                   numericInput("num_peaks", "Number of Peaks:", value = 1, min = 1),
                                   actionButton("set_peaks", "Auto Pick Peaks"),
                                   actionButton("add_peak", "Add Peak (Auto)")
                                 ),
                                 conditionalPanel(
                                   condition = "input.mode_select == 'manual'",
                                   hr(),
                                   h4("Manual Mode Settings"),
                                   checkboxInput("manual_pick_mode", "Enable Manual Peak Picking", value = FALSE),
                                   helpText("Check to pick peaks manually; click the plot to add peaks.")
                                 ),
                                 hr(),
                                 actionButton("fit_button", "Fit Peaks", icon = icon("play"), class = "btn-primary"),
                                 hr(),
                                 DTOutput("peak_table"),
                                 actionButton("remove_peak", "Remove Selected Peak"),
                                 collapsed = FALSE
                 ),
                 hr(),
                 checkboxInput("show_individual_peaks", "Show Individual Peaks", TRUE),
                 plotlyOutput("plot_spectrum", height = "500px"),
                 plotlyOutput("plot_residuals"),
                 plotlyOutput("plot_derivative")
        ),
        tabPanel("Fit Summary & Download",
                 h4("Download results"),
                 downloadButton("download_combined_csv", "Fitted Data (CSV)"),
                 downloadButton("download_fit_summary", "Fit Summary (TXT)"),
                 hr(),
                 uiOutput("fit_summary")
        ),
        tabPanel("Instructions",
                 h3("Instructions"),
                 tags$ol(
                   tags$li(
                     strong("Load Data"),
                     tags$ul(
                       tags$li("Go to the “Data file” section and upload your original CSV."),
                       tags$li("Select the columns for X and Y data in the dropdowns.")
                     )
                   ),
                   tags$li(
                     strong("(Optional) Blank Correction"),
                     tags$ul(
                       tags$li("Load your blank CSV file (if any)."),
                       tags$li("Choose the correct columns for blank X and Y."),
                       tags$li("Adjust the numeric inputs (a, b) and the operation."),
                       tags$li("Click “Apply Blank Correction” to combine/subtract.")
                     )
                   ),
                   tags$li(
                     strong("Set Fitting Range and Baseline"),
                     tags$ul(
                       tags$li("Under “Baseline & Noise Settings,” specify x min and x max to limit the fitting region."),
                       tags$li("Select a baseline type (constant, linear, quadratic, cubic, or manual).")
                     )
                   ),
                   tags$li(
                     strong("Manual Baseline (if baseline_type = 'manual')"),
                     tags$ul(
                       tags$li("Check “Enable Manual Baseline Picking.”"),
                       tags$li("Click on the main plot to add baseline points at desired X–Y locations."),
                       tags$li("Points appear in the table below."),
                       tags$li("Use “Remove Selected Point” or “Clear Baseline Points” as needed."),
                       tags$li("The app linearly interpolates these points to form the baseline.")
                     )
                   ),
                   tags$li(
                     strong("Set Peak Parameter Limits (Optional)"),
                     tags$ul(
                       tags$li("Define min/max limits for peak center, height, and width (HWHM)."),
                       tags$li("Leave these blank if no constraint is desired.")
                     )
                   ),
                   tags$li(
                     strong("Select Peak Shape and Sign"),
                     tags$ul(
                       tags$li("Choose from “gauss,” “lorentz,” “voigt,” “mixed,” or “lognormal.”"),
                       tags$li("Pick “positive,” “negative,” or “both” for peak sign.")
                     )
                   ),
                   tags$li(
                     strong("Pick Peaks"),
                     tags$ul(
                       tags$li(
                         "Automatic mode:",
                         tags$ul(
                           tags$li("Select sensitivity (Low, Mid, or High)."),
                           tags$li("Tune Savitzky-Golay window/polynomial if necessary."),
                           tags$li("Enter “Number of Peaks,” then click “Auto Pick Peaks.”"),
                           tags$li("Click “Add Peak (Auto)” to add more peaks from detection.")
                         )
                       ),
                       tags$li(
                         "Manual mode:",
                         tags$ul(
                           tags$li("Enable “Manual Peak Picking.”"),
                           tags$li("Click on the main plot to place peaks at chosen X–Y points.")
                         )
                       )
                     )
                   ),
                   tags$li(
                     strong("Inspect and Edit Picked Peaks"),
                     tags$ul(
                       tags$li("Review the table of centers, heights, widths, etc."),
                       tags$li("Edit parameters in the table cells if needed."),
                       tags$li("Remove any unwanted peaks by selecting their rows and clicking “Remove Selected Peak.”")
                     )
                   ),
                   tags$li(
                     strong("Fit the Peaks"),
                     tags$ul(
                       tags$li("Click “Fit Peaks” to fit the chosen model (plus baseline) to your data."),
                       tags$li("A red line shows the fitted curve; dashed blue lines show each peak if “Show Individual Peaks” is checked.")
                     )
                   ),
                   tags$li(
                     strong("Visualize Residuals and Derivative"),
                     tags$ul(
                       tags$li("Residuals plot: difference between raw data and fitted curve."),
                       tags$li("Derivative plot: derivative of the data (helpful in auto picking).")
                     )
                   ),
                   tags$li(
                     strong("Review Fit Summary & Download"),
                     tags$ul(
                       tags$li("“Fit Summary & Download” tab shows final baseline and peak parameters, areas, etc."),
                       tags$li("If manual baseline was chosen, your picked points are listed."),
                       tags$li("Download a single CSV (“Fitted Data”) with X, Original, Baseline, Fitted, Residual, and each Peak."),
                       tags$li("Download a TXT (“Fit Summary”) for a textual summary.")
                     )
                   )
                 )
        )
      )
    )
  )
)


#############################
# SERVER
#############################

server <- function(input, output, session) {
  
    # Reactive Values
    rv <- reactiveValues(
    data = NULL,
    full_data = NULL,
    blank_data = NULL,
    peaks = data.frame(center = numeric(0), height = numeric(0), width = numeric(0),
                       extra = numeric(0), stringsAsFactors = FALSE),
    fit_result = NULL,
    fit_error = FALSE,
    # Baseline points for manual picking
    baseline_points = data.frame(X = numeric(0), Y = numeric(0))
  )
  
  # Baseline Function
  baseline_fun <- function(x, bltype, blparams, baseline_points) {
    if (bltype == "manual") {
      if (nrow(baseline_points) == 0) {
        # no user points => baseline = 0
        return(rep(0, length(x)))
      } else if (nrow(baseline_points) == 1) {
        # one point => constant baseline at that Y
        return(rep(baseline_points$Y[1], length(x)))
      } else {
        # >=2 points => piecewise linear interpolation
        bp <- baseline_points[order(baseline_points$X), ]
        out <- approx(x = bp$X, y = bp$Y,
                      xout = x,
                      method = "linear",
                      rule = 2)  # rule=2 => extends as constant beyond min/max
        return(out$y)
      }
    }
    
    # else for numeric baseline types
    if (bltype == "constant") {
      return(rep(blparams[1], length(x)))
    } else if (bltype == "linear") {
      return(blparams[1] + blparams[2] * x)
    } else if (bltype == "quadratic") {
      return(blparams[1] + blparams[2] * x + blparams[3] * x^2)
    } else if (bltype == "cubic") {
      return(blparams[1] + blparams[2] * x + blparams[3] * x^2 + blparams[4] * x^3)
    } else {
      return(rep(0, length(x)))
    }
  }
  
  # Peak Shape Helpers
  hwhm_to_sigma_gauss <- function(hwhm) {
    hwhm / sqrt(2 * log(2))
  }
  
  hwhm_to_sigma_lognormal <- function(hwhm, center) {
    (1 / (sqrt(2 * log(2)))) * log(1 + hwhm / center)
  }
  
  gauss_peak <- function(x, center, height, width_hwhm) {
    sigma <- hwhm_to_sigma_gauss(width_hwhm)
    height * exp(-0.5 * ((x - center) / sigma)^2)
  }
  
  lorentz_peak <- function(x, center, height, width_hwhm) {
    height * (width_hwhm^2 / ((x - center)^2 + width_hwhm^2))
  }
  
  voigt_peak <- function(x, center, height, width_hwhm, alpha = 0.5) {
    sigma_gauss <- hwhm_to_sigma_gauss(width_hwhm)
    gauss_part <- height * exp(-0.5 * ((x - center) / sigma_gauss)^2)
    lorentz_part <- height * (width_hwhm^2 / ((x - center)^2 + width_hwhm^2))
    alpha * gauss_part + (1 - alpha) * lorentz_part
  }
  
  mixed_peak <- function(x, center, height, width_hwhm, gauss_frac = 0.5) {
    sigma_gauss <- hwhm_to_sigma_gauss(width_hwhm)
    gauss_part <- height * exp(-0.5 * ((x - center) / sigma_gauss)^2)
    lorentz_part <- height * (width_hwhm^2 / ((x - center)^2 + width_hwhm^2))
    gauss_frac * gauss_part + (1 - gauss_frac) * lorentz_part
  }
  
  lognormal_peak <- function(x, center, height, width_hwhm) {
    sigma_log <- hwhm_to_sigma_lognormal(width_hwhm, center)
    ifelse(x > 0, 
           height * exp(-((log(x) - log(center))^2) / (2 * sigma_log^2)), 
           0)
  }

  # Composite Model
  composite_model <- function(params, x, peak_func, baseline_type, nparams_baseline, baseline_points) {
    # baseline params
    blparams <- params[1:nparams_baseline]
    # get baseline
    baseline <- baseline_fun(x, baseline_type, blparams, baseline_points)
    
    # peak params
    np_per_peak <- if (peak_func %in% c("voigt", "mixed")) 4 else 3
    peak_params <- params[(nparams_baseline + 1):length(params)]
    n_peaks <- length(peak_params) / np_per_peak
    
    fit_y <- baseline
    idx_start <- 1
    for (i in seq_len(n_peaks)) {
      idx <- ((i - 1)*np_per_peak + 1):((i - 1)*np_per_peak + np_per_peak)
      pvals <- peak_params[idx]
      center <- pvals[1]
      height <- pvals[2]
      width_hwhm <- pvals[3]
      
      if (peak_func == "gauss") {
        fit_y <- fit_y + gauss_peak(x, center, height, width_hwhm)
      } else if (peak_func == "lorentz") {
        fit_y <- fit_y + lorentz_peak(x, center, height, width_hwhm)
      } else if (peak_func == "voigt") {
        alpha <- pvals[4]
        fit_y <- fit_y + voigt_peak(x, center, height, width_hwhm, alpha)
      } else if (peak_func == "mixed") {
        gauss_frac <- pvals[4]
        fit_y <- fit_y + mixed_peak(x, center, height, width_hwhm, gauss_frac)
      } else if (peak_func == "lognormal") {
        fit_y <- fit_y + lognormal_peak(x, center, height, width_hwhm)
      }
    }
    
    return(fit_y)
  }
  
  # Residual Function
  # This passes the baseline_points to composite_model
  resid_fun <- function(p, x, y, peak_func, baseline_type, nparams_bl, baseline_points) {
    fit <- composite_model(p, x, peak_func, baseline_type, nparams_bl, baseline_points)
    return(y - fit)
  }
  
  # Other Helpers
  trapz <- function(x, y) {
    n <- length(x)
    if (n < 2) return(0)
    sum((x[2:n] - x[1:(n - 1)]) * (y[1:(n - 1)] + y[2:n])) / 2
  }
  
  apply_numeric_derivative <- function(x, y) {
    dx <- mean(diff(x))
    y_deriv <- diff(y) / dx
    x_deriv <- x[-1]
    list(x = x_deriv, y = y_deriv)
  }
  
  compute_derivative <- function(x, y, order) {
    d_x <- x
    d_y <- y
    for (i in seq_len(order)) {
      res <- apply_numeric_derivative(d_x, d_y)
      d_x <- res$x
      d_y <- res$y
    }
    data.frame(X = d_x, Y = d_y)
  }
  
  adjust_peak_sign <- function(height) {
    if (input$peak_sign=="positive" && height<0) {
      return(abs(height))
    } else if (input$peak_sign=="negative" && height>0) {
      return(-abs(height))
    }
    return(height)
  }
  
  # Observers & Reactives
  observeEvent(input$file, {
    req(input$file)
    rv$full_data <- tryCatch({
      df <- read.csv(input$file$datapath)
      if (ncol(df) < 2) stop("The CSV file must have at least two columns.")
      df
    }, error = function(e) {
      showNotification(paste("Error reading original file:", e$message), type = "error")
      NULL
    })
    rv$data <- rv$full_data
  })
  
  observeEvent(input$blank_file, {
    rv$blank_data <- tryCatch({
      df <- read.csv(input$blank_file$datapath)
      if (ncol(df) < 2) stop("The blank CSV file must have at least two columns.")
      df
    }, error = function(e) {
      showNotification(paste("Error reading blank file:", e$message), type = "error")
      NULL
    })
  })
  
  output$choose_columns <- renderUI({
    req(rv$full_data)
    cols <- names(rv$full_data)
    tagList(
      selectInput("xcol", "Original X Column", choices = cols, selected = cols[1]),
      selectInput("ycol", "Original Y Column", choices = cols, selected = cols[2])
    )
  })
  
  output$choose_blank_columns <- renderUI({
    req(rv$blank_data)
    cols <- names(rv$blank_data)
    tagList(
      selectInput("bxcol", "Blank X Column", choices = cols, selected = cols[1]),
      selectInput("bycol", "Blank Y Column", choices = cols, selected = cols[2])
    )
  })

  observeEvent(input$apply_blank, {
    if (is.null(rv$full_data)) {
      showNotification("No original data loaded yet.", type = "error")
      return()
    }
    if (is.null(rv$blank_data)) {
      showNotification("No blank data provided. Using original data as is.", type = "message")
      rv$data <- rv$full_data
      return()
    }
    
    req(input$xcol, input$ycol, input$bxcol, input$bycol)
    if (!(input$xcol %in% names(rv$full_data)) || !(input$ycol %in% names(rv$full_data))) {
      showNotification("Selected columns for original data not found.", type = "error")
      return()
    }
    if (!(input$bxcol %in% names(rv$blank_data)) || !(input$bycol %in% names(rv$blank_data))) {
      showNotification("Selected columns for blank data not found.", type = "error")
      return()
    }
    
    x_o <- rv$full_data[[input$xcol]]
    y_o <- rv$full_data[[input$ycol]]
    x_b <- rv$blank_data[[input$bxcol]]
    y_b <- rv$blank_data[[input$bycol]]
    
    if (length(x_o) != length(x_b)) {
      showNotification("Original and blank data lengths differ.", type = "error")
      return()
    }
    if (!all(x_o == x_b)) {
      showNotification("X values differ. Align before correction.", type = "error")
      return()
    }
    
    A <- input$A_factor
    B <- input$B_factor
    op <- input$op
    
    y_new <- switch(op,
                    "+" = A * y_o + B * y_b,
                    "-" = A * y_o - B * y_b,
                    "*" = A * y_o * (B * y_b),
                    "/" = {
                      if (any(B * y_b == 0)) {
                        showNotification("Division by zero.", type = "error")
                        return()
                      }
                      A * y_o / (B * y_b)
                    })
    
    rv$data <- data.frame(X = x_o, Y = y_new)
    showNotification("Blank correction applied.", type = "message")
  })
  
  filtered_data <- reactive({
    req(rv$data, input$xcol, input$ycol)
    df <- rv$data
    xmin <- input$fit_xmin
    xmax <- input$fit_xmax
    if (!is.na(xmin) && !is.na(xmax) && xmax >= xmin) {
      df <- df[df[[input$xcol]] >= xmin & df[[input$xcol]] <= xmax, , drop=FALSE]
    } else if (!is.na(xmin) && !is.na(xmax) && xmax < xmin) {
      showNotification("Max < Min. Using full data.", type="warning")
    }
    df
  })
  
  smoothed_data <- reactive({
    data_now <- filtered_data()
    req(data_now, input$ycol)
    
    if (nrow(data_now) > input$sg_window) {
      y_smooth <- sgolayfilt(data_now[[input$ycol]], p = input$sg_polyorder, n = input$sg_window, m = 0)
      data_smooth <- data.frame(X=data_now[[input$xcol]], Y=y_smooth)
    } else {
      data_smooth <- data_now
      names(data_smooth) <- c("X","Y")
    }
    data_smooth
  })

  output$baseline_params_ui <- renderUI({
    req(input$baseline_type)
    if (input$baseline_type == "manual") {
      # no numeric inputs if manual
      return(NULL)
    }
    if (input$baseline_type=="constant") {
      numericInput("bl_a0","a0",value=0)
    } else if (input$baseline_type=="linear") {
      tagList(
        numericInput("bl_a0","a0",value=0),
        numericInput("bl_a1","a1",value=0)
      )
    } else if (input$baseline_type=="quadratic") {
      tagList(
        numericInput("bl_a0","a0",value=0),
        numericInput("bl_a1","a1",value=0),
        numericInput("bl_a2","a2",value=0)
      )
    } else if (input$baseline_type=="cubic") {
      tagList(
        numericInput("bl_a0","a0",value=0),
        numericInput("bl_a1","a1",value=0),
        numericInput("bl_a2","a2",value=0),
        numericInput("bl_a3","a3",value=0)
      )
    }
  })
  
  derivative_spectrum <- reactive({
    data_smooth <- smoothed_data()
    req(data_smooth, nrow(data_smooth)>1)
    x <- data_smooth$X
    y <- data_smooth$Y
    
    sensitivity <- input$sensitivity_level
    order <- switch(sensitivity,
                    "low"=2,
                    "mid"=3,
                    "high"=4)
    
    if (length(y) < input$sg_window) {
      showNotification("Not enough data points for SG window.", type="error")
      return(data.frame(X=numeric(0),Y=numeric(0)))
    }
    
    deriv_data <- compute_derivative(x, y, order)
    if (nrow(deriv_data) > input$sg_window) {
      deriv_data$Y <- sgolayfilt(deriv_data$Y, p=input$sg_polyorder, n=input$sg_window, m=0)
    }
    deriv_data
  })
  
  # Manual Baseline: pick points
  # Only pick baseline if baseline_type == 'manual' and manual_baseline_pick = TRUE
  observeEvent(event_data("plotly_click", source="spectrum"), {
    if (input$baseline_type != "manual") return()
    if (!input$manual_baseline_pick) return()
    
    clk <- event_data("plotly_click", source="spectrum")
    if (is.null(clk)) return()
    
    df <- filtered_data()
    x_click <- clk$x
    idx <- which.min(abs(df[[input$xcol]] - x_click))
    
    new_point_x <- df[[input$xcol]][idx]
    new_point_y <- df[[input$ycol]][idx]
    
    rv$baseline_points <- rbind(rv$baseline_points,
                                data.frame(X = new_point_x, Y = new_point_y))
    showNotification(paste("Added baseline point at X =", round(new_point_x, 2)), type = "message")
  })
  
  observeEvent(input$clear_baseline_points, {
    rv$baseline_points <- data.frame(X = numeric(0), Y = numeric(0))
    showNotification("All baseline points cleared.", type = "message")
  })
  
  observeEvent(input$remove_baseline_point, {
    sel <- input$baseline_points_table_rows_selected
    if (length(sel) > 0) {
      rv$baseline_points <- rv$baseline_points[-sel, ]
      showNotification("Selected baseline point removed.", type = "message")
    } else {
      showNotification("No point selected.", type = "warning")
    }
  })
  
  output$baseline_points_table <- renderDT({
    datatable(rv$baseline_points, selection = "single", editable = FALSE, rownames = FALSE)
  }, server = FALSE)
  
  # Peak: auto/manual picking
  observeEvent(input$set_peaks, {
    data_smooth <- smoothed_data()
    derivative_data <- derivative_spectrum()
    req(data_smooth, derivative_data, nrow(derivative_data)>0)
    
    x_deriv <- derivative_data$X
    y_deriv <- derivative_data$Y
    
    sensitivity <- input$sensitivity_level
    minpeakheight <- switch(sensitivity,
                            "low"=max(y_deriv,na.rm=TRUE)*0.1,
                            "mid"=max(y_deriv,na.rm=TRUE)*0.05,
                            "high"=max(y_deriv,na.rm=TRUE)*0.02)
    
    peaks_detected <- findpeaks(y_deriv, minpeakheight=minpeakheight, sortstr=TRUE)
    if (is.null(peaks_detected)) {
      showNotification("No peaks detected.", type="warning")
      rv$peaks <- data.frame(center=numeric(0),height=numeric(0),width=numeric(0),extra=numeric(0))
      return()
    }
    
    peak_indices <- peaks_detected[,2]
    peak_heights <- peaks_detected[,1]
    peak_positions <- x_deriv[peak_indices]
    
    num_peaks <- input$num_peaks
    if (length(peak_positions)<num_peaks) {
      showNotification(paste("Only",length(peak_positions),"peaks detected."), type="warning")
      num_peaks <- length(peak_positions)
    }
    
    top_indices <- order(peak_heights,decreasing=TRUE)[1:num_peaks]
    selected_centers <- peak_positions[top_indices]
    
    original_df <- filtered_data()
    x_range <- diff(range(original_df[[input$xcol]]))
    default_width <- x_range/20
    
    selected_heights <- sapply(selected_centers, function(cx) {
      idx <- which.min(abs(original_df[[input$xcol]] - cx))
      original_df[[input$ycol]][idx]
    })
    selected_heights <- sapply(selected_heights, adjust_peak_sign)
    selected_widths <- rep(default_width, num_peaks)
    selected_extras <- rep(0, num_peaks)
    
    # Apply user-defined limits
    min_c <- input$min_peak_center
    max_c <- input$max_peak_center
    min_h <- input$min_peak_height
    max_h <- input$max_peak_height
    min_w <- input$min_peak_width
    max_w <- input$max_peak_width
    
    if (!is.na(min_c)) selected_centers <- pmax(selected_centers, min_c)
    if (!is.na(max_c)) selected_centers <- pmin(selected_centers, max_c)
    if (!is.na(min_h)) selected_heights <- pmax(selected_heights, min_h)
    if (!is.na(max_h)) selected_heights <- pmin(selected_heights, max_h)
    if (!is.na(min_w)) selected_widths <- pmax(selected_widths, min_w)
    if (!is.na(max_w)) selected_widths <- pmin(selected_widths, max_w)
    
    rv$peaks <- data.frame(center=selected_centers, height=selected_heights, width=selected_widths, extra=selected_extras)
    showNotification(paste("Set", num_peaks, "peaks."), type="message")
  })
  
  observeEvent(input$add_peak, {
    data_smooth <- smoothed_data()
    derivative_data <- derivative_spectrum()
    req(data_smooth, derivative_data, nrow(derivative_data)>0)
    
    x_deriv <- derivative_data$X
    y_deriv <- derivative_data$Y
    
    sensitivity <- input$sensitivity_level
    minpeakheight <- switch(sensitivity,
                            "low"=max(y_deriv,na.rm=TRUE)*0.1,
                            "mid"=max(y_deriv,na.rm=TRUE)*0.05,
                            "high"=max(y_deriv,na.rm=TRUE)*0.02)
    
    peaks_detected <- findpeaks(y_deriv, minpeakheight=minpeakheight, sortstr=TRUE)
    if (is.null(peaks_detected)) {
      showNotification("No peaks to add.", type="warning")
      return()
    }
    
    peak_positions <- x_deriv[peaks_detected[,2]]
    existing_centers <- rv$peaks$center
    new_positions <- peak_positions[!peak_positions %in% existing_centers]
    if (length(new_positions)==0) {
      showNotification("No new peaks to add.", type="warning")
      return()
    }
    
    new_center <- new_positions[1]
    original_df <- filtered_data()
    x_range <- diff(range(original_df[[input$xcol]]))
    default_width <- x_range/20
    
    idx <- which.min(abs(original_df[[input$xcol]] - new_center))
    new_height <- adjust_peak_sign(original_df[[input$ycol]][idx])
    
    min_c <- input$min_peak_center
    max_c <- input$max_peak_center
    min_h <- input$min_peak_height
    max_h <- input$max_peak_height
    min_w <- input$min_peak_width
    max_w <- input$max_peak_width
    
    if (!is.na(min_c)) new_center <- max(new_center, min_c)
    if (!is.na(max_c)) new_center <- min(new_center, max_c)
    if (!is.na(min_h)) new_height <- max(new_height, min_h)
    if (!is.na(max_h)) new_height <- min(new_height, max_h)
    new_width <- default_width
    if (!is.na(min_w)) new_width <- max(new_width, min_w)
    if (!is.na(max_w)) new_width <- min(new_width, max_w)
    
    rv$peaks <- rbind(rv$peaks, data.frame(center=new_center, height=new_height, width=new_width, extra=0))
    showNotification("Added a new peak.", type="message")
  })
  
  observeEvent(event_data("plotly_click", source="spectrum"), {
    if (input$mode_select != "manual") return()
    if (!input$manual_pick_mode) return()
    clk <- event_data("plotly_click", source="spectrum")
    if (is.null(clk)) return()
    
    click_x <- clk$x
    original_df <- filtered_data()
    idx <- which.min(abs(original_df[[input$xcol]] - click_x))
    new_center <- original_df[[input$xcol]][idx]
    new_height <- adjust_peak_sign(original_df[[input$ycol]][idx])
    
    x_range <- diff(range(original_df[[input$xcol]]))
    default_width <- x_range/20
    
    min_c <- input$min_peak_center
    max_c <- input$max_peak_center
    min_h <- input$min_peak_height
    max_h <- input$max_peak_height
    min_w <- input$min_peak_width
    max_w <- input$max_peak_width
    
    if (!is.na(min_c)) new_center <- max(new_center, min_c)
    if (!is.na(max_c)) new_center <- min(new_center, max_c)
    if (!is.na(min_h)) new_height <- max(new_height, min_h)
    if (!is.na(max_h)) new_height <- min(new_height, max_h)
    new_width <- default_width
    if (!is.na(min_w)) new_width <- max(new_width, min_w)
    if (!is.na(max_w)) new_width <- min(new_width, max_w)
    
    rv$peaks <- rbind(rv$peaks, data.frame(center=new_center, height=new_height, width=new_width, extra=0))
    showNotification(paste("Manually added a peak at X =", round(new_center,2)), type="message")
  })
  
  observeEvent(input$remove_peak, {
    sel <- input$peak_table_rows_selected
    if (length(sel)>0) {
      rv$peaks <- rv$peaks[-sel,]
      showNotification("Removed selected peak(s).", type="message")
    }
  })
  
  output$peak_table <- renderDT({
    dt <- rv$peaks
    extra_label <- if (input$peak_shape=="voigt") {
      "alpha"
    } else if (input$peak_shape=="mixed") {
      "gauss_frac"
    } else {
      "extra"
    }
    
    if (!(input$peak_shape %in% c("voigt","mixed"))) {
      datatable(dt[, c("center","height","width")],
                selection="single", editable=TRUE, rownames=FALSE)
    } else {
      dt2 <- dt
      colnames(dt2) <- c("center","height","width", extra_label)
      datatable(dt2, selection="single", editable=TRUE, rownames=FALSE)
    }
  }, server=FALSE)

  observeEvent(input$peak_table_cell_edit, {
    info <- input$peak_table_cell_edit
    i <- info$row
    j <- info$col
    value <- suppressWarnings(as.numeric(info$value))
    if (is.na(value)) {
      showNotification("Invalid numeric value.", type="error")
      return()
    }
    
    if (input$peak_shape %in% c("voigt","mixed")) {
      rv$peaks[i,j] <- value
    } else {
      # center(1), height(2), width(3)
      if (j==1) rv$peaks$center[i] <- value
      if (j==2) rv$peaks$height[i] <- value
      if (j==3) rv$peaks$width[i] <- value
    }
    
    min_c <- input$min_peak_center
    max_c <- input$max_peak_center
    min_h <- input$min_peak_height
    max_h <- input$max_peak_height
    min_w <- input$min_peak_width
    max_w <- input$max_peak_width
    
    if (!is.na(min_c)) rv$peaks$center[i] <- max(rv$peaks$center[i], min_c)
    if (!is.na(max_c)) rv$peaks$center[i] <- min(rv$peaks$center[i], max_c)
    if (!is.na(min_h)) rv$peaks$height[i] <- max(rv$peaks$height[i], min_h)
    if (!is.na(max_h)) rv$peaks$height[i] <- min(rv$peaks$height[i], max_h)
    if (!is.na(min_w)) rv$peaks$width[i] <- max(rv$peaks$width[i], min_w)
    if (!is.na(max_w)) rv$peaks$width[i] <- min(rv$peaks$width[i], max_w)
    
    showNotification("Peak parameters updated.", type="message")
  })
  
  # Fitting
  observeEvent(input$fit_button, {
    data_smooth <- smoothed_data()
    req(data_smooth, nrow(rv$peaks)>0)
    
    if (nrow(rv$peaks)==0) {
      showNotification("No peaks set.",type="error")
      return()
    }
    
    # Validate
    min_c <- input$min_peak_center
    max_c <- input$max_peak_center
    min_h <- input$min_peak_height
    max_h <- input$max_peak_height
    min_w <- input$min_peak_width
    max_w <- input$max_peak_width
    
    if (!is.na(min_c)&&!is.na(max_c)&&min_c>max_c) {
      showNotification("Min center > Max center.", type="error")
      return()
    }
    if (!is.na(min_h)&&!is.na(max_h)&&min_h>max_h) {
      showNotification("Min height > Max height.", type="error")
      return()
    }
    if (!is.na(min_w)&&!is.na(max_w)&&min_w>max_w) {
      showNotification("Min width > Max width.", type="error")
      return()
    }
    
    x <- data_smooth$X
    y <- data_smooth$Y
    
    if (length(x) < 2) {
      showNotification("Not enough data points.", type="error")
      return()
    }
    
    # Construct start params
    if (input$baseline_type %in% c("constant","linear","quadratic","cubic")) {
      blparams <- switch(input$baseline_type,
                         "constant"=c(input$bl_a0),
                         "linear"=c(input$bl_a0,input$bl_a1),
                         "quadratic"=c(input$bl_a0,input$bl_a1,input$bl_a2),
                         "cubic"=c(input$bl_a0,input$bl_a1,input$bl_a2,input$bl_a3))
    } else {
      # baseline_type == 'manual'
      blparams <- numeric(0)
    }
    
    nparams_baseline <- length(blparams)
    np_per_peak <- if (input$peak_shape %in% c("voigt","mixed")) 4 else 3
    
    peak_params <- c()
    for (i in seq_len(nrow(rv$peaks))) {
      h_adj <- rv$peaks$height[i]
      h_adj <- if (input$peak_sign=="positive" && h_adj<0) abs(h_adj) else h_adj
      h_adj <- if (input$peak_sign=="negative" && h_adj>0) -abs(h_adj) else h_adj
      
      peak_params <- c(peak_params, rv$peaks$center[i], h_adj, rv$peaks$width[i])
      if (input$peak_shape %in% c("voigt","mixed")) {
        peak_params <- c(peak_params, rv$peaks$extra[i])
      }
    }
    
    start_params <- c(blparams, peak_params)
    lower <- rep(-Inf, length(start_params))
    upper <- rep(Inf,  length(start_params))
    
    for (i in seq_len(nrow(rv$peaks))) {
      idx_center <- nparams_baseline + (i-1)*np_per_peak + 1
      idx_height <- nparams_baseline + (i-1)*np_per_peak + 2
      idx_width  <- nparams_baseline + (i-1)*np_per_peak + 3
      
      if (!is.na(min_c)) lower[idx_center] <- max(lower[idx_center],min_c)
      if (!is.na(max_c)) upper[idx_center] <- min(upper[idx_center],max_c)
      if (!is.na(min_h)) lower[idx_height] <- max(lower[idx_height],min_h)
      if (!is.na(max_h)) upper[idx_height] <- min(upper[idx_height],max_h)
      if (!is.na(min_w)) lower[idx_width]  <- max(lower[idx_width],min_w)
      if (!is.na(max_w)) upper[idx_width]  <- min(upper[idx_width],max_w)
    }
    
    # run fit
    fit <- tryCatch({
      nls.lm(
        par = start_params,
        fn = resid_fun,
        x = x, y = y,
        peak_func = input$peak_shape,
        baseline_type = input$baseline_type,
        nparams_bl = nparams_baseline,
        baseline_points = rv$baseline_points,  # pass the user points for interpolation
        lower = lower,
        upper = upper,
        control = nls.lm.control(maxiter = 200)
      )
    }, error = function(e) {
      showNotification(paste("Fitting error:",e$message),type="error")
      NULL
    })
    
    if (!is.null(fit)) {
      rv$fit_error <- FALSE
      # store results
      rv$fit_result <- list(
        params = coef(fit),
        x = x,
        y = y,
        baseline_type = input$baseline_type,
        peak_func = input$peak_shape,
        nparams_bl = nparams_baseline
      )
      # update rv$peaks with fitted peak params
      newparams <- coef(fit)
      fitted_peakparams <- newparams[(nparams_baseline+1):length(newparams)]
      n_peaks_fitted <- length(fitted_peakparams)/np_per_peak
      
      new_peaks <- data.frame(center=numeric(n_peaks_fitted),
                              height=numeric(n_peaks_fitted),
                              width=numeric(n_peaks_fitted),
                              extra=numeric(n_peaks_fitted),
                              stringsAsFactors=FALSE)
      
      for (i in seq_len(n_peaks_fitted)) {
        idx <- ((i-1)*np_per_peak+1):((i-1)*np_per_peak+np_per_peak)
        pvals <- fitted_peakparams[idx]
        new_peaks$center[i] <- pvals[1]
        new_peaks$height[i] <- pvals[2]
        new_peaks$width[i] <- pvals[3]
        if (input$peak_shape %in% c("voigt","mixed")) {
          new_peaks$extra[i] <- pvals[4]
        }
      }
      
      # apply sign filter
      if (input$peak_sign=="positive") {
        new_peaks <- new_peaks[new_peaks$height>0, , drop=FALSE]
      } else if (input$peak_sign=="negative") {
        new_peaks <- new_peaks[new_peaks$height<0, , drop=FALSE]
      }
      rv$peaks <- new_peaks
      
      showNotification("Fitting completed successfully.", type="message")
    } else {
      rv$fit_error <- TRUE
      rv$fit_result <- NULL
    }
  })
  

  #  Plotly outputs
  output$plot_spectrum <- renderPlotly({
    original_df <- filtered_data()
    req(original_df, input$xcol, input$ycol)
    x <- original_df[[input$xcol]]
    y <- original_df[[input$ycol]]
    
    p <- ggplot() +
      geom_line(aes(x, y), color="black") +
      xlab("X") + ylab("Y") +
      ggtitle("Original and Fitted Spectrum")
    
    if (!is.null(rv$fit_result)) {
      fit_y <- composite_model(
        rv$fit_result$params, 
        rv$fit_result$x,
        rv$fit_result$peak_func,
        rv$fit_result$baseline_type,
        rv$fit_result$nparams_bl,
        rv$baseline_points
      )
      df_fit <- data.frame(X=rv$fit_result$x, Fitted=fit_y)
      p <- p + geom_line(data=df_fit, aes(X, Fitted), color="red", size=1)
      
      # Show individual peaks if requested
      if (input$show_individual_peaks) {
        newparams <- rv$fit_result$params
        nparams_bl <- rv$fit_result$nparams_bl
        peak_func <- rv$fit_result$peak_func
        np_per_peak <- if (peak_func %in% c("voigt","mixed")) 4 else 3
        peak_params <- newparams[(nparams_bl+1):length(newparams)]
        n_peaks <- length(peak_params)/np_per_peak
        # current baseline
        baseline_vec <- baseline_fun(rv$fit_result$x, 
                                     rv$fit_result$baseline_type,
                                     newparams[1:nparams_bl],
                                     rv$baseline_points)
        
        for (i in seq_len(n_peaks)) {
          idx <- ((i-1)*np_per_peak+1):((i-1)*np_per_peak+np_per_peak)
          pvals <- peak_params[idx]
          center <- pvals[1]
          height <- pvals[2]
          width_hwhm <- pvals[3]
          
          if (input$peak_sign=="positive" && height<=0) next
          if (input$peak_sign=="negative" && height>=0) next
          
          if (peak_func=="voigt") {
            alpha <- pvals[4]
            peak_y <- voigt_peak(rv$fit_result$x, center, height, width_hwhm, alpha) + baseline_vec
          } else if (peak_func=="gauss") {
            peak_y <- gauss_peak(rv$fit_result$x, center, height, width_hwhm) + baseline_vec
          } else if (peak_func=="lorentz") {
            peak_y <- lorentz_peak(rv$fit_result$x, center, height, width_hwhm) + baseline_vec
          } else if (peak_func=="mixed") {
            gauss_frac <- pvals[4]
            peak_y <- mixed_peak(rv$fit_result$x, center, height, width_hwhm, gauss_frac) + baseline_vec
          } else if (peak_func=="lognormal") {
            peak_y <- lognormal_peak(rv$fit_result$x, center, height, width_hwhm) + baseline_vec
          }
          
          df_peak <- data.frame(X=rv$fit_result$x, Y=peak_y)
          p <- p + geom_line(data=df_peak, aes(X, Y), color="blue", linetype="dashed")
        }
      }
    }
    
    # Label peaks
    if (nrow(rv$peaks) > 0) {
      peak_labels <- data.frame(
        X = rv$peaks$center,
        Y = sapply(rv$peaks$center, function(cx) {
          idx <- which.min(abs(x - cx))
          y[idx]
        }),
        Label = paste0("P", 1:nrow(rv$peaks))
      )
      p <- p + geom_text(data=peak_labels, aes(X, Y, label=Label),
                         vjust=-1, color="blue", size=4)
    }
    
    # Show baseline points if baseline_type == manual
    if (input$baseline_type == "manual" && nrow(rv$baseline_points) > 0) {
      p <- p + geom_point(data=rv$baseline_points, 
                          aes(X, Y), color="red", size=2)
    }
    
    xp <- ggplotly(p, tooltip="all", source="spectrum")
    xp <- event_register(xp, 'plotly_click')
    xp
  })
  
  output$plot_residuals <- renderPlotly({
    if (is.null(rv$fit_result)) {
      p <- ggplot() + 
        ggtitle("Residuals") + 
        xlab("X") + ylab("Residual") +
        annotate("text", x=0.5, y=0.5, label="No fit results yet.", size=5)
      return(ggplotly(p))
    }
    x <- rv$fit_result$x
    y <- rv$fit_result$y
    fit_y <- composite_model(rv$fit_result$params, x,
                             rv$fit_result$peak_func,
                             rv$fit_result$baseline_type,
                             rv$fit_result$nparams_bl,
                             rv$baseline_points)
    res <- y - fit_y
    df_res <- data.frame(X=x, Residual=res)
    p <- ggplot(df_res, aes(X, Residual))+
      geom_line(color='green')+
      geom_hline(yintercept=0, color='red', linetype='dashed')+
      ggtitle("Residuals") + xlab("X") + ylab("Residual")
    ggplotly(p, tooltip="all")
  })
  
  output$plot_derivative <- renderPlotly({
    derivative_data <- derivative_spectrum()
    if (nrow(derivative_data) == 0) {
      p <- ggplot() +
        ggtitle("Derivative Spectrum") +
        annotate("text", x=0.5, y=0.5, label="Not enough points or error.", size=5)
      return(ggplotly(p))
    }
    
    derivative_order <- switch(input$sensitivity_level,
                               "low"=2,
                               "mid"=3,
                               "high"=4)
    
    p <- ggplot(derivative_data, aes(X, Y)) +
      geom_line(color='purple') +
      ggtitle(paste("Derivative Spectrum (Order:", derivative_order, ")")) +
      xlab("X") + ylab(paste0("d^", derivative_order,"Y/dX^",derivative_order))
    ggplotly(p, tooltip="all")
  })
  
  # Fit Summary
  
  fit_summary_text <- reactive({
    if (rv$fit_error) {
      return("Fitting failed.")
    }
    if (is.null(rv$fit_result)) {
      return("No fit performed yet.")
    }
    
    p <- rv$fit_result$params
    nparams_bl <- rv$fit_result$nparams_bl
    baseline_params <- p[1:nparams_bl]
    peak_func <- rv$fit_result$peak_func
    bltype <- rv$fit_result$baseline_type
    
    baseline_labels <- switch(bltype,
                              "constant"=c("a0"),
                              "linear"=c("a0","a1"),
                              "quadratic"=c("a0","a1","a2"),
                              "cubic"=c("a0","a1","a2","a3"),
                              "manual"=character(0))
    
    final_peaks <- rv$peaks
    x <- rv$fit_result$x
    y <- rv$fit_result$y
    
    # compute final fit + baseline
    fit_y <- composite_model(
      rv$fit_result$params, 
      x, 
      rv$fit_result$peak_func, 
      bltype, 
      rv$fit_result$nparams_bl,
      rv$baseline_points
    )
    original_area <- trapz(x, y)
    fitted_area   <- trapz(x, fit_y)
    
    # Calculate peak areas
    peak_area_list <- numeric(0)
    if (nrow(final_peaks) > 0) {
      for (i in seq_len(nrow(final_peaks))) {
        center <- final_peaks$center[i]
        height <- final_peaks$height[i]
        width_hwhm <- final_peaks$width[i]
        
        if (peak_func=="voigt") {
          alpha <- final_peaks$extra[i]
          peak_y <- voigt_peak(x, center, height, width_hwhm, alpha)
        } else if (peak_func=="gauss") {
          peak_y <- gauss_peak(x, center, height, width_hwhm)
        } else if (peak_func=="lorentz") {
          peak_y <- lorentz_peak(x, center, height, width_hwhm)
        } else if (peak_func=="mixed") {
          gauss_frac <- final_peaks$extra[i]
          peak_y <- mixed_peak(x, center, height, width_hwhm, gauss_frac)
        } else if (peak_func=="lognormal") {
          peak_y <- lognormal_peak(x, center, height, width_hwhm)
        }
        area_val <- trapz(x, peak_y)
        peak_area_list <- c(peak_area_list, area_val)
      }
    }
    
    # Build text
    txt <- "Fit Summary\n\n"
    txt <- paste0(txt, "Peak Shape Used for Fitting: ", peak_func, "\n\n")
    
    if (bltype == "manual") {
      txt <- paste0(txt, "Baseline Type: manual (using user-picked points)\n")
    } else {
      txt <- paste0(txt, "Baseline Type: ", bltype, "\n")
      txt <- paste0(txt, "Baseline Parameters:\n")
      for (i in seq_along(baseline_labels)) {
        txt <- paste0(txt, baseline_labels[i], " = ", round(baseline_params[i],4), "\n")
      }
    }
    
    # If manual baseline, list user points
    if (bltype == "manual") {
      if (nrow(rv$baseline_points) > 0) {
        txt <- paste0(txt, "\nUser-Picked Baseline Points (X, Y):\n")
        for (i in seq_len(nrow(rv$baseline_points))) {
          txt <- paste0(
            txt, "   ", i, ": (",
            round(rv$baseline_points$X[i],4), ", ",
            round(rv$baseline_points$Y[i],4), ")\n"
          )
        }
      } else {
        txt <- paste0(txt, "\nNo baseline points picked.\n")
      }
    }
    
    txt <- paste0(txt, "\nPeak Parameters (All widths = HWHM):\n")
    if (nrow(final_peaks) > 0) {
      total_peaks_area <- sum(peak_area_list)
      peak_area_perc   <- (peak_area_list / total_peaks_area) * 100
      for (i in seq_len(nrow(final_peaks))) {
        txt <- paste0(txt, "Peak ", i, ": center=", round(final_peaks$center[i],4),
                      ", height=", round(final_peaks$height[i],4),
                      ", width=", round(final_peaks$width[i],4))
        if (peak_func %in% c("voigt","mixed")) {
          txt <- paste0(txt, ", extra=", round(final_peaks$extra[i],4))
        }
        txt <- paste0(txt,
                      ", Area=", round(peak_area_list[i],4),
                      ", Area(%)=", round(peak_area_perc[i],2), "%\n")
      }
    } else {
      txt <- paste0(txt, "No peaks.\n")
    }
    
    txt <- paste0(txt, "\nArea Information:\n")
    txt <- paste0(txt, "Total area under original data: ", round(original_area,4), "\n")
    txt <- paste0(txt, "Total area under fitted data: ", round(fitted_area,4), "\n")
    if (length(peak_area_list) > 0) {
      txt <- paste0(txt, "Sum of all peak areas: ", round(sum(peak_area_list),4), "\n")
    }
    
    txt
  })
  
  output$fit_summary <- renderUI({
    if (rv$fit_error) {
      return(tags$div(style="color:red;", "Fitting failed."))
    }
    if (is.null(rv$fit_result)) {
      return("No fit performed yet.")
    }
    
    p <- rv$fit_result$params
    nparams_bl <- rv$fit_result$nparams_bl
    baseline_params <- p[1:nparams_bl]
    peak_func <- rv$fit_result$peak_func
    bltype <- rv$fit_result$baseline_type
    
    baseline_labels <- switch(bltype,
                              "constant"=c("a0"),
                              "linear"=c("a0","a1"),
                              "quadratic"=c("a0","a1","a2"),
                              "cubic"=c("a0","a1","a2","a3"),
                              "manual"=character(0))
    
    final_peaks <- rv$peaks
    x <- rv$fit_result$x
    y <- rv$fit_result$y
    fit_y <- composite_model(
      rv$fit_result$params, x,
      peak_func, bltype,
      rv$fit_result$nparams_bl,
      rv$baseline_points
    )
    original_area <- trapz(x,y)
    fitted_area   <- trapz(x,fit_y)
    
    peak_area_list <- numeric(0)
    if (nrow(final_peaks) > 0) {
      for (i in seq_len(nrow(final_peaks))) {
        center <- final_peaks$center[i]
        height <- final_peaks$height[i]
        width_hwhm <- final_peaks$width[i]
        
        if (peak_func=="voigt") {
          alpha <- final_peaks$extra[i]
          peak_y <- voigt_peak(x, center, height, width_hwhm, alpha)
        } else if (peak_func=="gauss") {
          peak_y <- gauss_peak(x, center, height, width_hwhm)
        } else if (peak_func=="lorentz") {
          peak_y <- lorentz_peak(x, center, height, width_hwhm)
        } else if (peak_func=="mixed") {
          gauss_frac <- final_peaks$extra[i]
          peak_y <- mixed_peak(x, center, height, width_hwhm, gauss_frac)
        } else if (peak_func=="lognormal") {
          peak_y <- lognormal_peak(x, center, height, width_hwhm)
        }
        
        area_val <- trapz(x, peak_y)
        peak_area_list <- c(peak_area_list, area_val)
      }
    }
    
    # Build final table of peaks
    peak_area_table <- data.frame()
    if (nrow(final_peaks) > 0) {
      total_peaks_area <- sum(peak_area_list)
      peak_area_perc <- (peak_area_list / total_peaks_area)*100
      
      # rename if voigt/mixed
      if (peak_func == "voigt") {
        colnames(final_peaks)[4] <- "alpha"
      } else if (peak_func == "mixed") {
        colnames(final_peaks)[4] <- "gauss_frac"
      } else {
        colnames(final_peaks)[4] <- "extra"
      }
      
      peak_area_table <- cbind(
        final_peaks,
        Area = round(peak_area_list, 4),
        `Area(%)` = round(peak_area_perc, 2)
      )
    }
    
    # baseline table
    bl_table <- data.frame()
    if (length(baseline_labels) > 0) {
      bl_table <- data.frame(Param=baseline_labels, Value=round(baseline_params,4))
    }
    
    tagList(
      h3("Fit Summary"),
      h4("Peak Shape Used for Fitting"),
      p(peak_func),
      if (bltype != "manual") {
        tagList(
          h4("Baseline Parameters"),
          renderTable(bl_table, digits=4)
        )
      } else {
        h4("Baseline Type: manual (using user-picked points)")
      },
      # Show baseline points in Fit Summary tab
      output$baseline_points_summary <- renderUI({
        if (input$baseline_type != "manual") {
          return(p("Automatic baseline"))
        }
        if (nrow(rv$baseline_points) == 0) {
          return(p("No baseline points picked."))
        }
        tagList(
          p("List of baseline points (X, Y):"),
          renderTable(rv$baseline_points, digits=4)
        )
      }),
      h4("Peak Parameters (All widths = HWHM)"),
      if (nrow(peak_area_table) > 0) {
        renderTable(peak_area_table, digits=4)
      } else {
        p("No peaks found.")
      },
      h4("Area Information"),
      p(paste("Total area under original data:", round(original_area,4))),
      p(paste("Total area under fitted data:",   round(fitted_area,4))),
      if (length(peak_area_list) > 0) {
        p(paste("Sum of all peak areas:", round(sum(peak_area_list),4)))
      }
    )
  })
  
  # Downloads
  output$download_combined_csv <- downloadHandler(
    filename = function() {
      paste0("fitted_peaks_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$fit_result)
      x <- rv$fit_result$x
      y_orig <- rv$fit_result$y
      
      # Baseline
      nparams_bl <- rv$fit_result$nparams_bl
      blparams   <- rv$fit_result$params[1:nparams_bl]
      baseline_vec <- baseline_fun(
        x,
        rv$fit_result$baseline_type,
        blparams,
        rv$baseline_points
      )
      
      # Fitted
      fit_y <- composite_model(
        rv$fit_result$params,
        x,
        rv$fit_result$peak_func,
        rv$fit_result$baseline_type,
        rv$fit_result$nparams_bl,
        rv$baseline_points
      )
      residual <- y_orig - fit_y
      
      # Individual peaks
      peak_func <- rv$fit_result$peak_func
      np_per_peak <- if (peak_func %in% c("voigt","mixed")) 4 else 3
      all_peak_params <- rv$fit_result$params[(nparams_bl+1):length(rv$fit_result$params)]
      n_peaks <- length(all_peak_params)/np_per_peak
      
      out_df <- data.frame(
        X = x,
        Original = y_orig,
        Baseline = baseline_vec,
        Fitted   = fit_y,
        Residual = residual
      )
      
      for (i in seq_len(n_peaks)) {
        idx <- ((i - 1)*np_per_peak + 1):((i - 1)*np_per_peak + np_per_peak)
        pvals <- all_peak_params[idx]
        center <- pvals[1]
        height <- pvals[2]
        width_hwhm <- pvals[3]
        
        if (peak_func == "voigt") {
          alpha <- pvals[4]
          peak_y <- voigt_peak(x, center, height, width_hwhm, alpha)
        } else if (peak_func == "gauss") {
          peak_y <- gauss_peak(x, center, height, width_hwhm)
        } else if (peak_func == "lorentz") {
          peak_y <- lorentz_peak(x, center, height, width_hwhm)
        } else if (peak_func == "mixed") {
          gauss_frac <- pvals[4]
          peak_y <- mixed_peak(x, center, height, width_hwhm, gauss_frac)
        } else if (peak_func == "lognormal") {
          peak_y <- lognormal_peak(x, center, height, width_hwhm)
        }
        
        out_df[[paste0("Peak", i)]] <- peak_y
      }
      
      write.csv(out_df, file, row.names=FALSE)
    }
  )
  
  # Fit Summary TXT
  output$download_fit_summary <- downloadHandler(
    filename = function(){
      paste0("fit_summary_", Sys.Date(), ".txt")
    },
    content = function(file) {
      txt <- fit_summary_text()
      writeLines(txt, con = file)
    }
  )
}

shinyApp(ui=ui, server=server)
