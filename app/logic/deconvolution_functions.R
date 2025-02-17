# app/logic/deconvolution_functions.R

box::use(
  ggplot2,
  plotly[config, event_register, ggplotly, layout],
  utils[read.table],
  scales[percent_format],
  shiny[showNotification],
  parallel[detectCores, makeCluster, parLapply, stopCluster],
  reticulate[use_python, py_config, py_run_string],
  viridis[scale_fill_viridis],
)

#' @export
deconvolute <- function(raw_dirs,
                        num_cores = detectCores() - 1,
                        startz = 1, endz = 50,
                        minmz = '', maxmz = '',
                        masslb = 5000, massub = 500000,
                        massbins = 10, peakthresh = 0.1,
                        peakwindow = 500, peaknorm = 1,
                        time_start = '', time_end = '') {

  # ensure python path and packages availability
  py_outcome <- tryCatch({
    use_python(py_config()$python, required = TRUE)
    TRUE
  }, error = function(e) {
    # showNotification(
    #   "Python modules could not be loaded. Aborting.",
    #   type = "error",
    #   duration = NULL
    # )
    FALSE
  })

  if (!py_outcome) {
    return()
  }

  # Deconvolution function for a single waters .raw
  process_single_dir <- function(waters_dir,
                                 startz, endz, minmz, maxmz,
                                 masslb, massub, massbins, peakthresh,
                                 peakwindow, peaknorm, time_start, time_end) {

    input_path <- gsub("\\\\", "/", waters_dir)

    # Function to properly format parameters for Python
    format_param <- function(x) {
      if (is.character(x) && x == "") {
        return("''")
      } else {
        return(as.character(x))
      }
    }

    # Create parameters string for Python
    params_string <- sprintf(
      '"startz": %s, "endz": %s, "minmz": %s, "maxmz": %s, "masslb": %s, "massub": %s, "massbins": %s, "peakthresh": %s, "peakwindow": %s, "peaknorm": %s, "time_start": %s, "time_end": %s',
      format_param(startz),
      format_param(endz),
      format_param(minmz),
      format_param(maxmz),
      format_param(masslb),
      format_param(massub),
      format_param(massbins),
      format_param(peakthresh),
      format_param(peakwindow),
      format_param(peaknorm),
      format_param(time_start),
      format_param(time_end)
    )

    reticulate::py_run_string(sprintf('
import sys
import unidec
import re

# Initialize UniDec engine
engine = unidec.UniDec()

# Convert Waters .raw to txt
input_file = r"%s"
engine.raw_process(input_file)
txt_file = re.sub(r"\\.raw$", "_rawdata.txt", input_file)
engine.open_file(txt_file)

# Parameters passed from R
params = {%s}

# Set configuration parameters
engine.config.startz = params["startz"]
engine.config.endz = params["endz"]
engine.config.minmz = params["minmz"]
engine.config.maxmz = params["maxmz"]
engine.config.masslb = params["masslb"]
engine.config.massub = params["massub"]
engine.config.massbins = params["massbins"]
engine.config.peakthresh = params["peakthresh"]
engine.config.peakwindow = params["peakwindow"]
engine.config.peaknorm = params["peaknorm"]
engine.config.time_start = params["time_start"]
engine.config.time_end = params["time_end"]

# Process and deconvolve the data
engine.process_data()
engine.run_unidec()
engine.pick_peaks()
', input_path, params_string))
  }

  # showNotification(paste0("Deconvolution initiated"),
  #                  type = "message", duration = NULL)

  # Process directories in parallel
  if(num_cores > 1) {
    cl <- makeCluster(num_cores)
    on.exit(stopCluster(cl))

    message(paste0(num_cores, " cores detected. Parallel processing started."))

    # Pass variables
    startz <- startz
    endz <- endz
    minmz <- minmz
    maxmz <- maxmz
    masslb <- masslb
    massub <- massub
    massbins <- massbins
    peakthresh <- peakthresh
    peakwindow <- peakwindow
    peaknorm <- peaknorm
    time_start <- time_start
    time_end <- time_end

    # Create wrapper function that includes all parameters
    process_wrapper <- function(dir) {
      process_single_dir(dir,
                         startz, endz,
                         minmz, maxmz,
                         masslb, massub,
                         massbins, peakthresh,
                         peakwindow, peaknorm,
                         time_start, time_end)
    }

    results <- parLapply(cl, raw_dirs, process_wrapper)

  } else {
    message(paste0(num_cores, " core(s) detected. Sequential processing started."))

    results <- lapply(raw_dirs, function(dir) {
      process_single_dir(dir,
                         startz, endz,
                         minmz, maxmz,
                         masslb, massub,
                         massbins, peakthresh,
                         peakwindow, peaknorm,
                         time_start, time_end)
    })
  }

  # Summarize results
  successful <- sum(sapply(results, function(x) !is.null(x)))
  failed <- length(results) - successful

  # showNotification("Deconvolution finalized", type = "message", duration = NULL)
  # message(sprintf(
  #   "\nProcessing complete:\n- Successfully processed: %d\n- Failed: %d",
  #   successful, failed))

  # return(results)
}

#' @export
plot_ms_spec <- function(waters_dir) {

  # Get results directories
  unidecfiles <- list.files(waters_dir, full.names = TRUE)

  # Get file
  mass_intensity <- grep("_mass\\.txt$", unidecfiles, value = TRUE)
  mass_data <- read.table(mass_intensity, sep = " ", header = TRUE)
  colnames(mass_data) <- c("mz", "intensity")

  plot(mass_data$mz, mass_data$intensity, type = "h",
       xlab = "Mass (Da)", ylab = "Intensity",
       main = "Deconvoluted Mass Spectrum",
       col = "blue", lwd = 3)
}

#' @export
create_384_plate_heatmap <- function(data) {
  # Create plate layout coordinates
  rows <- rev(LETTERS[1:16])
  cols <- 1:24
  plate_layout <- expand.grid(row = rows, col = cols)
  plate_layout$well_id <- paste0(plate_layout$row, plate_layout$col)

  # Merge data with plate layout
  plate_data <- merge(plate_layout, data, by = "well_id", all.x = TRUE)

  # Create tooltip text with NA handling
  plate_data$tooltip_text <- sprintf(
    "Well: %s\nValue: %s\nSample: %s",
    plate_data$well_id,
    ifelse(is.na(plate_data$value), "NA", sprintf("%.2f", plate_data$value)),
    ifelse(is.na(plate_data$sample), "Empty", as.character(plate_data$sample))
  )

  min_value <- min(plate_data$value, na.rm = TRUE)
  max_value <- max(plate_data$value, na.rm = TRUE)

  # Ensure a color range even if only one unique value
  if (min_value == max_value) {
    min_value <- min_value - 0.01
    max_value <- max_value + 0.01
  }

  # Create the ggplot
  plate_plot <- ggplot2$ggplot(
    plate_data,
    ggplot2$aes(x = col, y = factor(row, levels = rev(rows)), fill = value)) +
    ggplot2$geom_rect(
      data = plate_layout,
      ggplot2$aes(
        xmin = col - 0.5,
        xmax = col + 0.5,
        ymin = match(row, rev(rows)) - 0.5,
        ymax = match(row, rev(rows)) + 0.5
      ),
      fill = NA,
      color = "black",
      linewidth = 0.5
    ) +
    ggplot2$geom_tile(ggplot2$aes(text = tooltip_text),
                      width = 0.95, height = 0.95) +
    ggplot2$scale_y_discrete(limits = rows) +
    ggplot2$scale_x_continuous(
      breaks = 1:24,
      labels = 1:24,
      position = "top",
      expand = c(0, 0)
    ) +
    scale_fill_viridis(name = "Peak m/z", option = "D",
                       limits = c(min_value, max_value)) +
    ggplot2$coord_fixed() +
    ggplot2$theme_minimal() +
    ggplot2$theme(
      axis.text.x = ggplot2$element_text(size = 8, angle = 0, vjust = 0,
                                         hjust = 0.5),
      axis.text.y = ggplot2$element_text(size = 8, hjust = 1),
      axis.title = ggplot2$element_blank(),
      panel.grid = ggplot2$element_blank(),
      axis.ticks = ggplot2$element_blank(),
      plot.margin = ggplot2$margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    )

  # Convert to plotly
  interactive_plot <- ggplotly(plate_plot, tooltip = "text")

  interactive_plot <- interactive_plot |>
    layout(
      dragmode = FALSE,
      showlegend = TRUE,
      yaxis = list(
        scaleanchor = "x",
        scaleratio = 1,
        showgrid = FALSE,
        zeroline = FALSE,
        tickson = "boundaries",
        tickfont = list(size = 12),
        tickangle = 0,
        automargin = TRUE,
        title = "",
        # ticklabelposition="inside",
        ticklabeloverflow = "allow"
      ),
      xaxis = list(
        anchor = "y",
        automargin = TRUE,
        overlaying = "y",
        side = "top",
        showgrid = FALSE,
        griddash = "20px",
        zeroline = FALSE,
        tickmode = "array",
        tickvals = 1:24,
        ticktext = as.character(1:24),
        tickfont = list(size = 12),
        tickangle = 0
      ),
      margin = list(t = 40, r = 0, b = 0, l = 50),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    ) |>
    config(
      displayModeBar = "hover",
      scrollZoom = FALSE,
      modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d",
                                 "hoverClosestCartesian",
                                 "hoverCompareCartesian"),
      modeBarButtonsToKeep = c("zoom2d", "toImage", "autoScale2d",
                               "resetScale2d", "zoomIn2d", "zoomOut2d")
    ) |>
    event_register("plotly_click")

  return(interactive_plot)
}

# Min-Max normalization and scale to percentage
normalize_min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)) * 100)  # Scale to 0-100
}

#' @export
spectrum_plot <- function(result_path, raw) {

  # Read the data
  base <- gsub("_unidecfiles", "", basename(result_path))

  if(raw) {
    mass <- utils::read.delim(file.path(result_path,
                                        paste0(base, "_rawdata.txt")),
                       sep = " ", header = FALSE)

    mass$V2 <- ((mass$V2 - min(mass$V2)) / (max(mass$V2) - min(mass$V2)) * 100)
  } else {
    mass <- utils::read.delim(file.path(result_path, paste0(base, "_mass.txt")),
                       sep = " ", header = FALSE)
    peaks <- utils::read.delim(file.path(result_path,
                                         paste0(base, "_peaks.dat")),
                        sep = " ", header = FALSE)

    mass$V2 <- ((mass$V2 - min(mass$V2)) / (max(mass$V2) - min(mass$V2)) * 100)
    highlight_peaks <- mass[mass$V1 %in% peaks$V1, ]
  }

  # Create plot with tooltip data
  plot <- ggplot2$ggplot(mass, ggplot2$aes(x = V1, y = V2, group = 1,
                           text = paste0("Mass: ", V1, " Da\nIntensity: ",
                                         round(V2, 2), "%"))) +
    ggplot2$geom_line() +  # Draw the main line
    ggplot2$scale_y_continuous(labels = percent_format(scale = 1)) +
    ggplot2$theme_minimal()

  if(raw) {
    plot <- plot + ggplot2$labs(y = "Intensity [%]", x = "m/z [Th]")
  } else {
    plot <- plot +
      ggplot2$geom_point(data = highlight_peaks,
                         ggplot2$aes(x = V1, y = V2),
                         fill = "#e8cb97", colour = "#35357A",
                         shape = 21, size = 2) +
      ggplot2$labs(y = "Intensity [%]", x = "Mass [Da]")
  }

  # Convert to interactive plot
  interactive_plot <- ggplotly(plot, tooltip = "text") |>
    config(
      displayModeBar = "hover",
      scrollZoom = FALSE,
      modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d",
                                 "hoverClosestCartesian",
                                 "hoverCompareCartesian"),
      modeBarButtonsToKeep = c("zoom2d", "toImage", "autoScale2d",
                               "resetScale2d", "zoomIn2d", "zoomOut2d")
    )

  # Display the plot
  interactive_plot
}
