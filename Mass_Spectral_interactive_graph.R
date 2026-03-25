# ---
# title: "Mass spectral interactive graph"
# author: "Gurveer Sidhu"
# format: html
# editor: visual
# ---

## Install packages 


library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(htmltools)





## Trying to name csv on file sheet



# You are able to update the folder name, it does not need to be CM1 :)
FOI <- "CM1"
# Get all CSV files inside CM folder 
files <- list.files(FOI, pattern = "\\.csv$", full.names = TRUE)

# Read and combine them into one dataframe, adding the file names to the dataframe
cm_data1 <- files %>%
  map_dfr(~ read_csv(.x, skip = 1, show_col_types = FALSE) %>%
            mutate(source_file = basename(.x)), .id = "file_id")

# View the combined dataframe with file names
View(cm_data1)



## One graph

#With colour lines 


data <- cm_data1
selected_file <- "01-01.csv"

file_column <- "source_file"
x_column <- "X(Thompsons)"
y_column <- "Y(Counts)"

one_data <- data %>%
  filter(.data[[file_column]] == selected_file) %>%
  mutate(
    mz = as.numeric(.data[[x_column]]),
    intensity = as.numeric(.data[[y_column]])
  ) %>%
  filter(
    !is.na(mz),
    !is.na(intensity),
    is.finite(mz),
    is.finite(intensity)
  )

if (nrow(one_data) == 0) {
  stop("No usable data found for the selected file.")
}

p <- plot_ly()

for (i in 1:nrow(one_data)) {
  p <- p %>%
    add_segments(
      x = one_data$mz[i],
      xend = one_data$mz[i],
      y = 0,
      yend = one_data$intensity[i],
      text = paste(
        "File:", one_data[[file_column]][i],
        "<br>m/z:", one_data$mz[i],
        "<br>Intensity:", one_data$intensity[i]
      ),
      hoverinfo = "text",
      showlegend = FALSE
    )
}

p %>%
  layout(
    title = paste("Mass Spectrum -", selected_file),
    xaxis = list(title = "m/z"),
    yaxis = list(title = "Intensity")
  )


#without colours



# =========================
# SETTINGS - CHANGE THESE
# =========================
data <- cm_data1
file_col <- "source_file"
x_col <- "X(Thompsons)"
y_col <- "Y(Counts)"
selected_file <- "01-01.csv"
# =========================

one_data <- data %>%
  mutate(
    file_name = as.character(.data[[file_col]]),
    mz = as.numeric(.data[[x_col]]),
    intensity = as.numeric(.data[[y_col]])
  ) %>%
  filter(
    file_name == selected_file,
    !is.na(mz),
    !is.na(intensity),
    is.finite(mz),
    is.finite(intensity)
  ) %>%
  arrange(mz)

if (nrow(one_data) == 0) {
  stop("No usable data found for the selected file.")
}

stick_data <- data.frame(
  x = c(rbind(one_data$mz, one_data$mz, NA)),
  y = c(rbind(0, one_data$intensity, NA)),
  text = c(rbind(
    paste0(
      "File: ", one_data$file_name,
      "<br>m/z: ", one_data$mz,
      "<br>Intensity: 0"
    ),
    paste0(
      "File: ", one_data$file_name,
      "<br>m/z: ", one_data$mz,
      "<br>Intensity: ", one_data$intensity
    ),
    NA
  ))
)

plot_ly(
  data = stick_data,
  x = ~x,
  y = ~y,
  type = "scatter",
  mode = "lines",
  text = ~text,
  hoverinfo = "text"
) %>%
  layout(
    title = paste("Mass Spectrum -", selected_file),
    xaxis = list(title = "m/z"),
    yaxis = list(title = "Intensity")
  )



## For all graphs


# =========================
# SETTINGS - CHANGE THESE
# =========================
data <- cm_data1
file_col <- "source_file"
x_col <- "X(Thompsons)"
y_col <- "Y(Counts)"
# =========================

clean_data <- data %>%
  mutate(
    file_name = as.character(.data[[file_col]]),
    mz = as.numeric(.data[[x_col]]),
    intensity = as.numeric(.data[[y_col]])
  ) %>%
  filter(
    !is.na(file_name),
    !is.na(mz),
    !is.na(intensity),
    is.finite(mz),
    is.finite(intensity)
  ) %>%
  arrange(file_name, mz)

all_files <- unique(clean_data$file_name)

plot_list <- list()

for (file in all_files) {
  one_data <- clean_data %>%
    filter(file_name == file)

  if (nrow(one_data) > 0) {
    stick_data <- data.frame(
      x = c(rbind(one_data$mz, one_data$mz, NA)),
      y = c(rbind(0, one_data$intensity, NA)),
      text = c(rbind(
        paste0(
          "File: ", one_data$file_name,
          "<br>m/z: ", one_data$mz,
          "<br>Intensity: 0"
        ),
        paste0(
          "File: ", one_data$file_name,
          "<br>m/z: ", one_data$mz,
          "<br>Intensity: ", one_data$intensity
        ),
        NA
      ))
    )

    p <- plot_ly(
      data = stick_data,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      text = ~text,
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Mass Spectrum -", file),
        xaxis = list(title = "m/z"),
        yaxis = list(title = "Intensity")
      )

    plot_list[[file]] <- p
  }
}

htmltools::tagList(plot_list)










