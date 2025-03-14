# rPeaks-Tachibana v0.1.0
A Shiny application for interactive **peak analysis and fitting**, featuring advanced baseline correction, peak detection (both manual and automatic), and comprehensive fit summary generation.

(This app is a part of the **WakaCitrus-Informatics** series. For other apps in the **WakaCitrus-Informatics** series, please visit: [https://github.com/yin4dev?tab=repositories](https://github.com/yin4dev?tab=repositories))

Welcome to **rPeaks-Tachibana v0.1.0**! This open-source project is designed to offer a user-friendly interface for spectral data analysis—enabling users to perform blank correction, choose from multiple baseline correction methods (including manual point selection), detect peaks with both automatic and manual approaches, and fit composite models using various peak shapes (gauss, lorentz, voigt, mixed, and lognormal). Contributions, improvements, and customizations are welcome—feel free to fork and submit pull requests.

---

## Table of Contents
1. [Overview](#overview)
2. [Key Features](#key-features)
3. [Dependencies](#dependencies)
4. [Installation and Setup](#installation-and-setup)
   - [Required Tools](#required-tools)
5. [Usage](#usage)
   - [Running the Application](#running-the-application)
   - [Feature Overview](#feature-overview)
6. [Configuration](#configuration)
7. [Special Thanks](#special-thanks)
8. [License](#license)

---

## Overview
**rPeaks-Tachibana v0.1.0** is an R Shiny application tailored for interactive peak analysis and model fitting of spectral data. The app supports:
- **Blank Correction:** Combine or subtract blank data from original spectra.
- **Baseline Correction:** Choose from constant, linear, quadratic, cubic, or manual baseline correction methods.
- **Peak Detection:** Utilize automatic detection (with derivative-based sensitivity and Savitzky–Golay filtering) or manual peak picking directly from the interactive plot.
- **Peak Fitting:** Fit composite models with a choice of peak shapes (gauss, lorentz, voigt, mixed, lognormal) and display individual peaks and residuals.
- **Result Export:** Download detailed fit summaries and the combined fitted data for further analysis.

---

## Key Features
- **Data Upload & Blank Correction:** Easily upload your original and blank CSV files and apply customizable blank corrections.
- **Flexible Baseline Correction:** Select a baseline correction method or manually pick baseline points on the plot.
- **Advanced Peak Detection:** Choose between manual and automatic peak picking modes, with adjustable Savitzky–Golay filter settings and sensitivity levels.
- **Multiple Peak Models:** Fit peaks using several models (gauss, lorentz, voigt, mixed, lognormal) with customizable parameters and constraints.
- **Interactive Visualization:** Explore the original spectrum, fitted curve, residuals, and derivative plots via interactive Plotly graphics.
- **Comprehensive Fit Summary:** Generate detailed textual and tabular summaries of the fitted peaks, including peak areas and baseline parameters, with download options (CSV and TXT).

---

## Dependencies
- **Programming Language/Framework:** R, Shiny
- **Libraries/Packages:**  
  `shiny`, `minpack.lm`, `DT`, `dplyr`, `shinyBS`, `ggplot2`, `plotly`, `signal`, `pracma`
- **System Tools:**  
  - [R](https://www.r-project.org/)
  - [RStudio](https://www.rstudio.com/)

---

## Installation and Setup

### Required Tools
1. **Clone or Download the Repository**  
   Clone this repository or download the source files.

2. **Install External Tools/Services**  
   Ensure you have R installed along with an IDE such as RStudio.

3. **Install Required Packages**  
   Install the necessary packages by running:
   ```r
   install.packages(c("shiny", "minpack.lm", "DT", "dplyr", "shinyBS", "ggplot2", "plotly", "signal", "pracma"))
   ```
   Alternatively, install packages using your preferred package manager.

---

## Usage

### Running the Application
Launch the application by opening the project in RStudio and running:
```r
library(shiny)
runApp("path_to_your_app_directory")
```
Alternatively, from the command line navigate to the app’s directory and run:
```r
R -e "shiny::runApp('.')"
```

### Feature Overview
- **Data Upload & Blank Correction:**  
  - Upload your original spectral CSV file and, if needed, a blank CSV file.
  - Select the appropriate columns and apply the blank correction (using operations such as addition, subtraction, multiplication, or division).

- **Baseline Correction:**  
  - Choose a baseline type (constant, linear, quadratic, cubic, or manual).
  - If using manual baseline correction, pick baseline points interactively on the plot.

- **Peak Detection & Fitting:**  
  - Use automatic peak detection with configurable sensitivity (via derivative order and Savitzky–Golay parameters) or manually pick peaks.
  - Set peak parameter limits (center, height, and width) if desired.
  - Fit the composite model to the data and visualize the original spectrum, fitted curve, individual peaks, residuals, and derivative.

- **Result Export:**  
  - Download the combined CSV file containing original, baseline, fitted data, residuals, and individual peak contributions.
  - Export a TXT file summarizing peak parameters, areas, and baseline settings.

---

## Configuration
Customize various settings directly within the Shiny interface:
- **Baseline Parameters:** Manually set or adjust parameters for the chosen baseline model.
- **Peak Detection Sensitivity:** Modify Savitzky–Golay window size, polynomial order, and sensitivity levels.
- **Peak Model Settings:** Select the peak shape (gauss, lorentz, voigt, mixed, lognormal) and adjust peak parameter limits and sign settings.
- **Blank Correction Factors:** Define the factors and operation (addition, subtraction, etc.) for blank correction.

---

## Special Thanks
A huge thank you to my loving Aimi Yago for her invaluable understanding, support, and inspiration in making this project better! 🎉

---

## License
This project is licensed under the GNU General Public License v3.0 (GPL-3.0).

Copyright (C) 2025 Hongrong Yin

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
