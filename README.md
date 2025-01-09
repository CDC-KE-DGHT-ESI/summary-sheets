# Kenya 2024 Bio-Behavioral Survey Data Visualization and Summary Sheets

## Overview

This R project is designed for visualizing and reporting data from the **2024 Bio-Behavioral Survey** conducted in 9 counties in Kenya. It provides tools to generate insights and create publication-ready charts and reports to support evidence-based decision-making.

## Features

- **Data Exploration**: Tools for exploring survey data across demographic and behavioral variables.
- **Advanced Visualizations**: Create high-quality visualizations using `ggplot2`, `plotly`, and other visualization libraries.
- **Custom Reports**: Generate tailored reports in HTML, PDF, and Word formats for stakeholders.
- **Reproducible Workflows**: Standardized scripts ensure consistency and reproducibility.
- **County-Level Insights**: Focused visualizations for each of the 9 counties surveyed.

## Prerequisites

Ensure the following are installed on your system:

- **R**: Version 4.0.0 or higher.
- **RStudio**: Optional but recommended.
- **Required R Packages**: See installation instructions below.

### Required Datasets
The project requires the 2024 Bio-Behavioral Survey data. Ensure the data files are placed in the `data/` directory in `.csv` or `.xlsx` format.

## Installation

Clone the repository to your local machine:

```bash
git clone https://github.com/CDC-KE-DGHT-ESI/bbs-summary-sheets.git
cd bbs-summary-sheets
```

## Running the code
- To run the code, you need the data folder. This can be shared upon request.
- Open *summary-sheets.qmd* and click render to generate the summary sheet for the specified in the YAML header. By default this is Nairobi.
- Use *render_reports.R* to generate summary sheets for all counties. You also specify the specific report in this code.


