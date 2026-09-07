# HIL-PH 360-Degree Leadership Assessment Dashboard

🚀 **[Access the Live Interactive App Here](https://ahlam912.github.io/HIL-PH-360-Dashboard./)**

## Overview
The **HIL-PH 360° Assessment Portal** is a web-based health informatics platform built using **R Shiny** and deployed via **Shinylive** (serverless WebAssembly). Designed for public health organizations and Monitoring & Evaluation (M&E) teams, this application converts raw multi-evaluator feedback into real-time visual intelligence.

## Key Features
* **Automated Data Pipeline:** Direct ingestion and transformation of survey data via the **KoboToolbox REST API**.
* **360-Degree Competency Analysis:** Interactive radar charts evaluating leadership domain performance across self and peer scores.
* **Perception Gap Analytics:** Statistical breakdown comparing self-assessments against evaluator group averages to highlight developmental priorities.
* **Automated Reporting Workflow:** Built-in email summary routing and dynamic dataset filtering (`dplyr`, `tidyr`).
* **Zero-Server Client Deployment:** Compiled with `shinylive` to run entirely within the user's web browser without requiring a backend R server.

## Tech Stack
* **Language & Framework:** R, R Shiny, Shinylive
* **Data Sources & APIs:** KoboToolbox REST API, JSON parsing (`jsonlite`, `httr`)
* **Visualization:** Plotly, DT (Interactive DataTables), shinydashboard
