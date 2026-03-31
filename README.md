# InZone
Umpire grading application that creates reports based on ball/strike call accuracy
This Project Definition outlines the roadmap for InZone Analytics, a specialized tool designed to bridge the gap between raw tracking data and actionable umpire performance evaluations.

# Project Goal
The primary objective is to develop a high-fidelity reporting engine that transforms raw CSV data from FlightScope and Trackman into professional Umpire Scorecards. The project will move from a functional R/Shiny prototype to a scalable, multi-tenant SaaS platform targeting collegiate and professional baseball organizations.

# Functional Requirements

Phase 1: Prototype (The Core Engine)
Data Ingestion: Support for .csv file uploads with automatic detection of FlightScope vs. Trackman column headers.
Data Processing: Logic to filter "Taken Pitches" and categorize them into four buckets based on plate coordinates (x,z) and the umpire's call.
Visualization: Generation of three distinct ggplot2 scatterplots:
False Positives: Balls incorrectly called strikes.
False Negatives: Strikes incorrectly called balls.
Overall Map: All taken pitches color-coded by accuracy.
Reporting: A "Download PDF" feature that compiles the plots and a summary statistics table (Accuracy %, Consistency, Zone Tendencies).

Phase 2: Production (The SaaS Transition)
Authentication: Secure login for teams, scouts, and conference officials.
Data Persistence: A database to store game history, allowing for "Season-Long" umpire leaderboards.
Branding: Customizable PDF templates for different clients (e.g., adding a specific university's logo).
Advanced Analytics: Heatmaps showing "Miss Consistency" and probability-based "Shadow Zone" scoring.

# Technical Architecture

Phase 1: Monolithic R/Shiny
The prototype will follow a standard reactive architecture where the user interface and data processing live within the same R environment.
UI: shiny and bslib for a clean dashboard look.
Server: tidyverse for data manipulation and ggplot2 for rendering.
Export: rmarkdown or pagedown to knit the dashboard state into a PDF.

Phase 2: Decoupled SaaS
To support multiple users and data persistence, the architecture will shift to a modern web stack.
Frontend: Next.js for a lightning-fast, SEO-friendly user interface.
Backend: FastAPI (Python) to handle heavy data processing asynchronously.
Storage: PostgreSQL for relational data and AWS S3 for storing generated PDF reports.

# Tech stack Comparison

Phase 1 (Prototype)

Language: R
Framework: Shiny
Styling: Boostrap (via Shiny)
Database: NA
Visuals: ggplot2
Deployment: shinyapps.io

Phase 2 (Production)

Language: JS/Python
Framework: Next.js / FastAPI
Styling: CSS
Database: PostgreSQL
Visuals: Matplotlib / Plotly
Deployment: Vercel / AWS / Docker (not sure yet)
