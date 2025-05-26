Repository supporting the publication on deploying and maintaining eNose models through a Shiny app. This app allows users to interact with trained eNose models for sample classification and analysis through an intuitive web interface.

Installation
1. Clone the repository
2. Open app.R in RStudio
3. Run shiny::runApp() in the R console

## Maintenance Policy
This application follows a scheduled maintenance plan:
- Version updates: Anual releases
- Model retraining: Triggered when either:
  1. >100 new samples available
  2. Accuracy drops below 90%

## How to Report Issues
Please submit bug reports to: joseluis.perezcalle@uca.es
Include in your report:
- App version (visible in footer)
- Browser/OS information
- Steps to reproduce the issue
