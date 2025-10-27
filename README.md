[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![ShinyApp](https://img.shields.io/badge/ShinyApp-Launch-blue)](https://vandycardiac.shinyapps.io/RMSI_app)

## deltaMSI Calculator for Myocardial Stress (DCD)

This Shiny app estimates myocardial stress in controlled Donation after Circulatory Death (DCD) donors during withdrawal of life support.

The app calculates:
- **MSI (Myocardial Stress Index)**  
  MSI = RPP<sub>z</sub> − Perf<sub>z</sub>  
  - **RPP (rate-pressure product)** = HR × SBP  
  - **Perfusion** = DBP × (SaO₂ / 100) × diastolic_fraction  
    - diastolic_fraction is estimated from the cardiac cycle timing at the given heart rate  
  - RPP and Perfusion are each z-scored to reference means and standard deviations to produce RPP<sub>z</sub> and Perf<sub>z</sub>.

- **deltaMSI**  
  deltaMSI(t) = MSI(t) − MSI(baseline_at_withdrawal)  
  This measures how much additional myocardial stress develops over time relative to the donor’s own baseline at the start of withdrawal.

### Outputs shown in the app
For each run, the app:
- Plots **deltaMSI over time**
- Calculates **time above deltaMSI = 0.5**
- Calculates **time above deltaMSI = 1.0**
- Calculates **area under the curve above 0.5 and 1.0**
- Estimates the **early slope of deltaMSI in the first 10 minutes**
- Reports total **Withdrawal-to-Declaration time** (minutes)

Values 0.5 and 1.0 are treated as clinically relevant stress thresholds.

### Data input methods
You can provide physiologic data in two ways:

1. **Upload CSV**  
   Upload a .csv file with the following columns (1 row per minute):
   - `time`   (minutes from withdrawal start)
   - `HR`     (beats per minute)
   - `SBP`    (systolic blood pressure, mmHg)
   - `DBP`    (diastolic blood pressure, mmHg)
   - `SaO2`   (arterial oxygen saturation, %)

   The app will derive all downstream metrics automatically.

2. **Manual Entry mode**  
   Enter HR / SBP / DBP / SaO₂ for each minute as it occurs.  
   The app tracks elapsed time for you and marks declaration.

### Live App
Launch the current deployed version here:  
👉 https://vandycardiac.shinyapps.io/RMSI_app

This public instance runs the same code in `app.R` in this repository.


