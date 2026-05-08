# Analysis of Weather, Time and Geographic Impacts on Canada's Air Quality (AQHI)
**Independent Project | BTMA 431: Gathering, Wrangling and Analyzing Data in R**

## 📌 Project Overview
As an international student in Canada, I observed significant fluctuations in air quality compared to my home country, Vietnam. This project explores the factors influencing the **Air Quality Health Index (AQHI)** across five major Canadian cities: Calgary, Ottawa, Toronto, Vancouver, and Victoria. 

The study investigates how meteorological variables (temperature, humidity, wind speed), geographic locations (Coastal vs. Inland), and temporal patterns (Time of day, Weekdays vs. Weekends) impact air health risks.

## 🛠 Tech Stack
- **Language:** R
- **Data Sourcing:** Open-Meteo API (Historical weather data) and Government of Canada (AQHI data).
- **Libraries:** - `tidyverse` (Data cleaning & manipulation)
  - `ggplot2` (Advanced data visualization)
  - `jsonlite` (API data parsing)
  - `broom` & `stats` (Linear regression & T-testing)

## 📊 Key Research Questions
1. **Weather Impact:** How do temperature, humidity, and wind speed correlate with AQHI levels?
2. **Geographic Factor:** Do inland cities experience worse air quality than coastal cities?
3. **Temporal Patterns:** Does air quality significantly differ between weekdays and weekends?

## 📈 Featured Visualizations & Insights

### 1. Correlation Heatmap
The analysis revealed that **Temperature** has a positive correlation with AQHI, while **Relative Humidity** shows a strong negative correlation. This suggests that warmer, drier conditions often lead to higher health risks related to air quality.

### 2. Coastal vs. Inland Analysis
Using T-tests and boxplots, the project confirms that **Inland cities** (like Calgary and Ottawa) have statistically higher AQHI means compared to **Coastal cities** (Vancouver and Victoria). This is likely due to maritime winds helping disperse pollutants in coastal regions.

### 3. Diurnal Patterns
Line charts indicate that AQHI levels typically peak during the **Morning** and **Afternoon** periods, coinciding with peak traffic hours and solar radiation which facilitates ground-level ozone formation.

## 📂 Project Structure
- `/data`: Contains raw and cleaned datasets (CSV format).
- `/scripts`: R scripts for API data retrieval, cleaning, and statistical modeling.
- `Final_Project_Report.pdf`: Comprehensive documentation of methodology and findings.

## 🎓 Author
**Hieu Dinh** Bachelor of Commerce, Business Analytics Major  
Haskayne School of Business, University of Calgary
