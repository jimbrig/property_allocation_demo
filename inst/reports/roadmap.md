# ROADMAP

## Milestone: Enhanced User Interface Experience 

Per the presentation with Ron and Team:

- The app is nice but could use some visuals to enhance the user's experience
-  Provide insights about data

+ Deadline: Friday September 5, 2020

Visuals to add:

- SOV Tab:
  - Doughnut Chart with TIV by:
     - a) BU
     - b) Region
     - c) Country
  - Value Boxes for:
     - Total TIV
     - Total Records/Units
  - Map of World with TIV heat map by region/country

Loss Data:

- Per Adam: Can try and mimic [FSRM KPI App]() 's chart functionality with counts, total paid/incurred, TIV (exposures), frequency, severity, loss cost/rate across years in experience period and split out by demographics (region, country, state, etc.)

- Trend Chart:
  - Counts by year
  - Severity by year

- Rates Tab:
  - BubbleChart
     - able to be filtered by coverage with the following:
     - X-axis: Rate Identifier
     - Y-axis: Rate
     - Bubbles: Prior Rate, Market Rate and Model Rate
     - Bubble Size: TIV in each bucket

- Count Surcharges:
  - Bar Chart
     - X-axis: Count Bucket
     - Y-axis #1: Loss amounts
     - Y-axis #2: Count amounts
     - Y-axis #3: Resulting surcharge per bucket?
  - Histogram of Claims

***

INSIGHTS tab for Allocation

*** 

Backlog:

- We need to address comparison to prior on the Details tab in the allocation section - as shown, the %change is for the premium before accounting for manual adjustments and risk engineering edits. We want to show the %change after manual adjustments and risk engineering on the Details tab. 
  - Move net change before manual changes to allocation tab
  - Change all other % changes to be on rates, and specify headers 

- Delete entity functionality on SOV
- Consistent Editable Tables
- Consistent Formatting and functions (remove decimal places for dollars, 2 for percent, 3 for TIV usage)
- Readjust data/dictionary for replacing entity with unit/location
- Visual divide between prior, market, and budget guidance on details tab in Allocation section
- Move Prior Premium to Allocation tab
- Create “Insights” tab
