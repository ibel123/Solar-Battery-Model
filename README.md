# Solar Battery Savings Model

An hourly model that tests whether a home battery is worth buying for a household with rooftop solar panels. It uses one year (2020) of hourly solar generation and electricity usage, simulates the battery hour by hour, and projects the saving over the battery's 20-year life under two electricity price scenarios.

**Author:** Ibaale Eric, Kampala, Uganda
**Language:** R (R Markdown)

---

## The question

Naomi has solar panels on her roof. Any electricity they make above what the house is using at that moment is wasted, because she gets nothing for it. She is considering a battery that would store the extra and give it back later, for example in the evening.

- Battery cost: $7,000, installed on 1 January 2022
- Capacity: 12.5 kWh (it cannot go below 0 or above 12.5)
- Working life: 20 years
- Electricity price: $0.17 per kWh from 1 January 2022, with annual price inflation from 1 January 2023
- Discount rate: 6% a year

The model answers three things:

1. How many extra kWh of Naomi's usage could be met with the battery compared with the panels alone, and what is that worth in dollars?
2. What is the NPV of the battery under each price scenario?
3. What is the IRR under each price scenario?

| Scenario | Electricity price inflation |
|---|---|
| 1 | 4% every year (the government estimate) |
| 2 | 4% from 1 Jan 2023, then rising by 0.25 percentage points a year (4.25%, 4.50%, ...), as estimated by Naomi |

---

## What is in this repository

```
.
├── Solar_Battery_Model.Rmd        R Markdown source: code, explanation and results in one file
├── Solar_Battery_Model.html       The knitted report
├── Solar_Battery_Model.R          The same code as a plain R script
├── reference.docx                 Word style template (only needed to knit to Word)
└── README.md
```

Files created when the code runs: `hourly_model_results.csv` (the full hourly results) and, if you run the .R script, seven png figures.

**Reading the report online:** GitHub shows .html files as plain code, not as a web page. To view the report as a page, switch on GitHub Pages (Settings > Pages > Deploy from a branch > main, root folder) and open
`https://<ibel123>.github.io/<Solar-Battery-Model>/index.html`
Renaming the file to `index.html` makes the shorter address work.

---

## The data

The Excel file has a sheet called "Raw Data" with one row per hour for 2020 (24 hours x 365 days = 8,760 rows; 29 February is left out):

| Column | Meaning |
|---|---|
| Hour | Hour of the day (0 = midnight to 1 am, 23 = 11 pm to midnight) |
| Date/hour start | Start of the hour, stored by Excel as a serial number |
| Solar electricity generation (kWh) | Electricity made by the panels in that hour |
| Electricity usage (kWh) | Electricity used in the house in that hour |

### Data problems found and what I did

I checked the data before using it. The report lists every check and its result. In summary:

| Problem | Action |
|---|---|
| Negative usage readings | Treated as errors and replaced |
| An impossible usage reading (tens of thousands of kWh in one hour) | Treated as an error and replaced |
| Daylight hours where solar and usage were both exactly zero | Treated as meter dropouts and replaced |
| Usage readings above 20 kWh in one hour | Flagged but kept, because a house can use that much in an hour; a sensitivity test shows what happens if they are removed |
| Tiny non-zero solar readings at night | Left alone (sensor noise, no practical effect) |

Replacement rule: the median for the same hour of the day, taken from the rows that were not flagged.

---

## How the model works

For every hour, in time order, starting with an empty battery:

1. **Bought with no battery** = usage - solar (never below zero)
2. **Excess solar** = solar - usage (never below zero)
3. **Battery charge:** excess solar goes into the battery, up to the room left (12.5 kWh minus the current level). Anything that does not fit is lost.
4. **Battery discharge:** the battery covers the shortfall from step 1, but can only give out what it holds.
5. **Bought with the battery** = bought with no battery - discharge
6. **Battery level** = old level + charge - discharge

Then:

- **Saving over 2020** = extra kWh met by the battery x $0.17 (2022 price, no discounting)
- **Projection:** the same extra kWh is assumed every year from 2022 to 2041, and each year's saving is that number of kWh x that year's price
- **NPV:** each year's saving is discounted at 6% (savings arrive at the end of each year), added up, and the $7,000 cost is taken off. The report shows the present value of the savings as well as the NPV after the cost.
- **IRR:** the discount rate at which the present value of the 20 years of savings equals $7,000, found with R's `uniroot`

---

## Results

The full-year results are in the report. Key figures:

| Measure | Scenario 1 | Scenario 2 |
|---|---|---|
| Extra electricity met by the battery in 2020 (kWh) | _fill in_ | _fill in_ |
| Saving over 2020 at 2022 prices ($) | _fill in_ | _fill in_ |
| Present value of 20 years of savings at 6% ($) | _fill in_ | _fill in_ |
| NPV after the $7,000 cost ($) | _fill in_ | _fill in_ |
| IRR | _fill in_ | _fill in_ |

_(The extra kWh and the 2020 saving are the same in both scenarios; only the price path differs.)_

---

## Checks built into the model

- The model runs 16 self-checks every time it is run. Examples: the battery never goes below 0 or above 12.5 kWh; every hour balances (usage = solar used + battery + bought); energy charged equals energy discharged plus what is left in the battery; monthly totals add up to annual totals; discounting 20 years of 1 at 6% matches the annuity formula; the present value at the IRR equals the battery cost.
- One of the checks recalculates the battery level with a different formula and compares it with the main loop.
- The report also includes reasonableness checks and a sensitivity table (removing the large usage readings, and receiving savings mid-year instead of year-end).

---

## Assumptions and limits

- 2020 is treated as a typical year and repeated for all 20 years, so the battery saves the same kWh each year and only the price changes.
- The battery has no charging or discharging losses, no limit on charge or discharge speed, and no loss of capacity over 20 years. This makes the savings look better than a real battery would give.
- Solar generated while the battery is full is lost.
- No maintenance, replacement, tax or panel degradation is included.
- Replacement values for bad readings are estimates, not the true readings.

The full list and an evaluation of the method are in the report.

---

## How to run it

### What you need

- R (a recent version) and RStudio
- R packages: `readxl`, `rmarkdown`, `knitr`

```r
install.packages(c("readxl", "rmarkdown", "knitr"))
```

### Steps

1. Download or clone the repository and keep all the files in one folder. The Excel file must stay in the same folder as the code, because it is read by name and there are no file paths in the code.
2. Open `Solar_Battery_Model.Rmd` in RStudio.
3. Knit it.
   - The file is set up for Word output, which uses `reference.docx` for Times New Roman 12 and 1.5 line spacing.
   - For an HTML report instead, run:
     ```r
     rmarkdown::render("Solar_Battery_Model.Rmd", output_format = "html_document")
     ```
4. To run the code without knitting, open `Solar_Battery_Model.R`, set the working directory to the folder (Session > Set Working Directory > To Source File Location) and run the whole script. It prints the tables, saves the figures and ends with a short summary of the main results.

All the numbers in the report are calculated from the Excel file when it is knitted; nothing needs to be typed in or changed to see both scenarios.

---

## Data source and notice

The input data and the scenario come from an analyst assessment exercise. The data is not mine to licence, so it is not covered by any licence on this repository. If you reuse the model, use your own data.
