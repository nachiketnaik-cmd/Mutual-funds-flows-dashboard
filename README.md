# Mutual Funds Flows Dashboard

An end-to-end analysis of **Indian mutual fund flows**, built using **R**. It studies how investor money moves across fund categories (equity, debt, hybrid, gold ETFs, index funds and others).

The data extraction, cleaning and statistical analysis are all written in **R**. The cleaned results are then visualised in Power BI and Tableau.

## Data
- **Source:** [AMFI](https://www.amfiindia.com/) (Association of Mutual Funds in India), monthly industry reports. AMFI is the industry body; SEBI is the regulator.
- **Period:** October 2023 to September 2026 (36 months).
- **Contents:** monthly inflows, outflows and net flows by scheme category, plus AUM and SIP data.

## Research questions
- Which fund categories are gaining or losing money, and is the shift steady or sudden?
- Do equity flows follow market returns (Nifty 50), i.e. is there **return chasing**?
- Do gold fund flows rise when equity markets fall (**flight to safety**)?
- How stable are SIP flows compared with lump-sum flows?

## Methods (all in R)
1. **Descriptive analysis:** summary statistics, category shares, growth rates, correlations.
2. **Hypothesis tests:** z-tests and t-tests on mean flows; tests of proportions.
3. **OLS regression by matrix algebra:** coefficients computed directly as `(X'X)^-1 X'y` with base R matrix operations, then checked against `lm()`.
4. **Regression with `lm()`:** trend models, return chasing and flight-to-safety models, read through `summary()` (coefficients, standard errors, p-values, R-squared).
5. **Time-series analysis:** monthly `ts` objects, trend and seasonal decomposition (`decompose()` / `stl()`), and autocorrelation checks (`acf()`, `Box.test()`).

## Project structure
```
data/    raw Excel files downloaded from AMFI
code/    R scripts for extraction, cleaning and analysis
```

## Tools
- **R** (4.6.1) with RStudio: extraction, cleaning, analysis
- **Power BI** and **Tableau**: dashboards

## Status
Work in progress.
