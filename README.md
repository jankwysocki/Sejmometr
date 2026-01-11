# Sejmometr Dashboard

An interactive R Shiny dashboard visualizing the activity and estimated costs of Polish MPs (Sejm deputies) for the current term.

## Overview

This project scrapes data from the official [Sejm API](https://api.sejm.gov.pl/) to provide transparency regarding parliamentary work. It calculates activity metrics and correlates them with estimated earnings to highlight the "Cost per Vote" for taxpayers.

**Live Demo:** [Sejmometr at Posit Connect Cloud](https://019baef9-e928-cbdd-08b7-5f93e1940feb.share.connect.posit.cloud/)

## Features

* **Activity Tracking:** Visualizes attendance rates, missed votes, and total votings.
* **Cost Analysis:** Estimates "Cost per Vote" based on parliamentary salaries, allowances, and committee bonuses.
* **Interactive Charts:**
    * **Scatter Plot:** Correlates Age vs. Attendance with hover tooltips (Plotly).
    * **Bar Chart:** Ranks political clubs by average attendance.
* **Search & Filter:** Filter by political club, attendance range, or search by name.

## Tech Stack

* **Language:** R
* **Framework:** Shiny, Shinydashboard
* **Data Processing:** `dplyr`, `purrr`, `lubridate`, `jsonlite`
* **Visualization:** `ggplot2`, `plotly`, `DT`

## Disclaimer

Salary figures are **estimates** based on public regulations (Base Salary + Parliamentary Allowance + Committee Bonuses). They do not include additional benefits like housing allowances or office lump sums. For exact figures, refer to official MP asset declarations.