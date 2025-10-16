
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eventreport <a href="#"><img src="man/figures/logo.png" align="right" height="200" style="margin-left:15px; margin-bottom:5px;" /></a>

<!-- badges: start -->

<!-- badges: end -->

The goal of the `eventreport` package is to *diagnose*, *visualize*, and
*aggregate* event report level data to the event level. Users provide an
event report level dataset, specify their aggregation rules, and the
package produces a dataset aggregated at the event level. The package
also allows the user to diagnose how sensitive their event report level
data is to aggregation choices. In addition, the package includes the
Modes and Agents of Election-Related Violence in Côte d’Ivoire and Kenya
(MAVERICK) dataset, an event report level dataset that records all
documented instances of electoral violence from the first multiparty
election to 2022 in Côte d’Ivoire (1995-2022) and Kenya (1992-2022).

When using the data, please refer to the following article and codebook:

> Sebastian van Baalen & Kristine Höglund (2026) Introducing the Modes
> and Agents of Election-Related Violence in Côte d’Ivoire (MAVERICK)
> datset. *Journal of Peace Research*, online first.
>
> Sebastian van Baalen, David Edberg Landeström, Tor Richardson-Golinski
> & Kristine Höglund (2025) The MAVERICK Dataset Codebook Version 1.0.
> Uppsala: Department of Peace and Conflict Research, Uppsala
> University.

For methodological details, and when using the package, please refer to
the following working paper:

> Sebastian van Baalen & Kristine Höglund (2025) Trials and
> Triangulations: Analyzing Aggregation Sensitivity in Event Data on
> Political Violence. Uppsala: Department of Peace and Conflict
> Research, Uppsala University.

## Installation

Once on CRAN, you can install the released version of **eventreport**
from [CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("eventreport")
```

You can also install the development version of `eventreport` from
GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("sebastianvanbaalen/eventreport")
```

## What is event report level data?

Event report level data refers to data where each observation is an
event that takes place on a single day and in a particular location *as
reported in a single source*. The report level means that multiple
reports about the same event constitute separate observations. For
example, if both BBC and Reuters report about a violent post-election
demonstration, the demonstration is the event, whereas the BBC and
Reuters reports constitute the *event reports*. For a solid primer on
event report level data, see this
[introduction](https://doi.org/10.1177/0022343314523807) to the method
by Nils B Weidmann and Espen Geelmuyden Rød and [this in-depth
exploration](https://doi.org/10.1111/ajps.12398) of aggregation
sensitivity by Scott J Cook and Nils B Weidmann.

The table below provides an example of event report level data from the
MAVERICK dataset, and lists six unique reports about a single electoral
violence event.

| event_id | city | location | actor1 | actor1_type | deaths_best | source |
|----|----|----|----|----|----|----|
| CIV-0004 | Abidjan | Abobo | Unknown security force (Côte d’Ivoire) | Security forces | 5 | Amnesty International (All Africa) (2011-01-12) Fresh Violence Erupts as Armed Groups Clash |
| CIV-0004 | Abidjan | Abobo | Unknown security force (Côte d’Ivoire) | Security forces | 1 | LEJD (2011-01-12) Nouveaux affrontements en Côte d’Ivoire |
| CIV-0004 | Abidjan |  | Unknown security force (Côte d’Ivoire) | Security forces | 5 | Reuters (2011-01-12) More die in Cote d’Ivoire violence |
| CIV-0004 | Abidjan | Abobo | Police (Côte d’Ivoire) | Security forces | 6 | Xinhua News Agency (2011-01-12) Côte d’Ivoire : au total six policiers tués dans un quartier pro Ouattara à Abidjan |
| CIV-0004 | Abidjan |  | Police (Côte d’Ivoire) | Security forces | 6 | Al Jazeera (2011-01-13) Tensions persist in Cote d’Ivoire |
| CIV-0004 | Abidjan | Abobo | Unknown actor (Côte d’Ivoire) |  | 7 | The Times (2011-01-15) Coup fears as death toll rises |

## Why the `eventreport` package?

`R` already contains some functions that can be used for aggregating
event report level data to the event level, such as the `mean` and
`median` base `R` calls. However, as we detail in the package
introduction article, the aggregation of event reports often demands
additional functionalities, such as the use of tie-break rules or
information contained in meta variables.

The `eventreport` package adds several functionalities not contained in
existing software. Among those benefits, the package:

- **Handles different variable classes:** `eventreport` handles a range
  of different variables, including character, date, numeric, and binary
  numeric variables. This feature makes the package ideal for working
  with event report datasets that include different variable classes.

- **Enables tie-breaking rules:** many vectors are multi-modal, meaning
  that simple functions for identifying the most frequent values will
  yield multiple results. `eventreport` therefore enables users to
  specify up to two tie-breaking rules that help adjudicate between
  multiple modes variables.

- **Integrates precision scores:** sometimes researchers are interested
  in recording the most precise value, such as more precise location
  estimates or more precise actor names. `eventreport` allows users to
  specify precision score variables that help prioritize what values to
  select when the values cannot be ranked.

- **Provides simple functions:** aggregating event report level data is
  a complex coding project. `eventreport` makes this procedure more
  straightforward by providing simple functions that carry out complex
  tasks. All functions were developed in the context of a concrete event
  report level data collection effort, and are therefore both
  needs-based and well-tested.

- **Allows easy customization:** the combination of simple functions and
  several convenience functions allows users to stipulate a range of
  complex aggregation rule sets with minimal coding. Moreover, because
  `eventreport` is `tidyverse` compatible, users can integrate the
  package functions in a tidy workflow.

## Examples

We provide a host of examples in our vignette and in the MAVERICK
dataset codebook. Below are three basic examples of the functionalities
in the `eventreport` package.

For aggregation diagnostics, users can use `mean_dscore` to visualize
the mean normalized divergence score per variable (the mean number of
divergent values per event divided by the total number of unique values
in each variable). This diagnostic helps users assess what and to what
extent variables are sensitive to aggregation choices. Simply run:

``` r
mean_dscore(
  small_maverick_event_report,
  group_var = "event_id",
  variables = c("country", "actor1", "deaths_best", "injuries_best"),
  normalize = TRUE,
  plot = TRUE
)
```

<img src="man/figures/logo.pngunnamed-chunk-3-1.png" width="70%" />

For aggregating data, users can use `calc_mode` to find the mode value
using two different tie-breaking rules:

``` r
calc_mode(
  c("Sweden", "Sweden", "Denmark", "Denmark"),
  tie_break = c(1, 1, 1, 1),
  second_tie_break = c(1, 4, 1, 1)
)
#> [1] "Sweden"
```

For aggregating entire dataframes, users can use `aggregateData` to
stipulate a set of aggregation rules and aggregate the full dataset
(here presented using the `tidytable` package):

``` r
output <- small_maverick_event_report %>% 
  aggregateData(
    group_var = "event_id",
    find_mode = "city"
) %>% 
utils::head(10)

tinytable::tt(output)
```

| event_id | city         | number_of_sources | unit_of_analysis |
|----------|--------------|-------------------|------------------|
| CIV-0001 | Duékoué      | 5                 | Event            |
| CIV-0002 |              | 2                 | Event            |
| CIV-0003 | Abidjan      | 12                | Event            |
| CIV-0004 | Abidjan      | 6                 | Event            |
| CIV-0008 | Man          | 1                 | Event            |
| CIV-0009 | Vavoua       | 2                 | Event            |
| CIV-0010 | Abidjan      | 1                 | Event            |
| CIV-0011 | Yamoussoukro | 1                 | Event            |
| CIV-0012 | Gagnoa       | 4                 | Event            |
| CIV-0013 | Daloa        | 4                 | Event            |
