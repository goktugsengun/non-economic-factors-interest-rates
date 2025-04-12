Non-Economic Factors on Interest Rates
This repository contains an RMarkdown file that analyzes the influence of non-economic factors on interest rates. The analysis includes the exploration of various social, political, and environmental factors that may impact interest rates beyond the traditional economic indicators.

Repository Structure
non-economic-factors-on-interest-rates.Rmd: This is the main RMarkdown file that contains the full analysis, code, and visualizations.

DATA: The “Census Tract File” contains mortgage-level data on all single-family (1-4 unit) properties.
Note: Fields are separated by one blank space. Beginning with the 2018 release, additional fields 40-57 are sourced from FHFA and based on available Enterprise data unless
otherwise indicated, as described in 85 Fed. Reg. 34196 (June 3, 2020), see https://www.govinfo.gov/content/pkg/FR-2020-06-03/pdf/2020-11819.pdf.

Required Packages
To run this analysis, you'll need the following R packages. If you don't have them installed, you can do so by running the following command in your R console:

install.packages(c("ggplot2", "dplyr", "tidyr", "lubridate", "zoo", "plotly", "tseries"))
If you're using RStudio, it will automatically handle package installation from CRAN.

How to Run
Prerequisites:
Install R and RStudio (if you haven't already).

Once the RMarkdown file is open in RStudio, click the "Knit" button at the top of the document to generate the analysis in HTML or PDF format (you may need to install rmarkdown and knitr packages if you don't have them already).

Running Code:
You can also run individual code chunks in the RMarkdown file directly by selecting them and pressing Ctrl + Enter (Windows) or Cmd + Enter (Mac).

Contribution
Feel free to fork this repository, create an issue, or submit a pull request if you would like to contribute. Any improvements or suggestions are welcome!

License
This project is licensed under the MIT License - see the LICENSE file for details.

