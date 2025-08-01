[![Lifecycle:Stable](https://img.shields.io/badge/Lifecycle-Stable-97ca00))](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-stable.md)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# BC Demographic Survey Weighting
---

This repo consists of code to produce standardized weights for the BC Demographic Survey.

In 2023, BC Stats conducted the BC Demographic Survey. More than 200,000 people responded to the 
voluntary survey, providing information about many aspects of their identity (such as race, ethnicity, 
ancestry, gender and many others). The BC Demographic Survey data is available through the 
Data Innovation Program (DIP), which securely links and de-identifies data from multiple ministries, 
organizations or agencies in a secure platform.

At the time, the BC population was just over five million people; therefore, while there are more 
than 200,000 respondents in the BCDS dataset, they represent less than 5% of the population. 
This code helps creates base weights so that the survey data represents the BC population by 
Men+ and Women+ in the age groups 0 to 14 years, 15 to 64 years and 65 years and over. Any analysis 
using these weights should incorporate additional weights relevant to that analysis where possible.

For more details on the approach to the BC Demographic Survey, see the [2024 technical report](https://www2.gov.bc.ca/assets/gov/british-columbians-our-governments/multiculturalism-anti-racism/anti-racism/anti-racism-hub/anti-racism-stats-and-research/2024-research-release/bc-demographic-survey-report.pdf).

For other research that has been done using the survey, see the [anti-racism website](https://antiracism.gov.bc.ca/?page_id=34752).

## Usage

* To create a set of base weights to align with the BC population by gender and age group.

## Structure

Code for this project is structured as follows:

* `functions.R` with custom functions  

* `weighting_workflow.R`, which sources functions.R and can be used to load data and create weights

* inputs folder with accompanying csv files needed to create weights


## Requirements

* This project is built in R, and utilizes the following base packages:

    * e.g., tidyverse, lubridate, survey, janitor

* In order to run properly, underlying BC Demographic Survey data must be available (i.e., in the DIP).
Edit the "SET VALUES" section in `weighting_workflow.R` accordingly.

Note: this code will not run outside of the DIP, as the underlying survey data is not included in the repo.

## Project Status

This project is in a stable state, with no changes expected. 

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/bcstats-bc-demographic-survey-weighting/issues/).

## How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## License

```
Copyright 2025 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```

---
