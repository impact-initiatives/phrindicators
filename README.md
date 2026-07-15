# phrindicators — Public Health Indicators

**phrindicators** focuses on `add_*` indicator-construction functions.

Core validation, error-handling, and related helper utilities are provided by
[`phrutils`](https://github.com/impact-initiatives/phrutils).

## Installation

```r
# install.packages("remotes")
remotes::install_github("impact-initiatives/phrutils")
remotes::install_github("SaeedR1987/phr_indicators")
library(phrindicators)
```

## Scope

The package includes add-functions across:

- Food security and livelihoods
- Nutrition
- Mortality
- WASH
- Household and roster standardization
- Health access

See function-level documentation in R, for example:

```r
?add_fcs
?add_muac
?add_standardized_deaths
?phrutils::phr_validate_columns
```

## License

MIT — see [LICENSE.md](LICENSE.md).
