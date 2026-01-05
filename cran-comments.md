Package: frheritage
Version: 0.1.0

## Purpose
The package provides tools to download, parse, and manage French heritage spatial vector data 
from official portals (e.g., Atlas Patrimoines). It is primarily intended for forest and 
heritage data management in France.

## Test Environments
- Local: Windows 11, R 4.5.0, UTF-8
- CRAN: Linux / macOS / Windows (tested via `devtools` and local R CMD check)
- All tests pass, including examples and internal data (`sevres.rda`)

## Notes on Checks
- **Language field:** The DESCRIPTION file does not include a `Language` field; the package 
  defaults to `en-US`. This is standard and intentional.
- **Documentation words:** Some documentation and data objects include French words 
  (e.g., “abords”, “Patrimoine”, “Musées”, “reprojection”). These are intentional, as the 
  package is focused on French spatial heritage datasets.
- **LICENSE:** The package uses `GPL (>= 3)` declared in DESCRIPTION. Any LICENSE.md file 
  at top-level has been removed or ignored.
- **Quarto:** Local devtools::check() may show a message regarding `quarto -V`; this is 
  only relevant to the development environment and does not affect CRAN checks.

## External Data / Network Access
- Some functions download data from the official portals (e.g., `get_heritage_layernames()`), 
  so examples that perform downloads are wrapped in `\donttest{}` to avoid network issues during CRAN checks.
- All examples that require heavy computation or network access have been marked with 
  `\donttest{}` to comply with CRAN policies.

## Maintainer / Authors
- Maintainer: Matthieu Chevereau <matthieu.chevereau@hotmail.fr>
- Authors@R field properly declared.

## Additional Information
- This is the **first CRAN submission** of the package.
- All non-English terms, including field names and documentation, are **intentional and 
  descriptive of the French heritage context**.
  
## R CMD check results
0 errors | 0 warnings | 1 notes

