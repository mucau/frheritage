# frheritage<img src="man/figures/frheritage_logo.png" align="right" height="138"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/mucau/frheritage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mucau/frcadastre/actions/workflows/R-CMD-check.yaml) [![CRAN status](https://www.r-pkg.org/badges/version/frheritage)](https://CRAN.R-project.org/package=frheritage) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

## Overview

The **Atlas du Patrimoine** is the official national platform of the French Ministry of Culture that centralizes cultural heritage information. It provides access to a wide range of spatial and descriptive datasets, documenting France’s **architectural, archaeological, movable, and intangible heritage**.

The Atlas gathers and harmonizes data from several major databases such as:

-   Merimee – architectural and monumental heritage (churches, castles, etc.)

-   Palissy – movable heritage (objects, furniture, sculptures, etc.)

-   Patrimoine Mondial – UNESCO World Heritage sites in France

-   Patrimoine Archéologique – archaeological sites and surveys

-   Musees de France, Joconde, and other cultural inventories

All datasets are georeferenced and available through web services and open data exports provided by the Ministry of Culture. More details and an overview of available datasets can be found at: <http://atlas.patrimoines.culture.fr/atlas/trunk/static/presentation.html>

## Citation and source

Data are provided by the French Ministry of Culture, through the Atlas du Patrimoine platform: <http://atlas.patrimoines.culture.fr> Please cite both the Atlas du Patrimoine and this package when using data in scientific or professional work.

## API and data access disclaimer

The **frheritage** package is an independent, open-source client designed to facilitate access to public cultural data provided by the French Ministry of Culture. This package is **not affiliated with, endorsed by, or officially supported** by the Ministry.

All data remain the property of the **French Ministry of Culture** and are distributed under the **Etalab Open License 2.0** (or any equivalent open-data license indicated by the data provider). Users are responsible for complying with the applicable terms of use, attribution requirements, and update policies of the Atlas du Patrimoine datasets.

For official documentation and metadata, please refer to: <https://www.culture.gouv.fr/aides-demarches/protections-labels-et-appellations>

------------------------------------------------------------------------

## The frheritage package

### Purpose

The **frheritage** R package provides functions to explore, query, and retrieve datasets from the **Atlas du Patrimoine** within R. It aims to facilitate spatial analysis and data integration in geographic workflows, particularly for heritage management, cultural mapping, and landscape studies.

It allows to:

-   Retrieve and filter heritage datasets by code (internal nomenclature) and departement

-   Perform spatial queries directly from geometries (sf objects)

-   List all available heritage layers

-   Automatically validate user inputs and ensure compatibility with functions

### Main features

The main functions are:

-   `get_heritage_layernames()` : Returns a data frame listing all available heritage datasets with their internal codes, and labels. Useful for identifying which datasets are accessible through the package.

-    `get_heritage()`: Downloads one or several datasets from the **Atlas du Patrimoine** from a given `sf` object. It can apply spatial filters (intersects, within, etc.) and automatically handle geometries.

-    `get_heritage_ids()`: Retrieves available heritage IDs from the **Atlas du Patrimoine** from a given `sf` object.

### Getting start

``` r
# Just run
devtools::install_github("mucau/frcadastre")

# Focus on a city
my_sf_polygon <- frcadastre::get_etalab(72191, data="communes")

# Retrieve the "immeuble classes ou inscrits" by using code "IMMH"
immh <- get_heritage(
  x = my_sf_polygon,
  data_code = "IMMH",
  buffer = 2500,
  spatial_filter = "within"
)

# To ask something else, try an other code !
# You don't know the codes ?
codes <- get_heritage_layernames()

# You don't trust my codes ?
# Okay, that's fine. You can check that I haven't forgotten any.
ids <- get_heritage_ids(my_sf_polygon)
```

### Problems and Issues

-   Please report any issues or bugs you may encounter on the [dedicated page on github](https://github.com/mucau/frheritage/issues).

------------------------------------------------------------------------

Have fun ! :)
