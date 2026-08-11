# frheritage 0.1.2

## Improvements

* Improved `get_heritage()` robustness when processing spatial queries that return no features.
* Added handling for `NonTelechargeable.txt` responses returned by the Atlas du Patrimoine when no spatial features match the requested extent.
* Improved error handling when shapefiles cannot be read from downloaded archives.
* Improved diagnostic messages for empty spatial query results and shapefile read failures.
