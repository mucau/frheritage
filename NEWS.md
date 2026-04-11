# frheritage 0.1.1

## New features

- Added a check to ensure the availability of the data source before querying, improving robustness when the external service is unavailable.
- Exported `get_deps()` to document and illustrate the processing workflow.

## Improvements

- Improved internal logic of `get_heritage_ids()` for better robustness and clarity.
- Improved internal logic of `get_heritage()` to ensure consistent outputs.

## Breaking changes

- Removed spatial filtering option due to inconsistency with upstream data source behavior.
