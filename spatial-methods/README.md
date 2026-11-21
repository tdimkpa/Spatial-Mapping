# Spatial Analysis Methods

Core spatial analysis methods applied to public-health questions, built in R with supporting maps made in QGIS. Each project covers one method end to end — the question it answers, the data it uses, the technique, and the map it produces.

## Contents

- [Census mapping](#census-mapping)
- [Geocoding and symbolizing points: Toxic Release Inventory](#geocoding-and-symbolizing-points-toxic-release-inventory)
- [Kernel density estimation: cholera in Soho, 1854](#kernel-density-estimation-cholera-in-soho-1854)
- [Geoprocessing and buffers](#geoprocessing-and-buffers)
- [Spatial interpolation: ground-level ozone](#spatial-interpolation-ground-level-ozone)
- [Space-time analysis](#space-time-analysis)
- [Running the code](#running-the-code)
- [Reproducibility notes](#reproducibility-notes)

---

## Census mapping

**Folder:** `lab-01-census-mapping/`

**Question:** How does health insurance coverage vary across counties in New Jersey?

**Data:** American Community Survey 2020 5-year estimates, pulled directly from the Census API with `tidycensus`.

**Methods:** API-based census data retrieval, joining tabular ACS data to county boundaries, choropleth mapping with classified breaks.

**Packages:** `tidycensus`, `tidyverse`, `sf`, `RColorBrewer`, `classInt`, `ggspatial`

**What the map shows:**

- `NJ_insured.jpg` — percent of the population who are insured, by New Jersey county.

**Note:** This project needs a personal Census API key. Get a free key at https://api.census.gov/data/key_signup.html and set it as the `CENSUS_API_KEY` environment variable before running the script — the script reads it with `Sys.getenv("CENSUS_API_KEY")` and no key is stored in this repository.

## Geocoding and symbolizing points: Toxic Release Inventory

**Folder:** `lab-03-tri-geocoding/`

**Question:** Where are New Jersey's industrial facilities that report toxic releases, how much do they emit, and how do emissions add up by county?

**Data:** EPA Toxics Release Inventory (TRI) 2020 facilities in New Jersey (`tri_2020_nj.csv`), geocoded from addresses, joined to New Jersey county boundaries.

**Methods:** Geocoding (`tidygeocoder`), map projections compared side by side (EPSG:4326 WGS 84, EPSG:3857 Pseudo-Mercator, EPSG:5070 Albers Conic), proportional-symbol mapping, spatial joins, and county-level aggregation of total emissions.

**Packages:** `tidyverse`, `sf`, `RColorBrewer`, `classInt`, `ggspatial`, `patchwork`, `tidygeocoder`

**What the maps show:**

- `TRI_release.png` — amount of toxic release from individual industrial facilities.
- `TRI_join_emissions.png` — TRI facilities joined to county boundaries.
- `tot_emissions_county.jpg` — total pounds of toxic emissions per county.

## Kernel density estimation: cholera in Soho, 1854

**Folder:** `lab-04-cholera-kde/`

**Question:** Where was cholera mortality concentrated in Soho during John Snow's 1854 outbreak, and what does a kernel density surface reveal that a dot map does not?

**Data:** Geocoded cholera death cases and the Soho street network / study-area boundary.

**Methods:** Kernel density estimation (`sfhotspot`, 10 m cells), classification with Jenks natural breaks, hotspot mapping.

**Packages:** `tidyverse`, `sf`, `sfhotspot`, `RColorBrewer`, `classInt`, `ggspatial`

**What the maps show:**

- `KDE_cholera.png` — kernel density of cholera deaths in Soho, 1854 (QGIS render).
- `kde_cholera.jpg` — kernel density surface (R render).
- `hotspot.jpg` — hotspot map of cholera deaths.

## Geoprocessing and buffers

**Folder:** `lab-05-geoprocessing-buffers/`

**Question:** How close are vape shops to schools, how far is each NYC ZIP code from its nearest COVID-19 vaccination site, and what can you reach from a single point on foot, by car, or by bike in 40 minutes?

**Data:** Trenton schools (`schools.shp`) and vape shops (`vaping.shp`), NYC ZIP codes (MODZCTA) and COVID-19 vaccination sites, and a point location for the Mailman School of Public Health.

**Methods:** Buffering (1,000 ft around schools), spatial joins and point-in-polygon counts, nearest-feature distance calculation, and routing-based isochrones with the OpenStreetMap routing engine (`osrm`).

**Packages:** `tidyverse`, `sf`, `ggspatial`, `classInt`, `RColorBrewer`, `osrm`, `patchwork`

**What the maps show:**

- `trenton_schools.jpg` — schools in Trenton, the starting point for the buffer analysis.
- `vape_qgis.png` / `vape2_qgis.png` — number of vape shops within 1,000 ft of school grounds (QGIS renders).
- `distance2vac_centers.jpg` — distance to the nearest COVID-19 vaccination site, by NYC ZIP code.
- `site_nyc.png` — NYC vaccination site locations.

**Reproducibility limit:** The input shapefiles `schools.shp` and `vaping.shp` were found in the source folder, but they are **not included in this staging** (only the script and map images are), and neither are the MODZCTA, vaccination-site, or Mailman School shapefiles the script reads. Re-running the script requires supplying those shapefiles alongside it.

## Spatial interpolation: ground-level ozone

**Folder:** `lab-06-ozone-idw/`

**Question:** What did ground-level ozone concentrations look like across the mainland United States in 1990, including in places with no monitoring station?

**Data:** 1990 ozone monitor readings (`ozone_1990.csv`), US state and county boundaries.

**Methods:** Inverse distance weighting (IDW) interpolation (`gstat`, power 2.0) onto a raster grid, clipped to the mainland US, classified with Fisher breaks, plus county-level zonal means.

**Packages:** `tidyverse`, `sf`, `gstat`, `ggspatial`, `RColorBrewer`, `classInt`

**What the maps show:**

- `us_ozone.jpg` — ground-level ozone concentration in the mainland United States, 1990.
- `ozone_qgis.png` — interpolated ozone surface (QGIS render).
- `counties_ozone.png` — county mean ozone concentrations.

## Space-time analysis

**Folder:** `lab-09-space-time/`

**Question:** How do health and social patterns change over both space and time — disability across Pennsylvania counties by year, teen pregnancy across New York, and street robbery hotspots in Washington Heights by police shift?

**Data:** Pennsylvania county disability data by year, New York teen pregnancy data, and Washington Heights robbery points (`robs.RData`) with shift information.

**Methods:** Animated choropleth mapping (`gganimate`, `gifski`), percent-change mapping, and kernel density estimation by time slice (`sfhotspot`).

**Packages:** `tidyverse`, `sf`, `sfhotspot`, `gganimate`, `ggspatial`, `classInt`, `RColorBrewer`, `gifski`

**What the maps show:**

- `teenpreg.png` — teen pregnancy map for New York.
- `pct_change.png` — percent change over time.
- `cluster_by_shift.jpg` — robbery clusters in Washington Heights, split by shift.

**Note:** The animated (GIF) output of the Pennsylvania disability animation is not included here — only the still images above. The script also reads `PA_county.shp`, `NY_zip.shp`, `WH.shp`, and `robs.RData`, which are not included in this repository.

## Running the code

Each folder is self-contained: an R script plus its map outputs. Requirements:

- R (scripts were written against R 4.x) and QGIS for the QGIS-rendered maps.
- Install the packages listed under each project above before running its script, e.g. `install.packages(c("tidyverse", "sf", ...))`.
- Run each script from inside its own folder — all file paths are relative. The original `setwd()` calls and `install.packages()` lines were removed for portability; see the header comment in each script.
- The census-mapping project needs a personal Census API key — see its section above.

## Reproducibility notes

- The map images are the original rendered outputs; the R scripts reproduce the R-rendered figures.
- Input data files (shapefiles, CSVs, and `.RData` files read by the scripts) are **not** included in this repository. The scripts read them by filename from the working directory, so place the corresponding data files alongside a script before re-running it. Where the source data is public (ACS/Census API, EPA TRI, NYC Open Data), the section above names the source.
- Large intermediate rasters were excluded deliberately: the 268 MB `KDE_clipped.tif` from the cholera project and the interpolated/clipped ozone `.tif` files from the ozone project. The finished maps are included instead. A duplicated 14.4 MB `US_states.shp` in the geocoding project was also excluded; the script reads it by filename if you re-run that project.
