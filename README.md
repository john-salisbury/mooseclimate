# mooseclimate
John Salisbury - NTNU Master's Thesis Project (2019-2021)

## How does moose herbivory affect carbon and albedo dynamics in successional boreal forest?

This is the GitHub repo for John Salisbury's MSc Biology thesis project. It is a subset of NTNU's SustHerb project, which seeks to explore the role of large herbivores in Norwegian forest dynamics. 

This project has two major components:

### Exclosure component:
An analysis of aboveground tree biomass, surface albedo, and climate forcing across 10 years of forest herbivore exclosure in 44 study sites across Norway. 

**Data**:
* Spatiotemporal dataset of tree species, diameter-at-ground-level (cm), and height (cm) - *provided by SustHerb project at NTNU*
* Spatiotemporal snow water-equivalent (SWE) and temperature (K) observations at each study site - *provided by SeNorge (Norwegian Meteorological Institute)*

**Key Methods**:
* Biomass calculations done using locally-calibrated biomass models by [Kolstad et al. (2018)](https://link.springer.com/article/10.1007/s10021-017-0202-4)
* Albedo estimates made using non-linear albedo models developed by [Hu et al. (2018)](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2018MS001403)
* Net climate forcing estimates made by NTNU Department of Industrial Ecology using methods adapted from [Cherubini et al. (2018)](https://www.tandfonline.com/doi/abs/10.1080/1747423X.2018.1529831?journalCode=tlus20)

**Key Results**:



---

### Spatial component: 
An exploratory spatial analysis of the relationship between large herbivore density and forest surface albedo in early successional forest.

**Data**:
* SatSkog spatial forest data product (vector data) from the Norwegian Institute for Bioeconomics ([data product here](https://www.nibio.no/tema/skog/kart-over-skogressurser/satskog))
* Spatiotemporal snow water-equivalent (SWE) and temperature (K) observations across Norway at 1km2 resolution (raster format) - *provided by SeNorge (Norwegian Meteorological Institute)*
* Spatiotemporal large herbivore density data (vector data) for all municipalities in Norway - *provided by SustHerb project at NTNU*

**Key Methods**:
* Custom data pipeline to process 400+ shapefiles and calculate albedo for over 6 million forest polygons
* CLARA clustering to better visualize trends in forest volume over a range of moose densities

**Key Results**:
The key relationship of interest here is moose density versus forest volume (since albedo is directly dependent upon forest volume). We used CLARA clustering to identify areas of Norway with similar climate and forest age. We then plotted rolling means of forest volume (k = 5) vs. moose density. Unfortunately, it appears that the spatial herbivore data might be too coarse to see any meaningful trends.




---