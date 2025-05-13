bioclima 0.1.2
=============
- Use mois code intead of soilm to define Moistue varibles. This technically allows to use any kind of moisture variables as climate and soil moisture. Also, bio31() use standard deviation instead of CV because climate index (as the one available in Chelsa can be negative)

bioclima 0.1.1
=============
- Fix bug for bio18 and bio19, which was using temperature periods intead of precipitation. Bug was introduced when adding the new option of static periods (2025-02-10). If you used a previous version, bios were calculated correctly.

bioclima 0.1.0
=============
- Easier names for internal functions
- Option to add bioclimatic variables bio24-35
- bionames() to get names easily
- Option to add static periods. Useful when you wnat to use same periods in time-series
- New stats_clima() to make your own summaries of variables (e.g., evapotraspiraion, cloudiness)

bioclima 0.0.4
=============
- Fix bug in mismatch_NA().
- Some small changes in documenting the package.

bioclima 0.0.3
=============
- Changing pipe and adding call to terra functions and internal functions. Thanks to the contribution of [@francisvolh](https://github.com/francisvolh)
- Updated node in pkgdown.

bioclima 0.0.2
=============
- Vignette in Spanish to be show in the "Modelamiento de Nichos Ecológicos" facebook group
- Add stdev for population in bio04 and bio15
- No round of variables
- More infomation in README file

bioclima 0.0.1
=============
- First code public