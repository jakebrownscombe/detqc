# Example files

`stoney_depth.tif` has the same grid and depths as `depth_raster` of
positionR 1.7.0, a 100 m depth template of Stoney Lake, Ontario, derived
from the bathymetry made before 2024, with the CRS set to EPSG:32617 (UTM
zone 17N on WGS 84). Depth is zero or negative metres and land is `NA`.
4,818 cells are water, and 56 of them hold 0;
`tests/testthat/test-extdata.R` checks both counts. The detfield,
arraydesign and arraydesignR packages carry the same file.

Every other file is simulated, and the receivers, stations, tags and
animals in them are invented. The detection, deployment, tag sheet and
`receiver_logs/` files hold one study of four walleye and 20 receivers in
the formats the readers take. `nextrak_receiver_log.csv` is one NexTrak
receiver over three days. `stoney_range_test.csv` is a range test of 8
transmitters on 1 receiver, a copy of `range_test.csv` from arraydesign.
The scripts in `data-raw/` of the repository,
<https://github.com/jakebrownscombe/detqc/tree/main/data-raw>, write these files; the installed
package does not hold them.
