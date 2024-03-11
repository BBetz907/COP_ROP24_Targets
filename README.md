
# COP24_KP_TARGETS

<!-- badges: start -->
<!-- badges: end -->

The goal of COPyy_KP_TARGETS is to show changes in proposed KP targets against past years and make salient comparisons to ensure COP/ROP targets are reachable and reasonable. The workbook primarily emphasizes key populations (KP), but also can be used to explore by age, sex, and modality as well as other comparisons leverging these aforementioned characteristics as filters.

The data is obtained from datapacks (target setting tools, as of COP23), PSNUxIM workbooks, and MER structured data. MER data are manipulated from the quarterly hyperfile hosted on Tableau Server and exported locally using Tableau Prep Builder. Targets are obtained via correspondence with country cluster members and perusal of PEPFAR sharepoint, early in the process, and later from the hyperfile after target approval by GHSD. Targets are transformed in R using the tameDP package and other Tidyverse + gagglr packages.

The process begins by obtaining target setting tools and PSNU x IM tools. Since the latter are more detailed, the TST will be dropped from the file transformation when the PSNU x IM tool is in the source folder. Move outdated versions of these files to the archive folders to avoid inclusion of multiple versions of either file type--an error which will be difficult to detect other than in QA/QC where inflated targets will be observed. <br>


Files should be run in the following order: <br>	
	0 packages.R <br>
	1 psnu x im.R <br>
	2a find and identify target setting tools.R <br>
	2b pre-psnu x im.R <br>
	3 tast transformation.R <br>
	MSD Processing.tfl <br>
	4 MSD.R <br>
	5 merge and export.R <br>

Note that the Tableau Prep Builder file (MSD Processing.tfl) only needs to be repeated when new versions of the MSD are released. It is not necessary when adding new targets.

Update/refresh the data source for the tableau workbook and publish to Tableau Server. For QA/QC compare the dashboard output to roll-up and row level targets and to past MER data.

The directory can be duplicated and renamed for use on a future COP/ROP.

Author: Bourke Betz
Date updated: 2024_03_11

---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*