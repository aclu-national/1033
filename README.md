# 1033

The racial justice protests of June 2020 highlighted what organizers and activists have known for years: police militarization is a dangerous trend that must be reversed. While the causes of and contributors to police militarization are many, a particularly egregious source of militarized equipment is the DoD's 1033 program. Born in 1996 when the scope of an existing program ("1208") was expanded, 1033 allows the Department of Defense to donate "surplus" military equipment to state, local, tribal, and federal law enforcement. Because the equipment is transferred as-is, straight from the battlefield, much of it is especially inappropriate for domestic policing, from armored vehicles to assault rifles. Building on the momentum of our 2014 ["War Comes Home"](https://www.aclu.org/report/war-comes-home-excessive-militarization-american-police) report, the ACLU set out to understand whether this is a program that can be reformed to better serve communities, or whether it must end entirely. We focused especially on the most significant set of reforms to the 1033 program to date - Obama's 2015 Executive Order 13688, and Trump's rollback of that reform in 2017 with Executive Order 13809.

A series of public information requests by journalists in 2014 and 2015 yielded preliminary data releases cataloguing equipment transferred through 1033. Later, policy changes formalized the tracking and publication of 1033 data, creating a relatively thorough data source that currently extends through December, 2020. This repository contains the data and code needed to replicate the results we published in our April, 2021 [article](https://www.aclu.org/news/criminal-law-reform/federal-militarization-of-law-enforcement-must-end), titled "Federal Militarization of Law Enforcement Must End". 

### Folder Organization 
```
1033     
 ¦--README.md 
 ¦--R    # code folder 
 ¦   ¦--00-utils-data-cleaning.R # data cleaning functions
 ¦   ¦--01-read-1033-data.R # downloads, cleans, and combines raw 1033 data
 ¦   ¦--02-identify-equipment-affected-by-EO13688.R  # identifies equipment banned by EO13688
 ¦   ¦--03-collapse-1033-by-station.R # groups by station in order to provide summary variables for each participating LEA 
 ¦   ¦--04-collapse-transfers-by-time.R  # groups transfers by unit of time, for longitudinal or time-bound analyses
 ¦   ¦--05-budget-test-case-analysis.Rmd  # documents the LEA budget test cases referenced at the end of the article
 ¦   ¦--06-1033-Article.Rmd # replicates the numbers provided in the article, with some additional explanation
 ¦   ¦--07-final-charts.Rmd # recreates the graphs provided in the article
 ¦--1033.Rproj 
 ¦--data  
 ¦   ¦--01-raw
 ¦   ¦   ¦--MP Repo-agency level # a clone of the Marshall project's folder for agency-level 1033 data
 ¦   ¦   °--NSN Group Index.xlsx # a lookup table mapping the first two digits of NSN codes to types of equipment
 ¦   ¦   °--EO 13688 banned nsns.csv # a list of NSNs banned by EO 13688
 ¦   ¦   °--35158-0001-Data.rda # FBI law enforcement crosswalk, for mapping law enforcement agencies geographically
 ¦   ¦   °--1033 LEA Budgets.xlsx # budget data for the budget test case analysis; manually constructed
 °-- °--02-interim # created by 01-read-1033-data.R, as most interim data is too big to store on Github and is generated by the scripts provided here   

 ```
  
Each of the numbered R scripts, 00-04, should be run sequentially and before 05-07. 05-07 are independent, and can be run in any order with regards to each other. For greater explanation about the data and scripts, see Data Sources and Methodology, below.

This repository uses the [R library renv](https://rstudio.github.io/renv/articles/renv.html) for package version management. To restore the package versions used to run these scripts, run renv::restore().


### Data Sources

Since the first FOIA-prompted releases in  2014 and 2015, the Marshall Project has been collecting and archiving all available 1033 data in a [github repository](https://github.com/themarshallproject/dod1033). Their archive was our primary data source, although we also submitted a FOIA request and pulled data directly from LESO for the past year. The data we requested matched the Marshall Project's data.

Each quarter, the Law Enforcement Supply Office, which is the office within the Defense Logistics Agency that manages 1033, publishes a data update. Their updates include 1) new transfers to federal law enforcement agencies; 2) a record of pending or cancelled transfer requests and 3) a current inventory of all controlled equipment transferred through 1033 and currently held by participating law enforcement agencies. The latest data files are available on the [LESO Public Information Page](https://www.dla.mil/DispositionServices/Offers/Reutilization/LawEnforcement/PublicInformation/). We set out to create a comprehensive record of all known 1033 transfers by combining the inventory updates and records of new transfers, where appropriate. Further detail about the join used to create the combined 1033 data file is below. 

A number of caveats about the 1033 inventory data should be highlighted:

* LESO updates its database of all transfers currently in circulation quarterly.
* Items that have been returned to the DoD, thrown out, or destroyed are not included; only items that are currently held by participating agencies.
* For each quarterly iteration of the data, items are both added and removed; the change in total number of rows could be negative or positive.
* Importantly, the DoD distinguishes between controlled and non-controlled items and drops all non-controlled items out of its database after one year.
* "Non-controlled" items have the demilitarization code A. For each iteration of the inventory file, about 80% of ship dates within the past year have demil code A, but when you go back farther than a year, the percent of equipment with demil code A drops to basically zero.
* This use/definition of "controlled" is NOT the same as items controlled or "restricted" by Obama's EO 13688 and shouldn't be conflated.
* Item names may correspond to many different NSNs (national stock numbers - an identifying equipment number referenced in the data), and can even span diverse categories of items.
* Acquisition value is per unit, so acquisition value has to be multiplied by quantity to determine the total value captured by each row of the data.
* The acquisition value indicates cost upon purchase, not adjusted for inflation, and some items may have been purchased up to 50 years ago.

**Other Data Sources**

* NSN Group Index.xlsx: copied from a Federal Supply Classification Catalogue, downloadable [here](http://everyspec.com/DoD/DoD-HDBK/CATALOGING_HDBK_H2_FEB2003_37910/)
* 35158-0001-Data.rda: Law Enforcement Agency Identifiers Crosswalk, United States, 2012 (ICPSR 35158), downloadable [here](https://www.icpsr.umich.edu/web/ICPSR/series/366)
* EO 13688 banned nsns.csv: The list of NSNs banned by EO13688 provided by the DLA's Michelle McCaskill, downloaded from KPCC's 1033 repository, [here](https://github.com/SCPR/kpcc-data-team/tree/master/data/1033-program-data)


### Methodology

#### 01: Creating a Merged 1033 Transfer File

Since each iteration of the LESO 1033 inventory data is only a current snapshot of controlled items currently in circulation, simply combining each dated inventory update with the next would result in hundreds of thousands of duplicated rows, where items remain in circulation from one inventory snapshot to the next. There is also no way to uniquely identify a particular transfer, as a number of identical pieces of equipment are often transferred to the same law enforcement agencies on the same date. And since equipment is recycled or returned to the DoD, even the number of units of a specific kind of equipment transferred on a certain day may appear to decrease over time, as some units are returned and others remain in circulation. Because there are no unique identifiers for each item, we used a methodology which adds *only net new transfers* from each inventory update after the first to the cumulative total count of items. 

01-read-1033-data.R does this: It starts with the oldest available agency-level inventory update and, for each next iteration, adds only those rows with a ship date after the latest ship date of the previous dataset. So if one dataset ends on 2018-06-30, then from the next dataset I take only shipments from after 2018-06-30 and append them. Also appended are 1) records of net new federal agency transfers (which at a certain point were separated out from the main inventory data and published as new transfers rather than inventor updates) and 2) a record of ammunition transfers made in 2017-2018.

01-read-1033-data.R also manually fixes a small amount of likely data entry errors: lines with ship dates that are a year after the date the data was released. Other quirks in the data are not corrected so easily. For example, a handful of transfers seem to have been recorded a retroactively: for instance, they first appear in the 06302020 update despite having a ship date before the end of the previous quarter on 03312020. Those few items would be excluded by this merge since their ship dates fall *before* the max date of the previous update. In general, the merge has been carefully reviewed and provides a thorough account of transfers, though it is not perfect.

It is important to remember that this merged 1033 data file can not speak to when equipment exits circulation nor what equipment is currently held. To examine equipment available to participating LEAs at a given point in time, one would need to use the nearest inventory snapshot to the date of interest.

#### 02: Identifying Equipment Banned by EO13688

A primary focus of our piece is Barack Obama's executive order 13688, which banned and recalled certain items and implemented new restrictions on the transfer of others. It also made some overarching changes to the 1033 program: in particular, establishing an oversight body and formalizing data tracking and publication requirements. In order to understand the effect of the EO on the quantity of 1033 transfers, we needed to identify banned items in the data. Banned items included:

* TRACKED ARMORED VEHICLES
* WEAPONIZED AIRCRAFT, VESSELS, AND VEHICLES OF ANY KIND
* FIREARMS OF .50‐CALIBER OR HIGHER
* AMMUNITION OF .50‐CALIBER OR HIGHER
* GRENADE LAUNCHERS
* BAYONETS
* CAMOUFLAGE UNIFORMS

Unfortunately the EO text doesn't contain NSNs (identifying numbers attached to equipment), and there's no surefire way to comprehensively identify these items purely from the dataset itself, as item names are also irregular. However, the Southern California Public Radio data team did ask the Defense Logistics Agency for a list of NSNs banned by EO 13688, and received a list from the DLA's Chief of Media Relations, Michelle McCaskill. See [here](https://github.com/SCPR/kpcc-data-team/blob/master/guides/primer-on-defense-logistics-agencys-1033-program-data.md). 02-identify-equipment-affected-by-EO13688.R reads in the list of banned NSNs to identify them in the merged 1033 dataset.

The LESO public information website also provides a small table of items that were recalled under EO 13688, available [here](https://www.dla.mil/DispositionServices/Offers/Reutilization/LawEnforcement/PublicInformation/). All of the recalled NSNs are present in the list provided by Michelle McCaskill.

#### 03-07: Data Analysis Scripts

Scripts 03 and 04 collapse the merged file of 1033 transfers by law enforcement agency and by time unit, respectively. 03-collapse-1033-by-station.R also joins the collapsed data to county-level census data. One important caveat is that law enforcement agencies serve diverse jurisdictions, and joining to census data is only valid when the agency's jurisdiction is coterminous with the geographic range of the census data. Specifically, we only join to county-level census data where the law enforcement agency specifically serves the county, which means only county sheriffs can be examined in this way. 

Script 05 examines about 20 law enforcement agency budget "test-cases." A common argument in favor of 1033 is that it saves law enforcement agencies (and, by extension, the taxpayer) money by donating equipment to them they'd otherwise have to buy. If this cost-saving argument is true, we would expect to see evidence in law enforcement agency budgets. Where 1033 transfers are received, we would expect to see a reduction in spending for that year. We test this hypothesis using a small sample of "most-extreme" cases - cases where police or sheriff's offices received a large amount (> $800,000) of equipment in a concentrated time period (less than one year). These criteria narrow the set of participating law enforcement agencies to about 35, and for 19 of those we were able to find budget year-to-year information. In only one case did an influx of 1033 equipment correspond to an equivalent decrease in police spending in the following year. In all other cases, either there was no reduction in spending at all or the size of the 1033 transfer dwarfed a nominal downward adjustment.

Scripts 06 and 07 are markdown files generating the numbers and graphs referenced in the article, respectively. 

### Other 1033 resources

[KPCC's Notes on Using 1033 Data](https://github.com/SCPR/kpcc-data-team/blob/master/guides/primer-on-defense-logistics-agencys-1033-program-data.md)

[The Marshall Project's 1033 data repository](https://github.com/themarshallproject/dod1033)

**ACLU Publications**

2014: [War Comes Home: the Excessive Militarization of American Policing](https://www.aclu.org/criminal-law-reform/war-comes-home-excessive-militarization-american-police-report)

2017: [As We Remember the Militarized Response to the Ferguson Uprising, Trump Says Civilian Police Are Making ‘Good Use’ of Military Weapons](https://www.aclu.org/blog/racial-justice/race-and-criminal-justice/we-remember-militarized-response-ferguson-uprising)

2017: [Even Fake Law Enforcement Agencies Can Get Weapons of War for ‘Policing’](https://www.aclu.org/blog/criminal-law-reform/even-fake-law-enforcement-agencies-can-get-weapons-war-policing)

2020: [Communities Deserve Better Than Bayonets and Grenade Launchers: The Defense 1033 Program Must End Now](https://www.aclu.org/news/criminal-law-reform/communities-deserve-better-than-bayonets-and-grenade-launchers-the-defense-1033-program-must-end-now/)


