## GPstat! (part of DailyMeasure) - 'Best Practice' dashboard for Quality Improvement

v1.6.2 (10th July 2020)

A quality improvement tool for [Best Practice](https://bpsoftware.net/).

GPstat! interrogates the appointment book, searching for ‘near-future’ opportunities for preventative health, screening, chronic disease management and quality improvement opportunities.

![GPstat! screenshot](https://photos.smugmug.com/Office/Eltham/i-q3shMp5/0/a5b42c39/O/GPstat%21Measles.jpg)

#### New features since version 1.5.9

* Medication module (counts and lists medications, by default lists patients with >= 4 medications)
* Custom patient lists

#### Also includes

* Search for COVID-19 bulk billing incentive opportunities
* Immunization lists (influenza, measles and Zostavax)
* Cancer screening (bowel cancer, breast cancer, cervical screening, measles)
* Quality Improvement Measure 1 to 10 – diabetes, BP, cardiovascular risk, smoking etc..
* Social history and Allergies completion list
* Post-natal list

![GPstat! screenshot 2](https://photos.smugmug.com/Office/Eltham/i-ZSfjQ6V/0/8b876c6a/O/GPstat_COVID19BB.png)

#### Videos and other documentation

[Video demonstration (Youtube)](https://youtu.be/mTJzcycPkRU)

[Installation video (Youtube)](https://youtu.be/stfFmX114CY)

[Installation documentation (in words)](https://rpubs.com/DavidFong/GPstatInstall)

Very technical [backend documentation (dMeasure)](https://rpubs.com/DavidFong/dMeasure)

The ‘bpsrawdata’ database access password（as used by ‘BPbrowser’) is required to configure GPstat! (this is described in the Youtube installation video).

#### Live demonstrations (using sample database):

[Live demonstration server 1](http://vkelim.dsmynas.com/)

[Live demonstration server 2](http://vkelim.3322.org/)

![GPstat! screenshot 3](https://photos.smugmug.com/Office/Eltham/i-XZbCQ66/0/ea215232/O/gpstat_cdm.jpg)

#### Download links

##### Current version

[OneDrive link v 1.6.2.0.9.11](https://unimelbcloud-my.sharepoint.com/:u:/g/personal/dfong1_unimelb_edu_au/Ed-uvfPKD95BorTES_VAokEBWOTEkRCiHLzJbo-aDDZytQ?e=bLB8dG)

[Google Drive link v 1.6.2.0.9.11](https://drive.google.com/file/d/1z9c9mPO4K1-CxqVC-xp8Xa0h4aJK2fEd/view?usp=sharing)


##### Older versions

[OneDrive link v 1.6.0.0.9.10](https://unimelbcloud-my.sharepoint.com/:u:/g/personal/dfong1_unimelb_edu_au/EdJbHvnot_tAgJ3rmIUpqWgBitqo8ZuyYrC43_SuB0KanQ?e=wJaXJE)

[Google Drive link v 1.6.0.0.9.10](https://drive.google.com/file/d/1oWKFcX3gta9ZyEazz-aIzfznMK8SXB5d/view?usp=sharing)

[OneDrive link v 1.5.9.0.9.9](https://unimelbcloud-my.sharepoint.com/:u:/g/personal/dfong1_unimelb_edu_au/Eb6gXcBy-J1BtcRt53XVULABhA0bz2VSH-lvLd6WmcB6Og?e=HEXEch)

[Google Drive link v 1.5.9.0.9.9](https://drive.google.com/file/d/1dJv0DQmcziNni0jp3fs2CBsDoX9cHMLe/view?usp=sharing)

#### Recent changelog

```

# Version 1.6.2
8th July 2020

## Changes

* Immunizations, Cancer screening, QIM, Administration to show chosen through modal
* change license to Mozilla Public License 2.0
* restrict `BMIvalue` display to one decimal place
* dMeasure 0.9.11 : change to `WeightDone` report, depends on `BMIDate` instead of `WeightDate`

# Version 1.6.1
3rd July 2020

## Changes

* move Billings UI to dMeasureBillings

# Version 1.6.0
27th June 2020

## New

* Support for dMeasureMedication module
* Support for custom patient list module interaction with
  configuration SQLite database

## Changes

* speed improvement for Chronic Disease Management (CDM) display
* speed improvement for Billings display
* Viewed billings types changed with modal
* responsibility for CDM module UI moved to module
  + this is what is already used for Medication and Custom modules
* changes for new version of DTedit (v2.0+)

## Bugfixes

* Contact minimum/maximum date not properly initialized at start-up

# Version 1.5.10
20th May 2020

* better restriction of date ranges ('negative' date ranges prevented)
* improved separation of tasks for billings_list
* changes to report specific number of days which are restricted
  by lack of registration/subscription as reported by 
  check_subscription_datechange_trigR

# Version 1.5.9
29th April 2020

New

* Download list of configured users, including 'Identifiers'
  - for registration/subscription application purposes
* 'bounce' reminder on update_clinicians and update_date
  button if chosen clinicians or dates change (shinyjqui effect)
* Contact filter includes date of last contact (minimum and maximum dates)

Change

* chosen clinician list has an 'Update' button
* Billings view adapted to change in billings module (version 0.4.0)
  dMeasureBillings::list_billings no longer provides print/HTML tags
  use dMeasureBillings::tag_billings_list to add print/HTML tags
** improvement in billings view to reduce time to change from
   print to HTML 'button' view

# Version 1.5.8
24th April 2020

New

* asthma view in Conditions tab
* 'include up-to-date' option for cancer screening
* COVID-19 bulk billing incentive prompt in billing view
* filters for different billing typess (private, WorkCare, DVA, 'bulk'/direct)
  in billing view

Fix

* fixed logic for updating 'date_to' input on right side-panel
```
