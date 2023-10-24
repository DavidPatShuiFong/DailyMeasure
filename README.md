## GPstat! (part of DailyMeasure) - 'Best Practice' dashboard for Quality Improvement

v2.4.0 (12th October 2023)

A quality improvement tool for [Best Practice](https://bpsoftware.net/).

GPstat! interrogates the appointment book, searching for ‘near-future’ opportunities for preventative health, screening, chronic disease management and quality improvement opportunities.

![GPstat! screenshot](https://photos.smugmug.com/Office/Eltham/i-q3shMp5/0/a5b42c39/O/GPstat%21Measles.jpg)

#### New features since version 1.6.4

* Medication module (counts and lists medications, by default lists patients with >= 4 medications)
* Custom patient lists
* Compatibility with BP Saffron

#### Also includes

* Search for COVID-19 bulk billing incentive opportunities
* Immunization lists (influenza, measles and Zostavax)
* Cancer screening (bowel cancer, breast cancer, cervical screening, measles)
* Quality Improvement Measure 1 to 10 – diabetes, BP, cardiovascular risk, smoking etc..
* Social history and Allergies completion list
* Post-natal list
* Document and PCEHR document display
* Search for terms in progress note 'visits'
* Search for patients in appointment book with large number of medications in medication list
* 'Action' search


![GPstat! screenshot 2](https://photos.smugmug.com/Office/Eltham/i-ZSfjQ6V/0/8b876c6a/O/GPstat_COVID19BB.png)

#### Videos and other documentation

[Video demonstration (Youtube)](https://youtu.be/mTJzcycPkRU)

[Installation video (Youtube)](https://youtu.be/stfFmX114CY)

[Installation documentation (in words)](https://rpubs.com/DavidFong/GPstatInstall)

[User manual (in words) - this is a copy of the 'in program' help documentation](https://rpubs.com/DavidFong/GPstatUserManual)

Very technical [backend documentation (dMeasure)](https://rpubs.com/DavidFong/dMeasure)

The ‘bpsrawdata’ database access password（as used by ‘BPbrowser’) is required to configure GPstat! (this is described in the Youtube installation video).

#### Live demonstrations (using sample database):

[Live demonstration server 1](http://vkelim.dsmynas.com/)

[Live demonstration server 2](http://vkelim.3322.org/)

![GPstat! screenshot 3](https://photos.smugmug.com/Office/Eltham/i-XZbCQ66/0/ea215232/O/gpstat_cdm.jpg)

#### Download links

##### Current version

[Google Drive link v 2.3.0.0.10.0b](https://drive.google.com/file/d/1dos3Ooa0pTZwX5PqQPSxgndbOJM1dHjP/view?usp=sharing)

##### Older versions

[Google Drive link v 2.2.2.0.9.17](https://drive.google.com/file/d/1mvo10hPhP4sG8xCqz1_9KEFbBxR_Qj7O/view?usp=sharing)

[Google Drive link v 2.2.2.0.9.16](https://drive.google.com/file/d/10YR9vJtFb18pUBoTbUYIZhAMOu8oBJBJ/view?usp=sharing)

[Google Drive link v 2.2.1.0.9.15](https://drive.google.com/file/d/1x2Y5aCc-oVxnbNqFF1NkkxkAQohbEmsL/view?usp=sharing)

[Google Drive link v 2.2.0.0.9.14](https://drive.google.com/file/d/1E2oZ_OEcdFeObCRE538yzfPY0q8IH7tC/view?usp=sharing)

[OneDrive link v 1.6.2.0.9.11](https://unimelbcloud-my.sharepoint.com/:u:/g/personal/dfong1_unimelb_edu_au/Ed-uvfPKD95BorTES_VAokEBWOTEkRCiHLzJbo-aDDZytQ?e=bLB8dG)

[Google Drive link v 1.6.2.0.9.11](https://drive.google.com/file/d/1z9c9mPO4K1-CxqVC-xp8Xa0h4aJK2fEd/view?usp=sharing)

[OneDrive link v 1.6.0.0.9.10](https://unimelbcloud-my.sharepoint.com/:u:/g/personal/dfong1_unimelb_edu_au/EdJbHvnot_tAgJ3rmIUpqWgBitqo8ZuyYrC43_SuB0KanQ?e=wJaXJE)

[Google Drive link v 1.6.0.0.9.10](https://drive.google.com/file/d/1oWKFcX3gta9ZyEazz-aIzfznMK8SXB5d/view?usp=sharing)

[OneDrive link v 1.5.9.0.9.9](https://unimelbcloud-my.sharepoint.com/:u:/g/personal/dfong1_unimelb_edu_au/Eb6gXcBy-J1BtcRt53XVULABhA0bz2VSH-lvLd6WmcB6Og?e=HEXEch)

[Google Drive link v 1.5.9.0.9.9](https://drive.google.com/file/d/1dJv0DQmcziNni0jp3fs2CBsDoX9cHMLe/view?usp=sharing)

#### Recent changelog

```

# version 2.3.0
13th October 2021

## New

* `sidebarmenuPriority` - optional function in dMeasure modules
  + priority of position in left side bar menu 
* support for `Select` extension in `$datatable_styled`

## Change

* Move Appointments tab to `dMeasureAppointments`
* dMeasure module package discovery moved to `dM$read_dMeasureModules`

# version 2.2.2
3rd July 2021

## New

* Display preferences module
  + Initial work to allow display of dates in preferred format e.g. YYYY-mm-dd or dd-mm-YYYY
  + Suggestion of RN Dat Le, thanks!

# version 2.2.1
16th June 2021

## Change

* Configuration file path chooser includes 'Documents' folder

## Bugfix

* proper call to module-specific reads of configuration database (`read_configuration_db`) when new configuration file created
* `userconfig.insert.callback` more graciously handles License Value `NA` (only applicable for empty userconfig)
* Best Practice database chooser responds better to dMeasure initiated change in database choice
  + usually due to incorrect database location description, user or password

# version 2.2.0
1st April 2021

## New

* Search for actions on basis of text in Action text or Comment
  + in `Administration` section

# version 2.1.0
25th October 2020

## Change

* `shinyFeedback` for server choice


# Version 2.0.0
19th August 2020

## New

* 'auto-load' of `dMeasure*` modules
  + this potentially allows auto-loading of user-written custom modules

## Change

* move from `shinycssloaders` to `shinybusy`

## Bugfix

* most recent contact picker
  + limit the minimum of the 'maximum' date of recent contact to
    be the same as the 'minimum' date
  + no longer trigger a refresh (set `IgnoreInit = TRUE`) on first sight

# Version 1.6.4
16th July 2020

## New

* myHealth (PCEHR) document display

## Change

* dropdown no longer open a a formal shiny modal. will change settings when closed.
  + use `shinyWidgets::dropMenu` instead of `shinyWidgets::dropdown` where possible
* move web UI of `dMeasureQIM` to `dMeasureQIM`

# Version 1.6.3
12th July 2020

## New

* Search for documents on basis of text in Category, Subject, Detail or Comment
  + in `Administration` section
  
## Change

* change in date picker restricting dates (`date_b` cannot be less than `date_a`) and 
  change in date picker restricting dates (`min_date` cannot be less than `max_date`)
  + allows typing in of arbitrary dates, according to restriction

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
