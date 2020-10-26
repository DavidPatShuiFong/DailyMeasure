# GP stat! (c) David Fong, 2019

front-end to DailyMeasure package

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
  by lack of subscription as reported by check_subscription_datechange_trigR

# Version 1.5.9
3rd May 2020

## New

* Download list of configured users, including 'Identifiers'
  - for subscription application purposes
* 'bounce' reminder on update_clinicians and update_date
  button if chosen clinicians or dates change (shinyjqui effect)
* Contact filter includes date of last contact (minimum and maximum dates)

## Change

* chosen clinician list has an 'Update' button
* Billings view adapted to change in billings module (version 0.4.0)
  dMeasureBillings::list_billings no longer provides print/HTML tags
  use dMeasureBillings::tag_billings_list to add print/HTML tags
  + improvement in billings view to reduce time to change from
    print to HTML 'button' view


# Version 1.5.8
24th April 2020

## New

* asthma view in Conditions tab
* 'include up-to-date' option for cancer screening
* COVID-19 bulk billing incentive prompt in billing view
* filters for different billing typess (private, WorkCare, DVA, 'bulk'/direct)
  in billing view

## Fix

* fixed logic for updating 'date_to' input on right side-panel

# Version 1.5.7
8th April 2020

## New

* Walkthrough (introduction) for appointments
* Walkthrough (introduction) for immunizations
* Walkthrouhg (introduction) for cancer screening
* Walkthrough (introduction) for billings
* Walkthrough (introduction) for CDM
* Walkthrough (introduction) for Conditions - Post-natal
* Walkthrough (introduction) for Quality Improvement Measures

## Change

* Updated video demonstration of features

## Fix

* fix to Javascript errors when displaying frisk/friskHI in qim_cvdRisk
* fix Javascript error when displaying an empty table in QIM active (report)

# Version 1.5.6
1st April 2020

## New

* 'Contact' option for CDM - list CDM opportunities by past contact
* 'include up-to-date' option for immunization/vaccination


# Version 1.5.5
30th March 2020

## New

* 'Contact' option for Immunization - list immunizations opportunities by past contact

# Version 1.5.4
25th March 2020

## New

* Custom tab

# Version 1.5.3
7th March 2020

## New

* if date changed because of no subscription, the list of user without subscription
  is shown in the alert
* introductory walkthrough with 'rintrojs'
* can choose Database Server Driver
* changes to allow shinyapps.io deployment
   app.R dependencies.R
   DailyMeasureUI and DailyMeasureServer are now exported objects

## Improvements & Fixes

* server database password is 'maintained' (if not modified) during server description edit
  bug fixed where sometimes database password was not useable until after restart
* warning if configuration database could not be successfully opened

# Version 1.5.2
20th February 2020

## Fix

* fix to demonstration mode

# Version 1.5.1
19th February 2020

## New

* subscription features
  +  reads subscription database (user-action, from user configuration panel)
  +  CDM and BIllings UI checks subscription status
  +  if clinician selected with no subscription, then
    appointments must be minimum one week old
  +  user configuration displays license information
  +  subscription information can be added manually in user configuration
  + updates date range dialog if dM$date_aR or dM$date_bR changes

## Changes

* warnings regarding changing configuration filepath location
   GPstat! restart recommended.
   though changing configuration filepath appears to work 'on-the-fly' currently!
* better handling if Best Practice database not opened


# Version 1.4.1
22nd December 2019

## Features

* modified module_Immunization to depend on dM$vaccine_choices to choose vaccines.
  (in effect, adds 'measles vaccine' to list of choices without
  explicitly adding 'measles' as a vaccine coice from within GPstat!)

* Conditions tab

  search for post-natal (or potentially post-natal) patients.
  Filtered by number of days post-natal, whether a 'visit' recorded by the
  selected physicians during the potential pregnancy period, and any
  recorded pregnancy outcome.

  Attached to appointments within the defined appointment search period.

# Version 1.4.0
12th November 2019

## Features

* demonstration mode disables log file choosing/creation
* Data Quality tab - allergies and social history

# Version 1.3.7
5th November 2019

## Features

* define host and port number in GPstat()
* multiple sessions can be opened. app does not stop until all sessions are closed
* demontration mode option in GPstat()

## Bugfix

* remove 'Download' option/button from HTML view of Billings module

# Version 1.3.6
28th September 2019

## Improvement/bugfix

* Added 'Invoiced' to contact_appointments types (reflects changes in dMeasure)

# Version 1.3.5
27th September 2019

## Improvement

* round framingham risk equation results to 3 decimal places

## Bug fixes

* COPD patients in QIM COPD appointments, instead of diabetic patients

# Version 1.3.4
23rd September 2019

## Bug fixes

* Fix crash if trying to access logs when no logs database is open
* Fix attempts to download 'graphical' (fomantic/semantic) tables
* Prevent opening of Chronic Disease Management (CDM) tab if Billings module not available

# Version 1.3.3
21st September 2019

## Modularization

* dMeasureQIM (quality improvement measures), dMeasureBillings (Billings) and
  dMeasureCDM (chronic disease management) are now an optional modules/packages.
  changes to UI and server to allow dynamic creation of tabItems and sidebarMenu


# Version 1.3.2
18th September 2019

## Improvements

* Quality Improvement Measures appointments view
  Uses separate package version of dMeasureQIM

* datatable_styled accepts arguments which allow
  selectable button extensions

* Remove DiabetesSIP and AsthmaSIP from default CDM list

  These items are no longer available for billing

## Bug-fix

* Move demographic_group and ignoreOld in QIM Measures
  to 'common' titlebar area

  Having separate demographic_group buttons could cause
  a race-condition

# Version 1.3.1
8th September 2019

* Requires dMeasure back-end 0.4.99

* Changes to billing display
  Integrates appointment, visit and billing view

* Bugfix : view Quality Improvement Measures failed if no open database
