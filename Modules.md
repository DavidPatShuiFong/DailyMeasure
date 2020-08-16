# Module interface

## Module discovery {#module_discovery}

* `DailyMeasure::GPstat`'s `DailyMeasureServer.R` will search for installed packages (using the `installed.packages`) which start with the name `dMeasure`
* `GPstat()` interrogates the namespace of those packages which have a name which starts with `dMeasure` for the function `dMeasureIntegration`.

## Module features {#module_features}

* For those packages found through [Module discovery](#module_discovery), that package's `dMeasureIntegration` function is called with
  + the parameter `information = "Provides"` - list of one, or can be a list of two or more
  + the parameter `information = "Requires"` - list of the name(s) of the `Provides` required
    * these will be used as parameters (in the same order of the list) when calling the module with the `$new` method
  + the parameter `information = "moduleID"` - will be used as a (shiny) module ID
    * can be a single 'character' entry e.g. `Custom_dt`
    * can be a list of lists containing `ID` and `extraArgs` e.g `list(list(ID = "qimRept", extraArgs = "contact = TRUE"), list(ID = "qimAppt", extraArgs = "contact = FALSE"))`
  + the parameter `information = "configID"`
  
* `dMeasure` itself has the following:
  + `"Provides"` = `list("dMeasure")`
  + `"Requires"` = `list(NULL)`
  
## Loading modules

* `DailyMeasure::GPstat` iterates through the module list. If all the `"Requires"` are defined (as described in each module's `"Provides"`) then the `$new` method is called on the `R6::R6Class` which has the *same* name as the package e.g. `dMeasureQIM` has an R6 class named `dMeasureQIM`.
  + The R6 objects, which have the same name as the list in `"Requires"`, are used as parameters, in the same order as the list. `dMeasure` is a common `Requires`!
  + the new R6 object is stored in `dMeasureModulesR6` with theh same name as in the `Provides`.


## Configuration database interaction

* If the module exports the *function* `read_configuration_db`, then the *method* `read_configuration_db` is called on the R6 object created during the [Loading modules](#loading modules) phase.
  + an examples is in `dMeasureCustom`

## Configuration user interface

* If the module exports the *function* `dMeasureConfigurationTabPanelItem`, then the function is called and the results are added to configuration_tabPanels.
  + an example is in `dMeasureCustom`
* If the module exports the *function* `dMeasureConfigurationTabPanel`, then the function is called with the parameters 
  + `configID` and
  + the R6 object created with the same name as in `Provides`
  + both `configID` and `Provides` are defined in the [Module features](#module_features)

## (Shiny) user interface

* If the module exports the *function* `shinydashboardmenuItem`, the results of this function is added to the sidebarmenu.
* If the module exports the *function* `dMeasureShinytabItems`, the results of this function is added to shinytabItems.

## (Shiny) modules

* For each definition of `moduleID` (see [Module features](#module_features)), the exported *function* named `datatableServer` (from the package which was used to create the R6 object) is called.
  + `datatableServer` is called with parameters:
    * `moduleID`
    * the R6 object the (index) name defined by `Provides` - also defined in [Module features](#module_features)
    * `$extraArgs`, if `moduleID` defined as a list, rather than a single character string. (see [Module features](#module_features))

