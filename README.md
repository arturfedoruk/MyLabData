# My Lab Data (MLD)

This is a small package for my personal use for processing data for my lab works at MIPT.

Main feature is extensive use of Wolfram Mathematica's **Datasets**, as well as syntax for using values with units (`Quantity`), 
and most of the code are functions for easy dealing with all that stuff, e.g.:

* Function to easy read data from Excel (`MLDReadExcel`) into Wolfram Mathematica list,
and then easy transforming it into Dataset (`MLDMakeLabData` & `MLDMakeLabDataWOUnits`)
* Making columns with values with errors from column of means and column of errors (`MLDMakeColumnWErrors`)
* Renaming columns (`MLDRenameColumn`)
* Fitting data straight from your Dataset (`MLDNonLinearFit` & `MLDLinearFit`)
(basically small adjustments for `NonlinearModelFit` & `LinearModelFit`)
* Converting data into TeX format (`MLDTexTable`)
* Some notes on which functions aer there in MLD (`MLDHelp`), on DataSet syntax (`MLDDatasetHelp`)
* and on plotting options (`MLDPlotHelp`) (added those bc i myself kept forgetting lol)

There are also commentaries for each function, to see them run 
```
? MLDsomefunction
```

Also there is an example of using those functions (`MyLabData_example.nb`)

Maybe it would be nice if you credit me if you use this code somewhere serious, idk

Good luck!
