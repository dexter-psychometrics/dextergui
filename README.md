# Dextergui

Dextergui is a graphical user interface for the main functionality of [dexter](https://dexter-psychometrics.github.io/dexter/) for use in educational and psychological measurement. It offers Classical Test Analysis, Item Response Theory and Data management.

## Installation

```
install.packages("dextergui")
```
or for the development version

```
library(devtools)
install_github('dexter-psychometrics/dextergui')
```


If you encounter a bug, please post a minimal reproducible example on [github](https://github.com/dexter-psychometrics/dexter/issues).

## Usage

```
dextergui()
```

This will open the user interface window in a shiny window. Switch to a Chrome, Firefox or Brave browser for best results&ast;. You must start a new project or open an existing project before more options will become available. For more information look on the help page in dextergui or read the [vignette](https://CRAN.R-project.org/package=dextergui/vignettes/dextergui.html). 

&ast; it also works in Internet Explorer but it does *not* work in Microsoft Edge. Not tested in Safari, Opera or other browsers.

