# linen

> Spreadsheet Data Structures

Spreadsheet data structures, common across different spreadsheet providers.

## Installation

```r
devtools::install_github("jennybc/linen")
```

## Design notes

We're using R6 so that we can mantain links back to the original sources, and so that sheets (and cell ranges) can mantain links back to workbooks that they are related to.  Hopefully little of that will be totally apparent to the user.

The driver packages (googlesheets and rexcel) will `Import` this package; it will not depend directly on them.  So we'll need to provide some hooks that can be used to refresh contents and things like that.
