# linen

> Spreadsheet Data Structures

Spreadsheet data structures, common across different spreadsheet providers.

**Warning: This project is in the early scoping stages; do not use for anything other than amusement/frustration purposes**

## Installation

```r
devtools::install_github("jennybc/linen")
```

## Design notes

We're using R6 so that we can mantain links back to the original sources, and so that sheets (and cell ranges) can mantain links back to workbooks that they are related to.  Hopefully little of that will be totally apparent to the user.

The driver packages (googlesheets and rexcel) will `Import` this package; it will not depend directly on them.  So we'll need to provide some hooks that can be used to refresh contents and things like that.

## Formatting

Formatting information is this bottomless pit that never stops; it's just not possible to store all the information that might possibly be used in a spreadsheet and at the same time allow it to be used in a consistent way.  This is particularly the case before get a full set of data out of google sheets, because the set of information is not really known.

In Excel, formatting information is stored in a series of tables:

* A cell has an index to a "style"
* That style includes lookups to a number of other tables of information on fonts, fills, borders, etc.
* The underlying tables have a bunch of information about colours and the like

The `linen_style` function standardises this in a way that hopefully scales.  We have any number of tables which have any number of columns.  Each table has an entry in the `lookup` table which is special.
