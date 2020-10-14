# Overview

Renders responses to our course survey as pandoc Markdown

# Install

`raco pkg install --clone 150vm-survey git://github.com/btlachance/150vm-survey.git`

* The code is shipped as a Racket package to manage Racket-level
  dependencies, but there is currently no Racket-level interface for
  dealing with the surveys. No Racket collections are installed by
  installing this package.

# Uninstall

`raco pkg remove 150vm-survey`

# Examples

```
cd 150vm-survey
racket reports.rkt --sample
racket reports.rkt --qualtrics responses.csv
racket reports.rkt --qualtrics responses.csv --openended-grouped
```
