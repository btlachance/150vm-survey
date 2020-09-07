# Overview

Renders responses to our course survey as pandoc Markdown

# Install

`raco pkg install --clone 150vm-survey git://github.com/btlachance/150vm-survey.git`

# Uninstall

`raco pkg remove 150vm-survey`

# Examples

```
cd 150vm-survey
racket reports.rkt --sample
racket reports.rkt --qualtrics responses.csv
racket reports.rkt --qualtrics responses.csv --openended-grouped
```
