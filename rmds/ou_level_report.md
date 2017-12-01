Operating Unit Level (OU Level)
================

``` r
library(kableExtra)
```

    ## Warning: package 'kableExtra' was built under R version 3.3.3

``` r
library(xtable)
```

    ## Warning: package 'xtable' was built under R version 3.3.3

    ## 
    ## Attaching package: 'xtable'

    ## The following object is masked from 'package:formattable':
    ## 
    ##     digits

Overall Achievement
-------------------

This table shows orverall achievement of the country from FY15 to FY17

``` r
knitr::kable(ou_level)
```

| indicator        |  fy2015apr|  fy2016apr|  fy2017q1|  fy2017q2|  fy2017q3|  fy2017q4|  fy2017\_targets|  fy2017Cum|  fy2017Perf|
|:-----------------|----------:|----------:|---------:|---------:|---------:|---------:|----------------:|----------:|-----------:|
| OVC\_HIVSTAT     |          0|          0|         0|     33489|         0|     50412|                0|      50412|         0.0|
| OVC\_SERV        |      56192|      80580|         0|     78160|         0|    110516|            78291|     110516|       141.2|
| TX\_CURR         |      66528|      80946|     82413|     85195|     87929|     91845|            93581|      91845|        98.1|
| TX\_PVLS         |          0|          0|         0|         0|         0|     39406|            70191|      39406|        56.1|
| TX\_RET          |      13783|      12535|         0|         0|         0|     14863|            17196|      14863|        86.4|
| HTS\_TST         |    1138886|    1228340|    275096|    294274|    308040|    291386|           985347|    1168796|       118.6|
| HTS\_TST\_POS    |      29756|      26856|      5608|      6335|      6235|      6816|            27152|      24994|        92.1|
| PMTCT\_ART       |       4665|       4133|       903|      1122|      1179|      1091|             4693|       4295|        91.5|
| PMTCT\_EID       |       6282|       3938|      1270|      1387|      1175|      1229|             4523|       5061|       111.9|
| PMTCT\_EID\_POS  |        438|        246|        74|        56|        81|        82|              197|        293|       148.7|
| PMTCT\_STAT      |     228045|     215895|     49095|     53455|     58229|     49989|           237460|     210768|        88.8|
| PMTCT\_STAT\_POS |       4962|       4485|       833|      1087|      1212|      1154|             4798|       4286|        89.3|
| TB\_ART          |       1352|       1253|         0|       813|         0|       705|             1936|       1518|        78.4|
| TB\_STAT         |      10866|      11437|         0|      5973|         0|      5103|            13500|      11076|        82.0|
| TB\_STAT\_POS    |       1472|       1691|         0|       957|         0|       902|                0|       1859|         0.0|
| TX\_NEW          |      17895|      22468|      5252|      5539|      5352|      5655|            26335|      21798|        82.8|
| TX\_NET\_NEW     |          0|      14418|      1467|      2782|      2734|      3916|            12635|      10899|        86.3|

Including Plots
---------------

You can also embed plots, for example:

![](ou_level_report_files/figure-markdown_github-ascii_identifiers/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
