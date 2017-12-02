Operating Unit Level (OU Level)
================

``` r
library(kableExtra)
library(xtable)
```

Overall Achievement (OU Level)
------------------------------

This table shows orverall achievement of the country from FY15 to FY17

``` r
knitr::kable(ou_level,"html") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = F, position = "float_right",font_size = 12)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="font-size: 12px; width: auto !important; float: right; margin-left: 10px;">
<thead>
<tr>
<th style="text-align:left;">
indicator
</th>
<th style="text-align:right;">
fy2015apr
</th>
<th style="text-align:right;">
fy2016apr
</th>
<th style="text-align:right;">
fy2017q1
</th>
<th style="text-align:right;">
fy2017q2
</th>
<th style="text-align:right;">
fy2017q3
</th>
<th style="text-align:right;">
fy2017q4
</th>
<th style="text-align:right;">
fy2017\_targets
</th>
<th style="text-align:right;">
fy2017Cum
</th>
<th style="text-align:right;">
fy2017Perf
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
OVC\_HIVSTAT
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
33489
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
50412
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
50412
</td>
<td style="text-align:right;">
0.0
</td>
</tr>
<tr>
<td style="text-align:left;">
OVC\_SERV
</td>
<td style="text-align:right;">
56192
</td>
<td style="text-align:right;">
80580
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
78160
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
110516
</td>
<td style="text-align:right;">
78291
</td>
<td style="text-align:right;">
110516
</td>
<td style="text-align:right;">
141.2
</td>
</tr>
<tr>
<td style="text-align:left;">
TX\_CURR
</td>
<td style="text-align:right;">
66528
</td>
<td style="text-align:right;">
80946
</td>
<td style="text-align:right;">
82413
</td>
<td style="text-align:right;">
85195
</td>
<td style="text-align:right;">
87929
</td>
<td style="text-align:right;">
91845
</td>
<td style="text-align:right;">
93581
</td>
<td style="text-align:right;">
91845
</td>
<td style="text-align:right;">
98.1
</td>
</tr>
<tr>
<td style="text-align:left;">
TX\_PVLS
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
39406
</td>
<td style="text-align:right;">
70191
</td>
<td style="text-align:right;">
39406
</td>
<td style="text-align:right;">
56.1
</td>
</tr>
<tr>
<td style="text-align:left;">
TX\_RET
</td>
<td style="text-align:right;">
13783
</td>
<td style="text-align:right;">
12535
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
14863
</td>
<td style="text-align:right;">
17196
</td>
<td style="text-align:right;">
14863
</td>
<td style="text-align:right;">
86.4
</td>
</tr>
<tr>
<td style="text-align:left;">
HTS\_TST
</td>
<td style="text-align:right;">
1138886
</td>
<td style="text-align:right;">
1228340
</td>
<td style="text-align:right;">
275096
</td>
<td style="text-align:right;">
294274
</td>
<td style="text-align:right;">
308040
</td>
<td style="text-align:right;">
291386
</td>
<td style="text-align:right;">
985347
</td>
<td style="text-align:right;">
1168796
</td>
<td style="text-align:right;">
118.6
</td>
</tr>
<tr>
<td style="text-align:left;">
HTS\_TST\_POS
</td>
<td style="text-align:right;">
29756
</td>
<td style="text-align:right;">
26856
</td>
<td style="text-align:right;">
5608
</td>
<td style="text-align:right;">
6335
</td>
<td style="text-align:right;">
6235
</td>
<td style="text-align:right;">
6816
</td>
<td style="text-align:right;">
27152
</td>
<td style="text-align:right;">
24994
</td>
<td style="text-align:right;">
92.1
</td>
</tr>
<tr>
<td style="text-align:left;">
PMTCT\_ART
</td>
<td style="text-align:right;">
4665
</td>
<td style="text-align:right;">
4133
</td>
<td style="text-align:right;">
903
</td>
<td style="text-align:right;">
1122
</td>
<td style="text-align:right;">
1179
</td>
<td style="text-align:right;">
1091
</td>
<td style="text-align:right;">
4693
</td>
<td style="text-align:right;">
4295
</td>
<td style="text-align:right;">
91.5
</td>
</tr>
<tr>
<td style="text-align:left;">
PMTCT\_EID
</td>
<td style="text-align:right;">
6282
</td>
<td style="text-align:right;">
3938
</td>
<td style="text-align:right;">
1270
</td>
<td style="text-align:right;">
1387
</td>
<td style="text-align:right;">
1175
</td>
<td style="text-align:right;">
1229
</td>
<td style="text-align:right;">
4523
</td>
<td style="text-align:right;">
5061
</td>
<td style="text-align:right;">
111.9
</td>
</tr>
<tr>
<td style="text-align:left;">
PMTCT\_EID\_POS
</td>
<td style="text-align:right;">
438
</td>
<td style="text-align:right;">
246
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
56
</td>
<td style="text-align:right;">
81
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:right;">
197
</td>
<td style="text-align:right;">
293
</td>
<td style="text-align:right;">
148.7
</td>
</tr>
<tr>
<td style="text-align:left;">
PMTCT\_STAT
</td>
<td style="text-align:right;">
228045
</td>
<td style="text-align:right;">
215895
</td>
<td style="text-align:right;">
49095
</td>
<td style="text-align:right;">
53455
</td>
<td style="text-align:right;">
58229
</td>
<td style="text-align:right;">
49989
</td>
<td style="text-align:right;">
237460
</td>
<td style="text-align:right;">
210768
</td>
<td style="text-align:right;">
88.8
</td>
</tr>
<tr>
<td style="text-align:left;">
PMTCT\_STAT\_POS
</td>
<td style="text-align:right;">
4962
</td>
<td style="text-align:right;">
4485
</td>
<td style="text-align:right;">
833
</td>
<td style="text-align:right;">
1087
</td>
<td style="text-align:right;">
1212
</td>
<td style="text-align:right;">
1154
</td>
<td style="text-align:right;">
4798
</td>
<td style="text-align:right;">
4286
</td>
<td style="text-align:right;">
89.3
</td>
</tr>
<tr>
<td style="text-align:left;">
TB\_ART
</td>
<td style="text-align:right;">
1352
</td>
<td style="text-align:right;">
1253
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
813
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
705
</td>
<td style="text-align:right;">
1936
</td>
<td style="text-align:right;">
1518
</td>
<td style="text-align:right;">
78.4
</td>
</tr>
<tr>
<td style="text-align:left;">
TB\_STAT
</td>
<td style="text-align:right;">
10866
</td>
<td style="text-align:right;">
11437
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5973
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5103
</td>
<td style="text-align:right;">
13500
</td>
<td style="text-align:right;">
11076
</td>
<td style="text-align:right;">
82.0
</td>
</tr>
<tr>
<td style="text-align:left;">
TB\_STAT\_POS
</td>
<td style="text-align:right;">
1472
</td>
<td style="text-align:right;">
1691
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
957
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
902
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1859
</td>
<td style="text-align:right;">
0.0
</td>
</tr>
<tr>
<td style="text-align:left;">
TX\_NEW
</td>
<td style="text-align:right;">
17895
</td>
<td style="text-align:right;">
22468
</td>
<td style="text-align:right;">
5252
</td>
<td style="text-align:right;">
5539
</td>
<td style="text-align:right;">
5352
</td>
<td style="text-align:right;">
5655
</td>
<td style="text-align:right;">
26335
</td>
<td style="text-align:right;">
21798
</td>
<td style="text-align:right;">
82.8
</td>
</tr>
<tr>
<td style="text-align:left;">
TX\_NET\_NEW
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
14418
</td>
<td style="text-align:right;">
1467
</td>
<td style="text-align:right;">
2782
</td>
<td style="text-align:right;">
2734
</td>
<td style="text-align:right;">
3916
</td>
<td style="text-align:right;">
12635
</td>
<td style="text-align:right;">
10899
</td>
<td style="text-align:right;">
86.3
</td>
</tr>
</tbody>
</table>
Overall Indicator achievement
-----------------------------

``` r
print(g_ou_level)
```

![](C:\Users\wsn8\Documents\QuarterlyProgress\rmds\ou_level_report_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)
