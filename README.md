# "Vacancies and Structural Unemployment"
## An analysis of unemployment and vacancies in Germany

In Germany we have a lot discussions about labour shortage and unemployed people how allegedly do not want to work. But how is it really?
I will give you here some instructions and R code to analys the relationship between unemployment and job vacancies between. The data on job 
vacancies come from the Institut für Arbeitsmarkt - und Berufsforschung (IAB), and the data on the number of unemployed people come from the 
official unemployment statistics of the Bundesagentur für Arbeit. <br>
<br>
Your can download the dataset with vacancies from IAB <a href="https://www.iab.de/stellenerhebung/download">here</a>! <br>
And the official statistic with absolute unemployment rate you can download <a href="https://www.destatis.de/static/de_/opendata/data/arbeitslose_deutschland_originalwert.csv">here</a>! <br>
<br>
Then copy the datasets into the datasets folder <br>
<br>
Make sure that you installed all packages you need for the code.
```  
install.packages("readxl")
install.packages("tidyverse")
install.packages("gt")
install.packages("stargazer")
```
Do not forget to update your working directory!
```
setwd("##### Your directory path ####/Vacancies and structural Unemployment")
```
Run the code and have fun! :)

# Results the code spits out

The first graph shows the time course between unemployment and October 2010 and January 2024. The vacancies from 2022 onwards are estimates.
![vanacies and unemployment](https://github.com/user-attachments/assets/31d5c11b-5f74-4643-bcde-31c94b373ab4)

Next, you get a table in which the missing jobs are calculated. For reasons of space, it only shows some data. The complete table is included at the end of the code. 
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>



</head>
<body>
<div id="rkukgyvprt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

  <table class="gt_table" style="table-layout: fixed;; width: 0px" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <colgroup>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
    <col style="width:100px;"/>
  </colgroup>
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4" scope="colgroup" id="Calculation of missing jobs">
        <span class="gt_column_spanner">Calculation of missing jobs</span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="date">date</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="total&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;">total<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="vacancies&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;">vacancies<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="$\Delta$ Vacancies&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;&quot;&gt;&lt;sup&gt;1,2&lt;/sup&gt;&lt;/span&gt;">$\Delta$ Vacancies<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;"><sup>1,2</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="date" class="gt_row gt_center">2010-10-01</td>
<td headers="total" class="gt_row gt_center">2941</td>
<td headers="vacancies" class="gt_row gt_center">573.3</td>
<td headers="delta_vacancies" class="gt_row gt_center">2367.7</td></tr>
    <tr><td headers="date" class="gt_row gt_center">2011-01-01</td>
<td headers="total" class="gt_row gt_center">3346</td>
<td headers="vacancies" class="gt_row gt_center">574.1</td>
<td headers="delta_vacancies" class="gt_row gt_center">2771.9</td></tr>
    <tr><td headers="date" class="gt_row gt_center">2017-01-01</td>
<td headers="total" class="gt_row gt_center">2777</td>
<td headers="vacancies" class="gt_row gt_center">752.9</td>
<td headers="delta_vacancies" class="gt_row gt_center">2024.1</td></tr>
    <tr><td headers="date" class="gt_row gt_center">2017-04-01</td>
<td headers="total" class="gt_row gt_center">2569</td>
<td headers="vacancies" class="gt_row gt_center">820.5</td>
<td headers="delta_vacancies" class="gt_row gt_center">1748.5</td></tr>
    <tr><td headers="date" class="gt_row gt_center">2022-10-01</td>
<td headers="total" class="gt_row gt_center">2442</td>
<td headers="vacancies" class="gt_row gt_center">1631.7</td>
<td headers="delta_vacancies" class="gt_row gt_center">810.3</td></tr>
    <tr><td headers="date" class="gt_row gt_center">2023-01-01</td>
<td headers="total" class="gt_row gt_center">2616</td>
<td headers="vacancies" class="gt_row gt_center">1324.5</td>
<td headers="delta_vacancies" class="gt_row gt_center">1291.5</td></tr>
    <tr><td headers="date" class="gt_row gt_center">2023-04-01</td>
<td headers="total" class="gt_row gt_center">2586</td>
<td headers="vacancies" class="gt_row gt_center">1353.7</td>
<td headers="delta_vacancies" class="gt_row gt_center">1232.3</td></tr>
    <tr><td headers="date" class="gt_row gt_center">2023-07-01</td>
<td headers="total" class="gt_row gt_center">2617</td>
<td headers="vacancies" class="gt_row gt_center">1296.9</td>
<td headers="delta_vacancies" class="gt_row gt_center">1320.1</td></tr>
    <tr><td headers="date" class="gt_row gt_center">2023-10-01</td>
<td headers="total" class="gt_row gt_center">2607</td>
<td headers="vacancies" class="gt_row gt_center">1450.6</td>
<td headers="delta_vacancies" class="gt_row gt_center">1156.4</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;"><sup>1</sup></span> Values in thousands</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;line-height: 0;"><sup>2</sup></span> From 2020 onwards, the calculation is based on estimates from the IAB</td>
    </tr>
  </tfoot>
</table>
</div>
</body>
</html>

The next thing the code does is calculate a regression model and output it as a table.

<table style="text-align:center"><caption><strong>Regression Results</strong></caption>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>total</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Constant</td><td>3,253.375<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(80.272)</td></tr>
<tr><td style="text-align:left">vacancies</td><td>-0.633<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.086)</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>54</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.509</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.499</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>190.236 (df = 52)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>53.813<sup>***</sup> (df = 1; 52) (p = 0.000)</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

And finally there is a visualization of the regression.
![vanacies unemployment regression](https://github.com/user-attachments/assets/a0df0530-ff80-4ab3-a002-29dbe4fe540b)

