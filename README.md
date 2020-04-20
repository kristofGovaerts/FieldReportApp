# FieldReportApp
 For visualizing a field and generating markdown reports from GUI. Instructions for use are outlined below. Instructions should be followed sequentially, as each tab's output is dependent on what is defined in the previous tabs. 

 ## 1. Fieldmap tab
 ![Example](/Example01.PNG)
 
 1. Upload a field ebook specifying **field name** (column name: *Field Id*), **series** (column name: *Series Id* **seedname** (column name: **'Seed Name'**), and **X/Y coordinates** (column names: *X* and *Y*). The field layout will then be visualized. It is possible to select which series will be further analyzed. For plotting purposes, a column called **Standard Ind** should also be present.
 
 2. Upload a file with the parameters per plot. It should have **X/Y** columns matching those in the field ebook, and a numeric **time** column. Parameters per plot should be numeric, and have an underscore separating the type of parameter from the type of summary statistic used to generate plot statistics (e.g. *ndvi_mean*, *cover_max*). It is then possible to subset the dataset by selecting series, timepoints and parameters to analyze. Default parameter analyzed is '_mean'. 
 
 ## 2. Field data tab 
 ![Example](/Example02.PNG)
 
 This tab allows for the visualization of plot statistics. Choose a parameter from the dropdown menu to visualize, and a timepoint for the heatmap. The heatmap is not subsetted by series. The checks (identified by 'Y' in the 'Standard Ind' column) are plotted across their entire time series. 
 
 ## 3. Data processing tab
  ![Example](/Example03.PNG)
    
This tab shows the different series, timepoints, parameters that will be analyzed, as well as the total number of analyses. As SpATS can take some time to process, it is important to keep in mind that the more parameters and timepoints are analyzed, the longer it will take. **Genotype** is set as a **random variable** by default. Click **Start SpATS** to start the analysis.
    
 ## 4. SpATS output tab 
  ![Example](/Example04.png)

SpATS output is visualized here. Choose parameter and timepoint to be visualized from the dropdown menu, and save the output as an Excel table if desired. The output table will have 3 tabs: **Raw**, with predicted values per timepoint. **Preds**, a tidy dataset with time in a seperate column, as well as providing scaled (1-9) data. **AUDPC**, with area under the disease progression curve per seedname, as well as providing scaled (1-9) data.

## 5. Author

© **Kristof Govaerts, PhD** - *[kristof.govaerts@sesvanderhave.com]*

© **SESVanderhave n.v.** - *Industriepark 15, 3300 Tienen* 
