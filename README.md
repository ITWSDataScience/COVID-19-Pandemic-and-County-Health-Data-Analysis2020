# Team5_2020

This project aims to promote healthy lives and well-being for people of all ages and from all backgrounds. 
In this project, the team explores county-level health statistics in the COVID-19 pandemic and seeks to find the correlated factors to the mortality rate. 
The team aims to explain the underlying causes for the wildly varying COVID-19 impacts across the U.S., with an emphasis on Massachusetts. 

To achieve that, the team finds two datasets: 2019 County Health Rankings National Data(Robert Wood Johnson Foundation, University of Wisconsin Population Health Institute) and COVID-19 Data Repository (Center for Systems Science and Engineering at Johns Hopkins University).
The datasets are chosen based on the team's previous experience related to health and are found on their official online websites. 
Our team pushed it directly to the securely hosted GitHub repository for the County Health Rankings data as R objects.

For the COVID-19 data, the team fetched the CSV directly in the R scripts.
2019 County Health Rankings National Data (Robert Wood Johnson Foundation, University of Wisconsin Population Health Institute) is in the format of XLSX. 
The detailed documentation and the metadata are stored on its first introduction spreadsheet in explicit texts. 
Important factors taken into the analysis are federal information processing standard, state, county, unemployment, income inequality, uninsured, etc. Since XLSX is a proprietary format, the team converts and preprocesses the data into RDS format for our research purposes.

COVID-19 Data Repository (Center for Systems Science and Engineering at Johns Hopkins University) is in CSV format.The detailed documentation and the metadata are elaborated in the README files on its GitHub repo

The County Hospital Bed Homeland Infrastructure Foundation-Level Data (HIFLD)

For this project, essential fields are federal information processing standard, state, region, deaths, confirmed, recovered, etc. 

Thanks to the existing datasets' outstanding quality, the team can understand and preprocess the data easily without any obstacles.

source link: 
County Health Rankings https://www.countyhealthrankings.org/app/new-york/2020/overview
Goals: https://www.un.org/sustainabledevelopment/sustainable-development-goals/
Covid-19 Repository： https://github.com/CSSEGISandData/COVID-19
County hospital Bed: https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals
