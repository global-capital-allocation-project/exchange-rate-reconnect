# Replication code for Lilley, Maggiori, Neiman, Schreger: "Exchange Rate Reconnect"
==============

I. INTRODUCTION
--------------

This README describes the overall structure of the Replication packet for this paper. The code for our project is organized in two steps. First, we have a build file, which takes a number of publicly available price series and concordance files contained in `data/raw` and prepares them for analysis, saving the prepared version of this data in `data/output`. Second, we have an analysis file, where we take the output of these files and generate the tables and figures used in our paper. The uppermost directory of the replication folder therefore has the following seven objects:

  i.    	`README.md` (the file you are reading right now)
  
  ii.   	`README.pdf` (the file you are reading right now)
  
  iii.  	`code` (a folder)
  
  iv.  		`data` (a folder)
  
  v.   		`graphs` (a folder for storing output figures)
  
  vi.   	`regs` (a folder for storing output regression tables)
  
  vii.   	`instructions.txt` (a guide to running the code)
  

II. INSTRUCTIONS
--------------

Step 1: Edit line 1 of `code/Load_Globals.do` so that the global variable user_dir points to your directory which contains this readme file

Step 2: Run `code/Master.do`. The master file will in turn 

    a) define the necessary globals 
    
    b) run the build file [optional, takes 15 minutes to 1 hour, comment out this line to use the prebuilt output files]
    
    c) run the analysis code to produce the figures and tables in the paper
  

III. PROPRIETARY DATA
--------------

Most results in the paper are replicable using solely public data, in which case the raw files can be found in the zip file on globalcapitalallocation.com. Tables and figures in the paper which contain references to "Morningstar" in the notes make use of proprietary data. The requirements for building this dataset are described in MNS_Data_Guide.pdf contained in the github repository global-capital-allocation-project/international-currencies-and-capital-allocation.

We provide the code, but not the data, which generates these results for the researcher's interest. The analysis code includes a check to see whether the necessary data files exist before executing each piece of analysis, in order to replicate as many results as possible without needing the full proprietary dataset.


IV. REFERENCES
--------------
Coppola, Antonio, Matteo Maggiori, Brent Neiman, and Jesse Schreger, “Redrawing the Map of Global Capital Flows: The Role of Cross-Border Financing and Tax Havens,” 2019.

Du, Wenxin, Joanne Im, and Jesse Schreger, “The U.S. Treasury Premium,” Journal of International Economics, 2018, 112, 167–181

He, Zhiguo., Bryan Kelly, and Asaf Manela, “Intermediary asset pricing: New evidence from many asset classes”. Journal of Financial Economics 2017, 126, 1–35.

Gilchrist, Simon and Egon Zakrajšek, “Credit spreads and business cycle fluctuations,” American Economic Review, 2012, 102 (4), 1692–1720.

Maggiori, Matteo, Brent Neiman, and Jesse Schreger, “International Currencies and Capital Allocation,” Forthcoming in Journal of Political Economy, 2019.

Miranda-Agrippino, Silvia, and Hélène Rey, “US Monetary Policy and the Global Financial Cycle,” NBER Working Papers (No. 21722). National Bureau of Economic Research
