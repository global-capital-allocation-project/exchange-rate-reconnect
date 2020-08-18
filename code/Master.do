
*************************
*** Replication of "Exchange Rate Reconnect"
*** 	Lilley, Maggiori, Neiman, Schreger
*** Before running, edit line 1 of Load_Globals.do so that user_dir points to
*** 	the directory which is the parent of the directories "code "and "data" .
*************************

do "Load_Globals.do"

*************************
*** Build data from data/raw to data/output
*************************

do "build/Prep_FX.do"
do "build/IMF_BoP.do"
do "build/Fundamentals.do"

*************************
*** Work with files from data/output
*************************

do "analysis/Analysis.do"
