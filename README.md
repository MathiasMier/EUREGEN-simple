# EUREGEN-simple
Basic model without complicated stuff
1) Download zip-file and unpack (leave folder structure as it is)
2) Get CPLEX or Gurobi solver (e.g., https://www.gurobi.com/features/academic-named-user-license/); Gurobi is actually manually activated, to use CPLEX go to line 1365 and place the * in the beginning of that line (remove the * in line 1366)
3) Download databases from https://zenodo.org/records/10413375 and put into database
4) Open shell and run the bat-files "run_31segments" (for database v1111xx_3d_1d) or "run_119segments" (for database v1111_4d_1d)
5) Consult "Output" for all variables and "Report" for condensed
6) There is a simple visualization routine via excel
