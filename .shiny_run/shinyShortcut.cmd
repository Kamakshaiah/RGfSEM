@echo off > NUL
"C:\Program Files\R\R-4.1.1\bin\Rscript.exe" -e "shiny::runApp('C:/rgfsem/RGfSEM-master/RSfSEM.R', launch.browser = TRUE)"
