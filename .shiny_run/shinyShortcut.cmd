REM @echo off > NUL
REM "C:\Program Files\R\R-4.1.1\bin\Rscript.exe" -e "shiny::runApp('C:/rgfsem/RGfSEM-master/RSfSEM.R', launch.browser = TRUE)"

@echo off > NUL
for /f "tokens=*" %%g in ('where Rscript') do (set rpath=%%g)
"%rpath%" -e "shiny::runApp('D:/Work/Shiny/rsfsem/RSfSEM.R', launch.browser = TRUE)"