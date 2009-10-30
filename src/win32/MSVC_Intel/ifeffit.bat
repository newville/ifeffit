@echo off
REM
REM  set this to point to your installation directory
SET IFEFFIT_DIR=C:\IFEFFIT


REM  Should be no need to edit past this line:

SET IFEFFIT_EXE=ifeffit10.exe
SET PATH=%PATH%;%IFEFFIT_DIR%
SET PGPLOT_DIR=%IFEFFIT_DIR%
SET PGPLOT_DEV=/GW

CD %IFEFFIT_DIR%
%IFEFFIT_DIR%\%IFEFFIT_EXE%


