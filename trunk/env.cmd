@ECHO OFF
REM --------------------------------------------------------------------------
REM NewView - a new OS/2 Help Viewer
REM
REM Copyright 2003-2006 Aaron Lawrence (aaronl at consultant dot com)
REM Copyright 2006 RonaldBrill (rbri at rbri.org)
REM
REM This software is released under the Gnu Public License
REM
REM
REM Setup for the environment used to build NewView.
REM You have to adjust the various paths
REM --------------------------------------------------------------------------


REM --------------------------------------------------------------------------
REM Path to the OpenWatcom compiler. Change this if needed.
REM
SET WATCOM=D:\progs\watcom
REM --------------------------------------------------------------------------


REM --------------------------------------------------------------------------
REM Output Directory.
REM Don't change this. There is no way to use this in the Sibyl project files.
REM Please use a tool like NetDrive to ensure this path exists.
REM
SET NV_DEV=P:\newview_dev
REM --------------------------------------------------------------------------

REM --------------------------------------------------------------------------
REM Enable debuging menus.
SET NEWVIEW_DEBUG=TRUE
REM --------------------------------------------------------------------------


REM --------------------------------------------------------------------------
REM Do not change one of the following lines
REM

SET NV_BUILD=%NV_DEV%\build
SET PMPRINTF_PATH=%NV_DEV%\tools\pmprintf

SET PATH=%WATCOM%\BINP;%WATCOM%\BINW;%PATH%
SET INCLUDE=%WATCOM%\H;%WATCOM%\H\OS2;%PMPRINTF_PATH%;
SET HELP=%WATCOM%\BINP\HELP;%HELP%
SET BOOKSHELF=%WATCOM%\BINP\HELP;%BOOKSHELF%
SET BEGINLIBPATH=%WATCOM%\BINP\DLL


