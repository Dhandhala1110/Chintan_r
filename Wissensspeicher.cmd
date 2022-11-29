@echo off

echo Start des interaktiven Wissensspeichers

:: Pfad an Installationspfad von R --> Bei Bedarf anpassen
SET PATH=%PATH%;c:\Program Files\R\R-4.1.1\bin\x64

:: Navigationspfad Zu Quellcode der Applikation --> Bei Bedarf anpassen
cd C:\Users\Julian\Documents\Studium\Semester_7\BPP\Git\Blasformen_Wissensspeicher

Rscript start.R

pause