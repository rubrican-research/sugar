@echo off
SET THEFILE=R:\Data\Dev\Libraries\sugar\src\tests\bin\testalpinejs.exe
echo Linking %THEFILE%
R:\Apps\Lazarus\3.0.0\fpc\3.2.2\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections  -s --subsystem windows --entry=_WinMainCRTStartup    -o R:\Data\Dev\Libraries\sugar\src\tests\bin\testalpinejs.exe R:\Data\Dev\Libraries\sugar\src\tests\bin\link7092.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
