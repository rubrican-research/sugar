@echo off
SET THEFILE=R:\Data\Dev\Libraries\sugar\tests\toast\bin\toasttest.exe
echo Linking %THEFILE%
R:\Apps\Lazarus\3.6\fpc\3.2.2\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections   --subsystem windows --entry=_WinMainCRTStartup    -o R:\Data\Dev\Libraries\sugar\tests\toast\bin\toasttest.exe R:\Data\Dev\Libraries\sugar\tests\toast\bin\link907540.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
