REM Run this from a VS2017 console
REM Which is itself run from clink
REM
call "%VS150COMNTOOLS%\..\..\VC\Auxiliary\Build\vcvarsall.bat" amd64

C:\Program Files (x86)\ ecl2 -load tools\preload.lisp

clpm bundle exec -- "C:\Program Files (x86)\ECL\ecl.exe" -load tools/preload.lisp