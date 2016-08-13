date /t >%temp%\date.now
time /t >%temp%\time.now

for /f "tokens=1,2,3 delims=. " %%i in (%temp%\date.now) do (
Set TheDateYMD=%%k-%%j-%%i
Set YYYY=%%k
set MM=%%j
set DD=%%i
)

for /f "tokens=1,2,3 delims=: " %%i in (%temp%\time.now) do (
Set HH=%%i
set NN=%%j
)
del %temp%\date.now
del %temp%\time.now
