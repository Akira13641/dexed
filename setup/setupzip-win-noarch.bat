cd output
7z a -tzip -mx9 %~n1.zip %1
if exist %1 del %1
if exist %~n1.obj del %~n1.obj