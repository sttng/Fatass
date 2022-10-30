isas32 -isdmg -w 2 fatass
@if errorlevel 1 goto done
islk32 -v -w bank-over fatass > banks.txt
@if errorlevel 1 goto done
del fatass.gb
@rem cvtisx -b0,2m -mdmg fatass.isx fatass.gb
@rem rgbfix -v fatass.gb
abisx fatass.isx /bfatass.gb /sfatass.sym /f66666666 /n /v
@if exist fatass.gb goto run
goto done
:run
@rem c:\progra~1\gamebo~1\dboy\dboy -cgb -nocrc rem c:\windows\desktop\fatass\fatass.gb
@rem c:\dev\nocash\no$gmb c:\windows\desktop\fatass\fatass.gb
c:\dev\no$gmb\no$gmb c:\windows\desktop\fatass\fatass.gb
@rem ..\gbt16 -l fatass.gb
:done

