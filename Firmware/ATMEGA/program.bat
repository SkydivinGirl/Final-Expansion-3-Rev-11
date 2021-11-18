cls
avrdude -F -c usbasp -p m1284p -U flash:w:newboot-0.4.1-larsp-m1284p.hex:i
pause
cls
avrdude -F -c usbasp -p m1284p -U lfuse:w:0xef:m -U hfuse:w:0x92:m -U efuse:w:0xfd:m
pause
cls
