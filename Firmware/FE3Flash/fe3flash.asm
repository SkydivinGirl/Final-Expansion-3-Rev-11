
  processor 6502                        ; VIC20


CHRPTR  = $7a                           ; Get Char Pointer
PT1     = $22                           ; Pointer
PT2     = $24                           ; Pointer
PT3     = $14                           ; Pointer

FAC     = $61                           ; Floating Point Accumulator

C_LINE  = $d1                           ; Pointer Current Screen Line Char RAM
C_COLP  = $f3                           ; Pointer Current Screen Line Color RAM

C_ROW   = $d6                           ; Value Cursor Row Current Line
C_COL   = $d3                           ; Value Cursor Column Current Line
C_CHR   = $d7                           ; Value Current Character

KEYANZ  = $c6                           ; Value Characters in Keyboard Buffer

CHRGET  = $73                           ; Kernal Routine Get Character
CHRGOT  = $79                           ; Kernal Routine Get Last Character

BIP     = $0200                         ; BASIC Input Buffer 88 Bytes
CAS_BUF = $033C                         ; Cassette Buffer

IO_FINAL = $9c02                        ; FINAL EXPANSION REGISTER 1 (39938,39939)

FEMOD_START = $00                       ; MODE START
FEMOD_ROM   = $40                       ; MODE EEPROM (READ EEPROM, WRITE RAM)
FEMOD_ROM_P = $20                       ; MODE FLASH EEPROM (READ EEPROM, WRITE EEPROM)
FEMOD_RAM   = $80                       ; MODE SRAM (SRAM 40KB, BANK 0 and BANK 1)
FEMOD_RAM2  = $A0                       ; MODE BIG SRAM (SRAM 512KB, BANK 0 TO 15)

LOADPTR = $c3                           ; Pointer Tape Load Temporary Address
LOADEND = $ae                           ; Pointer End of Program Address

SOFT_RESET = 64802                      ; SOFT RESET
CURSOR_POS = $e50a                      ; Save or Restore Cursor Position

BSOUT      = $ffd2                      ; Kernal Routine Output One Byte
GETIN      = $ffe4                      ; Kernal Routine Get Keyboard Input

START_ADR  = $1200                      ; Program Load Address

  org START_ADR -1                      ; Set Origin

  byte <(START_ADR +1),>(START_ADR +1)
loader_start:
  byte $1b,$10,$d9,$07,$9e,$c2,"(44)",$ac,"256",$aa,$c2,"(43)",$aa,"26",0,0,0     ; 2009 SYSPEEK(44)*256+PEEK(43)+28



; ==============================================================
; START OF CODE
; ==============================================================

PRG_LEN    = FLASHER_E - START_ADR

START
  lda 44
  cmp #>(START_ADR)
  beq START_2

; MOVE CODE
  ldx 43
  dex
  clc
  adc #>(PRG_LEN)
  sta PT2 +1                            ; REAL END ADDRESS
  stx PT2

  ldx #<(START_ADR)
  lda #(>START_ADR + >PRG_LEN)
  stx PT1
  sta PT1 +1                            ; TARGET END ADDRESS

  ldx #>PRG_LEN +1
  ldy #0
START_0
  lda (PT2),y
  sta (PT1),y
  iny
  bne START_0
  dec PT1 +1
  dec PT2 +1
  dex
  bne START_0

  ldx #>(START_ADR)
  stx 44

START_2
  jmp FLASHER


; ==============================================================
; MAIN CODE
; ==============================================================

FLASHER
  sta $a000                             ; UNLOCK IO
  lda #FEMOD_ROM_P                      ; PROG MODE
  sta IO_FINAL
  jsr TestEE
  bcs START_E

  jsr BLANK_CHECK
  beq START_5

  jsr FLASH_ERASE
  bcs START_ERR                         ; ERROR -->

  jsr BLANK_CHECK
  beq START_5

START_ERR
  lda #<MSG_ERROR
  ldy #>MSG_ERROR
  jsr STROUT                            ; Error
  jmp START_E

START_5
  jsr FLASH_FIRMWARE

START_E
  jsr FlashCodeEndSequ                  ; RESET

  lda #FEMOD_ROM                        ; EEP MODE
  sta IO_FINAL
  rts





; ==============================================================
; TEST FOR RIGHT EEPROM TYPE
; ==============================================================

TestEE
  lda #<MSG_VENDOR
  ldy #>MSG_VENDOR
  jsr STROUT                            ; Vendor:

  jsr FlashCodeVendorID                 ; GET VENDOR ID IN X AND DEVICE ID IN Y
  tya
  pha
  cpx #$01                              ; AMD
  beq VENDOROK
  cpx #$c2                              ; AMD by MX
;  beq VENDOROK
;  cpx #$20                              ; AMD by STMicroelectronics
  bne EE_ERR0

VENDOROK
  lda #0
  jsr HEXOUT

  lda #<MSG_DEVICE
  ldy #>MSG_DEVICE
  jsr STROUT                            ; Device:
  pla
  tax
;  ldx #$a4
  cpx #$a4                              ; AMD or MX 29F040
;  beq DEVICEOK
;  cpx #$20                              ; STMicroelectronics 29F040
;  cpx #$e2                              ; STMicroelectronics 29F040
  bne EE_ERR1

DEVICEOK
  lda #0
  jsr HEXOUT

  lda #13
  jsr CHROUT                            ; CR/LF
  clc
  rts

EE_ERR0
  pla
EE_ERR1
  lda #<MSG_EEE
  ldy #>MSG_EEE
  jsr STROUT                            ; Bad EEPROM!
  sec
  rts




; ==============================================================
; 29F040 SUBS
; ==============================================================

_flashBase    = $2000                   ; Base EEPROM Address
_flash555     = _flashBase + $555       ; EEPROM Address + $555
_flash2aa     = _flashBase + $2aa       ; EEPROM Address + $2aa

FLASH_ALG_ERROR_BIT    = $20            ; Erase Error DQ3=1 DQ5=1
; FLASH_ALG_RUNNING_BIT  = $08


_flashCodeMagic                         ; COMMON DATA COMMANDS FOR EEPROM BUS OPERATIONS
  lda #$aa
  sta _flash555                         ; Address/Data: 555/AA
  lda #$55
  sta _flash2aa                         ; Address/Data: 2AA/55
  rts

FlashCodeEndSequ                        ; RESET
_flashCodeEndSequ
  lda #$f0
  sta _flashBase
  rts

_flashCodeSectorErase                   ; ERASE FLASH SECTORS
  jsr _flashCodeMagic                   ; Address/Data: 555/AA 2AA/55
  lda #$80
  sta _flash555                         ; Address/Data: 555/80
  jsr _flashCodeMagic                   ; Address/Data: 555/AA 2AA/55
  lda #$30
  sta _flashBase                        ; Address/Data: _flashBase/30
  rts

_flashCodeChipErase
  jsr _flashCodeMagic                   ; Address/Data: 555/AA 2AA/55
  lda #$80
  sta _flash555                         ; Address/Data: 555/80
  jsr _flashCodeMagic                   ; Address/Data: 555/AA 2AA/55
  lda #$10
  sta _flash555                         ; Address/Data: 555/10
  rts

_flashCodeWrite
  pha
  jsr _flashCodeMagic                   ; Address/Data: 555/AA 2AA/55
  lda #$A0
  sta _flash555                         ; Address/Data: 555/A0
  pla
  ldy #0
  sta (LOADPTR),y
  rts


_flashCodeCheckProgress                 ; TOGGLE CHECK
  ; Push Accumulator and X to Stack
  pha
  txa
  pha
_flashCodeCP0
  ldx #2
_flashCodeCP1
  lda _flashBase
  cmp _flashBase
  beq _flashCodeCP2

  and #FLASH_ALG_ERROR_BIT
  beq _flashCodeCP0

  ; ERROR!!
  lda _flashBase
  cmp _flashBase
  beq _flashCodeCP4

  jsr FlashCodeEndSequ                  ; RESET
  sec
  bcs _flashCodeCPE                     ; ERROR!

_flashCodeCP2
  dex
  bne _flashCodeCP1
_flashCodeCP4
  clc
_flashCodeCPE
  pla
  tax
  pla
  rts


;=============== GET VENDOR/DEVICE ID in X,Y
FlashCodeVendorID                       ; ENTER AUTOSELECT MODE TO READ VENDOR/DEVICE ID
  jsr _flashCodeMagic                   ; Address/Data: 555/AA 2AA/55
  lda #$90
  sta _flash555                         ; Address/Data: 555/90
  ldx _flashBase
  ldy _flashBase+1
  jmp _flashCodeEndSequ

;=============== ERASE SECTOR
FlashCodeSectorErase
  jsr _flashCodeSectorErase
  jmp _flashCodeCheckProgress

;=============== FLASH BYTE    AC ==> (LOADPTR)
FlashCodeWrite
  jsr _flashCodeWrite
  jmp _flashCodeCheckProgress




;------------
MSG_VENDOR
  dc.b FONT2,RVSON,"vENDOR:",RVSOFF,0
MSG_DEVICE
  dc.b RVSON,"dEVICE:",RVSOFF,0

MSG_EEE
  dc.b "??",13,"BAD EEPROM",0


;------------
HEXOUT
  pha
  lda #"$"
  jsr BSOUT                             ; Write $ to Screen
  pla
  beq HEX0
  jsr HEX2
HEX0
  txa
HEX2
  pha
  lsr
  lsr
  lsr
  lsr
  jsr HEX1
  pla
  and #15
HEX1
  clc
  adc #246
  bcc HEX1_2
  adc #6
HEX1_2
  adc #58
  jmp BSOUT




; ==============================================================
; DISPLAY CHAR
; ==============================================================

CHROUT
  pha
  STA $d7
  txa
  pha
  tya
  pha
  lda $d7
  bmi CHOU_7
  cmp #$20
  bcs CHOU_1
  jmp $e756

CHOU_1
  cmp #$60
  bcc CHOU_2
  and #$df
  bne CHOU_3
CHOU_2
  and #$3f
CHOU_3
  ldx $c7                           ;revers?
  beq CHOU_4
  ora #$80
CHOU_4
  ldx $0286                         ;Farbcode
  jsr $eaa1                         ;Zeichen ausgeben
  lda $d3
  cmp #$15
  beq CHOU_5
  inc $d3
CHOU_5
  pla
  tay
  pla
  tax
  pla
  rts

CHOU_7
  and #$7f
  cmp #$20
  bcs CHOU_8
  jmp $e82a
CHOU_8
  ora #$40
  bne CHOU_3

PUTCHR
  pha
  jsr $eab2                         ;zeiger in color RAM
  pla
PUTCHR2
  ldy C_COL
PUTCHR3
  sta (C_LINE),y
  lda $0286                         ;Farbcode
  sta (C_COLP),y
  rts

; ==============================================================
; CONVERT CHAR TO VRAM CODE       C=1:CTRL CHAR
; ==============================================================

CONVCHR
  STA C_CHR

  and #$7f
  cmp #$20
  bcc COCHO_9
  ldx C_CHR
  bpl COCHO_1
  ora #$40
  bne COCHO_3

COCHO_1
  cmp #$60
  bcc COCHO_2
  and #$df
  bne CHOU_3
COCHO_2
  and #$3f
COCHO_3
  ldx $c7                           ;revers?
  beq COCHO_4
  ora #$80
COCHO_4
  sec
COCHO_9
  rts


; ==============================================================
; DISPLAY STRING    in AC/YR
; ==============================================================

STROUT
  sta PT1
  sty PT1 +1
  ldy #0
  ;sty 658                         ;Scroll Flag
  ;dey
STOU_1
  lda (PT1),y
  beq STOU_E
  jsr CHROUT
  iny
  bne STOU_1
STOU_E
  rts



; ==============================================================
; OPEN FIRMWARE FILE AND FLASH
; ==============================================================

LOADER_FN
  dc.b "FE3FIRMWARE"
LOADER_FN2

MSG_ERRFNF
  dc.b "NO FIRMWARE FILE?",13,0
MSG_ERRFLSH
  dc.b "FLASH "
MSG_ERROR
  dc.b "ERROR",13,0

MSG_FLASH
  dc.b "FLASHING ...",13,0
MSG_ERASE
  dc.b "ERASING ...",13,0
MSG_BLANK
  dc.b "BLANK CHECK ...",13,0



SY_VERIFY   = $93                       ; Load/Verify Select
SY_STATUS   = $90                       ; Kernal I/O Status Word
SY_SA       = $b9                       ; Current Secondary Address
SY_DN       = $ba                       ; Current Device Number

SETFNUM = $ffba                         ; Kernal Routine Set Logical, First, and Second Addresses
SETFNAM = $ffbd                         ; Kernal Routine Set Filename
LOAD    = $ffd5                         ; Kernal Routine Load from Device

BASCLR  = $c659
PGMLINK = $c533

SYS_IECOPEN = $f495
SYS_TALK    = $ee14
SYS_TALKSA  = $eece
SYS_LOAD2   = $f587
SYS_IECIN   = $ef19
SYS_UNLISTEN = $eef6
SYS_CLOSE   = $f6da
SYS_CLRCH   = $ffcc                     ; Kernal Routine Close input and output channels
SYS_STOP    = $ffe1                     ; Kernal Routine Scan Stop Key
SYS_ERRFNF  = $f787


FLASH_FIRMWARE
  lda #1                                  ; SA
  lda #1
  ldx #8
  jsr SETFNUM

  lda #LOADER_FN2-LOADER_FN
  ldy #>LOADER_FN
  ldx #<LOADER_FN
  jsr SETFNAM

  jsr SET_PTR

;LOAD_CART
  lda #0                                  ;load
  tax                                     ;SA - LOAD AT ADDRESS
  sta SY_VERIFY
  sta SY_STATUS
  lda #$60
  sta SY_SA
  jsr SYS_IECOPEN
  lda SY_DN
  jsr SYS_TALK
  lda SY_SA
  jsr SYS_TALKSA
;  jmp SYS_LOAD2

  jsr SYS_IECIN
  pha
  lda SY_STATUS
  lsr
  lsr
  bcs FLFI_ER3

  lda #<MSG_FLASH
  ldy #>MSG_FLASH
  jsr STROUT                            ; Flashing ...
  pla
  jsr DOBYTE
FLFI_5
  lda #$fd
  and SY_STATUS
  sta SY_STATUS
  jsr SYS_IECIN
  tax
  lda SY_STATUS
  lsr
  lsr
  bcs FLFI_5

  txa
  jsr DOBYTE
  bit SY_STATUS
  bvc FLFI_5                              ; EOI?

FLFI_9
  jsr SYS_UNLISTEN
  jmp SYS_CLOSE


FLFI_ER3
  pla
  lda #<MSG_ERRFNF
  ldy #>MSG_ERRFNF
ERROUT
  jsr STROUT                            ; No Firmware File?
  jmp SYS_CLRCH


DOBYTE
  ;jsr HEX2
  jsr FlashCodeWrite
  bcs DOBY_err

  cmp (LOADPTR),y
  bne DOBY_err

  jmp INC_PTR

DOBY_err
  pla
  pla
DOBY_err2
  lda #<MSG_ERRFLSH
  ldy #>MSG_ERRFLSH
  jmp ERROUT



FLASH_ERASE
  lda #<MSG_ERASE
  ldy #>MSG_ERASE
  jsr STROUT                            ; Erasing ...
;  jsr _flashCodeChipErase
  jsr FlashCodeSectorErase
  bcs DOBY_err2
  rts



BLANK_CHECK
  lda #<MSG_BLANK
  ldy #>MSG_BLANK
  jsr STROUT                            ; Blank Check ...

  jsr SET_PTR
  jsr BLANK_CHECK_2
  bne BLCH_E

  jsr SET_PTR2
BLANK_CHECK_2
  lda #$ff
BLCH_2
  cmp (LOADPTR),y
  bne BLCH_E
  iny
  bne BLCH_2
  inc LOADPTR +1
  dex
  bne BLCH_2
BLCH_E
  rts



SET_PTR
  lda #$70                                ;DEFAULT CARTRIDGE ADDRESS
  sta LOADPTR +1
  lda #$00                                ;DEFAULT CARTRIDGE ADDRESS
  sta LOADPTR
  ldx #16                                 ;BLOCK COUNT
  ldy #0
  rts

SET_PTR2
  lda #$a0                                ;DEFAULT CARTRIDGE ADDRESS
  sta LOADPTR +1
  lda #$00                                ;DEFAULT CARTRIDGE ADDRESS
  sta LOADPTR
  ldx #32                                 ;BLOCK COUNT
  ldy #0
  rts

INC_PTR
  inc LOADPTR
  bne INPT_2
  inc LOADPTR +1

  lda LOADPTR +1
  cmp #$80
  beq SET_PTR2
INPT_2
  rts


FLASHER_E


; ==============================================================
; Define some common PETSCII codes
; http://sta.c64.org/cbm64petkey.html
; ==============================================================

CLRHOME = $93
HOME    = $13
RVSON   = $12
RVSOFF  = $92
CR      = $0D
BLACK   = $90
WHITE   = $05
RED     = $1C
BLUE    = $1F
PURPLE  = $9C
YELLOW  = $9E
FONT1   = 142               ; BIG LETTERS & GRAFIC
FONT2   = 14                ; BIG AND SMALL LETTERS
AT      = $40
