; VIC 20 Final Expansion Cartridge - Revision 022
; Thomas Winkler - Sep. 2009

; Thanks to Leif Bloomquist
; Thanks to everyone on the Denial forums
; http://www.sleepingelephant.com/denial/
; Flash patch r021 by Nils Eilers
; firmware patch r022d by Dirk Vroomen, April 2014 (full ram for loader-files)
; firmware patch r022e by Dirk Vroomen, May 2015   (Basic-Relinker call missing)

  processor 6502                        ;VIC20

  org $9000,0                           ;Modulstart (Fill value=0)

  rorg $7000



;------------------------------
CAS_BUF     = $033c                     ;CASSETTE BUFFER
F_IO        = CAS_BUF +0                ;IO FLAG
F_WE        = CAS_BUF +1                ;WEDGE FLAG
F_CURDEV    = CAS_BUF +2                ;WEDGE FLAG
F_JOYREP    = CAS_BUF +3                ;JOYSTICK TIMER

MY_WEDGE_LO = CAS_BUF +4                ;WEDGE LOW ADDRESS

F_END       = CAS_BUF +5
;------------------------------


FLGCOM  = $08                                                                   


CHRPTR  = $7a                           ;Char Pointer
PT1     = $22                           ;Pointer
PT2     = $24                           ;Pointer
PT3     = $14                           ;Pointer

FAC     = $61

C_LINE  = $d1                           ;pointer current line char RAM
C_COLP  = $f3                           ;pointer current line color RAM

C_CTRL  = $d4                           ;control mode

C_ROW   = $d6                           ;cursor row
C_COL   = $d3                           ;cursor column
C_CHR   = $d7                           ;cuurent char


BASSTRT = $2B                           ;BASIC START
BASVAR  = $2d                           ;BASIC VARS
BASARR  = $2f                           ;BASIC ARRAYS
BASAEND = $31                           ;BASIC ARRAYS END
BASSTR  = $33                           ;BASIC STRINGS
STRPTR  = $35                           ;STRING POINTER
BASEND  = $37                           ;BASIC END

SAVESTART = $c1
LOADPTR   = $c3
LOADSTART = $ac
LOADEND   = $ae

KEYANZ  = $c6


IECSTAT  = $90

LEN_FNAM = $b7
PTR_FNAM = $bb

SY_VERIFY   = $93
SY_STATUS   = $90
SY_SA       = $b9
SY_DN       = $ba
SY_FN       = $b8

DIRECT_MODE = $9d                       ;Direct=$80/RUN=0


CHRGET  = $0073                         ;GET NEXT CHAR
CHRGOT  = $0079                         ;GET LAST CHAR

BIP     = $0200                         ;BASIC Input Buffer 88 Bytes
BIP_E   = BIP +88                       ;BASIC Input Buffer End - Stack
STP     = $0100                         ;LOW Stack - Text Buffer


IO_FINAL = $9c02                        ;FINAL EXPANSION REGISTER 1 (39938,39939)

FEMOD_START = $00                       ;MODE START
FEMOD_ROM   = $40                       ;MODE EEPROM (READ EEPROM, WRITE RAM)
FEMOD_FLASH = $20                       ;MODE FLASH EEPROM (READ EEPROM, WRITE EEPROM)
FEMOD_RAM_1 = $80                       ;MODE RAM 1 (READ::BANK 1,WRITE::BANK 1/2)
FEMOD_RAM_2 = $C0                       ;MODE RAM 2 (WRITE::BANK 1,READ::BANK 1/2)
FEMOD_SRAM  = $A0                       ;MODE BIG SRAM (SRAM 512KB, BANK 0 TO 15)

SOFT_RESET = $fd22                      ;SOFT RESET
CURSOR_POS = $e50a

BSOUT      = $ffd2
GETIN      = $ffe4



OPEN    = $ffc0     ; Open Vector [F40A] Open File
                    ; Preparing: SETLFS, SETNAM
CLOSE   = $ffc3     ; Close Vector [F34A] Close File
SETLFS  = $ffba     ; Set Logical File Parameters
SETNAM  = $ffbd     ; Set Filename / Command
LOAD    = $ffd5     ; Load RAM From Device
;CHROUT  = $ffd2     ; Output Vector, chrout [F27A] Output One Character
                    ; Preparing: OPEN, CHKOUT (not for video output)
CHKIN   = $ffc6     ; Set Input [F2C7] Set Input Device
                    ; Preparing: OPEN
CHRIN   = $ffcf     ; Input Vector, chrin [F20E] Input a byte
                    ; Preparing: OPEN, CHKIN
PRNSTR  = $cb1e     ; print string in A/Y, 0 terminated
PRNINT  = $ddcd     ; print integer in X/A
PRNERR  = $c437     ; print basic error message in X = (from $01 to $1e)
BASSFT  = $e467     ; BASIC Warm Restart [RUNSTOP-RESTORE]
CHRGETSUB = $0079   ; CHRGET subroutine return point after modify
SY_MOVEMEM = $c3bf     ; Move memory from (start($5f-$60)/end+1($5a-$5b)) to (end+1($58-$59))
GETIN   = $ffe4     ; Get a byte From Keyboad or serial device
FRMEVL  = $cd9e     ; Evaluate Expression in Text
RESET   = $fd22     ; Reset Vic
READY   = $c474     ; Restart BASIC
CINT1   = $e518     ; Initialize I/O
CLRSCN  = $e55f     ; clear screen

; TurboLoad
secnd	= $ff93	    ; send secondary address for LISTEN
ciout	= $ffa8     ; write serial data
unlsn	= $ffae	    ; send UNLISTEN command
listn	= $ffb1	    ; send LISTEN command
fa	= $ba	    ; Current Device Number

CLRCH   = $ffcc     ; Restore Default I/O status

dir_current_device = $ba
dir_current_curposline = $d3
dir_status_word_st = $90
dir_reverse_flag = $c7
dir_basicstartaddress_lo = $2b
dir_basicstartaddress_hi = $2c

dir_basiclinenumber_hi = $3a
dir_index1_lo = $22
dir_index1_hi = $23
dir_basicpointer_lo = $7a
dir_basicpointer_hi = $7b
dir_tapecharparity = $9b ;unmovable ???
dir_current_key = $c5
dir_chars_in_key_buffer = $c6
dir_output_control = $9d     ;Direct=$80/RUN=0
dir_char_under_cursor = $ce

; TurboLoad
fl_status_word_st = $90
fl_load_verify_flag = $93      ; Load=0 else Verify
fl_current_device = $ba
fl_char_in_file_name = $b7
fl_filename_pointer_lo = $bb
fl_filename_pointer_hi = $bc
fl_current_sa = $b9
fl_current_logical_file = $b8
fl_endprogram_address_lo = $ae ; is used as temp pointer of code
fl_endprogram_address_hi = $af ; to send to the drive too
fl_filename_pointer = $bb      ; $BB-$BC Pointer: to file name
fl_output_control = $9d        ; Direct=$80/RUN=0 

fl_basicstartaddress_lo = $2b
fl_basicstartaddress_hi = $2c

; ----- other zero page locations (movable) ------

dir_old_key = $fd
dir_command_set = $fe

; TurboLoad
; $f7-$fa is RS232 pointers, $fb-$fe is unused
iec0d1a = $f7
iec0d1b = $f8
iec1d1a = $f9
iec1d1b = $fa

store   = $fc

mwcmd = $f7   ;f7-fc used first to temp store SETLFS & SETNAM parameters
              ;      and after to send the "M-W" command to the drive.
	      ;      Since these operations are made before the
	      ;      fastloading process i share the same location of
	      ;      iec0d1a,iec0d1b,iec1d1a,iec1d1b and store variables.
              ;      Used as temp value for loader start routine too.

; shared address because used with CTRL+F1/F3/F5/F7 
From_Lo    = $f7 ;FB        ;From address (lo-byte)
From_Hi    = $f8 ;FC        ;From address (hi-byte)
To_Lo      = $f9 ;FD        ;to address (lo-byte)
To_Hi      = $fa ;FE        ;ti address (hi-byte)

; **************** other page locations used ****************

; ----- used by the kernal (unmovable) ------
dir_shift_ctrl_cbm_flag = $028d
dir_keyboard_buffer = $0277 ;($0277-$0280)

dir_start_memory_hi = $282 ; Start of memory (hi-byte)
dir_top_memory_hi = $284   ; Top of memory (hi-byte)
dir_screen_mem_page = $288 ; Screen memory page start (hi-byte)


; **************** I/O constants ****************************

; TurboLoad
AMOUNT = $20	; amount of data bytes to transfer with one M-W command
ESCBYTE = $ef	; the escape char used in the transfers
RETRIES = 20	; amount of retries in reading a block

DEFAULT_DEVICE = 8	; Default device number

CheckByte = #214

iecport1 = $912c	;$dd00	;$912c
dato = 32		;32	;32
clko = 2		;16	;2
iecport2 = $911f	;$dd00	;$911f
atno = 128		;8	;128
clki = 1		;64	;1
dati = 2		;128	;2

; ***********************************************************************







; ==============================================================
; Startup screen
; ==============================================================

STARTUPSCREEN
; dc.b CLRHOME, WHITE, CR, RVSON, "DISK UTILITY CARTRIDGE", CR, CR
  dc.b CLRHOME,FONT2,YELLOW,RVSON,"*fINAL eXPANSION V3.2*", CR
  dc.b RVSON,                     "512/512kb sYSTEM R022E", CR, CR, CR
  dc.b WHITE,RVSON,"f1",RVSOFF," ram mANAGER", CR, CR
;  dc.b "",RVSON,"f2",RVSOFF,"  basic uN-new", CR, CR
  dc.b CR, CR
  dc.b RVSON,"f3",RVSOFF," dISK lOADER", CR, CR
  dc.b RVSON,"f4",RVSOFF,"  hELP", CR, CR
  dc.b RVSON,"f5",RVSOFF," cART lOADER", CR, CR
;  dc.b CR, CR
  dc.b RVSON,"f6",RVSOFF,"  fe3 uTILITIES", CR, CR
;  dc.b CR, CR
  dc.b WHITE,RVSON,"f7",RVSOFF," basic (wEDGE)", CR, CR
  dc.b "",RVSON,"f8",RVSOFF,"  basic (NORMAL)", CR, CR, CR
  dc.b RVSON,"+",RVSOFF,"/",RVSON,"-",RVSOFF," dRIVE #"
  dc.b $00


; ==============================================================
; Help screen
; ==============================================================


HELPSCREEN
  dc.b WHITE
HELPSCREEN3
  dc.b CLRHOME,FONT2,RVSON,"fe3 wEDGE cOMMANDS", CR, CR  ;RVSOFF Not Needed
  dc.b "$  dIRECTORY", CR
  dc.b AT, "  sTATUS/cOMMAND", CR
  dc.b "/  lOAD", CR
  dc.b "%  lOAD bINARY", CR
  dc.b "#  dRIVE", CR,CR
  dc.b "OLD/UNNEW basic", CR
  dc.b "KILL/OFF  END WEDGE", CR
  dc.b "NOIO/BLKD/BLKP", CR
  dc.b "RESET", CR
  dc.b CR  ;RVSOFF Not Needed
  dc.b "sys41000 hELP", CR
  dc.b "sys41003 wEDGE", CR
  dc.b "sys41006 wEDGE at $340", CR, CR, CR
  dc.b $00

; ==============================================================
; RAM setting menu
; ==============================================================

RAMSCREEN
  dc.b CLRHOME,FONT2,YELLOW,RVSON, "  ram cONFIGURATION   ",CR,CR,CR
  dc.b WHITE,RVSON,"f1",RVSOFF," 3 kb (6655)", CR, CR
  dc.b "",RVSON,"f2",RVSOFF,"  8 kb (11775)", CR, CR
  dc.b RVSON,"f3",RVSOFF," 16 kb (19967)", CR, CR
  dc.b "",RVSON,"f4",RVSOFF,"  24 kb (28159)", CR, CR
  dc.b RVSON,"f5",RVSOFF," 24+3 kb (28159)", CR, CR
  dc.b "",RVSON,"f6",RVSOFF,"  oFF (NO wEDGE!)", CR, CR
  dc.b RVSON,"f7",RVSOFF," aLL ram (28159)", CR, CR, CR
  dc.b "",RVSON,"f8",RVSOFF,"  mAIN mENU", CR, CR, CR
  dc.b "io ",RVSON,"r",RVSOFF,"EGISTER ( )", CR
  dc.b "eASY ",RVSON,"w",RVSOFF,"EDGE  ( )"
  dc.b $00


; ==============================================================
; UTILITY menu
; ==============================================================

UTILSCREEN
  dc.b CLRHOME,FONT2,YELLOW,RVSON, "    fe3 uTILITIES     ",CR,CR,CR
  dc.b WHITE,RVSON,"f1",RVSOFF," fLASH pROGRAM", CR, CR
  dc.b "",RVSON,"f2",RVSOFF,"  fLASH fIRMWARE", CR, CR
  dc.b RVSON,"f3",RVSOFF," fLASH iNFO", CR, CR
  ;dc.b CR, CR
  ;dc.b "",RVSON,"f4",RVSOFF,"  24 kb (28159)", CR, CR
  dc.b CR, CR
;  dc.b RVSON,"f5",RVSOFF," bACKUP fLASH", CR, CR
  dc.b CR, CR
  ;dc.b "",RVSON,"f6",RVSOFF,"  oFF", CR, CR
  dc.b CR, CR
;  dc.b RVSON,"f7",RVSOFF," rESTORE fLASH", CR, CR, CR
  dc.b CR, CR, CR
F8SCREEN
  dc.b "",RVSON,"f8",RVSOFF,"  mAIN mENU", CR, CR, CR
  dc.b $00


; ==============================================================
; MENU FOOTER
; ==============================================================

MENUSCREEN
  dc.b HOME,YELLOW, CR, CR
;  dc.b 210,210,210,210,210,210,210,210,210,210,210
  dc.b 96,96,96,96,96,96,96,96,96,96,96
  dc.b 96,96,96,96,96,96,96,96,96,96,96
  dc.b CR, CR, CR, CR, CR, CR, CR, CR, CR
  dc.b CR, CR, CR, CR, CR, CR, CR, CR, CR
  dc.b 96,96,96,96,96,96,96,96,96,96,96
  dc.b 96,96,96,96,96,96,96,96,96,96,96, CR
;  dc.b 197,197,197,197,197,197,197,197,197,197,197,CR

;      "---- DISK LOADER -----"
  dc.b RVSON,"f1",RVSOFF,"/",RVSON,"f3",RVSOFF,":pGuP/dWN "
  dc.b RVSON,"f8",RVSOFF,":eXIT",CR
  dc.b RVSON,"f5",RVSOFF,"/",RVSON,"f7",RVSOFF,":fIRST-/lAST lINE"
  dc.b HOME,$00


; ==============================================================
; The credits!
; ==============================================================

THECREDITS
  dc.b CLRHOME, YELLOW, "V3 06/2009", CR, CR
  dc.b RED, "CREATED BY:", CR, CR
  dc.b BLACK
  dc.b "LEIF BLOOMQUIST", CR
  dc.b "ANDERS PERSSON", CR
  dc.b "ANDERS CARLSSON", CR
  dc.b "CHRISTOPHER PREST", CR
  dc.b "BRIAN LYONS", CR
  dc.b "LEE DAVIDSON", CR
  dc.b "SCHLOWSKI", CR
  dc.b "VIPERSAN", CR
  dc.b "DANIEL KAHLIN", CR
  dc.b "JEFF DANIELS", CR
  dc.b "MICHAEL KLEIN", CR
  dc.b "DAVID A. HOOK", CR
  dc.b "NILS EILERS", CR
  dc.b "DIRK VROOMEN", CR
  dc.b "TOMMY WINKLER", CR, CR
  dc.b $00

THECREDITSP2
  dc.b PURPLE
  dc.b "WWW.SLEEPINGELEPHANT", CR
  dc.b ".COM/DENIAL/", CR
  dc.b BLUE
  dc.b $00

MSG_EXISTS
  dc.b CR,RVSON,"A",RVSOFF,"BORT ",RVSON,"R",RVSOFF,"EPLACE ",RVSON,"U",RVSOFF,"PDATE ",CR,0





; ==============================================================
; ROM $6 FUNCTIONS
; ==============================================================

EXEC_SAVE     = 0
EXEC_LOADER   = 1


DO_EXECUTE subroutine
  tax
  bne .1
  jmp RPROC_SAVE

.1
  dex
  bne .2
  jmp RPROC_LOADER

.2
  rts




; ==============================================================
; PROC AFTER "FILE EXISTS"
; ==============================================================

CH_OLDFILE = 39                        ; '
;CH_OLDFILE = 36                         ; $


RPROC_SAVE subroutine
  lda STP
  cmp #"6"
  bne .rts
  lda STP +1
  cmp #"3"
  bne .rts

  lda #<MSG_EXISTS
  ldy #>MSG_EXISTS
  jsr STROUT

.key
  jsr WAIT_KEY
  cmp #"A"
  beq .rts
  cmp #"R"
  beq .replace
  cmp #"U"
  beq .update
  bne .key

.err
  ;jsr PRINT_DISK_ERR
  jsr GET_DISK_STAT
  jsr PRINT_BIP_0
.rts
  clc
  rts


.replace
  lda #<MSG_DEL_FILE
  ldy #>MSG_DEL_FILE
  jsr SY_STROUT
.repl2
  jsr START_DISK_SCRATCH
  bcs .err
  jsr IECNAMOUT_2

.okul
  jsr UNLISTEN
  sec
  rts


.update
  lda #<MSG_DEL_OLDFILE
  ldy #>MSG_DEL_OLDFILE
  jsr SY_STROUT

  jsr START_DISK_SCRATCH
  bcs .err
  lda #CH_OLDFILE
  jsr IECOUT
  jsr IECNAMOUT_2
  jsr UNLISTEN

  lda #<MSG_RENAME_FILE
  ldy #>MSG_RENAME_FILE
  jsr SY_STROUT

  jsr START_DISK_RENAME
  bcs .err
  lda #CH_OLDFILE
  jsr IECOUT
  jsr IECNAMOUT_2
  lda #"="
  jsr IECOUT
  jsr IECNAMOUT_2
  jmp .okul




MSG_DEL_FILE
  dc.b "DELETING FILE ...",13,0
MSG_DEL_OLDFILE
  dc.b "DELETING OLD FILE ...",13,0
MSG_RENAME_FILE
  dc.b "RENAMING FILE ...",13,0




; ==============================================================
; DISK COMMANDS RENAME AND SCRATCH
; ==============================================================


START_DISK_SCRATCH
  lda #"S"
  bne START_DISK_CMD

START_DISK_RENAME
  lda #"R"

START_DISK_CMD subroutine
  pha
  jsr DISK_LISTEN_6F
  lda IECSTAT
  bmi .err

  pla
  jsr IECOUT
  lda #":"
  jsr IECOUT
  clc
  rts

.err
  jsr UNLISTEN
  pla
  sec
  rts




; =====================================================================
; MENU LOADER
; Schaltet in ROM Modus und lädt 1 oder mehrere Dateien in den Speicher
; Lade Instruktionen stehen in der Tabelle DL_LDBUF
; =====================================================================

RPROC_LOADER  subroutine
  ;LOADER PARAM       :: "filename",B|P|C [,$adress]     B=BASIC Code,P=Program,C=Cartridge
  jsr SET_PT2_LDBUF

  ldx DL_CNTLDR
.2
  txa
  pha
  jsr PRINT_LOADMSG                     ;SET LOAD INFO, PRINT MESSAGE
  jsr GET_FILETYP
  cpx #"B"
  bne .3

  jsr LOAD_BASIC_2
  jmp .4

.3
  jsr MY_IECLOAD
.4
  bcs .ERR

  jsr ADD_PT2_LDBUF                     ;NEXT LDBUF

  pla
  tax
  dex
  bne .2

.E
  jsr PRINT_IO
  clc
  rts

.ERR
  pla
  jmp LOAD_ERR




  ;PRINT LOAD MESSAGE
PRINT_LOADMSG subroutine
  ldy #LDBUF_LAD
  lda (PT2),y                             ; LOAD ADR LOW
  sta LOADPTR
  iny
  lda (PT2),y                             ; LOAD ADR HIGH
  sta LOADPTR +1
  beq .2B

  lda #$ff
.2B
  tay                                     ; SA
  iny
  lda #1
  ldx F_CURDEV                          ; device#
  jsr SETFNUM

  jsr GET_FILETYP
  cpx #"B"
  bne .3B

  jsr PRINT_LOADING
  dc.b "LOAD BASIC PROG",0
  jmp LOAD_BASIC_START

.3B
  cpx #"C"
  bne .3C

  lda LOADPTR +1
  bne .3B2
  lda #$a0                                ;DEFAULT CARTRIDGE ADDRESS
  sta LOADPTR +1
.3B2
  ldy #2
  sty SY_SA
  jsr PRINT_LOADING
  dc.b "LOAD CART",0
  rts

.3C
  jsr PRINT_LOADING
  dc.b "LOAD PROG",0
  rts

GET_FILETYP subroutine
  ldy #LDBUF_TYP
  lda (PT2),y
  tax
  rts





; ==============================================================
; END :: ROM $6 FUNCTIONS
; ==============================================================


  rend




  org $a000,0                           ;$A000   (Fill value=0)

; ==============================================================
; Startup
; ==============================================================
  dc.w START   ; Entry point for power up
  dc.w RESTORE ; Entry point for warm start (RESTORE)

CARTID
  dc.b "A0",$C3,$C2,$CD	; 'A0CBM' boot string


SY_STROUT  = $cb1e                      ;String in AC/YR ausgeben

SY_RAMTAS  = $fd8d
;SY_RAMTAS2 = $fd9b
SY_INITVEC = $fd52
SY_INITIO1 = $fdf9
SY_INITIO2 = $e518

SY_INITVEC2 = $e45b ; Init Vectors
SY_BASRAM   = $e3a4 ; BASIC RAM
SY_INITMSG  = $e404 ; INIT Message (needed so keycheck routine below works)




START
  jsr SY_RAMTAS                         ; RAMTAS - Initialise System Constants
  jsr SY_INITVEC                        ; Init Vectors
  jsr SY_INITIO1                        ; Init I/O
  jsr SY_INITIO2                        ; Init I/O
  cli

  ;BASIC Init (Partial)
  jsr SY_INITVEC2                       ; Init Vectors
  jsr SY_BASRAM                         ; BASIC RAM
  jsr SY_INITMSG                        ; INIT Message (needed so keycheck routine below works)

  ; Force startup with device #8 (disk)
  lda #$08
  sta F_CURDEV
  bne START2


  org $a028                             ; - equals 41000 decimal, easy to remember!

JMP_TABLE
  jmp HELP2                             ; sys 41000: Help sreen
  jmp MY_WEDGE_INIT                     ; sys 41003: Init Wedge
  jmp MOVE_WEDGE_LOW                    ; sys 41006: Copy Wedge to low mem
  jmp SET_BASE_VECTORS                  ; sys 41009: Set Base vectors


  ; COPY FIRMWARE FROM EEPROM TO SRAM
COPYROM subroutine
  lda #>$a000
  ldy #<$a000
  ldx #32                               ; copy 32 pages

COPYROM_2
  sta PT2 +1
  sty PT2

  jsr TEST_RAM                          ;RAM OK?
  bcc .02

  ldy #0
.01
  lda (PT2),y
  sta (PT2),y
  iny
  bne .01
  inc PT2 +1
  dex
  bne .01
  ;sty CARTID
  ;sty CARTID +1

  lda #FEMOD_RAM_1 +$10                 ; ALL RAM, PROTECT BLK-5
  sta IO_FINAL
  sec
.02
  rts



INIT_CART subroutine
  lda #$6e                ; Blue Screen
  ;lda #$6d                 ; Blue Screen,green border
  ;lda #$6b                 ; Blue Screen, cyan border
  sta $900f                ; Screen Color
  rts


START2 subroutine
  lda #"."
  sta F_IO
  lda #"X"
  sta F_WE

.CHECK
; Check which control keys are pressed.
  lda $028d

; if C=, go straight to BASIC.
  cmp #$02
  bne .SHIFT
.basic
  jmp BASIC

; if SHIFT, go straight to BASIC with wedge enabled.
.SHIFT
  cmp #$01
  bne .CTRL
.wedge
  jmp STARTWEDGE

; if CTRL, start Boot-LOADER
.CTRL
  cmp #$04
  bne .01

.bootloader ; execute boot-loader
  lda #FEMOD_ROM                        ;ROM
  jsr UNLOCK_IO                         ;UNLOCK IO
  jsr INIT_CART
  jsr COPYROM
  lda #$08
  sta F_CURDEV

  jsr INIT_CART                         ; Screen Colors
  jsr SPAR_PRINTSTRING
  dc.b CLRHOME,YELLOW,FONT2,RVSON,"---- bOOT lOADER  ----",CR,CR,0
  jsr COPYROM                           ; COPY FIRMWARE TO SRAM
  jsr DLOADER_INIT
  jsr BLD_TABLE
  jsr LIST_PAGE
  jsr LIST_MARKER
  pla
  pla
  jsr DLOA_00
  jmp SSCREEN

.01
  lda #FEMOD_ROM                        ;ROM
  jsr UNLOCK_IO                         ;UNLOCK IO
  bne .basic
  jsr COPYROM
  bcc .basic

  ;CARTID


; ==============================================================
; Default Startup - show init screen
; ==============================================================

SSCREEN subroutine
  jsr INIT_CART
  lda #<STARTUPSCREEN
  ldy #>STARTUPSCREEN
  jsr STROUT_R
  jsr SHOWDRIVE

SETUPTIMEOUT
  ;Initialize Timer
;  ldy $a1 ; part of TI clock, updated every 256/60 = 4.2 seconds
;  iny
;  iny
;  iny     ;Jump ahead three 'ticks' = 12.6 seconds

; Check for timeout
.TIMEOUT
;  cpy $a1
;  bne KEYS

;STARTWEDGE_H
;  jmp STARTWEDGE

; Check keys
.KEYS
  jsr WAIT_KEY

.DENIAL
  cmp #$43			; 'C' --> show credits
  bne .F1
  jsr CREDITS
  jmp WAITSPACE_2

.F1
  cmp #$85  ;F1
  bne .DEL
  jmp RAMMENU

.DEL
;  cmp #$14  ;DEL
;  bne .F2
;  jmp STARTWEDGE

.F2
;  cmp #$89  ;F2
;  bne F3
;  jsr INIT_BASIC
;  jsr SY_INITMSG                        ; INIT Message
;  jmp DO_UNNEW

.F3
  cmp #$86  ;F3
  bne .F4

  jmp DLOADER

.F4
  cmp #$8a  ;F4
  bne .F5
  jmp HELP

.F5
  cmp #$87  ;F5
  bne .F6
  jmp CART_LOADER

.F6
  cmp #$8b  ;F6
  bne .PLUS
  jmp UTIL_MENU

.PLUS
  cmp #$2b  ;+
  bne .MINUS
  jmp INCDRIVE

.MINUS
  cmp #$2D  ;-
  bne .F7
  jmp DECDRIVE

.F7
  cmp #$88  ;F7
  bne .F8
  jmp STARTWEDGE

.F8
  cmp #$8C
  bne .TIMEOUT
  jmp BASIC




; ==============================================================
; DISPLAY STRING IN ROM    in AC/YR
; ==============================================================

STROUT_R subroutine
  jsr SET_ROM
  jsr STROUT
  jmp SET_BACK

; ==============================================================
; EXECUTE FUNCTION IN ROM    in AC
; ==============================================================

EXECUTE_R subroutine
  jsr SET_ROM
  jsr DO_EXECUTE

SET_BACK
  lda 2
  sta IO_FINAL
  rts

SET_ROM
  pha
  lda IO_FINAL
  sta 2
  lda #FEMOD_ROM
  sta IO_FINAL
  pla
  rts


; ==============================================================
; Start Wedge and return to BASIC
; ==============================================================
INIT_BASIC
  jsr SY_INITVEC                        ; Init Vectors
  jsr SY_INITIO1                        ; Init I/O
  jsr SY_INITIO2                        ; Init I/O
  cli

;  lda #$06
;  sta $0286 ; Restore blue text

  ;BASIC Init (Partial)
  jsr SY_INITVEC2                       ; Init Vectors
  jmp SY_BASRAM                         ; BASIC RAM

  ;Clear keyboard buffer
;  lda #$00
;  sta KEYANZ

  ; Restore normal startup colors
INIT_DEF
  lda #$1b
  sta $900f

  ; Restore normal startup colors
  lda #FONT1
  jmp CHROUT


BASIC_WARM = $e467


STARTWEDGE
  sei
  jsr SY_RAMTAS                         ; RAMTAS - Initialise System Constants
  jsr INIT_BASIC

  jsr MY_WEDGE_INIT

STARTWEDGE1
  jsr SY_INITMSG                        ; INIT Message
  jsr SET_BASE_VECTORS                  ; Set Base vectors

STARTWEDGE2

  ;jsr COPYROM                           ; COPY FIRMWARE TO SRAM
  ;bcc xxx                               ; ==> RAM ERROR

  lda #<WEDGEMESSAGE1b
  ldy #>WEDGEMESSAGE1b
  jsr STROUT_R

  lda PTR_INPUT_LOOP +1

STWE_3
  jsr HEXOUT2
  lda #<WEDGEMESSAGE2
  ldy #>WEDGEMESSAGE2
  jsr STROUT_R
  jmp BASIC_WARM


; ==============================================================
; Wedge Message
; ==============================================================

;WEDGEMESSAGE1a
;  dc.b $11, $1C, "EASYWEDGE ($", 0

WEDGEMESSAGE2
  dc.b ")",$11, $1F, 0


; ==============================================================
; Return to BASIC
; ==============================================================
BASIC
  sei
  jsr SY_RAMTAS                         ; RAMTAS - Initialise System Constants
  jsr INIT_BASIC
BASIC1
  jsr SY_INITMSG                        ; INIT Message
BASIC2
  ;lda #0
  ;jsr SY_RAMTAS2                         ;calculate RAM size

  ; Complete BASIC Warm Start (since init is done above)
  jmp BASIC_WARM





; ==============================================================
; Handle the RESTORE key
; ==============================================================
RESTORE
  jmp $fec7   ; Continue as if no cartridge installed



; ==============================================================
; List all the Denial Helpers
; ==============================================================
CREDITS
  lda #$1e
  sta $900f ; Screen colors
  lda #<THECREDITS
  ldy #>THECREDITS
  jsr STROUT_R
  lda #<THECREDITSP2
  ldy #>THECREDITSP2
  jmp STROUT_R


; ==============================================================
; Wait here for space bar or F8
; ==============================================================
WAITSPACE_2
  jsr WAITSPACE
  jmp SSCREEN


WAITSPACE
  jsr GETIN
  cmp #$8C                              ;F8
  beq WASP_1
  cmp #$20                              ;SPACE
  bne WAITSPACE
WASP_1
  rts

WAIT_KEY_TX
  jsr SPAR_PRINTSTRING
  dc.b 13,13,"PRESS ANY KEY ...",13,0
WAIT_KEY
  jsr GETIN
  cmp #0
  beq WAIT_KEY
  rts


; ==============================================================
; Help Screen
; ==============================================================
HELP
  jsr HELP2
  lda #<F8SCREEN
  ldy #>F8SCREEN
  jsr STROUT_R
  jmp WAITSPACE_2


HELP2
  lda #<HELPSCREEN3
  ldy #>HELPSCREEN3
  jmp STROUT_R


; ==============================================================
; Increment/Decrement the drive#
; ==============================================================

INCDRIVE
  inc F_CURDEV
  jmp CHECKDRIVE

DECDRIVE
  dec F_CURDEV

; Make sure drive is valid - 8 to 15
CHECKDRIVE
  clc
CHECKLOW
  lda F_CURDEV
  sbc #$08
  bcs CHECKHIGH
  lda #$08
  sta F_CURDEV
CHECKHIGH
  clc
  lda F_CURDEV
  sbc #$0F
  bcc SHOWDRIVE
  lda #$0F
  sta F_CURDEV

SHOWDRIVE
  lda #$01
  sta $0286  ;White text

  ;Move Cursor
  ldx #$15              ; Row
  ldy #$0b              ; Column
  jsr SET_CURSOR

  lda F_CURDEV
  cmp #$08
  beq DIGIT
  cmp #$09
  beq DIGIT
  jmp DIGITS
  
DIGIT
  adc #$2F  ;convert to ascii
  jsr $ffd2
  lda #$20
  jsr $ffd2 ;append a space
  jmp SETUPTIMEOUT

DIGITS
  lda #$31  ;'1'
  jsr $ffd2
  lda F_CURDEV
  adc #$26
  jsr $ffd2
  jmp SETUPTIMEOUT


; ==============================================================
; SUBMENU RAM CONFIG
; ==============================================================

RAMMENU
  lda #<RAMSCREEN
  ldy #>RAMSCREEN
  jsr STROUT_R

  ; Check keys
KEYS_0
  jsr SHOW_IO
  jsr SHOW_WE

KEYS_1
  jsr GETIN



F1_1
  cmp #$85  ;F1
  bne F2_1

  ;jsr SetVicAs3K
  lda #FEMOD_RAM_1 +$1E                 ;3KB RAM
  bne SET_RAM

F2_1
  cmp #$89  ;F2
  bne F3_1

  ;jsr SetVicAs8K
  lda #FEMOD_RAM_1 +$1D                 ;8KB RAM
  bne SET_RAM

F3_1
  cmp #$86  ;F3
  bne F4_1

  ;jsr SetVicAs16K
  lda #FEMOD_RAM_1 +$19                 ;16KB RAM
  bne SET_RAM

F4_1
  cmp #$8a  ;F4
  bne F5_1

  ;jsr SetVicAs24K
  lda #FEMOD_RAM_1 +$11                 ;24KB RAM
  bne SET_RAM

F5_1
  cmp #$87  ;F5
  bne F6_1

  ;jsr SetVicAs24K
  lda #FEMOD_RAM_1 +$10                 ;24+3KB RAM
  bne SET_RAM

F6_1
  cmp #$8b  ;F6
  bne F7_1

  lda #FEMOD_RAM_1                      ;3KB RAM
  ldx #$FF                              ;OFF
  jmp RESET_SYSTEM

F7_1
  cmp #$88  ;F7
  bne F8_1

  ;jsr SetVicAs24K
  lda #FEMOD_RAM_1                      ;ALL RAM
  bne SET_RAM

F8_1
  cmp #$8C
  bne KEY_W
  jmp SSCREEN

KEY_W
  cmp #"W"
  beq W_FLAG

KEY_R
  cmp #"R"
  beq IO_FLAG
  bne KEYS_1


;-------- CHANGE IO REGISTER FLAG
IO_FLAG
  lda #"."
  cmp F_IO
  bne IOF2
  lda #"X"

IOF2
  sta F_IO
  jmp KEYS_0


;-------- CHANGE WEDGE FLAG
W_FLAG
  lda #"."
  cmp F_WE
  bne WEF2
  lda #"X"

WEF2
  sta F_WE
  jmp KEYS_0




SET_RAM
  ;sta DL_IOBASE
  sta IO_FINAL
  sta CARTID                            ;DELETE CARTRIDGE ID IF RAM
  pha
  ldx F_IO
  cpx #"."
  bne SET_RAM5

  ldx #$80
  stx IO_FINAL +1                       ; LOCK IO (REGISTER INVISIBLE)
SET_RAM5
  and #$a0
  cmp #$80
  bne SET_RAM6                          ; WEDGE ONLY FOR RAM, RAM-NOIO

  ldx F_WE
  cpx #"X"
  beq SET_RAM8
SET_RAM6
  pla
  jsr SetVicMemConfig
  jmp BASIC1

SET_RAM8
  pla
  pha
  jsr SetVicMemConfig
  pla
  and #$bf
  cmp #$80                              ; WEDGE $5 ONLY FOR ALL RAM
  bne SET_RAM9
  jsr SY_INITMSG                        ; INIT Message
  jsr MOVE_WEDGE_LOW                    ; COPY WEDGE TO $500
  jmp STARTWEDGE2
SET_RAM9
  jsr MY_WEDGE_INIT
  jmp STARTWEDGE1


;-------- SWITCH CARTRIDGE AND RUN BASIC
RUNBASIC
  ;jsr INIT_BASIC
  ;jsr $e404 ; INIT Message

  ; Complete BASIC Warm Start (since init is done above)
  ;jmp BASIC_WARM

  ldy #(__SET_IO_RUN_E - __SET_IO_RUN)
  lda #>__SET_IO_RUN
  ldx #<__SET_IO_RUN
  jsr COPY_PROC

  lda BIP_IOBASE
  ldx BIP_IOBASE +1
  jmp BIP


;-------- RAM PROCEDURE TO SWITCH CARTRIDGE AND RUN BASIC
__SET_IO_RUN
  sta IO_FINAL
  stx IO_FINAL +1
  jsr SY_PGMLINK                        ;Relinks BASIC Program from and to any address...
  lda #0
  jsr $ff90
  JSR SY_BASCLR2                        ;CLR
  JMP $C7AE
__SET_IO_RUN_E





;-------- SWITCH CARTRIDGE AND RUN CODE
SYS_XY
  tya
  pha
  txa
  pha

  ;jsr INIT_BASIC

  ldy #(__SET_IO_RESET_E - __SET_IO_RESET)
  lda #>__SET_IO_RESET
  ldx #<__SET_IO_RESET
  jsr COPY_PROC

  ldy #(__SET_IO_RESET_E - __SET_IO_RESET) -2
  pla
  sta BIP,y
  pla
  sta BIP+1,y
SYS_XY_2
  lda BIP_IOBASE
  ldx BIP_IOBASE +1
  jmp BIP








;-------- UNLOCK IO IN START-MODE
UNLOCK_IO
  pha
  ldy #(__UNLOCK_IO_E - __UNLOCK_IO)
  lda #>__UNLOCK_IO
  ldx #<__UNLOCK_IO
  jsr COPY_PROC
  pla
  jmp BIP

;-------- UNLOCK START MODE AND SET MODE
__UNLOCK_IO
;  sei
  ldx $a000                             ;
  stx $a000                             ;UNLOCK IO
  sta IO_FINAL
  cmp IO_FINAL
;  cli
  rts
__UNLOCK_IO_E






;-------- TEST IF RAM IS AT (PT1)
TEST_RAM
  ldy #(__TEST_RAM_E - __TEST_RAM)
  lda #>__TEST_RAM
  ldx #<__TEST_RAM
  jsr COPY_PROC
  jmp BIP

;-------- TEST IF RAM IS AT (PT1)  ZF=1 :: RAM IS OK
__TEST_RAM
  ;ldx IO_FINAL
  sei
  lda #FEMOD_RAM_1                      ;RAM
  sta IO_FINAL

  ldy #0
  lda (PT2),y
  pha
  eor #$ff
  sta (PT2),y                           ;STORE INVERTED VALUE
  cmp (PT2),y                           ;CHECK IF ($A) WRITEABLE
  beq __TERA_1
  clc                                   ;RAM ERROR!!
__TERA_1
  pla
  sta (PT2),y                           ;RESTORE OLD VALUE

  ldx #FEMOD_ROM                        ;ROM
  stx IO_FINAL
  cli
  rts
__TEST_RAM_E




SHOW_IO
  ;Move Cursor
  ldx #21               ; Row
  ldy #13               ; Column
  jsr SET_CURSOR

  lda F_IO
  jmp BSOUT

SHOW_WE
  ;Move Cursor
  ldx #22               ; Row
  ldy #13               ; Column
  jsr SET_CURSOR

  lda F_WE
  jmp BSOUT


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
GREEN   = $1E
PURPLE  = $9C
YELLOW  = $9E
FONT1   = 142               ; BIG LETTERS & GRAFIC
FONT2   = 14                ; BIG AND SMALL LETTERS
AT      = $40



; ==============================================================
; SUBMENU UTILITY
; ==============================================================
UTIL_FLI
  jsr FLASH_INFO

UTIL_MENU subroutine
  jsr INIT_CART                         ; Screen Colors
  lda #<UTILSCREEN
  ldy #>UTILSCREEN
  jsr STROUT_R

  ; Check keys
.KEYS
  ;jsr SHOW_IO
  ;jsr SHOW_WE

  jsr GETIN



.F1
  cmp #$85  ;F1
  bne .F2

  jmp CART_FLASHER


.F2
  cmp #$89  ;F2
  bne F3_UT1

  jmp FIRMW_FLASHER


F3_UT1
  cmp #$86  ;F3
  beq UTIL_FLI

.F4
;  cmp #$8a  ;F4
;  bne F5_UT1


.F5
  cmp #$87  ;F5
  bne .F6


.F6
  cmp #$8b  ;F6
  bne .F7


.F7
  cmp #$88  ;F7
  bne .F8


.F8
  cmp #$8C
  bne .KEYS
  jmp SSCREEN




; ========================================================================
; DISK LOADER
; Schaltet in den RAM Modus
; Ladet die Loaderkonfig Datei (LOADER) in den BASIC Speicher
; Baut eine String Zeigertabelle auf ab TBL_NAMES ($6)
; Führt die Menü Auswahl Prozedur aus
; ========================================================================



;-------------- PRINT DLOADER SCREEN
DLOA_00 subroutine
  jsr MENU_SEL
  bcc .01
  jsr MESE_EXEC
.01

DLOADER subroutine
  lda #$91                              ; Patch von dabone, damit auf 24k umgeschaltet wird
  jsr SetVicMemConfig
  jsr INIT_CART                         ; Screen Colors
  jsr SPAR_PRINTSTRING
  dc.b CLRHOME,YELLOW,FONT2,RVSON,"---- dISK lOADER  ----",CR,CR,0

  jsr DLOADER1
  cpx #13                                 ;CR
  beq DLOA_00
  jmp SSCREEN


DLOADER1 subroutine
  jsr COPYROM                           ; COPY FIRMWARE TO SRAM
  bcs .02                               ; ==> RAM OK

  jsr SPAR_MSGBOX
  dc.b 2, 3, RED, "ram eRROR at $a ...",0
.exit
  ldx #0
  rts

  ;lda #<MSG_RAMMERR
  ;ldy #>MSG_RAMMERR
  ;jmp MSG_BOX


.02
  jsr DLOADER_INIT
  bcs .exit

  jsr BLD_TABLE
  jsr MENU_HANDLER
  clv
  bvc DLOADER1


;-------------- LOAD LOADER FILE
DLOADER_INIT
  jsr MENU_INIT

  ;lda #<MSG_LOAD
  ;ldy #>MSG_LOAD
  ;jsr STR_AT
  jsr SPAR_PRINTSTRING_AT
  dc.b 1, 3, WHITE, "LOADING ...",0

  jsr LOAD_MENU
  php
  ldx #1
  jsr DEL_LINE
  plp
DLOADER_E
  rts


;-------------- PRINT DLOADER SCREEN
MENU_INIT
  lda #<MENUSCREEN
  ldy #>MENUSCREEN
  jsr STROUT_R

  lda #5
  sta FLGCOM
  lda #0
  sta DL_SLINIDX
  lda #DL_STARTLINE
  sta DL_SEL
MEIN_E
  rts


; ==============================================================
; GET KEY / JOYSTICK
; ==============================================================

GETJOY
  lda $a2
  cmp F_JOYREP
  bne GEJO_E
  
  clc
  adc #4
  sta F_JOYREP
  
  sei
  lda VIA1 +$f
  and #$3c
  ldx #127
  stx VIA2 +$2
  ldx VIA2
  bmi GEJO_1
  ora #2
GEJO_1
  ldx #255
  stx VIA2 +$2
  cli
  tay
  ldx #17
  and #8                                ;down
  beq GEJO_9
  tya
  ldx #145
  and #4                                ;up
  beq GEJO_9
  tya
  ldx #13
  and #32                               ;fire
  beq GEJO_9
  tya
  ldx #29
  and #2                                ;right
  beq GEJO_9
  tya
  ldx #157
  and #16                               ;left
  beq GEJO_9
GEJO_E
  jmp GETIN

GEJO_9
  txa
  rts


; ==============================================================
; MENU HANDLER
; ==============================================================

TBL_NAMES  = $6000

TBL_NAMES_CNT = TBL_NAMES


DL_CURLIN  = C_ROW                      ;TEMP. AKT LINE TO DRAW

TBL_CLBUF  = F_END                      ;CATALOG BUFFER (CATALOG ENTRY)

TBL_DATA   = F_END + CLBUF_END
DL_SLINIDX = TBL_DATA                   ;INDEX OF FIRST LINE
DL_CURIDX  = TBL_DATA +1                ;TEMP. AKT. INDEX
DL_CNT     = TBL_DATA +2                ;TEMP. COUNTER
DL_SEL     = TBL_DATA +3                ;selected menu line
DL_LINCNT  = TBL_DATA +4                ;count of lines
DL_LASTLIN = TBL_DATA +5                ;last line
DL_CNTLDR  = TBL_DATA +6                ;LOAD FILES COUNT
DL_CNTLDR2 = TBL_DATA +7                ;LOAD FILES COUNT
DL_IOBASE  = TBL_DATA +8                ;BASE FOR IO2 REGISTER 1 AND 2
;DL_RUNADR  = TBL_DATA +10              ;RUN ADDRESS: 0=RUN BASIC, 1=RESET, 2=invalid, n=SYS


  ;FILE HEADER (LOAD BUFFER)
DL_LDBUF   = TBL_DATA +12               ;LOAD BUFFER - bis zu 4 x DATEI,OPTIONS
LDBUF_LEN  = 0                          ;OFFSET FILENAME LENGTH
LDBUF_FN   = 1                          ;OFFSET FILENAME
LDBUF_TYP  = 17                         ;OFFSET TYPE: B=BASIC, P=PROGRAM, C=CARTRIDGE
LDBUF_LAD  = 18                         ;OFFSET LOAD ADRESS
LDBUF_FLEN = 20                         ;OFFSET FILE LENGTH
LDBUF_END  = 22                         ;STRUCT LDBUF LEN


    ;CATALOG ENTRY
CLBUF_LEN   = 0                         ;OFFSET ENTRY NAME LENGTH
CLBUF_NAM   = 1                         ;OFFSET ENTRY NAME
CLBUF_CNT   = 21                        ;OFFSET FILES COUNT
CLBUF_PLEN  = 22                        ;OFFSET PACKAGE LENGTH
CLBUF_SADR  = 24                        ;OFFSET START ADDRESS
CLBUF_SBANK = 26                        ;OFFSET START BANK
CLBUF_EADR  = 27                        ;OFFSET END ADDRESS
CLBUF_EBANK = 29                        ;OFFSET END BANK
CLBUF_END   = 32                        ;STRUCT LDBUF LEN


    ;CLOADER PACKAGE HEADER
CLHDR_CMD   = 0                         ;OFFSET START COMMAND (SYS/RES/RUN)
CLHDR_IO1   = 1                         ;OFFSET IO REGISTER 1
CLHDR_IO2   = 2                         ;OFFSET IO REGISTER 2
CLHDR_ADR   = 3                         ;OFFSET START ADDRESS
CLHDR_END   = 5                         ;STRUCT LDHDR LEN







DL_STARTLINE = 3                        ;start line
DL_LINES     = 17                       ;number of lines



;-------------- RETURN TO MAIN SCREEN
MEHA_E
  pla
  pla
  rts


;-------------- HANDLE MENU KEY INPUT
MENU_HANDLER
  jsr LIST_PAGE
  jsr LIST_MARKER

MEHA_1
  lda #0
  sta KEYANZ
MEHA_5
  jsr GETJOY
  tax
  
  cmp #$00
  beq MEHA_5

  cmp #$8C                                ;F8
  beq MEHA_E

  cmp #13                                 ;CR
  beq MEHA_E

  jsr MEHA_8
  clv
  bvc MEHA_1


MEHA_8
  cmp #17                                 ;CURSOR DOWN
  beq MARKER_DOWN

  cmp #145                                ;CURSOR up
  beq MARKER_UP

  cmp #$85                                ;F1
  beq PAGEUP

  cmp #$86                                ;F3
  beq PAGEDWN

  cmp #$87                                ;F5
  beq MARKER_FIRST

  cmp #$88                                ;F7
  beq MARKER_LAST
  rts


;--------------------
PAGEUP
  lda DL_SLINIDX
  beq PAUP_E
  sec
  sbc #DL_LINES
PAUP_1
  sta DL_SLINIDX
  jsr LIST_PAGE

  ldx DL_SEL
  cpx DL_LASTLIN
  bcs MARKER_DOWN
PAUP_E
  inx
  rts


PAGEDWN
  lda DL_SLINIDX
  clc
  adc #DL_LINES
  bcs PADW_E
  cmp TBL_NAMES_CNT
  bcc PAUP_1
PADW_E
  ldx #1
  rts


MARKER_FIRST
  jsr HIDE_MARKER
  ldx #(DL_STARTLINE)
  bne SET_MARKER

MARKER_LAST
  jsr HIDE_MARKER
  ldx DL_LASTLIN
  bne SET_MARKER

MARKER_UP
  jsr HIDE_MARKER
  ldx DL_SEL
  dex
  cpx #DL_STARTLINE-1
  bne MAUP_2
  ldx DL_LASTLIN
MAUP_2
  bne SET_MARKER

MARKER_DOWN
  jsr HIDE_MARKER
  ldx DL_SEL
  cpx DL_LASTLIN
  bcc MADO_2
MADO_0
  ldx #(DL_STARTLINE-1)
MADO_2
  inx

SET_MARKER
  stx DL_SEL
  jmp LIST_MARKER




; ========================================================================
; DISK LOADER - SELEKTIERTEN MENÜPUNKT AUSFÜHREN
; Sucht den gewählten Menü Eintrag im BASIC Text
; Interpretiert die Befehle im BASIC Text
; Datei Ladebefehle kommen in einen Buffer (DL_LDBUF) für den LOADER
; Diskbefehle werden sofort ausgeführt
; Folgebefehle werden interpretiert (RAM Konfig, IO Konfig, Start Modus)
; ========================================================================

;----MENU PUNKT GEWÄHLT!
BASPTR = $7a




  ;INIT CATALOG BUFFER
INIT_CATALOG subroutine
  lda #0
  ldy #CLBUF_END -1
.00
  sta TBL_CLBUF,y
  dey
  bpl .00

  ldx #0

  ;COPY ENTRY STRING
  ldy #5
.1
  lda (PT2),y
  beq .E
  cmp #34
  beq .E

  sta TBL_CLBUF+1,x                     ;
  inx
  iny
  cpx #(CLBUF_CNT - CLBUF_NAM)          ;MAX. LENGTH
  bne .1
.E
  stx TBL_CLBUF                         ;ENTRY LENGTH
  rts



  ;SET PTR TO LOADER TAB
SET_LDBUF subroutine
  lda #<(DL_LDBUF)
  ldy #>(DL_LDBUF)
  sta PT1
  sty PT1 +1
  rts





  ;PROCESS SELECTED MENU ENTRY
MENU_SEL_INIT subroutine
  lda #0
  sta BIP_CMD
  sta DL_CNTLDR
  jsr INV_ADDR

  lda #FEMOD_RAM_1                      ;RAM SELECT
  sta DL_IOBASE
  lda #0                                ;ALL RESOURCES
  sta DL_IOBASE +1

  lda DL_SEL
  sec
  sbc #DL_STARTLINE
  bcs .1
  rts
.1
  clc
  adc DL_SLINIDX
  jmp TBL_PTR                           ;SET PT2 TO ENTRY STRING


  ;PROCESS SELECTED MENU ENTRY
MENU_SEL subroutine
  lda DL_LINCNT
  bne .0
  clc
  rts

.0
  jsr MENU_SEL_INIT                     ;SET PT2 TO ENTRY STRING
  bcs .EE

  jsr INIT_CATALOG                      ;INITIALIZE CATALOG BUFFER
  jsr SET_LDBUF                         ;

.nextline
  jsr NEXTLINE
  beq .E
  ldy #4
.5
  lda (PT2),y
  beq .E                                ;CODE ENDE
  cmp #34                               ;NEXT MENU ENTRY
  beq .E

  cmp #$aa                              ;"+" - LOADER ANWEISUNG
  bne .nextline

  tya
  sec
  adc PT2
  sta CHRPTR
  lda PT2 +1
  adc #0
  sta CHRPTR +1

.do_cmd
  jsr CHRGOT
  bne .2b
.2c
  tax
  beq .nextline

  jsr CHRGET                            ;SKIP ":"
  beq .2c
.2b
  jsr .do_line
  jmp .do_cmd

  ;INTERPRET COMMAND
.do_line
  cmp #34                               ;+"FILENAME"
  bne .6
  jmp SEL_LOAD

.6
  cmp #"@"                              ;+@"diskcmd"
  bne .7
  jmp DO_DISKCMD

.7
  jsr TOKENIZER
  bne .8
  jmp NEXT_STATEMENT

.8
  cpx #TOK_NOIO                         ;NOIO
  beq SEL_NOIO
  cpx #TOK_BLKP                         ;BLOCK WRITE PROTECT
  beq SEL_BLOCK1
  cpx #TOK_BLKD                         ;BLOCK DISABLE
  beq SEL_BLOCK2

  stx BIP_CMD
  ;cpx #TOK_RELO                         ;RELOAD
  ;beq SEL_RELOAD
  ;cpx #TOK_RUN                          ;RUN
  ;beq SEL_RUN
  cpx #TOK_SYS                          ;SYS
  beq SEL_SYS
  ;cpx #TOK_RES                          ;RESET
  ;beq SEL_RESET
.EE
  clc
  rts

.E
SET_BIP_IOBASE SUBROUTINE
  lda DL_IOBASE +1
  sta BIP_IOBASE +1
  lda DL_IOBASE
  sta BIP_IOBASE
  sec
  rts




; ========================================================================
; DISK LOADER - SELEKTIERTEN MENÜPUNKT AUSFÜHREN
; Alle Zeilen mit Anweisungen sind bereits verarbeitet
; + Anweisungen auf den Stack
; + Dateien laden
; + Anweisungen vom Stack ausführen
; ========================================================================

BIP_CMD     = $0293                     ;COMMAND BYTE
BIP_IOBASE  = BIP_CMD +1                ;BASE FOR IO2 REGISTER 1 AND 2
BIP_RUNADR  = BIP_CMD +3                ;RUN ADDRESS: 0=RUN BASIC, 1=RESET, 2=invalid, n=SYS


  ;EXECUTE DLOADER BUFFER
MESE_EXEC subroutine
  lda DL_CNTLDR
  beq .RELOAD
  ldx BIP_CMD
  beq .RELOAD
  cpx #TOK_RELO                         ;RELOAD
  beq .RELOAD

  jsr CLROUT
  jsr SET_MEM_CONFIG
  jsr SY_INITMSG                        ; INIT Message (e404)
  jsr CROUT

  lda #FEMOD_ROM
  sta IO_FINAL                          ; EEPROM!
  jsr DO_LOADER                         ; LOAD FILES
  bcs .err

EXEC_PACKAGE
  ldx BIP_CMD
  cpx #TOK_RES                          ;RESET
  beq LOCO_RES
  cpx #TOK_RUN                          ;RUN
  beq LOCO_RUN
  cpx #TOK_SYS                          ;SYS
  beq LOCO_SYS
.err
  jmp DLOADER

.RELOAD
  rts


;------------
SEL_SYS subroutine
  jsr GETADR
  bcs .5
INV_ADDR
  lda #0
  tax
  sta BIP_CMD
.5
  stx BIP_RUNADR
  sta BIP_RUNADR +1
  rts


;------------
SEL_NOIO subroutine
  lda #$80                              ;RAM, NO-IO
  ora DL_IOBASE +1
  sta DL_IOBASE +1
  rts




;------------ BLOCK WRITE PROTECT
SEL_BLOCK1 subroutine
  ldy #0
  beq .2a


;------------ BLOCK DISABLE
SEL_BLOCK2
  ldy #1
.2a
  jmp SEL_BLOCK_P


;------------
LOCO_RES subroutine
  lda #<MSG_RES
  ldy #>MSG_RES
  jsr STROUT

  lda BIP_IOBASE
  ldx BIP_IOBASE +1
  jmp RESET_SYSTEM

;------------
LOCO_RUN subroutine
  lda #<MSG_RUN
  ldy #>MSG_RUN
  jsr STROUT
  jmp RUNBASIC

;------------
LOCO_SYS subroutine
  lda #<MSG_SYS
  ldy #>MSG_SYS
  jsr STROUT
  lda #32
  jsr BSOUT
  ldx BIP_RUNADR
  lda BIP_RUNADR +1
  jsr HEXOUT

  ldx BIP_RUNADR
  ldy BIP_RUNADR +1

  jmp SYS_XY



; ==============================================================
; SUBS
; ==============================================================



NEXTLINE subroutine
  ldy #1
  lda (PT2),y
  beq .E
  tax
  dey
  lda (PT2),y
  sta PT2
  stx PT2 +1
  txa                                   ;RESET Z
.E
  rts


;----PRINT MARKER
LIST_MARKER subroutine
  lda DL_LINCNT
  bne LIMA_1
  rts

LIMA_1
  lda #YELLOW
  jsr CHROUT
  ldx DL_SEL
  ldy #0
  jsr SET_CURSOR
  lda #62                                 ;">"
  jsr PUTCHR
  lda #60                                 ;"<"
  ldy #$15
  jmp PUTCHR3

HIDE_MARKER subroutine
  ldx DL_SEL
  ldy #0
  jsr SET_CURSOR
  lda #32
  jsr PUTCHR
  ldy #$15
  lda #32
  jmp PUTCHR3






;----LIST PAGE
LIST_PAGE subroutine
  lda #0
  sta DL_LINCNT
  lda #DL_STARTLINE
  sta DL_CURLIN
  lda #DL_LINES
  sta DL_CNT

  lda DL_SLINIDX
  sta DL_CURIDX

LIPA_2
  ldx DL_CURLIN
  ldy #1
  jsr SET_CURSOR
  jsr $eab2                         ;zeiger in color RAM

  lda #WHITE
  jsr CHROUT

  lda DL_CURIDX
  jsr LIST_LINE                     ;LIST LINE (AC)
  inc DL_CURLIN
  inc DL_CURIDX
  dec DL_CNT
  bne LIPA_2

  lda DL_LINCNT
  clc
  adc #DL_STARTLINE -1
  sta DL_LASTLIN
  rts



;----LIST LINE. AC=TAB ENTRY
LIST_LINE subroutine
  jsr TBL_PTR                           ;SET PT2 TO ENTRY STRING
  bcs .E

  inc DL_LINCNT
  ;ldy #5
  ldy FLGCOM
.1
  lda (PT2),y
  beq .E
  cmp #34
  beq .E
  ;jsr CHROUT
  jsr CONVCHR
  bcc .5
.3
  sty C_CHR
  jsr PUTCHR2
  lda C_COL
  cmp #$15
  beq .EE
  inc C_COL
  ldy C_CHR
.5
  iny
  bne .1
.E
  lda #32
  ldy $d3
.E2
  sta (C_LINE),y
  iny
  cpy #$15
  bne .E2
.EE
  rts


  ;----OFFSET (AC) --> TAB POINTER
  ; :: SET PT1 TO MENU ENTRY TABLE
  ; :: SET PT2 TO MENU ENTRY STRING
TBL_PTR subroutine
  cmp TBL_NAMES                         ;NUMBER OF ENTRIES
  bcs .exit

  ldy #>(TBL_NAMES+1)
  asl                                   ;*2
  bcc .1
  iny
  clc
.1
  adc #<(TBL_NAMES+1)
  sta PT1                               ;ADDR LO - ENTRY
  bcc .2
  iny
.2
  sty PT1 +1                            ;ADDR HI - ENTRY

  ldy #1
  lda (PT1),y
  sta PT2 +1                            ;ADDR LO - ENTRY STRING
  dey
  lda (PT1),y
  sta PT2                               ;ADDR HI - ENTRY STRING

  clc
.exit
  rts


;----BUILD NAME TABLE FROM BASIC TEXT
BLD_TABLE subroutine
  jsr BLD_TABLE_INIT                    ;INITIALIZE BLD TABLE

  lda BASSTRT
  ldx BASSTRT +1

.2
  sta PT1
  stx PT1 +1

  ldy #1
  lda (PT1),y
  beq .E

  tax
  ldy #4
  lda (PT1),y
  cmp #34
  bne .5

; FOUND PROGRAM NAME
  ldy #0
  lda PT1
  sta (PT2),y
  lda PT1 +1
  iny
  sta (PT2),y

  jsr INC_PT2
  jsr INC_PT2
  inc TBL_NAMES

.5
  ldy #0
  lda (PT1),y
  jmp .2

.E
  rts

  ;INITIALIZE BLD TABLE
BLD_TABLE_INIT subroutine
  lda #0
  sta TBL_NAMES

  lda #<(TBL_NAMES+1)
  ldy #>(TBL_NAMES+1)
  sta PT2
  sty PT2 +1
  rts




; ==============================================================
; MENU SELECT PARAM
; ==============================================================

SEL_LOAD
  ;LOADER PARAM       :: +"filename",B|P|C|@ [,$adress]
  ;                      B=BASIC Code,P=Program,C=Cartridge,@=DOS COMMAND
  ldy #LDBUF_FN
  ldx #16
SELO_2
  lda (CHRPTR),y
  beq SELO_5
  cmp #34                               ; "
  beq SELO_5
  sta (PT1),y                           ; SAVE FILENAME
  iny
  dex
  bne SELO_2
SELO_5
  dey
  beq SELO_E
  tya
  pha
  txa
  beq SELO_7a
  lda #0
SELO_7
  iny
  sta (PT1),y                           ; FILL FILENAME WITH 0
  dex
  bne SELO_7
SELO_7a
  tay
  pla
  sta (PT1),y                           ; SAVE FILENAME LENGTH
  sec
  adc CHRPTR
  sta CHRPTR
  bcc SELO_8
  inc CHRPTR +1
SELO_8
  jsr CHRGET
  jsr CHKCOM
  bcs SELO_8a
  cmp #"B"                              ;BASIC FILE
  beq SELO_9
  cmp #"C"                              ;CARTRIDGE FILE
  beq SELO_9
  ;cmp #"@"
  ;beq SELO_9
SELO_8a
  lda #"P"                              ;NORMAL PROG
SELO_9
  ldy #LDBUF_TYP
  sta (PT1),y                           ; PROGRAM TYPE
  jsr CHRGET
  jsr CHKCOM
  bcs SELO_9a
  jsr GETADR
  bcs SELO_9b
SELO_9a
  lda #0
  tax
SELO_9b
  ldy #LDBUF_LAD +1
  sta (PT1),y                           ; LOAD ADR HIGH
  dey
  txa
  sta (PT1),y                           ; LOAD ADR LOW

  inc DL_CNTLDR
  clc
  lda #LDBUF_END
  adc PT1
  sta PT1
  bcc SELO_E
  inc PT1 +1
SELO_E
  rts




; =====================================================================
; MENU LOADER
; Schaltet in ROM Modus und lädt 1 oder mehrere Dateien in den Speicher
; Lade Instruktionen stehen in der Tabelle DL_LDBUF
; =====================================================================

DO_LOADER  subroutine
  lda #EXEC_LOADER
  jmp EXECUTE_R



; ==============================================================
; LOAD MENU DATA    "LOADER"
; ==============================================================

SETFNUM = $ffba
SETFNAM = $ffbd
LOAD    = $ffd5

SY_BASCLR2 = $c659
SY_BASCLR  = $c660
SY_PGMLINK = $c533

LOAD_MENU subroutine
  ldx #<(MSG_LOADER)
  ldy #>(MSG_LOADER)
  lda #6
  jsr SETFNAM
  lda #1
  ldx F_CURDEV                          ; device#
  ldy #0
  jsr SETFNUM
  lda #0                  ;load
  ldx BASSTRT             ;Start lo#
  ldy BASSTRT +1          ;Start hi#
  jsr LOAD
  jmp BASLOAD_END

LOAD_BASIC subroutine
  jsr LOAD_BASIC_START
LOAD_BASIC_2
  jsr MY_IECLOAD
  jmp BASLOAD_END

LOAD_BASIC_START subroutine
  lda #1
  ldx F_CURDEV                          ; device#
  ldy #0
  jsr SETFNUM
  ldx BASSTRT             ;Start lo#
  ldy BASSTRT +1          ;Start hi#
  stx LOADPTR
  sty LOADPTR +1
  rts




SYS_IECOPEN = $f495
SYS_TALK    = $ee14
SYS_TALKSA  = $eece
;SYS_LOAD2   = $f58a
;SYS_LOAD2C  = $f5a3
SYS_SETADR  = $e4c1




; ==============================================================
; LOADER TEXT
; ==============================================================

MSG_SYS
  dc.b "SYS",0
MSG_RUN
  dc.b "RUN",0

MSG_LOADER
  dc.b "LOADER",0

MSG_DOS
  dc.b "@ ",60,0
;  lda #60                                 ;"<"
;  lda #62                                 ;">"






; ========================================================================
; FIRMWARE FLASHER
; Schaltet in den RAM Modus
; Ladet die Firmware Datei (FE3FIRMWARE) in den Speicher ab $2
; Flashed den Bereich $2 bis $4 in den EEPROM
; ========================================================================


FLLO_STARTADR = $2000


FIRMW_FLASHER subroutine
  lda #FEMOD_RAM_1 +$10                 ;RAM MODUS, BLK-5 PROTECTED
  sta IO_FINAL
  jsr SetVicMemConfig

  jsr SY_INITMSG                        ; INIT Message (e404)
  jsr CROUT

  ;CHANGE TO ROOT
  jsr SPAR_DISKCMD
  dc.b "CD_",0                          ;EXIT D64
  jsr SPAR_DISKCMD
  dc.b "CD//",0                         ;--> ROOT

  ;SET FILENAME PARAM
  jsr SPAR_GETSTRING
  dc.b 11,"FE3FIRMWARE",0
  sta PT2
  sty PT2 +1

  ;SET FILE PARAM
  jsr LOAD_CART_AT
  bcc .ok

  jsr SPAR_GETSTRING
  dc.b 15,"FE3FIRMWARE.PRG",0
  sta PT2
  sty PT2 +1
  jsr LOAD_CART_AT
  bcs .err

.ok
  ;FLASH FIRMWARE
  jsr MOVE_FLASH_FW
  lda #<FLLO_STARTADR
  ldy #>FLLO_STARTADR
  jsr FLASHER

.err
  jsr WAIT_KEY
  jmp UTIL_MENU




; ========================================================================
; CART FLASHER
; Schaltet in den RAM Modus
; Ladet die Loaderkonfig Datei (LOADER) in den BASIC Speicher
; Baut eine String Zeigertabelle auf ab TBL_NAMES ($6)
; Führt die Menü Auswahl Prozedur aus
; ========================================================================



  ;-------------- PRINT DLOADER SCREEN AND FLASH SELECTED FILES

  subroutine
.00
  jsr MENU_SEL
  bcc .01
  jsr MENU_FLASHER
.01

CART_FLASHER
  jsr INIT_CART                         ; Screen Colors
  jsr SPAR_PRINTSTRING
  dc.b CLRHOME,YELLOW,FONT2,RVSON,"---- cART fLASHER ----",CR,CR,0

  jsr DLOADER1
  cpx #13                               ;CR
  beq .00
  jmp UTIL_MENU



  ;-------------- EXECUTE SELECTED MENU

MENU_FLASHER subroutine
  lda DL_CNTLDR                         ;NUMBER OF FILES
  beq .exit

  jsr CLROUT                            ;CLEAR SCREEN

  ldx BIP_CMD
  beq .exit
  cpx #TOK_RELO                         ;RELOAD
  beq .exit

  lda #FEMOD_RAM_1 +$10                 ;RAM MODUS, BLK-5 PROTECTED
  sta IO_FINAL
  jsr SetVicMemConfig

  jsr SY_INITMSG                        ; INIT Message (e404)
  jsr CROUT

  ;SAVE FILE COUNT
  lda DL_CNTLDR
  sta TBL_CLBUF +CLBUF_CNT

  ;GET ENTRY COUNT
  jsr FLASH_SEARCH_END                   ;SEARCH LAST ENTRY
  txa
  bmi .errfull

  ;LOAD FILES TO RAM DISK
  jsr RAMD_INIT                         ; INIT RAM DISK
  jsr FLSH_LOADER                       ; LOAD FILES
  bcs .exit

  jsr RAMD_SIZE                         ; GET LENGTH OF PACKAGE
  sta LOADEND +1
  stx LOADEND
  sta TBL_CLBUF +CLBUF_PLEN +1
  stx TBL_CLBUF +CLBUF_PLEN

  ;PRINT PACKAGE SIZE
  jsr SPAR_PRINTSTRING
  dc.b 13,"PACKAGE SIZE ",0
  lda LOADEND +1
  ldx LOADEND
  jsr HEXOUT

FLASH_PACKAGE_DEBUG
  jsr FLASH_PACKAGE                     ; FLASH PACKAGE
  bcs .errflash

  jsr FLASH_ENTRY                       ; FLASH CATALOG
  bcs .errflash

.wait
  jsr WAIT_KEY_TX
.exit
  jmp CART_FLASHER



.errfull
  jsr SPAR_PRINTSTRING
  dc.b CR,RED,"CATALOG FULL!",0
  clc
  bcc .exit

.errflash
  jsr SPAR_PRINTSTRING
  dc.b CR,RED,"FLASH ERROR!",0
  clc
  bcc .wait


  ;WRITE PACKAGE INTO FLASH
FLASH_PACKAGE subroutine
  jsr SPAR_PRINTSTRING
  dc.b CR,"FLASHING PACKAGE...",0

  ;------------------------
  ;PREPARE TO FLASH PACKAGE
  ;------------------------
  jsr RAMD_INIT                         ;INIT RAM DISK
  jsr MOVE_FLASH_FW                     ;PREPARE FLASH CODE

  jsr FLASH_SEARCH_END                  ;SEARCH LAST ENTRY
  jsr FLASH_EADR                        ;GET FLASH START ADDRESS
  sta TBL_CLBUF +CLBUF_SADR +1          ;ADR HI
  stx TBL_CLBUF +CLBUF_SADR             ;ADR LO
  sty TBL_CLBUF +CLBUF_SBANK            ;BANK

  ;PREPARE RAMD WRITE TO FLASH
  stx SAVESTART
  sta SAVESTART +1
  tya
  ora #FEMOD_FLASH                      ;PROG MODE
  sta __flash_BANK

  ldy #0
.flash
  lda LOADEND
  bne .05
  lda LOADEND +1
  beq .exit
  dec LOADEND +1
.05
  dec LOADEND
  jsr RAMD_GETC                         ;BYTE FROM RAMDISK

  jsr FlashCodeWriteXP
  bcs .rts
  jsr FlashCodeWriteXpInc               ;INCREMENT ADDRESS
  jmp .flash

.exit
  ;SET END BANK/ADDRESS
  lda __flash_BANK
  and #$0f
  ldx SAVESTART
  ldy SAVESTART +1

  sty TBL_CLBUF +CLBUF_EADR +1          ;ADR HI
  stx TBL_CLBUF +CLBUF_EADR             ;ADR LO
  sta TBL_CLBUF +CLBUF_EBANK            ;BANK
  clc                                   ;OK!
  bcc PRINT_OK

.rts
  rts



  ;WRITE ENTRY INTO CATALOG
FLASH_ENTRY subroutine
  jsr SPAR_PRINTSTRING
  dc.b CR,"FLASHING CATALOG...",0

  jsr FLASH_SEARCH_END                  ;SEARCH LAST ENTRY
  txa                                   ;ENTRY COUNT
  beq .00
  jsr ADD_CLBUF                         ;NEXT ENTRY (FREE SLOT)
.00
  ;jsr FLASH_EADR                       ;GET FLASH ADDRESS
  lda LOADPTR
  ldx LOADPTR +1
  sta SAVESTART
  stx SAVESTART +1

  lda #FEMOD_FLASH                      ;PROG MODE, BANK 0
  sta __flash_BANK

  ldy #0
.04
  lda TBL_CLBUF,y
  jsr FlashCodeWriteXP
  bcs .rts

  iny
  cpy #CLBUF_END
  bne .04

PRINT_OK
  jsr SPAR_PRINTSTRING
  dc.b GREEN,"OK",BLUE,0
  clc
.rts
  rts



; =====================================================================
; FLASH LOADER
; lädt eine oder mehrere Dateien in den Speicher ab $2
; kopiert die Ladeanweisungen + Datei in den Super RAM
; =====================================================================


  ;FLASH LOADER PARAM       :: "filename",B|P|C [,$adress]     B=BASIC Code,P=Program,C=Cartridge
FLSH_LOADER subroutine
  jsr WRITE_CLHDR                       ;WRITE PACKAGE HEADER

  jsr SET_PT2_LDBUF
.00
;  lda #2                                ; SA=2: CARTRIDGE - WHOLE FILE
  jsr LOAD_CART_AT
  bcs .err

  ;CALC FILE LENGTH
  ;  ldx LOADEND
  ;  ldy LOADEND +1
  tya
  sec
  sbc #>FLLO_STARTADR
  sta LOADEND +1


  ;STORE FILE LENGTH INTO LOAD BUFFER
  ldy #LDBUF_FLEN +1
  sta (PT2),y                           ; FILE LENGTGH HI
  dey
  txa
  sta (PT2),y                           ; FILE LENGTGH LO



  ;COPY LOAD BUFFER INTO RAMDISK
  ldx #LDBUF_END
  ldy #0
.05
  lda (PT2),y
  jsr RAMD_PUTC
  iny
  dex
  bne .05


  ;COPY FILE INTO RAMDISK
  jsr SET_LPT_STARTADR
  ldy #0
.07
  lda LOADEND
  bne .06
  lda LOADEND +1
  beq .09
  dec LOADEND +1
.06
  dec LOADEND

  lda (LOADPTR),y                       ;BYTE FROM FILE
  jsr RAMD_PUTC                         ;INTO RAMDISK

  iny
  bne .07
  inc LOADPTR +1
  bne .07

.09


  ;GOTO NEXT FILE
  jsr ADD_PT2_LDBUF
  dec DL_CNTLDR
  bne .00

.exit
  jsr PRINT_IO
  clc
  rts

.err
LOAD_ERR
  jsr PRINT_LOADERR
  jsr WAITSPACE
  sec
  rts



  ;NEXT LOAD HEADER
ADD_PT2_LDBUF subroutine
  clc
  lda #LDBUF_END
  adc PT2
  sta PT2
  bcc .0
  inc PT2 +1
.0
  rts


  ;PRINT LOAD MESSAGE, GET FILENAME
PRINT_LOADING subroutine
  jsr SPAR_GETPTR
  jsr SPAR_PRINTSTRING_2

  jsr SPAR_PRINTSTRING
  dc.b CR, "  ",60,0

  ldy #LDBUF_LEN
  lda (PT2),y                             ; FILENAME LEN
  ldy PT2 +1
  ldx PT2
  inx
  bne .2
  iny
.2
  jsr SETFNAM                           ;SET FILENAME
  jsr STRNOUT                           ;PRINT FILENAME

  lda #62                               ;">"
  ldx #CR
  jmp CHROUT2



 ;------------
SET_PT2_LDBUF subroutine
  ;LOADER PARAM       :: "filename",B|P|C [,$adress]     B=BASIC Code,P=Program,C=Cartridge
  lda #<(DL_LDBUF)
  ldy #>(DL_LDBUF)
SET_PT2_AY
  sta PT2
  sty PT2 +1
  rts

;------------
SET_LPT_STARTADR subroutine
  lda #<FLLO_STARTADR
  ldy #>FLLO_STARTADR
SET_LPT_AY
  sta LOADPTR
  sty LOADPTR +1
  rts


  ;LOAD FILE INTO BUFFER
LOAD_CART_AT subroutine
  lda #2                                ; SA! - CARTRIDGE
LOAD_AT subroutine
  tay                                   ; SA
  ldx F_CURDEV                          ; device#
  jsr SETFNUM

  jsr PRINT_LOADING
  dc.b "LOAD FILE",0

  jsr SET_LPT_STARTADR

  ;LOAD FILE AT (FLLO_STARTADR)
  jsr MY_IECLOAD
  bcc .ok
PRINT_LOADERR
.err
  jsr SPAR_PRINTSTRING
  dc.b 13,"LOAD ERROR!",13,0
  sec
.ok
  rts





;BIP_CMD     = $0293                     ;COMMAND BYTE
;BIP_IOBASE  = BIP_CMD +1                ;BASE FOR IO2 REGISTER 1 AND 2
;BIP_RUNADR  = BIP_CMD +3                ;RUN ADDRESS: 0=RUN BASIC, 1=RESET, 2=invalid, n=SYS




  ;WRITE PACKAGE HEADER
WRITE_CLHDR subroutine
  ldy #0
.00
  lda BIP_CMD,y
  jsr RAMD_PUTC
  iny
  cpy #CLHDR_END
  bcc .00
  rts




; ========================================================================
; CART LOADER
; Schaltet in den RAM Modus
; Ladet die Flash File List in den BASIC Speicher
; Baut eine String Zeigertabelle auf ab TBL_NAMES ($6)
; Führt die Menü Auswahl Prozedur aus
; ========================================================================

  ;-------------- PRINT CLOADER SCREEN AND START SELECTED FILES

  subroutine
.00
  jsr MENU_CSEL
  bcs .01
  jsr MENU_CLOADER
.01

CART_LOADER
  jsr INIT_CART                         ; Screen Colors
  jsr SPAR_PRINTSTRING
  dc.b CLRHOME,YELLOW,FONT2,RVSON,"---- cART  lOADER ----",CR,CR,0

  jsr CLOADER1
  cpx #13                               ;CR
  beq .00
  jmp SSCREEN



CLOADER1 subroutine
  jsr COPYROM                           ; COPY FIRMWARE TO SRAM
  bcs .02                               ; ==> RAM OK

  jsr SPAR_MSGBOX
  dc.b 2, 3, RED, "ram eRROR at $a ...",0
.exit
  ldx #0
  rts

.02
  jsr MENU_INIT
  lda #1
  sta FLGCOM                             ;STRING OFFSET
  jsr MENU_CATALOG
  ;bcs .exit

  jsr MENU_HANDLER

  clv
  bvc CLOADER1


;-------------- LOAD CATALOG LOADER FILE
MENU_CATALOG subroutine
  lda #>$2000
  ldy #<$2000
  ldx #16                               ; copy 16 pages
  jsr COPYROM_2                         ; COPY CATALOG TO SRAM

  ;jsr FLASH_SEARCH_END
;  jmp BLD_TABLE_CAT



  ;----BUILD NAME TABLE FROM CATALOG
BLD_TABLE_CAT subroutine
  jsr BLD_TABLE_INIT                    ;INITIALIZE BLD TABLE
  jsr SET_CLO_START                     ;SET CLOADER TABLE ADDRESS
  ldy #0
.00
  lda (LOADPTR),y
  cmp #$ff
  beq .tblend                           ;TABLE END!!

  lda LOADPTR
  sta (PT2),y
  jsr INC_PT2

  lda LOADPTR +1
  sta (PT2),y
  jsr INC_PT2

  jsr ADD_CLBUF

  inc TBL_NAMES
  bpl .00

.tblend
  rts





  ;INCREMENT PT2
INC_PT2 subroutine
  inc PT2
  bne .2
  inc PT2 +1
.2
  rts



  ;INCREMENT LOADPTR
INC_LOADPTR subroutine
  inc LOADPTR
  bne .2
  inc LOADPTR +1
.2
  rts



  ;---EXECUTE SELECTED MENU ENTRY
MENU_CSEL subroutine
  jsr MENU_SEL_INIT                     ;SET PT2 TO ENTRY STRING
  bcs .err

  ;COPY CATALOG ENTRY
COPY_CATALOG
  ldy #0
.1
  lda (PT2),y
  sta TBL_CLBUF,y
  iny
  cpy #CLBUF_END                        ;LENGTH
  bne .1
  clc
.err
  rts




  ;---LOAD SELECTED MENU ENTRY
MENU_CLOADER subroutine
  ;SAVE FILE COUNT
  lda TBL_CLBUF +CLBUF_CNT
  bne .1
  rts
.1
  sta DL_CNTLDR

  ;------------------------
  ;PREPARE TO READ PACKAGE
  ;------------------------
  jsr RAMD_INIT                         ;INIT RAM DISK
  lda #FEMOD_ROM                        ;ROM MODUS
  sta __RAMD_READ_MODE

  lda #FEMOD_ROM                        ;ROM MODUS
  ora TBL_CLBUF +CLBUF_SBANK            ;+ BANK
  tay
  lda TBL_CLBUF +CLBUF_SADR +1
  ldx TBL_CLBUF +CLBUF_SADR
  sta __RAMD_READ_ADR +1                ;ADR HI
  stx __RAMD_READ_ADR                   ;ADR LO
  sty __RAMD_READ_BANK                  ;BANK
  jsr READ_CLHDR                        ;RESTORE PACKAGE HEADER

  jsr SET_MEM_CONFIG
  jsr SY_INITMSG                        ; INIT Message (e404)
  jsr SPAR_PRINTSTRING
  dc.b CR,CR,"LOADING PACKAGE...",CR,0

  lda #FEMOD_ROM
  sta IO_FINAL                          ; EEPROM!

  lda DL_CNTLDR
.loadfile
  pha
  jsr READ_LDBUF                        ;RESTORE FILE LOAD BUFFER
  jsr READ_FILE                         ;READ FILE FROM RAMDISK
  pla
  tax
  dex
  txa
  ;dec DL_CNTLDR
  bne .loadfile

  jsr PRINT_IO                          ;PRINT IO SETTING
;  jsr WAIT_KEY
  jmp EXEC_PACKAGE



  ;RESTORE PACKAGE HEADER
READ_CLHDR subroutine
  ldy #0
.00
  jsr RAMD_GETC
  sta BIP_CMD,y
  iny
  cpy #CLHDR_END
  bcc .00
  rts



  ;RESTORE FILE LOAD BUFFER
READ_LDBUF subroutine
  ldy #0
.00
  jsr RAMD_GETC
  sta DL_LDBUF,y
  iny
  cpy #LDBUF_END
  bcc .00
  rts



  ;READ FILE FROM RAMDISK
READ_FILE subroutine
  jsr SET_PT2_LDBUF
  jsr PRINT_LOADMSG                     ;SET LOAD INFO, PRINT MESSAGE

  lda DL_LDBUF +LDBUF_FLEN
  sta LOADEND                           ;FILE LENGTH
  lda DL_LDBUF +LDBUF_FLEN +1
  sta LOADEND +1                        ;FILE LENGTH HI

  ;ldx DL_LDBUF +LDBUF_TYP
  ;cpx #"C"
  ;beq .0

  ldx SY_SA                             ;SA
  cpx #2
  beq .0                                ;CARTRIDGE

  lda LOADEND                           ;SUBTRACT LOAD ADDRESS
  sec
  sbc #2
  sta LOADEND
  bcs .01
  dec LOADEND +1
.01

  jsr RAMD_GETC                         ;SKIP LOAD ADDRESS
  tay
  jsr RAMD_GETC
  dex
  bne .0                                ;SA=0: -->
  sta LOADPTR +1
  sty LOADPTR                           ;LOAD ADDRESS FROM FILE
.0
  lda #LOADPTR
  jsr PRINT_ATADR_2                     ;PRINT LOADING FROM $xxxx
  ldy #0
.1
  lda LOADEND
  bne .2
  lda LOADEND +1
  beq .le
  dec LOADEND +1
.2
  dec LOADEND
  jsr RAMD_GETC
  sta (LOADPTR),y
  inc LOADPTR
  bne .1
  inc LOADPTR +1
  bne .1

.le
  lda #LOADPTR
  jsr PRINT_TOADR_2                     ;PRINT TO $xxxx

  ldx LOADPTR
  ldy LOADPTR +1
  stx LOADEND                           ;LOAD END POINTER FOR 3d LABY ...
  sty LOADEND +1
  lda DL_LDBUF +LDBUF_TYP
  cmp #"B"
  bne .nobas                            ;BASIC PROG? --> no
  jsr BASLOAD_END_2                     ;RELINK BASIC
.nobas
  rts




DL_LDBUF   = TBL_DATA +12               ;LOAD BUFFER - bis zu 4 x DATEI,OPTIONS
LDBUF_LEN  = 0                          ;OFFSET FILENAME LENGTH
LDBUF_FN   = 1                          ;OFFSET FILENAME
LDBUF_TYP  = 17                         ;OFFSET TYPE: B=BASIC, P=PROGRAM, C=CARTRIDGE
LDBUF_LAD  = 18                         ;OFFSET LOAD ADRESS
LDBUF_FLEN = 20                         ;OFFSET FILE LENGTH
LDBUF_END  = 22                         ;STRUCT LDBUF LEN



CLBUF_LEN   = 0                         ;OFFSET ENTRY NAME LENGTH
CLBUF_NAM   = 1                         ;OFFSET ENTRY NAME
CLBUF_CNT   = 21                        ;OFFSET FILES COUNT
CLBUF_PLEN  = 22                        ;OFFSET PACKAGE LENGTH
CLBUF_SADR  = 24                        ;OFFSET START ADDRESS
CLBUF_SBANK = 26                        ;OFFSET START BANK
CLBUF_EADR  = 27                        ;OFFSET END ADDRESS
CLBUF_EBANK = 29                        ;OFFSET END BANK
CLBUF_END   = 32                        ;STRUCT LDBUF LEN

CLHDR_CMD   = 0                         ;OFFSET START COMMAND (SYS/RES/RUN)
CLHDR_IO1   = 1                         ;OFFSET IO REGISTER 1
CLHDR_IO2   = 2                         ;OFFSET IO REGISTER 2
CLHDR_ADR   = 3                         ;OFFSET START ADDRESS
CLHDR_END   = 5                         ;STRUCT LDHDR LEN


; ========================================================================
; FLASH STATISTIC (F6,F3)
; get info: number of entries, bytes free, bytes allocated
; print info, wait for key
; ========================================================================

CLO_STARTADR = $2000

FLASH_INFO subroutine
  ;jsr CLROUT                           ;CLEAR SCREEN
  jsr SPAR_PRINTSTRING
  dc.b CLRHOME,FONT2,YELLOW
  dc.b "## fLASH sTATUS ##",13,13,13
  dc.b WHITE,"eNTRIES: ",0

  jsr FLASH_SEARCH_END                  ;SEARCH LAST ENTRY
  lda #0
  jsr PRNINT                            ;print integer in X/A

  ;txa
  ;jsr HEXOUT2

  jsr SPAR_PRINTSTRING
  dc.b 13,13,13,"bYTES FREE:$",0
  jsr FLASH_FREE                        ;BYTES FREE
  jsr HEXOUT_LA

  jsr SPAR_PRINTSTRING
  dc.b 13,13,"ALLOCATED: $",0
  jsr FLASH_ALLOC                       ;BYTES ALLOCATED
  jsr HEXOUT_LA
  jmp WAIT_KEY




  ;PRINT LARGE ADDRESS (24 bit)
HEXOUT_LA subroutine
  lda LEN_FNAM
  jsr HEXOUT2
  lda PTR_FNAM +1
  ldx PTR_FNAM
  jmp HEXOUT4



  ;SEARCH LAST ENTRY / COUNT IN XR
FLASH_SEARCH_END subroutine
  lda #FEMOD_ROM                        ;ROM MODUS
  sta IO_FINAL
  jsr SET_CLO_START                     ;SET CLOADER TABLE ADDRESS
  ldx #0
  ldy #0
  beq .00

.03
  jsr .07
.00
  lda (LOADPTR),y
  cmp #$ff
  beq .tblend                           ;TABLE END!!
  inx
  tya
  bne .03

  ldy #CLBUF_END                        ;ENTRY LENGTH
  bne .00

.tblend
  rts


ADD_CLBUF
  lda #CLBUF_END                        ;ENTRY LENGTH
.07
  clc
  adc LOADPTR
  sta LOADPTR
  bcc .08
  inc LOADPTR +1
.08
  rts



  ;CALC PACKAGE START ADDRESS
FLASH_SADR subroutine
  ldy #CLBUF_SADR
  bne .00

  ;CALC PACKAGE END ADDRESS
FLASH_EADR
  ldy #CLBUF_EADR
.00
  lda (LOADPTR),y
  tax
  iny
  lda (LOADPTR),y
  pha
  iny
  lda (LOADPTR),y
  cmp #$ff
  bne .01

  pla
  lda #>$2000                           ;BLK 1 :: START OF CATALOG
  ldx #<$2000
  ldy #1
  bne .02

.01
  and #$0f                              ;BANK
  tay
  pla
.02
  sta PTR_FNAM +1                       ;ADR HI
  stx PTR_FNAM                          ;ADR LO
  sty LEN_FNAM                          ;BANK
  rts



  ;CALC ALLOCATED BYTES IN FLASH (YR/XR/AC)
FLASH_FREE subroutine
  jsr FLASH_ALLOC
  sec
  lda #0                                ;BUILD COMPLEMENT
  sbc PTR_FNAM
  sta PTR_FNAM
  lda #$80
  sbc PTR_FNAM +1
  sta PTR_FNAM +1
  lda #7
  sbc LEN_FNAM
  sta LEN_FNAM
  rts



  ;CALC FREE BYTES IN FLASH (YR/XR/AC)
FLASH_ALLOC subroutine
  jsr FLASH_EADR
  lda PTR_FNAM +1                       ;ADR HI
  jsr CALC_BLK2OFFS                     ;BLOCK ADDRESS TO OFFSET
  dec LEN_FNAM
  lsr LEN_FNAM                          ;64K BLOCK - HI HI
  bcc .04
  ora #$80                              ;ADD 32K ON ODD BANK
.04
  sta PTR_FNAM +1                       ;ADR HI
  rts



  ;---- SET CLOADER TABLE ADDRESS
SET_CLO_START subroutine
  lda #<CLO_STARTADR
  ldy #>CLO_STARTADR
  jmp SET_LPT_AY



  ;---- BLOCK ADDRESS TO OFFSET
CALC_BLK2OFFS subroutine
  cmp #$a0
  bcc .00
  ;adr $A to $C
  sec
  sbc #$20
.00
  ;adr $2 to $8
  sec
  sbc #$20
  and #$7f
  rts




    ;CLOADER HEADER (CLOAD BUFFER)
CLBUF_LEN   = 0                         ;OFFSET ENTRY NAME LENGTH
CLBUF_NAM   = 1                         ;OFFSET ENTRY NAME
CLBUF_CNT   = 21                        ;OFFSET FILES COUNT
CLBUF_PLEN  = 22                        ;OFFSET PACKAGE LENGTH
CLBUF_SADR  = 24                        ;OFFSET START ADDRESS
CLBUF_SBANK = 26                        ;OFFSET START BANK
CLBUF_EADR  = 27                        ;OFFSET END ADDRESS
CLBUF_EBANK = 29                        ;OFFSET END BANK
CLBUF_END   = 32                        ;STRUCT LDBUF LEN



; ========================================================================
; RAM DISK
; ========================================================================

  ;PREPARE FOR READ
RAMD_INIT subroutine
  ldy #(__RAMDISK_E - __RAMDISK)
  lda #>__RAMDISK
  ldx #<__RAMDISK
  jmp COPY_PROC


  ;GET SIZE
RAMD_SIZE subroutine
  lda __RAMD_WRITE_BANK
  and #15
  tay
  dey
  dey

  lda __RAMD_WRITE_ADR +1
  cmp #$80
  bcc .08

  sbc #($A0 - $80)
.08
  sec
  sbc #>FLLO_STARTADR

  ldx __RAMD_WRITE_ADR
  rts


  ;-----------------------
RAMD_PUTC subroutine
  jsr __RAMD_PUTC
  ;-----------------------
RAMD_PUTC_REST subroutine
  inc __RAMD_WRITE_ADR
  bne .exit

  lda __RAMD_WRITE_ADR +1
  jsr RAMD_INC_PTR
  sta __RAMD_WRITE_ADR +1
  bcc .exit

  inc __RAMD_WRITE_BANK
.exit
  rts


  ;-----------------------
RAMD_GETC subroutine
  jsr __RAMD_GETC
  ;-----------------------
RAMD_GETC_REST subroutine
  inc __RAMD_READ_ADR
  bne .exit

  pha
  lda __RAMD_READ_ADR +1
  jsr RAMD_INC_PTR
  sta __RAMD_READ_ADR +1
  bcc .exit2
  inc __RAMD_READ_BANK
.exit2
  pla
.exit
  rts



  ;-----------------------
RAMD_INC_PTR subroutine
  sec
  adc #0
  cmp #$80
  bcc .exit

  cmp #$A0
  bcs .01

  lda #$A0
  bne .exit

.01
  cmp #$C0
  bcc .exit
.02
  lda #>FLLO_STARTADR
.exit
  rts



;-------- SUPER RAM PROCEDURE TO WRITE A BYTE
__RAMDISK
  RORG BIP

__RAMD_PUTC subroutine
  pha

__RAMD_WRITE_BANK = . +1
  lda #FEMOD_SRAM + 0                   ;SUPER RAM MODUS, BANK 0
  sta IO_FINAL

  pla
__RAMD_WRITE_ADR = . +1
  sta FLLO_STARTADR

__RAMD_WRITE_MODE = . +1
  lda #FEMOD_RAM_1 +$10                  ;RAM MODUS, BLK-5 PROTECTED
; lda #FEMOD_ROM                        ;ROM MODUS
  sta IO_FINAL
  rts


;-------- SUPER RAM PROCEDURE TO READ A BYTE

__RAMD_GETC subroutine
__RAMD_READ_BANK = . +1
  lda #FEMOD_SRAM + 0                   ;SUPER RAM MODUS, BANK 0
  sta IO_FINAL

__RAMD_READ_ADR = . +1
  lda FLLO_STARTADR
  pha

__RAMD_READ_MODE = . +1
;  lda #FEMOD_RAM_1 +$10                 ;RAM MODUS, BLK-5 PROTECTED
  lda #FEMOD_ROM                        ;ROM MODUS
  sta IO_FINAL
  pla
  rts

  REND
__RAMDISK_E







; ==============================================================
; SET MEMORY CONFIG
; ==============================================================

MOVE_WEDGE_LOW subroutine
  lda #<MY_WEDGE_LO                     ; store the new dest address lo-byte
  ldy #>MY_WEDGE_LO                     ; store the new dest address hi-byte $516


;MOVE_WEDGE
  ;sty mwcmd + 1                        ; store the new dest address hi-byte $516
  ;stx mwcmd                            ; store the new dest address lo-byte
  ;txa

  clc
  adc #<(MY_WEDGE_END - MY_WEDGE_START)
  sta $58                               ; low-byte new end address +1 $0501+fl len

  tya
  adc #>(MY_WEDGE_END - MY_WEDGE_START)
  sta $59                               ; high-byte new end address +1 $0501+fl len

  lda #<MY_WEDGE_START
  sta $5f                               ; low-byte start address $a000
  lda #>MY_WEDGE_START
  sta $60                               ; high-byte start address $a000

  lda #<MY_WEDGE_END
  sta $5a                               ; low-byte end address +1 $a???
  lda #>MY_WEDGE_END
  sta $5b                               ; high-byte end address +1 $a???

;  sei
  jsr SY_MOVEMEM                        ; execute move memory vic routine and return
;  cli
  jmp MY_WEDGE_LO

;  jsr .01
;  cli
;  rts

;.01
;  jmp (mwcmd)


MOVE_CODE






; ==============================================================
; SET MEMORY CONFIG
; ==============================================================

;  lda #$9F                    ;0kb
;  lda #$9E                    ;3kb
;  lda #$9D                    ;8kb          %1001 1101
;  lda #$99                    ;16kb         %1001 1001
;  lda #$91                    ;24kb         %1001 0001
;  lda #$90                    ;24+3kb       %1001 0000
;  lda #$80                    ;ALL RAM      %1000 0000

SET_MEM_CONFIG SUBROUTINE
;  jsr CLROUT                            ;CLEAR SCREEN
  lda BIP_IOBASE
  sta IO_FINAL
  ora BIP_IOBASE +1

SetVicMemConfig  subroutine
  tax
  and #$03
  cmp #$02
  beq SetVicAs3K
  cmp #$03
  beq SetVicAsUnexpanded
  txa
  and #$0c
  cmp #$0c
  beq SetVicAs8K
  cmp #$00
  beq SetVicAs24K

SetVicAs16K:                            ; set vic as 16k expanded
  lda #$10			        ; Screen memory page start $1000 (hi-byte)
  ldx #$12			        ; Start of memory $1200 (hi-byte)
  ldy #$60			        ; Top of memory $6000 (hi-byte)
  bne dir_unexpand

SetVicAsUnexpanded:                     ; set vic as unexpanded
  lda #$1e			        ; Screen memory page start $1e00 (hi-byte)
  ldx #$10			        ; Start of memory $1000 (hi-byte)
  ldy #$1e			        ; Top of memory $1e00 (hi-byte)
  bne dir_unexpand

SetVicAs3K:                             ; set vic as 3k expanded
  lda #$1e			        ; Screen memory page start $1e00 (hi-byte)
  ldx #$04                              ; Start of memory $0400 (hi-byte)
  ldy #$1e                              ; Top of memory $1e00 (hi-byte)
  bne dir_unexpand

SetVicAs8K:                             ; set vic as 8k expanded
  lda #$10			        ; Screen memory page start $1000 (hi-byte)
  ldx #$12			        ; Start of memory $1200 (hi-byte)
  ldy #$40			        ; Top of memory $4000 (hi-byte)
  bne dir_unexpand

SetVicAs24K:                            ; set vic as 24k expanded
  lda #$10			        ; Screen memory page start $1000 (hi-byte)
  ldx #$12			        ; Start of memory $1200 (hi-byte)
  ldy #$80			        ; Top of memory $6000 (hi-byte)

dir_unexpand:       ;@@@
  sta dir_screen_mem_page               ; Screen memory page start (hi-byte)
;  lda dir_start_memory_hi
  stx dir_start_memory_hi               ; Start of memory (hi-byte)
  sty dir_top_memory_hi                 ; Top of memory (hi-byte)

  ;jsr UnexpandMoveMemory

  ;jsr SY_INITIO2                        ; Initialize I/O
  jmp INIT_BASIC





;Pad to end to create valid cart image
;  org $afff
;  dc.b #$00



















; ==============================================================
; MY BIG WEDGE (ROM only)
; ==============================================================

ROM_INPLOOP subroutine
  cmp #95                               ; SAVE? (left arrow)
  beq DO_SAVE
  tax
  pla
  pla
  txa
  jmp RAM_INPUT_LOOP


DO_SAVE subroutine
  ldx F_CURDEV                          ; device#
  lda #$01                              ; lfn #01
  tay
  jsr SETLFS
  jsr GET_FILENAM
  ;jmp $e156

.1
  ldx BASVAR
  ldy BASVAR +1
  lda #BASSTRT
  jsr $ffd8
  bcs .2
  rts
  ;jmp CROUT

.2
  lda #EXEC_SAVE
  jsr EXECUTE_R
  bcs .1

.rts
  rts

  ;jmp MY_IECSAVE



  ;jmp MY_WDGE_START

MY_WEDGE_START




; == START OF MINI WEDGE ============================================================


; ==============================================================
; RELOCATOR
; ==============================================================

STACK           = $0100


SY_TIMOUTFLG = $ffa2
SY_GETSTATUS = $ffb7


MY_RELOCATOR subroutine
_relo = . +2
  jsr SY_GETSTATUS                      ;WEDGE ADDRESS TO STACK
_relo0000 = . +1
  lda _relo
  tsx
  lda STACK,x                           ;ADR HIGH
  tay
  dex
  lda STACK,x                           ;ADR LO
  sta PT1
  sty PT1 +1
;  sta PT3
;  sty PT3 +1                            ;BASE ADDRESS

  ldy #(_relo0000 - _relo)
  sec
  sbc (PT1),y
  sta PT2
  iny
  lda PT1 +1
  sbc (PT1),y
  sta PT2 +1
  ora PT2
  beq RELO_E                            ;RELOCATION NOT NESSECARY


RELO_0
  clc
  lda #>(RELO_TAB - _relo)
  adc PT1 +1                            ;RELO_TAB POINTER HI
  sta PT1 +1
  lda #<(RELO_TAB - _relo)
  ldx #1
RELO_2
  stx FLGCOM
RELO_3
  clc
  adc PT1
  sta PT1                               ;RELO_TAB POINTER LO
  bcc RELO_3a
  inc PT1 +1                            ;RELO_TAB POINTER HI
RELO_3a
  ldy #0
  lda (PT1),y
  bne RELO_3c

  tax
  iny
  lda (PT1),y
  bne RELO_3b

;-------------- 00.00 GET OFFSET
  iny
  lda (PT1),y
  beq RELO_E
  tax
  lda #3
  bne RELO_2

RELO_3b
  dey
  txa
RELO_3c
  clc
  adc PT2
  sta (PT1),y
  sta LOADPTR                           ;MODIFY ADDRESS LO
  iny
  lda (PT1),y
  adc PT2 +1
  sta (PT1),y
  sta LOADPTR +1                        ;MODIFY ADDRESS HI

  dey
  clc
  lda (LOADPTR),y
  adc PT2
  sta (LOADPTR),y                       ;RELINK ADDRESS LO
  ldy FLGCOM
  lda (LOADPTR),y
  adc PT2 +1
  sta (LOADPTR),y                       ;RELINK ADDRESS HI

  lda #2
  bne RELO_3

RELO_E
;_relo0003 = . +1
;  jmp MY_WEDGE_INIT




; ==============================================================
; MY WEDGE
; ==============================================================

PTR_ERROR_OUT   = $0300
PTR_INPUT_LOOP  = $0302
PTR_FRMELEM     = $030a
PTR_LOAD        = $0330
PTR_SAVE        = $0332

;-------------------- WEDGE INIT
MY_WEDGE_INIT
_relo5000 = . +1
  lda #<INPUT_LOOP
  ldx #>INPUT_LOOP
  sta PTR_INPUT_LOOP
  stx PTR_INPUT_LOOP +1
_relo5001 = . +1
  lda #<MY_LOAD
  ldx #>MY_LOAD
  sta PTR_LOAD
  stx PTR_LOAD +1
_relo5003 = . +1
  lda #<MY_SAVE
  ldx #>MY_SAVE
  sta PTR_SAVE
  stx PTR_SAVE +1
_relo5002 = . +1
  lda #<MY_FRMELEM
  ldx #>MY_FRMELEM
  sta PTR_FRMELEM
  stx PTR_FRMELEM +1
  lda #$08
  sta F_CURDEV
  rts



; ==============================================================
; RELOCATOR TABLE
; ==============================================================

;  org $be00


RELO_TAB                                ;RELOC TABLE - 2 BYTE OFFSET LOW/HI
  dc.w _relo0000
  dc.w _relo0001,_relo0002;,_relo0003
  dc.w _relo0010,_relo0011,_relo0012,_relo0013,_relo0014,_relo0015,_relo0016
  dc.w _relo0020,_relo0021,_relo0022,_relo0023,_relo0024,_relo0025,_relo0026
  dc.w _relo0030,_relo0031,_relo0032                                        ,_relo0037,_relo0038,_relo0039
  dc.w _relo0040,_relo0041,_relo0042
  dc.w _relo0050
  dc.w _relo0060,_relo0061,_relo0062
  dc.w                     _relo0072
  dc.w _relo0080,_relo0081,_relo0082          ,_relo0084,_relo0085,_relo0086,_relo0087,_relo0088;,_relo0089
  dc.w _relo0090,_relo0091,          _relo0093,_relo0094,_relo0095          ,_relo0097,_relo0098
  dc.w _relo0100,_relo0101,_relo0102,_relo0103,_relo0104,_relo0105,_relo0106
  dc.w _relo0110,_relo0111,_relo0112,_relo0113,_relo0114,_relo0115,_relo0116,_relo0117,_relo0118,_relo0119
  dc.w _relo0120,_relo0121,_relo0122,_relo0123,_relo0124,_relo0125,_relo0126,_relo0127,_relo0128,_relo0129
  dc.w _relo0130,_relo0131,_relo0132,_relo0133,_relo0134,_relo0135
  dc.w           _relo0141,_relo0142;,_relo0143,_relo0144
  dc.w _relo0150,_relo0151
  dc.w _relo0160
  dc.w _relo0170,_relo0171,_relo0172,_relo0173,_relo0174,_relo0175
  dc.w _relo0180,_relo0181,_relo0182,          _relo0184,_relo0185,_relo0186,_relo0187,_relo0188,_relo0189
  dc.w _relo0200,_relo0201
  dc.w _relo0211,_relo0212
  dc.w _relo0220,_relo0221,_relo0222
  dc.w _relo0230,_relo0231
  dc.w _relo0250,_relo0251
  dc.w _relo0300,                    _relo0303,          _relo0305,_relo0306,_relo0307,_relo0308,_relo0309
  dc.w _relo0310,_relo0311,_relo0312,_relo0313,_relo0314,_relo0315,_relo0316,_relo0317,_relo0318,_relo0319
  dc.w _relo0320          ,_relo0322,_relo0323,_relo0324 ;,_relo0325,_relo0326,_relo0327,_relo0328,_relo0329
  dc.w _relo0350,_relo0351,_relo0352,_relo0353,_relo0354,_relo0355,_relo0356,_relo0357,_relo0358,_relo0359
  dc.w _relo0360,_relo0361,_relo0362,_relo0363          ,_relo0365
  dc.w _relo0400,_relo0401,_relo0402,_relo0403,_relo0404,_relo0405,_relo0406,_relo0407,_relo0408,_relo0409
  dc.w _relo0410,_relo0411,_relo0412;,_relo0413,_relo0414,_relo0415,_relo0416,_relo0417,_relo0418,_relo0419
  dc.w _relo0420,_relo0421;,_relo0412,_relo0413,_relo0414,_relo0415,_relo0416,_relo0417,_relo0418,_relo0419
  dc.w _relo0430;,_relo0431;,_relo0412,_relo0413,_relo0414,_relo0415,_relo0416,_relo0417,_relo0418,_relo0419
  dc.w 0

  dc.b 2                                ;RELOC TABLE - 2 BYTE OFFSET LOW/HI
  dc.w _relo5000,_relo5001,_relo5002,_relo5003
  dc.w _relo5010,_relo5011,_relo5012,_relo5013
  dc.w _relo5070
  dc.w _relo5090,_relo5091
  dc.w _relo5230
  dc.w _relo5250
  dc.w 0
  dc.b 0




; ==============================================================
; MY INPUT LOOP
; ==============================================================


INLO_1 subroutine
  ldx PTR_INPUT_LOOP +1                 ; CODE IN ROM?
  cpx #>INPUT_LOOP
  bne .10
  jsr ROM_INPLOOP
  jmp INPUT_LOOP

.10
RAM_INPUT_LOOP
_relo0001 = . +1
  jsr INLO_LOOP2

INPUT_LOOP subroutine
  jsr $c560                             ; INPUT LINE
  stx CHRPTR
  sty CHRPTR +1
  jsr CHRGET
  tax
  beq INPUT_LOOP
  ldx #$ff
  stx dir_basiclinenumber_hi            ; DIRECT MODE
  bcs INLO_1
  jmp $c49c                             ; INSERT LINE INTO BASICTEXT





DOLO_ERR
_relo0024 = . +1
  jsr PRINT_DISK_ERR
  sec
  rts




BASLOAD_END
  bcs DOLO_ERR
BASLOAD_END_2
  stx BASVAR
  sty BASVAR +1
  ;jsr SY_PGMLINK
DOLO_E
  clc                                   ; LOAD OK
  rts



INLO_LOOP2
  cmp #"$"                              ; CATALOG?
  beq DO_CATALOG
  cmp #"@"                              ; DISK CMD?
  beq DO_DISKCMD
  cmp #"/"                              ; LOAD BASIC?
  beq DO_LOADBAS
  cmp #"%"                              ; LOAD BINARY?
  beq DO_LOADBIN
  cmp #">"                              ; VERIFY BINARY?
  beq DO_VERIFY


_relo0002 = . +1
  jsr TOKENIZER
  bne INLO_LOOP2b

  jsr $c579                             ; CONVERT LINE
  jsr CHRGET
  cmp #"#"                              ; DEVICE?
  beq DO_SETDEV
  cmp #","                              ; HEX CALC
  beq DO_HEXCALC
INLO_LOOP2a
  pla
  pla
  jsr CHRGOT
  jmp $c7e7                             ; EXECUTE LINE


  ;LIST DISK CATALOG ($)
DO_CATALOG
_relo0011 = . +1
  jmp PRINT_CATALOG                     ;JSR!!!!


  ;SET / LIST DEVICE# FOR DISK COMMANDS (#)
DO_SETDEV
_relo0012 = . +1
  jmp SET_DEVICE


  ;SEND DISK COMMAND (@)
DO_DISKCMD
_relo0014 = . +1
  jmp DISK_CMD


  ;SHOW WORD VALUE IN DEC, HEX, BIN, ASC (,)
DO_HEXCALC
  jsr CHRGET
_relo0015 = . +1
  jsr FRMWORD                           ; GET WORD VALUE
_relo0016 = . +1
  jmp PRINT_VALUE



  ;LOAD BASIC OR ABSOLUTE (/)
DO_VERIFY
  ldy #$1                               ; SA=1
  tya
  bne DOLO_2

  ;LOAD BASIC OR ABSOLUTE (/)
DO_LOADBAS
  ldx BASSTRT
  lda BASSTRT +1
  stx LOADPTR
  sta LOADPTR +1
  ldy #$0                               ; SA=0
  beq DOLO_1

  ;LOAD BINARY OR CARTRIDGE (%)
DO_LOADBIN
  ldy #$1                               ; SA=1
DOLO_1
  lda #0                                ; LOAD
DOLO_2
  pha
  ldx F_CURDEV                          ; device#
  lda #$01                              ; lfn #01
  jsr SETLFS
_relo0020 = . +1
  jsr GET_LOADPAR
  pla
_relo0021 = . +1
  jsr MY_IECLOAD_0
  bcs DOLO_ERR

  lda SY_VERIFY
  beq MYLO_1
  jmp $e17b                             ;verify error?

MYLO_1
  lda SY_SA                             ; SA=0
  bne DOLO_E

_relo0022 = . +1
  jsr BASLOAD_END_2
  jsr SY_BASCLR2
_relo0023 = . +1
  jmp INPUT_LOOP



;------------ EXECUTE SOFT RESET
INLO_UNNEW
_relo0025 = . +1
  jmp DO_UNNEW



;------------ EXECUTE TOKEN
INLO_LOOP2b
  ;cpx #TOK_RUN
  ;beq INLO_LOOP2a
  ;cpx #TOK_SYS
  ;beq INLO_LOOP2a

  cpx #TOK_RES
  beq INLO_RESET
  ;cpx #TOK_RELO
  cpx #TOK_NOIO                         ;NOIO
  beq INLO_NOIO

  ldy #0
  cpx #TOK_BLKP                         ;BLOCK WRITE PROTECT
  beq INLO_SETIO
  iny
  cpx #TOK_BLKD                         ;BLOCK DISABLE
  beq INLO_SETIO

  cpx #TOK_OLD
  beq INLO_UNNEW
  cpx #TOK_OFF
  beq INLO_OFF
  cpx #TOK_KILL
  beq INLO_KILL

  lda #0
  sta CHRPTR                            ;RESET CHRPTR TO START OF BIP
  beq INLO_LOOP2a





;------------ EXECUTE SOFT RESET
INLO_RESET
_relo0026 = . +1
  jsr RESET_IO
  jmp SOFT_RESET



;------------ KILL FE3 (IO,BANKS,WEDGE)
INLO_KILL
_relo0030 = . +1
  jsr INLO_OFF
_relo0031 = . +1
  jsr INLO_NOIO3
  lda #128
  ldx #$9f
_relo0032 = . +1
  jmp RESET_SYSTEM




;------------ DISABLE WEDGE
INLO_OFF
_relo5010 = . +1
  lda #<WEDGEMESSAGE1b
  ldy #>WEDGEMESSAGE1b
;_relo0033 = . +1
  jsr SY_STROUT
_relo5011 = . +1
  lda #<WEDGEMESSAGE1c
  ldy #>WEDGEMESSAGE1c
;_relo0034 = . +1
  jsr SY_STROUT
  jsr $e45b                             ;INIT BASIC VECTORS
  jsr $ff8a                             ;INIT IO VECTORS
  jmp $c480                             ;STD. INPUT LOOP




;------------ SET NOIO
INLO_NOIO
  lda #$80                              ;RAM, NO-IO
INLO_NOIO2
  ora IO_FINAL +1
  sta IO_FINAL +1
INLO_NOIO3
_relo5012 = . +1
  lda #<MSG_NOIO
  ldy #>MSG_NOIO
;_relo0035 = . +1
  jmp SY_STROUT



;------------ SET BLK IO
INLO_SETIO
  lda IO_FINAL +1
  sta DL_IOBASE +1
  lda IO_FINAL
  sta DL_IOBASE

  jsr CHRGOT
  beq PRINT_IO

  lda DL_IOBASE,y
  and #$e0
  sta DL_IOBASE,y
_relo0042 = . +1
  jsr SEL_BLOCK_P

  lda DL_IOBASE +1
  sta IO_FINAL +1
  lda DL_IOBASE
  sta IO_FINAL

PRINT_IO
_relo5013 = . +1
  lda #<MSG_PRINTIO
  ldy #>MSG_PRINTIO
;_relo0036 = . +1
  jsr SY_STROUT
  ldx BIP_IOBASE
  lda #0
_relo0037 = . +1
  jsr HEXOUT
  lda #"/"
_relo0040 = . +1
  jsr CHROUT
  ldx BIP_IOBASE +1
  lda #0
_relo0038 = . +1
  jsr HEXOUT
_relo0039 = . +1
  jmp CROUT



;------------
SEL_BLOCK_P
  jsr CHRGOT
  bcs SEBL_E
SEBL_1
  and #7
  tax
  lda #1
  cpx #5
  bcc SEBL_2                            ; <5
  bne SEBL_8
  dex                                   ; 5 --> 4
SEBL_2
  dex
  bmi SEBL_6
  asl
  bne SEBL_2
SEBL_6
  and #$1f
  ora DL_IOBASE,y
  sta DL_IOBASE,y
SEBL_8
  jsr CHRGET
  bcc SEBL_1
_relo0041 = . +1
  jsr CHKCOM
  bcc SEL_BLOCK_P
SEBL_E
  rts





; ==============================================================
; UNNEW (OLD) BASIC PROGRAM
; ==============================================================


DO_UNNEW
  ldy #$01
  tya
  sta (BASSTRT),Y                       ; store a non zero in the MSB of the first link addr
  jsr SY_PGMLINK                        ; relinker
  lda PT1                               ; set end of basic/start of variables to the address of the last '0' found + 2
  clc
  adc #$02
  sta BASVAR
  lda PT1 +1
  adc #$00
  sta BASVAR +1
  jsr SY_BASCLR2                        ; Reset TXTPTR and Perform [clr]
  ;lda #<UnNewMessage    ; string start point lo-byte
  ;ldy #>UnNewMessage    ; string start point hi-byte
  ;jsr PRNSTR            ; print string in A/Y, 0 terminated

  jmp BASIC_WARM        ; BASIC Warm Restart [RUNSTOP-RESTORE]

;UnNewMessage:
;	.byte "PROGRAM RESTORED",0



; ==============================================================
; GET STRING PARAM
; ==============================================================


GET_STRING
  lda F_CURDEV                          ; device
  sta SY_DN                             ; prime address
  jsr CHRGET
  tay
  beq GEST_5
  ldy #0
  sty FLGCOM
  cmp #34
  bne GEST_3
  sta FLGCOM
_relo0050 = . +1
  jsr INC_CHRPTR
GEST_3
  lda (CHRPTR),y
  beq GEST_5
  cmp FLGCOM
  beq GEST_5
  iny
  bne GEST_3

GEST_5
  tya                                   ; STR LEN
  ldx CHRPTR
  ldy CHRPTR +1
  jsr SETFNAM

  tay
  lda (CHRPTR),y
  beq GEST_7
  iny
GEST_7
  tya

ADD_CHRPTR
  clc
  adc CHRPTR
  sta CHRPTR
  bcc ADCH_1
ADCH_0
  inc CHRPTR +1
ADCH_1
  rts

INC_CHRPTR
  inc CHRPTR
  beq ADCH_0
  rts



; ==============================================================
; GET LOAD PARAM
; ==============================================================

GET_FILENAM
  lda BIP +1
  cmp #34
  beq GELO_2
  lda BIP +3
  cmp #34
  bne GELO_2
  inc CHRPTR
  inc CHRPTR
GELO_2
_relo0060 = . +1
  jmp GET_STRING

GET_LOADPAR
_relo0062 = . +1
  jsr GET_FILENAM
_relo0061 = . +1
  jsr FRMWORD2                          ; GET WORD VALUE
  bcs GELO_5

  ; SET LOAD ADDRESS
  ldx PT3
  lda PT3 +1
  stx LOADPTR
  sta LOADPTR +1

  ldx SY_SA                             ; SA
  beq GELO_5                            ; SA=0: LOAD AT

  ldx #2                                ; LOAD CARTRIDGE ON :: %"jj",adr
  stx SY_SA                             ; SA
GELO_5
  rts



; ==============================================================
; SET DEVICE#
; ==============================================================

SET_DEVICE
  jsr CHRGET
  beq PRINT_DEV

;_relo0070 = . +1

  jsr FRMBYTE
  cpx #4
  bcc SEDE_2
  cpx #13
  bcc SEDE_E
SEDE_2
  ldx #8
SEDE_E
  stx F_CURDEV

PRINT_DEV
_relo5070 = . +1
  lda #<MSG_DEVICE
  ldy #>MSG_DEVICE
;_relo0071 = . +1
  jsr SY_STROUT

  ldx F_CURDEV
  lda #0
  jsr PRNINT                            ;print integer in X/A

_relo0072 = . +1
  jmp CROUT




; ==============================================================
; SYS PROCS
; ==============================================================

;UNLISTEN = $ffae	                ; send UNLISTEN command
;LISTEN	 = $ffb1	                ; send LISTEN command
;LISTENSA = $ff93	                ; send SA for LISTEN command
;TALK     = $ffb4	                ; send TALK command
;UNTALK   = $ffab	                ; send UNTALK command
;TALKSA   = $ff96	                ; send SA for TALK command
;IECIN    = $ffa5	                ; get char from IEC
;IECOUT   = $ffa8	                ; send char to IEC

SETSTAT  = $fe6a	                ; set status
CHKSTOP  = $ffe1                        ; check stop key

LISTEN	 = JIF_LISTEN                   ; send LISTEN command
TALK     = JIF_TALK	                ; send TALK command
LISTENSA = JIF_LISTENSA                 ; send SA for LISTEN command
TALKSA   = JIF_TALKSA	                ; send SA for TALK command
UNLISTEN = JIF_UNLISTEN	                ; send UNLISTEN command
UNTALK   = JIF_UNTALK	                ; send UNTALK command
IECIN    = JIF_IECIN	                ; get char from IEC
IECOUT   = JIF_IECOUT	                ; send char to IEC



; ==============================================================
; MY LOAD
; ==============================================================

FRMNUM   = $cd8a	                ; GET NUMERIC VALUE
FRMBYTE  = $d79e	                ; GET BYTE VALUE TO X
CNVWORD  = $d7f7	                ; CONVERT TO WORD VALUE INTO Y/A; $14 (PT3)


  ; LOAD VECTOR                         :: "fnam",PA,SA[,loadadr]
MY_LOAD  subroutine
  ldx SY_DN                             ; PA (device#)
  cpx #4
  bcs .0
  jmp $f549                             ; OLD LOAD PROC


;MY_IECVERIFY
;  lda #1
;  bne MYLO_0

MY_IECLOAD
  lda #0
MY_IECLOAD_0
.0
  sta SY_VERIFY
  lda #0
  sta SY_FN                             ; file# - flag for first byte

  lda #$f0                              ; channel
_relo0080 = . +1
  jsr DISK_LISTEN
_relo0081 = . +1
  jsr IECNAMOUT
  bcc .00a
;MYLO_ERR2
  rts

.00a
  LDY #$00
  LDA ($BB),Y                           ;Filename
  CMP #$24                              ;"$"
  BNE .00b                              ;Directory? -->
  jmp $f56d                             ;normal LOAD


.00b
  lda #$60
_relo0082 = . +1
  jsr DISK_TALK



_relo0084 = . +1
  jsr IECIN                             ; load address lo
  sta LOADEND

  lda SY_STATUS
  lsr
  lsr
  bcc .00c
  jmp $f787

.00c
_relo0300 = . +1
  jsr IECIN                             ; load address hi
  sta LOADEND +1

  ldx SY_SA                             ; SA
  beq .00                               ; SA=0 -->

  dex
  dex
  bne .01                               ; SA=1 -->

.02                                     ; SA=2: LOAD CARTRIDGE AT $c3
  pha
  lda LOADEND
  pha                                   ; FIRST TWO BYTES ...

.00                                     ; SA=0: LOAD PROGRAM AT $c3
_relo0085 = . +1
  jsr FRMWORD2                          ; GET WORD VALUE
  lda LOADPTR +1
  ldx LOADPTR
  bcs .2

  lda PT3 +1
  ldx PT3

.2
  sta LOADEND +1
  stx LOADEND



.01
_relo0086 = . +1
  jsr PRINT_ATADR


  ldx SY_SA                             ; SA
  dex
  dex
  bne .3                                ; SA!=2 -->

  ;STORE FIRST TWO BYTES
  ldy #0
  pla
_relo0097 = . +1
  jsr STOREBYTE
  pla
_relo0098 = . +1
  jsr STOREBYTE

.3



;_relo0087 = . +1
;  jsr MY_IECIN
;  bcc MYLO_6

;MYLO_ERR
;_relo0302 = . +1
;  jsr DISK_CLOSE_LO
;  jmp $f787


;--------------JIFFY FASTLOAD INIT
.FASTLOAD
  BIT $A3
;  BVC .F253                             ;no Jiffy -->
  BVS .FB1F                             ;Jiffy -->
.F253
  JSR $F58A                             ;normales LOAD
.err2
  bcs .err

.MYLO_E
_relo0090 = . +1
  jsr DISK_CLOSE_LO

_relo0091 = . +1
  jsr PRINT_TOADR

  ;PRINT DISKERR ON ERROR
  lda SY_STATUS
  pha                                   ;SAVE IECSTAT FOR VERIFY!!!
_relo0412 = . +1
  jsr PRINT_DISK_ERR
  pla
  sta SY_STATUS

  clc
  ldx LOADEND
  ldy LOADEND +1
  rts


;--------------JIFFY FASTLOAD INIT
.FB1F
_relo0087 = . +1
  JSR UNTALK                            ;UNTALK
  lda #$61
_relo0088 = . +1
  jsr DISK_TALK
;--------------JIFFY FASTLOAD
  SEI
  LDA $B2
  PHA
  LDY #$00
.FB25
  JSR $F755                             ;STOP Taste abfragen
  CMP #$FE
  BEQ .FB5B
  LDA $912C
  AND #$DD
  TAX
  ORA #$20
  STA $B2
  STX $912C
  LDA #$80
  STA $9C
.FB3D
  LDA $911F
  LSR
  BCC .FB3D
  AND #$01
  BEQ .FB67
  LDX #$6D
.FB49
  BIT $911F
  BEQ .FB54
  DEX
  BNE .FB49
  LDA #$42
  .byte $2c                             ;BIT ABS
.FB54
  LDA #$40
  JSR $FE6A                             ;SET STATUS
  CLC
  .byte $24                             ;BIT ZP
.FB5B                                   ;STOP!
  SEC
  PLA
  STA $B2
  bcc .MYLO_E
.err
  JMP $F6CB                             ;UNLISTEN, CLOSE, BREAK
; JMP $F5BF                             ;UNLISTEN, CLOSE


.FB67
  LDA #$02
.FB69
  BIT $911F
  BEQ .FB69
.FB6E
  PHA
  PLA
  NOP
  LDA $B2
  STA $912C
  LDA #$01
  BIT $911F
  BEQ .FB25
  STX $912C
  LDA $911F
  ROR
  ROR
  NOP
  AND #$80
  ORA $911F
  ROL
  ROL
  NOP
  STA $B3
  LDA $911F
  ROR
  ROR
  AND $9C
  ORA $911F
  ROL
  ROL
  STA $C0

;_relo0430 = . +1
;  JSR lEC4E                            ;Byte zusammenbauen aus 2 Nibble
;_relo0089 = . +1
;  jsr STOREBYTE
;  CLV
;  BVC .FB6E

  lda #>(.FB6E -1)                      ;Rücksprungadresse auf Stack
  pha
  lda #<(.FB6E -1)
  pha
_relo0430 = . +1
  JSR lEC4E                             ;Byte zusammenbauen aus 2 Nibble

STOREBYTE subroutine
  CPY $93
  BNE .FBB0
  STA ($AE),Y
.FBA7
  INC $AE
  BNE .FB6E
  INC $AF
.FB6E
  rts

.FBB0                                   ;VERIFY
  CMP ($AE),Y
  BEQ .FBA7
  LDA #$10                              ;VERIFY ERROR
  STA $90
  BNE .FBA7



;--------------JIFFY FASTLOAD









;.MYLO_5
;  jsr CHKSTOP
;  bne MYLO_05a

;_relo0088 = . +1
;  jsr DISK_CLOSE_LO
;  jmp $f6ce


;.MYLO_05a
;_relo0089 = . +1
;  jsr MY_IECIN2
;.MYLO_6
;  ldy SY_VERIFY
;  beq MYLO_7                            ; --> load

;  ldy #0
;  cmp (LOADEND),y
;  beq MYLO_8

;  lda #$10
;  jsr SETSTAT
;  .byte $2c
;.MYLO_7
;  sta (LOADEND),y
;.MYLO_8
;  inc LOADEND
;  bne MYLO_7a
;  inc LOADEND +1
;.MYLO_7a
;  bit SY_STATUS
;  bvc MYLO_5                            ; EOI?





  ; PRINT LOAD AT ADDRESS
PRINT_ATADR subroutine
  lda #LOADEND
PRINT_ATADR_2
  ldx DIRECT_MODE
  bmi .1
.rts
  rts

.1
  pha
_relo5090 = . +1
  lda #<MSG_LOADAT
  ldy #>MSG_LOADAT
;_relo0096 = . +1
  jsr SY_STROUT
  pla
.3
  tax
HEXOUT_ZP
  lda 1,x
  pha
  lda 0,x
  tax
  pla
_relo0095 = . +1
  jmp HEXOUT

  ; PRINT LOAD AT ADDRESS
PRINT_TOADR
  lda #LOADEND
PRINT_TOADR_2
  ldx DIRECT_MODE
  bpl .rts

  pha
_relo5091 = . +1
  lda #<MSG_LOADTO
  ldy #>MSG_LOADTO
  jsr SY_STROUT
  pla
_relo0093 = . +1
  jsr .3
_relo0094 = . +1
  jmp CROUT



; ========================================================================
; MY SAVE                   ENDADDR   = ($AE/$AF)    STARTADDR = ($C1/$C2)
;
; SAVESTART = $c1
; LOADPTR   = $c3
; LOADSTART = $ac
; LOADEND   = $ae
; ========================================================================

  ; SAVE VECTOR                         :: "fnam",PA,SA[,fromadr,toaddr]
MY_SAVE subroutine
  ldx SY_DN                             ; PA (device#)
  cpx #4
  bcs MY_IECSAVE
  jmp $f685                             ; OLD LOAD PROC


MY_IECSAVE
_relo0420 = . +1
  jsr FRMWORD2                          ; GET WORD VALUE
  bcs MYSA_0

  sty LOADSTART
  sta LOADSTART +1

_relo0421 = . +1
  jsr FRMWORD2                          ; GET WORD VALUE
  bcs MYSA_0

  sty LOADEND
  sta LOADEND +1

  ldy LOADSTART
  lda LOADSTART +1
  sty SAVESTART
  sta SAVESTART +1


MYSA_0
  lda #$f1                              ; channel
_relo0400 = . +1
  jsr DISK_LISTEN
_relo0401 = . +1
  jsr IECNAMOUT
  bcs .err

  lda #$61
_relo0402 = . +1
  jsr DISK_LISTEN

  jsr $fbd2                             ; $C1/$C2 --> $ac/$ad

  lda LOADSTART
_relo0403 = . +1
  jsr IECOUT
  lda LOADSTART +1
_relo0404 = . +1
  jsr IECOUT

  lda #LOADSTART
  jsr PRINT_ATADR_2

  ldy #0
MYSA_00
  jsr $fd11                             ;END ADDRESS?
  bcs MYSA_E0                           ;YES -->
  lda (LOADSTART),y
_relo0405 = . +1
  jsr IECOUT

  jsr CHKSTOP
  bne MYSA_02

_relo0406 = . +1
  jsr UNLISTEN
_relo0407 = . +1
  jsr DISK_CLOSE_SA
  jmp $f6ce


MYSA_02
  jsr $fd1b                             ;INCR ADDR
  bne MYSA_00

MYSA_E0
_relo0408 = . +1
  jsr UNLISTEN
_relo0409 = . +1
  jsr DISK_CLOSE_SA
  lda #LOADSTART
_relo0410 = . +1
  jsr PRINT_TOADR_2
_relo0411 = . +1
  jsr PRINT_DISK_ERR
  beq .rts
.err
  sec
.rts
  rts




; ==============================================================
; DISK CMD
; ==============================================================

  ;DISK CMD PARAM       :: +@"cmd"
DISK_CMD
_relo0013 = . +1
  jsr GET_STRING

DISK_CMD_2
  ldx LEN_FNAM
  beq PRINT_DISK_STAT
_relo0100 = . +1
  jsr DISK_LISTEN_6F


  ;PUT NAME TO IEC AND UNLISTEN
IECNAMOUT
  lda IECSTAT
  bmi DICM_ERR1

_relo0363 = . +1
  jsr IECNAMOUT_2
DICM_OK2
_relo0306 = . +1
  jsr UNLISTEN
DICM_OK
  clc
  rts



  ;PUT NAME TO IEC
IECNAMOUT_2
  ldx LEN_FNAM
  beq DICM_OK2
  ldy #0
DICM_2
  lda (PTR_FNAM),y
_relo0305 = . +1
  jsr IECOUT
  iny
  dex
  bne DICM_2
  rts





  ;CHECK 'DEVICE NOT PRESENT'
CHKDNP
_relo0106 = . +1
  jsr DISK_LISTEN_6F
  bcc DICM_OK2
DICM_ERR1
  jmp $f78a                             ;ERR 'DEVICE NOT PRESENT'    CF=1


DISK_LISTEN_6F
  lda #$6f                              ; channel
DISK_LISTEN
  pha
  lda #0
  sta IECSTAT
  beq DILI_2

DISK_LISTEN_2
  pha
DILI_2
  lda SY_DN                             ; device#
_relo0307 = . +1
  jsr LISTEN
  pla
_relo0308 = . +1
  jsr LISTENSA
DITA_5
  lda IECSTAT
  bpl DICM_OK
  sec
  rts


DISK_TALK
  pha
  lda #0
  sta IECSTAT

  lda SY_DN                             ; device#
_relo0309 = . +1
  jsr TALK
  pla
_relo0310 = . +1
  jmp TALKSA


DISK_CLOSE_SA
  lda #$e1
  bne DICL_1

DISK_CLOSE_LO
_relo0303 = . +1
  jsr UNTALK
  lda #$e0
DICL_1
_relo0101 = . +1
  jsr DISK_LISTEN_2
_relo0311 = . +1
  jmp UNLISTEN


; PRINT DISK STATUS TO SCREEN
PRINT_DISK_STAT
_relo0102 = . +1
  jsr GET_DISK_STAT
PRINT_BIP_0
  bcs PRDI_E
PRINT_BIP
  ldy #0
PRDI_2
  lda STP,y
  beq PRDI_7
  jsr BSOUT
  iny
  bne PRDI_2
PRDI_7
  tya
;  beq PRDI_E
  ;lda #13
  ;jsr BSOUT
PRDI_E
  rts


; PRINT DISK STATUS TO SCREEN IF ERROR
PRINT_DISK_ERR
_relo0103 = . +1
  jsr GET_DISK_STAT
  bne PRINT_BIP_0
  rts



; LOAD DISK STAT TO BIP
GET_DISK_STAT
_relo0104 = . +1
  jsr CHKDNP
  bcs DIST_ERR

  lda #$6f                              ; channel
_relo0105 = . +1
  jsr DISK_TALK

  ldy #0
DIST_2
_relo0312 = . +1
  jsr IECIN
  sta STP,y
  iny
  lda IECSTAT
  beq DIST_2

_relo0313 = . +1
  jsr UNTALK
  tya
  beq DIST_7
  lda #13
  cmp STP,y
  bne DIST_7
  sta STP,y
  iny
DIST_7
  lda #0
  sta STP,y
  lda STP
  cmp #"0"
  bne DIST_E
  eor STP +1
  beq DIST_E
  cmp #1
DIST_E
  clc
DIST_ERR
  rts






; ==============================================================
; DISK CATALOG
; ==============================================================

  ;DISK CATALOG       :: +$"fil"
PRINT_CATALOG
_relo0010 = . +1
  jsr GET_STRING

  lda #$f0                              ; channel
_relo0110 = . +1
  jsr DISK_LISTEN
  bcc PRCA_0
  rts

PRCA_0
  lda #"$"
_relo0314 = . +1
  jsr IECOUT
_relo0111 = . +1
  jsr IECNAMOUT_2
  lda #"*"
_relo0362 = . +1
  jsr IECOUT
_relo0365 = . +1
  jsr UNLISTEN

  lda #$60
_relo0112 = . +1
  jsr DISK_TALK

;PRINT HEADLINE (DISKNAME)

  ldx #6
_relo0113 = . +1
  jsr PRCA_SKIP_X                       ; skip x bytes

;PRCA_1
_relo0114 = . +1
  jsr PRCA_SKIPSPC                      ; skip spaces

  cmp #18                               ; check for reverse on char (Disk Name, first row only)
  bne PRCA_EE

_relo0115 = . +1
  jsr CHROUT                            ; here on first row only (Disk Name)
_relo0315 = . +1
  jsr IECIN                             ; skip a char (")

_relo0116 = . +1
  jsr PRCA_QSTR1                        ; PRINT QUOTED STRING

  ldx #19
_relo0117 = . +1
  jsr PRCA_SPCOL                        ; print spaces up to column
_relo0118 = . +1
  jsr PRCA_SKIPSPC                      ; skip spaces

_relo0119 = . +1
  jsr CHROUT                            ; ID1
_relo0316 = . +1
  jsr IECIN
_relo0120 = . +1
  jsr CHROUT                            ; ID2

;PRINT STD. LINE (FILE)

PRCA_4
_relo0317 = . +1
  jsr IECIN
  tax
  bne PRCA_4

PRCA_5
  lda dir_current_key
  cmp #24                               ; check for RUN-STOP key
  beq PRCA_STOP

  lda dir_shift_ctrl_cbm_flag
  and #2                                ; filter commodore key (Bit 2)
  bne PRCA_5

_relo0135 = . +1
  jsr CROUT                             ; CR

  ldx #2
_relo0121 = . +1
  jsr PRCA_SKIP_X                       ; skip x bytes

  lda IECSTAT
  bne PRCA_E

_relo0318 = . +1
  jsr IECIN
  tax                                   ;LO
  ;sta PT3
_relo0319 = . +1
  jsr IECIN
  ;ldx PT3
  jsr PRNINT                            ;print integer in X/A

_relo0122 = . +1
  jsr PRCA_SKIPSPC
  cmp #34
  bne PRCA_8
  pha
  ldx #2
_relo0123 = . +1
  jsr PRCA_SPCOL                        ; print spaces up to column
  pla
_relo0124 = . +1
  jsr PRCA_QSTR1                        ; PRINT QUOTED STRING

  ldx #20
_relo0125 = . +1
  jsr PRCA_SPCOL                        ; print spaces up to column

_relo0126 = . +1
  jsr PRCA_SKIPSPC
_relo0127 = . +1
  jsr CHROUT                            ; FILETYP
  clv
  bvc PRCA_4



PRCA_8
  pha
_relo0128 = . +1
  jsr SPCOUT                            ; BLANK
  pla
PRCA_9
_relo0129 = . +1
  jsr CHROUT                            ;
_relo0320 = . +1
  jsr IECIN
  tax
  bne PRCA_9

PRCA_STOP
_relo0130 = . +1
  jsr CROUT                             ; CR
PRCA_E
PRCA_EE
PRCA_ERR
_relo0131 = . +1
  jmp DISK_CLOSE_LO



; SKIP SPACE
PRCA_SKIPSPC
_relo0322 = . +1
  jsr IECIN
  cmp #32                               ;spaces
  beq PRCA_SKIPSPC
  rts

; SKIP X BYTES
PRCA_SKIP_X
_relo0323 = . +1
  jsr IECIN
  dex
  bne PRCA_SKIP_X
  rts

; PRINT QUOTED STRING
PRCA_QSTR1
_relo0132 = . +1
  jsr CHROUT                            ; else print character in A
PRCA_QSTR2
  ldy #17
PRCAQS_2
_relo0324 = . +1
  jsr IECIN
_relo0133 = . +1
  jsr CHROUT                            ; else print character in A
  cmp #34                               ; check for "quote" char
  beq PRCAQS_E
  dey
  bne PRCAQS_2
PRCAQS_E
  rts


PRCA_SP_1
_relo0134 = . +1
  jsr SPCOUT

; PRINT SPACE UP TO COL
PRCA_SPCOL
  cpx C_COL
  bcs PRCA_SP_1
  rts



; ==============================================================
; DISPLAY CHAR
; ==============================================================


  ;PRINT 2 BLANK
SPCOUT2 subroutine
_relo0141 = . +1
  jsr SPCOUT
SPCOUT
  lda #32
  bne CHROUT

  ;CLEAR SCREEN
CLROUT subroutine
  lda #CLRHOME
  bne CHROUT

  ;PRINT CR
CROUT subroutine
  lda #13
  bne CHROUT

  ;PRINT 2 CHR IN AC/XR
CHROUT2 subroutine
_relo0142 = . +1
  jsr CHROUT
  txa
CHROUT subroutine
  pha
  sta $d7
  txa
  pha
  tya
  pha
  lda $d7
  bmi .7
  cmp #$20
  bcs .1
  jmp $e756

.1
  cmp #$60
  bcc .2
  and #$df
  bne .3
.2
  and #$3f
.3
CHOU_3
  ldx $c7                           ;revers?
  beq .4
  ora #$80
.4
  ldx $0286                         ;Farbcode
  jsr $eaa1                         ;Zeichen ausgeben
  lda $d3
  cmp #$15
  beq .5
  inc $d3
.5
  pla
  tay
  pla
  tax
  pla
  rts

.7
  and #$7f
  cmp #$20
  bcs .8
  jmp $e82a
.8
  ora #$40
  bne .3

PUTCHR subroutine
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

CONVCHR subroutine
  STA C_CHR

  and #$7f
  cmp #$20
  bcc .9
  ldx C_CHR
  bpl .1
  ora #$40
  bne .3

.1
  cmp #$60
  bcc .2
  and #$df
  bne .3
.2
  and #$3f
.3
  ldx $c7                           ;revers?
  beq .4
  ora #$80
.4
  sec
.9
  rts


; ==============================================================
; EVALUATE NEXT FORMULA ELEMENT
; ==============================================================

MY_FRMELEM subroutine
  lda #0
  sta $0d
  jsr CHRGET
  cmp #"$"
  beq MYFR_HEX
  cmp #"%"
  beq MYFR_BIN

  jsr CHRGOT
  jmp $ce8d                             ; ORIGINAL PROC


MYFR_HEX subroutine
_relo0150 = . +1
  jsr GETHEX                            ; GET HEX NUMBER A=HI, X=LO, C=0:error
  bcc ERR_TYPMIS

AXFAC
  stx FAC +2
  sta FAC +1
AXFAC_1
  ldx #$90
  sec
  jmp $dc49


MYFR_BIN
  ldx #16
  lda #0
  sta FAC+1
  sta FAC+2

MYBI_1
  jsr CHRGET
  cmp #"1"
  beq MYBI_2
  cmp #"0"
  bne MYBI_4
  clc
MYBI_2
  rol FAC+2
  rol FAC+1
  dex
  bne MYBI_1
  jsr CHRGET
MYBI_3
_relo0151 = . +1
  jmp AXFAC_1

MYBI_4
  txa
  and #19
  beq MYBI_3

ERR_TYPMIS
  ldx #$16
  jmp $c437                             ; ERROR


; ==============================================================
; CONVERT VALUES BETWEEN HEX/BIN
; ==============================================================

GETADR  subroutine                      ; GET HEX NUMBER A=HI, X=LO, C=0:error
  jsr CHRGOT
  cmp #"$"                              ; HEX VALUE
  beq GETHEX
  clc
  rts


  ; GET WORD VALUE IN Y/A AND (PT3)
FRMWORD2
_relo0160 = . +1
  jsr CHKCOM
  bcs .3
FRMWORD
  jsr FRMNUM
  jsr CNVWORD
  clc
.3
  rts


CHKCOM subroutine
  jsr CHRGOT
  cmp #$2c                              ; ","
  sec
  bne .3
  jsr CHRGET
  clc
.3
  rts




GETHEX subroutine                       ; GET HEX NUMBER A=HI, X=LO, C=0:error
  jsr CHRGET

HEX_0
_relo0170 = . +1
  jsr HEXBYTE
  bcc HEX_E
  pha
  jsr CHRGET
_relo0171 = . +1
  jsr HEXBYTE
  bcs HEX_2
  pla
  tax
  lda #0
  beq HEX_3

HEX_2
  tax
  jsr CHRGET
  pla
HEX_3
  sec
HEX_E
  rts

HEXBYTE
  jsr CHRGOT
_relo0172 = . +1
  jsr HEXCHAR
  bcc HEBY_E
_relo0173 = . +1
  jsr ASCBYTE
  asl
  asl
  asl
  asl
  sta FAC
  jsr CHRGET
_relo0174 = . +1
  jsr HEXCHAR
  bcc HEBY_E
_relo0175 = . +1
  jsr ASCBYTE
  ora FAC
  sec
HEBY_E
  rts

HEXCHAR
  bcc HECH_2
  cmp #65                                 ; "A"
  bcc HECH_E
  sbc #91
  sec
  sbc #165
HECH_E
  rts
HECH_2
  sec
  rts

ASCBYTE
  cmp #58
  php
  and #15
  plp
  bcc ASBY_E
  adc #8
ASBY_E
  rts



; ==============================================================
; PRINT WORD VALUE BETWEEN HEX/BIN
; ==============================================================
PRINT_VALUE
_relo0180 = . +1
  jsr SPCOUT
  ldx PT3
  lda PT3 +1
_relo0181 = . +1
  jsr HEXOUT

_relo0182 = . +1
  jsr SPCOUT2
  ldx PT3
  lda PT3 +1
  jsr PRNINT                            ;print integer in X/A

_relo0184 = . +1
  jsr SPCOUT2
  ldx PT3
  lda PT3 +1
_relo0185 = . +1
  jsr ASCOUT

_relo0186 = . +1
  jsr CROUT
_relo0187 = . +1
  jsr SPCOUT
  ldx PT3
  lda PT3 +1
_relo0188 = . +1
  jsr BINOUT
_relo0189 = . +1
  jmp CROUT



;------------ PRINT HEX VALUE IN  X/A
HEXOUT subroutine
  pha
  lda #"$"
  jsr BSOUT
  pla
  beq .0

HEXOUT4
_relo0200 = . +1
  jsr .2
.0
  txa
HEXOUT2  
.2
  pha
  lsr
  lsr
  lsr
  lsr
_relo0201 = . +1
  jsr .1
  pla
  and #15
.1
  clc
  adc #246
  bcc .12
  adc #6
.12
  adc #58
  jmp BSOUT




;------------ PRINT BIN VALUE IN  X/A
BINOUT
  pha
  lda #"%"
  jsr BSOUT
  pla
  beq BIN0
_relo0211 = . +1
  jsr BIN2
BIN0
  txa
BIN2
  ldy #8
BIN2a
_relo0212 = . +1
  jsr BIN1
  dey
  bne BIN2a
  rts

BIN1
  asl
  pha
  lda #"0"
  bcc BIN1a
  lda #"1"
BIN1a
  jsr BSOUT
  pla
  rts




;------------ PRINT ASC VALUE IN  X/A
ASCOUT
  pha
  txa
  tay
  lda #34
_relo0220 = . +1
  jsr CHROUT
  pla
  beq ASC0
_relo0221 = . +1
  jsr ASC2
ASC0
  tya
ASC2
  ldx #0
  stx C_CTRL
  tax
  and #127
  cmp #32
  bcc ASC1_1

  lda #RVSOFF
  bne ASC1_2

ASC1_1
  txa
  clc
  adc #64
  tax
  lda #RVSON
ASC1_2
_relo0222 = . +1
  jmp CHROUT2




; ==============================================================
; TOKENIZER                   X=Token#, 0=no token found
; ==============================================================

TOKENIZER subroutine
  jsr CHRGOT
_relo5230 = . +1
  lda #<TOKEN
  ldx #>TOKEN
  sta PT3
  stx PT3 +1

  ldx #1
TOZE_1
  ldy #0
TOZE_2
  lda (PT3),y
  beq TOZE_10
  sec
  sbc (CHRPTR),y
  beq TOZE_3
  cmp #$80
  beq TOZE_10

  ;NEXT TOKEN
TOZE_8
  iny
  lda (PT3),y
  bne TOZE_8

  tya
  sec
  adc PT3
  sta PT3
  bcc TOZE_9
  inc PT3 +1
TOZE_9
  inx
  bne TOZE_1

TOZE_3
  iny
  bne TOZE_2

  ;TOKEN FOUND?
TOZE_10
  tya
  beq TOZE_11

  ;TOKEN FOUND!
_relo0230 = . +1
  jsr ADD_CHRPTR                        ;SET CHRPTR AFTER TOKEN
  txa
_relo0231 = . +1
  lda TOKEN_TAB -1,x
TOZE_11
  tax
TOZE_E
  rts

NEXT_STATEMENT
  jsr CHRGET
  bne NEXT_STATEMENT                    ;SEARCH NEXT STATEMENT
  rts







;-------- SWITCH CARTRIDGE AND RESET SYSTEM

VIC    = $9000
VIA1   = $9110
VIA2   = $9120

RESET_SYSTEM
  pha
  txa
  pha

  ldy #(__SET_IO_RESET_E - __SET_IO_RESET)
_relo5250 = . +1
  ldx #<__SET_IO_RESET
  lda #>__SET_IO_RESET
_relo0250 = . +1
  jsr COPY_PROC

_relo0251 = . +1
  jsr RESET_IO

  pla
  tax
  pla
  jmp BIP

;-------- COPY PROCEDURE TO BIP AND EXECUTE
COPY_PROC subroutine
  sta PT1 +1
  stx PT1
  dey
.2
  lda (PT1),y
  sta BIP,y
  dey
  bpl .2
  rts




;-------- RAM PROCEDURE TO SWITCH CARTRIDGE AND RESET
__SET_IO_RESET
  sta IO_FINAL
  stx IO_FINAL +1
  jmp SOFT_RESET
__SET_IO_RESET_E





RESET_IO
  sei
  lda #$00
  sta VIA1 +$02                         ; DDRA
  sta VIA1 +$03                         ; DDRB
  sta VIA2 +$02                         ; DDRA
  sta VIA2 +$03                         ; DDRB

  lda #$7f
  sta VIA1 +$0e                         ; IER: clear all bits
  sta VIA2 +$0e                         ; IER: clear all bits
  rts



; ==============================================================
; JIFFY PROCS
; ==============================================================

;--------------JIFFY LISTEN
JIF_TALK
  ORA #$40
  .byte $2c
JIF_LISTEN
  ORA #$20
  JSR $F160                             ;SET TIMER
lEE1C
  PHA
  BIT $94
  BPL l6E2B
  SEC
  ROR $A3
_relo0361 = . +1
  JSR lfc41                             ;NEW BYTE OUT
  LSR $94
  LSR $A3
l6E2B
  PLA
  STA $95

  ;JSR lF19A                            ;NEW DAV hi
  SEI
  LDA #$00
  STA $A3
  JSR $E4A0                             ;DAV hi

  CMP #$3F
  BNE l6E38
  JSR $EF84                             ;NDAC lo
l6E38
  LDA $911F
  ORA #$80
  STA $911F
lEE40
  JSR $EF8D                             ;PCR BIT 1 LÖSCHEN
  JSR $E4A0
  JSR $EF96
  ;jmp $ee49                            ;ORIG BYTE OUT

OLD_IECOUT
  SEI
  JSR $E4A0                             ;DAV lo
  JSR $E4B2                             ;NRFD hi
  LSR
  BCS l6EB4                             ;err DEV NOT PRES

  JSR $EF84                             ;NDAC lo
  BIT $A3
  BPL l6E66
l6E5A
  JSR $E4B2                             ;NRFD hi
  LSR
  BCC l6E5A
l6E60
  JSR $E4B2                             ;NRFD hi
  LSR
  BCS l6E60
l6E66
  JSR $E4B2                             ;NRFD hi
  LSR
  BCC l6E66
  JSR $EF8D                             ;PCR BIT 1 LÖSCHEN

  TXA
  PHA
  LDX #$08                              ;8 BIT

l6E73
  LDA $911F
  AND #$02
  BNE l6E7F
  PLA
  TAX
  JMP $EEB7                             ;ERR TIMEOUT

l6E7F
  JSR $E4A0                             ;DAV hi
  ROR $95
  BCS l6E89
  JSR $E4A9                             ;DAV lo
l6E89
  JSR $EF84                             ;NDAC lo
  LDA $912C
  AND #$DD
  ORA #$02
  PHP
  PHA
_relo0350 = . +1
  JSR lF96E
  PLA
  PLP
  DEX
  BNE l6E73

  PLA
  TAX
  jmp $EEA0
;  NOP
;lEEA5
;  LDA #$04
;  STA $9129
;l6EA5
;  LDA $912D
;  AND #$20
;  BNE l6EB7
;  JSR $E4B2
;  LSR
;  BCS l6EA5
;  CLI
;  RTS

l6EB4
  jmp $eeb4                             ;err DEV NOT PRES

l6EB7
  jmp $eeb7                             ;err TIME OUT


lF96E
  STA $912C
  BIT $911F
  BPL lF997
  CPX #$02
  BNE lF997
  LDA #$02
  LDX #$20
lF97E
  BIT $911F
  BEQ lF988
  DEX
  BNE lF97E
  BEQ lF995
lF988
  BIT $911F
  BEQ lF988
  LDA $95
  ROR
  ROR
  ORA #$40
  STA $A3
lF995
  LDX #$02
lF997
  rts





;--------------JIFFY BYTE IN
JIF_IECIN
lfbe0                                   ;NEW BYTE IN??
  sei
  bit $a3
  bvs l7be5
  LDA #$00
  JMP $EF1C                             ;ORIG BYTE IN

JIFFY_IN
l7be5
  LDA $911F
  AND #$03
  BEQ l7be5
  LDA #$80
  STA $9C
  TXA
  PHA
  PHA
  PLA
  PHA
  PLA
  LDA $912C
  AND #$DD
  STA $912C
  ORA #$20
  TAX
  BIT $9C
  BIT $9C
  BIT $9C
  LDA $911F
  ROR
  ROR
  NOP
  AND #$80
  ORA $911F
  ROL
  ROL
  STA $B3
  LDA $911F
  ROR
  ROR
  AND #$80
  NOP
  ORA $911F
  ROL
  ROL
  STA $C0
  LDA $911F
  STX $912C
  STA $9C
_relo0351 = . +1
  JSR lEC4E                             ;BYTE AUS 2 NIBBLES

  STA $A4
  PLA
  TAX
  LDA $9C
  ROR
  ROR
  BPL l7C54
  BCC lfC4f
  LDA #$42
  JMP $EEB9                             ;ERR STATUS, UNLISTEN
;--------------JIFFY BYTE IN


;--------------JIFFY BYTE OUT
JIF_IECOUT
  BIT $94
  BMI lEEED
  SEC
  ROR $94
  BNE lEEF2
lEEED
  PHA
_relo0352 = . +1
  JSR NEW_IECOUT
  PLA
lEEF2
  STA $95
  CLC
  RTS


NEW_IECOUT
lfc41                                   ;NEW BYTE OUT
  sei
  bit $a3
  bvs lfc59
  LDA $A3
  CMP #$A0
  BCS lfc59
_relo0353 = . +1
  JMP OLD_IECOUT
  ;JMP $EE49                             ;ORIG BYTE OUT

lfC4f
  LDA #$40
  JSR $FE6A                             ;SET STATUS
l7C54
  LDA $A4
l7C56
  CLI
  CLC
  RTS


JIFFY_OUT
lfc59                                   ;JIFFY BYTE OUT
  TXA
  PHA
  LDA $95
  LSR
  LSR
  LSR
  LSR
  TAX
_relo0354 = . +1
  LDA lFCCE,X
  PHA
  TXA
  LSR
  LSR
  TAX
_relo0355 = . +1
  LDA lFCCE,X
  STA $B3
  LDA $95
  AND #$0F
  TAX
  LDA #$02
l7C76
  BIT $911F
  BEQ l7C76

  LDA $912C
  AND #$DD
  STA $9C
  PHA
  PLA
  PHA
  PLA
  NOP
  NOP
  NOP
  STA $912C
  PLA
  ORA $9C
  NOP
  STA $912C
  LDA $B3
  ORA $9C
  ORA $9C
  STA $912C
_relo0356 = . +1
  LDA lFBBA,X
  ORA $9C
  NOP
  STA $912C
_relo0357 = . +1
  LDA lF39E,X
  ORA $9C
  NOP
  STA $912C
  NOP
  AND #$DD
  BIT $A3
  BMI l7CB7
  ORA #$02
l7CB7
  STA $912C
  PLA
  TAX
  NOP
  LDA $9C
  ORA #$02
  STA $912C
  LDA $911F
  AND #$02
  BEQ l7C56
  JMP $EEB7                             ; err TIME OUT
;--------------JIFFY BYTE OUT



;--------------BAUT EIN BYTE AUS 2 NIBBLES ZUSAMMEN
lEC4E
  LDA $B3
  AND #$0F
  STA $B3
  LDA $C0
  ASL
  ASL
  ASL
  ASL
  ORA $B3
  RTS
;--------------JIFFY BYTE IN



;--------------JIFFY UNTALK/UNLISTEN
JIF_UNTALK
lEEF6
  LDA $911F
  ORA #$80                              ;ATN ausgeben
  STA $911F
  JSR $EF8D
  LDA #$5F
  .byte $2c
JIF_UNLISTEN
  LDA #$3F
_relo0358 = . +1
  JSR lEE1C                             ;PART OF LISTEN
  JSR $EEC5
  TXA
  LDX #$0B
lEF0F
  DEX
  BNE lEF0F
  TAX
  JSR $EF84
  JMP $E4A0
;--------------JIFFY UNTALK/UNLISTEN



;--------------JIFFY TALK SA
JIF_TALKSA
  STA $95
_relo0359 = . +1
  JSR lEE40
  jmp $eed3
;--------------JIFFY TALK SA


;--------------JIFFY LISTEN SA
JIF_LISTENSA
  STA $95
_relo0360 = . +1
  JSR lEE40
  jmp $eec5
;--------------JIFFY LISTEN SA




;--------------JIFFY DATA TABLE
lFCCE
  .byte $00,$02,$20,$22,$00,$02,$20,$22,$00,$02,$20,$22,$00,$02,$20,$22

lFBBA
  .byte $00,$00,$20,$20,$00,$00,$20,$20,$02,$02,$22,$22,$02,$02,$22,$22

lF39E
  .byte $00,$20,$00,$20,$02,$22,$02,$22,$00,$20,$00,$20,$02,$22,$02,$22
;--------------JIFFY DATA TABLE





; ==============================================================
; LOADER TEXTE AND TOKEN TABLE
; ==============================================================

TOK_NOIO = 1
TOK_BLKP = TOK_NOIO +1
TOK_BLKD = TOK_NOIO +2
TOK_RES  = TOK_NOIO +3
TOK_RELO = TOK_NOIO +4
TOK_RUN  = TOK_NOIO +5
TOK_SYS  = TOK_NOIO +6
TOK_OLD  = TOK_NOIO +7
TOK_OFF  = TOK_NOIO +8
TOK_KILL = TOK_NOIO +9



TOKEN
  dc.b "NOIO",0,"BLKD",0,"BLKP",0,"BLK",0
MSG_RES
  dc.b "RESET",0
  dc.b "OFF",0
  dc.b "KILL",0
  dc.b "OLD",0
  dc.b "UNNEW",0
  dc.b "RE",$93,0                       ; RELOAD
  dc.b $8a,0                            ; RUN
  dc.b $9e,0                            ; SYS
  dc.b 0                                ; ::END

TOKEN_TAB
  dc.b TOK_NOIO,TOK_BLKD,TOK_BLKP,TOK_BLKP,TOK_RES,TOK_OFF,TOK_KILL,TOK_OLD,TOK_OLD
  dc.b TOK_RELO,TOK_RUN,TOK_SYS






; ==============================================================
; MESSAGE TEXTE
; ==============================================================


MSG_LOADAT
  dc.b " FROM ",0
MSG_LOADTO
  dc.b " TO ",0

MSG_DEVICE
  dc.b "DEVICE#",0


MSG_PRINTIO
  dc.b "IO=",0
MSG_NOIO
  dc.b "IO OFF",13,0


MY_WEDGE_END
; == END OF MINI WEDGE ============================================================



WEDGEMESSAGE1b
  dc.b $11, RED, "FE3 WEDGE (", 0
WEDGEMESSAGE1c
  dc.b "OFF)",BLUE,13,0











; ==============================================================
; JIFFY IO CODE (CHKIN, CHKOUT, BASIN, BSOUT)   from SJLOAD-128
; ==============================================================

PTR_CHKIN       = $031e
PTR_CHKOUT      = $0320
PTR_BASIN       = $0324
PTR_CLRCH       = $0322
PTR_BASOUT      = $0326
PTR_GETIN       = $032a
PTR_CLRALL      = $032c

;-------------------- WEDGE INIT
SET_BASE_VECTORS
  lda #<JChkIn
  ldx #>JChkIn
  sta PTR_CHKIN
  stx PTR_CHKIN +1
  lda #<JChkOut
  ldx #>JChkOut
  sta PTR_CHKOUT
  stx PTR_CHKOUT +1
  lda #<JBasIn
  ldx #>JBasIn
  sta PTR_BASIN
  stx PTR_BASIN +1
  lda #<JBasOut
  ldx #>JBasOut
  sta PTR_BASOUT
  stx PTR_BASOUT +1
  lda #<JGetIn
  ldx #>JGetIn
  sta PTR_GETIN
  stx PTR_GETIN +1
  lda #<JClrCh
  ldx #>JClrCh
  sta PTR_CLRCH
  stx PTR_CLRCH +1
  lda #<JClrAll
  ldx #>JClrAll
  sta PTR_CLRALL
  stx PTR_CLRALL +1
  rts




;
; JIFFY CHKIN    ($031e vector)
;
JChkIn subroutine
  jsr $f3cf                             ;search logical file#
  beq .1	                        ;file not open error
  jmp $f784                             ;err "file not open"

.1
  jsr $f3df                             ;set file param
  lda SY_DN                             ;device#
  cmp #8
  bcs .2
  jmp $f2d2                             ;std. ChkIn

.2
  tax
  jsr TALK
  lda SY_SA
  bpl .3
  jmp $f2f8

.3
  jsr TALKSA
  jmp $f301





;
; JIFFY CHKOUT    ($0320 vector)
;
JChkOut subroutine
  jsr $f3cf                             ;search logical file#
  beq .1	                        ;file not open error
  jmp $f784                             ;err "file not open"

.1
  jsr $f3df                             ;set file param
  lda SY_DN                             ;device#
  cmp #8
  bcs .2
  jmp $f314                             ;std. ChkOut

.2
  tax
  jsr LISTEN
  lda SY_SA
  bpl .3
  jmp $f33a

.3
  jsr LISTENSA
  jmp $f342




;
; JIFFY GETIN    ($032a vector)
;
JGetIn subroutine
  lda $99                               ;device#
  cmp #8
  bcs .2
  jmp $f1f5                             ;std. GetIn



;
; JIFFY BASIN    ($0324 vector)
;
JBasIn
  lda $99                               ;device#
  cmp #8
  bcs .2
  jmp $f20e                             ;std. BasIn

.2
  lda SY_STATUS
  beq .3
  jmp $f268                             ;std. IECIN

.3
  jmp JIF_IECIN




;
; JIFFY BASOUT    ($0326 vector)
;
JBasOut subroutine
  pha
  lda $9a                               ;device#
  cmp #8
  bcs .2
  jmp $f27b                             ;std. BasOut

.2
  pla
  jmp JIF_IECOUT



;
; JIFFY CLRCH / CLRALL    ($0322 / $032c vector)
;
JClrAll subroutine
  lda #0
  sta $98

JClrCh
  ldx #3
  cpx $9a                               ;device# out
  bcs .1
  jsr JIF_UNLISTEN
.1
  cpx $99                               ;device# in
  bcs .2
  jsr JIF_UNTALK
.2
  jmp $f403                             ;std. ClrAll





; ==============================================================
; FIRMWARE FLASHER / 29F040 CODE
; ==============================================================

FLASH_FW_DEST = 4700                    ;$125c

FLASH_FW_LEN = (FLASH_FW_END - FLASH_FW_START)


MOVE_FLASH_FW subroutine
  lda #<(FLASH_FW_DEST + FLASH_FW_LEN)
  sta $58                               ; low-byte new end address +1 $0501+fl len
  lda #>(FLASH_FW_DEST + FLASH_FW_LEN)
  sta $59                               ; high-byte new end address +1 $0501+fl len

  lda #<FLASH_FW_START
  sta $5f                               ; low-byte start address $a000
  lda #>FLASH_FW_START
  sta $60                               ; high-byte start address $a000

  lda #<FLASH_FW_END
  sta $5a                               ; low-byte end address +1 $a???
  lda #>FLASH_FW_END
  sta $5b                               ; high-byte end address +1 $a???

  jmp SY_MOVEMEM                        ; execute move memory vic routine and return


;FLASH_FW subroutine
;  jsr MOVE_FLASH_FW
;  jmp FLASHER


; ==============================================================
; FLASH CODE / 29F040 CODE
; ==============================================================

FLASH_FW_START
  RORG FLASH_FW_DEST

FLASHER subroutine
  sta LOADSTART
  sty LOADSTART +1

  ;sta $a000                             ; UNLOCK IO
  lda #FEMOD_FLASH                      ; PROG MODE
  sta IO_FINAL

  jsr TestEE
  bcs .rts

  jsr BLANK_CHECK
  beq .05

  jsr FLASH_ERASE
  bcs .err                              ; ERROR -->

  jsr BLANK_CHECK_FULL
  bcc .05

.err
  jsr FLASH_CROUT
  lda #<MSG_ERROR
  ldy #>MSG_ERROR
  jsr SY_STROUT
  sec
  bcs .exit

.05
  jsr FLASH_FIRMWARE

.exit
  php
  jsr FlashCodeEndSequ                  ; RESET

  lda #FEMOD_ROM                        ; EEP MODE
  sta IO_FINAL
  plp
  bcs .errexit
  jmp SOFT_RESET

.errexit
  jmp BASIC_WARM

.rts
  rts


; ==============================================================
; 29F040 SUBS
; ==============================================================

_flashBase    = $2000
_flash555     = _flashBase + $555
_flash2aa     = _flashBase + $2aa

FLASH_ALG_ERROR_BIT    = $20
FLASH_ALG_RUNNING_BIT  = $08


_flashCodeMagic subroutine
  lda #$aa
  sta _flash555
  lda #$55
  sta _flash2aa
  rts

FlashCodeEndSequ subroutine             ; RESET
_flashCodeEndSequ
  lda #$f0
  sta _flashBase
  rts

_flashCodeSectorErase subroutine
  jsr _flashCodeMagic
  lda #$80
  sta _flash555
  jsr _flashCodeMagic
  lda #$30
  sta _flashBase
  rts

_flashCodeChipErase subroutine
  jsr _flashCodeMagic
  lda #$80
  sta _flash555
  jsr _flashCodeMagic
  lda #$10
  sta _flash555
  rts

;_flashCodeWrite subroutine
;  jsr _flashCodeWritePrep
;  sta (SAVESTART),y
;  rts


_flashCodeWritePrep subroutine
  pha
  jsr _flashCodeMagic
  lda #$A0
  sta _flash555
  pla
  rts


_flashCodeCheckProgress subroutine      ; TOGGLE CHECK
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
FlashCodeVendorID subroutine
  jsr _flashCodeMagic
  lda #$90
  sta _flash555
  ldx _flashBase
  ldy _flashBase+1
  jmp _flashCodeEndSequ

;=============== ERASE SECTOR
FlashCodeSectorErase subroutine
  jsr _flashCodeSectorErase
  jmp _flashCodeCheckProgress

;=============== FLASH BYTE    AC ==> (SAVESTART)
FlashCodeWrite subroutine
  jsr _flashCodeWritePrep
  sta (SAVESTART),y
  jmp _flashCodeCheckProgress

;=============== SET BANK AND FLASH BYTE    AC ==> (SAVESTART)  C=1:error
FlashCodeWriteXP subroutine
  pha
__flash_BANK = . +1
  lda #FEMOD_FLASH
  sta IO_FINAL
  pla
  jsr FlashCodeWrite
  bcs .exit
  cmp (SAVESTART),y
  beq .clc
  sec
  bcs .exit
.clc
  clc
.exit
; lda #FEMOD_RAM +$10                   ;RAM MODUS, BLK-5 PROTECTED
  lda #FEMOD_ROM                        ;ROM MODUS
  sta IO_FINAL
  rts


  ;-----------------------
FlashCodeWriteXpInc subroutine
  inc SAVESTART
  bne .exit

  lda SAVESTART +1
  jsr RAMD_INC_PTR
  sta SAVESTART +1
  bcc .exit

  inc __flash_BANK
.exit
  rts



;------------
MSG_VENDOR
  dc.b RVSON,13,"VENDOR:",RVSOFF,0
MSG_DEVICEID
  dc.b RVSON,"01  DEVICE:",RVSOFF,0
MSG_A4
  dc.b "A4",13,0

MSG_EEE
  dc.b "??",13,"BAD EEPROM",0






; ==============================================================
; OPEN FIRMWARE FILE AND FLASH
; ==============================================================

MSG_ERRFLSH
  dc.b "FLASH "
MSG_ERROR
  dc.b "ERROR",13,0

MSG_FLASH
  dc.b "FLASHING ...",13,0
MSG_ERASE
  dc.b "ERASING ...",13,0
MSG_BLANK
  dc.b "BLANK CHECK ...",0




; ==============================================================
; TEST FOR RIGHT EEPROM TYPE
; ==============================================================

TestEE subroutine
  lda #<MSG_VENDOR
  ldy #>MSG_VENDOR
  jsr SY_STROUT

  jsr FlashCodeVendorID
  tya
  pha
  cpx #$01                              ; AMD
  beq .VENDOROK
  cpx #$c2                              ; AMD by MX
  bne .ERR0

.VENDOROK
  lda #<MSG_DEVICEID
  ldy #>MSG_DEVICEID
  jsr SY_STROUT
  pla
  tax
;     ldx #$a4
  cpx #$a4                                                    ; 29F040
  bne .ERR1
  lda #<MSG_A4
  ldy #>MSG_A4
  jsr SY_STROUT

  clc
  rts

.ERR0
  pla
.ERR1
  lda #<MSG_EEE
  ldy #>MSG_EEE
  jsr SY_STROUT
  sec
  rts






; ==============================================================
; OPEN FIRMWARE FILE AND FLASH
; ==============================================================


FLASH_FIRMWARE subroutine
  lda #<MSG_FLASH
  ldy #>MSG_FLASH
  jsr SY_STROUT

  jsr SET_LOADPTR
  jsr SET_SAVEPTR
  jsr FLASH_FIRMWARE_2
  bcs .rts
  jsr SET_SAVEPTR_2
  ;ldy #0
FLASH_FIRMWARE_2
.02
  lda #FEMOD_RAM_1                      ; RAM MODE
  sta IO_FINAL
  lda (LOADPTR),y

  pha
  lda #FEMOD_FLASH                      ; PROG MODE
  sta IO_FINAL
  pla

  jsr FlashCodeWrite
  bcs .err2

  cmp (SAVESTART),y
  bne .err2

  iny
  bne .02
  inc SAVESTART +1
  inc LOADPTR +1
  dex
  bne .02
  clc
.rts
  rts


;.err
;  pla
;  pla
.err2
  lda #<MSG_ERRFLSH
  ldy #>MSG_ERRFLSH
  jsr SY_STROUT
  sec
  rts



  ;ERASE CURRENT SECTOR
FLASH_ERASE
  lda #<MSG_ERASE
  ldy #>MSG_ERASE
  jsr SY_STROUT
  jsr FlashCodeSectorErase
  bcs .err2
  jsr FlashCodeEndSequ                  ; RESET
  clc
  rts


  ;BLANK CHECK BANK 1 to 15
BLANK_CHECK_FULL subroutine
  lda #FEMOD_FLASH                      ; EEP MODE
.2
  clc
  adc #2
  sta IO_FINAL
  and #$10
  bne .e                                ;BANK 16? -->

  jsr BLANK_CHECK
  beq .3

  jsr FLASH_ERASE
  bcc .4
  rts

.3
  lda #145                              ;CURSOR UP
  jsr BSOUT
.4
  lda IO_FINAL
  bne .2
.e
  lda #FEMOD_ROM                        ; EEP MODE
  sta IO_FINAL
  jsr FLASH_CROUT
  clc
;.rts
  rts



  ;BLANK CHECK BANK 0
BLANK_CHECK subroutine
  lda #<MSG_BLANK
  ldy #>MSG_BLANK
  jsr P_BANK

  jsr SET_SAVEPTR_1
  jsr .BLANK_CHECK
  bne .exit

  jsr SET_SAVEPTR_2
.BLANK_CHECK
  lda #$ff
.02
  cmp (SAVESTART),y
  bne .exit
  iny
  bne .02
  inc SAVESTART +1
  dex
  bne .02
.exit
  rts



SET_LOADPTR subroutine
  lda LOADSTART
  sta LOADPTR
  lda LOADSTART +1
  sta LOADPTR +1
  rts

SET_SAVEPTR subroutine
  lda #$70                                ;FIRMWARE START ADDRESS
  ldx #16                                 ;BLOCK COUNT
  bne SET_SAVEPTR_E

SET_SAVEPTR_1 subroutine
  lda #$20                                ;DEFAULT LOW CARTRIDGE ADDRESS
  ldx #96                                 ;BLOCK COUNT
  bne SET_SAVEPTR_E

SET_SAVEPTR_2 subroutine
  lda #$A0                                ;DEFAULT HI CARTRIDGE ADDRESS
  ldx #32                                 ;BLOCK COUNT
SET_SAVEPTR_E
  sta SAVESTART +1
  lda #$00                                ;DEFAULT CARTRIDGE ADDRESS
  sta SAVESTART
  ldy #0
  rts


P_BANK subroutine
  jsr SY_STROUT
  lda IO_FINAL
  jsr FLASH_HEXOUT
;  lda #157                                ;CURSOR LEFT
;  jmp BSOUT
FLASH_CROUT subroutine
  lda #13
  jmp BSOUT


FLASH_HEXOUT subroutine
  and #15
.1
  clc
  adc #246
  bcc .12
  adc #6
.12
  adc #58
  jmp BSOUT



  REND
FLASH_FW_END




; ==============================================================
; DISPLAY STRING    in AC/YR
; ==============================================================

STROUT subroutine
  sta PT1
  sty PT1 +1
  ldy #0
  ;sty 658                              ;Scroll Flag
  ;dey
.1
  lda (PT1),y
  beq .E
;_relo0140 = . +1
  jsr CHROUT
  iny
  bne .1
.E
  rts



; ==============================================================
; DISPLAY STRING AT POS   in AC/YR
; ==============================================================

STR_AT
  sta PT1
  sty PT1 +1
  ldy #0
  lda (PT1),y
  tax
  iny
  lda (PT1),y
  tay
;_relo0143 = . +1
  jsr SET_CURSOR
  ldy #2
  bne .1


; ==============================================================
; DELETE LINE      XR=Row
; ==============================================================

DEL_LINE
  ldy #0
;_relo0144 = . +1
  jsr SET_CURSOR
  ldy #21
  lda #32
DELI_2
  sta (C_LINE),y
  dey
  bpl DELI_2
  rts


; ==============================================================
; SET CURSO POS          XR=Row, YR=Col
; ==============================================================

SET_CURSOR
  clc
  jmp CURSOR_POS



; ==============================================================
; STACK PARAM PROCS
; ==============================================================

  ; PRINT STRING PARAMETER AT RETURN ADDRESS
SPAR_PRINTSTRING
  jsr SPAR_GETPTR
SPAR_PRINTSTRING_2
  jsr STRGET_2
  jmp STROUT


  ; PRINT STRING PARAMETER AT RETURN ADDRESS
SPAR_PRINTSTRING_AT
  jsr SPAR_GETPTR
SPAR_PRAT
  ldy #2
  jsr STRGET_2
  jmp STR_AT


  ; PRINT MESSAGE, WAIT KEY
SPAR_MSGBOX
  jsr SPAR_GETPTR
  jsr SPAR_PRAT
  jsr WAIT_KEY
SPAR_MSGCLR
  ldy #0
  lda (PTR_FNAM),y
  tax
  jmp DEL_LINE


  ; GET STRING
SPAR_GETSTRING subroutine
  jsr SPAR_GETPTR
  jmp STRGET_2


  ; SEND DISK COMMAND
SPAR_DISKCMD subroutine
  jsr SPAR_GETPTR
  jsr STRGET_2
  jmp DISK_CMD_2


  ; GET POINTER TO PARAMETER AT RETURN ADDRESS
SPAR_GETPTR subroutine
  tsx
  lda STACK +3,x
  clc
  adc #1
  sta PTR_FNAM
  lda STACK +4,x
  adc #0
  sta PTR_FNAM+1
  ldy #0
  rts


  ; ADD OFFSET +1 TO RETURN ADDRESS
STRGET
  ldy #0
STRGET_2
  jsr STRLEN_2
  iny
MEMGET
  tya
MEMGET_2
  ;dex
  ;dex
  jsr SPAR_ADD
  lda PTR_FNAM
  ldy PTR_FNAM +1
  rts


  ; ADD OFFSET +1 TO RETURN ADDRESS
SPAR_ADD
  clc
  adc STACK +3,x
  sta STACK +3,x
  bcc SPAD_0
  inc STACK +4,x
SPAD_0
  rts

  ; GET STRING LEN IN YR
STRLEN
  ldy #0
STRLEN_2
STLE_0
  lda (PTR_FNAM),y
  beq STLE_E
  iny
  bne STLE_0
STLE_E
  sty LEN_FNAM
  rts


  ; PRINT STRING IN PTR_FNAM
STRNOUT
  ldy #0
  ldx LEN_FNAM
STNO_0
  lda (PTR_FNAM),y
  beq STNO_E
  jsr CHROUT
  iny
  dex
  bne STNO_0
STNO_E
  rts











  org $bfff
  dc.b #$00
