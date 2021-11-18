; SJLOAD
; ------
; C64 floppy speeder and disk utility based on VDOS by Edward Carroll 1986
; modified for Jiffy compatibility by 1570 in 2008
;

#processor 6502
#seg code



START_ADR  = $0400



  org START_ADR -2

  .byte <(START_ADR),>(START_ADR)




MY_WEDGE_LO = $0500


F_IO     = 1000                         ;IO FLAG
F_WE     = 1001                         ;WEDGE FLAG
F_CURDEV = 1002                         ;WEDGE FLAG



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

LOADPTR = $c3
LOADEND = $ae

KEYANZ  = $c6


IECSTAT  = $90

LEN_FNAM = $b7
PTR_FNAM = $bb

SY_VERIFY   = $93
SY_STATUS   = $90
SY_SA       = $b9
SY_DN       = $ba
SY_FN       = $b8



CHRGET  = $0073                      ;GET NEXT CHAR
CHRGOT  = $0079                      ;GET LAST CHAR

BIP     = $0200                      ;BASIC Input Buffer 88 Bytes
CAS_BUF = 828                        ;Kassetten Buffer



PTR_LOAD        = $0330



;-------------------- WEDGE INIT
MY_WEDGE_INIT
_relo5000 = . +1
  lda #<MY_LOAD
  ldx #>MY_LOAD
  sta PTR_LOAD
  stx PTR_LOAD +1
  ;lda #8
  ;sta F_CURDEV
  rts





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
  LDA lFCCE,X
  PHA
  TXA
  LSR
  LSR
  TAX
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
  LDA lFBBA,X
  ORA $9C
  NOP
  STA $912C
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

lFCCE
  .byte $00,$02,$20,$22,$00,$02,$20,$22,$00,$02,$20,$22,$00,$02,$20,$22

lFBBA
  .byte $00,$00,$20,$20,$00,$00,$20,$20,$02,$02,$22,$22,$02,$02,$22,$22

lF39E
  .byte $00,$20,$00,$20,$02,$22,$02,$22,$00,$20,$00,$20,$02,$22,$02,$22
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
  JSR lEE40
  jmp $eed3
;--------------JIFFY TALK SA


;--------------JIFFY LISTEN SA
JIF_LISTENSA
  STA $95
  JSR lEE40
  jmp $eec5
;--------------JIFFY LISTEN SA



; ==============================================================
; SYS PROCS
; ==============================================================

FRMNUM   = $cd8a	                ; GET NUMERIC VALUE
FRMBYTE  = $d79e	                ; GET BYTE VALUE TO X
CNVWORD  = $d7f7	                ; CONVERT TO WORD VALUE INTO Y/A; $14 (PT3)

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


UNLISTEN = JIF_UNLISTEN	                ; send UNLISTEN command
UNTALK   = JIF_UNTALK	                ; send UNTALK command
LISTEN	 = JIF_LISTEN                   ; send LISTEN command
TALK     = JIF_TALK	                ; send TALK command
IECIN    = JIF_IECIN	                ; get char from IEC
IECOUT   = JIF_IECOUT	                ; send char to IEC


LISTENSA = JIF_LISTENSA                 ; send SA for LISTEN command
TALKSA   = JIF_TALKSA	                ; send SA for TALK command


  ; LOAD VECTOR                         :: "fnam",PA,SA[,loadadr]
MY_LOAD
  ldx SY_DN                             ; PA (device#)
  cpx #4
  bcs MYLO_0
  jmp $f549                             ; OLD LOAD PROC


MY_IECVERIFY
  lda #1
  bne MYLO_0

MY_IECLOAD
  lda #0
MYLO_0
  sta SY_VERIFY
  lda #0
  sta SY_FN                             ; file# - flag for first byte

  lda #$f0                              ; channel
_relo0080 = . +1
  jsr DISK_LISTEN
_relo0081 = . +1
  jsr IECNAMOUT

  lda #$60
_relo0082 = . +1
  jsr DISK_TALK

  ldx SY_SA                             ; SA
  beq MYLO_00                           ; SA=0: LOAD AT $c3

  dex
  dex
  beq MYLO_02


MYLO_01                                 ; SA=1: LOAD AT FILE-ADR
_relo0083 = . +1
  jsr MY_IECIN
  bcs MYLO_ERR

  tax                                   ; load address lo
  jsr IECIN                             ; load address hi
  clv
  bvc MYLO_2


MYLO_00                                 ; SA=0: LOAD AT $c3
_relo0084 = . +1
  jsr MY_IECIN
  bcs MYLO_ERR

  jsr IECIN                             ; skip load address

MYLO_02                                 ; SA=2: LOAD CARTRIDGE AT $c3
_relo0085 = . +1
  jsr FRMWORD2                          ; GET WORD VALUE
  lda LOADPTR +1
  ldx LOADPTR
  bcs MYLO_2

  lda PT3 +1
  ldx PT3

MYLO_2
  sta LOADEND +1
  stx LOADEND

_relo0086 = . +1
  jsr PRINT_ATADR

_relo0087 = . +1
  jsr MY_IECIN
  bcc MYLO_6

MYLO_ERR
  jsr UNTALK
_relo0088 = . +1
  jsr DISK_CLOSE
  jmp $f787


MYLO_5
_relo0089 = . +1
  jsr MY_IECIN2
MYLO_6
  ldy SY_VERIFY
  beq MYLO_7                            ; --> load

  ldy #0
  cmp (LOADEND),y
  beq MYLO_8

  lda #$10
  jsr SETSTAT
  .byte $2c
  
MYLO_7
  sta (LOADEND),y
MYLO_8
  inc LOADEND
  bne MYLO_7a
  inc LOADEND +1
MYLO_7a
  bit SY_STATUS
  bvc MYLO_5                            ; EOI?

MYLO_E
  jsr UNTALK
_relo0090 = . +1
  jsr DISK_CLOSE

_relo0091 = . +1
  jsr PRINT_TOADR

  clc
  ldx LOADEND
  ldy LOADEND +1
  rts




  ; GET BYTE FROM IEC, CHECK STATUS
MY_IECIN2
  jsr IECIN
  pha
  lda SY_STATUS
  lsr
  lsr
  bcc MYIE_4                            ; timeout --> no

  lda SY_FN                             ; file# - flag for first byte
  beq MYIE_4                            ; first byte --> error

  pla
  lda #$fd
  and SY_STATUS
  sta SY_STATUS
  jsr CHKSTOP
  bne MY_IECIN2
  sec
  rts

MY_IECIN
_relo0092 = . +1
  jsr MY_IECIN2
  pha
  lda #1
  sta SY_FN
MYIE_4
  pla
  rts



IECNAMOUT
  lda IECSTAT
  bmi DICM_ERR

  ldx LEN_FNAM
  beq DICM_ERR
  ldy #0
DICM_2
  lda (PTR_FNAM),y
  jsr IECOUT
  iny
  dex
  bne DICM_2
DICM_ERR
  jmp UNLISTEN


DISK_LISTEN
  pha
  lda #0
  sta IECSTAT

  lda SY_DN                             ; device#
  jsr LISTEN
  pla
  jmp LISTENSA


DISK_TALK
  pha
  lda #0
  sta IECSTAT

  lda SY_DN                             ; device#
  jsr TALK
  pla
  jmp TALKSA


DISK_CLOSE
  lda #$e0
_relo0101 = . +1
  jsr DISK_LISTEN
  jmp UNLISTEN




  ; GET WORD VALUE IN Y/A AND (PT3)
FRMWORD2
_relo0160 = . +1
  jsr CHKCOM
  bcs FRWO_3
FRMWORD
  jsr FRMNUM
  jsr CNVWORD
  clc
FRWO_3
  rts


CHKCOM
  jsr CHRGOT
  cmp #$2c                                ; ","
  sec
  bne CHCO_3
  jsr CHRGET
  clc
CHCO_3
  rts




  ; PRINT LOAD AT ADDRESS
PRINT_ATADR
_relo5090 = . +1
  lda #<MSG_LOADAT
  ldy #>MSG_LOADAT

PRAT_1
_relo0096 = . +1
  jsr STROUT
  ldx LOADEND
  lda LOADEND +1
_relo0095 = . +1
  jmp HEXOUT

  ; PRINT LOAD AT ADDRESS
PRINT_TOADR
_relo5091 = . +1
  lda #<MSG_LOADTO
  ldy #>MSG_LOADTO
_relo0093 = . +1
  jsr PRAT_1
_relo0094 = . +1
  jmp CROUT




;------------ PRINT HEX VALUE IN  X/A
HEXOUT
  pha
  lda #"$"
  jsr BSOUT
  pla
  beq HEX0
_relo0200 = . +1
  jsr HEX2
HEX0
  txa
HEX2
  pha
  lsr
  lsr
  lsr
  lsr
_relo0201 = . +1
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
; DISPLAY STRING    in AC/YR
; ==============================================================

BSOUT      = $ffd2
GETIN      = $ffe4


STROUT
  sta PT1
  sty PT1 +1
  ldy #0
  ;sty 658                         ;Scroll Flag
  ;dey
STOU_1
  lda (PT1),y
  beq STOU_E
_relo0140 = . +1
  jsr BSOUT
  iny
  bne STOU_1
STOU_E
  rts


CROUT
  lda #13
  jmp BSOUT


; ==============================================================
; MESSAGE TEXTE
; ==============================================================

MSG_LOADAT
  dc.b " FROM ",0
MSG_LOADTO
  dc.b " TO ",0
