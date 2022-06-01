; example SNES code - mode 2

.p816
.smart

.include "regs.asm"
.include "variables.asm"
.include "macros.asm"
.include "init.asm"






.segment "CODE"

; enters here in forced blank
Main:
.a16 ; the setting from init code
.i16
	phk
	plb
	

	
; DMA from BG_Palette to CGRAM
	A8
	stz CGADD ; $2121 cgram address = zero
	
	stz $4300 ; transfer mode 0 = 1 register write once
	lda #$22  ; $2122
	sta $4301 ; destination, cgram data
	ldx #.loword(BG_Palette)
	stx $4302 ; source
	lda #^BG_Palette
	sta $4304 ; bank
	ldx #256
	stx $4305 ; length
	lda #1
	sta MDMAEN ; $420b start dma, channel 0
	
	
	
	
	

; DMA from Tiles to VRAM	
	lda #V_INC_1 ; the value $80
	sta VMAIN  ; $2115 = set the increment mode +1
	ldx #$0000
	stx VMADDL ; $2116 set an address in the vram of $0000
	
	lda #1
	sta $4300 ; transfer mode, 2 registers 1 write
			  ; $2118 and $2119 are a pair Low/High
	lda #$18  ; $2118
	sta $4301 ; destination, vram data
	ldx #.loword(Tiles)
	stx $4302 ; source
	lda #^Tiles
	sta $4304 ; bank
	ldx #(End_Tiles-Tiles)
	stx $4305 ; length
	lda #1
	sta MDMAEN ; $420b start dma, channel 0
	
	
	
; DMA from Tilemap to VRAM	
	ldx #$6000
	stx VMADDL ; $2116 set an address in the vram of $6000
	
	lda #1
	sta $4300 ; transfer mode, 2 registers 1 write
			  ; $2118 and $2119 are a pair Low/High
	lda #$18  ; $2118
	sta $4301 ; destination, vram data
	ldx #.loword(Tilemap1)
	stx $4302 ; source
	lda #^Tilemap1
	sta $4304 ; bank
	ldx #$800
	stx $4305 ; length
	lda #1
	sta MDMAEN ; $420b start dma, channel 0	
	
	
	ldx #$7000
	stx VMADDL ; $2116 set an address in the vram of $7000
	
	lda #1
	sta $4300 ; transfer mode, 2 registers 1 write
			  ; $2118 and $2119 are a pair Low/High
	lda #$18  ; $2118
	sta $4301 ; destination, vram data
	ldx #.loword(Tilemap3)
	stx $4302 ; source
	lda #^Tilemap3
	sta $4304 ; bank
	ldx #$800
	stx $4305 ; length
	lda #1
	sta MDMAEN ; $420b start dma, channel 0	

	
	
	lda #2 ; mode 2
	sta BGMODE ; $2105
	

	
	
	; 210b = tilesets for bg 1 and bg 2
; (210c for bg 3 and bg 4)
; steps of $1000 -321-321... bg2 bg1
	stz BG12NBA ; $210b BG 1 and 2 TILES at VRAM address $0000

;	lda #$22
;	sta BG34NBA ; ?? bg 3 + 4 tiles at $2000
	
	; 2107 map address bg 1, steps of $400... -54321yx
	; y/x = map size... 0,0 = 32x32 tiles
	; $6000 / $100 = $60
	lda #$60 ; bg1 map at VRAM address $6000
	sta BG1SC ; $2107
	
;	lda #$68 ; bg2 map at VRAM address $6800
;	sta BG2SC ; $2108
	
	lda #$70 ; bg3 map at VRAM address $7000
	sta BG3SC ; $2109
	

	lda #BG1_ON	; bg1 on
	sta TM ; $212c main screen
;	sta TS ; $212d sub screen
	
	
	;turn on NMI interrupts and auto-controller reads
	lda #NMI_ON|AUTO_JOY_ON
	sta NMITIMEN ;$4200
	


; a is still 8 bit.

	stz BG3HOFS
	stz BG3HOFS
	stz BG3VOFS
	stz BG3VOFS
	
	lda #FULL_BRIGHT ; $0f = turn the screen on (end forced blank)
	sta INIDISP ; $2100
;	jsr Screen_On


Infinite_Loop:	
	A8
	XY16
	jsr Wait_NMI
;we are now in v-blank	
	
	A8
	lda BG1_H
	sta BG1HOFS
	stz BG1HOFS

;the left most column isn't affected by offset-per-tile
;just use the same value as the right most column
	lda buffer+62 ;BG1_V
	sta BG1VOFS
	stz BG1VOFS

;copy the buffer to the vram	
	ldx #$7020 ; map 3 + $20 = the vertical offsets
	stx VMADDL
	ldy #$0020
	ldx #$0000
	AXY16
@push:
	lda buffer, x
	sta VMDATAL
	inx
	inx
	dey
	bne @push
	
	A8

	jsr Pad_Poll
	
	
	A16
	lda pad1
	and #KEY_B
	beq :+
	A8
	dec b_offset
:
	A16
	lda pad1
	and #KEY_A
	beq :+
	A8
	inc b_offset
:
	A16
	lda pad1
	and #KEY_LEFT
	beq :+
	dec BG1_H
:
	A16
	lda pad1
	and #KEY_RIGHT
	beq :+
	inc BG1_H
:


;copy current mode 2 offset to map 3 buffer
	A8
	lda #0
	xba
	lda b_offset
	lsr a
	lsr a
	lsr a ; /8
	tax
	ldy #0
@copy_loop:
	lda Tables, x
	sta buffer, y
	inx
	iny
	lda #$60 ;see notes below
	sta buffer, y
	iny
	cpy #64
	bcc @copy_loop

	A8
	jmp Infinite_Loop
	
;the bits for mode 2 are
;15 = 0 H, 1 V - for mode 4 only
;14 = BG2
;13 = BG1
;10-12 = xxx
;0-9 = scroll offset
	
	
Screen_Off:
.a8
.i16
	jsr Wait_NMI
	lda #FORCE_BLANK
	sta INIDISP ; $2100
	rts
	
	
Screen_On:
.a8
.i16
	jsr Wait_NMI
	lda #FULL_BRIGHT
	sta INIDISP ; $2100
	rts	
	
	

	
	
	
	
	
	
Wait_NMI:
.a8
.i16
;should work fine regardless of size of A
	lda in_nmi ;load A register with previous in_nmi
@check_again:	
	WAI ;wait for an interrupt
	cmp in_nmi	;compare A to current in_nmi
				;wait for it to change
				;make sure it was an nmi interrupt
	beq @check_again
	rts
	
	
	
Pad_Poll:
.a8
.i16
; reads both controllers to pad1, pad1_new, pad2, pad2_new
; auto controller reads done, call this once per main loop
; copies the current controller reads to these variables
; pad1, pad1_new, pad2, pad2_new (all 16 bit)
	php
	A8
@wait:
; wait till auto-controller reads are done
	lda $4212
	lsr a
	bcs @wait
	
	A16
	lda pad1
	sta temp1 ; save last frame
	lda $4218 ; controller 1
	sta pad1
	eor temp1
	and pad1
	sta pad1_new
	
	lda pad2
	sta temp1 ; save last frame
	lda $421a ; controller 2
	sta pad2
	eor temp1
	and pad2
	sta pad2_new
	plp
	rts	
	
	
	
	
	


Tables:
;sine wave
;https://www.daycounter.com/Calculators/Sine-Generator-Calculator.phtml
.byte $0,$1,$3,$5,$7,$a,$d,$10,$12,$15,$18,$1a,$1c
.byte $1e,$1f,$1f,$1f,$1e,$1c,$1a,$18,$15,$12,$10,$d
.byte $a,$7,$5,$3,$1,$0,$0
;doubled
.byte $0,$1,$3,$5,$7,$a,$d,$10,$12,$15,$18,$1a,$1c
.byte $1e,$1f,$1f,$1f,$1e,$1c,$1a,$18,$15,$12,$10,$d
.byte $a,$7,$5,$3,$1,$0,$0


.include "header.asm"	


.segment "RODATA1"

BG_Palette:
; 256 bytes
.incbin "M1TE/Greenzone.pal"



Tilemap1:
; $800 bytes
.incbin "M1TE/Greenzone.map"

Tilemap3:
;should be blank



.segment "RODATA2"

Tiles:
; 4bpp tileset
.incbin "M1TE/Greenzone.chr"
End_Tiles:




