; Functions relating to the timer interrupt and the real-time-clock.

AskTimer::
	push af
	ldh a, [hMobile]
	and a
	jr z, .not_mobile
	call Timer

.not_mobile
	pop af
	reti

LatchClock::
; latch clock counter data
	ld a, LATCH_RTC
	ld [TPP1LatchClock], a

; waste some time to allow clock to latch properly
	nop
	nop
	nop

	ret

UpdateTime::
	call GetClock
	call FixDays
	call FixTime
	farcall GetTimeOfDay
	ret

GetClock::
; store clock data in hRTCDayHi-hRTCSeconds
	call LatchClock

; enable clock r/w
	ld a, RTC_REG_ENABLE
	ld [TPP1SRamEnable], a

	ld de, TPP1RTC

	ld a, [de] ; weeks
	maskbits 100
	cp 50
	jr c, .got_weeks
	sub 50
.got_weeks
	push bc
	ld c, a
	ld b, 0
	ld hl, 0
	ld a, 7
	call AddNTimes
	pop bc
	inc de

	ld a, [de] ; day / hour
	and RTC_D_MASK ; day
	swap a
	srl a
	add l
	ldh [hRTCDayLo], a
	ld a, h
	jr nc, .set_day_hi
	inc h
.set_day_hi
	ldh [hRTCDayHi], a

	ld a, [de] ; day / hour
	and RTC_H_MASK ; hour
	ldh [hRTCHours], a
	inc de

	ld a, [de] ; minutes
	maskbits 60
	ldh [hRTCMinutes], a
	inc de

	ld a, [de] ; seconds
	maskbits 60
	ldh [hRTCSeconds], a

; unlatch clock / disable clock r/w
	jp CloseSRAM

FixDays::
; fix day count
; mod by 140

; check if day count > 255 (bit 8 set)
	ldh a, [hRTCDayHi] ; DH
	bit 0, a
	jr z, .daylo
; reset dh (bit 8)
	res 0, a
	ldh [hRTCDayHi], a ; DH

; mod 140
; mod twice since bit 8 (DH) was set
	ldh a, [hRTCDayLo] ; DL
.modh
	sub 140
	jr nc, .modh
.modl
	sub 140
	jr nc, .modl
	add 140

; update dl
	ldh [hRTCDayLo], a ; DL

; flag for sRTCStatusFlags
	ld a, %01000000
	jr .set

.daylo
; quit if fewer than 140 days have passed
	ldh a, [hRTCDayLo] ; DL
	cp 140
	jr c, .quit

; mod 140
.mod
	sub 140
	jr nc, .mod
	add 140

; update dl
	ldh [hRTCDayLo], a ; DL

; flag for sRTCStatusFlags
	ld a, %00100000

.set
; update clock with modded day value
	push af
	call SetClock
	pop af
	scf
	ret

.quit
	xor a
	ret

FixTime::
; add ingame time (set at newgame) to current time
;				  day     hr    min    sec
; store time in wCurDay, hHours, hMinutes, hSeconds

; second
	ldh a, [hRTCSeconds] ; S
	ld c, a
	ld a, [wStartSecond]
	add c
	sub 60
	jr nc, .updatesec
	add 60
.updatesec
	ldh [hSeconds], a

; minute
	ccf ; carry is set, so turn it off
	ldh a, [hRTCMinutes] ; M
	ld c, a
	ld a, [wStartMinute]
	adc c
	sub 60
	jr nc, .updatemin
	add 60
.updatemin
	ldh [hMinutes], a

; hour
	ccf ; carry is set, so turn it off
	ldh a, [hRTCHours] ; H
	ld c, a
	ld a, [wStartHour]
	adc c
	sub 24
	jr nc, .updatehr
	add 24
.updatehr
	ldh [hHours], a

; day
	ccf ; carry is set, so turn it off
	ldh a, [hRTCDayLo] ; DL
	ld c, a
	ld a, [wStartDay]
	adc c
	ld [wCurDay], a
	ret

InitTimeOfDay::
	xor a
	ld [wStringBuffer2], a
	ld a, $0 ; useless
	ld [wStringBuffer2 + 3], a
	jr InitTime

InitDayOfWeek::
	call UpdateTime
	ldh a, [hHours]
	ld [wStringBuffer2 + 1], a
	ldh a, [hMinutes]
	ld [wStringBuffer2 + 2], a
	ldh a, [hSeconds]
	ld [wStringBuffer2 + 3], a
	jr InitTime ; useless

InitTime::
	farcall _InitTime
	ret

PanicResetClock::
	xor a
	ldh [hRTCSeconds], a
	ldh [hRTCMinutes], a
	ldh [hRTCHours], a
	ldh [hRTCDayLo], a
	ldh [hRTCDayHi], a
; fallthrough

SetClock::
; set clock data from hram
	call LatchClock

; enable clock r/w
	ld a, RTC_REG_ENABLE
	ld [TPP1SRamEnable], a

	ld de, TPP1RTC

	; hRTCDayHi is always 0 when calling SetClock
	ldh a, [hRTCDayLo]
	ld c, 7
	call SimpleDivide
	push af
	ld a, b
	ld [de], a
	inc de

	pop af
	add a
	swap a
	ld b, a
	ldh a, [hRTCHours]
	or b
	ld [de], a
	inc de

	ldh a, [hRTCMinutes]
	ld [de], a
	inc de

	ldh a, [hRTCSeconds]
	ld [de], a

	ld a, SET_RTC
	ld [TPP1LatchClock], a

; cleanup
	jp CloseSRAM ; unlatch clock, disable clock r/w

ClearRTCStatus::
; clear sRTCStatusFlags
	xor a
	push af
	ld a, BANK(sRTCStatusFlags)
	call GetSRAMBank
	pop af
	ld [sRTCStatusFlags], a
	jp CloseSRAM

RecordRTCStatus::
; append flags to sRTCStatusFlags
	ld hl, sRTCStatusFlags
	push af
	ld a, BANK(sRTCStatusFlags)
	call GetSRAMBank
	pop af
	or [hl]
	ld [hl], a
	jp CloseSRAM

CheckRTCStatus::
; check sRTCStatusFlags
	ld a, BANK(sRTCStatusFlags)
	call GetSRAMBank
	ld a, [sRTCStatusFlags]
	jp CloseSRAM
