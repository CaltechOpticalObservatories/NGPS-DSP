; Miscellaneous CCD control routines, common to all detector types

; test to see if the power is on on the backplane.
POWER_CHECK
CHECK_LVEN
        JCLR    #LVEN,X:HDR,CHECK_HVEN
CHECK_HVEN
        JCLR    #HVEN,X:HDR,PWR_CHECK_OK
        MOVE    #0,Y1   ; send a 1 back if the power is on
        JMP     <FINISH1
        ; jump here is power is ok
PWR_CHECK_OK
        MOVE    #1,Y1   ; send a 1 back if the power is on
        JMP     <FINISH1

POWER_OFF
	JSR	<CLEAR_SWITCHES		; Clear all analog switches
	BSET	#LVEN,X:HDR 
	BSET	#HVEN,X:HDR 
        BCLR    #POWERST,X:<STATUS      ; Set the power state in the X: status word
	JMP	<FINISH

; Execute the power-on cycle, as a command
POWER_ON
	JSR	<CLEAR_SWITCHES		; Clear all analog switches
	JSR	<PON			; Turn on the power control board
	JCLR	#PWROK,X:HDR,PWR_ERR	; Test if the power turned on properly
	JSR	<SET_BIASES		; Turn on the DC bias supplies
	JSR	<SEL_OS			; Set up readout parameters
	MOVE	#IDLE,R0		; Put controller in IDLE state
;	MOVE	#TST_RCV,R0		; Put controller in non-IDLE state
	MOVE	R0,X:<IDL_ADR
  BSET    #POWERST,X:<STATUS      ; Set the power state bit in the X: status word

  ; get the gain setting and put it into the appropriate place in Y memory
  MOVE    Y:GAIN_SETTING,A
  MOVE	#$0D0000,X0
	SUB	X0,A
  NOP
  MOVE    A,Y:<GAIN

; !!!	MOVE	#$1064,X0
; !!!	MOVE	X0,X:<STATUS
; !!!	JSR	<SEL_OS
  BSET    #POWERST,X:<STATUS      ; Set the power state bit in the X: status word
	JMP	<FINISH

; The power failed to turn on because of an error on the power control board
PWR_ERR	BSET	#LVEN,X:HDR		; Turn off the low voltage enable line
	BSET	#HVEN,X:HDR		; Turn off the high voltage enable line
	JMP	<ERROR

; As a subroutine, turn on the low voltages (+/- 6.5V, +/- 16.5V) and delay
PON	BCLR	#LVEN,X:HDR		; Set these signals to DSP outputs 
	MOVE	#2000000,X0
	DO      X0,*+3			; Wait 20 millisec for settling
	NOP 	

; Turn on the high +36 volt power line and then delay
	BCLR	#HVEN,X:HDR		; HVEN = Low => Turn on +36V
	MOVE	#10000000,X0
	DO      X0,*+3			; Wait 100 millisec for settling
	NOP
	RTS


RAW_COMMAND
	BSET	#3,X:PCRD	; Turn on the serial clock
  
        MOVE    X:(R3)+,A       ; Get the command which should just be a word
	JSR	<XMIT_A_WORD	; Transmit A to TIM-A-STD
	JSR	<PAL_DLY	; Wait for the number to be sent
	BCLR    #3,X:PCRD	; Turn off the serial clock
	JMP	<FINISH

; Set all the DC bias voltages and video processor offset values, reading
;   them from the 'DACS' table
SET_BIASES
	BSET	#3,X:PCRD		; Turn on the serial clock
	BCLR	#1,X:<LATCH		; Separate updates of clock driver
	BSET	#CDAC,X:<LATCH		; Disable clearing of DACs
	BSET	#ENCK,X:<LATCH		; Enable clock and DAC output switches
	MOVEP	X:LATCH,Y:WRLATCH	; Write it to the hardware
	JSR	<PAL_DLY		; Delay for all this to happen

; Read DAC values from a table, and write them to the DACs
	MOVE	#DACS,R0		; Get starting address of DAC values
	NOP
	NOP
	DO      Y:(R0)+,L_DAC		; Repeat Y:(R0)+ times
	MOVE	Y:(R0)+,A		; Read the table entry
	JSR	<XMIT_A_WORD		; Transmit it to TIM-A-STD
	NOP
L_DAC

; Let the DAC voltages all ramp up before exiting
	MOVE	#400000,X0
	DO	X0,*+3			; 4 millisec delay
	NOP
	BCLR	#3,X:PCRD		; Turn the serial clock off
	RTS

SET_BIAS_VOLTAGES
	JSR	<SET_BIASES
	JMP	<FINISH

CLR_SWS	JSR	<CLEAR_SWITCHES
	JMP	<FINISH

; Clear all video processor analog switches to lower their power dissipation
CLEAR_SWITCHES
	BSET	#3,X:PCRD	; Turn the serial clock on
	MOVE	#$0C3000,A	; Value of integrate speed and gain switches
	CLR	B
	MOVE	#$100000,X0	; Increment over board numbers for DAC writes
	MOVE	#$001000,X1	; Increment over board numbers for WRSS writes
	DO	#15,L_VIDEO	; Fifteen video processor boards maximum
	JSR	<XMIT_A_WORD	; Transmit A to TIM-A-STD
	ADD	X0,A
	MOVE	B,Y:WRSS
	JSR	<PAL_DLY	; Delay for the serial data transmission
	ADD	X1,B
L_VIDEO	
	BCLR	#CDAC,X:<LATCH		; Enable clearing of DACs
	BCLR	#ENCK,X:<LATCH		; Disable clock and DAC output switches
	MOVEP	X:LATCH,Y:WRLATCH 	; Execute these two operations
	BCLR	#3,X:PCRD		; Turn the serial clock off
	RTS

SET_SHUTTER_STATE
	MOVE	X:LATCH,A
	AND	#$FFEF,A
	OR	X0,A
	NOP
	MOVE	A1,X:LATCH
	MOVEP	A1,Y:WRLATCH	
	RTS	
	
; Open the shutter from the timing board, executed as a command
OPEN_SHUTTER
	BSET    #ST_SHUT,X:<STATUS 	; Set status bit to mean shutter open
	MOVE	#>$10,X0
	JSR	<SET_SHUTTER_STATE
	JMP	<FINISH

; Close the shutter from the timing board, executed as a command
CLOSE_SHUTTER
	BCLR    #ST_SHUT,X:<STATUS 	; Clear status to mean shutter closed
	MOVE	#0,X0
	JSR	<SET_SHUTTER_STATE
	JMP	<FINISH

; Shutter subroutines
OSHUT	BSET    #ST_SHUT,X:<STATUS 	; Set status bit to mean shutter open
	MOVE	#>$10,X0
	JSR	<SET_SHUTTER_STATE
	RTS

CSHUT	BCLR    #ST_SHUT,X:<STATUS 	; Clear status to mean shutter closed
	MOVE	#0,X0
	JSR	<SET_SHUTTER_STATE
	RTS

; Clear the CCD, executed as a command
CLEAR	JSR	<CLR_CCD
	JMP     <FINISH

; Default clearing routine with serial clocks inactive
; Fast clear image before each exposure, executed as a subroutine
CLR_CCD	DO      Y:<NPCLR,LPCLR2	; Loop over number of lines in image
	MOVE    Y:<PARALLEL_CLEAR,R0	; Address of parallel transfer waveform
	CLOCK
	JCLR    #EF,X:HDR,LPCLR1 ; Simple test for fast execution
	MOVE	#COM_BUF,R3
	JSR	<GET_RCV		; Check for FO command
	JCC	<LPCLR1			; Continue no commands received

	MOVE	#LPCLR1,R0
	MOVE	R0,X:<IDL_ADR
	JMP	<PRC_RCV
LPCLR1	NOP
LPCLR2
	MOVE	#COM_BUF,R3
	JSR	<GET_RCV		; Check for FO command
	RTS

;CLR_FS  DO  #NP_FS,FSCLR2
;  MOVE  Y:<CLEAR_FS,R0
;  CLOCK
;  JCLR  #EF,X:HDR,FSCLR1
;  MOVE  #COM_BUF,R3
;  JSR   <GET_RCV
;  JCC   <FSCLR1
;
;  MOVE  #FSCLR1,R0
;  MOVE  R0,X:<IDL_ADR
;  JMP   <PRC_RCV
;FSCLR1  NOP
;FSCLR2
;  MOVE  #COM_BUF,R3
;  JSR   <GET_RCV
;  RTS

; Start the exposure timer and monitor its progress
EXPOSE	JSSET   #ST_SYNC,X:STATUS,SYNCH_CONTROLLER ; Sync up two controllers
	MOVEP	#0,X:TLR0		; Load 0 into counter timer
	MOVE	#0,X0
	MOVE	X0,X:<ELAPSED_TIME	; Set elapsed exposure time to zero
	MOVE	X:<EXPOSURE_TIME,B
	TST	B			; Special test for zero exposure time
	JEQ	<END_EXP		; Don't even start an exposure
	SUB	#1,B			; Timer counts from X:TCPR0+1 to zero
	BSET	#TIM_BIT,X:TCSR0	; Enable the timer #0
	MOVE	B,X:TCPR0
CHK_RCV	JCLR    #EF,X:HDR,CHK_TIM	; Simple test for fast execution
	MOVE	#COM_BUF,R3		; The beginning of the command buffer
	JSR	<GET_RCV		; Check for an incoming command
	JCS	<PRC_RCV		; If command is received, go check it
CHK_TIM	JCLR	#TCF,X:TCSR0,CHK_RCV	; Wait for timer to equal compare value
END_EXP	BCLR	#TIM_BIT,X:TCSR0	; Disable the timer
	JMP	(R7)			; This contains the return address

; Start the exposure, operate the shutter, and initiate the CCD readout
START_EXPOSURE
	MOVE	#$020102,B
	JSR	<XMT_WRD
	MOVE	#'IIA',B		; Initialize the PCI image address
	JSR	<XMT_WRD
	JSCLR	#NOT_CLR,X:STATUS,CLR_CCD ; Jump to clear out routine if bit set 
	MOVE	#COM_BUF,R3		; The beginning of the command buffer
	JSR	<GET_RCV		; Check for FO command
	JCS	<PRC_RCV		; Process the command 
	MOVE	#TST_RCV,R0		; Process commands during the exposure
	MOVE	R0,X:<IDL_ADR
	JSR	<WAIT_TO_FINISH_CLOCKING

; Operate the shutter if needed and begin exposure
	JCLR	#SHUT,X:STATUS,L_SEX0
	JSR	<OSHUT			; Open the shutter if needed
L_SEX0	MOVE	#L_SEX1,R7		; Return address at end of exposure

; delay to ensure controllers are synced
	MOVE	#100000,X0
	DO	#100,SHUTTER_SYNC0		; Delay by Y:SHDEL milliseconds
	DO	X0,SHUTTER_SYNC1
	NOP
SHUTTER_SYNC1 NOP
SHUTTER_SYNC0	NOP

	JMP	<EXPOSE			; Delay for specified exposure time
L_SEX1

; Now we really start the CCD readout, alerting the PCI board, closing the 
;  shutter, waiting for it to close and then reading out
STR_RDC	JSR	<PCI_READ_IMAGE		; Get the PCI board reading the image
	BSET	#ST_RDC,X:<STATUS 	; Set status to reading out
	JCLR	#SHUT,X:STATUS,TST_SYN
	JSR	<CSHUT			; Close the shutter if necessary
TST_SYN	JSET	#TST_IMG,X:STATUS,SYNTHETIC_IMAGE

; Delay readout until the shutter has fully closed
	MOVE	Y:<SHDEL,A
	TST	A
	JLE	<S_DEL0
	MOVE	#100000,X0
	DO	A,S_DEL0		; Delay by Y:SHDEL milliseconds
	DO	X0,S_DEL1
	NOP
S_DEL1	NOP
S_DEL0	NOP

	JMP	<RDCCD			; Finally, go read out the CCD
  
;TEST_READ_AD
;       DO Y:NPR,END_PARALLEL
;       DO Y:NSR,END_SERIAL
;       JSR <READ_AD 
;       NOP
;END_SERIAL    
;        NOP
;END_PARALLEL        

;RDC_ENDT	JCLR	#IDLMODE,X:<STATUS,NO_IDLT ; Don't idle after readout
;	MOVE	#IDLE,R0
;	MOVE	R0,X:<IDL_ADR
;	JMP	<RDC_ET
;NO_IDLT	MOVE	#TST_RCV,R0
;	MOVE	R0,X:<IDL_ADR
;RDC_ET	JSR	<WAIT_TO_FINISH_CLOCKING
;	BCLR	#ST_RDC,X:<STATUS	; Set status to not reading out
;        JMP     <START
        
TEST_AD
  MOVE    X:(RDAD+1),B
	ASL	#16,B,B
	NOP
	MOVE	B2,X1
	ASL	#8,B,B
	NOP
	MOVE	B2,X0
	NOP
	MOVE	X1,Y1
	MOVE	X0,Y0
  MOVE    Y1,Y:<BYTE_1
  MOVE    Y0,Y:<BYTE_2

  JMP     <FINISH

; Set the desired exposure time
SET_EXPOSURE_TIME
	MOVE	X:(R3)+,Y0
	MOVE	Y0,X:EXPOSURE_TIME
	MOVEP	Y0,X:TCPR0
	JMP	<FINISH

; Read the time remaining until the exposure ends
READ_EXPOSURE_TIME
	JSET	#TIM_BIT,X:TCSR0,RD_TIM	; Read DSP timer if its running
	MOVE	X:<ELAPSED_TIME,Y1
	JMP	<FINISH1
RD_TIM	MOVE	X:TCR0,Y1		; Read elapsed exposure time
	JMP	<FINISH1

; Pause the exposure - close the shutter and stop the timer
PAUSE_EXPOSURE
	MOVEP	X:TCR0,X:ELAPSED_TIME	; Save the elapsed exposure time
	BCLR    #TIM_BIT,X:TCSR0	; Disable the DSP exposure timer
	JSR	<CSHUT			; Close the shutter
	JMP	<FINISH

; Resume the exposure - open the shutter if needed and restart the timer
RESUME_EXPOSURE
	BSET	#TRM,X:TCSR0		; To be sure it will load TLR0
	MOVEP	X:TCR0,X:TLR0		; Restore elapsed exposure time
	BSET	#TIM_BIT,X:TCSR0	; Re-enable the DSP exposure timer
	JCLR	#SHUT,X:STATUS,L_RES
	JSR	<OSHUT			; Open the shutter if necessary
L_RES	JMP	<FINISH

; Abort exposure - close the shutter, stop the timer and resume idle mode
ABORT_EXPOSURE
	JSR	<CSHUT			; Close the shutter
	BCLR    #TIM_BIT,X:TCSR0	; Disable the DSP exposure timer
	JCLR	#IDLMODE,X:<STATUS,NO_IDL2 ; Don't idle after readout
	MOVE	#IDLE,R0
	MOVE	R0,X:<IDL_ADR
	JMP	<RDC_E2
NO_IDL2	MOVE	#TST_RCV,R0
	MOVE	R0,X:<IDL_ADR
RDC_E2	JSR	<WAIT_TO_FINISH_CLOCKING
	BCLR	#ST_RDC,X:<STATUS	; Set status to not reading out
	DO      #4000,*+3		; Wait 40 microsec for the fiber
	NOP				;  optic to clear out 
	JMP	<FINISH
; Generate a synthetic image by simply incrementing the pixel counts
SYNTHETIC_IMAGE
;	JSR	<PCI_READ_IMAGE		; Get the PCI board reading the image
	BSET	#ST_RDC,X:<STATUS 	; Set status to reading out
	JSR	<PAL_DLY
	CLR	A
	DO      Y:<NPR,LPR_TST      	; Loop over each line readout
	DO      Y:<NSR,LSR_TST		; Loop over number of pixels per line
	REP	#20			; #20 => 1.0 microsec per pixel
	NOP
	ADD	#1,A			; Pixel data = Pixel data + 1
	NOP
	MOVE	A,B
	JSR	<XMT_PIX		;  transmit them
	NOP
LSR_TST	
	NOP
LPR_TST	
        JMP     <RDC_END		; Normal exit

; Transmit the 16-bit pixel datum in B1 to the host computer
XMT_PIX	ASL	#16,B,B
	NOP
	MOVE	B2,X1
	ASL	#8,B,B
	NOP
	MOVE	B2,X0
	NOP
	MOVEP	X1,Y:WRFO
	MOVEP	X0,Y:WRFO
	RTS

; Test the hardware to read A/D values directly into the DSP instead
;   of using the SXMIT option, A/Ds #2 and 3.
READ_AD	MOVE	X:(RDAD+2),B
	ASL	#16,B,B
	NOP
	MOVE	B2,X1
	ASL	#8,B,B
	NOP
	MOVE	B2,X0
	NOP
	MOVEP	X1,Y:WRFO
	MOVEP	X0,Y:WRFO
	REP	#10
	NOP
	MOVE	X:(RDAD+3),B
	ASL	#16,B,B
	NOP
	MOVE	B2,X1
	ASL	#8,B,B
	NOP
	MOVE	B2,X0
	NOP
	MOVEP	X1,Y:WRFO
	MOVEP	X0,Y:WRFO
	REP	#10
	NOP
	RTS

; Alert the PCI interface board that images are coming soon
PCI_READ_IMAGE
	MOVE	#$020104,B		; Send header word to the FO xmtr
	JSR	<XMT_WRD
	MOVE	#'RDA',B
	JSR	<XMT_WRD
	MOVE	Y:NSR,B			; Number of columns to read
	JSR	<XMT_WRD
	MOVE	Y:NPR,B			; Number of rows to read
	JSR	<XMT_WRD
	RTS

; Wait for the clocking to be complete before proceeding
WAIT_TO_FINISH_CLOCKING
	JSET	#SSFEF,X:PDRD,*		; Wait for the SS FIFO to be empty
	RTS

; Delay for serial writes to the PALs and DACs by 8 microsec
PAL_DLY	DO	#800,*+3		; Wait 8 usec for serial data xmit
	NOP
	RTS

; Let the host computer read the controller configuration
READ_CONTROLLER_CONFIGURATION
	MOVE	Y:<CONFIG,Y1		; Just transmit the configuration
	JMP	<FINISH1

; Set the video processor gain and integrator speed for all video boards
;					  #SPEED = 0 for slow, 1 for fast
ST_GAIN	BSET	#3,X:PCRD	; Turn on the serial clock
	MOVE	X:(R3)+,A	; Gain value (1,2,5 or 10)
	MOVE	#>1,X0
	CLR     B

	DO #15,CHK_GAIN
	CMP B,A
	JEQ <STG_A
	ADD X0,B
	NOP
CHK_GAIN        
	JMP <ERR_SGN

STG_A	MOVE	A,Y:<GAIN	; Store the GAIN value for later use
	MOVE	#$0D0000,X0
	OR	X0,A
	NOP

; Send this same value to 15 video processor boards whether they exist or not
;	MOVE	#$100000,X0	; Increment value
;	DO	#15,STG_LOOP
; DO NOT LOOP!  You will send out a word which is 0x2D000x where x is the
; gain settings.  This will set one of the reset drain voltages to ~0V and
; so mess up the CCD depletion.  You can't set the HV board address to get
; around this...  If you end up with more than one video card in a box
; then you'll have to manually set this.
	JSR	<XMIT_A_WORD	; Transmit A to TIM-A-STD
	JSR	<PAL_DLY	; Wait for SSI and PAL to be empty
	ADD	X0,A		; Increment the video processor board number
	NOP
STG_LOOP
	BCLR	#3,X:PCRD		; Turn the serial clock off
	JMP	<FINISH

ERR_SGN	MOVE	X:(R3)+,A
	BCLR	#3,X:PCRD		; Turn the serial clock off
	JMP	<ERROR

; Set a particular DAC numbers, for setting DC bias voltages, clock driver  
;   voltages and video processor offset
; This is code for the ARC32 clock driver and ARC45 CCD video processor
;
; SBN  #BOARD  #DAC  ['CLK' or 'VID'] voltage
;
;				#BOARD is from 0 to 15
;				#DAC number
;				#voltage is from 0 to 4095

SET_BIAS_NUMBER			; Set bias number
	BSET	#3,X:PCRD	; Turn on the serial clock
	MOVE	X:(R3)+,A	; First argument is board number, 0 to 15
	REP	#20
	LSL	A
	NOP
	MOVE	A,X1		; Save the board number
	MOVE	X:(R3)+,A	; Second argument is DAC number
	MOVE	X:(R3),B	; Third argument is 'VID' or 'CLK' string
	CMP	#'VID',B
	JNE	<CLK_DRV
	REP	#14
	LSL	A
	NOP
	BSET	#19,A1		; Set bits to mean video processor DAC
	NOP
	BSET	#18,A1
	JMP	<BD_SET
CLK_DRV	CMP	#'CLK',B
	JNE	<ERR_SBN

; For ARC32 do some trickiness to set the chip select and address bits
	MOVE	A1,B
	REP	#14
	LSL	A
	MOVE	#$0E0000,X0
	AND	X0,A
	MOVE	#>7,X0
	AND	X0,B		; Get 3 least significant bits of clock #
	CMP	#0,B
	JNE	<CLK_1
	BSET	#8,A
	JMP	<BD_SET
CLK_1	CMP	#1,B
	JNE	<CLK_2
	BSET	#9,A
	JMP	<BD_SET
CLK_2	CMP	#2,B
	JNE	<CLK_3
	BSET	#10,A
	JMP	<BD_SET
CLK_3	CMP	#3,B
	JNE	<CLK_4
	BSET	#11,A
	JMP	<BD_SET
CLK_4	CMP	#4,B
	JNE	<CLK_5
	BSET	#13,A
	JMP	<BD_SET
CLK_5	CMP	#5,B
	JNE	<CLK_6
	BSET	#14,A
	JMP	<BD_SET
CLK_6	CMP	#6,B
	JNE	<CLK_7
	BSET	#15,A
	JMP	<BD_SET
CLK_7	CMP	#7,B
	JNE	<BD_SET
	BSET	#16,A

BD_SET	OR	X1,A		; Add on the board number
	NOP
	MOVE	A,X0
	MOVE	X:(R3)+,B	; Third argument (again) is 'VID' or 'CLK' string
	CMP	#'VID',B
	JEQ	<VID
	MOVE	X:(R3)+,A	; Fourth argument is voltage value, 0 to $fff
	REP	#4
	LSR	A		; Convert 12 bits to 8 bits for ARC32
	MOVE	#>$FF,Y0	; Mask off just 8 bits
	AND	Y0,A
	OR	X0,A
	JMP	<XMT_SBN
VID	MOVE	X:(R3)+,A	; Fourth argument is voltage value for ARC45, 12 bits
	OR	X0,A

XMT_SBN	JSR	<XMIT_A_WORD	; Transmit A to TIM-A-STD
	JSR	<PAL_DLY	; Wait for the number to be sent
	BCLR	#3,X:PCRD	; Turn the serial clock off
	JMP	<FINISH
ERR_SBN	MOVE	X:(R3)+,A	; Read and discard the fourth argument
	BCLR	#3,X:PCRD	; Turn the serial clock off
	JMP	<ERROR


; Specify the MUX value to be output on the clock driver board
; Command syntax is  SMX  #clock_driver_board #MUX1 #MUX2
;				#clock_driver_board from 0 to 15
;				#MUX1, #MUX2 from 0 to 23

SET_MUX	BSET	#3,X:PCRD	; Turn on the serial clock
	MOVE	X:(R3)+,A	; Clock driver board number
	REP	#20
	LSL	A
	MOVE	#$003000,X0
	OR	X0,A
	NOP
	MOVE	A,X1		; Move here for storage

; Get the first MUX number
	MOVE	X:(R3)+,A	; Get the first MUX number
	JLT	ERR_SM1
	MOVE	#>24,X0		; Check for argument less than 32
	CMP	X0,A
	JGE	ERR_SM1
	MOVE	A,B
	MOVE	#>7,X0
	AND	X0,B
	MOVE	#>$18,X0
	AND	X0,A
	JNE	<SMX_1		; Test for 0 <= MUX number <= 7
	BSET	#3,B1
	JMP	<SMX_A
SMX_1	MOVE	#>$08,X0
	CMP	X0,A		; Test for 8 <= MUX number <= 15
	JNE	<SMX_2
	BSET	#4,B1
	JMP	<SMX_A
SMX_2	MOVE	#>$10,X0
	CMP	X0,A		; Test for 16 <= MUX number <= 23
	JNE	<ERR_SM1
	BSET	#5,B1
SMX_A	OR	X1,B1		; Add prefix to MUX numbers
	NOP
	MOVE	B1,Y1

; Add on the second MUX number
	MOVE	X:(R3)+,A	; Get the next MUX number
	JLT	<ERROR
	MOVE	#>24,X0		; Check for argument less than 32
	CMP	X0,A
	JGE	<ERROR
	REP	#6
	LSL	A
	NOP
	MOVE	A,B
	MOVE	#$1C0,X0
	AND	X0,B
	MOVE	#>$600,X0
	AND	X0,A
	JNE	<SMX_3		; Test for 0 <= MUX number <= 7
	BSET	#9,B1
	JMP	<SMX_B
SMX_3	MOVE	#>$200,X0
	CMP	X0,A		; Test for 8 <= MUX number <= 15
	JNE	<SMX_4
	BSET	#10,B1
	JMP	<SMX_B
SMX_4	MOVE	#>$400,X0
	CMP	X0,A		; Test for 16 <= MUX number <= 23
	JNE	<ERROR
	BSET	#11,B1
SMX_B	ADD	Y1,B		; Add prefix to MUX numbers
	NOP
	MOVE	B1,A
	JSR	<XMIT_A_WORD	; Transmit A to TIM-A-STD
	JSR	<PAL_DLY	; Delay for all this to happen
	BCLR #3,X:PCRD		; Turn off the serial clock
	JMP	<FINISH
ERR_SM1	MOVE	X:(R3)+,A
	BCLR #3,X:PCRD		; Turn off the serial clock
	JMP	<ERROR

; Specify subarray readout coordinates, one rectangle only
SET_SUBARRAY_SIZES
	CLR	A
	NOP	
	MOVE	A,Y:<NBOXES		; Number of subimage boxes = 0 to start
	MOVE    X:(R3)+,X0
	MOVE	X0,Y:<NR_BIAS		; Number of bias pixels to read
	MOVE    X:(R3)+,X0
	MOVE	X0,Y:<NS_READ		; Number of columns in subimage read
	MOVE    X:(R3)+,X0
	MOVE	X0,Y:<NP_READ		; Number of rows in subimage read	
	JMP	<FINISH

; Call this routine once for every subarray to be added to the table
SET_SUBARRAY_POSITIONS
	MOVE	Y:<NBOXES,X0
	MOVE	X:<THREE,X1
	MPY	X0,X1,A
	ASR	A
	MOVE	A0,A1
	MOVE	#>10,X0
	CMP	X0,A
	JGT	<ERROR		; Error if number of boxes > 10
	MOVE	#READ_TABLE,X0
	ADD	X0,A
	NOP
	MOVE	A1,R7
	MOVE	X:(R3)+,X0
	NOP
	NOP
	MOVE	X0,Y:(R7)+	; Number of rows (parallels) to clear
	MOVE	X:(R3)+,X0
	MOVE	X0,Y:(R7)+	; Number of columns (serials) clears before
	MOVE	X:(R3)+,X0	;  the box readout
	MOVE	X0,Y:(R7)+	; Number of columns (serials) clears after	
	MOVE	Y:<NBOXES,A	;  the box readout
	MOVE	X:<ONE,X0
	ADD	X0,A
	NOP
	MOVE	A,Y:<NBOXES
	JMP	<FINISH

; Generate the serial readout waveform table for the chosen
;   value of readout and serial binning.

GENERATE_SERIAL_WAVEFORM
	MOVE	#CCD_RESET,R0
	MOVE	#(PXL_TBL+1),R1
	MOVE	Y:(R0)+,A
	NOP	
	DO	A1,L_CCD_RESET
	MOVE	Y:(R0)+,X0
	MOVE	X0,Y:(R1)+
L_CCD_RESET

; Generate the first set of clocks
	MOVE	Y:FIRST_CLOCKS,R0
	NOP
	NOP
	MOVE	Y:(R0)+,A
	NOP
	DO	A1,L_FIRST_CLOCKS
	MOVE	Y:(R0)+,X0
	MOVE	X0,Y:(R1)+
L_FIRST_CLOCKS

	
; Generate the binning waveforms if needed
	MOVE	Y:<NSBIN,A
	SUB	#1,A
	JLE	<GEN_VID		
	DO	A1,L_BIN
	MOVE	Y:CLOCK_LINE,R0
	NOP
	NOP
	MOVE	Y:(R0)+,A
	NOP
	DO	A1,L_CLOCK_LINE
	MOVE	Y:(R0)+,X0
	MOVE	X0,Y:(R1)+
L_CLOCK_LINE
	NOP
L_BIN

; Generate the video processor waveforms
GEN_VID	MOVE	#VIDEO_PROCESS,R0
	NOP
	NOP
	MOVE	Y:(R0)+,A
	NOP
	DO	A1,L_VID
	MOVE	Y:(R0)+,X0
	MOVE	X0,Y:(R1)+
L_VID

; Finally, calculate the number of entries in the waveform table just generated
	MOVE	#PXL_TBL,X0
	MOVE	X0,R0
	MOVE	R1,A
	SUB	X0,A
	SUB	#1,A
	NOP
	MOVE	A1,Y:(R0)
	RTS

; Select which readouts to process
;   'SOS'  Amplifier_name  
;	Amplifier_name = '__L', '__R', or '_LR'

SELECT_OUTPUT_SOURCE
	MOVE    X:(R3)+,Y0
	MOVE	Y0,Y:<OS
	JSR	<SEL_OS
	JMP	<FINISH
	
SEL_OS	MOVE	Y:<OS,Y0
        MOVE    #'_U1',A
        CMP     Y0,A
        JNE     <COMP_U2

        MOVE    #$F0C3,X0
        MOVE    X0,Y:SXL
        MOVE    #PARALLEL_1,X0
        MOVE    X0,Y:PARALLEL
        MOVE    #SERIAL_READ_LEFT,X0
        MOVE    X0,Y:SERIAL_READ
	MOVE	#SERIAL_SKIP_LEFT,X0
	MOVE	X0,Y:SERIAL_SKIP
        MOVE    #SERIAL_IDLE_LEFT,X0
        MOVE    X0,Y:SERIAL_IDLE
        MOVE    #PARALLEL_CLEAR_1,X0
        MOVE    X0,Y:PARALLEL_CLEAR
	BCLR    #SPLIT_S,X:STATUS
        BCLR    #SPLIT_P,X:STATUS
        RTS

COMP_U2 MOVE    #'_U2',A
        CMP     Y0,A
        JNE     <COMP_L1

        MOVE    #$F041,X0
        MOVE    X0,Y:SXR
        MOVE    #PARALLEL_2,X0
        MOVE    X0,Y:PARALLEL
        MOVE    #SERIAL_READ_RIGHT,X0
        MOVE    X0,Y:SERIAL_READ
        MOVE	  #SERIAL_SKIP_RIGHT,X0
        MOVE	  X0,Y:SERIAL_SKIP
        MOVE    #SERIAL_IDLE_RIGHT,X0
        MOVE    X0,Y:SERIAL_IDLE
        MOVE    #PARALLEL_CLEAR_2,X0
        MOVE    X0,Y:PARALLEL_CLEAR
	BCLR	#SPLIT_S,X:STATUS
        BCLR    #SPLIT_P,X:STATUS
        RTS

COMP_L1 MOVE    #'_L1',A
        CMP     Y0,A
        JNE     <COMP_L2


        MOVE    #$F082,X0
        MOVE    X0,Y:SXR
        MOVE    #PARALLEL_1,X0
        MOVE    X0,Y:PARALLEL
        MOVE    #SERIAL_READ_RIGHT,X0
        MOVE    X0,Y:SERIAL_READ
	MOVE	#SERIAL_SKIP_RIGHT,X0
	MOVE	X0,Y:SERIAL_SKIP
        MOVE    #SERIAL_IDLE_RIGHT,X0
        MOVE    X0,Y:SERIAL_IDLE
        MOVE    #PARALLEL_CLEAR_1,X0
        MOVE    X0,Y:PARALLEL_CLEAR
	BCLR	#SPLIT_S,X:STATUS
        BCLR    #SPLIT_P,X:STATUS
        RTS

COMP_L2 MOVE    #'_L2',A
        CMP     Y0,A
        JNE     <COMP_2

        MOVE    #$F000,X0
        MOVE    X0,Y:SXL
        MOVE    #PARALLEL_2,X0
        MOVE    X0,Y:PARALLEL
        MOVE    #SERIAL_READ_LEFT,X0
        MOVE    X0,Y:SERIAL_READ
	MOVE	#SERIAL_SKIP_LEFT,X0
	MOVE	X0,Y:SERIAL_SKIP
        MOVE    #SERIAL_IDLE_LEFT,X0
        MOVE    X0,Y:SERIAL_IDLE
        MOVE    #PARALLEL_CLEAR_2,X0
        MOVE    X0,Y:PARALLEL_CLEAR
	BCLR    #SPLIT_S,X:STATUS
        BCLR    #SPLIT_P,X:STATUS
        RTS

COMP_2  MOVE    #'__2',A
        CMP     Y0,A
        JNE     <COMP_1

        MOVE    #$F0C2,X0
;        MOVE    #$F041,X0
        MOVE    X0,Y:SXRL
        MOVE    #PARALLEL_2,X0
        MOVE    X0,Y:PARALLEL
        MOVE    #SERIAL_READ_SPLIT_SPECIAL__2,X0
        MOVE    X0,Y:SERIAL_READ
        MOVE	#SERIAL_SKIP_SPLIT,X0
        MOVE	X0,Y:SERIAL_SKIP
        MOVE    #SERIAL_IDLE_SPLIT,X0
        MOVE    X0,Y:SERIAL_IDLE
        MOVE    #PARALLEL_CLEAR_2,X0
        MOVE    X0,Y:PARALLEL_CLEAR
	BSET    #SPLIT_S,X:STATUS
        BCLR    #SPLIT_P,X:STATUS
        RTS

COMP_1  MOVE    #'__1',A
        CMP     Y0,A
        JNE     <COMP_ALL

        MOVE    #$F0C2,X0
        MOVE    X0,Y:SXRL
        MOVE    #PARALLEL_1,X0
        MOVE    X0,Y:PARALLEL
        MOVE    #SERIAL_READ_SPLIT_SPECIAL__1,X0
        MOVE    X0,Y:SERIAL_READ
	MOVE	#SERIAL_SKIP_SPLIT,X0
	MOVE	X0,Y:SERIAL_SKIP
        MOVE    #SERIAL_IDLE_SPLIT,X0
        MOVE    X0,Y:SERIAL_IDLE
        MOVE    #PARALLEL_CLEAR_1,X0
        MOVE    X0,Y:PARALLEL_CLEAR
	BSET    #SPLIT_S,X:STATUS
        BCLR    #SPLIT_P,X:STATUS
        RTS

COMP_ALL  MOVE    #'ALL',A
        CMP     Y0,A
        JNE     <COMP_TEMP

        MOVE    #$F0C0,X0
        MOVE    X0,Y:SXRL
        MOVE    #PARALLEL_SPLIT,X0
        MOVE    X0,Y:PARALLEL
        MOVE    #SERIAL_READ_SPLIT_SPECIAL_QUAD,X0
        MOVE    X0,Y:SERIAL_READ
	MOVE	#SERIAL_SKIP_SPLIT,X0
	MOVE	X0,Y:SERIAL_SKIP
        MOVE    #SERIAL_IDLE_SPLIT,X0
        MOVE    X0,Y:SERIAL_IDLE
        MOVE    #PARALLEL_CLEAR_SPLIT,X0
        MOVE    X0,Y:PARALLEL_CLEAR
	BSET    #SPLIT_S,X:STATUS
        BSET    #SPLIT_P,X:STATUS
        RTS

COMP_TEMP	
	MOVE	#>5,A
	CMP	Y0,A
	JNE	<ERROR

	MOVE    #$F040,X0
	MOVE    X0,Y:SXR
	MOVE    #PARALLEL_SPLIT,X0
	MOVE    X0,Y:PARALLEL
	MOVE    #SERIAL_READ_RIGHT,X0
	MOVE    X0,Y:SERIAL_READ
	MOVE	#SERIAL_SKIP_RIGHT,X0
	MOVE	X0,Y:SERIAL_SKIP
	MOVE    #SERIAL_IDLE_RIGHT,X0
	MOVE    X0,Y:SERIAL_IDLE
	MOVE    #PARALLEL_CLEAR_SPLIT,X0
	MOVE    X0,Y:PARALLEL_CLEAR
	BCLR    #SPLIT_S,X:STATUS
	BSET    #SPLIT_P,X:STATUS
	RTS



NO_POLARITY_SHIFT
        MOVE	Y:<OS,Y0
        MOVE    #'_U1',A
        CMP     Y0,A
        JNE     <COMP_U2_NO_POL

        MOVE    #SERIAL_IDLE_LEFT_NO_POL,X0
        MOVE    X0,Y:SERIAL_IDLE
        JMP     <FINISH

COMP_U2_NO_POL
        MOVE    #'_U2',A
        CMP     Y0,A
        JNE     <COMP_L1_NO_POL

        MOVE    #SERIAL_IDLE_RIGHT_NO_POL,X0
        MOVE    X0,Y:SERIAL_IDLE
        JMP     <FINISH

COMP_L1_NO_POL
        MOVE    #'_L1',A
        CMP     Y0,A
        JNE     <COMP_L2_NO_POL

        MOVE    #SERIAL_IDLE_RIGHT_NO_POL,X0
        MOVE    X0,Y:SERIAL_IDLE
        JMP     <FINISH

COMP_L2_NO_POL
        MOVE    #'_L2',A
        CMP     Y0,A
        JNE     <COMP_2_NO_POL

        MOVE    #SERIAL_IDLE_LEFT_NO_POL,X0
        MOVE    X0,Y:SERIAL_IDLE
        JMP     <FINISH

COMP_2_NO_POL
        MOVE    #'__2',A
        CMP     Y0,A
        JNE     <COMP_1_NO_POL

        MOVE    #SERIAL_IDLE_SPLIT_NO_POL,X0
        MOVE    X0,Y:SERIAL_IDLE
        JMP     <FINISH

COMP_1_NO_POL
        MOVE    #'__1',A
        CMP     Y0,A
        JNE     <COMP_ALL_NO_POL

        MOVE    #SERIAL_IDLE_SPLIT_NO_POL,X0
        MOVE    X0,Y:SERIAL_IDLE
        JMP     <FINISH

COMP_ALL_NO_POL  MOVE    #'ALL',A
        CMP     Y0,A
        JNE     <ERROR

        MOVE    #SERIAL_IDLE_SPLIT_NO_POL,X0
        MOVE    X0,Y:SERIAL_IDLE
        JMP     <FINISH

        


ERASE   MOVE X:(R3)+,X0                 ; Get TIME1 off the command buffer
        MOVE X0,Y:<TIME1                ; Move it to the address in tim.asm
        MOVE X:(R3)+,X0                 ; Ditto for TIME2
        MOVE X0,Y:<TIME2

        ;TODO: Get rid of this horrible hack
        ;Horrible hack to get over time out on command return
        JSR     <HACK_FINISH
	BSET	#3,X:PCRD		; Turn on the serial clock - this should already be the case
	BCLR	#1,X:<LATCH		; Separate updates of clock driver
	BSET	#CDAC,X:<LATCH		; Disable clearing of DACs
	BSET	#ENCK,X:<LATCH		; Enable clock and DAC output switches
	MOVEP	X:LATCH,Y:WRLATCH	; Write it to the hardware
	JSR	<PAL_DLY		; Delay for all this to happen

; Read DAC values from a table, and write them to the DACs
        MOVE    #ERHI,R0                ; Get starting address of erase clock values
	NOP
	NOP
	DO      Y:(R0)+,E_DAC   	; Repeat Y:(R0)+ times
	MOVE	Y:(R0)+,A		; Read the table entry
	JSR	<XMIT_A_WORD		; Transmit it to TIM-A-STD
	NOP
E_DAC

; Let the DAC voltages all ramp up before exiting
	MOVE	#400000,X0
	DO	X0,*+3			; 4 millisec delay
	NOP

; Start the delay loop        

        DO      Y:<TIME1,ER_T1          ; Delay TIME1 msec
        JSR     <LNG_DLY
        NOP

ER_T1   MOVE    Y:VSUBN,A               ; Reset the Vsub value
        JSR     <XMIT_A_WORD
        JSR     <PAL_DLY                ; Wait for SSI and PAL to empty

        DO      Y:<TIME2,ER_T2          ; Delay TIME2 msec
        JSR     <LNG_DLY
        NOP

ER_T2   MOVE    #ERHI_END,R0               ; Get the original clock values back.  
;        MOVE    #DACS,R0               ; This line would do the same job -- pointing back to the 
                                        ; original clocking values, but you'd be setting everything.

; Read DAC values from a table, and write them to the DACs
	NOP
	NOP
	DO      Y:(R0)+,END_DAC_RESTORE ; Repeat Y:(R0)+ times
	MOVE	Y:(R0)+,A		; Read the table entry
	JSR	<XMIT_A_WORD		; Transmit it to TIM-A-STD
	NOP
END_DAC_RESTORE

; Let the DAC voltages all ramp up before exiting
	MOVE	#400000,X0
	DO	X0,*+3			; 4 millisec delay
	NOP
        JMP     <START

; Routine to delay for 1msec
; 40ns for DO and NOP line.  1ms/80ns = 125000

LNG_DLY MOVE	#100000,X0
	DO      X0,*+3			; Wait 1 millisec for settling
	NOP 	
        RTS    

; Hack finish subroutine to send 'DON' reply back to the PIC
; card.  Some of this should be redundant -- such as jumping to the
; command processing.
HACK_FINISH     MOVE    X:<DONE,Y1	; Send 'DON' as the reply
HACK_FINISH1	MOVE    X:<HEADER,B	; Get header of incoming command
	MOVE    X:<SMASK,Y0	; This was the source byte, and is to 
	MOVE	#<COM_BUF,R3	;     become the destination byte
	AND     Y0,B X:<TWO,Y0
	LSR	#8,B		; Shift right eight bytes, add it to the
	MOVE	Y0,X:<NWORDS	;     header, and put 2 as the number
	ADD     Y0,B X:<SBRD,Y0	;     of words in the string
	ADD	Y0,B		; Add source board's header, set Y1 for above
	NOP
	MOVE    B,X:(R3)+       ; Put the new header on the transmitter stack
	MOVE	Y1,X:(R3)+	; Put the argument on the transmitter stack
	MOVE	B,X:<HEADER
        MOVE	#<COM_BUF,R3		; Restore R3 = beginning of the command

; Is this command for the timing board?
	MOVE    X:<HEADER,X0
	MOVE	X:<DMASK,B
	AND	X0,B  X:<TIM_DRB,X1 	; Extract destination byte
	CMP	X1,B		 	; Does header = timing board number? 
	JEQ	<COMMAND		; Yes, process it here
	JLT	<HACK_FO_XMT			; Send it to fiber optic transmitter
HACK_FO_XMT	MOVE	#COM_BUF,R3
	DO	X:<NWORDS,HACK_DON_FFO 	; Transmit all the words in the command
	MOVE	X:(R3)+,B
	JSR	<XMT_WRD
	NOP
HACK_DON_FFO	RTS

EPURGE  MOVE X:(R3)+,X0                 ; Get TIME1 off the command buffer
        MOVE X0,Y:<TIME1                ; Move it to the address in tim.asm
        
        ;TODO: Get rid of this horrible hack
        ;Horrible hack to get over time out on command return
        JSR     <HACK_FINISH
	BSET	#3,X:PCRD		; Turn on the serial clock - this should already be the case
	BCLR	#1,X:<LATCH		; Separate updates of clock driver
	BSET	#CDAC,X:<LATCH		; Disable clearing of DACs
	BSET	#ENCK,X:<LATCH		; Enable clock and DAC output switches
	MOVEP	X:LATCH,Y:WRLATCH	; Write it to the hardware
	JSR	<PAL_DLY		; Delay for all this to happen

; Read DAC values from a table, and write them to the DACs
        MOVE    #EPUR,R0                ; Get starting address of erase clock values
	NOP
	NOP
	DO      Y:(R0)+,E_DAC2   	; Repeat Y:(R0)+ times
	MOVE	Y:(R0)+,A		; Read the table entry
	JSR	<XMIT_A_WORD		; Transmit it to TIM-A-STD
	NOP
E_DAC2

; Let the DAC voltages all ramp before exiting
	MOVE	#400000,X0
	DO	X0,*+3			; 4 millisec delay
	NOP

; Start the delay loop        

        DO      Y:<TIME1,EPUR_T1          ; Delay TIME1 msec
        JSR     <LNG_DLY
        NOP

EPUR_T1   MOVE    #ERHI_END,R0               ; Get the original clock values back.  
	NOP
	NOP
	DO      Y:(R0)+,END_DAC_RESTORE2 ; Repeat Y:(R0)+ times
	MOVE	Y:(R0)+,A		; Read the table entry
	JSR	<XMIT_A_WORD		; Transmit it to TIM-A-STD
	NOP
END_DAC_RESTORE2

; Let the DAC voltages all ramp up before exiting
	MOVE	#400000,X0
	DO	X0,*+3			; 4 millisec delay
	NOP
        JMP     <START

; change the idling direction.  CID
CHANGE_IDLE_DIRECTION
        MOVE    X:(R3)+,Y0
        JSR     <CHG_IDL
        JMP     <FINISH

CHG_IDL MOVE	#'__L',A		; Shift right
	CMP	Y0,A
	JNE	<COMP_IR

        MOVE    #SERIAL_IDLE_LEFT,X0
        MOVE    X0,Y:SERIAL_IDLE
        BCLR    #SPLIT_S,X:STATUS
        RTS

COMP_IR MOVE    #'__R',A
        CMP     Y0,A
        JNE     <COMP_IS

        MOVE    #SERIAL_IDLE_RIGHT,X0
        MOVE    X0,Y:SERIAL_IDLE
        BCLR    #SPLIT_S,X:STATUS
        RTS

COMP_IS MOVE    #'__S',A
        CMP     Y0,A
        JNE     <ERROR

        MOVE    #SERIAL_IDLE_SPLIT,X0
        MOVE    X0,Y:SERIAL_IDLE
        BSET    #SPLIT_S,X:STATUS
        RTS

CHANGE_NUMBER_PARALLEL_CLEARS
        MOVE    X:(R3)+,Y0
        MOVE    Y0,Y:<N_PARALLEL_CLEARS
        JMP     <FINISH


CHANGE_IDLE_READ_DIRECTION
        MOVE    X:(R3)+,Y0
        JSR     <CHG_IDL_READ
        JMP     <FINISH

CHG_IDL_READ
        MOVE	#'__L',A		; Shift right
	CMP	Y0,A
	JNE	<COMP_I_R_R

        MOVE    #SERIAL_IDLE_LEFT,X0
        MOVE    X0,Y:SERIAL_IDLE
        MOVE    #SERIAL_READ_LEFT,X0
        MOVE    X0,Y:SERIAL_READ
        BCLR    #SPLIT_S,X:STATUS
        RTS

COMP_I_R_R
        MOVE    #'__R',A
        CMP     Y0,A
        JNE     <COMP_IS

        MOVE    #SERIAL_IDLE_RIGHT,X0
        MOVE    X0,Y:SERIAL_IDLE
        MOVE    #SERIAL_READ_RIGHT,X0
        MOVE    X0,Y:SERIAL_READ
        BCLR    #SPLIT_S,X:STATUS
        RTS

COMP_I_R_S 
        MOVE    #'__S',A
        CMP     Y0,A
        JNE     <ERROR

        MOVE    #SERIAL_READ_SPLIT,X0
        MOVE    X0,Y:SERIAL_READ
        BSET    #SPLIT_S,X:STATUS
        RTS



; Set synchronization mode of two controllers either ON or OFF
SET_SYNC_MODE
	MOVE	X:(R3)+,X0		; =1 for SYNC yes, =0 for not
	JCLR	#0,X0,DONT_SYNC
	BSET	#ST_SYNC,X:STATUS
	JMP	<FINISH
DONT_SYNC
	BCLR	#ST_SYNC,X:STATUS
	JMP	<FINISH

;SYNC_2

;***********************************************************
; routine to synchronise the controllers
; at least one controller must output the 25 MHz clk
; this code sends a high pulse out then goes low
; the controllers must determine if they are MASTER or SLAVE
; by looking at the JP2 LINK - they then run a synch
; routine depending on this input

SYNCH_DLY
	MOVE	#100000,X0		; 1 ms delay
	DO	X0,S_DLY1
	NOP
S_DLY1
	NOP
	RTS

SYNCH_CONTROLLER
	JSET	#SLAVE,X:PDRD,SLAVE_SYSTEM

MASTER_SYSTEM
	JSR	<SYNCH_DLY		; wait so that the slave goes first

	BCLR	#EXT_OUT0,X:HDR		; clear SYNCH bit
	REP	#2000			; wait 20 us
	NOP
;	BSET	#EXT_OUT0,X:HDR		; set SYNCH bit	
;	REP	#2000			; wait 20 us
;	NOP

CHK_1	JCLR    #EXT_IN0,X:HDR,CHK_1	; Wait for the slave signal to be high
	NOP
	REP	#2000			; ensure that it wasn't noise
	NOP
	JSET	#EXT_IN0,X:HDR,GO_LOW_PULSE
	JMP	<CHK_1			; invalid - look again	
GO_LOW_PULSE
	REP	#2000			; wait 20 us
	NOP
	BSET	#EXT_OUT0,X:HDR		; set SYNCH bit	
	REP	#2000			; wait 20 us
	NOP

CHK_2	JSET    #EXT_IN0,X:HDR,*	; Wait for a low signal from the slave
	NOP
	REP	#1990			; Let it be low for almost as long
	NOP				;  as the slave keeps it low
	JCLR	#EXT_IN0,X:HDR,MASTER_GOT_IT
	JMP	<CHK_2			; invalid - look again
MASTER_GOT_IT
	REP	#10
	NOP
	BCLR	#EXT_OUT0,X:HDR		; clear SYNCH bit	
	NOP
	JMP	<SYNCHED_NOW

SLAVE_SYSTEM
;	BCLR	#EXT_OUT0,X:HDR		; clear SYNCH bit
;	REP	#2000			; wait 20 us
;	NOP

	BSET	#EXT_OUT0,X:HDR		; set SYNCH bit	
	REP	#2000			; wait 20 us
	NOP

CHK_3	JCLR    #EXT_IN0,X:HDR,CHK_3	; keep checking
	NOP
	REP	#1000			; ensure that it wasn't noise
	NOP
	JSET	#EXT_IN0,X:HDR,GO_LOW_PULSE2
	JMP	<CHK_3		; invalid - look again	
GO_LOW_PULSE2
	REP	#1000			; wait 10 us
	NOP
	BCLR	#EXT_OUT0,X:HDR		; clear SYNCH bit	
	REP	#2000			; wait 20 us
	NOP
CHK_4	JSET    #EXT_IN0,X:HDR,CHK_4	; keep checking
	NOP
	REP	#1000			; ensure that it wasn't noise
	NOP
	JCLR	#EXT_IN0,X:HDR,SLAVE_GOT_IT
	JMP	<CHK_4			; invalid - look again
SLAVE_GOT_IT
	MOVE	#1000,X0
	DO	X0,*+3
	NOP	
	BCLR	#EXT_OUT0,X:HDR		; clear SYNCH bit	
	NOP	
SYNCHED_NOW

; Generate a recognizable pattern

	BSET	#EXT_OUT0,X:HDR		
	REP	#100
	NOP
	BCLR	#EXT_OUT0,X:HDR	
	REP	#100
	NOP
	BSET	#EXT_OUT0,X:HDR		
	REP	#100
	NOP
	BCLR	#EXT_OUT0,X:HDR		
	REP	#100
	NOP
	
	RTS

TIMS_TEST
  MOVE #020102,B
  JSR <XMT_WRD
  NOP
  MOVE #0,X0
  MOVE  X0,Y:NSR
  JMP <START

;CHANGE_VSUB
;  MOVE  X:(R3)+,Y0 ; put the Vsub voltage into Y0
;  CLR   A
;  MOVE  #$3F0000,X0 ; put base address of Vsub into A
;  ADD   X0,A
;  OR    Y0,A       ; or it with the voltage
;  NOP
;  MOVE  #VSUBN,X0  ; Get the address of Vsubn
;  MOVE  A,X0       ; Put A into this
;  JSR   <XMIT_A_WORD ; Send it out
;  JMP   <FINISH     ; Finish




