Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  tim.asm  Page 1



1                                 COMMENT *
2      
3                          This file is used to generate boot DSP code for the 250 MHz fiber optic
4                                  timing board using a DSP56303 as its main processor. It supports
5                                  split serial and frame transfer, but not split parallel nor binning.
6                                  *
7                                    PAGE    132                               ; Printronix page width - 132 columns
8      
9                          ; Include the boot and header files so addressing is easy
10                                   INCLUDE "timhdr.asm"
11                                COMMENT *
12     
13                         This is a header file that is shared between the fiber optic timing board
14                         boot and application code files for Rev. 5 = 250 MHz timing boards
15                                 *
16     
17                                   PAGE    132                               ; Printronix page width - 132 columns
18     
19                         ; Various addressing control registers
20        FFFFFB           BCR       EQU     $FFFFFB                           ; Bus Control Register
21        FFFFF9           AAR0      EQU     $FFFFF9                           ; Address Attribute Register, channel 0
22        FFFFF8           AAR1      EQU     $FFFFF8                           ; Address Attribute Register, channel 1
23        FFFFF7           AAR2      EQU     $FFFFF7                           ; Address Attribute Register, channel 2
24        FFFFF6           AAR3      EQU     $FFFFF6                           ; Address Attribute Register, channel 3
25        FFFFFD           PCTL      EQU     $FFFFFD                           ; PLL control register
26        FFFFFE           IPRP      EQU     $FFFFFE                           ; Interrupt Priority register - Peripheral
27        FFFFFF           IPRC      EQU     $FFFFFF                           ; Interrupt Priority register - Core
28     
29                         ; Port E is the Synchronous Communications Interface (SCI) port
30        FFFF9F           PCRE      EQU     $FFFF9F                           ; Port Control Register
31        FFFF9E           PRRE      EQU     $FFFF9E                           ; Port Direction Register
32        FFFF9D           PDRE      EQU     $FFFF9D                           ; Port Data Register
33        FFFF9C           SCR       EQU     $FFFF9C                           ; SCI Control Register
34        FFFF9B           SCCR      EQU     $FFFF9B                           ; SCI Clock Control Register
35     
36        FFFF9A           SRXH      EQU     $FFFF9A                           ; SCI Receive Data Register, High byte
37        FFFF99           SRXM      EQU     $FFFF99                           ; SCI Receive Data Register, Middle byte
38        FFFF98           SRXL      EQU     $FFFF98                           ; SCI Receive Data Register, Low byte
39     
40        FFFF97           STXH      EQU     $FFFF97                           ; SCI Transmit Data register, High byte
41        FFFF96           STXM      EQU     $FFFF96                           ; SCI Transmit Data register, Middle byte
42        FFFF95           STXL      EQU     $FFFF95                           ; SCI Transmit Data register, Low byte
43     
44        FFFF94           STXA      EQU     $FFFF94                           ; SCI Transmit Address Register
45        FFFF93           SSR       EQU     $FFFF93                           ; SCI Status Register
46     
47        000009           SCITE     EQU     9                                 ; X:SCR bit set to enable the SCI transmitter
48        000008           SCIRE     EQU     8                                 ; X:SCR bit set to enable the SCI receiver
49        000000           TRNE      EQU     0                                 ; This is set in X:SSR when the transmitter
50                                                                             ;  shift and data registers are both empty
51        000001           TDRE      EQU     1                                 ; This is set in X:SSR when the transmitter
52                                                                             ;  data register is empty
53        000002           RDRF      EQU     2                                 ; X:SSR bit set when receiver register is full
54        00000F           SELSCI    EQU     15                                ; 1 for SCI to backplane, 0 to front connector
55     
56     
57                         ; ESSI Flags
58        000006           TDE       EQU     6                                 ; Set when transmitter data register is empty
59        000007           RDF       EQU     7                                 ; Set when receiver is full of data
60        000010           TE        EQU     16                                ; Transmitter enable
61     
62                         ; Phase Locked Loop initialization
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timhdr.asm  Page 2



63        050003           PLL_INIT  EQU     $050003                           ; PLL = 25 MHz x 2 = 100 MHz
64     
65                         ; Port B general purpose I/O
66        FFFFC4           HPCR      EQU     $FFFFC4                           ; Control register (bits 1-6 cleared for GPIO)
67        FFFFC9           HDR       EQU     $FFFFC9                           ; Data register
68        FFFFC8           HDDR      EQU     $FFFFC8                           ; Data Direction Register bits (=1 for output)
69     
70                         ; Port C is Enhanced Synchronous Serial Port 0 = ESSI0
71        FFFFBF           PCRC      EQU     $FFFFBF                           ; Port C Control Register
72        FFFFBE           PRRC      EQU     $FFFFBE                           ; Port C Data direction Register
73        FFFFBD           PDRC      EQU     $FFFFBD                           ; Port C GPIO Data Register
74        FFFFBC           TX00      EQU     $FFFFBC                           ; Transmit Data Register #0
75        FFFFB8           RX0       EQU     $FFFFB8                           ; Receive data register
76        FFFFB7           SSISR0    EQU     $FFFFB7                           ; Status Register
77        FFFFB6           CRB0      EQU     $FFFFB6                           ; Control Register B
78        FFFFB5           CRA0      EQU     $FFFFB5                           ; Control Register A
79     
80                         ; Port D is Enhanced Synchronous Serial Port 1 = ESSI1
81        FFFFAF           PCRD      EQU     $FFFFAF                           ; Port D Control Register
82        FFFFAE           PRRD      EQU     $FFFFAE                           ; Port D Data direction Register
83        FFFFAD           PDRD      EQU     $FFFFAD                           ; Port D GPIO Data Register
84        FFFFAC           TX10      EQU     $FFFFAC                           ; Transmit Data Register 0
85        FFFFA7           SSISR1    EQU     $FFFFA7                           ; Status Register
86        FFFFA6           CRB1      EQU     $FFFFA6                           ; Control Register B
87        FFFFA5           CRA1      EQU     $FFFFA5                           ; Control Register A
88     
89                         ; Timer module addresses
90        FFFF8F           TCSR0     EQU     $FFFF8F                           ; Timer control and status register
91        FFFF8E           TLR0      EQU     $FFFF8E                           ; Timer load register = 0
92        FFFF8D           TCPR0     EQU     $FFFF8D                           ; Timer compare register = exposure time
93        FFFF8C           TCR0      EQU     $FFFF8C                           ; Timer count register = elapsed time
94        FFFF83           TPLR      EQU     $FFFF83                           ; Timer prescaler load register => milliseconds
95        FFFF82           TPCR      EQU     $FFFF82                           ; Timer prescaler count register
96        000000           TIM_BIT   EQU     0                                 ; Set to enable the timer
97        000009           TRM       EQU     9                                 ; Set to enable the timer preloading
98        000015           TCF       EQU     21                                ; Set when timer counter = compare register
99     
100                        ; Board specific addresses and constants
101       FFFFF1           RDFO      EQU     $FFFFF1                           ; Read incoming fiber optic data byte
102       FFFFF2           WRFO      EQU     $FFFFF2                           ; Write fiber optic data replies
103       FFFFF3           WRSS      EQU     $FFFFF3                           ; Write switch state
104       FFFFF5           WRLATCH   EQU     $FFFFF5                           ; Write to a latch
105       010000           RDAD      EQU     $010000                           ; Read A/D values into the DSP
106       000009           EF        EQU     9                                 ; Serial receiver empty flag
107    
108                        ; DSP port A bit equates
109       000000           PWROK     EQU     0                                 ; Power control board says power is OK
110       000001           LED1      EQU     1                                 ; Control one of two LEDs
111       000002           LVEN      EQU     2                                 ; Low voltage power enable
112       000003           HVEN      EQU     3                                 ; High voltage power enable
113       00000E           SSFHF     EQU     14                                ; Switch state FIFO half full flag
114       00000A           EXT_IN0   EQU     10                                ; External digital I/O to the timing board
115       00000B           EXT_IN1   EQU     11
116       00000C           EXT_OUT0  EQU     12
117       00000D           EXT_OUT1  EQU     13
118    
119                        ; Port D equate
120       000000           SLAVE     EQU     0                                 ; Set if not a master by having jumper 2 not installe
d
121       000001           SSFEF     EQU     1                                 ; Switch state FIFO empty flag
122    
123                        ; Other equates
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timhdr.asm  Page 3



124       000002           WRENA     EQU     2                                 ; Enable writing to the EEPROM
125    
126                        ; Latch U25 bit equates
127       000000           CDAC      EQU     0                                 ; Clear the analog board DACs
128       000002           ENCK      EQU     2                                 ; Enable the clock outputs
129       000004           SHUTTER   EQU     4                                 ; Control the shutter
130       000005           TIM_U_RST EQU     5                                 ; Reset the utility board
131    
132                        ; Software status bits, defined at X:<STATUS = X:0
133       000000           ST_RCV    EQU     0                                 ; Set to indicate word is from SCI = utility board
134       000002           IDLMODE   EQU     2                                 ; Set if need to idle after readout
135       000003           ST_SHUT   EQU     3                                 ; Set to indicate shutter is closed, clear for open
136       000004           ST_RDC    EQU     4                                 ; Set if executing 'RDC' command - reading out
137       000005           SPLIT_S   EQU     5                                 ; Set if split serial
138       000006           SPLIT_P   EQU     6                                 ; Set if split parallel
139       000007           MPP       EQU     7                                 ; Set if parallels are in MPP mode
140       000008           NOT_CLR   EQU     8                                 ; Set if not to clear CCD before exposure
141       00000A           TST_IMG   EQU     10                                ; Set if controller is to generate a test image
142       00000B           SHUT      EQU     11                                ; Set if opening shutter at beginning of exposure
143       00000C           ST_DITH   EQU     12                                ; Set if to dither during exposure
144       00000D           ST_SYNC   EQU     13                                ; Set if starting exposure on SYNC = high signal
145       00000E           ST_CNRD   EQU     14                                ; Set if in continous readout mode
146                        ; didn't need to commenct out above as we have a 24-bit status word...
147       00000F           POWERST   EQU     15                                ; current power state
148    
149                        ; Address for the table containing the incoming SCI words
150       000400           SCI_TABLE EQU     $400
151    
152    
153                        ; Specify controller configuration bits of the X:STATUS word
154                        ;   to describe the software capabilities of this application file
155                        ; The bit is set (=1) if the capability is supported by the controller
156    
157    
158                                COMMENT *
159    
160                        BIT #'s         FUNCTION
161                        2,1,0           Video Processor
162                                                000     CCD Rev. 3
163                                                001     CCD Gen I
164                                                010     IR Rev. 4
165                                                011     IR Coadder
166                                                100     CCD Rev. 5, Differential input
167                                                101     8x IR
168    
169                        4,3             Timing Board
170                                                00      Rev. 4, Gen II
171                                                01      Gen I
172                                                10      Rev. 5, Gen III, 250 MHz
173    
174                        6,5             Utility Board
175                                                00      No utility board
176                                                01      Utility Rev. 3
177    
178                        7               Shutter
179                                                0       No shutter support
180                                                1       Yes shutter support
181    
182                        9,8             Temperature readout
183                                                00      No temperature readout
184                                                01      Polynomial Diode calibration
185                                                10      Linear temperature sensor calibration
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timhdr.asm  Page 4



186    
187                        10              Subarray readout
188                                                0       Not supported
189                                                1       Yes supported
190    
191                        11              Binning
192                                                0       Not supported
193                                                1       Yes supported
194    
195                        12              Split-Serial readout
196                                                0       Not supported
197                                                1       Yes supported
198    
199                        13              Split-Parallel readout
200                                                0       Not supported
201                                                1       Yes supported
202    
203                        14              MPP = Inverted parallel clocks
204                                                0       Not supported
205                                                1       Yes supported
206    
207                        16,15           Clock Driver Board
208                                                00      Rev. 3
209                                                11      No clock driver board (Gen I)
210    
211                        19,18,17                Special implementations
212                                                000     Somewhere else
213                                                001     Mount Laguna Observatory
214                                                010     NGST Aladdin
215                                                xxx     Other
216                                *
217    
218                        CCDVIDREV3B
219       000000                     EQU     $000000                           ; CCD Video Processor Rev. 3
220       000001           VIDGENI   EQU     $000001                           ; CCD Video Processor Gen I
221       000002           IRREV4    EQU     $000002                           ; IR Video Processor Rev. 4
222       000003           COADDER   EQU     $000003                           ; IR Coadder
223       000004           CCDVIDREV5 EQU    $000004                           ; Differential input CCD video Rev. 5
224       000000           TIMREV4   EQU     $000000                           ; Timing Revision 4 = 50 MHz
225       000008           TIMGENI   EQU     $000008                           ; Timing Gen I = 40 MHz
226       000010           TIMREV5   EQU     $000010                           ; Timing Revision 5 = 250 MHz
227       000020           UTILREV3  EQU     $000020                           ; Utility Rev. 3 supported
228       000080           SHUTTER_CC EQU    $000080                           ; Shutter supported
229       000100           TEMP_POLY EQU     $000100                           ; Polynomial calibration
230                        TEMP_LINEAR
231       000200                     EQU     $000200                           ; Linear calibration
232       000400           SUBARRAY  EQU     $000400                           ; Subarray readout supported
233       000800           BINNING   EQU     $000800                           ; Binning supported
234                        SPLIT_SERIAL
235       001000                     EQU     $001000                           ; Split serial supported
236                        SPLIT_PARALLEL
237       002000                     EQU     $002000                           ; Split parallel supported
238       004000           MPP_CC    EQU     $004000                           ; Inverted clocks supported
239       018000           CLKDRVGENI EQU    $018000                           ; No clock driver board - Gen I
240       020000           MLO       EQU     $020000                           ; Set if Mount Laguna Observatory
241       040000           NGST      EQU     $040000                           ; NGST Aladdin implementation
242       100000           CONT_RD   EQU     $100000                           ; Continuous readout implemented
243    
244                                  INCLUDE "timboot.asm"
245                               COMMENT *
246    
247                        This file is used to generate boot DSP code for the 250 MHz fiber optic
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 5



248                                timing board using a DSP56303 as its main processor.
249                        Added utility board support Dec. 2002
250                                *
251                                  PAGE    132                               ; Printronix page width - 132 columns
252    
253                        ; Special address for two words for the DSP to bootstrap code from the EEPROM
254                                  IF      @SCP("HOST","ROM")
261                                  ENDIF
262    
263                                  IF      @SCP("HOST","HOST")
264       P:000000 P:000000                   ORG     P:0,P:0
265       P:000000 P:000000 0C0190            JMP     <INIT
266       P:000001 P:000001 000000            NOP
267                                           ENDIF
268    
269                                 ;  This ISR receives serial words a byte at a time over the asynchronous
270                                 ;    serial link (SCI) and squashes them into a single 24-bit word
271       P:000002 P:000002 602400  SCI_RCV   MOVE              R0,X:<SAVE_R0           ; Save R0
272       P:000003 P:000003 052139            MOVEC             SR,X:<SAVE_SR           ; Save Status Register
273       P:000004 P:000004 60A700            MOVE              X:<SCI_R0,R0            ; Restore R0 = pointer to SCI receive regist
er
274       P:000005 P:000005 542300            MOVE              A1,X:<SAVE_A1           ; Save A1
275       P:000006 P:000006 452200            MOVE              X1,X:<SAVE_X1           ; Save X1
276       P:000007 P:000007 54A600            MOVE              X:<SCI_A1,A1            ; Get SRX value of accumulator contents
277       P:000008 P:000008 45E000            MOVE              X:(R0),X1               ; Get the SCI byte
278       P:000009 P:000009 0AD041            BCLR    #1,R0                             ; Test for the address being $FFF6 = last by
te
279       P:00000A P:00000A 000000            NOP
280       P:00000B P:00000B 000000            NOP
281       P:00000C P:00000C 000000            NOP
282       P:00000D P:00000D 205862            OR      X1,A      (R0)+                   ; Add the byte into the 24-bit word
283       P:00000E P:00000E 0E0013            JCC     <MID_BYT                          ; Not the last byte => only restore register
s
284       P:00000F P:00000F 545C00  END_BYT   MOVE              A1,X:(R4)+              ; Put the 24-bit word into the SCI buffer
285       P:000010 P:000010 60F400            MOVE              #SRXL,R0                ; Re-establish first address of SCI interfac
e
                            FFFF98
286       P:000012 P:000012 2C0000            MOVE              #0,A1                   ; For zeroing out SCI_A1
287       P:000013 P:000013 602700  MID_BYT   MOVE              R0,X:<SCI_R0            ; Save the SCI receiver address
288       P:000014 P:000014 542600            MOVE              A1,X:<SCI_A1            ; Save A1 for next interrupt
289       P:000015 P:000015 05A139            MOVEC             X:<SAVE_SR,SR           ; Restore Status Register
290       P:000016 P:000016 54A300            MOVE              X:<SAVE_A1,A1           ; Restore A1
291       P:000017 P:000017 45A200            MOVE              X:<SAVE_X1,X1           ; Restore X1
292       P:000018 P:000018 60A400            MOVE              X:<SAVE_R0,R0           ; Restore R0
293       P:000019 P:000019 000004            RTI                                       ; Return from interrupt service
294    
295                                 ; Clear error condition and interrupt on SCI receiver
296       P:00001A P:00001A 077013  CLR_ERR   MOVEP             X:SSR,X:RCV_ERR         ; Read SCI status register
                            000025
297       P:00001C P:00001C 077018            MOVEP             X:SRXL,X:RCV_ERR        ; This clears any error
                            000025
298       P:00001E P:00001E 000004            RTI
299    
300       P:00001F P:00001F                   DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
301       P:000030 P:000030                   DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
302       P:000040 P:000040                   DC      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
303    
304                                 ;Tune the table so the following instruction is at P:$50 exactly.
305       P:000050 P:000050 0D0002            JSR     SCI_RCV                           ; SCI receive data interrupt
306       P:000051 P:000051 000000            NOP
307       P:000052 P:000052 0D001A            JSR     CLR_ERR                           ; SCI receive error interrupt
308       P:000053 P:000053 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 6



309    
310                                 ; *******************  Command Processing  ******************
311    
312                                 ; Read the header and check it for self-consistency
313       P:000054 P:000054 609F00  START     MOVE              X:<IDL_ADR,R0
314       P:000055 P:000055 018FA0            JSET    #TIM_BIT,X:TCSR0,EXPOSING         ; If exposing go check the timer
                            00038E
315       P:000057 P:000057 0A00A4            JSET    #ST_RDC,X:<STATUS,CONTINUE_READING
                            100000
316       P:000059 P:000059 0AE080            JMP     (R0)
317    
318       P:00005A P:00005A 330700  TST_RCV   MOVE              #<COM_BUF,R3
319       P:00005B P:00005B 0D00A5            JSR     <GET_RCV
320       P:00005C P:00005C 0E005B            JCC     *-1
321    
322                                 ; Check the header and read all the remaining words in the command
323       P:00005D P:00005D 0C00FF  PRC_RCV   JMP     <CHK_HDR                          ; Update HEADER and NWORDS
324       P:00005E P:00005E 578600  PR_RCV    MOVE              X:<NWORDS,B             ; Read this many words total in the command
325       P:00005F P:00005F 000000            NOP
326       P:000060 P:000060 01418C            SUB     #1,B                              ; We've already read the header
327       P:000061 P:000061 000000            NOP
328       P:000062 P:000062 06CF00            DO      B,RD_COM
                            00006A
329       P:000064 P:000064 205B00            MOVE              (R3)+                   ; Increment past what's been read already
330       P:000065 P:000065 0B0080  GET_WRD   JSCLR   #ST_RCV,X:STATUS,CHK_FO
                            0000A9
331       P:000067 P:000067 0B00A0            JSSET   #ST_RCV,X:STATUS,CHK_SCI
                            0000D5
332       P:000069 P:000069 0E0065            JCC     <GET_WRD
333       P:00006A P:00006A 000000            NOP
334       P:00006B P:00006B 330700  RD_COM    MOVE              #<COM_BUF,R3            ; Restore R3 = beginning of the command
335    
336                                 ; Is this command for the timing board?
337       P:00006C P:00006C 448500            MOVE              X:<HEADER,X0
338       P:00006D P:00006D 579B00            MOVE              X:<DMASK,B
339       P:00006E P:00006E 459A4E            AND     X0,B      X:<TIM_DRB,X1           ; Extract destination byte
340       P:00006F P:00006F 20006D            CMP     X1,B                              ; Does header = timing board number?
341       P:000070 P:000070 0EA080            JEQ     <COMMAND                          ; Yes, process it here
342       P:000071 P:000071 0E909D            JLT     <FO_XMT                           ; Send it to fiber optic transmitter
343    
344                                 ; Transmit the command to the utility board over the SCI port
345       P:000072 P:000072 060600            DO      X:<NWORDS,DON_XMT                 ; Transmit NWORDS
                            00007E
346       P:000074 P:000074 60F400            MOVE              #STXL,R0                ; SCI first byte address
                            FFFF95
347       P:000076 P:000076 44DB00            MOVE              X:(R3)+,X0              ; Get the 24-bit word to transmit
348       P:000077 P:000077 060380            DO      #3,SCI_SPT
                            00007D
349       P:000079 P:000079 019381            JCLR    #TDRE,X:SSR,*                     ; Continue ONLY if SCI XMT is empty
                            000079
350       P:00007B P:00007B 445800            MOVE              X0,X:(R0)+              ; Write to SCI, byte pointer + 1
351       P:00007C P:00007C 000000            NOP                                       ; Delay for the status flag to be set
352       P:00007D P:00007D 000000            NOP
353                                 SCI_SPT
354       P:00007E P:00007E 000000            NOP
355                                 DON_XMT
356       P:00007F P:00007F 0C0054            JMP     <START
357    
358                                 ; Process the receiver entry - is it in the command table ?
359       P:000080 P:000080 0203DF  COMMAND   MOVE              X:(R3+1),B              ; Get the command
360       P:000081 P:000081 205B00            MOVE              (R3)+
361       P:000082 P:000082 205B00            MOVE              (R3)+                   ; Point R3 to the first argument
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 7



362       P:000083 P:000083 302800            MOVE              #<COM_TBL,R0            ; Get the command table starting address
363       P:000084 P:000084 062C80            DO      #NUM_COM,END_COM                  ; Loop over the command table
                            00008B
364       P:000086 P:000086 47D800            MOVE              X:(R0)+,Y1              ; Get the command table entry
365       P:000087 P:000087 62E07D            CMP     Y1,B      X:(R0),R2               ; Does receiver = table entries address?
366       P:000088 P:000088 0E208B            JNE     <NOT_COM                          ; No, keep looping
367       P:000089 P:000089 00008C            ENDDO                                     ; Restore the DO loop system registers
368       P:00008A P:00008A 0AE280            JMP     (R2)                              ; Jump execution to the command
369       P:00008B P:00008B 205800  NOT_COM   MOVE              (R0)+                   ; Increment the register past the table addr
ess
370                                 END_COM
371       P:00008C P:00008C 0C008D            JMP     <ERROR                            ; The command is not in the table
372    
373                                 ; It's not in the command table - send an error message
374       P:00008D P:00008D 479D00  ERROR     MOVE              X:<ERR,Y1               ; Send the message - there was an error
375       P:00008E P:00008E 0C0090            JMP     <FINISH1                          ; This protects against unknown commands
376    
377                                 ; Send a reply packet - header and reply
378       P:00008F P:00008F 479800  FINISH    MOVE              X:<DONE,Y1              ; Send 'DON' as the reply
379    
380       P:000090 P:000090 578500  FINISH1   MOVE              X:<HEADER,B             ; Get header of incoming command
381       P:000091 P:000091 469C00            MOVE              X:<SMASK,Y0             ; This was the source byte, and is to
382       P:000092 P:000092 330700            MOVE              #<COM_BUF,R3            ;     become the destination byte
383       P:000093 P:000093 46935E            AND     Y0,B      X:<TWO,Y0
384       P:000094 P:000094 0C1ED1            LSR     #8,B                              ; Shift right eight bytes, add it to the
385       P:000095 P:000095 460600            MOVE              Y0,X:<NWORDS            ;     header, and put 2 as the number
386       P:000096 P:000096 469958            ADD     Y0,B      X:<SBRD,Y0              ;     of words in the string
387       P:000097 P:000097 200058            ADD     Y0,B                              ; Add source board's header, set Y1 for abov
e
388       P:000098 P:000098 000000            NOP
389       P:000099 P:000099 575B00            MOVE              B,X:(R3)+               ; Put the new header on the transmitter stac
k
390       P:00009A P:00009A 475B00            MOVE              Y1,X:(R3)+              ; Put the argument on the transmitter stack
391       P:00009B P:00009B 570500            MOVE              B,X:<HEADER
392       P:00009C P:00009C 0C006B            JMP     <RD_COM                           ; Decide where to send the reply, and do it
393    
394                                 ; Transmit words to the host computer over the fiber optics link
395       P:00009D P:00009D 63F400  FO_XMT    MOVE              #COM_BUF,R3
                            000007
396       P:00009F P:00009F 060600            DO      X:<NWORDS,DON_FFO                 ; Transmit all the words in the command
                            0000A3
397       P:0000A1 P:0000A1 57DB00            MOVE              X:(R3)+,B
398       P:0000A2 P:0000A2 0D00EB            JSR     <XMT_WRD
399       P:0000A3 P:0000A3 000000            NOP
400       P:0000A4 P:0000A4 0C0054  DON_FFO   JMP     <START
401    
402                                 ; Check for commands from the fiber optic FIFO and the utility board (SCI)
403       P:0000A5 P:0000A5 0D00A9  GET_RCV   JSR     <CHK_FO                           ; Check for fiber optic command from FIFO
404       P:0000A6 P:0000A6 0E80A8            JCS     <RCV_RTS                          ; If there's a command, check the header
405       P:0000A7 P:0000A7 0D00D5            JSR     <CHK_SCI                          ; Check for an SCI command
406       P:0000A8 P:0000A8 00000C  RCV_RTS   RTS
407    
408                                 ; Because of FIFO metastability require that EF be stable for two tests
409       P:0000A9 P:0000A9 0A8989  CHK_FO    JCLR    #EF,X:HDR,TST2                    ; EF = Low,  Low  => CLR SR, return
                            0000AC
410       P:0000AB P:0000AB 0C00AF            JMP     <TST3                             ;      High, Low  => try again
411       P:0000AC P:0000AC 0A8989  TST2      JCLR    #EF,X:HDR,CLR_CC                  ;      Low,  High => try again
                            0000D1
412       P:0000AE P:0000AE 0C00A9            JMP     <CHK_FO                           ;      High, High => read FIFO
413       P:0000AF P:0000AF 0A8989  TST3      JCLR    #EF,X:HDR,CHK_FO
                            0000A9
414    
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 8



415       P:0000B1 P:0000B1 08F4BB            MOVEP             #$028FE2,X:BCR          ; Slow down RDFO access
                            028FE2
416       P:0000B3 P:0000B3 000000            NOP
417       P:0000B4 P:0000B4 000000            NOP
418       P:0000B5 P:0000B5 5FF000            MOVE                          Y:RDFO,B
                            FFFFF1
419       P:0000B7 P:0000B7 2B0000            MOVE              #0,B2
420       P:0000B8 P:0000B8 0140CE            AND     #$FF,B
                            0000FF
421       P:0000BA P:0000BA 0140CD            CMP     #>$AC,B                           ; It must be $AC to be a valid word
                            0000AC
422       P:0000BC P:0000BC 0E20D1            JNE     <CLR_CC
423       P:0000BD P:0000BD 4EF000            MOVE                          Y:RDFO,Y0   ; Read the MS byte
                            FFFFF1
424       P:0000BF P:0000BF 0C1951            INSERT  #$008010,Y0,B
                            008010
425       P:0000C1 P:0000C1 4EF000            MOVE                          Y:RDFO,Y0   ; Read the middle byte
                            FFFFF1
426       P:0000C3 P:0000C3 0C1951            INSERT  #$008008,Y0,B
                            008008
427       P:0000C5 P:0000C5 4EF000            MOVE                          Y:RDFO,Y0   ; Read the LS byte
                            FFFFF1
428       P:0000C7 P:0000C7 0C1951            INSERT  #$008000,Y0,B
                            008000
429       P:0000C9 P:0000C9 000000            NOP
430       P:0000CA P:0000CA 516300            MOVE              B0,X:(R3)               ; Put the word into COM_BUF
431       P:0000CB P:0000CB 0A0000            BCLR    #ST_RCV,X:<STATUS                 ; Its a command from the host computer
432       P:0000CC P:0000CC 000000  SET_CC    NOP
433       P:0000CD P:0000CD 0AF960            BSET    #0,SR                             ; Valid word => SR carry bit = 1
434       P:0000CE P:0000CE 08F4BB            MOVEP             #$028FE1,X:BCR          ; Restore RDFO access
                            028FE1
435       P:0000D0 P:0000D0 00000C            RTS
436       P:0000D1 P:0000D1 0AF940  CLR_CC    BCLR    #0,SR                             ; Not valid word => SR carry bit = 0
437       P:0000D2 P:0000D2 08F4BB            MOVEP             #$028FE1,X:BCR          ; Restore RDFO access
                            028FE1
438       P:0000D4 P:0000D4 00000C            RTS
439    
440                                 ; Test the SCI (= synchronous communications interface) for new words
441       P:0000D5 P:0000D5 44F000  CHK_SCI   MOVE              X:(SCI_TABLE+33),X0
                            000421
442       P:0000D7 P:0000D7 228E00            MOVE              R4,A
443       P:0000D8 P:0000D8 209000            MOVE              X0,R0
444       P:0000D9 P:0000D9 200045            CMP     X0,A
445       P:0000DA P:0000DA 0EA0D1            JEQ     <CLR_CC                           ; There is no new SCI word
446       P:0000DB P:0000DB 44D800            MOVE              X:(R0)+,X0
447       P:0000DC P:0000DC 446300            MOVE              X0,X:(R3)
448       P:0000DD P:0000DD 220E00            MOVE              R0,A
449       P:0000DE P:0000DE 0140C5            CMP     #(SCI_TABLE+32),A                 ; Wrap it around the circular
                            000420
450       P:0000E0 P:0000E0 0EA0E4            JEQ     <INIT_PROCESSED_SCI               ;   buffer boundary
451       P:0000E1 P:0000E1 547000            MOVE              A1,X:(SCI_TABLE+33)
                            000421
452       P:0000E3 P:0000E3 0C00E9            JMP     <SCI_END
453                                 INIT_PROCESSED_SCI
454       P:0000E4 P:0000E4 56F400            MOVE              #SCI_TABLE,A
                            000400
455       P:0000E6 P:0000E6 000000            NOP
456       P:0000E7 P:0000E7 567000            MOVE              A,X:(SCI_TABLE+33)
                            000421
457       P:0000E9 P:0000E9 0A0020  SCI_END   BSET    #ST_RCV,X:<STATUS                 ; Its a utility board (SCI) word
458       P:0000EA P:0000EA 0C00CC            JMP     <SET_CC
459    
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 9



460                                 ; Transmit the word in B1 to the host computer over the fiber optic data link
461                                 XMT_WRD
462       P:0000EB P:0000EB 08F4BB            MOVEP             #$028FE2,X:BCR          ; Slow down RDFO access
                            028FE2
463       P:0000ED P:0000ED 60F400            MOVE              #FO_HDR+1,R0
                            000002
464       P:0000EF P:0000EF 060380            DO      #3,XMT_WRD1
                            0000F3
465       P:0000F1 P:0000F1 0C1D91            ASL     #8,B,B
466       P:0000F2 P:0000F2 000000            NOP
467       P:0000F3 P:0000F3 535800            MOVE              B2,X:(R0)+
468                                 XMT_WRD1
469       P:0000F4 P:0000F4 60F400            MOVE              #FO_HDR,R0
                            000001
470       P:0000F6 P:0000F6 61F400            MOVE              #WRFO,R1
                            FFFFF2
471       P:0000F8 P:0000F8 060480            DO      #4,XMT_WRD2
                            0000FB
472       P:0000FA P:0000FA 46D800            MOVE              X:(R0)+,Y0              ; Should be MOVEP  X:(R0)+,Y:WRFO
473       P:0000FB P:0000FB 4E6100            MOVE                          Y0,Y:(R1)
474                                 XMT_WRD2
475       P:0000FC P:0000FC 08F4BB            MOVEP             #$028FE1,X:BCR          ; Restore RDFO access
                            028FE1
476       P:0000FE P:0000FE 00000C            RTS
477    
478                                 ; Check the command or reply header in X:(R3) for self-consistency
479       P:0000FF P:0000FF 46E300  CHK_HDR   MOVE              X:(R3),Y0
480       P:000100 P:000100 579600            MOVE              X:<MASK1,B              ; Test for S.LE.3 and D.LE.3 and N.LE.7
481       P:000101 P:000101 20005E            AND     Y0,B
482       P:000102 P:000102 0E208D            JNE     <ERROR                            ; Test failed
483       P:000103 P:000103 579700            MOVE              X:<MASK2,B              ; Test for either S.NE.0 or D.NE.0
484       P:000104 P:000104 20005E            AND     Y0,B
485       P:000105 P:000105 0EA08D            JEQ     <ERROR                            ; Test failed
486       P:000106 P:000106 579500            MOVE              X:<SEVEN,B
487       P:000107 P:000107 20005E            AND     Y0,B                              ; Extract NWORDS, must be > 0
488       P:000108 P:000108 0EA08D            JEQ     <ERROR
489       P:000109 P:000109 44E300            MOVE              X:(R3),X0
490       P:00010A P:00010A 440500            MOVE              X0,X:<HEADER            ; Its a correct header
491       P:00010B P:00010B 550600            MOVE              B1,X:<NWORDS            ; Number of words in the command
492       P:00010C P:00010C 0C005E            JMP     <PR_RCV
493    
494                                 ;  *****************  Boot Commands  *******************
495    
496                                 ; Test Data Link - simply return value received after 'TDL'
497       P:00010D P:00010D 47DB00  TDL       MOVE              X:(R3)+,Y1              ; Get the data value
498       P:00010E P:00010E 0C0090            JMP     <FINISH1                          ; Return from executing TDL command
499    
500                                 ; Read DSP or EEPROM memory ('RDM' address): read memory, reply with value
501       P:00010F P:00010F 47DB00  RDMEM     MOVE              X:(R3)+,Y1
502       P:000110 P:000110 20EF00            MOVE              Y1,B
503       P:000111 P:000111 0140CE            AND     #$0FFFFF,B                        ; Bits 23-20 need to be zeroed
                            0FFFFF
504       P:000113 P:000113 21B000            MOVE              B1,R0                   ; Need the address in an address register
505       P:000114 P:000114 20EF00            MOVE              Y1,B
506       P:000115 P:000115 000000            NOP
507       P:000116 P:000116 0ACF14            JCLR    #20,B,RDX                         ; Test address bit for Program memory
                            00011A
508       P:000118 P:000118 07E087            MOVE              P:(R0),Y1               ; Read from Program Memory
509       P:000119 P:000119 0C0090            JMP     <FINISH1                          ; Send out a header with the value
510       P:00011A P:00011A 0ACF15  RDX       JCLR    #21,B,RDY                         ; Test address bit for X: memory
                            00011E
511       P:00011C P:00011C 47E000            MOVE              X:(R0),Y1               ; Write to X data memory
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 10



512       P:00011D P:00011D 0C0090            JMP     <FINISH1                          ; Send out a header with the value
513       P:00011E P:00011E 0ACF16  RDY       JCLR    #22,B,RDR                         ; Test address bit for Y: memory
                            000122
514       P:000120 P:000120 4FE000            MOVE                          Y:(R0),Y1   ; Read from Y data memory
515       P:000121 P:000121 0C0090            JMP     <FINISH1                          ; Send out a header with the value
516       P:000122 P:000122 0ACF17  RDR       JCLR    #23,B,ERROR                       ; Test address bit for read from EEPROM memo
ry
                            00008D
517       P:000124 P:000124 479400            MOVE              X:<THREE,Y1             ; Convert to word address to a byte address
518       P:000125 P:000125 220600            MOVE              R0,Y0                   ; Get 16-bit address in a data register
519       P:000126 P:000126 2000B8            MPY     Y0,Y1,B                           ; Multiply
520       P:000127 P:000127 20002A            ASR     B                                 ; Eliminate zero fill of fractional multiply
521       P:000128 P:000128 213000            MOVE              B0,R0                   ; Need to address memory
522       P:000129 P:000129 0AD06F            BSET    #15,R0                            ; Set bit so its in EEPROM space
523       P:00012A P:00012A 0D0178            JSR     <RD_WORD                          ; Read word from EEPROM
524       P:00012B P:00012B 21A700            MOVE              B1,Y1                   ; FINISH1 transmits Y1 as its reply
525       P:00012C P:00012C 0C0090            JMP     <FINISH1
526    
527                                 ; Program WRMEM ('WRM' address datum): write to memory, reply 'DON'.
528       P:00012D P:00012D 47DB00  WRMEM     MOVE              X:(R3)+,Y1              ; Get the address to be written to
529       P:00012E P:00012E 20EF00            MOVE              Y1,B
530       P:00012F P:00012F 0140CE            AND     #$0FFFFF,B                        ; Bits 23-20 need to be zeroed
                            0FFFFF
531       P:000131 P:000131 21B000            MOVE              B1,R0                   ; Need the address in an address register
532       P:000132 P:000132 20EF00            MOVE              Y1,B
533       P:000133 P:000133 46DB00            MOVE              X:(R3)+,Y0              ; Get datum into Y0 so MOVE works easily
534       P:000134 P:000134 0ACF14            JCLR    #20,B,WRX                         ; Test address bit for Program memory
                            000138
535       P:000136 P:000136 076086            MOVE              Y0,P:(R0)               ; Write to Program memory
536       P:000137 P:000137 0C008F            JMP     <FINISH
537       P:000138 P:000138 0ACF15  WRX       JCLR    #21,B,WRY                         ; Test address bit for X: memory
                            00013C
538       P:00013A P:00013A 466000            MOVE              Y0,X:(R0)               ; Write to X: memory
539       P:00013B P:00013B 0C008F            JMP     <FINISH
540       P:00013C P:00013C 0ACF16  WRY       JCLR    #22,B,WRR                         ; Test address bit for Y: memory
                            000140
541       P:00013E P:00013E 4E6000            MOVE                          Y0,Y:(R0)   ; Write to Y: memory
542       P:00013F P:00013F 0C008F            JMP     <FINISH
543       P:000140 P:000140 0ACF17  WRR       JCLR    #23,B,ERROR                       ; Test address bit for write to EEPROM
                            00008D
544       P:000142 P:000142 013D02            BCLR    #WRENA,X:PDRC                     ; WR_ENA* = 0 to enable EEPROM writing
545       P:000143 P:000143 460E00            MOVE              Y0,X:<SV_A1             ; Save the datum to be written
546       P:000144 P:000144 479400            MOVE              X:<THREE,Y1             ; Convert word address to a byte address
547       P:000145 P:000145 220600            MOVE              R0,Y0                   ; Get 16-bit address in a data register
548       P:000146 P:000146 2000B8            MPY     Y1,Y0,B                           ; Multiply
549       P:000147 P:000147 20002A            ASR     B                                 ; Eliminate zero fill of fractional multiply
550       P:000148 P:000148 213000            MOVE              B0,R0                   ; Need to address memory
551       P:000149 P:000149 0AD06F            BSET    #15,R0                            ; Set bit so its in EEPROM space
552       P:00014A P:00014A 558E00            MOVE              X:<SV_A1,B1             ; Get the datum to be written
553       P:00014B P:00014B 060380            DO      #3,L1WRR                          ; Loop over three bytes of the word
                            000154
554       P:00014D P:00014D 07588D            MOVE              B1,P:(R0)+              ; Write each EEPROM byte
555       P:00014E P:00014E 0C1C91            ASR     #8,B,B
556       P:00014F P:00014F 469E00            MOVE              X:<C100K,Y0             ; Move right one byte, enter delay = 1 msec
557       P:000150 P:000150 06C600            DO      Y0,L2WRR                          ; Delay by 12 milliseconds for EEPROM write
                            000153
558       P:000152 P:000152 060CA0            REP     #12                               ; Assume 100 MHz DSP56303
559       P:000153 P:000153 000000            NOP
560                                 L2WRR
561       P:000154 P:000154 000000            NOP                                       ; DO loop nesting restriction
562                                 L1WRR
563       P:000155 P:000155 013D22            BSET    #WRENA,X:PDRC                     ; WR_ENA* = 1 to disable EEPROM writing
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 11



564    
565       P:000156 P:000156 0C008F            JMP     <FINISH
566    
567                                 ; Load application code from P: memory into its proper locations
568       P:000157 P:000157 47DB00  LDAPPL    MOVE              X:(R3)+,Y1              ; Application number, not used yet
569       P:000158 P:000158 0D015A            JSR     <LOAD_APPLICATION
570       P:000159 P:000159 0C008F            JMP     <FINISH
571    
572                                 LOAD_APPLICATION
573       P:00015A P:00015A 60F400            MOVE              #$8000,R0               ; Starting EEPROM address
                            008000
574       P:00015C P:00015C 0D0178            JSR     <RD_WORD                          ; Number of words in boot code
575       P:00015D P:00015D 21A600            MOVE              B1,Y0
576       P:00015E P:00015E 479400            MOVE              X:<THREE,Y1
577       P:00015F P:00015F 2000B8            MPY     Y0,Y1,B
578       P:000160 P:000160 20002A            ASR     B
579       P:000161 P:000161 213000            MOVE              B0,R0                   ; EEPROM address of start of P: application
580       P:000162 P:000162 0AD06F            BSET    #15,R0                            ; To access EEPROM
581       P:000163 P:000163 0D0178            JSR     <RD_WORD                          ; Read number of words in application P:
582       P:000164 P:000164 61F400            MOVE              #(X_BOOT_START+1),R1    ; End of boot P: code that needs keeping
                            00022B
583       P:000166 P:000166 06CD00            DO      B1,RD_APPL_P
                            000169
584       P:000168 P:000168 0D0178            JSR     <RD_WORD
585       P:000169 P:000169 07598D            MOVE              B1,P:(R1)+
586                                 RD_APPL_P
587       P:00016A P:00016A 0D0178            JSR     <RD_WORD                          ; Read number of words in application X:
588       P:00016B P:00016B 61F400            MOVE              #END_COMMAND_TABLE,R1
                            000036
589       P:00016D P:00016D 06CD00            DO      B1,RD_APPL_X
                            000170
590       P:00016F P:00016F 0D0178            JSR     <RD_WORD
591       P:000170 P:000170 555900            MOVE              B1,X:(R1)+
592                                 RD_APPL_X
593       P:000171 P:000171 0D0178            JSR     <RD_WORD                          ; Read number of words in application Y:
594       P:000172 P:000172 310100            MOVE              #1,R1                   ; There is no Y: memory in the boot code
595       P:000173 P:000173 06CD00            DO      B1,RD_APPL_Y
                            000176
596       P:000175 P:000175 0D0178            JSR     <RD_WORD
597       P:000176 P:000176 5D5900            MOVE                          B1,Y:(R1)+
598                                 RD_APPL_Y
599       P:000177 P:000177 00000C            RTS
600    
601                                 ; Read one word from EEPROM location R0 into accumulator B1
602       P:000178 P:000178 060380  RD_WORD   DO      #3,L_RDBYTE
                            00017B
603       P:00017A P:00017A 07D88B            MOVE              P:(R0)+,B2
604       P:00017B P:00017B 0C1C91            ASR     #8,B,B
605                                 L_RDBYTE
606       P:00017C P:00017C 00000C            RTS
607    
608                                 ; Come to here on a 'STP' command so 'DON' can be sent
609                                 STOP_IDLE_CLOCKING
610       P:00017D P:00017D 305A00            MOVE              #<TST_RCV,R0            ; Execution address when idle => when not
611       P:00017E P:00017E 601F00            MOVE              R0,X:<IDL_ADR           ;   processing commands or reading out
612       P:00017F P:00017F 0A0002            BCLR    #IDLMODE,X:<STATUS                ; Don't idle after readout
613       P:000180 P:000180 0C008F            JMP     <FINISH
614    
615                                 ; Routines executed after the DSP boots and initializes
616       P:000181 P:000181 305A00  STARTUP   MOVE              #<TST_RCV,R0            ; Execution address when idle => when not
617       P:000182 P:000182 601F00            MOVE              R0,X:<IDL_ADR           ;   processing commands or reading out
618       P:000183 P:000183 44F400            MOVE              #50000,X0               ; Delay by 500 milliseconds
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 12



                            00C350
619       P:000185 P:000185 06C400            DO      X0,L_DELAY
                            000188
620       P:000187 P:000187 06E8A3            REP     #1000
621       P:000188 P:000188 000000            NOP
622                                 L_DELAY
623       P:000189 P:000189 57F400            MOVE              #$020002,B              ; Normal reply after booting is 'SYR'
                            020002
624       P:00018B P:00018B 0D00EB            JSR     <XMT_WRD
625       P:00018C P:00018C 57F400            MOVE              #'SYR',B
                            535952
626       P:00018E P:00018E 0D00EB            JSR     <XMT_WRD
627    
628       P:00018F P:00018F 0C0054            JMP     <START                            ; Start normal command processing
629    
630                                 ; *******************  DSP  INITIALIZATION  CODE  **********************
631                                 ; This code initializes the DSP right after booting, and is overwritten
632                                 ;   by application code
633       P:000190 P:000190 08F4BD  INIT      MOVEP             #PLL_INIT,X:PCTL        ; Initialize PLL to 100 MHz
                            050003
634       P:000192 P:000192 000000            NOP
635    
636                                 ; Set operation mode register OMR to normal expanded
637       P:000193 P:000193 0500BA            MOVEC             #$0000,OMR              ; Operating Mode Register = Normal Expanded
638       P:000194 P:000194 0500BB            MOVEC             #0,SP                   ; Reset the Stack Pointer SP
639    
640                                 ; Program the AA = address attribute pins
641       P:000195 P:000195 08F4B9            MOVEP             #$FFFC21,X:AAR0         ; Y = $FFF000 to $FFFFFF asserts commands
                            FFFC21
642       P:000197 P:000197 08F4B8            MOVEP             #$008909,X:AAR1         ; P = $008000 to $00FFFF accesses the EEPROM
                            008909
643       P:000199 P:000199 08F4B7            MOVEP             #$010C11,X:AAR2         ; X = $010000 to $010FFF reads A/D values
                            010C11
644       P:00019B P:00019B 08F4B6            MOVEP             #$080621,X:AAR3         ; Y = $080000 to $0BFFFF R/W from SRAM
                            080621
645    
646                                 ; Program the DRAM memory access and addressing
647       P:00019D P:00019D 08F4BB            MOVEP             #$028FE1,X:BCR          ; Bus Control Register
                            028FE1
648    
649                                 ; Program the Host port B for parallel I/O
650       P:00019F P:00019F 08F484            MOVEP             #>1,X:HPCR              ; All pins enabled as GPIO
                            000001
651       P:0001A1 P:0001A1 08F489            MOVEP             #$810C,X:HDR
                            00810C
652       P:0001A3 P:0001A3 08F488            MOVEP             #$B10E,X:HDDR           ; Data Direction Register
                            00B10E
653                                                                                     ;  (1 for Output, 0 for Input)
654    
655                                 ; Port B conversion from software bits to schematic labels
656                                 ;       PB0 = PWROK             PB08 = PRSFIFO*
657                                 ;       PB1 = LED1              PB09 = EF*
658                                 ;       PB2 = LVEN              PB10 = EXT-IN0
659                                 ;       PB3 = HVEN              PB11 = EXT-IN1
660                                 ;       PB4 = STATUS0           PB12 = EXT-OUT0
661                                 ;       PB5 = STATUS1           PB13 = EXT-OUT1
662                                 ;       PB6 = STATUS2           PB14 = SSFHF*
663                                 ;       PB7 = STATUS3           PB15 = SELSCI
664    
665                                 ; Program the serial port ESSI0 = Port C for serial communication with
666                                 ;   the utility board
667       P:0001A5 P:0001A5 07F43F            MOVEP             #>0,X:PCRC              ; Software reset of ESSI0
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 13



                            000000
668       P:0001A7 P:0001A7 07F435            MOVEP             #$180809,X:CRA0         ; Divide 100 MHz by 20 to get 5.0 MHz
                            180809
669                                                                                     ; DC[4:0] = 0 for non-network operation
670                                                                                     ; WL0-WL2 = 3 for 24-bit data words
671                                                                                     ; SSC1 = 0 for SC1 not used
672       P:0001A9 P:0001A9 07F436            MOVEP             #$020020,X:CRB0         ; SCKD = 1 for internally generated clock
                            020020
673                                                                                     ; SCD2 = 0 so frame sync SC2 is an output
674                                                                                     ; SHFD = 0 for MSB shifted first
675                                                                                     ; FSL = 0, frame sync length not used
676                                                                                     ; CKP = 0 for rising clock edge transitions
677                                                                                     ; SYN = 0 for asynchronous
678                                                                                     ; TE0 = 1 to enable transmitter #0
679                                                                                     ; MOD = 0 for normal, non-networked mode
680                                                                                     ; TE0 = 0 to NOT enable transmitter #0 yet
681                                                                                     ; RE = 1 to enable receiver
682       P:0001AB P:0001AB 07F43F            MOVEP             #%111001,X:PCRC         ; Control Register (0 for GPIO, 1 for ESSI)
                            000039
683       P:0001AD P:0001AD 07F43E            MOVEP             #%000110,X:PRRC         ; Data Direction Register (0 for In, 1 for O
ut)
                            000006
684       P:0001AF P:0001AF 07F43D            MOVEP             #%000100,X:PDRC         ; Data Register - WR_ENA* = 1
                            000004
685    
686                                 ; Port C version = Analog boards
687                                 ;       MOVEP   #$000809,X:CRA0 ; Divide 100 MHz by 20 to get 5.0 MHz
688                                 ;       MOVEP   #$000030,X:CRB0 ; SCKD = 1 for internally generated clock
689                                 ;       MOVEP   #%100000,X:PCRC ; Control Register (0 for GPIO, 1 for ESSI)
690                                 ;       MOVEP   #%000100,X:PRRC ; Data Direction Register (0 for In, 1 for Out)
691                                 ;       MOVEP   #%000000,X:PDRC ; Data Register: 'not used' = 0 outputs
692    
693       P:0001B1 P:0001B1 07F43C            MOVEP             #0,X:TX00               ; Initialize the transmitter to zero
                            000000
694       P:0001B3 P:0001B3 000000            NOP
695       P:0001B4 P:0001B4 000000            NOP
696       P:0001B5 P:0001B5 013630            BSET    #TE,X:CRB0                        ; Enable the SSI transmitter
697    
698                                 ; Conversion from software bits to schematic labels for Port C
699                                 ;       PC0 = SC00 = UTL-T-SCK
700                                 ;       PC1 = SC01 = 2_XMT = SYNC on prototype
701                                 ;       PC2 = SC02 = WR_ENA*
702                                 ;       PC3 = SCK0 = TIM-U-SCK
703                                 ;       PC4 = SRD0 = UTL-T-STD
704                                 ;       PC5 = STD0 = TIM-U-STD
705    
706                                 ; Program the serial port ESSI1 = Port D for serial transmission to
707                                 ;   the analog boards and two parallel I/O input pins
708       P:0001B6 P:0001B6 07F42F            MOVEP             #>0,X:PCRD              ; Software reset of ESSI0
                            000000
709       P:0001B8 P:0001B8 07F425            MOVEP             #$000809,X:CRA1         ; Divide 100 MHz by 20 to get 5.0 MHz
                            000809
710                                                                                     ; DC[4:0] = 0
711                                                                                     ; WL[2:0] = ALC = 0 for 8-bit data words
712                                                                                     ; SSC1 = 0 for SC1 not used
713       P:0001BA P:0001BA 07F426            MOVEP             #$000030,X:CRB1         ; SCKD = 1 for internally generated clock
                            000030
714                                                                                     ; SCD2 = 1 so frame sync SC2 is an output
715                                                                                     ; SHFD = 0 for MSB shifted first
716                                                                                     ; CKP = 0 for rising clock edge transitions
717                                                                                     ; TE0 = 0 to NOT enable transmitter #0 yet
718                                                                                     ; MOD = 0 so its not networked mode
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 14



719       P:0001BC P:0001BC 07F42F            MOVEP             #%100000,X:PCRD         ; Control Register (0 for GPIO, 1 for ESSI)
                            000020
720                                                                                     ; PD3 = SCK1, PD5 = STD1 for ESSI
721       P:0001BE P:0001BE 07F42E            MOVEP             #%000100,X:PRRD         ; Data Direction Register (0 for In, 1 for O
ut)
                            000004
722       P:0001C0 P:0001C0 07F42D            MOVEP             #%000100,X:PDRD         ; Data Register: 'not used' = 0 outputs
                            000004
723       P:0001C2 P:0001C2 07F42C            MOVEP             #0,X:TX10               ; Initialize the transmitter to zero
                            000000
724       P:0001C4 P:0001C4 000000            NOP
725       P:0001C5 P:0001C5 000000            NOP
726       P:0001C6 P:0001C6 012630            BSET    #TE,X:CRB1                        ; Enable the SSI transmitter
727    
728                                 ; Conversion from software bits to schematic labels for Port D
729                                 ; PD0 = SC10 = 2_XMT_? input
730                                 ; PD1 = SC11 = SSFEF* input
731                                 ; PD2 = SC12 = PWR_EN
732                                 ; PD3 = SCK1 = TIM-A-SCK
733                                 ; PD4 = SRD1 = PWRRST
734                                 ; PD5 = STD1 = TIM-A-STD
735    
736                                 ; Program the SCI port to communicate with the utility board
737       P:0001C7 P:0001C7 07F41C            MOVEP             #$0B04,X:SCR            ; SCI programming: 11-bit asynchronous
                            000B04
738                                                                                     ;   protocol (1 start, 8 data, 1 even parity
,
739                                                                                     ;   1 stop); LSB before MSB; enable receiver
740                                                                                     ;   and its interrupts; transmitter interrup
ts
741                                                                                     ;   disabled.
742       P:0001C9 P:0001C9 07F41B            MOVEP             #$0003,X:SCCR           ; SCI clock: utility board data rate =
                            000003
743                                                                                     ;   (390,625 kbits/sec); internal clock.
744       P:0001CB P:0001CB 07F41F            MOVEP             #%011,X:PCRE            ; Port Control Register = RXD, TXD enabled
                            000003
745       P:0001CD P:0001CD 07F41E            MOVEP             #%000,X:PRRE            ; Port Direction Register (0 = Input)
                            000000
746    
747                                 ;       PE0 = RXD
748                                 ;       PE1 = TXD
749                                 ;       PE2 = SCLK
750    
751                                 ; Program one of the three timers as an exposure timer
752       P:0001CF P:0001CF 07F403            MOVEP             #$C34F,X:TPLR           ; Prescaler to generate millisecond timer,
                            00C34F
753                                                                                     ;  counting from the system clock / 2 = 50 M
Hz
754       P:0001D1 P:0001D1 07F40F            MOVEP             #$208200,X:TCSR0        ; Clear timer complete bit and enable presca
ler
                            208200
755       P:0001D3 P:0001D3 07F40E            MOVEP             #0,X:TLR0               ; Timer load register
                            000000
756    
757                                 ; Enable interrupts for the SCI port only
758       P:0001D5 P:0001D5 08F4BF            MOVEP             #$000000,X:IPRC         ; No interrupts allowed
                            000000
759       P:0001D7 P:0001D7 08F4BE            MOVEP             #>$80,X:IPRP            ; Enable SCI interrupt only, IPR = 1
                            000080
760       P:0001D9 P:0001D9 00FCB8            ANDI    #$FC,MR                           ; Unmask all interrupt levels
761    
762                                 ; Initialize the fiber optic serial receiver circuitry
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 15



763       P:0001DA P:0001DA 061480            DO      #20,L_FO_INIT
                            0001DF
764       P:0001DC P:0001DC 5FF000            MOVE                          Y:RDFO,B
                            FFFFF1
765       P:0001DE P:0001DE 0605A0            REP     #5
766       P:0001DF P:0001DF 000000            NOP
767                                 L_FO_INIT
768    
769                                 ; Pulse PRSFIFO* low to revive the CMDRST* instruction and reset the FIFO
770       P:0001E0 P:0001E0 44F400            MOVE              #1000000,X0             ; Delay by 10 milliseconds
                            0F4240
771       P:0001E2 P:0001E2 06C400            DO      X0,*+3
                            0001E4
772       P:0001E4 P:0001E4 000000            NOP
773       P:0001E5 P:0001E5 0A8908            BCLR    #8,X:HDR
774       P:0001E6 P:0001E6 0614A0            REP     #20
775       P:0001E7 P:0001E7 000000            NOP
776       P:0001E8 P:0001E8 0A8928            BSET    #8,X:HDR
777    
778                                 ; Reset the utility board
779       P:0001E9 P:0001E9 0A0F05            BCLR    #5,X:<LATCH
780       P:0001EA P:0001EA 09F0B5            MOVEP             X:LATCH,Y:WRLATCH       ; Clear reset utility board bit
                            00000F
781       P:0001EC P:0001EC 06C8A0            REP     #200                              ; Delay by RESET* low time
782       P:0001ED P:0001ED 000000            NOP
783       P:0001EE P:0001EE 0A0F25            BSET    #5,X:<LATCH
784       P:0001EF P:0001EF 09F0B5            MOVEP             X:LATCH,Y:WRLATCH       ; Clear reset utility board bit
                            00000F
785       P:0001F1 P:0001F1 56F400            MOVE              #200000,A               ; Delay 2 msec for utility boot
                            030D40
786       P:0001F3 P:0001F3 06CE00            DO      A,*+3
                            0001F5
787       P:0001F5 P:0001F5 000000            NOP
788    
789                                 ; Put all the analog switch inputs to low so they draw minimum current
790       P:0001F6 P:0001F6 012F23            BSET    #3,X:PCRD                         ; Turn the serial clock on
791       P:0001F7 P:0001F7 56F400            MOVE              #$0C3000,A              ; Value of integrate speed and gain switches
                            0C3000
792       P:0001F9 P:0001F9 20001B            CLR     B
793       P:0001FA P:0001FA 241000            MOVE              #$100000,X0             ; Increment over board numbers for DAC write
s
794       P:0001FB P:0001FB 45F400            MOVE              #$001000,X1             ; Increment over board numbers for WRSS writ
es
                            001000
795       P:0001FD P:0001FD 060F80            DO      #15,L_ANALOG                      ; Fifteen video processor boards maximum
                            000205
796       P:0001FF P:0001FF 0D020C            JSR     <XMIT_A_WORD                      ; Transmit A to TIM-A-STD
797       P:000200 P:000200 200040            ADD     X0,A
798       P:000201 P:000201 5F7000            MOVE                          B,Y:WRSS    ; This is for the fast analog switches
                            FFFFF3
799       P:000203 P:000203 0620A3            REP     #800                              ; Delay for the serial data transmission
800       P:000204 P:000204 000000            NOP
801       P:000205 P:000205 200068            ADD     X1,B                              ; Increment the video and clock driver numbe
rs
802                                 L_ANALOG
803       P:000206 P:000206 0A0F00            BCLR    #CDAC,X:<LATCH                    ; Enable clearing of DACs
804       P:000207 P:000207 0A0F02            BCLR    #ENCK,X:<LATCH                    ; Disable clock and DAC output switches
805       P:000208 P:000208 09F0B5            MOVEP             X:LATCH,Y:WRLATCH       ; Execute these two operations
                            00000F
806       P:00020A P:00020A 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
807       P:00020B P:00020B 0C0223            JMP     <SKIP
808    
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 16



809                                 ; Transmit contents of accumulator A1 over the synchronous serial transmitter
810                                 XMIT_A_WORD
811       P:00020C P:00020C 07F42C            MOVEP             #0,X:TX10               ; This helps, don't know why
                            000000
812       P:00020E P:00020E 547000            MOVE              A1,X:SV_A1
                            00000E
813       P:000210 P:000210 000000            NOP
814       P:000211 P:000211 01A786            JCLR    #TDE,X:SSISR1,*                   ; Start bit
                            000211
815       P:000213 P:000213 07F42C            MOVEP             #$010000,X:TX10
                            010000
816       P:000215 P:000215 060380            DO      #3,L_X
                            00021B
817       P:000217 P:000217 01A786            JCLR    #TDE,X:SSISR1,*                   ; Three data bytes
                            000217
818       P:000219 P:000219 04CCCC            MOVEP             A1,X:TX10
819       P:00021A P:00021A 0C1E90            LSL     #8,A
820       P:00021B P:00021B 000000            NOP
821                                 L_X
822       P:00021C P:00021C 01A786            JCLR    #TDE,X:SSISR1,*                   ; Zeroes to bring transmitter low
                            00021C
823       P:00021E P:00021E 07F42C            MOVEP             #0,X:TX10
                            000000
824       P:000220 P:000220 54F000            MOVE              X:SV_A1,A1
                            00000E
825       P:000222 P:000222 00000C            RTS
826    
827                                 SKIP
828    
829                                 ; Set up the circular SCI buffer, 32 words in size
830       P:000223 P:000223 64F400            MOVE              #SCI_TABLE,R4
                            000400
831       P:000225 P:000225 051FA4            MOVE              #31,M4
832       P:000226 P:000226 647000            MOVE              R4,X:(SCI_TABLE+33)
                            000421
833    
834                                           IF      @SCP("HOST","ROM")
842                                           ENDIF
843    
844       P:000228 P:000228 44F400            MOVE              #>$AC,X0
                            0000AC
845       P:00022A P:00022A 440100            MOVE              X0,X:<FO_HDR
846    
847       P:00022B P:00022B 0C0181            JMP     <STARTUP
848    
849                                 ;  ****************  X: Memory tables  ********************
850    
851                                 ; Define the address in P: space where the table of constants begins
852    
853                                  X_BOOT_START
854       00022A                              EQU     @LCV(L)-2
855    
856                                           IF      @SCP("HOST","ROM")
858                                           ENDIF
859                                           IF      @SCP("HOST","HOST")
860       X:000000 X:000000                   ORG     X:0,X:0
861                                           ENDIF
862    
863                                 ; Special storage area - initialization constants and scratch space
864       X:000000 X:000000         STATUS    DC      $1064                             ; Controller status bits
865    
866       000001                    FO_HDR    EQU     STATUS+1                          ; Fiber optic write bytes
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 17



867       000005                    HEADER    EQU     FO_HDR+4                          ; Command header
868       000006                    NWORDS    EQU     HEADER+1                          ; Number of words in the command
869       000007                    COM_BUF   EQU     NWORDS+1                          ; Command buffer
870       00000E                    SV_A1     EQU     COM_BUF+7                         ; Save accumulator A1
871    
872                                           IF      @SCP("HOST","ROM")
877                                           ENDIF
878    
879                                           IF      @SCP("HOST","HOST")
880       X:00000F X:00000F                   ORG     X:$F,X:$F
881                                           ENDIF
882    
883                                 ; Parameter table in P: space to be copied into X: space during
884                                 ;   initialization, and is copied from ROM by the DSP boot
885       X:00000F X:00000F         LATCH     DC      $7A                               ; Starting value in latch chip U25
886                                  EXPOSURE_TIME
887       X:000010 X:000010                   DC      0                                 ; Exposure time in milliseconds
888                                  ELAPSED_TIME
889       X:000011 X:000011                   DC      0                                 ; Time elapsed so far in the exposure
890       X:000012 X:000012         ONE       DC      1                                 ; One
891       X:000013 X:000013         TWO       DC      2                                 ; Two
892       X:000014 X:000014         THREE     DC      3                                 ; Three
893       X:000015 X:000015         SEVEN     DC      7                                 ; Seven
894       X:000016 X:000016         MASK1     DC      $FCFCF8                           ; Mask for checking header
895       X:000017 X:000017         MASK2     DC      $030300                           ; Mask for checking header
896       X:000018 X:000018         DONE      DC      'DON'                             ; Standard reply
897       X:000019 X:000019         SBRD      DC      $020000                           ; Source Identification number
898       X:00001A X:00001A         TIM_DRB   DC      $000200                           ; Destination = timing board number
899       X:00001B X:00001B         DMASK     DC      $00FF00                           ; Mask to get destination board number
900       X:00001C X:00001C         SMASK     DC      $FF0000                           ; Mask to get source board number
901       X:00001D X:00001D         ERR       DC      'ERR'                             ; An error occurred
902       X:00001E X:00001E         C100K     DC      100000                            ; Delay for WRROM = 1 millisec
903       X:00001F X:00001F         IDL_ADR   DC      TST_RCV                           ; Address of idling routine
904       X:000020 X:000020         EXP_ADR   DC      0                                 ; Jump to this address during exposures
905    
906    
907                                 ; Places for saving register values
908       X:000021 X:000021         SAVE_SR   DC      0                                 ; Status Register
909       X:000022 X:000022         SAVE_X1   DC      0
910       X:000023 X:000023         SAVE_A1   DC      0
911       X:000024 X:000024         SAVE_R0   DC      0
912       X:000025 X:000025         RCV_ERR   DC      0
913       X:000026 X:000026         SCI_A1    DC      0                                 ; Contents of accumulator A1 in RCV ISR
914       X:000027 X:000027         SCI_R0    DC      SRXL
915    
916                                 ; Command table
917       000028                    COM_TBL_R EQU     @LCV(R)
918       X:000028 X:000028         COM_TBL   DC      'TDL',TDL                         ; Test Data Link
919       X:00002A X:00002A                   DC      'RDM',RDMEM                       ; Read from DSP or EEPROM memory
920       X:00002C X:00002C                   DC      'WRM',WRMEM                       ; Write to DSP memory
921       X:00002E X:00002E                   DC      'LDA',LDAPPL                      ; Load application from EEPROM to DSP
922       X:000030 X:000030                   DC      'STP',STOP_IDLE_CLOCKING
923       X:000032 X:000032                   DC      'DON',START                       ; Nothing special
924       X:000034 X:000034                   DC      'ERR',START                       ; Nothing special
925    
926                                  END_COMMAND_TABLE
927       000036                              EQU     @LCV(R)
928    
929                                 ; The table at SCI_TABLE is for words received from the utility board, written by
930                                 ;   the interrupt service routine SCI_RCV. Note that it is 32 words long,
931                                 ;   hard coded, and the 33rd location contains the pointer to words that have
932                                 ;   been processed by moving them from the SCI_TABLE to the COM_BUF.
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timboot.asm  Page 18



933    
934                                           IF      @SCP("HOST","ROM")
936                                           ENDIF
937    
938       000036                    END_ADR   EQU     @LCV(L)                           ; End address of P: code written to ROM
939    
940       P:00022C P:00022C                   ORG     P:,P:
941    
942       001DB4                    CC        EQU     CCDVIDREV5+TIMREV5+TEMP_POLY+UTILREV3+SPLIT_SERIAL+SUBARRAY+BINNING+SHUTTER_CC
943    
944                                 ; Put number of words of application in P: for loading application from EEPROM
945       P:00022C P:00022C                   DC      TIMBOOT_X_MEMORY-@LCV(L)-1
946    
947                                 ; Define CLOCK as a macro to produce in-line code to reduce execution time
948                                 CLOCK     MACRO
949  m                                        JCLR    #SSFHF,X:HDR,*                    ; Don't overfill the WRSS FIFO
950  m                                        REP     Y:(R0)+                           ; Repeat # of times at address Y:(R0)+
951  m                                        MOVEP   Y:(R0)+,Y:WRSS                    ; Write the waveform to the FIFO
952  m                                        ENDM
953    
954                                 ; Set software to IDLE mode
955                                 START_IDLE_CLOCKING
956       P:00022D P:00022D 60F400            MOVE              #IDLE,R0
                            000233
957       P:00022F P:00022F 000000            NOP
958       P:000230 P:000230 601F00            MOVE              R0,X:<IDL_ADR
959       P:000231 P:000231 0A0022            BSET    #IDLMODE,X:<STATUS                ; Idle after readout
960       P:000232 P:000232 0C008F            JMP     <FINISH                           ; Need to send header and 'DON'
961    
962                                 ; Keep the CCD idling when not reading out
963                                 IDLE
964       P:000233 P:000233 5EA300            MOVE                          Y:<NSRI,A   ; NSERIALS_READ = NSR
965       P:000234 P:000234 0A0085            JCLR    #SPLIT_S,X:STATUS,*+3
                            000237
966       P:000236 P:000236 200022            ASR     A                                 ; Split serials requires / 2
967       P:000237 P:000237 000000            NOP
968    
969       P:000238 P:000238 06CE00            DO      A,IDL1                            ; Loop over number of pixels per line
                            000245
970       P:00023A P:00023A 68F000            MOVE                          Y:SERIAL_IDLE,R0 ; Serial transfer on pixel
                            000014
971                                           CLOCK                                     ; Go to it
975       P:000240 P:000240 330700            MOVE              #COM_BUF,R3
976       P:000241 P:000241 0D00A5            JSR     <GET_RCV                          ; Check for FO or SSI commands
977       P:000242 P:000242 0E0245            JCC     <NO_COM                           ; Continue IDLE if no commands received
978       P:000243 P:000243 00008C            ENDDO
979       P:000244 P:000244 0C005D            JMP     <PRC_RCV                          ; Go process header and command
980       P:000245 P:000245 000000  NO_COM    NOP
981                                 IDL1
982       P:000246 P:000246 061D40            DO      Y:<N_PARALLEL_CLEARS,PAR
                            00024C
983       P:000248 P:000248 689100            MOVE                          Y:<PARALLEL_CLEAR,R0 ; Address of parallel clocking wave
form
984                                           CLOCK                                     ; Go clock out the CCD charge
988                                 PAR
989       P:00024D P:00024D 0C0233            JMP     <IDLE
990    
991    
992                                 ;  *****************  Exposure and readout routines  *****************
993    
994                                 ; Overall loop - transfer and read NPR lines
995                                 RDCCD
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  tim.asm  Page 19



996       P:00024E P:00024E 0B00AD            JSSET   #ST_SYNC,X:STATUS,SYNCH_CONTROLLER ; Sync up two controllers
                            000778
997    
998                                 ; Calculate some readout parameters
999       P:000250 P:000250 5EAA00            MOVE                          Y:<NBOXES,A ; NBOXES = 0 => full image readout
1000      P:000251 P:000251 000000            NOP
1001      P:000252 P:000252 200003            TST     A
1002      P:000253 P:000253 0E225E            JNE     <SUB_IMG
1003      P:000254 P:000254 5C2700            MOVE                          A1,Y:<NP_SKIP ; Zero these all out
1004      P:000255 P:000255 5C2800            MOVE                          A1,Y:<NS_SKP1
1005      P:000256 P:000256 5C2900            MOVE                          A1,Y:<NS_SKP2
1006      P:000257 P:000257 5E8100            MOVE                          Y:<NSR,A    ; NSERIALS_READ = NSR
1007      P:000258 P:000258 0A0085            JCLR    #SPLIT_S,X:STATUS,*+3
                            00025B
1008      P:00025A P:00025A 200022            ASR     A                                 ; Split serials requires / 2
1009      P:00025B P:00025B 000000            NOP
1010      P:00025C P:00025C 5E0A00            MOVE                          A,Y:<NSERIALS_READ ; Number of columns in each subimage
1011      P:00025D P:00025D 0C0274            JMP     <WT_CLK
1012   
1013                                ; Loop over the required number of subimage boxes
1014      P:00025E P:00025E 67F400  SUB_IMG   MOVE              #READ_TABLE,R7          ; Parameter table for subimage readout
                            00002E
1015      P:000260 P:000260 062A40            DO      Y:<NBOXES,L_NBOXES                ; Loop over number of boxes
                            0002DA
1016      P:000262 P:000262 4CDF00            MOVE                          Y:(R7)+,X0
1017      P:000263 P:000263 4C2700            MOVE                          X0,Y:<NP_SKIP
1018      P:000264 P:000264 4CDF00            MOVE                          Y:(R7)+,X0
1019      P:000265 P:000265 4D8500            MOVE                          Y:<NSBIN,X1 ; Multiply by serial binning number
1020      P:000266 P:000266 2000A0            MPY     X0,X1,A
1021      P:000267 P:000267 200022            ASR     A
1022      P:000268 P:000268 582800            MOVE                          A0,Y:<NS_SKP1
1023      P:000269 P:000269 4CDF00            MOVE                          Y:(R7)+,X0
1024      P:00026A P:00026A 4D8500            MOVE                          Y:<NSBIN,X1 ; Multiply by serial binning number
1025      P:00026B P:00026B 2000A0            MPY     X0,X1,A
1026      P:00026C P:00026C 200022            ASR     A
1027      P:00026D P:00026D 582900            MOVE                          A0,Y:<NS_SKP2
1028      P:00026E P:00026E 5EAC00            MOVE                          Y:<NS_READ,A
1029      P:00026F P:00026F 0A0085            JCLR    #SPLIT_S,X:STATUS,*+3             ; Split serials require / 2
                            000272
1030      P:000271 P:000271 200022            ASR     A
1031      P:000272 P:000272 000000            NOP
1032      P:000273 P:000273 5E0A00            MOVE                          A,Y:<NSERIALS_READ ; Number of columns in each subimage
1033   
1034                                ; Start the loop for parallel shifting desired number of lines
1035                                ;WT_CLK JSR     <GENERATE_SERIAL_WAVEFORM
1036      P:000274 P:000274 000000  WT_CLK    NOP                                       ; not using the binning waveform generation 
at the moment.
1037      P:000275 P:000275 0D043B            JSR     <WAIT_TO_FINISH_CLOCKING
1038   
1039                                ; Skip over the required number of rows for subimage readout
1040      P:000276 P:000276 5EA700            MOVE                          Y:<NP_SKIP,A ; Number of rows to skip
1041      P:000277 P:000277 200003            TST     A
1042      P:000278 P:000278 0EA283            JEQ     <CLR_SR
1043      P:000279 P:000279 062740            DO      Y:<NP_SKIP,L_PSKP
                            000282
1044      P:00027B P:00027B 060640            DO      Y:<NPBIN,L_PSKIP
                            000281
1045      P:00027D P:00027D 301000            MOVE              #<PARALLEL,R0           ; Couldn't this be above the start of the do
 loop?
1046                                          CLOCK
1050      P:000282 P:000282 000000  L_PSKIP   NOP
1051                                L_PSKP
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  tim.asm  Page 20



1052   
1053                                ; Clear out the accumulated charge from the serial shift register
1054      P:000283 P:000283 060340  CLR_SR    DO      Y:<NSCLR,L_CLRSR                  ; Loop over number of pixels to skip
                            000289
1055      P:000285 P:000285 688F00            MOVE                          Y:<SERIAL_SKIP,R0
1056                                          CLOCK                                     ; Go clock out the CCD charge
1060                                L_CLRSR                                             ; Do loop restriction
1061   
1062                                ; Parallel shift the image into the serial shift register
1063      P:00028A P:00028A 5E8200            MOVE                          Y:<NPR,A    ; Number of rows set by host computer
1064      P:00028B P:00028B 000000            NOP
1065      P:00028C P:00028C 5FAA00            MOVE                          Y:<NBOXES,B ; NBOXES = 0 => full image readout
1066      P:00028D P:00028D 20000B            TST     B
1067      P:00028E P:00028E 0EA290            JEQ     *+2
1068      P:00028F P:00028F 5EAD00            MOVE                          Y:<NP_READ,A ; If NBOXES .NE. 0 use subimage table
1069      P:000290 P:000290 000000            NOP
1070   
1071      P:000291 P:000291 0A0086            JCLR    #SPLIT_P,X:STATUS,P_CLK
                            000296
1072      P:000293 P:000293 5E8200            MOVE                          Y:<NPR,A
1073      P:000294 P:000294 200022            ASR     A
1074      P:000295 P:000295 000000            NOP
1075   
1076                                ; This is the main loop over each line to be read out
1077   
1078      P:000296 P:000296 06CC00  P_CLK     DO      A1,LPR                            ; Number of rows to read out
                            0002D9
1079   
1080                                ; Exercise the parallel clocks, including binning if needed
1081      P:000298 P:000298 060640            DO      Y:<NPBIN,L_PBIN
                            00029E
1082      P:00029A P:00029A 689000            MOVE                          Y:<PARALLEL,R0
1083                                          CLOCK
1087                                L_PBIN
1088   
1089                                ; Check for a command once per line. Only the ABORT command should be issued.
1090      P:00029F P:00029F 330700            MOVE              #COM_BUF,R3
1091      P:0002A0 P:0002A0 0D00A5            JSR     <GET_RCV                          ; Was a command received?
1092      P:0002A1 P:0002A1 0E02AB            JCC     <CONTINUE_READ                    ; If no, continue reading out
1093      P:0002A2 P:0002A2 0C005D            JMP     <PRC_RCV                          ; If yes, go process it
1094   
1095                                ; Abort the readout currently underway
1096      P:0002A3 P:0002A3 0A0084  ABR_RDC   JCLR    #ST_RDC,X:<STATUS,ABORT_EXPOSURE
                            0003E9
1097      P:0002A5 P:0002A5 00008C            ENDDO                                     ; Properly terminate readout loop
1098      P:0002A6 P:0002A6 5EAA00            MOVE                          Y:<NBOXES,A ; NBOXES = 0 => full image readout
1099      P:0002A7 P:0002A7 200003            TST     A
1100      P:0002A8 P:0002A8 0EA2AA            JEQ     *+2
1101      P:0002A9 P:0002A9 00008C            ENDDO                                     ; Properly terminate readout loop
1102      P:0002AA P:0002AA 0C03E9            JMP     <ABORT_EXPOSURE
1103   
1104                                ; Skip over NS_SKP1 columns for subimage readout
1105                                CONTINUE_READ
1106      P:0002AB P:0002AB 5EA800            MOVE                          Y:<NS_SKP1,A ; Number of columns to skip
1107      P:0002AC P:0002AC 200003            TST     A
1108      P:0002AD P:0002AD 0EF2B5            JLE     <L_READ
1109      P:0002AE P:0002AE 062840            DO      Y:<NS_SKP1,L_SKP1                 ; Number of waveform entries total
                            0002B4
1110      P:0002B0 P:0002B0 688F00            MOVE                          Y:<SERIAL_SKIP,R0 ; Waveform table starting address
1111                                          CLOCK                                     ; Go clock out the CCD charge
1115                                L_SKP1
1116   
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  tim.asm  Page 21



1117                                ; Finally read some real pixels
1118      P:0002B5 P:0002B5 060A40  L_READ    DO      Y:<NSERIALS_READ,L_RD
                            0002BB
1119      P:0002B7 P:0002B7 688E00            MOVE                          Y:<SERIAL_READ,R0
1120                                          CLOCK                                     ; Go clock out the CCD charge
1124                                L_RD
1125   
1126                                ; Skip over NS_SKP2 columns if needed for subimage readout
1127      P:0002BC P:0002BC 5EA900            MOVE                          Y:<NS_SKP2,A ; Number of columns to skip
1128      P:0002BD P:0002BD 200003            TST     A
1129      P:0002BE P:0002BE 0EF2C6            JLE     <L_BIAS
1130      P:0002BF P:0002BF 062940            DO      Y:<NS_SKP2,L_SKP2
                            0002C5
1131      P:0002C1 P:0002C1 688F00            MOVE                          Y:<SERIAL_SKIP,R0 ; Waveform table starting address
1132                                          CLOCK                                     ; Go clock out the CCD charge
1136                                L_SKP2
1137   
1138                                ; And read the bias pixels if in subimage readout mode
1139      P:0002C6 P:0002C6 5EAA00  L_BIAS    MOVE                          Y:<NBOXES,A ; NBOXES = 0 => full image readout
1140      P:0002C7 P:0002C7 200003            TST     A
1141      P:0002C8 P:0002C8 0EF2D9            JLE     <END_ROW
1142      P:0002C9 P:0002C9 5EAB00            MOVE                          Y:<NR_BIAS,A ; NR_BIAS = 0 => no bias pixels
1143      P:0002CA P:0002CA 200003            TST     A
1144      P:0002CB P:0002CB 0EF2D9            JLE     <END_ROW
1145      P:0002CC P:0002CC 0A0085            JCLR    #SPLIT_S,X:STATUS,*+3
                            0002CF
1146      P:0002CE P:0002CE 200022            ASR     A                                 ; Split serials require / 2
1147      P:0002CF P:0002CF 000000            NOP
1148      P:0002D0 P:0002D0 06CC00            DO      A1,L_BRD                          ; Number of pixels to read out
                            0002D7
1149      P:0002D2 P:0002D2 60F400            MOVE              #PXL_TBL,R0
                            00020A
1150                                          CLOCK                                     ; Go clock out the CCD charg
1154      P:0002D8 P:0002D8 000000  L_BRD     NOP
1155      P:0002D9 P:0002D9 000000  END_ROW   NOP
1156      P:0002DA P:0002DA 000000  LPR       NOP                                       ; End of parallel loop
1157      P:0002DB P:0002DB 000000  L_NBOXES  NOP                                       ; End of subimage boxes loop
1158   
1159                                ; Restore the controller to non-image data transfer and idling if necessary
1160      P:0002DC P:0002DC 0A0082  RDC_END   JCLR    #IDLMODE,X:<STATUS,NO_IDL         ; Don't idle after readout
                            0002E2
1161      P:0002DE P:0002DE 60F400            MOVE              #IDLE,R0
                            000233
1162      P:0002E0 P:0002E0 601F00            MOVE              R0,X:<IDL_ADR
1163      P:0002E1 P:0002E1 0C02E4            JMP     <RDC_E
1164      P:0002E2 P:0002E2 305A00  NO_IDL    MOVE              #TST_RCV,R0
1165      P:0002E3 P:0002E3 601F00            MOVE              R0,X:<IDL_ADR
1166      P:0002E4 P:0002E4 0D043B  RDC_E     JSR     <WAIT_TO_FINISH_CLOCKING
1167      P:0002E5 P:0002E5 0A0004            BCLR    #ST_RDC,X:<STATUS                 ; Set status to not reading out
1168      P:0002E6 P:0002E6 0C0054            JMP     <START
1169   
1170                                ; ******  Include many routines not directly needed for readout  *******
1171                                          INCLUDE "timCCDmisc.asm"
1172                                ; Miscellaneous CCD control routines, common to all detector types
1173   
1174                                ; test to see if the power is on on the backplane.
1175                                POWER_CHECK
1176                                CHECK_LVEN
1177      P:0002E7 P:0002E7 0A8982            JCLR    #LVEN,X:HDR,CHECK_HVEN
                            0002E9
1178                                CHECK_HVEN
1179      P:0002E9 P:0002E9 0A8983            JCLR    #HVEN,X:HDR,PWR_CHECK_OK
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 22



                            0002ED
1180      P:0002EB P:0002EB 270000            MOVE              #0,Y1                   ; send a 1 back if the power is on
1181      P:0002EC P:0002EC 0C0090            JMP     <FINISH1
1182                                                                                    ; jump here is power is ok
1183                                PWR_CHECK_OK
1184      P:0002ED P:0002ED 270100            MOVE              #1,Y1                   ; send a 1 back if the power is on
1185      P:0002EE P:0002EE 0C0090            JMP     <FINISH1
1186   
1187                                POWER_OFF
1188      P:0002EF P:0002EF 0D0337            JSR     <CLEAR_SWITCHES                   ; Clear all analog switches
1189      P:0002F0 P:0002F0 0A8922            BSET    #LVEN,X:HDR
1190      P:0002F1 P:0002F1 0A8923            BSET    #HVEN,X:HDR
1191      P:0002F2 P:0002F2 0A000F            BCLR    #POWERST,X:<STATUS                ; Set the power state in the X: status word
1192      P:0002F3 P:0002F3 0C008F            JMP     <FINISH
1193   
1194                                ; Execute the power-on cycle, as a command
1195                                POWER_ON
1196      P:0002F4 P:0002F4 0D0337            JSR     <CLEAR_SWITCHES                   ; Clear all analog switches
1197      P:0002F5 P:0002F5 0D0309            JSR     <PON                              ; Turn on the power control board
1198      P:0002F6 P:0002F6 0A8980            JCLR    #PWROK,X:HDR,PWR_ERR              ; Test if the power turned on properly
                            000306
1199      P:0002F8 P:0002F8 0D031C            JSR     <SET_BIASES                       ; Turn on the DC bias supplies
1200      P:0002F9 P:0002F9 0D0563            JSR     <SEL_OS                           ; Set up readout parameters
1201      P:0002FA P:0002FA 60F400            MOVE              #IDLE,R0                ; Put controller in IDLE state
                            000233
1202                                ;       MOVE    #TST_RCV,R0             ; Put controller in non-IDLE state
1203      P:0002FC P:0002FC 601F00            MOVE              R0,X:<IDL_ADR
1204      P:0002FD P:0002FD 0A002F            BSET    #POWERST,X:<STATUS                ; Set the power state bit in the X: status w
ord
1205   
1206                                                                                    ; get the gain setting and put it into the a
ppropriate place in Y memory
1207      P:0002FE P:0002FE 5EF000            MOVE                          Y:GAIN_SETTING,A
                            000201
1208      P:000300 P:000300 240D00            MOVE              #$0D0000,X0
1209      P:000301 P:000301 200044            SUB     X0,A
1210      P:000302 P:000302 000000            NOP
1211      P:000303 P:000303 5E0000            MOVE                          A,Y:<GAIN
1212   
1213                                ; !!!   MOVE    #$1064,X0
1214                                ; !!!   MOVE    X0,X:<STATUS
1215                                ; !!!   JSR     <SEL_OS
1216      P:000304 P:000304 0A002F            BSET    #POWERST,X:<STATUS                ; Set the power state bit in the X: status w
ord
1217      P:000305 P:000305 0C008F            JMP     <FINISH
1218   
1219                                ; The power failed to turn on because of an error on the power control board
1220      P:000306 P:000306 0A8922  PWR_ERR   BSET    #LVEN,X:HDR                       ; Turn off the low voltage enable line
1221      P:000307 P:000307 0A8923            BSET    #HVEN,X:HDR                       ; Turn off the high voltage enable line
1222      P:000308 P:000308 0C008D            JMP     <ERROR
1223   
1224                                ; As a subroutine, turn on the low voltages (+/- 6.5V, +/- 16.5V) and delay
1225      P:000309 P:000309 0A8902  PON       BCLR    #LVEN,X:HDR                       ; Set these signals to DSP outputs
1226      P:00030A P:00030A 44F400            MOVE              #2000000,X0
                            1E8480
1227      P:00030C P:00030C 06C400            DO      X0,*+3                            ; Wait 20 millisec for settling
                            00030E
1228      P:00030E P:00030E 000000            NOP
1229   
1230                                ; Turn on the high +36 volt power line and then delay
1231      P:00030F P:00030F 0A8903            BCLR    #HVEN,X:HDR                       ; HVEN = Low => Turn on +36V
1232      P:000310 P:000310 44F400            MOVE              #10000000,X0
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 23



                            989680
1233      P:000312 P:000312 06C400            DO      X0,*+3                            ; Wait 100 millisec for settling
                            000314
1234      P:000314 P:000314 000000            NOP
1235      P:000315 P:000315 00000C            RTS
1236   
1237   
1238                                RAW_COMMAND
1239      P:000316 P:000316 012F23            BSET    #3,X:PCRD                         ; Turn on the serial clock
1240   
1241      P:000317 P:000317 56DB00            MOVE              X:(R3)+,A               ; Get the command which should just be a wor
d
1242      P:000318 P:000318 0D020C            JSR     <XMIT_A_WORD                      ; Transmit A to TIM-A-STD
1243      P:000319 P:000319 0D043E            JSR     <PAL_DLY                          ; Wait for the number to be sent
1244      P:00031A P:00031A 012F03            BCLR    #3,X:PCRD                         ; Turn off the serial clock
1245      P:00031B P:00031B 0C008F            JMP     <FINISH
1246   
1247                                ; Set all the DC bias voltages and video processor offset values, reading
1248                                ;   them from the 'DACS' table
1249                                SET_BIASES
1250      P:00031C P:00031C 012F23            BSET    #3,X:PCRD                         ; Turn on the serial clock
1251      P:00031D P:00031D 0A0F01            BCLR    #1,X:<LATCH                       ; Separate updates of clock driver
1252      P:00031E P:00031E 0A0F20            BSET    #CDAC,X:<LATCH                    ; Disable clearing of DACs
1253      P:00031F P:00031F 0A0F22            BSET    #ENCK,X:<LATCH                    ; Enable clock and DAC output switches
1254      P:000320 P:000320 09F0B5            MOVEP             X:LATCH,Y:WRLATCH       ; Write it to the hardware
                            00000F
1255      P:000322 P:000322 0D043E            JSR     <PAL_DLY                          ; Delay for all this to happen
1256   
1257                                ; Read DAC values from a table, and write them to the DACs
1258      P:000323 P:000323 60F400            MOVE              #DACS,R0                ; Get starting address of DAC values
                            0001C1
1259      P:000325 P:000325 000000            NOP
1260      P:000326 P:000326 000000            NOP
1261      P:000327 P:000327 065840            DO      Y:(R0)+,L_DAC                     ; Repeat Y:(R0)+ times
                            00032B
1262      P:000329 P:000329 5ED800            MOVE                          Y:(R0)+,A   ; Read the table entry
1263      P:00032A P:00032A 0D020C            JSR     <XMIT_A_WORD                      ; Transmit it to TIM-A-STD
1264      P:00032B P:00032B 000000            NOP
1265                                L_DAC
1266   
1267                                ; Let the DAC voltages all ramp up before exiting
1268      P:00032C P:00032C 44F400            MOVE              #400000,X0
                            061A80
1269      P:00032E P:00032E 06C400            DO      X0,*+3                            ; 4 millisec delay
                            000330
1270      P:000330 P:000330 000000            NOP
1271      P:000331 P:000331 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
1272      P:000332 P:000332 00000C            RTS
1273   
1274                                SET_BIAS_VOLTAGES
1275      P:000333 P:000333 0D031C            JSR     <SET_BIASES
1276      P:000334 P:000334 0C008F            JMP     <FINISH
1277   
1278      P:000335 P:000335 0D0337  CLR_SWS   JSR     <CLEAR_SWITCHES
1279      P:000336 P:000336 0C008F            JMP     <FINISH
1280   
1281                                ; Clear all video processor analog switches to lower their power dissipation
1282                                CLEAR_SWITCHES
1283      P:000337 P:000337 012F23            BSET    #3,X:PCRD                         ; Turn the serial clock on
1284      P:000338 P:000338 56F400            MOVE              #$0C3000,A              ; Value of integrate speed and gain switches
                            0C3000
1285      P:00033A P:00033A 20001B            CLR     B
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 24



1286      P:00033B P:00033B 241000            MOVE              #$100000,X0             ; Increment over board numbers for DAC write
s
1287      P:00033C P:00033C 45F400            MOVE              #$001000,X1             ; Increment over board numbers for WRSS writ
es
                            001000
1288      P:00033E P:00033E 060F80            DO      #15,L_VIDEO                       ; Fifteen video processor boards maximum
                            000345
1289      P:000340 P:000340 0D020C            JSR     <XMIT_A_WORD                      ; Transmit A to TIM-A-STD
1290      P:000341 P:000341 200040            ADD     X0,A
1291      P:000342 P:000342 5F7000            MOVE                          B,Y:WRSS
                            FFFFF3
1292      P:000344 P:000344 0D043E            JSR     <PAL_DLY                          ; Delay for the serial data transmission
1293      P:000345 P:000345 200068            ADD     X1,B
1294                                L_VIDEO
1295      P:000346 P:000346 0A0F00            BCLR    #CDAC,X:<LATCH                    ; Enable clearing of DACs
1296      P:000347 P:000347 0A0F02            BCLR    #ENCK,X:<LATCH                    ; Disable clock and DAC output switches
1297      P:000348 P:000348 09F0B5            MOVEP             X:LATCH,Y:WRLATCH       ; Execute these two operations
                            00000F
1298      P:00034A P:00034A 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
1299      P:00034B P:00034B 00000C            RTS
1300   
1301                                SET_SHUTTER_STATE
1302      P:00034C P:00034C 568F00            MOVE              X:LATCH,A
1303      P:00034D P:00034D 0140C6            AND     #$FFEF,A
                            00FFEF
1304      P:00034F P:00034F 200042            OR      X0,A
1305      P:000350 P:000350 000000            NOP
1306      P:000351 P:000351 540F00            MOVE              A1,X:LATCH
1307      P:000352 P:000352 09CC35            MOVEP             A1,Y:WRLATCH
1308      P:000353 P:000353 00000C            RTS
1309   
1310                                ; Open the shutter from the timing board, executed as a command
1311                                OPEN_SHUTTER
1312      P:000354 P:000354 0A0023            BSET    #ST_SHUT,X:<STATUS                ; Set status bit to mean shutter open
1313      P:000355 P:000355 240000            MOVE              #0,X0
1314      P:000356 P:000356 0D034C            JSR     <SET_SHUTTER_STATE
1315      P:000357 P:000357 0C008F            JMP     <FINISH
1316   
1317                                ; Close the shutter from the timing board, executed as a command
1318                                CLOSE_SHUTTER
1319      P:000358 P:000358 0A0003            BCLR    #ST_SHUT,X:<STATUS                ; Clear status to mean shutter closed
1320      P:000359 P:000359 44F400            MOVE              #>$10,X0
                            000010
1321      P:00035B P:00035B 0D034C            JSR     <SET_SHUTTER_STATE
1322      P:00035C P:00035C 0C008F            JMP     <FINISH
1323   
1324                                ; Shutter subroutines
1325      P:00035D P:00035D 0A0023  OSHUT     BSET    #ST_SHUT,X:<STATUS                ; Set status bit to mean shutter open
1326      P:00035E P:00035E 240000            MOVE              #0,X0
1327      P:00035F P:00035F 0D034C            JSR     <SET_SHUTTER_STATE
1328      P:000360 P:000360 00000C            RTS
1329   
1330      P:000361 P:000361 0A0003  CSHUT     BCLR    #ST_SHUT,X:<STATUS                ; Clear status to mean shutter closed
1331      P:000362 P:000362 44F400            MOVE              #>$10,X0
                            000010
1332      P:000364 P:000364 0D034C            JSR     <SET_SHUTTER_STATE
1333      P:000365 P:000365 00000C            RTS
1334   
1335                                ; Clear the CCD, executed as a command
1336      P:000366 P:000366 0D0368  CLEAR     JSR     <CLR_CCD
1337      P:000367 P:000367 0C008F            JMP     <FINISH
1338   
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 25



1339                                ; Default clearing routine with serial clocks inactive
1340                                ; Fast clear image before each exposure, executed as a subroutine
1341      P:000368 P:000368 060440  CLR_CCD   DO      Y:<NPCLR,LPCLR2                   ; Loop over number of lines in image
                            000378
1342      P:00036A P:00036A 689100            MOVE                          Y:<PARALLEL_CLEAR,R0 ; Address of parallel transfer wave
form
1343                                          CLOCK
1347      P:00036F P:00036F 0A8989            JCLR    #EF,X:HDR,LPCLR1                  ; Simple test for fast execution
                            000378
1348      P:000371 P:000371 330700            MOVE              #COM_BUF,R3
1349      P:000372 P:000372 0D00A5            JSR     <GET_RCV                          ; Check for FO command
1350      P:000373 P:000373 0E0378            JCC     <LPCLR1                           ; Continue no commands received
1351   
1352      P:000374 P:000374 60F400            MOVE              #LPCLR1,R0
                            000378
1353      P:000376 P:000376 601F00            MOVE              R0,X:<IDL_ADR
1354      P:000377 P:000377 0C005D            JMP     <PRC_RCV
1355      P:000378 P:000378 000000  LPCLR1    NOP
1356                                LPCLR2
1357      P:000379 P:000379 330700            MOVE              #COM_BUF,R3
1358      P:00037A P:00037A 0D00A5            JSR     <GET_RCV                          ; Check for FO command
1359      P:00037B P:00037B 00000C            RTS
1360   
1361                                ;CLR_FS  DO  #NP_FS,FSCLR2
1362                                ;  MOVE  Y:<CLEAR_FS,R0
1363                                ;  CLOCK
1364                                ;  JCLR  #EF,X:HDR,FSCLR1
1365                                ;  MOVE  #COM_BUF,R3
1366                                ;  JSR   <GET_RCV
1367                                ;  JCC   <FSCLR1
1368                                ;
1369                                ;  MOVE  #FSCLR1,R0
1370                                ;  MOVE  R0,X:<IDL_ADR
1371                                ;  JMP   <PRC_RCV
1372                                ;FSCLR1  NOP
1373                                ;FSCLR2
1374                                ;  MOVE  #COM_BUF,R3
1375                                ;  JSR   <GET_RCV
1376                                ;  RTS
1377   
1378                                ; Start the exposure timer and monitor its progress
1379      P:00037C P:00037C 0B00AD  EXPOSE    JSSET   #ST_SYNC,X:STATUS,SYNCH_CONTROLLER ; Sync up two controllers
                            000778
1380      P:00037E P:00037E 07F40E            MOVEP             #0,X:TLR0               ; Load 0 into counter timer
                            000000
1381      P:000380 P:000380 240000            MOVE              #0,X0
1382      P:000381 P:000381 441100            MOVE              X0,X:<ELAPSED_TIME      ; Set elapsed exposure time to zero
1383      P:000382 P:000382 579000            MOVE              X:<EXPOSURE_TIME,B
1384      P:000383 P:000383 20000B            TST     B                                 ; Special test for zero exposure time
1385      P:000384 P:000384 0EA390            JEQ     <END_EXP                          ; Don't even start an exposure
1386      P:000385 P:000385 01418C            SUB     #1,B                              ; Timer counts from X:TCPR0+1 to zero
1387      P:000386 P:000386 010F20            BSET    #TIM_BIT,X:TCSR0                  ; Enable the timer #0
1388      P:000387 P:000387 577000            MOVE              B,X:TCPR0
                            FFFF8D
1389      P:000389 P:000389 0A8989  CHK_RCV   JCLR    #EF,X:HDR,CHK_TIM                 ; Simple test for fast execution
                            00038E
1390      P:00038B P:00038B 330700            MOVE              #COM_BUF,R3             ; The beginning of the command buffer
1391      P:00038C P:00038C 0D00A5            JSR     <GET_RCV                          ; Check for an incoming command
1392      P:00038D P:00038D 0E805D            JCS     <PRC_RCV                          ; If command is received, go check it
1393      P:00038E P:00038E 018F95  CHK_TIM   JCLR    #TCF,X:TCSR0,CHK_RCV              ; Wait for timer to equal compare value
                            000389
1394      P:000390 P:000390 010F00  END_EXP   BCLR    #TIM_BIT,X:TCSR0                  ; Disable the timer
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 26



1395      P:000391 P:000391 0AE780            JMP     (R7)                              ; This contains the return address
1396   
1397                                ; Start the exposure, operate the shutter, and initiate the CCD readout
1398                                START_EXPOSURE
1399      P:000392 P:000392 57F400            MOVE              #$020102,B
                            020102
1400      P:000394 P:000394 0D00EB            JSR     <XMT_WRD
1401      P:000395 P:000395 57F400            MOVE              #'IIA',B                ; Initialize the PCI image address
                            494941
1402      P:000397 P:000397 0D00EB            JSR     <XMT_WRD
1403      P:000398 P:000398 0B0088            JSCLR   #NOT_CLR,X:STATUS,CLR_CCD         ; Jump to clear out routine if bit set
                            000368
1404      P:00039A P:00039A 330700            MOVE              #COM_BUF,R3             ; The beginning of the command buffer
1405      P:00039B P:00039B 0D00A5            JSR     <GET_RCV                          ; Check for FO command
1406      P:00039C P:00039C 0E805D            JCS     <PRC_RCV                          ; Process the command
1407      P:00039D P:00039D 305A00            MOVE              #TST_RCV,R0             ; Process commands during the exposure
1408      P:00039E P:00039E 601F00            MOVE              R0,X:<IDL_ADR
1409      P:00039F P:00039F 0D043B            JSR     <WAIT_TO_FINISH_CLOCKING
1410   
1411                                ; Operate the shutter if needed and begin exposure
1412      P:0003A0 P:0003A0 0A008B            JCLR    #SHUT,X:STATUS,L_SEX0
                            0003A3
1413      P:0003A2 P:0003A2 0D035D            JSR     <OSHUT                            ; Open the shutter if needed
1414      P:0003A3 P:0003A3 67F400  L_SEX0    MOVE              #L_SEX1,R7              ; Return address at end of exposure
                            0003AF
1415   
1416                                ; delay to ensure controllers are synced
1417      P:0003A5 P:0003A5 44F400            MOVE              #100000,X0
                            0186A0
1418      P:0003A7 P:0003A7 066480            DO      #100,SHUTTER_SYNC0                ; Delay by Y:SHDEL milliseconds
                            0003AC
1419      P:0003A9 P:0003A9 06C400            DO      X0,SHUTTER_SYNC1
                            0003AB
1420      P:0003AB P:0003AB 000000            NOP
1421                                 SHUTTER_SYNC1
1422      P:0003AC P:0003AC 000000            NOP
1423                                 SHUTTER_SYNC0
1424      P:0003AD P:0003AD 000000            NOP
1425   
1426      P:0003AE P:0003AE 0C037C            JMP     <EXPOSE                           ; Delay for specified exposure time
1427                                L_SEX1
1428   
1429                                ; Now we really start the CCD readout, alerting the PCI board, closing the
1430                                ;  shutter, waiting for it to close and then reading out
1431      P:0003AF P:0003AF 0D042E  STR_RDC   JSR     <PCI_READ_IMAGE                   ; Get the PCI board reading the image
1432      P:0003B0 P:0003B0 0A0024            BSET    #ST_RDC,X:<STATUS                 ; Set status to reading out
1433      P:0003B1 P:0003B1 0A008B            JCLR    #SHUT,X:STATUS,TST_SYN
                            0003B4
1434      P:0003B3 P:0003B3 0D0361            JSR     <CSHUT                            ; Close the shutter if necessary
1435      P:0003B4 P:0003B4 0A00AA  TST_SYN   JSET    #TST_IMG,X:STATUS,SYNTHETIC_IMAGE
                            0003F9
1436   
1437                                ; Delay readout until the shutter has fully closed
1438      P:0003B6 P:0003B6 5E8800            MOVE                          Y:<SHDEL,A
1439      P:0003B7 P:0003B7 200003            TST     A
1440      P:0003B8 P:0003B8 0EF3C1            JLE     <S_DEL0
1441      P:0003B9 P:0003B9 44F400            MOVE              #100000,X0
                            0186A0
1442      P:0003BB P:0003BB 06CE00            DO      A,S_DEL0                          ; Delay by Y:SHDEL milliseconds
                            0003C0
1443      P:0003BD P:0003BD 06C400            DO      X0,S_DEL1
                            0003BF
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 27



1444      P:0003BF P:0003BF 000000            NOP
1445      P:0003C0 P:0003C0 000000  S_DEL1    NOP
1446      P:0003C1 P:0003C1 000000  S_DEL0    NOP
1447   
1448      P:0003C2 P:0003C2 0C024E            JMP     <RDCCD                            ; Finally, go read out the CCD
1449   
1450                                ;TEST_READ_AD
1451                                ;       DO Y:NPR,END_PARALLEL
1452                                ;       DO Y:NSR,END_SERIAL
1453                                ;       JSR <READ_AD
1454                                ;       NOP
1455                                ;END_SERIAL
1456                                ;        NOP
1457                                ;END_PARALLEL
1458   
1459                                ;RDC_ENDT       JCLR    #IDLMODE,X:<STATUS,NO_IDLT ; Don't idle after readout
1460                                ;       MOVE    #IDLE,R0
1461                                ;       MOVE    R0,X:<IDL_ADR
1462                                ;       JMP     <RDC_ET
1463                                ;NO_IDLT        MOVE    #TST_RCV,R0
1464                                ;       MOVE    R0,X:<IDL_ADR
1465                                ;RDC_ET JSR     <WAIT_TO_FINISH_CLOCKING
1466                                ;       BCLR    #ST_RDC,X:<STATUS       ; Set status to not reading out
1467                                ;        JMP     <START
1468   
1469                                TEST_AD
1470      P:0003C3 P:0003C3 57F000            MOVE              X:(RDAD+1),B
                            010001
1471      P:0003C5 P:0003C5 0C1DA1            ASL     #16,B,B
1472      P:0003C6 P:0003C6 000000            NOP
1473      P:0003C7 P:0003C7 216500            MOVE              B2,X1
1474      P:0003C8 P:0003C8 0C1D91            ASL     #8,B,B
1475      P:0003C9 P:0003C9 000000            NOP
1476      P:0003CA P:0003CA 216400            MOVE              B2,X0
1477      P:0003CB P:0003CB 000000            NOP
1478      P:0003CC P:0003CC 20A700            MOVE              X1,Y1
1479      P:0003CD P:0003CD 208600            MOVE              X0,Y0
1480      P:0003CE P:0003CE 4F1700            MOVE                          Y1,Y:<BYTE_1
1481      P:0003CF P:0003CF 4E1800            MOVE                          Y0,Y:<BYTE_2
1482   
1483      P:0003D0 P:0003D0 0C008F            JMP     <FINISH
1484   
1485                                ; Set the desired exposure time
1486                                SET_EXPOSURE_TIME
1487      P:0003D1 P:0003D1 46DB00            MOVE              X:(R3)+,Y0
1488      P:0003D2 P:0003D2 461000            MOVE              Y0,X:EXPOSURE_TIME
1489      P:0003D3 P:0003D3 04C68D            MOVEP             Y0,X:TCPR0
1490      P:0003D4 P:0003D4 0C008F            JMP     <FINISH
1491   
1492                                ; Read the time remaining until the exposure ends
1493                                READ_EXPOSURE_TIME
1494      P:0003D5 P:0003D5 018FA0            JSET    #TIM_BIT,X:TCSR0,RD_TIM           ; Read DSP timer if its running
                            0003D9
1495      P:0003D7 P:0003D7 479100            MOVE              X:<ELAPSED_TIME,Y1
1496      P:0003D8 P:0003D8 0C0090            JMP     <FINISH1
1497      P:0003D9 P:0003D9 47F000  RD_TIM    MOVE              X:TCR0,Y1               ; Read elapsed exposure time
                            FFFF8C
1498      P:0003DB P:0003DB 0C0090            JMP     <FINISH1
1499   
1500                                ; Pause the exposure - close the shutter and stop the timer
1501                                PAUSE_EXPOSURE
1502      P:0003DC P:0003DC 07700C            MOVEP             X:TCR0,X:ELAPSED_TIME   ; Save the elapsed exposure time
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 28



                            000011
1503      P:0003DE P:0003DE 010F00            BCLR    #TIM_BIT,X:TCSR0                  ; Disable the DSP exposure timer
1504      P:0003DF P:0003DF 0D0361            JSR     <CSHUT                            ; Close the shutter
1505      P:0003E0 P:0003E0 0C008F            JMP     <FINISH
1506   
1507                                ; Resume the exposure - open the shutter if needed and restart the timer
1508                                RESUME_EXPOSURE
1509      P:0003E1 P:0003E1 010F29            BSET    #TRM,X:TCSR0                      ; To be sure it will load TLR0
1510      P:0003E2 P:0003E2 07700C            MOVEP             X:TCR0,X:TLR0           ; Restore elapsed exposure time
                            FFFF8E
1511      P:0003E4 P:0003E4 010F20            BSET    #TIM_BIT,X:TCSR0                  ; Re-enable the DSP exposure timer
1512      P:0003E5 P:0003E5 0A008B            JCLR    #SHUT,X:STATUS,L_RES
                            0003E8
1513      P:0003E7 P:0003E7 0D035D            JSR     <OSHUT                            ; Open the shutter if necessary
1514      P:0003E8 P:0003E8 0C008F  L_RES     JMP     <FINISH
1515   
1516                                ; Abort exposure - close the shutter, stop the timer and resume idle mode
1517                                ABORT_EXPOSURE
1518      P:0003E9 P:0003E9 0D0361            JSR     <CSHUT                            ; Close the shutter
1519      P:0003EA P:0003EA 010F00            BCLR    #TIM_BIT,X:TCSR0                  ; Disable the DSP exposure timer
1520      P:0003EB P:0003EB 0A0082            JCLR    #IDLMODE,X:<STATUS,NO_IDL2        ; Don't idle after readout
                            0003F1
1521      P:0003ED P:0003ED 60F400            MOVE              #IDLE,R0
                            000233
1522      P:0003EF P:0003EF 601F00            MOVE              R0,X:<IDL_ADR
1523      P:0003F0 P:0003F0 0C03F3            JMP     <RDC_E2
1524      P:0003F1 P:0003F1 305A00  NO_IDL2   MOVE              #TST_RCV,R0
1525      P:0003F2 P:0003F2 601F00            MOVE              R0,X:<IDL_ADR
1526      P:0003F3 P:0003F3 0D043B  RDC_E2    JSR     <WAIT_TO_FINISH_CLOCKING
1527      P:0003F4 P:0003F4 0A0004            BCLR    #ST_RDC,X:<STATUS                 ; Set status to not reading out
1528      P:0003F5 P:0003F5 06A08F            DO      #4000,*+3                         ; Wait 40 microsec for the fiber
                            0003F7
1529      P:0003F7 P:0003F7 000000            NOP                                       ;  optic to clear out
1530      P:0003F8 P:0003F8 0C008F            JMP     <FINISH
1531                                ; Generate a synthetic image by simply incrementing the pixel counts
1532                                SYNTHETIC_IMAGE
1533                                ;       JSR     <PCI_READ_IMAGE         ; Get the PCI board reading the image
1534      P:0003F9 P:0003F9 0A0024            BSET    #ST_RDC,X:<STATUS                 ; Set status to reading out
1535      P:0003FA P:0003FA 0D043E            JSR     <PAL_DLY
1536      P:0003FB P:0003FB 200013            CLR     A
1537      P:0003FC P:0003FC 060240            DO      Y:<NPR,LPR_TST                    ; Loop over each line readout
                            000407
1538      P:0003FE P:0003FE 060140            DO      Y:<NSR,LSR_TST                    ; Loop over number of pixels per line
                            000406
1539      P:000400 P:000400 0614A0            REP     #20                               ; #20 => 1.0 microsec per pixel
1540      P:000401 P:000401 000000            NOP
1541      P:000402 P:000402 014180            ADD     #1,A                              ; Pixel data = Pixel data + 1
1542      P:000403 P:000403 000000            NOP
1543      P:000404 P:000404 21CF00            MOVE              A,B
1544      P:000405 P:000405 0D0409            JSR     <XMT_PIX                          ;  transmit them
1545      P:000406 P:000406 000000            NOP
1546                                LSR_TST
1547      P:000407 P:000407 000000            NOP
1548                                LPR_TST
1549      P:000408 P:000408 0C02DC            JMP     <RDC_END                          ; Normal exit
1550   
1551                                ; Transmit the 16-bit pixel datum in B1 to the host computer
1552      P:000409 P:000409 0C1DA1  XMT_PIX   ASL     #16,B,B
1553      P:00040A P:00040A 000000            NOP
1554      P:00040B P:00040B 216500            MOVE              B2,X1
1555      P:00040C P:00040C 0C1D91            ASL     #8,B,B
1556      P:00040D P:00040D 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 29



1557      P:00040E P:00040E 216400            MOVE              B2,X0
1558      P:00040F P:00040F 000000            NOP
1559      P:000410 P:000410 09C532            MOVEP             X1,Y:WRFO
1560      P:000411 P:000411 09C432            MOVEP             X0,Y:WRFO
1561      P:000412 P:000412 00000C            RTS
1562   
1563                                ; Test the hardware to read A/D values directly into the DSP instead
1564                                ;   of using the SXMIT option, A/Ds #2 and 3.
1565      P:000413 P:000413 57F000  READ_AD   MOVE              X:(RDAD+2),B
                            010002
1566      P:000415 P:000415 0C1DA1            ASL     #16,B,B
1567      P:000416 P:000416 000000            NOP
1568      P:000417 P:000417 216500            MOVE              B2,X1
1569      P:000418 P:000418 0C1D91            ASL     #8,B,B
1570      P:000419 P:000419 000000            NOP
1571      P:00041A P:00041A 216400            MOVE              B2,X0
1572      P:00041B P:00041B 000000            NOP
1573      P:00041C P:00041C 09C532            MOVEP             X1,Y:WRFO
1574      P:00041D P:00041D 09C432            MOVEP             X0,Y:WRFO
1575      P:00041E P:00041E 060AA0            REP     #10
1576      P:00041F P:00041F 000000            NOP
1577      P:000420 P:000420 57F000            MOVE              X:(RDAD+3),B
                            010003
1578      P:000422 P:000422 0C1DA1            ASL     #16,B,B
1579      P:000423 P:000423 000000            NOP
1580      P:000424 P:000424 216500            MOVE              B2,X1
1581      P:000425 P:000425 0C1D91            ASL     #8,B,B
1582      P:000426 P:000426 000000            NOP
1583      P:000427 P:000427 216400            MOVE              B2,X0
1584      P:000428 P:000428 000000            NOP
1585      P:000429 P:000429 09C532            MOVEP             X1,Y:WRFO
1586      P:00042A P:00042A 09C432            MOVEP             X0,Y:WRFO
1587      P:00042B P:00042B 060AA0            REP     #10
1588      P:00042C P:00042C 000000            NOP
1589      P:00042D P:00042D 00000C            RTS
1590   
1591                                ; Alert the PCI interface board that images are coming soon
1592                                PCI_READ_IMAGE
1593      P:00042E P:00042E 57F400            MOVE              #$020104,B              ; Send header word to the FO xmtr
                            020104
1594      P:000430 P:000430 0D00EB            JSR     <XMT_WRD
1595      P:000431 P:000431 57F400            MOVE              #'RDA',B
                            524441
1596      P:000433 P:000433 0D00EB            JSR     <XMT_WRD
1597      P:000434 P:000434 5FF000            MOVE                          Y:NSR,B     ; Number of columns to read
                            000001
1598      P:000436 P:000436 0D00EB            JSR     <XMT_WRD
1599      P:000437 P:000437 5FF000            MOVE                          Y:NPR,B     ; Number of rows to read
                            000002
1600      P:000439 P:000439 0D00EB            JSR     <XMT_WRD
1601      P:00043A P:00043A 00000C            RTS
1602   
1603                                ; Wait for the clocking to be complete before proceeding
1604                                WAIT_TO_FINISH_CLOCKING
1605      P:00043B P:00043B 01ADA1            JSET    #SSFEF,X:PDRD,*                   ; Wait for the SS FIFO to be empty
                            00043B
1606      P:00043D P:00043D 00000C            RTS
1607   
1608                                ; Delay for serial writes to the PALs and DACs by 8 microsec
1609      P:00043E P:00043E 062083  PAL_DLY   DO      #800,*+3                          ; Wait 8 usec for serial data xmit
                            000440
1610      P:000440 P:000440 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 30



1611      P:000441 P:000441 00000C            RTS
1612   
1613                                ; Let the host computer read the controller configuration
1614                                READ_CONTROLLER_CONFIGURATION
1615      P:000442 P:000442 4F8900            MOVE                          Y:<CONFIG,Y1 ; Just transmit the configuration
1616      P:000443 P:000443 0C0090            JMP     <FINISH1
1617   
1618                                ; Set the video processor gain and integrator speed for all video boards
1619                                ;                                         #SPEED = 0 for slow, 1 for fast
1620      P:000444 P:000444 012F23  ST_GAIN   BSET    #3,X:PCRD                         ; Turn on the serial clock
1621      P:000445 P:000445 56DB00            MOVE              X:(R3)+,A               ; Gain value (1,2,5 or 10)
1622      P:000446 P:000446 44F400            MOVE              #>1,X0
                            000001
1623      P:000448 P:000448 20001B            CLR     B
1624   
1625      P:000449 P:000449 060F80            DO      #15,CHK_GAIN
                            00044E
1626      P:00044B P:00044B 200005            CMP     B,A
1627      P:00044C P:00044C 0EA450            JEQ     <STG_A
1628      P:00044D P:00044D 200048            ADD     X0,B
1629      P:00044E P:00044E 000000            NOP
1630                                CHK_GAIN
1631      P:00044F P:00044F 0C045A            JMP     <ERR_SGN
1632   
1633      P:000450 P:000450 5E0000  STG_A     MOVE                          A,Y:<GAIN   ; Store the GAIN value for later use
1634      P:000451 P:000451 240D00            MOVE              #$0D0000,X0
1635      P:000452 P:000452 200042            OR      X0,A
1636      P:000453 P:000453 000000            NOP
1637   
1638                                ; Send this same value to 15 video processor boards whether they exist or not
1639                                ;       MOVE    #$100000,X0     ; Increment value
1640                                ;       DO      #15,STG_LOOP
1641                                ; DO NOT LOOP!  You will send out a word which is 0x2D000x where x is the
1642                                ; gain settings.  This will set one of the reset drain voltages to ~0V and
1643                                ; so mess up the CCD depletion.  You can't set the HV board address to get
1644                                ; around this...  If you end up with more than one video card in a box
1645                                ; then you'll have to manually set this.
1646      P:000454 P:000454 0D020C            JSR     <XMIT_A_WORD                      ; Transmit A to TIM-A-STD
1647      P:000455 P:000455 0D043E            JSR     <PAL_DLY                          ; Wait for SSI and PAL to be empty
1648      P:000456 P:000456 200040            ADD     X0,A                              ; Increment the video processor board number
1649      P:000457 P:000457 000000            NOP
1650                                STG_LOOP
1651      P:000458 P:000458 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
1652      P:000459 P:000459 0C008F            JMP     <FINISH
1653   
1654      P:00045A P:00045A 56DB00  ERR_SGN   MOVE              X:(R3)+,A
1655      P:00045B P:00045B 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
1656      P:00045C P:00045C 0C008D            JMP     <ERROR
1657   
1658                                ; Set a particular DAC numbers, for setting DC bias voltages, clock driver
1659                                ;   voltages and video processor offset
1660                                ; This is code for the ARC32 clock driver and ARC45 CCD video processor
1661                                ;
1662                                ; SBN  #BOARD  #DAC  ['CLK' or 'VID'] voltage
1663                                ;
1664                                ;                               #BOARD is from 0 to 15
1665                                ;                               #DAC number
1666                                ;                               #voltage is from 0 to 4095
1667   
1668                                SET_BIAS_NUMBER                                     ; Set bias number
1669      P:00045D P:00045D 012F23            BSET    #3,X:PCRD                         ; Turn on the serial clock
1670      P:00045E P:00045E 56DB00            MOVE              X:(R3)+,A               ; First argument is board number, 0 to 15
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 31



1671      P:00045F P:00045F 0614A0            REP     #20
1672      P:000460 P:000460 200033            LSL     A
1673      P:000461 P:000461 000000            NOP
1674      P:000462 P:000462 21C500            MOVE              A,X1                    ; Save the board number
1675      P:000463 P:000463 56DB00            MOVE              X:(R3)+,A               ; Second argument is DAC number
1676      P:000464 P:000464 57E300            MOVE              X:(R3),B                ; Third argument is 'VID' or 'CLK' string
1677      P:000465 P:000465 0140CD            CMP     #'VID',B
                            564944
1678      P:000467 P:000467 0E246F            JNE     <CLK_DRV
1679      P:000468 P:000468 060EA0            REP     #14
1680      P:000469 P:000469 200033            LSL     A
1681      P:00046A P:00046A 000000            NOP
1682      P:00046B P:00046B 0ACC73            BSET    #19,A1                            ; Set bits to mean video processor DAC
1683      P:00046C P:00046C 000000            NOP
1684      P:00046D P:00046D 0ACC72            BSET    #18,A1
1685      P:00046E P:00046E 0C0499            JMP     <BD_SET
1686      P:00046F P:00046F 0140CD  CLK_DRV   CMP     #'CLK',B
                            434C4B
1687      P:000471 P:000471 0E24AE            JNE     <ERR_SBN
1688   
1689                                ; For ARC32 do some trickiness to set the chip select and address bits
1690      P:000472 P:000472 218F00            MOVE              A1,B
1691      P:000473 P:000473 060EA0            REP     #14
1692      P:000474 P:000474 200033            LSL     A
1693      P:000475 P:000475 240E00            MOVE              #$0E0000,X0
1694      P:000476 P:000476 200046            AND     X0,A
1695      P:000477 P:000477 44F400            MOVE              #>7,X0
                            000007
1696      P:000479 P:000479 20004E            AND     X0,B                              ; Get 3 least significant bits of clock #
1697      P:00047A P:00047A 01408D            CMP     #0,B
1698      P:00047B P:00047B 0E247E            JNE     <CLK_1
1699      P:00047C P:00047C 0ACE68            BSET    #8,A
1700      P:00047D P:00047D 0C0499            JMP     <BD_SET
1701      P:00047E P:00047E 01418D  CLK_1     CMP     #1,B
1702      P:00047F P:00047F 0E2482            JNE     <CLK_2
1703      P:000480 P:000480 0ACE69            BSET    #9,A
1704      P:000481 P:000481 0C0499            JMP     <BD_SET
1705      P:000482 P:000482 01428D  CLK_2     CMP     #2,B
1706      P:000483 P:000483 0E2486            JNE     <CLK_3
1707      P:000484 P:000484 0ACE6A            BSET    #10,A
1708      P:000485 P:000485 0C0499            JMP     <BD_SET
1709      P:000486 P:000486 01438D  CLK_3     CMP     #3,B
1710      P:000487 P:000487 0E248A            JNE     <CLK_4
1711      P:000488 P:000488 0ACE6B            BSET    #11,A
1712      P:000489 P:000489 0C0499            JMP     <BD_SET
1713      P:00048A P:00048A 01448D  CLK_4     CMP     #4,B
1714      P:00048B P:00048B 0E248E            JNE     <CLK_5
1715      P:00048C P:00048C 0ACE6D            BSET    #13,A
1716      P:00048D P:00048D 0C0499            JMP     <BD_SET
1717      P:00048E P:00048E 01458D  CLK_5     CMP     #5,B
1718      P:00048F P:00048F 0E2492            JNE     <CLK_6
1719      P:000490 P:000490 0ACE6E            BSET    #14,A
1720      P:000491 P:000491 0C0499            JMP     <BD_SET
1721      P:000492 P:000492 01468D  CLK_6     CMP     #6,B
1722      P:000493 P:000493 0E2496            JNE     <CLK_7
1723      P:000494 P:000494 0ACE6F            BSET    #15,A
1724      P:000495 P:000495 0C0499            JMP     <BD_SET
1725      P:000496 P:000496 01478D  CLK_7     CMP     #7,B
1726      P:000497 P:000497 0E2499            JNE     <BD_SET
1727      P:000498 P:000498 0ACE70            BSET    #16,A
1728   
1729      P:000499 P:000499 200062  BD_SET    OR      X1,A                              ; Add on the board number
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 32



1730      P:00049A P:00049A 000000            NOP
1731      P:00049B P:00049B 21C400            MOVE              A,X0
1732      P:00049C P:00049C 57DB00            MOVE              X:(R3)+,B               ; Third argument (again) is 'VID' or 'CLK' s
tring
1733      P:00049D P:00049D 0140CD            CMP     #'VID',B
                            564944
1734      P:00049F P:00049F 0EA4A8            JEQ     <VID
1735      P:0004A0 P:0004A0 56DB00            MOVE              X:(R3)+,A               ; Fourth argument is voltage value, 0 to $ff
f
1736      P:0004A1 P:0004A1 0604A0            REP     #4
1737      P:0004A2 P:0004A2 200023            LSR     A                                 ; Convert 12 bits to 8 bits for ARC32
1738      P:0004A3 P:0004A3 46F400            MOVE              #>$FF,Y0                ; Mask off just 8 bits
                            0000FF
1739      P:0004A5 P:0004A5 200056            AND     Y0,A
1740      P:0004A6 P:0004A6 200042            OR      X0,A
1741      P:0004A7 P:0004A7 0C04AA            JMP     <XMT_SBN
1742      P:0004A8 P:0004A8 56DB00  VID       MOVE              X:(R3)+,A               ; Fourth argument is voltage value for ARC45
, 12 bits
1743      P:0004A9 P:0004A9 200042            OR      X0,A
1744   
1745      P:0004AA P:0004AA 0D020C  XMT_SBN   JSR     <XMIT_A_WORD                      ; Transmit A to TIM-A-STD
1746      P:0004AB P:0004AB 0D043E            JSR     <PAL_DLY                          ; Wait for the number to be sent
1747      P:0004AC P:0004AC 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
1748      P:0004AD P:0004AD 0C008F            JMP     <FINISH
1749      P:0004AE P:0004AE 56DB00  ERR_SBN   MOVE              X:(R3)+,A               ; Read and discard the fourth argument
1750      P:0004AF P:0004AF 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
1751      P:0004B0 P:0004B0 0C008D            JMP     <ERROR
1752   
1753   
1754                                ; Specify the MUX value to be output on the clock driver board
1755                                ; Command syntax is  SMX  #clock_driver_board #MUX1 #MUX2
1756                                ;                               #clock_driver_board from 0 to 15
1757                                ;                               #MUX1, #MUX2 from 0 to 23
1758   
1759      P:0004B1 P:0004B1 012F23  SET_MUX   BSET    #3,X:PCRD                         ; Turn on the serial clock
1760      P:0004B2 P:0004B2 56DB00            MOVE              X:(R3)+,A               ; Clock driver board number
1761      P:0004B3 P:0004B3 0614A0            REP     #20
1762      P:0004B4 P:0004B4 200033            LSL     A
1763      P:0004B5 P:0004B5 44F400            MOVE              #$003000,X0
                            003000
1764      P:0004B7 P:0004B7 200042            OR      X0,A
1765      P:0004B8 P:0004B8 000000            NOP
1766      P:0004B9 P:0004B9 21C500            MOVE              A,X1                    ; Move here for storage
1767   
1768                                ; Get the first MUX number
1769      P:0004BA P:0004BA 56DB00            MOVE              X:(R3)+,A               ; Get the first MUX number
1770      P:0004BB P:0004BB 0AF0A9            JLT     ERR_SM1
                            0004FF
1771      P:0004BD P:0004BD 44F400            MOVE              #>24,X0                 ; Check for argument less than 32
                            000018
1772      P:0004BF P:0004BF 200045            CMP     X0,A
1773      P:0004C0 P:0004C0 0AF0A1            JGE     ERR_SM1
                            0004FF
1774      P:0004C2 P:0004C2 21CF00            MOVE              A,B
1775      P:0004C3 P:0004C3 44F400            MOVE              #>7,X0
                            000007
1776      P:0004C5 P:0004C5 20004E            AND     X0,B
1777      P:0004C6 P:0004C6 44F400            MOVE              #>$18,X0
                            000018
1778      P:0004C8 P:0004C8 200046            AND     X0,A
1779      P:0004C9 P:0004C9 0E24CC            JNE     <SMX_1                            ; Test for 0 <= MUX number <= 7
1780      P:0004CA P:0004CA 0ACD63            BSET    #3,B1
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 33



1781      P:0004CB P:0004CB 0C04D7            JMP     <SMX_A
1782      P:0004CC P:0004CC 44F400  SMX_1     MOVE              #>$08,X0
                            000008
1783      P:0004CE P:0004CE 200045            CMP     X0,A                              ; Test for 8 <= MUX number <= 15
1784      P:0004CF P:0004CF 0E24D2            JNE     <SMX_2
1785      P:0004D0 P:0004D0 0ACD64            BSET    #4,B1
1786      P:0004D1 P:0004D1 0C04D7            JMP     <SMX_A
1787      P:0004D2 P:0004D2 44F400  SMX_2     MOVE              #>$10,X0
                            000010
1788      P:0004D4 P:0004D4 200045            CMP     X0,A                              ; Test for 16 <= MUX number <= 23
1789      P:0004D5 P:0004D5 0E24FF            JNE     <ERR_SM1
1790      P:0004D6 P:0004D6 0ACD65            BSET    #5,B1
1791      P:0004D7 P:0004D7 20006A  SMX_A     OR      X1,B1                             ; Add prefix to MUX numbers
1792      P:0004D8 P:0004D8 000000            NOP
1793      P:0004D9 P:0004D9 21A700            MOVE              B1,Y1
1794   
1795                                ; Add on the second MUX number
1796      P:0004DA P:0004DA 56DB00            MOVE              X:(R3)+,A               ; Get the next MUX number
1797      P:0004DB P:0004DB 0E908D            JLT     <ERROR
1798      P:0004DC P:0004DC 44F400            MOVE              #>24,X0                 ; Check for argument less than 32
                            000018
1799      P:0004DE P:0004DE 200045            CMP     X0,A
1800      P:0004DF P:0004DF 0E108D            JGE     <ERROR
1801      P:0004E0 P:0004E0 0606A0            REP     #6
1802      P:0004E1 P:0004E1 200033            LSL     A
1803      P:0004E2 P:0004E2 000000            NOP
1804      P:0004E3 P:0004E3 21CF00            MOVE              A,B
1805      P:0004E4 P:0004E4 44F400            MOVE              #$1C0,X0
                            0001C0
1806      P:0004E6 P:0004E6 20004E            AND     X0,B
1807      P:0004E7 P:0004E7 44F400            MOVE              #>$600,X0
                            000600
1808      P:0004E9 P:0004E9 200046            AND     X0,A
1809      P:0004EA P:0004EA 0E24ED            JNE     <SMX_3                            ; Test for 0 <= MUX number <= 7
1810      P:0004EB P:0004EB 0ACD69            BSET    #9,B1
1811      P:0004EC P:0004EC 0C04F8            JMP     <SMX_B
1812      P:0004ED P:0004ED 44F400  SMX_3     MOVE              #>$200,X0
                            000200
1813      P:0004EF P:0004EF 200045            CMP     X0,A                              ; Test for 8 <= MUX number <= 15
1814      P:0004F0 P:0004F0 0E24F3            JNE     <SMX_4
1815      P:0004F1 P:0004F1 0ACD6A            BSET    #10,B1
1816      P:0004F2 P:0004F2 0C04F8            JMP     <SMX_B
1817      P:0004F3 P:0004F3 44F400  SMX_4     MOVE              #>$400,X0
                            000400
1818      P:0004F5 P:0004F5 200045            CMP     X0,A                              ; Test for 16 <= MUX number <= 23
1819      P:0004F6 P:0004F6 0E208D            JNE     <ERROR
1820      P:0004F7 P:0004F7 0ACD6B            BSET    #11,B1
1821      P:0004F8 P:0004F8 200078  SMX_B     ADD     Y1,B                              ; Add prefix to MUX numbers
1822      P:0004F9 P:0004F9 000000            NOP
1823      P:0004FA P:0004FA 21AE00            MOVE              B1,A
1824      P:0004FB P:0004FB 0D020C            JSR     <XMIT_A_WORD                      ; Transmit A to TIM-A-STD
1825      P:0004FC P:0004FC 0D043E            JSR     <PAL_DLY                          ; Delay for all this to happen
1826      P:0004FD P:0004FD 012F03            BCLR    #3,X:PCRD                         ; Turn off the serial clock
1827      P:0004FE P:0004FE 0C008F            JMP     <FINISH
1828      P:0004FF P:0004FF 56DB00  ERR_SM1   MOVE              X:(R3)+,A
1829      P:000500 P:000500 012F03            BCLR    #3,X:PCRD                         ; Turn off the serial clock
1830      P:000501 P:000501 0C008D            JMP     <ERROR
1831   
1832                                ; Specify subarray readout coordinates, one rectangle only
1833                                SET_SUBARRAY_SIZES
1834      P:000502 P:000502 200013            CLR     A
1835      P:000503 P:000503 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 34



1836      P:000504 P:000504 5E2A00            MOVE                          A,Y:<NBOXES ; Number of subimage boxes = 0 to start
1837      P:000505 P:000505 44DB00            MOVE              X:(R3)+,X0
1838      P:000506 P:000506 4C2B00            MOVE                          X0,Y:<NR_BIAS ; Number of bias pixels to read
1839      P:000507 P:000507 44DB00            MOVE              X:(R3)+,X0
1840      P:000508 P:000508 4C2C00            MOVE                          X0,Y:<NS_READ ; Number of columns in subimage read
1841      P:000509 P:000509 44DB00            MOVE              X:(R3)+,X0
1842      P:00050A P:00050A 4C2D00            MOVE                          X0,Y:<NP_READ ; Number of rows in subimage read
1843      P:00050B P:00050B 0C008F            JMP     <FINISH
1844   
1845                                ; Call this routine once for every subarray to be added to the table
1846                                SET_SUBARRAY_POSITIONS
1847      P:00050C P:00050C 4CAA00            MOVE                          Y:<NBOXES,X0
1848      P:00050D P:00050D 459400            MOVE              X:<THREE,X1
1849      P:00050E P:00050E 2000A0            MPY     X0,X1,A
1850      P:00050F P:00050F 200022            ASR     A
1851      P:000510 P:000510 210C00            MOVE              A0,A1
1852      P:000511 P:000511 44F400            MOVE              #>10,X0
                            00000A
1853      P:000513 P:000513 200045            CMP     X0,A
1854      P:000514 P:000514 0E708D            JGT     <ERROR                            ; Error if number of boxes > 10
1855      P:000515 P:000515 44F400            MOVE              #READ_TABLE,X0
                            00002E
1856      P:000517 P:000517 200040            ADD     X0,A
1857      P:000518 P:000518 000000            NOP
1858      P:000519 P:000519 219700            MOVE              A1,R7
1859      P:00051A P:00051A 44DB00            MOVE              X:(R3)+,X0
1860      P:00051B P:00051B 000000            NOP
1861      P:00051C P:00051C 000000            NOP
1862      P:00051D P:00051D 4C5F00            MOVE                          X0,Y:(R7)+  ; Number of rows (parallels) to clear
1863      P:00051E P:00051E 44DB00            MOVE              X:(R3)+,X0
1864      P:00051F P:00051F 4C5F00            MOVE                          X0,Y:(R7)+  ; Number of columns (serials) clears before
1865      P:000520 P:000520 44DB00            MOVE              X:(R3)+,X0              ;  the box readout
1866      P:000521 P:000521 4C5F00            MOVE                          X0,Y:(R7)+  ; Number of columns (serials) clears after
1867      P:000522 P:000522 5EAA00            MOVE                          Y:<NBOXES,A ;  the box readout
1868      P:000523 P:000523 449200            MOVE              X:<ONE,X0
1869      P:000524 P:000524 200040            ADD     X0,A
1870      P:000525 P:000525 000000            NOP
1871      P:000526 P:000526 5E2A00            MOVE                          A,Y:<NBOXES
1872      P:000527 P:000527 0C008F            JMP     <FINISH
1873   
1874                                ; Generate the serial readout waveform table for the chosen
1875                                ;   value of readout and serial binning.
1876   
1877                                GENERATE_SERIAL_WAVEFORM
1878      P:000528 P:000528 60F400            MOVE              #CCD_RESET,R0
                            0000E1
1879      P:00052A P:00052A 61F400            MOVE              #(PXL_TBL+1),R1
                            00020B
1880      P:00052C P:00052C 5ED800            MOVE                          Y:(R0)+,A
1881      P:00052D P:00052D 000000            NOP
1882      P:00052E P:00052E 06CC00            DO      A1,L_CCD_RESET
                            000531
1883      P:000530 P:000530 4CD800            MOVE                          Y:(R0)+,X0
1884      P:000531 P:000531 4C5900            MOVE                          X0,Y:(R1)+
1885                                L_CCD_RESET
1886   
1887                                ; Generate the first set of clocks
1888      P:000532 P:000532 68F000            MOVE                          Y:FIRST_CLOCKS,R0
                            00000C
1889      P:000534 P:000534 000000            NOP
1890      P:000535 P:000535 000000            NOP
1891      P:000536 P:000536 5ED800            MOVE                          Y:(R0)+,A
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 35



1892      P:000537 P:000537 000000            NOP
1893      P:000538 P:000538 06CC00            DO      A1,L_FIRST_CLOCKS
                            00053B
1894      P:00053A P:00053A 4CD800            MOVE                          Y:(R0)+,X0
1895      P:00053B P:00053B 4C5900            MOVE                          X0,Y:(R1)+
1896                                L_FIRST_CLOCKS
1897   
1898   
1899                                ; Generate the binning waveforms if needed
1900      P:00053C P:00053C 5E8500            MOVE                          Y:<NSBIN,A
1901      P:00053D P:00053D 014184            SUB     #1,A
1902      P:00053E P:00053E 0EF54C            JLE     <GEN_VID
1903      P:00053F P:00053F 06CC00            DO      A1,L_BIN
                            00054B
1904      P:000541 P:000541 68F000            MOVE                          Y:CLOCK_LINE,R0
                            00000D
1905      P:000543 P:000543 000000            NOP
1906      P:000544 P:000544 000000            NOP
1907      P:000545 P:000545 5ED800            MOVE                          Y:(R0)+,A
1908      P:000546 P:000546 000000            NOP
1909      P:000547 P:000547 06CC00            DO      A1,L_CLOCK_LINE
                            00054A
1910      P:000549 P:000549 4CD800            MOVE                          Y:(R0)+,X0
1911      P:00054A P:00054A 4C5900            MOVE                          X0,Y:(R1)+
1912                                L_CLOCK_LINE
1913      P:00054B P:00054B 000000            NOP
1914                                L_BIN
1915   
1916                                ; Generate the video processor waveforms
1917      P:00054C P:00054C 60F400  GEN_VID   MOVE              #VIDEO_PROCESS,R0
                            0000FE
1918      P:00054E P:00054E 000000            NOP
1919      P:00054F P:00054F 000000            NOP
1920      P:000550 P:000550 5ED800            MOVE                          Y:(R0)+,A
1921      P:000551 P:000551 000000            NOP
1922      P:000552 P:000552 06CC00            DO      A1,L_VID
                            000555
1923      P:000554 P:000554 4CD800            MOVE                          Y:(R0)+,X0
1924      P:000555 P:000555 4C5900            MOVE                          X0,Y:(R1)+
1925                                L_VID
1926   
1927                                ; Finally, calculate the number of entries in the waveform table just generated
1928      P:000556 P:000556 44F400            MOVE              #PXL_TBL,X0
                            00020A
1929      P:000558 P:000558 209000            MOVE              X0,R0
1930      P:000559 P:000559 222E00            MOVE              R1,A
1931      P:00055A P:00055A 200044            SUB     X0,A
1932      P:00055B P:00055B 014184            SUB     #1,A
1933      P:00055C P:00055C 000000            NOP
1934      P:00055D P:00055D 5C6000            MOVE                          A1,Y:(R0)
1935      P:00055E P:00055E 00000C            RTS
1936   
1937                                ; Select which readouts to process
1938                                ;   'SOS'  Amplifier_name
1939                                ;       Amplifier_name = '__L', '__R', or '_LR'
1940   
1941                                SELECT_OUTPUT_SOURCE
1942      P:00055F P:00055F 46DB00            MOVE              X:(R3)+,Y0
1943      P:000560 P:000560 4E0B00            MOVE                          Y0,Y:<OS
1944      P:000561 P:000561 0D0563            JSR     <SEL_OS
1945      P:000562 P:000562 0C008F            JMP     <FINISH
1946   
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 36



1947      P:000563 P:000563 4E8B00  SEL_OS    MOVE                          Y:<OS,Y0
1948      P:000564 P:000564 56F400            MOVE              #'_U1',A
                            5F5531
1949      P:000566 P:000566 200055            CMP     Y0,A
1950      P:000567 P:000567 0E2583            JNE     <COMP_U2
1951   
1952      P:000568 P:000568 44F400            MOVE              #$F0C3,X0
                            00F0C3
1953      P:00056A P:00056A 4C7000            MOVE                          X0,Y:SXL
                            000110
1954      P:00056C P:00056C 44F400            MOVE              #PARALLEL_1,X0
                            00004F
1955      P:00056E P:00056E 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
1956      P:000570 P:000570 44F400            MOVE              #SERIAL_READ_LEFT,X0
                            000107
1957      P:000572 P:000572 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
1958      P:000574 P:000574 44F400            MOVE              #SERIAL_SKIP_LEFT,X0
                            000151
1959      P:000576 P:000576 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
1960      P:000578 P:000578 44F400            MOVE              #SERIAL_IDLE_LEFT,X0
                            000077
1961      P:00057A P:00057A 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
1962      P:00057C P:00057C 44F400            MOVE              #PARALLEL_CLEAR_1,X0
                            000059
1963      P:00057E P:00057E 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
1964      P:000580 P:000580 0A0005            BCLR    #SPLIT_S,X:STATUS
1965      P:000581 P:000581 0A0006            BCLR    #SPLIT_P,X:STATUS
1966      P:000582 P:000582 00000C            RTS
1967   
1968      P:000583 P:000583 56F400  COMP_U2   MOVE              #'_U2',A
                            5F5532
1969      P:000585 P:000585 200055            CMP     Y0,A
1970      P:000586 P:000586 0E25A2            JNE     <COMP_L1
1971   
1972      P:000587 P:000587 44F400            MOVE              #$F041,X0
                            00F041
1973      P:000589 P:000589 4C7000            MOVE                          X0,Y:SXR
                            000148
1974      P:00058B P:00058B 44F400            MOVE              #PARALLEL_2,X0
                            000045
1975      P:00058D P:00058D 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
1976      P:00058F P:00058F 44F400            MOVE              #SERIAL_READ_RIGHT,X0
                            00013F
1977      P:000591 P:000591 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
1978      P:000593 P:000593 44F400            MOVE              #SERIAL_SKIP_RIGHT,X0
                            00015A
1979      P:000595 P:000595 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
1980      P:000597 P:000597 44F400            MOVE              #SERIAL_IDLE_RIGHT,X0
                            000099
1981      P:000599 P:000599 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
1982      P:00059B P:00059B 44F400            MOVE              #PARALLEL_CLEAR_2,X0
                            000063
1983      P:00059D P:00059D 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 37



                            000011
1984      P:00059F P:00059F 0A0005            BCLR    #SPLIT_S,X:STATUS
1985      P:0005A0 P:0005A0 0A0006            BCLR    #SPLIT_P,X:STATUS
1986      P:0005A1 P:0005A1 00000C            RTS
1987   
1988      P:0005A2 P:0005A2 56F400  COMP_L1   MOVE              #'_L1',A
                            5F4C31
1989      P:0005A4 P:0005A4 200055            CMP     Y0,A
1990      P:0005A5 P:0005A5 0E25C1            JNE     <COMP_L2
1991   
1992   
1993      P:0005A6 P:0005A6 44F400            MOVE              #$F082,X0
                            00F082
1994      P:0005A8 P:0005A8 4C7000            MOVE                          X0,Y:SXR
                            000148
1995      P:0005AA P:0005AA 44F400            MOVE              #PARALLEL_1,X0
                            00004F
1996      P:0005AC P:0005AC 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
1997      P:0005AE P:0005AE 44F400            MOVE              #SERIAL_READ_RIGHT,X0
                            00013F
1998      P:0005B0 P:0005B0 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
1999      P:0005B2 P:0005B2 44F400            MOVE              #SERIAL_SKIP_RIGHT,X0
                            00015A
2000      P:0005B4 P:0005B4 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2001      P:0005B6 P:0005B6 44F400            MOVE              #SERIAL_IDLE_RIGHT,X0
                            000099
2002      P:0005B8 P:0005B8 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2003      P:0005BA P:0005BA 44F400            MOVE              #PARALLEL_CLEAR_1,X0
                            000059
2004      P:0005BC P:0005BC 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2005      P:0005BE P:0005BE 0A0005            BCLR    #SPLIT_S,X:STATUS
2006      P:0005BF P:0005BF 0A0006            BCLR    #SPLIT_P,X:STATUS
2007      P:0005C0 P:0005C0 00000C            RTS
2008   
2009      P:0005C1 P:0005C1 56F400  COMP_L2   MOVE              #'_L2',A
                            5F4C32
2010      P:0005C3 P:0005C3 200055            CMP     Y0,A
2011      P:0005C4 P:0005C4 0E25E0            JNE     <COMP_2
2012   
2013      P:0005C5 P:0005C5 44F400            MOVE              #$F000,X0
                            00F000
2014      P:0005C7 P:0005C7 4C7000            MOVE                          X0,Y:SXL
                            000110
2015      P:0005C9 P:0005C9 44F400            MOVE              #PARALLEL_2,X0
                            000045
2016      P:0005CB P:0005CB 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2017      P:0005CD P:0005CD 44F400            MOVE              #SERIAL_READ_LEFT,X0
                            000107
2018      P:0005CF P:0005CF 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2019      P:0005D1 P:0005D1 44F400            MOVE              #SERIAL_SKIP_LEFT,X0
                            000151
2020      P:0005D3 P:0005D3 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2021      P:0005D5 P:0005D5 44F400            MOVE              #SERIAL_IDLE_LEFT,X0
                            000077
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 38



2022      P:0005D7 P:0005D7 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2023      P:0005D9 P:0005D9 44F400            MOVE              #PARALLEL_CLEAR_2,X0
                            000063
2024      P:0005DB P:0005DB 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2025      P:0005DD P:0005DD 0A0005            BCLR    #SPLIT_S,X:STATUS
2026      P:0005DE P:0005DE 0A0006            BCLR    #SPLIT_P,X:STATUS
2027      P:0005DF P:0005DF 00000C            RTS
2028   
2029      P:0005E0 P:0005E0 56F400  COMP_2    MOVE              #'__2',A
                            5F5F32
2030      P:0005E2 P:0005E2 200055            CMP     Y0,A
2031      P:0005E3 P:0005E3 0E25FF            JNE     <COMP_1
2032   
2033      P:0005E4 P:0005E4 44F400            MOVE              #$F0C2,X0
                            00F0C2
2034                                ;        MOVE    #$F041,X0
2035      P:0005E6 P:0005E6 4C7000            MOVE                          X0,Y:SXRL
                            000122
2036      P:0005E8 P:0005E8 44F400            MOVE              #PARALLEL_2,X0
                            000045
2037      P:0005EA P:0005EA 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2038                                ;        MOVE    #SERIAL_READ_SPLIT,X0
2039      P:0005EC P:0005EC 44F400            MOVE              #SERIAL_READ_SPLIT_SPECIAL,X0
                            00012B
2040      P:0005EE P:0005EE 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2041      P:0005F0 P:0005F0 44F400            MOVE              #SERIAL_SKIP_SPLIT,X0
                            000163
2042      P:0005F2 P:0005F2 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2043      P:0005F4 P:0005F4 44F400            MOVE              #SERIAL_IDLE_SPLIT,X0
                            0000BD
2044      P:0005F6 P:0005F6 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2045      P:0005F8 P:0005F8 44F400            MOVE              #PARALLEL_CLEAR_2,X0
                            000063
2046      P:0005FA P:0005FA 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2047      P:0005FC P:0005FC 0A0025            BSET    #SPLIT_S,X:STATUS
2048      P:0005FD P:0005FD 0A0006            BCLR    #SPLIT_P,X:STATUS
2049      P:0005FE P:0005FE 00000C            RTS
2050   
2051      P:0005FF P:0005FF 56F400  COMP_1    MOVE              #'__1',A
                            5F5F31
2052      P:000601 P:000601 200055            CMP     Y0,A
2053      P:000602 P:000602 0E261E            JNE     <COMP_ALL
2054   
2055      P:000603 P:000603 44F400            MOVE              #$F0C2,X0
                            00F0C2
2056      P:000605 P:000605 4C7000            MOVE                          X0,Y:SXRL
                            000122
2057      P:000607 P:000607 44F400            MOVE              #PARALLEL_1,X0
                            00004F
2058      P:000609 P:000609 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2059      P:00060B P:00060B 44F400            MOVE              #SERIAL_READ_SPLIT,X0
                            000119
2060      P:00060D P:00060D 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 39



2061      P:00060F P:00060F 44F400            MOVE              #SERIAL_SKIP_SPLIT,X0
                            000163
2062      P:000611 P:000611 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2063      P:000613 P:000613 44F400            MOVE              #SERIAL_IDLE_SPLIT,X0
                            0000BD
2064      P:000615 P:000615 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2065      P:000617 P:000617 44F400            MOVE              #PARALLEL_CLEAR_1,X0
                            000059
2066      P:000619 P:000619 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2067      P:00061B P:00061B 0A0025            BSET    #SPLIT_S,X:STATUS
2068      P:00061C P:00061C 0A0006            BCLR    #SPLIT_P,X:STATUS
2069      P:00061D P:00061D 00000C            RTS
2070   
2071      P:00061E P:00061E 56F400  COMP_ALL  MOVE              #'ALL',A
                            414C4C
2072      P:000620 P:000620 200055            CMP     Y0,A
2073      P:000621 P:000621 0E263D            JNE     <COMP_TEMP
2074   
2075      P:000622 P:000622 44F400            MOVE              #$F0C0,X0
                            00F0C0
2076      P:000624 P:000624 4C7000            MOVE                          X0,Y:SXRL
                            000122
2077      P:000626 P:000626 44F400            MOVE              #PARALLEL_SPLIT,X0
                            00003B
2078      P:000628 P:000628 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2079      P:00062A P:00062A 44F400            MOVE              #SERIAL_READ_SPLIT,X0
                            000119
2080      P:00062C P:00062C 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2081      P:00062E P:00062E 44F400            MOVE              #SERIAL_SKIP_SPLIT,X0
                            000163
2082      P:000630 P:000630 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2083      P:000632 P:000632 44F400            MOVE              #SERIAL_IDLE_SPLIT,X0
                            0000BD
2084      P:000634 P:000634 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2085      P:000636 P:000636 44F400            MOVE              #PARALLEL_CLEAR_SPLIT,X0
                            00006D
2086      P:000638 P:000638 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2087      P:00063A P:00063A 0A0025            BSET    #SPLIT_S,X:STATUS
2088      P:00063B P:00063B 0A0026            BSET    #SPLIT_P,X:STATUS
2089      P:00063C P:00063C 00000C            RTS
2090   
2091                                COMP_TEMP
2092      P:00063D P:00063D 56F400            MOVE              #>5,A
                            000005
2093      P:00063F P:00063F 200055            CMP     Y0,A
2094      P:000640 P:000640 0E208D            JNE     <ERROR
2095   
2096      P:000641 P:000641 44F400            MOVE              #$F040,X0
                            00F040
2097      P:000643 P:000643 4C7000            MOVE                          X0,Y:SXR
                            000148
2098      P:000645 P:000645 44F400            MOVE              #PARALLEL_SPLIT,X0
                            00003B
2099      P:000647 P:000647 4C7000            MOVE                          X0,Y:PARALLEL
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 40



                            000010
2100      P:000649 P:000649 44F400            MOVE              #SERIAL_READ_RIGHT,X0
                            00013F
2101      P:00064B P:00064B 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2102      P:00064D P:00064D 44F400            MOVE              #SERIAL_SKIP_RIGHT,X0
                            00015A
2103      P:00064F P:00064F 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2104      P:000651 P:000651 44F400            MOVE              #SERIAL_IDLE_RIGHT,X0
                            000099
2105      P:000653 P:000653 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2106      P:000655 P:000655 44F400            MOVE              #PARALLEL_CLEAR_SPLIT,X0
                            00006D
2107      P:000657 P:000657 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2108      P:000659 P:000659 0A0005            BCLR    #SPLIT_S,X:STATUS
2109      P:00065A P:00065A 0A0026            BSET    #SPLIT_P,X:STATUS
2110      P:00065B P:00065B 00000C            RTS
2111   
2112   
2113   
2114                                NO_POLARITY_SHIFT
2115      P:00065C P:00065C 4E8B00            MOVE                          Y:<OS,Y0
2116      P:00065D P:00065D 56F400            MOVE              #'_U1',A
                            5F5531
2117      P:00065F P:00065F 200055            CMP     Y0,A
2118      P:000660 P:000660 0E2666            JNE     <COMP_U2_NO_POL
2119   
2120      P:000661 P:000661 44F400            MOVE              #SERIAL_IDLE_LEFT_NO_POL,X0
                            000088
2121      P:000663 P:000663 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2122      P:000665 P:000665 0C008F            JMP     <FINISH
2123   
2124                                COMP_U2_NO_POL
2125      P:000666 P:000666 56F400            MOVE              #'_U2',A
                            5F5532
2126      P:000668 P:000668 200055            CMP     Y0,A
2127      P:000669 P:000669 0E266F            JNE     <COMP_L1_NO_POL
2128   
2129      P:00066A P:00066A 44F400            MOVE              #SERIAL_IDLE_RIGHT_NO_POL,X0
                            0000AB
2130      P:00066C P:00066C 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2131      P:00066E P:00066E 0C008F            JMP     <FINISH
2132   
2133                                COMP_L1_NO_POL
2134      P:00066F P:00066F 56F400            MOVE              #'_L1',A
                            5F4C31
2135      P:000671 P:000671 200055            CMP     Y0,A
2136      P:000672 P:000672 0E2678            JNE     <COMP_L2_NO_POL
2137   
2138      P:000673 P:000673 44F400            MOVE              #SERIAL_IDLE_RIGHT_NO_POL,X0
                            0000AB
2139      P:000675 P:000675 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2140      P:000677 P:000677 0C008F            JMP     <FINISH
2141   
2142                                COMP_L2_NO_POL
2143      P:000678 P:000678 56F400            MOVE              #'_L2',A
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 41



                            5F4C32
2144      P:00067A P:00067A 200055            CMP     Y0,A
2145      P:00067B P:00067B 0E2681            JNE     <COMP_2_NO_POL
2146   
2147      P:00067C P:00067C 44F400            MOVE              #SERIAL_IDLE_LEFT_NO_POL,X0
                            000088
2148      P:00067E P:00067E 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2149      P:000680 P:000680 0C008F            JMP     <FINISH
2150   
2151                                COMP_2_NO_POL
2152      P:000681 P:000681 56F400            MOVE              #'__2',A
                            5F5F32
2153      P:000683 P:000683 200055            CMP     Y0,A
2154      P:000684 P:000684 0E268A            JNE     <COMP_1_NO_POL
2155   
2156      P:000685 P:000685 44F400            MOVE              #SERIAL_IDLE_SPLIT_NO_POL,X0
                            0000CF
2157      P:000687 P:000687 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2158      P:000689 P:000689 0C008F            JMP     <FINISH
2159   
2160                                COMP_1_NO_POL
2161      P:00068A P:00068A 56F400            MOVE              #'__1',A
                            5F5F31
2162      P:00068C P:00068C 200055            CMP     Y0,A
2163      P:00068D P:00068D 0E2693            JNE     <COMP_ALL_NO_POL
2164   
2165      P:00068E P:00068E 44F400            MOVE              #SERIAL_IDLE_SPLIT_NO_POL,X0
                            0000CF
2166      P:000690 P:000690 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2167      P:000692 P:000692 0C008F            JMP     <FINISH
2168   
2169                                 COMP_ALL_NO_POL
2170      P:000693 P:000693 56F400            MOVE              #'ALL',A
                            414C4C
2171      P:000695 P:000695 200055            CMP     Y0,A
2172      P:000696 P:000696 0E208D            JNE     <ERROR
2173   
2174      P:000697 P:000697 44F400            MOVE              #SERIAL_IDLE_SPLIT_NO_POL,X0
                            0000CF
2175      P:000699 P:000699 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2176      P:00069B P:00069B 0C008F            JMP     <FINISH
2177   
2178   
2179   
2180   
2181      P:00069C P:00069C 44DB00  ERASE     MOVE              X:(R3)+,X0              ; Get TIME1 off the command buffer
2182      P:00069D P:00069D 4C2500            MOVE                          X0,Y:<TIME1 ; Move it to the address in tim.asm
2183      P:00069E P:00069E 44DB00            MOVE              X:(R3)+,X0              ; Ditto for TIME2
2184      P:00069F P:00069F 4C2600            MOVE                          X0,Y:<TIME2
2185   
2186                                                                                    ;TODO: Get rid of this horrible hack
2187                                                                                    ;Horrible hack to get over time out on comma
nd return
2188      P:0006A0 P:0006A0 0D06D7            JSR     <HACK_FINISH
2189      P:0006A1 P:0006A1 012F23            BSET    #3,X:PCRD                         ; Turn on the serial clock - this should alr
eady be the case
2190      P:0006A2 P:0006A2 0A0F01            BCLR    #1,X:<LATCH                       ; Separate updates of clock driver
2191      P:0006A3 P:0006A3 0A0F20            BSET    #CDAC,X:<LATCH                    ; Disable clearing of DACs
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 42



2192      P:0006A4 P:0006A4 0A0F22            BSET    #ENCK,X:<LATCH                    ; Enable clock and DAC output switches
2193      P:0006A5 P:0006A5 09F0B5            MOVEP             X:LATCH,Y:WRLATCH       ; Write it to the hardware
                            00000F
2194      P:0006A7 P:0006A7 0D043E            JSR     <PAL_DLY                          ; Delay for all this to happen
2195   
2196                                ; Read DAC values from a table, and write them to the DACs
2197      P:0006A8 P:0006A8 60F400            MOVE              #ERHI,R0                ; Get starting address of erase clock values
                            00016D
2198      P:0006AA P:0006AA 000000            NOP
2199      P:0006AB P:0006AB 000000            NOP
2200      P:0006AC P:0006AC 065840            DO      Y:(R0)+,E_DAC                     ; Repeat Y:(R0)+ times
                            0006B0
2201      P:0006AE P:0006AE 5ED800            MOVE                          Y:(R0)+,A   ; Read the table entry
2202      P:0006AF P:0006AF 0D020C            JSR     <XMIT_A_WORD                      ; Transmit it to TIM-A-STD
2203      P:0006B0 P:0006B0 000000            NOP
2204                                E_DAC
2205   
2206                                ; Let the DAC voltages all ramp up before exiting
2207      P:0006B1 P:0006B1 44F400            MOVE              #400000,X0
                            061A80
2208      P:0006B3 P:0006B3 06C400            DO      X0,*+3                            ; 4 millisec delay
                            0006B5
2209      P:0006B5 P:0006B5 000000            NOP
2210   
2211                                ; Start the delay loop
2212   
2213      P:0006B6 P:0006B6 062540            DO      Y:<TIME1,ER_T1                    ; Delay TIME1 msec
                            0006B9
2214      P:0006B8 P:0006B8 0D06D1            JSR     <LNG_DLY
2215      P:0006B9 P:0006B9 000000            NOP
2216   
2217      P:0006BA P:0006BA 5EF000  ER_T1     MOVE                          Y:VSUBN,A   ; Reset the Vsub value
                            00016C
2218      P:0006BC P:0006BC 0D020C            JSR     <XMIT_A_WORD
2219      P:0006BD P:0006BD 0D043E            JSR     <PAL_DLY                          ; Wait for SSI and PAL to empty
2220   
2221      P:0006BE P:0006BE 062640            DO      Y:<TIME2,ER_T2                    ; Delay TIME2 msec
                            0006C1
2222      P:0006C0 P:0006C0 0D06D1            JSR     <LNG_DLY
2223      P:0006C1 P:0006C1 000000            NOP
2224   
2225      P:0006C2 P:0006C2 60F400  ER_T2     MOVE              #ERHI_END,R0            ; Get the original clock values back.
                            000187
2226                                ;        MOVE    #DACS,R0               ; This line would do the same job -- pointing back to th
e
2227                                                                                    ; original clocking values, but you'd be set
ting everything.
2228   
2229                                ; Read DAC values from a table, and write them to the DACs
2230      P:0006C4 P:0006C4 000000            NOP
2231      P:0006C5 P:0006C5 000000            NOP
2232      P:0006C6 P:0006C6 065840            DO      Y:(R0)+,END_DAC_RESTORE           ; Repeat Y:(R0)+ times
                            0006CA
2233      P:0006C8 P:0006C8 5ED800            MOVE                          Y:(R0)+,A   ; Read the table entry
2234      P:0006C9 P:0006C9 0D020C            JSR     <XMIT_A_WORD                      ; Transmit it to TIM-A-STD
2235      P:0006CA P:0006CA 000000            NOP
2236                                END_DAC_RESTORE
2237   
2238                                ; Let the DAC voltages all ramp up before exiting
2239      P:0006CB P:0006CB 44F400            MOVE              #400000,X0
                            061A80
2240      P:0006CD P:0006CD 06C400            DO      X0,*+3                            ; 4 millisec delay
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 43



                            0006CF
2241      P:0006CF P:0006CF 000000            NOP
2242      P:0006D0 P:0006D0 0C0054            JMP     <START
2243   
2244                                ; Routine to delay for 1msec
2245                                ; 40ns for DO and NOP line.  1ms/80ns = 125000
2246   
2247      P:0006D1 P:0006D1 44F400  LNG_DLY   MOVE              #100000,X0
                            0186A0
2248      P:0006D3 P:0006D3 06C400            DO      X0,*+3                            ; Wait 1 millisec for settling
                            0006D5
2249      P:0006D5 P:0006D5 000000            NOP
2250      P:0006D6 P:0006D6 00000C            RTS
2251   
2252                                ; Hack finish subroutine to send 'DON' reply back to the PIC
2253                                ; card.  Some of this should be redundant -- such as jumping to the
2254                                ; command processing.
2255                                 HACK_FINISH
2256      P:0006D7 P:0006D7 479800            MOVE              X:<DONE,Y1              ; Send 'DON' as the reply
2257                                 HACK_FINISH1
2258      P:0006D8 P:0006D8 578500            MOVE              X:<HEADER,B             ; Get header of incoming command
2259      P:0006D9 P:0006D9 469C00            MOVE              X:<SMASK,Y0             ; This was the source byte, and is to
2260      P:0006DA P:0006DA 330700            MOVE              #<COM_BUF,R3            ;     become the destination byte
2261      P:0006DB P:0006DB 46935E            AND     Y0,B      X:<TWO,Y0
2262      P:0006DC P:0006DC 0C1ED1            LSR     #8,B                              ; Shift right eight bytes, add it to the
2263      P:0006DD P:0006DD 460600            MOVE              Y0,X:<NWORDS            ;     header, and put 2 as the number
2264      P:0006DE P:0006DE 469958            ADD     Y0,B      X:<SBRD,Y0              ;     of words in the string
2265      P:0006DF P:0006DF 200058            ADD     Y0,B                              ; Add source board's header, set Y1 for abov
e
2266      P:0006E0 P:0006E0 000000            NOP
2267      P:0006E1 P:0006E1 575B00            MOVE              B,X:(R3)+               ; Put the new header on the transmitter stac
k
2268      P:0006E2 P:0006E2 475B00            MOVE              Y1,X:(R3)+              ; Put the argument on the transmitter stack
2269      P:0006E3 P:0006E3 570500            MOVE              B,X:<HEADER
2270      P:0006E4 P:0006E4 330700            MOVE              #<COM_BUF,R3            ; Restore R3 = beginning of the command
2271   
2272                                ; Is this command for the timing board?
2273      P:0006E5 P:0006E5 448500            MOVE              X:<HEADER,X0
2274      P:0006E6 P:0006E6 579B00            MOVE              X:<DMASK,B
2275      P:0006E7 P:0006E7 459A4E            AND     X0,B      X:<TIM_DRB,X1           ; Extract destination byte
2276      P:0006E8 P:0006E8 20006D            CMP     X1,B                              ; Does header = timing board number?
2277      P:0006E9 P:0006E9 0EA080            JEQ     <COMMAND                          ; Yes, process it here
2278      P:0006EA P:0006EA 0E96EB            JLT     <HACK_FO_XMT                      ; Send it to fiber optic transmitter
2279                                 HACK_FO_XMT
2280      P:0006EB P:0006EB 330700            MOVE              #COM_BUF,R3
2281      P:0006EC P:0006EC 060600            DO      X:<NWORDS,HACK_DON_FFO            ; Transmit all the words in the command
                            0006F0
2282      P:0006EE P:0006EE 57DB00            MOVE              X:(R3)+,B
2283      P:0006EF P:0006EF 0D00EB            JSR     <XMT_WRD
2284      P:0006F0 P:0006F0 000000            NOP
2285                                 HACK_DON_FFO
2286      P:0006F1 P:0006F1 00000C            RTS
2287   
2288      P:0006F2 P:0006F2 44DB00  EPURGE    MOVE              X:(R3)+,X0              ; Get TIME1 off the command buffer
2289      P:0006F3 P:0006F3 4C2500            MOVE                          X0,Y:<TIME1 ; Move it to the address in tim.asm
2290   
2291                                                                                    ;TODO: Get rid of this horrible hack
2292                                                                                    ;Horrible hack to get over time out on comma
nd return
2293      P:0006F4 P:0006F4 0D06D7            JSR     <HACK_FINISH
2294      P:0006F5 P:0006F5 012F23            BSET    #3,X:PCRD                         ; Turn on the serial clock - this should alr
eady be the case
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 44



2295      P:0006F6 P:0006F6 0A0F01            BCLR    #1,X:<LATCH                       ; Separate updates of clock driver
2296      P:0006F7 P:0006F7 0A0F20            BSET    #CDAC,X:<LATCH                    ; Disable clearing of DACs
2297      P:0006F8 P:0006F8 0A0F22            BSET    #ENCK,X:<LATCH                    ; Enable clock and DAC output switches
2298      P:0006F9 P:0006F9 09F0B5            MOVEP             X:LATCH,Y:WRLATCH       ; Write it to the hardware
                            00000F
2299      P:0006FB P:0006FB 0D043E            JSR     <PAL_DLY                          ; Delay for all this to happen
2300   
2301                                ; Read DAC values from a table, and write them to the DACs
2302      P:0006FC P:0006FC 60F400            MOVE              #EPUR,R0                ; Get starting address of erase clock values
                            0001A4
2303      P:0006FE P:0006FE 000000            NOP
2304      P:0006FF P:0006FF 000000            NOP
2305      P:000700 P:000700 065840            DO      Y:(R0)+,E_DAC2                    ; Repeat Y:(R0)+ times
                            000704
2306      P:000702 P:000702 5ED800            MOVE                          Y:(R0)+,A   ; Read the table entry
2307      P:000703 P:000703 0D020C            JSR     <XMIT_A_WORD                      ; Transmit it to TIM-A-STD
2308      P:000704 P:000704 000000            NOP
2309                                E_DAC2
2310   
2311                                ; Let the DAC voltages all ramp before exiting
2312      P:000705 P:000705 44F400            MOVE              #400000,X0
                            061A80
2313      P:000707 P:000707 06C400            DO      X0,*+3                            ; 4 millisec delay
                            000709
2314      P:000709 P:000709 000000            NOP
2315   
2316                                ; Start the delay loop
2317   
2318      P:00070A P:00070A 062540            DO      Y:<TIME1,EPUR_T1                  ; Delay TIME1 msec
                            00070D
2319      P:00070C P:00070C 0D06D1            JSR     <LNG_DLY
2320      P:00070D P:00070D 000000            NOP
2321   
2322      P:00070E P:00070E 60F400  EPUR_T1   MOVE              #ERHI_END,R0            ; Get the original clock values back.
                            000187
2323      P:000710 P:000710 000000            NOP
2324      P:000711 P:000711 000000            NOP
2325      P:000712 P:000712 065840            DO      Y:(R0)+,END_DAC_RESTORE2          ; Repeat Y:(R0)+ times
                            000716
2326      P:000714 P:000714 5ED800            MOVE                          Y:(R0)+,A   ; Read the table entry
2327      P:000715 P:000715 0D020C            JSR     <XMIT_A_WORD                      ; Transmit it to TIM-A-STD
2328      P:000716 P:000716 000000            NOP
2329                                END_DAC_RESTORE2
2330   
2331                                ; Let the DAC voltages all ramp up before exiting
2332      P:000717 P:000717 44F400            MOVE              #400000,X0
                            061A80
2333      P:000719 P:000719 06C400            DO      X0,*+3                            ; 4 millisec delay
                            00071B
2334      P:00071B P:00071B 000000            NOP
2335      P:00071C P:00071C 0C0054            JMP     <START
2336   
2337                                ; change the idling direction.  CID
2338                                CHANGE_IDLE_DIRECTION
2339      P:00071D P:00071D 46DB00            MOVE              X:(R3)+,Y0
2340      P:00071E P:00071E 0D0720            JSR     <CHG_IDL
2341      P:00071F P:00071F 0C008F            JMP     <FINISH
2342   
2343      P:000720 P:000720 56F400  CHG_IDL   MOVE              #'__L',A                ; Shift right
                            5F5F4C
2344      P:000722 P:000722 200055            CMP     Y0,A
2345      P:000723 P:000723 0E272A            JNE     <COMP_IR
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 45



2346   
2347      P:000724 P:000724 44F400            MOVE              #SERIAL_IDLE_LEFT,X0
                            000077
2348      P:000726 P:000726 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2349      P:000728 P:000728 0A0005            BCLR    #SPLIT_S,X:STATUS
2350      P:000729 P:000729 00000C            RTS
2351   
2352      P:00072A P:00072A 56F400  COMP_IR   MOVE              #'__R',A
                            5F5F52
2353      P:00072C P:00072C 200055            CMP     Y0,A
2354      P:00072D P:00072D 0E2734            JNE     <COMP_IS
2355   
2356      P:00072E P:00072E 44F400            MOVE              #SERIAL_IDLE_RIGHT,X0
                            000099
2357      P:000730 P:000730 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2358      P:000732 P:000732 0A0005            BCLR    #SPLIT_S,X:STATUS
2359      P:000733 P:000733 00000C            RTS
2360   
2361      P:000734 P:000734 56F400  COMP_IS   MOVE              #'__S',A
                            5F5F53
2362      P:000736 P:000736 200055            CMP     Y0,A
2363      P:000737 P:000737 0E208D            JNE     <ERROR
2364   
2365      P:000738 P:000738 44F400            MOVE              #SERIAL_IDLE_SPLIT,X0
                            0000BD
2366      P:00073A P:00073A 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2367      P:00073C P:00073C 0A0025            BSET    #SPLIT_S,X:STATUS
2368      P:00073D P:00073D 00000C            RTS
2369   
2370                                CHANGE_NUMBER_PARALLEL_CLEARS
2371      P:00073E P:00073E 46DB00            MOVE              X:(R3)+,Y0
2372      P:00073F P:00073F 4E1D00            MOVE                          Y0,Y:<N_PARALLEL_CLEARS
2373      P:000740 P:000740 0C008F            JMP     <FINISH
2374   
2375   
2376                                CHANGE_IDLE_READ_DIRECTION
2377      P:000741 P:000741 46DB00            MOVE              X:(R3)+,Y0
2378      P:000742 P:000742 0D0744            JSR     <CHG_IDL_READ
2379      P:000743 P:000743 0C008F            JMP     <FINISH
2380   
2381                                CHG_IDL_READ
2382      P:000744 P:000744 56F400            MOVE              #'__L',A                ; Shift right
                            5F5F4C
2383      P:000746 P:000746 200055            CMP     Y0,A
2384      P:000747 P:000747 0E2752            JNE     <COMP_I_R_R
2385   
2386      P:000748 P:000748 44F400            MOVE              #SERIAL_IDLE_LEFT,X0
                            000077
2387      P:00074A P:00074A 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2388      P:00074C P:00074C 44F400            MOVE              #SERIAL_READ_LEFT,X0
                            000107
2389      P:00074E P:00074E 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2390      P:000750 P:000750 0A0005            BCLR    #SPLIT_S,X:STATUS
2391      P:000751 P:000751 00000C            RTS
2392   
2393                                COMP_I_R_R
2394      P:000752 P:000752 56F400            MOVE              #'__R',A
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 46



                            5F5F52
2395      P:000754 P:000754 200055            CMP     Y0,A
2396      P:000755 P:000755 0E2734            JNE     <COMP_IS
2397   
2398      P:000756 P:000756 44F400            MOVE              #SERIAL_IDLE_RIGHT,X0
                            000099
2399      P:000758 P:000758 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000014
2400      P:00075A P:00075A 44F400            MOVE              #SERIAL_READ_RIGHT,X0
                            00013F
2401      P:00075C P:00075C 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2402      P:00075E P:00075E 0A0005            BCLR    #SPLIT_S,X:STATUS
2403      P:00075F P:00075F 00000C            RTS
2404   
2405                                COMP_I_R_S
2406      P:000760 P:000760 56F400            MOVE              #'__S',A
                            5F5F53
2407      P:000762 P:000762 200055            CMP     Y0,A
2408      P:000763 P:000763 0E208D            JNE     <ERROR
2409   
2410      P:000764 P:000764 44F400            MOVE              #SERIAL_READ_SPLIT,X0
                            000119
2411      P:000766 P:000766 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2412      P:000768 P:000768 0A0025            BSET    #SPLIT_S,X:STATUS
2413      P:000769 P:000769 00000C            RTS
2414   
2415   
2416   
2417                                ; Set synchronization mode of two controllers either ON or OFF
2418                                SET_SYNC_MODE
2419      P:00076A P:00076A 44DB00            MOVE              X:(R3)+,X0              ; =1 for SYNC yes, =0 for not
2420      P:00076B P:00076B 0AC400            JCLR    #0,X0,DONT_SYNC
                            00076F
2421      P:00076D P:00076D 0A002D            BSET    #ST_SYNC,X:STATUS
2422      P:00076E P:00076E 0C008F            JMP     <FINISH
2423                                DONT_SYNC
2424      P:00076F P:00076F 0A000D            BCLR    #ST_SYNC,X:STATUS
2425      P:000770 P:000770 0C008F            JMP     <FINISH
2426   
2427                                ;SYNC_2
2428   
2429                                ;***********************************************************
2430                                ; routine to synchronise the controllers
2431                                ; at least one controller must output the 25 MHz clk
2432                                ; this code sends a high pulse out then goes low
2433                                ; the controllers must determine if they are MASTER or SLAVE
2434                                ; by looking at the JP2 LINK - they then run a synch
2435                                ; routine depending on this input
2436   
2437                                SYNCH_DLY
2438      P:000771 P:000771 44F400            MOVE              #100000,X0              ; 1 ms delay
                            0186A0
2439      P:000773 P:000773 06C400            DO      X0,S_DLY1
                            000775
2440      P:000775 P:000775 000000            NOP
2441                                S_DLY1
2442      P:000776 P:000776 000000            NOP
2443      P:000777 P:000777 00000C            RTS
2444   
2445                                SYNCH_CONTROLLER
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 47



2446      P:000778 P:000778 01ADA0            JSET    #SLAVE,X:PDRD,SLAVE_SYSTEM
                            000798
2447   
2448                                MASTER_SYSTEM
2449      P:00077A P:00077A 0D0771            JSR     <SYNCH_DLY                        ; wait so that the slave goes first
2450   
2451      P:00077B P:00077B 0A890C            BCLR    #EXT_OUT0,X:HDR                   ; clear SYNCH bit
2452      P:00077C P:00077C 06D0A7            REP     #2000                             ; wait 20 us
2453      P:00077D P:00077D 000000            NOP
2454                                ;       BSET    #EXT_OUT0,X:HDR         ; set SYNCH bit
2455                                ;       REP     #2000                   ; wait 20 us
2456                                ;       NOP
2457   
2458      P:00077E P:00077E 0A898A  CHK_1     JCLR    #EXT_IN0,X:HDR,CHK_1              ; Wait for the slave signal to be high
                            00077E
2459      P:000780 P:000780 000000            NOP
2460      P:000781 P:000781 06D0A7            REP     #2000                             ; ensure that it wasn't noise
2461      P:000782 P:000782 000000            NOP
2462      P:000783 P:000783 0A89AA            JSET    #EXT_IN0,X:HDR,GO_LOW_PULSE
                            000786
2463      P:000785 P:000785 0C077E            JMP     <CHK_1                            ; invalid - look again
2464                                GO_LOW_PULSE
2465      P:000786 P:000786 06D0A7            REP     #2000                             ; wait 20 us
2466      P:000787 P:000787 000000            NOP
2467      P:000788 P:000788 0A892C            BSET    #EXT_OUT0,X:HDR                   ; set SYNCH bit
2468      P:000789 P:000789 06D0A7            REP     #2000                             ; wait 20 us
2469      P:00078A P:00078A 000000            NOP
2470   
2471      P:00078B P:00078B 0A89AA  CHK_2     JSET    #EXT_IN0,X:HDR,*                  ; Wait for a low signal from the slave
                            00078B
2472      P:00078D P:00078D 000000            NOP
2473      P:00078E P:00078E 06C6A7            REP     #1990                             ; Let it be low for almost as long
2474      P:00078F P:00078F 000000            NOP                                       ;  as the slave keeps it low
2475      P:000790 P:000790 0A898A            JCLR    #EXT_IN0,X:HDR,MASTER_GOT_IT
                            000793
2476      P:000792 P:000792 0C078B            JMP     <CHK_2                            ; invalid - look again
2477                                MASTER_GOT_IT
2478      P:000793 P:000793 060AA0            REP     #10
2479      P:000794 P:000794 000000            NOP
2480      P:000795 P:000795 0A890C            BCLR    #EXT_OUT0,X:HDR                   ; clear SYNCH bit
2481      P:000796 P:000796 000000            NOP
2482      P:000797 P:000797 0C07B7            JMP     <SYNCHED_NOW
2483   
2484                                SLAVE_SYSTEM
2485                                ;       BCLR    #EXT_OUT0,X:HDR         ; clear SYNCH bit
2486                                ;       REP     #2000                   ; wait 20 us
2487                                ;       NOP
2488   
2489      P:000798 P:000798 0A892C            BSET    #EXT_OUT0,X:HDR                   ; set SYNCH bit
2490      P:000799 P:000799 06D0A7            REP     #2000                             ; wait 20 us
2491      P:00079A P:00079A 000000            NOP
2492   
2493      P:00079B P:00079B 0A898A  CHK_3     JCLR    #EXT_IN0,X:HDR,CHK_3              ; keep checking
                            00079B
2494      P:00079D P:00079D 000000            NOP
2495      P:00079E P:00079E 06E8A3            REP     #1000                             ; ensure that it wasn't noise
2496      P:00079F P:00079F 000000            NOP
2497      P:0007A0 P:0007A0 0A89AA            JSET    #EXT_IN0,X:HDR,GO_LOW_PULSE2
                            0007A3
2498      P:0007A2 P:0007A2 0C079B            JMP     <CHK_3                            ; invalid - look again
2499                                GO_LOW_PULSE2
2500      P:0007A3 P:0007A3 06E8A3            REP     #1000                             ; wait 10 us
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 48



2501      P:0007A4 P:0007A4 000000            NOP
2502      P:0007A5 P:0007A5 0A890C            BCLR    #EXT_OUT0,X:HDR                   ; clear SYNCH bit
2503      P:0007A6 P:0007A6 06D0A7            REP     #2000                             ; wait 20 us
2504      P:0007A7 P:0007A7 000000            NOP
2505      P:0007A8 P:0007A8 0A89AA  CHK_4     JSET    #EXT_IN0,X:HDR,CHK_4              ; keep checking
                            0007A8
2506      P:0007AA P:0007AA 000000            NOP
2507      P:0007AB P:0007AB 06E8A3            REP     #1000                             ; ensure that it wasn't noise
2508      P:0007AC P:0007AC 000000            NOP
2509      P:0007AD P:0007AD 0A898A            JCLR    #EXT_IN0,X:HDR,SLAVE_GOT_IT
                            0007B0
2510      P:0007AF P:0007AF 0C07A8            JMP     <CHK_4                            ; invalid - look again
2511                                SLAVE_GOT_IT
2512      P:0007B0 P:0007B0 44F400            MOVE              #1000,X0
                            0003E8
2513      P:0007B2 P:0007B2 06C400            DO      X0,*+3
                            0007B4
2514      P:0007B4 P:0007B4 000000            NOP
2515      P:0007B5 P:0007B5 0A890C            BCLR    #EXT_OUT0,X:HDR                   ; clear SYNCH bit
2516      P:0007B6 P:0007B6 000000            NOP
2517                                SYNCHED_NOW
2518   
2519                                ; Generate a recognizable pattern
2520   
2521      P:0007B7 P:0007B7 0A892C            BSET    #EXT_OUT0,X:HDR
2522      P:0007B8 P:0007B8 0664A0            REP     #100
2523      P:0007B9 P:0007B9 000000            NOP
2524      P:0007BA P:0007BA 0A890C            BCLR    #EXT_OUT0,X:HDR
2525      P:0007BB P:0007BB 0664A0            REP     #100
2526      P:0007BC P:0007BC 000000            NOP
2527      P:0007BD P:0007BD 0A892C            BSET    #EXT_OUT0,X:HDR
2528      P:0007BE P:0007BE 0664A0            REP     #100
2529      P:0007BF P:0007BF 000000            NOP
2530      P:0007C0 P:0007C0 0A890C            BCLR    #EXT_OUT0,X:HDR
2531      P:0007C1 P:0007C1 0664A0            REP     #100
2532      P:0007C2 P:0007C2 000000            NOP
2533   
2534      P:0007C3 P:0007C3 00000C            RTS
2535   
2536                                TIMS_TEST
2537      P:0007C4 P:0007C4 57F400            MOVE              #020102,B
                            004E86
2538      P:0007C6 P:0007C6 0D00EB            JSR     <XMT_WRD
2539      P:0007C7 P:0007C7 000000            NOP
2540      P:0007C8 P:0007C8 240000            MOVE              #0,X0
2541      P:0007C9 P:0007C9 4C7000            MOVE                          X0,Y:NSR
                            000001
2542      P:0007CB P:0007CB 0C0054            JMP     <START
2543   
2544                                ;CHANGE_VSUB
2545                                ;  MOVE  X:(R3)+,Y0 ; put the Vsub voltage into Y0
2546                                ;  CLR   A
2547                                ;  MOVE  #$3F0000,X0 ; put base address of Vsub into A
2548                                ;  ADD   X0,A
2549                                ;  OR    Y0,A       ; or it with the voltage
2550                                ;  NOP
2551                                ;  MOVE  #VSUBN,X0  ; Get the address of Vsubn
2552                                ;  MOVE  A,X0       ; Put A into this
2553                                ;  JSR   <XMIT_A_WORD ; Send it out
2554                                ;  JMP   <FINISH     ; Finish
2555   
2556   
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  timCCDmisc.asm  Page 49



2557   
2558   
2559   
2560   
2561                                 TIMBOOT_X_MEMORY
2562      0007CC                              EQU     @LCV(L)
2563   
2564                                ;  ****************  Setup memory tables in X: space ********************
2565   
2566                                ; Define the address in P: space where the table of constants begins
2567   
2568                                          IF      @SCP("HOST","HOST")
2569      X:000036 X:000036                   ORG     X:END_COMMAND_TABLE,X:END_COMMAND_TABLE
2570                                          ENDIF
2571   
2572                                          IF      @SCP("HOST","ROM")
2574                                          ENDIF
2575   
2576                                ; Application commands
2577      X:000036 X:000036                   DC      'PON',POWER_ON
2578      X:000038 X:000038                   DC      'POF',POWER_OFF
2579      X:00003A X:00003A                   DC      'SBV',SET_BIAS_VOLTAGES
2580      X:00003C X:00003C                   DC      'IDL',START_IDLE_CLOCKING
2581      X:00003E X:00003E                   DC      'OSH',OPEN_SHUTTER
2582      X:000040 X:000040                   DC      'CSH',CLOSE_SHUTTER
2583      X:000042 X:000042                   DC      'RDC',RDCCD                       ; Begin CCD readout
2584      X:000044 X:000044                   DC      'CLR',CLEAR                       ; Fast clear the CCD
2585   
2586                                ; Exposure and readout control routines
2587      X:000046 X:000046                   DC      'SET',SET_EXPOSURE_TIME
2588      X:000048 X:000048                   DC      'RET',READ_EXPOSURE_TIME
2589      X:00004A X:00004A                   DC      'SEX',START_EXPOSURE
2590      X:00004C X:00004C                   DC      'PEX',PAUSE_EXPOSURE
2591      X:00004E X:00004E                   DC      'REX',RESUME_EXPOSURE
2592      X:000050 X:000050                   DC      'AEX',ABORT_EXPOSURE
2593      X:000052 X:000052                   DC      'ABR',ABR_RDC
2594      X:000054 X:000054                   DC      'CRD',CONTINUE_READ
2595      X:000056 X:000056                   DC      'WSI',SYNTHETIC_IMAGE
2596      X:000058 X:000058                   DC      'SSM',SET_SYNC_MODE
2597      X:00005A X:00005A                   DC      'STR',STR_RDC
2598   
2599                                ; Support routines
2600      X:00005C X:00005C                   DC      'SGN',ST_GAIN
2601      X:00005E X:00005E                   DC      'SBN',SET_BIAS_NUMBER
2602      X:000060 X:000060                   DC      'SMX',SET_MUX
2603      X:000062 X:000062                   DC      'CSW',CLR_SWS
2604      X:000064 X:000064                   DC      'SOS',SELECT_OUTPUT_SOURCE
2605      X:000066 X:000066                   DC      'SSS',SET_SUBARRAY_SIZES
2606      X:000068 X:000068                   DC      'SSP',SET_SUBARRAY_POSITIONS
2607      X:00006A X:00006A                   DC      'RCC',READ_CONTROLLER_CONFIGURATION
2608      X:00006C X:00006C                   DC      'RAW',RAW_COMMAND                 ; So I can set voltages as I please
2609      X:00006E X:00006E                   DC      'ERS',ERASE                       ;Persistent image erase
2610      X:000070 X:000070                   DC      'EPG',EPURGE                      ; E-Purge procedure
2611      X:000072 X:000072                   DC      'TAD',TEST_AD
2612      X:000074 X:000074                   DC      'CID',CHANGE_IDLE_DIRECTION
2613      X:000076 X:000076                   DC      'CIR',CHANGE_IDLE_READ_DIRECTION
2614      X:000078 X:000078                   DC      'CPC',CHANGE_NUMBER_PARALLEL_CLEARS
2615      X:00007A X:00007A                   DC      'NPS',NO_POLARITY_SHIFT
2616      X:00007C X:00007C                   DC      'TST',TIMS_TEST
2617                                                                                    ;  DC  'CVS',CHANGE_VSUB
2618                                                                                    ;  DC  'FSC',FRAME_STORE_CLEAR
2619      X:00007E X:00007E                   DC      'PCK',POWER_CHECK
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  tim.asm  Page 50



2620   
2621                                 END_APPLICATION_COMMAND_TABLE
2622      000080                              EQU     @LCV(L)
2623   
2624                                          IF      @SCP("HOST","HOST")
2625      00002C                    NUM_COM   EQU     (@LCV(R)-COM_TBL_R)/2             ; Number of boot +
2626                                                                                    ;  application commands
2627      00038E                    EXPOSING  EQU     CHK_TIM                           ; Address if exposing
2628                                 CONTINUE_READING
2629      100000                              EQU     CONT_RD                           ; Address if reading out
2630                                          ENDIF
2631   
2632                                          IF      @SCP("HOST","ROM")
2634                                          ENDIF
2635   
2636                                ; Now let's go for the timing waveform tables
2637                                          IF      @SCP("HOST","HOST")
2638      Y:000000 Y:000000                   ORG     Y:0,Y:0
2639                                          ENDIF
2640   
2641      Y:000000 Y:000000         GAIN      DC      END_APPLICATON_Y_MEMORY-@LCV(L)-1
2642   
2643      Y:000001 Y:000001         NSR       DC      4200                              ; Number Serial Read, prescan + image + bias
2644      Y:000002 Y:000002         NPR       DC      2100                              ; Number Parallel Read
2645      Y:000003 Y:000003         NSCLR     DC      NS_CLR                            ; To clear the serial register
2646      Y:000004 Y:000004         NPCLR     DC      NP_CLR                            ; To clear the parallel register
2647      Y:000005 Y:000005         NSBIN     DC      1                                 ; Serial binning parameter
2648      Y:000006 Y:000006         NPBIN     DC      1                                 ; Parallel binning parameter
2649      Y:000007 Y:000007         TST_DAT   DC      0                                 ; Temporary definition for test images
2650      Y:000008 Y:000008         SHDEL     DC      SH_DEL                            ; Delay in milliseconds between shutter clos
ing
2651                                                                                    ; and image readout
2652      Y:000009 Y:000009         CONFIG    DC      CC                                ; Controller configuration
2653                                 NSERIALS_READ
2654      Y:00000A Y:00000A                   DC      4200                              ; Number of serials to read
2655   
2656                                ; Waveform parameters
2657      Y:00000B Y:00000B         OS        DC      '__2'                             ; Output source
2658                                 FIRST_CLOCKS
2659      Y:00000C Y:00000C                   DC      0                                 ; Address of first clocks waveforms
2660      Y:00000D Y:00000D         CLOCK_LINE DC     0                                 ; Clock one complete line of charge
2661   
2662                                ; Readout peculiarity parameters. Default values are selected here.
2663                                 SERIAL_READ
2664      Y:00000E Y:00000E                   DC      SERIAL_READ_SPLIT                 ;14  ; Serial readout waveforms
2665                                 SERIAL_SKIP
2666      Y:00000F Y:00000F                   DC      SERIAL_SKIP_SPLIT                 ;15      ; Serial skipping waveforms
2667      Y:000010 Y:000010         PARALLEL  DC      PARALLEL_2                        ;16  ; Parallel shifting waveforms
2668                                 PARALLEL_CLEAR
2669      Y:000011 Y:000011                   DC      PARALLEL_CLEAR_2                  ;17
2670      Y:000012 Y:000012         PC_1      DC      PARALLEL_CLEAR_1                  ;18
2671      Y:000013 Y:000013         PC_2      DC      PARALLEL_CLEAR_2                  ;19
2672                                 SERIAL_IDLE
2673      Y:000014 Y:000014                   DC      SERIAL_IDLE_SPLIT                 ;20
2674      Y:000015 Y:000015         SI_L      DC      SERIAL_IDLE_LEFT                  ;21
2675      Y:000016 Y:000016         SI_R      DC      SERIAL_IDLE_RIGHT                 ;22
2676      Y:000017 Y:000017         BYTE_1    DC      0                                 ;23
2677      Y:000018 Y:000018         BYTE_2    DC      0                                 ;24
2678      Y:000019 Y:000019         SR_L      DC      SERIAL_READ_LEFT                  ;25
2679      Y:00001A Y:00001A         SR_R      DC      SERIAL_READ_RIGHT                 ;26
2680      Y:00001B Y:00001B         P_1       DC      PARALLEL_1                        ;27
2681      Y:00001C Y:00001C         P_2       DC      PARALLEL_2                        ;28
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  tim.asm  Page 51



2682                                 N_PARALLEL_CLEARS
2683      Y:00001D Y:00001D                   DC      1                                 ;29
2684      Y:00001E Y:00001E         SR_S      DC      SERIAL_READ_SPLIT                 ;30
2685      Y:00001F Y:00001F         TMP_1     DC      TMP_PXL_TBL1                      ;31 stuff where I can load a idle without re
setting.
2686      Y:000020 Y:000020         TMP_2     DC      TMP_PXL_TBL2                      ;32
2687      Y:000021 Y:000021         TMP_3     DC      TMP_PXL_TBL3                      ;33
2688      Y:000022 Y:000022         PC_S      DC      PARALLEL_CLEAR_SPLIT              ;34
2689      Y:000023 Y:000023         NSRI      DC      4200
2690   
2691   
2692      Y:000024 Y:000024         INT_TIME  DC      0
2693      Y:000025 Y:000025         TIME1     DC      0
2694      Y:000026 Y:000026         TIME2     DC      0
2695   
2696                                ; These three parameters are read from the READ_TABLE when needed by the
2697                                ;   RDCCD routine as it loops through the required number of boxes
2698      Y:000027 Y:000027         NP_SKIP   DC      0                                 ; Number of rows to skip
2699      Y:000028 Y:000028         NS_SKP1   DC      0                                 ; Number of serials to clear before read
2700      Y:000029 Y:000029         NS_SKP2   DC      0                                 ; Number of serials to clear after read
2701   
2702                                ; Subimage readout parameters. Ten subimage boxes maximum.
2703      Y:00002A Y:00002A         NBOXES    DC      0                                 ; Number of boxes to read
2704      Y:00002B Y:00002B         NR_BIAS   DC      0                                 ; Number of bias pixels to read
2705      Y:00002C Y:00002C         NS_READ   DC      0                                 ; Number of columns in subimage read
2706      Y:00002D Y:00002D         NP_READ   DC      0                                 ; Number of rows in subimage read
2707      Y:00002E Y:00002E         READ_TABLE DC     0,0,0                             ; #1 = Number of rows to clear
2708                                                                                    ; #2 = Number of columns to skip before
2709                                                                                    ;   subimage read
2710                                                                                    ; #3 = Number of columns to clear after
2711                                                                                    ;   subimage clear
2712   
2713                                ; Include the waveform table for the designated type of CCD
2714                                          INCLUDE "sg2_190khz.waveforms.reverse"    ; Readout and clocking waveform file
2715                                ; vim: syntax=asm
2716   
2717                                ; Waveform tables and definitions for the LBNL CCD
2718                                ; This is for a slow, low noise readout
2719                                ; In this version the reset encloses the three serial clocks
2720                                ;**********************************************************************
2721   
2722                                ; U2 and L2 working.
2723                                ; Parallel clocking also needs fixing as shifting to register 1 has
2724                                ; charge under different phases during integration and clocking
2725   
2726                                ; Miscellaneous definitions
2727   
2728      000000                    VIDEO     EQU     $000000                           ; Video processor board select
2729      000000                    VID0      EQU     $000000                           ; Video processor board select
2730      002000                    BIAS      EQU     $002000                           ; Bias Generator board select = 3
2731      003000                    HVBIAS    EQU     $003000                           ; Bias Generator board select = 3
2732      002000                    CLK2      EQU     $002000                           ; Clock driver board select = 2
2733      003000                    CLK3      EQU     $003000                           ; Clock driver board select = 3
2734                                ;CLK4      EQU $004000 ; Clock driver board select = 4
2735                                ;CLK5      EQU $005000 ; Clock driver board select = 5
2736   
2737                                 VIDEO_CONFIG
2738      0C000C                              EQU     $0C000C                           ; WARP = DAC_OUT = ON; H16B, Reset FIFOs
2739      0E0000                    DAC_ADDR  EQU     $0E0000                           ; DAC Channel Address
2740      0F4000                    DAC_RegM  EQU     $0F4000                           ; DAC m Register
2741      0F8000                    DAC_RegC  EQU     $0F8000                           ; DAC c Register
2742      0FC000                    DAC_RegD  EQU     $0FC000                           ; DAC X1 Register
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 52



2743   
2744      -5.000000E+000            VABG      EQU     -5.0                              ; Anti-blooming gate
2745      8.700000E+000             VRSV_MAX  EQU     8.70
2746      000D9A                    DAC_VRSV  EQU     @CVI(((VABG+VRSV_MAX)/VRSV_MAX)*8192-1) ; Bipolar
2747   
2748      000834                    NP_CLR    EQU     2100
2749      001068                    NS_CLR    EQU     4200
2750   
2751                                ; GenIII: if bit #23=1; 22-16 = # of 640 nanos cycles that bits #15-0 held at
2752                                ;                THINK THIS ACTUALLY 320ns...
2753                                ;         if bit #23=0; 22-16 = # of  40 nanos cycles that bits #15-0 held at
2754   
2755   
2756                                ;
2757                                ;  Total pixel time is 5.24us
2758                                ;  Total readout time (4200x2040) 44.9s / # amps  (measured at XXXs)
2759                                ;  Equivilent speed is 190khz
2760                                ;  Max gain difference to default 50khz rate.
2761                                ;
2762   
2763   
2764                                ; Delay numbers for clocking
2765                                ; 180000 is 1 mus, 310000 is 2 mus, 4a0000 is 3 mus, 630000 is 4 mus
2766   
2767      090000                    I_DELAY   EQU     $090000                           ; 9 * 40ns + 40ns = 400ns
2768   
2769                                ; Delay numbers for clocking
2770                                ; Extra 4*40ns (160ns) for commands in pattern
2771      980000                    P_DELAY   EQU     $980000                           ; 24 * 320ns + 40ns = 7720ns
2772   
2773      070000                    S_DELAY   EQU     $070000                           ; Serial register transfer delay
2774                                                                                    ; 7*40ns + 40ns = 320ns (six in pattern)
2775      070000                    SW_DELAY  EQU     $070000                           ; Sum_well  clock delay
2776                                                                                    ; 7*40ns + 40ns = 320ns
2777                                 PRE_SET_DLY
2778      180000                              EQU     $180000                           ; 24*40ns + 40ns = 1000ns
2779                                 POST_SET_DLY
2780      0C0000                              EQU     $0c0000                           ; 12*40ns + 40ns = 520ns
2781                                 DCRST_DELAY
2782      0C0000                              EQU     $0c0000                           ; 12*40ns + 40ns = 520ns
2783   
2784                                ; TODO: Is this a waste of clearing in split readout mode?  Calculate.
2785                                ;NP_CLR  EQU     2048  ; 2040 parallel direction
2786                                ;This doesn't do anything
2787                                ;NP_CLR  EQU     2100  ; 2040 parallel direction
2788                                ;NS_CLR  EQU     4200    ; 4128 in serial direction
2789                                ;NS_CLR  EQU     4128    ; 4128 in serial direction
2790      000064                    SH_DEL    EQU     100
2791   
2792                                ; CHANGE ABOVE TO GENIII TIMING BOARD DELAYS
2793   
2794                                ; Macros to help getting from volts to bits.
2795                                ; The \ in front of NAME substitutes the value of NAME into the variable. (tx)
2796   
2797                                VDEF      MACRO   NAME,BRDTYP,BRDNUM,DAC,ALO,AHI
2798 m 
2799 m                              LO_\NAME  EQU     ALO
2800 m                              HI_\NAME  EQU     AHI
2801 m                              DAC_\NAME EQU     DAC
2802 m                               BRDNUM_\NAME
2803 m                                        EQU     BRDNUM
2804 m                                        IF      @SCP("BRDTYP",'VID')
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 53



2805 m                               BRDTYP_\NAME
2806 m                                        EQU     3
2807 m                                        ELSE
2808 m                               BRDTYP_\NAME
2809 m                                        EQU     0
2810 m                                        ENDIF
2811 m 
2812 m                              ;        MSG     'Defining voltage ',"NAME",' type ',"BRDTYP",' board ',"BRDNUM",' dac ',"DAC",'
 with limits ',"ALO",' ',"AHI"
2813 m                                        ENDM
2814   
2815                                VOLTS     MACRO   NAME,F
2816 m 
2817 m                              DUMMY     SET     @CVI(@MIN(4095,@MAX(0,(F-LO_\NAME)/(HI_\NAME-LO_\NAME)*4096.)))
2818 m                              DUMMY2    SET     @CVI((BRDNUM_\NAME<<20)|(BRDTYP_\NAME<<18)|(DAC_\NAME<<14)|DUMMY)
2819 m                                        DC      DUMMY2
2820 m                                        MSG     'Setting voltage ',"NAME ","F",'V ',DUMMY,DUMMY2
2821 m                                        ENDM
2822   
2823                                ;*********************************************************************
2824                                ;
2825                                ; ; define bias board voltage symbols
2826                                          VDEF    VDDL2,VID,2,0,0.0,-25.0
2839                                          VDEF    VDDU2,VID,2,1,0.0,-25.0
2852                                          VDEF    VDDL1,VID,2,2,0.0,-25.0
2865                                          VDEF    VDDU1,VID,2,3,0.0,-25.0
2878                                          VDEF    VRL2,VID,2,4,0.0,-25.0
2891                                          VDEF    VRU2,VID,2,5,0.0,-25.0
2904                                          VDEF    VRL1,VID,2,6,0.0,-25.0
2917                                          VDEF    VRU1,VID,2,7,0.0,-25.0
2930                                          VDEF    VOGL2,VID,2,8,0.0,5
2943                                          VDEF    VOGU2,VID,2,9,0.0,5
2956                                          VDEF    VOGL1,VID,2,10,0.0,5
2969                                          VDEF    VOGU1,VID,2,11,0.0,5
2982                                          VDEF    VSUB,VID,2,12,0.0,80.0
2995                                          VDEF    RAMP,VID,2,13,0.0,10.0            ;  for ramping p.s.
3008                                ;
3009                                ; ; define clock board symbols bank0
3010                                ;
3011   
3012   
3013                                ; Output video offset parameters
3014                                ;OFFSET0 EQU $2750
3015                                ;OFFSET1 EQU $2500
3016                                ;OFFSET2 EQU $2600
3017                                ;OFFSET3 EQU $2470
3018   
3019                                ;OFFSET0 EQU $26f0
3020                                ;OFFSET1 EQU $2600
3021                                ;OFFSET2 EQU $2370
3022                                ;OFFSET3 EQU $24f0
3023   
3024      001F00                    OFFSET0   EQU     $1f00
3025      001F23                    OFFSET1   EQU     $1f23
3026      001F00                    OFFSET2   EQU     $1f00
3027      001F00                    OFFSET3   EQU     $1f00
3028   
3029                                ; CCD clock voltage
3030      0.000000E+000             ZERO      EQU     0.0                               ; Unused pins
3031      1.300000E+001             Vmax      EQU     +13.0                             ; Clock driver board rails
3032   
3033                                ;LBNL DEFINITIONS
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 54



3034      5.000000E+000             V1_HI     EQU     5.0                               ; Vertical High
3035      -3.000000E+000            V1_LO     EQU     -3.0                              ; Vertical Low
3036      5.000000E+000             V2_HI     EQU     5.0                               ; Vertical High
3037      -3.000000E+000            V2_LO     EQU     -3.0                              ; Vertical Low
3038      5.000000E+000             V3_HI     EQU     5.0                               ; Vertical High
3039      -3.000000E+000            V3_LO     EQU     -3.0                              ; Vertical Low
3040   
3041      5.000000E+000             FS1_HI    EQU     5.0                               ; Vertical High
3042      -3.000000E+000            FS1_LO    EQU     -3.0                              ; Vertical Low
3043      5.000000E+000             FS2_HI    EQU     5.0                               ; Vertical High
3044      -3.000000E+000            FS2_LO    EQU     -3.0                              ; Vertical Low
3045      5.000000E+000             FS3_HI    EQU     5.0                               ; Vertical High
3046      -3.000000E+000            FS3_LO    EQU     -3.0                              ; Vertical Low
3047   
3048      5.000000E+000             T2_HI     EQU     5.0                               ; Transfer gate High
3049      -3.000000E+000            T2_LO     EQU     -3.0                              ; Transfer gate Low
3050      5.000000E+000             T1_HI     EQU     5.0                               ; Transfer gate High
3051      -3.000000E+000            T1_LO     EQU     -3.0                              ; Transfer gate Low
3052   
3053      6.000000E+000             H1U1_L2_HI EQU    +6.0                              ; Horizontal High
3054      -3.900000E+000            H1U1_L2_LO EQU    -3.9                              ; Horizontal Low
3055      6.000000E+000             H2U1_L2_HI EQU    +6.0                              ; Horizontal High
3056      -3.900000E+000            H2U1_L2_LO EQU    -3.9                              ; Horizontal Low
3057      6.000000E+000             H3U1_L2_HI EQU    +6.0                              ; Horizontal High
3058      -3.900000E+000            H3U1_L2_LO EQU    -3.9                              ; Horizontal Low
3059      6.000000E+000             H1U2_L1_HI EQU    +6.0                              ; Horizontal High
3060      -3.900000E+000            H1U2_L1_LO EQU    -3.9                              ; Horizontal Low
3061      6.000000E+000             H2U2_L1_HI EQU    +6.0                              ; Horizontal High
3062      -3.900000E+000            H2U2_L1_LO EQU    -3.9                              ; Horizontal Low
3063      6.000000E+000             H3U2_L1_HI EQU    +6.0                              ; Horizontal High
3064      -3.900000E+000            H3U2_L1_LO EQU    -3.9                              ; Horizontal Low
3065   
3066                                ;H1U1_L2_HI  EQU +0.0 ; Horizontal High
3067                                ;H1U1_L2_LO  EQU -0.0 ; Horizontal Low
3068                                ;H2U1_L2_HI  EQU +0.0 ; HoVR2rizontal High
3069                                ;H2U1_L2_LO  EQU -0.0 ; Horizontal Low
3070                                ;H3U1_L2_HI  EQU +0.0 ; Horizontal High
3071                                ;H3U1_L2_LO  EQU -0.0 ; Horizontal Low
3072                                ;H1U2_L1_HI  EQU +0.0 ; Horizontal High
3073                                ;H1U2_L1_LO  EQU -0.0 ; Horizontal Low
3074                                ;H2U2_L1_HI  EQU +0.0 ; Horizontal High
3075                                ;H2U2_L1_LO  EQU -0.0 ; Horizontal Low
3076                                ;H3U2_L1_HI  EQU +0.0 ; Horizontal High
3077                                ;H3U2_L1_LO  EQU -0.0 ; Horizontal Low
3078                                ;
3079                                ;Put summing wells low for conduction channel
3080      5.000000E+000             SWU_HI    EQU     +5.0                              ; Summing Well High
3081      -5.000000E+000            SWU_LO    EQU     -5.0                              ; Summing Well Low
3082      5.000000E+000             SWL_HI    EQU     +5.0                              ; Summing Well High
3083      -5.000000E+000            SWL_LO    EQU     -5.0                              ; Summing Well Low
3084   
3085      -6.000000E+000            RU_HI     EQU     -6.0                              ; Reset ACTIVE wrong polarity....
3086      -1.000000E-001            RU_LO     EQU     -0.1                              ; Reset INACTIVE
3087      -6.000000E+000            RL_HI     EQU     -6.0                              ; Reset ACTIVE wrong polarity....
3088      -1.000000E-001            RL_LO     EQU     -0.1                              ; Reset INACTIVE
3089   
3090   
3091                                ; Bit defintions for bottom half of clock driver board, CLK2
3092                                ; Clock FS ando vertical regions together
3093                                ; 1,2,4,8,10,20,40,80,100,200,400,800
3094   
3095      000009                    V1_1H     EQU     1|8                               ; V1: Pin 1 -- FS1: pin 4
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 55



3096      000000                    V1_1L     EQU     0
3097   
3098      000012                    V2_1H     EQU     2|$10                             ; V2: Pin 2 -- FS2: pin 5
3099      000000                    V2_1L     EQU     0
3100   
3101      000024                    V3_1H     EQU     4|$20                             ; V3: pin 3 -- FS3: pin 6
3102      000000                    V3_1L     EQU     0
3103   
3104      000240                    V1_2H     EQU     $40|$200                          ; V1: pin 7 -- FS1: pin 10
3105      000000                    V1_2L     EQU     0
3106   
3107      000480                    V2_2H     EQU     $80|$400                          ; V2: pin 8 -- FS2: pin 11
3108      000000                    V2_2L     EQU     0
3109   
3110      000900                    V3_2H     EQU     $100|$800                         ; V3: pin 9 -- FS3: pin 12
3111      000000                    V3_2L     EQU     0
3112   
3113   
3114                                ; Top bank
3115   
3116      000001                    H1_1H     EQU     1                                 ; Horizontal 1 Upper Pin 13
3117      000000                    H1_1L     EQU     0
3118   
3119      000002                    H1_2H     EQU     2                                 ; Horizontal 2 Upper, Pin 14
3120      000000                    H1_2L     EQU     0
3121   
3122      000004                    H1_3H     EQU     4                                 ; Horizontal 3 Upper, Pin 15
3123      000000                    H1_3L     EQU     0
3124   
3125      000008                    H2_1H     EQU     8                                 ; Horizontal 1 Lower, Pin 16
3126      000000                    H2_1L     EQU     0
3127   
3128      000010                    H2_2H     EQU     $10                               ; Horizontal 2 Lower, Pin 17
3129      000000                    H2_2L     EQU     0
3130   
3131      000020                    H2_3H     EQU     $20                               ; Horizontal 3 Lower, Pin 18
3132      000000                    H2_3L     EQU     0
3133   
3134      000040                    SWLH      EQU     $40                               ; Summing Well Upper, Pin 19
3135      000000                    SWLL      EQU     0
3136   
3137      000080                    SWUH      EQU     $80                               ; Summing Well Lower, Pin 33
3138      000000                    SWUL      EQU     0
3139   
3140      000100                    RLH       EQU     $100                              ; Reset Gate Upper, Pin 34
3141      000000                    RLL       EQU     0
3142   
3143      000200                    RUH       EQU     $200                              ; Reset Gate Lower, Pin 35
3144      000000                    RUL       EQU     0
3145   
3146      000400                    T1        EQU     $400                              ; Transfer Gate Upper, Pin 36
3147      000800                    T2        EQU     $800                              ; Transfer Gate Lower, Pin 37
3148   
3149                                ;Both summing wells;
3150                                ;SWUH+SWLH = $80+$100
3151                                ;This was wrong -- should be $40+$80.
3152      000000                    WL        EQU     $00
3153      0000C0                    WH        EQU     $c0
3154   
3155      000000                    RL        EQU     $000
3156      000300                    RH        EQU     $300
3157   
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 56



3158                                ;both transfer gates together
3159      000000                    TL        EQU     $000
3160      000C00                    TH        EQU     $c00
3161   
3162                                ; frame store definitions for clearing out FS region separately.
3163      000008                    FS1_1H    EQU     8
3164      000000                    FS1_1L    EQU     0
3165      000010                    FS2_1H    EQU     $10
3166      000000                    FS2_1L    EQU     0
3167      000020                    FS3_1H    EQU     $20
3168      000000                    FS3_1L    EQU     0
3169      000200                    FS1_2H    EQU     $200
3170      000000                    FS1_2L    EQU     0
3171      000400                    FS2_2H    EQU     $400
3172      000000                    FS2_2L    EQU     0
3173      000800                    FS3_2H    EQU     $800
3174      000000                    FS3_2L    EQU     0
3175                                ; LBNL waveforms
3176   
3177                                ;TODO:  Need to remove references to frame store clocks -- I've bussed
3178                                ;       those together with the verticals in the 1&2 halves.  Fortunately
3179                                ;       the switch state bits select the right thing at the moment...
3180                                ;TODO:  Check first CLK3 line for parallel split is correct.
3181   
3182      Y:000031 Y:000031         CLEAR_FS  DC      END_CLEAR_FS-1
3183      Y:000032 Y:000032                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3184      Y:000033 Y:000033                   DC      CLK2|P_DELAY|FS1_1H|FS2_1L|FS3_1H|FS1_2H|FS2_2L|FS3_2H
3185      Y:000034 Y:000034                   DC      CLK2|P_DELAY|FS1_1L|FS2_1L|FS3_1H|FS1_2H|FS2_2L|FS3_2L
3186      Y:000035 Y:000035                   DC      CLK2|P_DELAY|FS1_1L|FS2_1H|FS3_1H|FS1_2H|FS2_2H|FS3_2L
3187      Y:000036 Y:000036                   DC      CLK2|P_DELAY|FS1_1L|FS2_1H|FS3_1L|FS1_2L|FS2_2H|FS3_2L
3188      Y:000037 Y:000037                   DC      CLK2|P_DELAY|FS1_1H|FS2_1H|FS3_1L|FS1_2L|FS2_2H|FS3_2H
3189      Y:000038 Y:000038                   DC      CLK2|$000000|FS1_1H|FS2_1L|FS3_1L|FS1_2L|FS2_2L|FS3_2H
3190   
3191      Y:000039 Y:000039                   DC      CLK3|P_DELAY|RL|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH
3192                                                                                    ; wfk -  Next line: Add DCRestore,StrtRstInt
 to machine state at end of CLEAR to stabilize baseline (04/04/07)
3193      Y:00003A Y:00003A                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3194                                END_CLEAR_FS
3195   
3196   
3197                                 PARALLEL_SPLIT
3198      Y:00003B Y:00003B                   DC      END_PARALLEL_SPLIT-PARALLEL_SPLIT-1
3199      Y:00003C Y:00003C                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3200      Y:00003D Y:00003D                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3201      Y:00003E Y:00003E                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2H|V2_2L|V3_2L
3202      Y:00003F Y:00003F                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2H|V2_2H|V3_2L
3203      Y:000040 Y:000040                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3204      Y:000041 Y:000041                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2L|V2_2H|V3_2H
3205      Y:000042 Y:000042                   DC      CLK2|$000000|V1_1H|V2_1L|V3_1L|V1_2L|V2_2L|V3_2H
3206      Y:000043 Y:000043                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3207      Y:000044 Y:000044                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3208   
3209                                END_PARALLEL_SPLIT
3210   
3211   
3212                                ;Shift towards register 2
3213      Y:000045 Y:000045         PARALLEL_2 DC     END_PARALLEL_2-PARALLEL_2-1
3214      Y:000046 Y:000046                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3215      Y:000047 Y:000047                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 57



3216      Y:000048 Y:000048                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1L|V1_2H|V2_2L|V3_2L
3217      Y:000049 Y:000049                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2H|V2_2H|V3_2L
3218      Y:00004A Y:00004A                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3219      Y:00004B Y:00004B                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2L|V2_2H|V3_2H
3220      Y:00004C Y:00004C                   DC      CLK2|$000000|V1_1L|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H ; shut TG
3221      Y:00004D Y:00004D                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3222      Y:00004E Y:00004E                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3223                                END_PARALLEL_2
3224   
3225                                ;Shift towards register 1
3226                                ;charge stored under 2&3.  Issue with switching between which register to go to.
3227      Y:00004F Y:00004F         PARALLEL_1 DC     END_PARALLEL_1-PARALLEL_1-1
3228      Y:000050 Y:000050                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3229      Y:000051 Y:000051                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3230      Y:000052 Y:000052                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H
3231      Y:000053 Y:000053                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2L|V2_2H|V3_2H
3232      Y:000054 Y:000054                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3233      Y:000055 Y:000055                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2H|V2_2H|V3_2L
3234      Y:000056 Y:000056                   DC      CLK2|$000000|V1_1H|V2_1L|V3_1L|V1_2H|V2_2L|V3_2L
3235      Y:000057 Y:000057                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3236      Y:000058 Y:000058                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3237                                END_PARALLEL_1
3238   
3239   
3240                                PARALLEL_CLEAR_1
3241      Y:000059 Y:000059                   DC      END_PARALLEL_CLEAR_1-PARALLEL_CLEAR_1-1
3242      Y:00005A Y:00005A                   DC      CLK3|$000000|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3243   
3244      Y:00005B Y:00005B                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3245      Y:00005C Y:00005C                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H
3246      Y:00005D Y:00005D                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2L|V2_2H|V3_2H
3247      Y:00005E Y:00005E                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3248      Y:00005F Y:00005F                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2H|V2_2H|V3_2L
3249      Y:000060 Y:000060                   DC      CLK2|$000000|V1_1H|V2_1L|V3_1L|V1_2H|V2_2L|V3_2L
3250   
3251      Y:000061 Y:000061                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3252                                                                                    ; wfk -  Next line: Add DCRestore,StrtRstInt
 to machine state at end of CLEAR to stabilize baseline (04/04/07)
3253      Y:000062 Y:000062                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3254                                END_PARALLEL_CLEAR_1
3255   
3256                                PARALLEL_CLEAR_2
3257      Y:000063 Y:000063                   DC      END_PARALLEL_CLEAR_2-PARALLEL_CLEAR_2-1
3258      Y:000064 Y:000064                   DC      CLK3|$000000|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3259   
3260      Y:000065 Y:000065                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3261      Y:000066 Y:000066                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1L|V1_2H|V2_2L|V3_2L
3262      Y:000067 Y:000067                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2H|V2_2H|V3_2L
3263      Y:000068 Y:000068                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3264      Y:000069 Y:000069                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2L|V2_2H|V3_2H
3265      Y:00006A Y:00006A                   DC      CLK2|$000000|V1_1L|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H
3266      Y:00006B Y:00006B                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3267   
3268   
3269                                                                                    ; wfk -  Next line: Add DCRestore,StrtRstInt
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 58



 to machine state at end of CLEAR to stabilize baseline (04/04/07)
3270      Y:00006C Y:00006C                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3271                                END_PARALLEL_CLEAR_2
3272   
3273                                ; this parallel split mixes two central rows on the CCD.
3274                                PARALLEL_CLEAR_SPLIT
3275      Y:00006D Y:00006D                   DC      END_PARALLEL_CLEAR_SPLIT-PARALLEL_CLEAR_SPLIT-1
3276      Y:00006E Y:00006E                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3277   
3278      Y:00006F Y:00006F                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3279      Y:000070 Y:000070                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2H|V2_2L|V3_2L
3280      Y:000071 Y:000071                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2H|V2_2H|V3_2L
3281      Y:000072 Y:000072                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3282      Y:000073 Y:000073                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2L|V2_2H|V3_2H
3283      Y:000074 Y:000074                   DC      CLK2|$000000|V1_1H|V2_1L|V3_1L|V1_2L|V2_2L|V3_2H
3284   
3285      Y:000075 Y:000075                   DC      CLK3|P_DELAY|RL|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH
3286                                                                                    ; wfk -  Next line: Add DCRestore,StrtRstInt
 to machine state at end of CLEAR to stabilize baseline (04/04/07)
3287      Y:000076 Y:000076                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3288                                END_PARALLEL_CLEAR_SPLIT
3289   
3290   
3291   
3293                                ;PARALLEL_CLEAR_SPLIT
3294                                ;  DC  END_PARALLEL_CLEAR_SPLIT-PARALLEL_CLEAR_SPLIT-1
3295                                ;  DC  CLK3|$000000|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3296                                ;
3297                                ;  DC  CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3298                                ;  DC  CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2H|V2_2L|V3_2L
3299                                ;  DC  CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2H|V2_2H|V3_2L
3300                                ;  DC  CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3301                                ;  DC  CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2L|V2_2H|V3_2H
3302                                ;  DC  CLK2|P_DELAY|V1_1H|V2_1L|V3_1L|V1_2L|V2_2L|V3_2H
3303                                ;  DC  CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H
3304                                ;  DC  CLK2|$000000|V1_1L|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H
3305                                ;
3306                                ;  DC  CLK3|P_DELAY|RL|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH
3307                                ;  ; wfk -  Next line: Add DCRestore,StrtRstInt to machine state at end of CLEAR to stabilize ba
seline (04/04/07)
3308                                ;  DC  VIDEO+$000000+%1111000  ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wfk add DCRestore,StrtR
stInt
3309                                ;END_PARALLEL_CLEAR_SPLIT
3310   
3311                                ; ARC47:  |xfer|A/D|integ|polarity|not used|not used|rst| (1 => switch open)
3312                                SERIAL_IDLE_LEFT                                    ; Clock serial charge from both L and R ends
3313      Y:000077 Y:000077                   DC      END_SERIAL_IDLE_LEFT-SERIAL_IDLE_LEFT-1
3314      Y:000078 Y:000078                   DC      VIDEO+$000000+%1111000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3315                                ; L2 idle version
3316                                ; 2->3->1->2->3
3317      Y:000079 Y:000079                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3318      Y:00007A Y:00007A                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3319      Y:00007B Y:00007B                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3320      Y:00007C Y:00007C                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3321      Y:00007D Y:00007D                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3322      Y:00007E Y:00007E                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3323      Y:00007F Y:00007F                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3324   
3325   
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 59



3326      Y:000080 Y:000080                   DC      CLK3|$0000000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;dummy for trans
mit delay
3327      Y:000081 Y:000081                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3328      Y:000082 Y:000082                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3329      Y:000083 Y:000083                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3330      Y:000084 Y:000084                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3331      Y:000085 Y:000085                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3332      Y:000086 Y:000086                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3333      Y:000087 Y:000087                   DC      VIDEO+DCRST_DELAY+%0010001        ;  Now sit around for at least 520ns while t
he conversion happens
3334                                END_SERIAL_IDLE_LEFT
3335   
3336   
3337                                ; ARC47:  |xfer|A/D|integ|polarity|not used|not used|rst| (1 => switch open)
3338                                SERIAL_IDLE_LEFT_NO_POL                             ; Clock serial charge from both L and R ends
3339      Y:000088 Y:000088                   DC      END_SERIAL_IDLE_LEFT_NO_POL-SERIAL_IDLE_LEFT_NO_POL-1
3340      Y:000089 Y:000089                   DC      VIDEO+$000000+%1111000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3341   
3342                                ; L2 idle version
3343                                ; 2->3->1->2->3
3344      Y:00008A Y:00008A                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3345      Y:00008B Y:00008B                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3L|H2_1H|H2_2L|H2_3L|WL|TH ;h2->hi
3346      Y:00008C Y:00008C                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2H|H1_3L|H2_1H|H2_2H|H2_3L|WL|TH ;h1->lo
3347      Y:00008D Y:00008D                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3L|H2_1L|H2_2H|H2_3L|WL|TH ;h3->hi
3348      Y:00008E Y:00008E                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3H|H2_1L|H2_2H|H2_3H|WL|TH ;h2->lo
3349      Y:00008F Y:00008F                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2L|H1_3H|H2_1L|H2_2L|H2_3H|WL|TH ;h1->hi
3350   
3351      Y:000090 Y:000090                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3352   
3353   
3354      Y:000091 Y:000091                   DC      CLK3|$0000000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;dummy for trans
mit delay
3355      Y:000092 Y:000092                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3356      Y:000093 Y:000093                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3357      Y:000094 Y:000094                   DC      VIDEO+$000000+%0011001            ; Stop Integrate and sel inverting int.
3358      Y:000095 Y:000095                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi -- charge d
ump
3359   
3360                                                                                    ;SW going low here suggests that no charge w
ill leak over OG barrier onto sense node.
3361      Y:000096 Y:000096                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3362      Y:000097 Y:000097                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3363      Y:000098 Y:000098                   DC      VIDEO+DCRST_DELAY+%0011001        ; ,NonInv  ;mF to do ADC sampling before res
etting
3364   
3365                                ;  DC  CLK3|POST_SET_DLY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL ;SW->lo
3366                                END_SERIAL_IDLE_LEFT_NO_POL
3367   
3368                                ; ARC47:  |xfer|A/D|integ|polarity|not used|not used|rst| (1 => switch open)
3369                                SERIAL_IDLE_RIGHT                                   ; Clock serial charge from both L and R ends
3370      Y:000099 Y:000099                   DC      END_SERIAL_IDLE_RIGHT-SERIAL_IDLE_RIGHT-1
3371      Y:00009A Y:00009A                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3372   
3373                                ; L2 read version
3374                                ; 2->3->1->2->3
3375      Y:00009B Y:00009B                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h2->lo,SW->lo,Reset
_On
3376      Y:00009C Y:00009C                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3377      Y:00009D Y:00009D                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->hi
3378      Y:00009E Y:00009E                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->l
3379      Y:00009F Y:00009F                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->hi
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 60



3380      Y:0000A0 Y:0000A0                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->lo
3381      Y:0000A1 Y:0000A1                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->hi, Reset_O
ff|Delay
3382   
3383   
3384      Y:0000A2 Y:0000A2                   DC      CLK3|$0000000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;dummy for trans
mit delay
3385      Y:0000A3 Y:0000A3                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3386      Y:0000A4 Y:0000A4                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3387      Y:0000A5 Y:0000A5                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3388      Y:0000A6 Y:0000A6                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi -- charge d
ump
3389   
3390                                                                                    ;SW going low here suggests that no charge w
ill leak over OG barrier onto sense node.
3391      Y:0000A7 Y:0000A7                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3392      Y:0000A8 Y:0000A8                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3393      Y:0000A9 Y:0000A9                   DC      VIDEO+$000000+%0110001            ; StopIntegrator
3394      Y:0000AA Y:0000AA                   DC      VIDEO+DCRST_DELAY+%0110001        ; ADCLatch,NonInv  ;mF to do ADC sampling be
fore resetting
3395   
3396                                ;  DC  CLK3|POST_SET_DLY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL ;SW->lo
3397   
3398                                END_SERIAL_IDLE_RIGHT
3399   
3400   
3401                                ; ARC47:  |xfer|A/D|integ|polarity|not used|not used|rst| (1 => switch open)
3402                                SERIAL_IDLE_RIGHT_NO_POL                            ; Clock serial charge from both L and R ends
3403      Y:0000AB Y:0000AB                   DC      END_SERIAL_IDLE_RIGHT_NO_POL-SERIAL_IDLE_RIGHT_NO_POL-1
3404      Y:0000AC Y:0000AC                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3405   
3406                                ; L2 read version
3407                                ; 2->3->1->2->3
3408      Y:0000AD Y:0000AD                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h2->lo,SW->lo,Reset
_On
3409      Y:0000AE Y:0000AE                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3410      Y:0000AF Y:0000AF                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->hi
3411      Y:0000B0 Y:0000B0                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->l
3412      Y:0000B1 Y:0000B1                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->hi
3413      Y:0000B2 Y:0000B2                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->lo
3414      Y:0000B3 Y:0000B3                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->hi, Reset_O
ff|Delay
3415   
3416   
3417      Y:0000B4 Y:0000B4                   DC      CLK3|$0000000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;dummy for trans
mit delay
3418      Y:0000B5 Y:0000B5                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3419      Y:0000B6 Y:0000B6                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3420      Y:0000B7 Y:0000B7                   DC      VIDEO+$000000+%0011001            ; Stop Integrate and sel inverting int.
3421      Y:0000B8 Y:0000B8                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi -- charge d
ump
3422   
3423                                                                                    ;SW going low here suggests that no charge w
ill leak over OG barrier onto sense node.
3424      Y:0000B9 Y:0000B9                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3425      Y:0000BA Y:0000BA                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3426      Y:0000BB Y:0000BB                   DC      VIDEO+$000000+%0011001            ; StopIntegrator
3427      Y:0000BC Y:0000BC                   DC      VIDEO+DCRST_DELAY+%0111001        ; ADCLatch,NonInv  ;mF to do ADC sampling be
fore resetting
3428   
3429                                ;  DC  CLK3|POST_SET_DLY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL ;SW->lo
3430   
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 61



3431                                END_SERIAL_IDLE_RIGHT_NO_POL
3432   
3433                                ; ARC47:  |xfer|A/D|integ|polarity|not used|not used|rst| (1 => switch open)
3434                                SERIAL_IDLE_SPLIT
3435      Y:0000BD Y:0000BD                   DC      END_SERIAL_IDLE_SPLIT-SERIAL_IDLE_SPLIT-1
3436      Y:0000BE Y:0000BE                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3437   
3438                                ; split read version:
3439      Y:0000BF Y:0000BF                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3440   
3441                                ;  DC  CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3442                                ;  DC  CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3443                                ;  DC  CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3444                                ;  DC  CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3445                                ;  DC  CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3446   
3447      Y:0000C0 Y:0000C0                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2L|H1_3H|H2_1H|H2_2L|H2_3L|WL|TH ;h2->hi
3448      Y:0000C1 Y:0000C1                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3H|H2_1H|H2_2H|H2_3L|WL|TH ;h1->lo
3449      Y:0000C2 Y:0000C2                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3L|H2_1L|H2_2H|H2_3L|WL|TH ;h3->hi
3450      Y:0000C3 Y:0000C3                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2H|H1_3L|H2_1L|H2_2H|H2_3H|WL|TH ;h2->lo
3451      Y:0000C4 Y:0000C4                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3L|H2_1L|H2_2L|H2_3H|WL|TH ;h1->hi
3452   
3453   
3454      Y:0000C5 Y:0000C5                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3455   
3456      Y:0000C6 Y:0000C6                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;dummy for transmit 
delay
3457      Y:0000C7 Y:0000C7                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3458      Y:0000C8 Y:0000C8                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3459      Y:0000C9 Y:0000C9                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3460      Y:0000CA Y:0000CA                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3461      Y:0000CB Y:0000CB                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3462      Y:0000CC Y:0000CC                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3463      Y:0000CD Y:0000CD                   DC      VIDEO+$000000+%0010001            ; StopIntegrator
3464      Y:0000CE Y:0000CE                   DC      VIDEO+DCRST_DELAY+%0110001        ; ADCLatch,NonInv  ;mF to do ADC sampling be
fore resetting
3465                                END_SERIAL_IDLE_SPLIT
3466   
3467                                ; ARC47:  |xfer|A/D|integ|polarity|not used|not used|rst| (1 => switch open)
3468                                SERIAL_IDLE_SPLIT_NO_POL
3469      Y:0000CF Y:0000CF                   DC      END_SERIAL_IDLE_SPLIT_NO_POL-SERIAL_IDLE_SPLIT_NO_POL-1
3470      Y:0000D0 Y:0000D0                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3471   
3472                                ; split read version:
3473      Y:0000D1 Y:0000D1                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3474      Y:0000D2 Y:0000D2                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3475      Y:0000D3 Y:0000D3                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3476      Y:0000D4 Y:0000D4                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3477      Y:0000D5 Y:0000D5                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3478      Y:0000D6 Y:0000D6                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3479      Y:0000D7 Y:0000D7                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3480   
3481      Y:0000D8 Y:0000D8                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;dummy for transmit 
delay
3482      Y:0000D9 Y:0000D9                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3483      Y:0000DA Y:0000DA                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3484      Y:0000DB Y:0000DB                   DC      VIDEO+$000000+%0011001            ; Stop Integrate and sel inverting int.
3485      Y:0000DC Y:0000DC                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3486      Y:0000DD Y:0000DD                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3487      Y:0000DE Y:0000DE                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 62



3488      Y:0000DF Y:0000DF                   DC      VIDEO+$000000+%0011001            ; StopIntegrator
3489      Y:0000E0 Y:0000E0                   DC      VIDEO+DCRST_DELAY+%0111001        ; ADCLatch,NonInv  ;mF to do ADC sampling be
fore resetting
3490                                END_SERIAL_IDLE_SPLIT_NO_POL
3491   
3492   
3493                                ;start binning waveforms
3494                                CCD_RESET                                           ;Used for binning only
3495      Y:0000E1 Y:0000E1                   DC      END_CCD_RESET-CCD_RESET-1
3496      Y:0000E2 Y:0000E2                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3497                                END_CCD_RESET
3498   
3499                                SERIAL_CLOCK_L                                      ;"NORMAL" clocking
3500      Y:0000E3 Y:0000E3                   DC      END_SERIAL_CLOCK_L-SERIAL_CLOCK_L-1
3501      Y:0000E4 Y:0000E4                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3502      Y:0000E5 Y:0000E5                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3503      Y:0000E6 Y:0000E6                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3504      Y:0000E7 Y:0000E7                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3505      Y:0000E8 Y:0000E8                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3506      Y:0000E9 Y:0000E9                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3507      Y:0000EA Y:0000EA                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3508      Y:0000EB Y:0000EB                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3509                                END_SERIAL_CLOCK_L
3510   
3511                                SERIAL_CLOCK_R                                      ;"REVERSE" clocking
3512      Y:0000EC Y:0000EC                   DC      END_SERIAL_CLOCK_R-SERIAL_CLOCK_R-1
3513      Y:0000ED Y:0000ED                   DC      VIDEO+$000000+%1011000            ; NonInv,DCRestore,StrtRstInt.
3514      Y:0000EE Y:0000EE                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3515      Y:0000EF Y:0000EF                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h2->hi
3516      Y:0000F0 Y:0000F0                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h1->lo
3517      Y:0000F1 Y:0000F1                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3518      Y:0000F2 Y:0000F2                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h2->lo
3519      Y:0000F3 Y:0000F3                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h1->hi
3520      Y:0000F4 Y:0000F4                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3521                                END_SERIAL_CLOCK_R
3522   
3523                                SERIAL_CLOCK_SPLIT                                  ;"SPLIT" clocking
3524      Y:0000F5 Y:0000F5                   DC      END_SERIAL_CLOCK_SPLIT-SERIAL_CLOCK_SPLIT-1
3525      Y:0000F6 Y:0000F6                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3526      Y:0000F7 Y:0000F7                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3527      Y:0000F8 Y:0000F8                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3528      Y:0000F9 Y:0000F9                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3529      Y:0000FA Y:0000FA                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3530      Y:0000FB Y:0000FB                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3531      Y:0000FC Y:0000FC                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3532      Y:0000FD Y:0000FD                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3533                                END_SERIAL_CLOCK_SPLIT
3534   
3535   
3536                                VIDEO_PROCESS
3537      Y:0000FE Y:0000FE                   DC      END_VIDEO_PROCESS-VIDEO_PROCESS-1
3538                                ;SXMIT  DC  $00F000     ; Transmit A/D data to host
3539      Y:0000FF Y:0000FF                   DC      VIDEO+$000000+%1011000            ; StopDCRestore and StopResetIntegrator
3540      Y:000100 Y:000100                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3541      Y:000101 Y:000101                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3542      Y:000102 Y:000102                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3543      Y:000103 Y:000103                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3544      Y:000104 Y:000104                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3545      Y:000105 Y:000105                   DC      VIDEO+$000000+%0010001            ; StopResetIntegrator
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 63



3546      Y:000106 Y:000106                   DC      VIDEO+DCRST_DELAY+%0110001        ; ADCLatch,NonInv  ;mF to do ADC sampeling b
evore resetting
3547                                END_VIDEO_PROCESS
3548                                ;end binning waveforms
3549   
3550   
3551                                ; Video processor bit definition
3552                                ;      xfer, A/D, integ, Pol+, Pol-, DCrestore, rst   (1 => switch open)
3553   
3554                                ; These are the three reading tables. Make sure they're all the same length
3555                                ; 2->3->1->2
3556                                SERIAL_READ_LEFT
3557      Y:000107 Y:000107                   DC      END_SERIAL_READ_LEFT-SERIAL_READ_LEFT-1
3558      Y:000108 Y:000108                   DC      VIDEO+$000000+%1111000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3559      Y:000109 Y:000109                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3560      Y:00010A Y:00010A                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3561      Y:00010B Y:00010B                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3562      Y:00010C Y:00010C                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3563      Y:00010D Y:00010D                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3564      Y:00010E Y:00010E                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3565      Y:00010F Y:00010F                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3566      Y:000110 Y:000110         SXL       DC      $00F000                           ;Transmit a/d data to host
3567      Y:000111 Y:000111                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3568      Y:000112 Y:000112                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3569      Y:000113 Y:000113                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3570      Y:000114 Y:000114                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3571      Y:000115 Y:000115                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3572      Y:000116 Y:000116                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3573      Y:000117 Y:000117                   DC      VIDEO+DCRST_DELAY+%0010001        ;  Now sit around for at least 520ns while t
he conversion happens
3574      Y:000118 Y:000118                   DC      VIDEO+$40000+%0010001             ;  Now sit around for at least 520ns while t
he conversion happens
3575                                END_SERIAL_READ_LEFT
3576   
3577                                SERIAL_READ_SPLIT
3578      Y:000119 Y:000119                   DC      END_SERIAL_READ_SPLIT-SERIAL_READ_SPLIT-1
3579      Y:00011A Y:00011A                   DC      VIDEO+$000000+%1111000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3580      Y:00011B Y:00011B                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3581   
3582      Y:00011C Y:00011C                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2L|H1_3H|H2_1H|H2_2L|H2_3L|WL|TH ;h2->hi
3583      Y:00011D Y:00011D                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3H|H2_1H|H2_2H|H2_3L|WL|TH ;h1->lo
3584      Y:00011E Y:00011E                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3L|H2_1L|H2_2H|H2_3L|WL|TH ;h3->hi
3585      Y:00011F Y:00011F                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2H|H1_3L|H2_1L|H2_2H|H2_3H|WL|TH ;h2->lo
3586      Y:000120 Y:000120                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3L|H2_1L|H2_2L|H2_3H|WL|TH ;h1->hi
3587   
3588                                ;  DC  CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3589                                ;  DC  CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3590                                ;  DC  CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3591                                ;  DC  CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3592                                ;  DC  CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3593   
3594   
3595      Y:000121 Y:000121                   DC      CLK3|PRE_SET_DLY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;Reset_Off|Delay
3596      Y:000122 Y:000122         SXRL      DC      $00F0C2                           ;Transmit a/d data to host
3597      Y:000123 Y:000123                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3598      Y:000124 Y:000124                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3599      Y:000125 Y:000125                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3600      Y:000126 Y:000126                   DC      CLK3|SW_DELAY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WH|TH ;SW->hi
3601      Y:000127 Y:000127                   DC      CLK3|POST_SET_DLY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;SW->lo
3602      Y:000128 Y:000128                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 64



3603      Y:000129 Y:000129                   DC      VIDEO+DCRST_DELAY+%0010001        ; Sit around whilst sampling.
3604      Y:00012A Y:00012A                   DC      VIDEO+$50000+%0010001             ;  Now sit around for at least 520ns while t
he conversion happens
3605                                END_SERIAL_READ_SPLIT
3606   
3607   
3608                                SERIAL_READ_SPLIT_SPECIAL
3609      Y:00012B Y:00012B                   DC      END_SERIAL_READ_SPLIT_SPECIAL-SERIAL_READ_SPLIT_SPECIAL-1
3610      Y:00012C Y:00012C                   DC      VIDEO+$000000+%1111000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3611      Y:00012D Y:00012D                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3612   
3613      Y:00012E Y:00012E                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2L|H1_3H|H2_1H|H2_2L|H2_3L|WL|TH ;h2->hi
3614      Y:00012F Y:00012F                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3H|H2_1H|H2_2H|H2_3L|WL|TH ;h1->lo
3615      Y:000130 Y:000130                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3L|H2_1L|H2_2H|H2_3L|WL|TH ;h3->hi
3616      Y:000131 Y:000131                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2H|H1_3L|H2_1L|H2_2H|H2_3H|WL|TH ;h2->lo
3617      Y:000132 Y:000132                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3L|H2_1L|H2_2L|H2_3H|WL|TH ;h1->hi
3618   
3619      Y:000133 Y:000133                   DC      CLK3|PRE_SET_DLY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;Reset_Off|Delay
3620      Y:000134 Y:000134                   DC      $00F041                           ;Transmit a/d data to host
3621      Y:000135 Y:000135                   DC      CLK3|$20000|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;Reset_Off|Delay
3622      Y:000136 Y:000136                   DC      $00F082                           ; get the other ADC
3623      Y:000137 Y:000137                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3624      Y:000138 Y:000138                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3625      Y:000139 Y:000139                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3626      Y:00013A Y:00013A                   DC      CLK3|SW_DELAY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WH|TH ;SW->hi
3627      Y:00013B Y:00013B                   DC      CLK3|POST_SET_DLY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;SW->lo
3628      Y:00013C Y:00013C                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3629      Y:00013D Y:00013D                   DC      VIDEO+DCRST_DELAY+%0010001        ; Sit around whilst sampling.
3630      Y:00013E Y:00013E                   DC      VIDEO+$50000+%0010001             ;  Now sit around for at least 520ns while t
he conversion happens
3631                                END_SERIAL_READ_SPLIT_SPECIAL
3632   
3633   
3634                                ; 2->1->3->2
3635                                SERIAL_READ_RIGHT
3636      Y:00013F Y:00013F                   DC      END_SERIAL_READ_RIGHT-SERIAL_READ_RIGHT-1
3637      Y:000140 Y:000140                   DC      VIDEO+$000000+%1111000            ; NonInv,DCRestore,StrtRstInt.
3638      Y:000141 Y:000141                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3639      Y:000142 Y:000142                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h2->hi
3640      Y:000143 Y:000143                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h1->lo
3641      Y:000144 Y:000144                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3642      Y:000145 Y:000145                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h2->lo
3643      Y:000146 Y:000146                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h1->hi
3644      Y:000147 Y:000147                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3645      Y:000148 Y:000148         SXR       DC      $00F041                           ;Transmit a/d data to host
3646      Y:000149 Y:000149                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3647      Y:00014A Y:00014A                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3648      Y:00014B Y:00014B                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3649      Y:00014C Y:00014C                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3650      Y:00014D Y:00014D                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3651      Y:00014E Y:00014E                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3652      Y:00014F Y:00014F                   DC      VIDEO+DCRST_DELAY+%0010001        ; Wait for sampling
3653      Y:000150 Y:000150                   DC      VIDEO+$30000+%0010001             ;
3654                                END_SERIAL_READ_RIGHT
3655   
3656   
3657                                ; These are the three skipping tables. Make sure they're all the same length
3658                                SERIAL_SKIP_LEFT                                    ; Serial clocking waveform for skipping left
3659      Y:000151 Y:000151                   DC      END_SERIAL_SKIP_LEFT-SERIAL_SKIP_LEFT-1
3660      Y:000152 Y:000152                   DC      VIDEO+$000000+%1011000            ; Change nearly everything
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 65



3661      Y:000153 Y:000153                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h2->hi
3662      Y:000154 Y:000154                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h1->lo
3663      Y:000155 Y:000155                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3664      Y:000156 Y:000156                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h2->lo
3665      Y:000157 Y:000157                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h1->hi
3666      Y:000158 Y:000158                   DC      CLK3|S_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3667      Y:000159 Y:000159                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3668                                END_SERIAL_SKIP_LEFT
3669   
3670                                SERIAL_SKIP_RIGHT                                   ; Serial clocking waveform for skipping righ
t
3671      Y:00015A Y:00015A                   DC      END_SERIAL_SKIP_RIGHT-SERIAL_SKIP_RIGHT-1
3672      Y:00015B Y:00015B                   DC      VIDEO+$000000+%1011000            ; Change nearly everything
3673      Y:00015C Y:00015C                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3674      Y:00015D Y:00015D                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3675      Y:00015E Y:00015E                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3676      Y:00015F Y:00015F                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3677      Y:000160 Y:000160                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3678      Y:000161 Y:000161                   DC      CLK3|S_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3679      Y:000162 Y:000162                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3680                                END_SERIAL_SKIP_RIGHT
3681   
3682                                SERIAL_SKIP_SPLIT                                   ; Serial clocking waveform for skipping both
 ends
3683      Y:000163 Y:000163                   DC      END_SERIAL_SKIP_SPLIT-SERIAL_SKIP_SPLIT-1
3684      Y:000164 Y:000164                   DC      VIDEO+$000000+%1011000            ; Change nearly everything
3685      Y:000165 Y:000165                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1L|H1_2L|H1_3H|WL|TH ;h2->hi
3686      Y:000166 Y:000166                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1L|H1_2H|H1_3H|WL|TH ;h1->lo
3687      Y:000167 Y:000167                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3688      Y:000168 Y:000168                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1H|H1_2H|H1_3L|WL|TH ;h2->lo
3689      Y:000169 Y:000169                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3L|WL|TH ;h1->hi
3690      Y:00016A Y:00016A                   DC      CLK3|S_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3691      Y:00016B Y:00016B                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3692                                END_SERIAL_SKIP_SPLIT
3693   
3695                                ; ORG Y:$1C0,Y:$1C0   ; Download address
3697                                VSUBN
3698                                          VOLTS   VSUB,45.0                         ; Vsub  0.0 140 V, pin #
**** 3703 [sg2_190khz.waveforms.reverse 808]: Setting voltage VSUB 45.0V 23043082496
3704      Y:00016D Y:00016D         ERHI      DC      ERHI_END-ERHI-1
3705                                          VOLTS   VSUB,0                            ; Vsub  0.0 140 V, pin #
**** 3710 [sg2_190khz.waveforms.reverse 810]: Setting voltage VSUB 0V 03080192
3711                                ; VOLTS V1_HI,9   ; Vertical High
3712                                ; VOLTS V1_LO,9   ; Vertical Low
3713                                ; VOLTS V2_HI,9   ; Vertical High
3714                                ; VOLTS V2_LO,9   ; Vertical Low
3715                                ; VOLTS V3_HI,9   ; Vertical High
3716                                ; VOLTS V3_LO,9   ; Vertical Low
3717                                ; VOLTS FS1_HI,9    ; Vertical High
3718                                ; VOLTS FS1_LO,9    ; Vertical Low
3719                                ; VOLTS FS2_HI,9    ; Vertical High
3720                                ; VOLTS FS2_LO,9    ; Vertical Low
3721                                ; VOLTS FS3_HI,9    ; Vertical High
3722                                ; VOLTS FS3_LO,9    ; Vertical Low
3723      Y:00016F Y:00016F                   DC      $200100+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #1, Vertical Clock 1
3724      Y:000170 Y:000170                   DC      $200200+@CVI((9+Vmax)/(2*Vmax)*255)
3725      Y:000171 Y:000171                   DC      $200400+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #2, Vertical Clock 2
3726      Y:000172 Y:000172                   DC      $200800+@CVI((9+Vmax)/(2*Vmax)*255)
3727      Y:000173 Y:000173                   DC      $202000+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #3, Vertical Clock 3
3728      Y:000174 Y:000174                   DC      $204000+@CVI((9+Vmax)/(2*Vmax)*255)
3729      Y:000175 Y:000175                   DC      $208000+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #4, Frame Store 1
3730      Y:000176 Y:000176                   DC      $210000+@CVI((9+Vmax)/(2*Vmax)*255)
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 66



3731      Y:000177 Y:000177                   DC      $220100+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #5, Frame Store 2
3732      Y:000178 Y:000178                   DC      $220200+@CVI((9+Vmax)/(2*Vmax)*255)
3733      Y:000179 Y:000179                   DC      $220400+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #6, Frame Store 3
3734      Y:00017A Y:00017A                   DC      $220800+@CVI((9+Vmax)/(2*Vmax)*255)
3735      Y:00017B Y:00017B                   DC      $222000+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #7, Transfer Gate 2
3736      Y:00017C Y:00017C                   DC      $224000+@CVI((9+Vmax)/(2*Vmax)*255)
3737      Y:00017D Y:00017D                   DC      $228000+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #8, Transger Gate 1
3738      Y:00017E Y:00017E                   DC      $230000+@CVI((9+Vmax)/(2*Vmax)*255)
3739      Y:00017F Y:00017F                   DC      $240100+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #9, Unused
3740      Y:000180 Y:000180                   DC      $240200+@CVI((9+Vmax)/(2*Vmax)*255)
3741      Y:000181 Y:000181                   DC      $240400+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #10, Unused
3742      Y:000182 Y:000182                   DC      $240800+@CVI((9+Vmax)/(2*Vmax)*255)
3743      Y:000183 Y:000183                   DC      $242000+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #11, Unused
3744      Y:000184 Y:000184                   DC      $244000+@CVI((9+Vmax)/(2*Vmax)*255)
3745      Y:000185 Y:000185                   DC      $248000+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #12, Unused
3746      Y:000186 Y:000186                   DC      $250000+@CVI((9+Vmax)/(2*Vmax)*255)
3747      Y:000187 Y:000187         ERHI_END  DC      EPUR-ERHI_END-1
3748                                ; VOLTS V1_HI,5.0 ; Vertical High
3749                                ; VOLTS V1_LO,-3.0  ; Vertical Low
3750                                ; VOLTS V2_HI,5.0 ; Vertical High
3751                                ; VOLTS V2_LO,-3.0  ; Vertical Low
3752                                ; VOLTS V3_HI,5.0 ; Vertical High
3753                                ; VOLTS V3_LO,-3.0  ; Vertical Low
3754                                ; VOLTS FS1_HI,5.0  ; Vertical High
3755                                ; VOLTS FS1_LO,-3.0 ; Vertical Low
3756                                ; VOLTS FS2_HI,5.0  ; Vertical High
3757                                ; VOLTS FS2_LO,-3.0 ; Vertical Low
3758                                ; VOLTS FS3_HI,5.0  ; Vertical High
3759                                ; VOLTS FS3_LO,-3.0 ; Vertical Low
3760                                ;Return to normal voltages
3761      Y:000188 Y:000188                   DC      $200100+@CVI((V1_HI+Vmax)/(2*Vmax)*255) ; Pin #1, Vertical Clock 1
3762      Y:000189 Y:000189                   DC      $200200+@CVI((V1_LO+Vmax)/(2*Vmax)*255)
3763      Y:00018A Y:00018A                   DC      $200400+@CVI((V2_HI+Vmax)/(2*Vmax)*255) ; Pin #2, Vertical Clock 2
3764      Y:00018B Y:00018B                   DC      $200800+@CVI((V2_LO+Vmax)/(2*Vmax)*255)
3765      Y:00018C Y:00018C                   DC      $202000+@CVI((V3_HI+Vmax)/(2*Vmax)*255) ; Pin #3, Vertical Clock 3
3766      Y:00018D Y:00018D                   DC      $204000+@CVI((V3_LO+Vmax)/(2*Vmax)*255)
3767      Y:00018E Y:00018E                   DC      $208000+@CVI((V1_HI+Vmax)/(2*Vmax)*255) ; Pin #4, Frame Store 1
3768      Y:00018F Y:00018F                   DC      $210000+@CVI((V1_LO+Vmax)/(2*Vmax)*255)
3769      Y:000190 Y:000190                   DC      $220100+@CVI((V2_HI+Vmax)/(2*Vmax)*255) ; Pin #5, Frame Store 2
3770      Y:000191 Y:000191                   DC      $220200+@CVI((V2_LO+Vmax)/(2*Vmax)*255)
3771      Y:000192 Y:000192                   DC      $220400+@CVI((V3_HI+Vmax)/(2*Vmax)*255) ; Pin #6, Frame Store 3
3772      Y:000193 Y:000193                   DC      $220800+@CVI((V3_LO+Vmax)/(2*Vmax)*255)
3773      Y:000194 Y:000194                   DC      $222000+@CVI((V1_HI+Vmax)/(2*Vmax)*255) ; Pin #7, Transfer Gate 2
3774      Y:000195 Y:000195                   DC      $224000+@CVI((V1_LO+Vmax)/(2*Vmax)*255)
3775      Y:000196 Y:000196                   DC      $228000+@CVI((V2_HI+Vmax)/(2*Vmax)*255) ; Pin #8, Transger Gate 1
3776      Y:000197 Y:000197                   DC      $230000+@CVI((V2_LO+Vmax)/(2*Vmax)*255)
3777      Y:000198 Y:000198                   DC      $240100+@CVI((V3_HI+Vmax)/(2*Vmax)*255) ; Pin #9, Unused
3778      Y:000199 Y:000199                   DC      $240200+@CVI((V3_LO+Vmax)/(2*Vmax)*255)
3779      Y:00019A Y:00019A                   DC      $240400+@CVI((FS1_HI+Vmax)/(2*Vmax)*255) ; Pin #10, Unused
3780      Y:00019B Y:00019B                   DC      $240800+@CVI((FS1_LO+Vmax)/(2*Vmax)*255)
3781      Y:00019C Y:00019C                   DC      $242000+@CVI((FS2_HI+Vmax)/(2*Vmax)*255) ; Pin #11, Unused
3782      Y:00019D Y:00019D                   DC      $244000+@CVI((FS2_LO+Vmax)/(2*Vmax)*255)
3783      Y:00019E Y:00019E                   DC      $248000+@CVI((FS3_HI+Vmax)/(2*Vmax)*255) ; Pin #12, Unused
3784      Y:00019F Y:00019F                   DC      $250000+@CVI((FS3_LO+Vmax)/(2*Vmax)*255)
3785   
3786      Y:0001A0 Y:0001A0                   DC      $2A0100+@CVI((RL_HI+Vmax)/(2*Vmax)*255) ; Pin #34, Reset Gate Upper
3787      Y:0001A1 Y:0001A1                   DC      $2A0200+@CVI((RL_LO+Vmax)/(2*Vmax)*255)
3788      Y:0001A2 Y:0001A2                   DC      $2A0400+@CVI((RU_HI+Vmax)/(2*Vmax)*255) ; Pin #35, Reset Gate Lower
3789      Y:0001A3 Y:0001A3                   DC      $2A0800+@CVI((RU_LO+Vmax)/(2*Vmax)*255)
3790   
3791      Y:0001A4 Y:0001A4         EPUR      DC      EPUR_END-EPUR-1
3792      Y:0001A5 Y:0001A5                   DC      $200100+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #1, Vertical Clock 1
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 67



3793      Y:0001A6 Y:0001A6                   DC      $200200+@CVI((-9+Vmax)/(2*Vmax)*255)
3794      Y:0001A7 Y:0001A7                   DC      $200400+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #2, Vertical Clock 2
3795      Y:0001A8 Y:0001A8                   DC      $200800+@CVI((-9+Vmax)/(2*Vmax)*255)
3796      Y:0001A9 Y:0001A9                   DC      $202000+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #3, Vertical Clock 3
3797      Y:0001AA Y:0001AA                   DC      $204000+@CVI((-9+Vmax)/(2*Vmax)*255)
3798      Y:0001AB Y:0001AB                   DC      $208000+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #4, Frame Store 1
3799      Y:0001AC Y:0001AC                   DC      $210000+@CVI((-9+Vmax)/(2*Vmax)*255)
3800      Y:0001AD Y:0001AD                   DC      $220100+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #5, Frame Store 2
3801      Y:0001AE Y:0001AE                   DC      $220200+@CVI((-9+Vmax)/(2*Vmax)*255)
3802      Y:0001AF Y:0001AF                   DC      $220400+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #6, Frame Store 3
3803      Y:0001B0 Y:0001B0                   DC      $220800+@CVI((-9+Vmax)/(2*Vmax)*255)
3804      Y:0001B1 Y:0001B1                   DC      $222000+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #7, Transfer Gate 2
3805      Y:0001B2 Y:0001B2                   DC      $224000+@CVI((-9+Vmax)/(2*Vmax)*255)
3806      Y:0001B3 Y:0001B3                   DC      $228000+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #8, Transger Gate 1
3807      Y:0001B4 Y:0001B4                   DC      $230000+@CVI((-9+Vmax)/(2*Vmax)*255)
3808      Y:0001B5 Y:0001B5                   DC      $240100+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #9, Unused
3809      Y:0001B6 Y:0001B6                   DC      $240200+@CVI((-9+Vmax)/(2*Vmax)*255)
3810      Y:0001B7 Y:0001B7                   DC      $240400+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #10, Unused
3811      Y:0001B8 Y:0001B8                   DC      $240800+@CVI((-9+Vmax)/(2*Vmax)*255)
3812      Y:0001B9 Y:0001B9                   DC      $242000+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #11, Unused
3813      Y:0001BA Y:0001BA                   DC      $244000+@CVI((-9+Vmax)/(2*Vmax)*255)
3814      Y:0001BB Y:0001BB                   DC      $248000+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #12, Unused
3815      Y:0001BC Y:0001BC                   DC      $250000+@CVI((-9+Vmax)/(2*Vmax)*255)
3816   
3817      Y:0001BD Y:0001BD                   DC      $2A0100+@CVI((-6+Vmax)/(2*Vmax)*255) ; Pin #34, Reset Gate Upper
3818      Y:0001BE Y:0001BE                   DC      $2A0200+@CVI((-6+Vmax)/(2*Vmax)*255)
3819      Y:0001BF Y:0001BF                   DC      $2A0400+@CVI((-6+Vmax)/(2*Vmax)*255) ; Pin #35, Reset Gate Lower
3820      Y:0001C0 Y:0001C0                   DC      $2A0800+@CVI((-6+Vmax)/(2*Vmax)*255)
3821   
3822                                EPUR_END
3823   
3824                                ; Code for ARC32 = universal clock driver board
3825      Y:0001C1 Y:0001C1         DACS      DC      END_DACS-DACS-1
3826      Y:0001C2 Y:0001C2                   DC      $2A0080                           ; DAC = unbuffered mode
3827   
3828      Y:0001C3 Y:0001C3                   DC      $200100+@CVI((V1_HI+Vmax)/(2*Vmax)*255) ; Pin #1, Vertical Clock 1
3829      Y:0001C4 Y:0001C4                   DC      $200200+@CVI((V1_LO+Vmax)/(2*Vmax)*255)
3830      Y:0001C5 Y:0001C5                   DC      $200400+@CVI((V2_HI+Vmax)/(2*Vmax)*255) ; Pin #2, Vertical Clock 2
3831      Y:0001C6 Y:0001C6                   DC      $200800+@CVI((V2_LO+Vmax)/(2*Vmax)*255)
3832      Y:0001C7 Y:0001C7                   DC      $202000+@CVI((V3_HI+Vmax)/(2*Vmax)*255) ; Pin #3, Vertical Clock 3
3833      Y:0001C8 Y:0001C8                   DC      $204000+@CVI((V3_LO+Vmax)/(2*Vmax)*255)
3834      Y:0001C9 Y:0001C9                   DC      $208000+@CVI((V1_HI+Vmax)/(2*Vmax)*255) ; Pin #4, Frame Store 1
3835      Y:0001CA Y:0001CA                   DC      $210000+@CVI((V1_LO+Vmax)/(2*Vmax)*255)
3836      Y:0001CB Y:0001CB                   DC      $220100+@CVI((V2_HI+Vmax)/(2*Vmax)*255) ; Pin #5, Frame Store 2
3837      Y:0001CC Y:0001CC                   DC      $220200+@CVI((V2_LO+Vmax)/(2*Vmax)*255)
3838      Y:0001CD Y:0001CD                   DC      $220400+@CVI((V3_HI+Vmax)/(2*Vmax)*255) ; Pin #6, Frame Store 3
3839      Y:0001CE Y:0001CE                   DC      $220800+@CVI((V3_LO+Vmax)/(2*Vmax)*255)
3840      Y:0001CF Y:0001CF                   DC      $222000+@CVI((V1_HI+Vmax)/(2*Vmax)*255) ; Pin #7, Transfer Gate 2
3841      Y:0001D0 Y:0001D0                   DC      $224000+@CVI((V1_LO+Vmax)/(2*Vmax)*255)
3842      Y:0001D1 Y:0001D1                   DC      $228000+@CVI((V2_HI+Vmax)/(2*Vmax)*255) ; Pin #8, Transger Gate 1
3843      Y:0001D2 Y:0001D2                   DC      $230000+@CVI((V2_LO+Vmax)/(2*Vmax)*255)
3844   
3845      Y:0001D3 Y:0001D3                   DC      $240100+@CVI((V3_HI+Vmax)/(2*Vmax)*255) ; Pin #9, Unused
3846      Y:0001D4 Y:0001D4                   DC      $240200+@CVI((V3_LO+Vmax)/(2*Vmax)*255)
3847      Y:0001D5 Y:0001D5                   DC      $240400+@CVI((FS1_HI+Vmax)/(2*Vmax)*255) ; Pin #10, Unused
3848      Y:0001D6 Y:0001D6                   DC      $240800+@CVI((FS1_LO+Vmax)/(2*Vmax)*255)
3849      Y:0001D7 Y:0001D7                   DC      $242000+@CVI((FS2_HI+Vmax)/(2*Vmax)*255) ; Pin #11, Unused
3850      Y:0001D8 Y:0001D8                   DC      $244000+@CVI((FS2_LO+Vmax)/(2*Vmax)*255)
3851      Y:0001D9 Y:0001D9                   DC      $248000+@CVI((FS3_HI+Vmax)/(2*Vmax)*255) ; Pin #12, Unused
3852      Y:0001DA Y:0001DA                   DC      $250000+@CVI((FS3_LO+Vmax)/(2*Vmax)*255)
3853   
3854      Y:0001DB Y:0001DB                   DC      $260100+@CVI((H1U2_L1_HI+Vmax)/(2*Vmax)*255) ; Pin #13, Horizontal 1 Upper
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 68



3855      Y:0001DC Y:0001DC                   DC      $260200+@CVI((H1U2_L1_LO+Vmax)/(2*Vmax)*255)
3856      Y:0001DD Y:0001DD                   DC      $260400+@CVI((H2U2_L1_HI+Vmax)/(2*Vmax)*255) ; Pin #14, Horizontal 2 Upper
3857      Y:0001DE Y:0001DE                   DC      $260800+@CVI((H2U2_L1_LO+Vmax)/(2*Vmax)*255)
3858      Y:0001DF Y:0001DF                   DC      $262000+@CVI((H3U2_L1_HI+Vmax)/(2*Vmax)*255) ; Pin #15, Horizontal 3 Upper
3859      Y:0001E0 Y:0001E0                   DC      $264000+@CVI((H3U2_L1_LO+Vmax)/(2*Vmax)*255)
3860      Y:0001E1 Y:0001E1                   DC      $268000+@CVI((H1U1_L2_HI+Vmax)/(2*Vmax)*255) ; Pin #16, Horizontal 1 Lower
3861      Y:0001E2 Y:0001E2                   DC      $270000+@CVI((H1U1_L2_LO+Vmax)/(2*Vmax)*255)
3862      Y:0001E3 Y:0001E3                   DC      $280100+@CVI((H2U1_L2_HI+Vmax)/(2*Vmax)*255) ; Pin #17, Horizontal 2 Lower
3863      Y:0001E4 Y:0001E4                   DC      $280200+@CVI((H2U1_L2_LO+Vmax)/(2*Vmax)*255)
3864      Y:0001E5 Y:0001E5                   DC      $280400+@CVI((H3U1_L2_HI+Vmax)/(2*Vmax)*255) ; Pin #18, Horizontal 3 Lower
3865      Y:0001E6 Y:0001E6                   DC      $280800+@CVI((H3U1_L2_LO+Vmax)/(2*Vmax)*255)
3866      Y:0001E7 Y:0001E7                   DC      $282000+@CVI((SWL_HI+Vmax)/(2*Vmax)*255) ; Pin #19, Summing Well Upper
3867      Y:0001E8 Y:0001E8                   DC      $284000+@CVI((SWL_LO+Vmax)/(2*Vmax)*255)
3868      Y:0001E9 Y:0001E9                   DC      $288000+@CVI((SWU_HI+Vmax)/(2*Vmax)*255) ; Pin #33, Summing Well Lower
3869      Y:0001EA Y:0001EA                   DC      $290000+@CVI((SWU_LO+Vmax)/(2*Vmax)*255)
3870      Y:0001EB Y:0001EB                   DC      $2A0100+@CVI((RL_HI+Vmax)/(2*Vmax)*255) ; Pin #34, Reset Gate Upper
3871      Y:0001EC Y:0001EC                   DC      $2A0200+@CVI((RL_LO+Vmax)/(2*Vmax)*255)
3872      Y:0001ED Y:0001ED                   DC      $2A0400+@CVI((RU_HI+Vmax)/(2*Vmax)*255) ; Pin #35, Reset Gate Lower
3873      Y:0001EE Y:0001EE                   DC      $2A0800+@CVI((RU_LO+Vmax)/(2*Vmax)*255)
3874      Y:0001EF Y:0001EF                   DC      $2A2000+@CVI((T1_HI+Vmax)/(2*Vmax)*255) ; Pin #36, Unused
3875      Y:0001F0 Y:0001F0                   DC      $2A4000+@CVI((T1_LO+Vmax)/(2*Vmax)*255)
3876      Y:0001F1 Y:0001F1                   DC      $2A8000+@CVI((T2_HI+Vmax)/(2*Vmax)*255) ; Pin #37, Unused
3877      Y:0001F2 Y:0001F2                   DC      $2B0000+@CVI((T2_LO+Vmax)/(2*Vmax)*255)
3878   
3879   
3880                                ; DC bias voltages for the LBL CCD chip
3881                                          VOLTS   VSUB,45.0                         ; Vsub  0.0 140 V
**** 3886 [sg2_190khz.waveforms.reverse 981]: Setting voltage VSUB 45.0V 23043082496
3887                                          VOLTS   RAMP,5.0                          ; Vsub  AVG RAMP RATE
**** 3892 [sg2_190khz.waveforms.reverse 982]: Setting voltage RAMP 5.0V 20483098624
3893                                          VOLTS   VDDL2,-22.0                       ; Vdd  -5.1 -25V
**** 3898 [sg2_190khz.waveforms.reverse 983]: Setting voltage VDDL2 -22.0V 36042887188
3899                                          VOLTS   VDDU2,-22.0                       ; Vdd  -5.1 -25V
**** 3904 [sg2_190khz.waveforms.reverse 984]: Setting voltage VDDU2 -22.0V 36042903572
3905                                          VOLTS   VDDL1,-22.0                       ; Vdd  -5.1 -25V
**** 3910 [sg2_190khz.waveforms.reverse 985]: Setting voltage VDDL1 -22.0V 36042919956
3911                                          VOLTS   VDDU1,-22.0                       ; Vdd  -5.1 -25V
**** 3916 [sg2_190khz.waveforms.reverse 986]: Setting voltage VDDU1 -22.0V 36042936340
3917                                          VOLTS   VRL2,-12.5                        ; Vr   -5.1 -25V
**** 3922 [sg2_190khz.waveforms.reverse 987]: Setting voltage VRL2 -12.5V 20482951168
3923                                          VOLTS   VRU2,-12.5                        ; Vr   -5.1 -25V
**** 3928 [sg2_190khz.waveforms.reverse 988]: Setting voltage VRU2 -12.5V 20482967552
3929                                          VOLTS   VRL1,-12.5                        ; Vr   -5.1 -25V
**** 3934 [sg2_190khz.waveforms.reverse 989]: Setting voltage VRL1 -12.5V 20482983936
3935                                          VOLTS   VRU1,-12.5                        ; Vr   -5.1 -25V
**** 3940 [sg2_190khz.waveforms.reverse 990]: Setting voltage VRU1 -12.5V 20483000320
3941                                          VOLTS   VOGL2,4.00                        ; Vopg  -10  10 V
**** 3946 [sg2_190khz.waveforms.reverse 991]: Setting voltage VOGL2 4.00V 32763017932
3947                                          VOLTS   VOGU2,2.50                        ; Vopg  -10  10 V
**** 3952 [sg2_190khz.waveforms.reverse 992]: Setting voltage VOGU2 2.50V 20483033088
3953                                          VOLTS   VOGL1,2.50                        ; Vopg  -10  10 V
**** 3958 [sg2_190khz.waveforms.reverse 993]: Setting voltage VOGL1 2.50V 20483049472
3959                                          VOLTS   VOGU1,2.50                        ; Vopg  -10  10 V
**** 3964 [sg2_190khz.waveforms.reverse 994]: Setting voltage VOGU1 2.50V 20483065856
3965   
3966                                ;GAIN_SETTING  DC       VID0+$0D000E                    ; Gain of 0.25 (from 0 to $F,one of 16 p
ossible values)
3967                                 GAIN_SETTING
3968      Y:000201 Y:000201                   DC      VID0+$0D0000                      ; Gain of 3.75 (from 0 to $F,one of 16 possi
ble values)
3969   
3970                                ;Initialize the ARC-47 DAC for video offsets, board #0
Motorola DSP56300 Assembler  Version 6.3.4   13-04-16  20:57:25  sg2_190khz.waveforms.reverse  Page 69



3971      Y:000202 Y:000202                   DC      VID0+DAC_ADDR+$000014
3972      Y:000203 Y:000203                   DC      VID0+DAC_RegD+OFFSET0
3973      Y:000204 Y:000204                   DC      VID0+DAC_ADDR+$000015
3974      Y:000205 Y:000205                   DC      VID0+DAC_RegD+OFFSET1
3975      Y:000206 Y:000206                   DC      VID0+DAC_ADDR+$000016
3976      Y:000207 Y:000207                   DC      VID0+DAC_RegD+OFFSET2
3977      Y:000208 Y:000208                   DC      VID0+DAC_ADDR+$000017
3978      Y:000209 Y:000209                   DC      VID0+DAC_RegD+OFFSET3
3979   
3980   
3981                                ;  DC   VID0+DAC_ADDR+$00000C           ; Vabg,pin 5
3982                                ;  DC   VID0+DAC_RegD+DAC_VRSV
3983                                ;  DC   VID0+DAC_ADDR+$00000D           ; Vrsv1,pin 47
3984                                ;  DC   VID0+DAC_RegD+DAC_VRSV
3985                                ;  DC   VID0+DAC_ADDR+$00000E           ; Vrsv2,pin 27
3986                                ;  DC   VID0+DAC_RegD+DAC_VRSV
3987                                ;  DC   VID0+DAC_ADDR+$00000F           ; Vrsv3,pin 6
3988                                ;  DC   VID0+DAC_RegD+DAC_VRSV
3989   
3990   
3991                                END_DACS
3992   
3993   
3994                                ; Pixel table generated in "timCCD.asm"
3995      Y:00020A Y:00020A         PXL_TBL   DC      0
3996   
3997   
3998   
3999      Y:00023D Y:00023D                   ORG     Y:@LCV(L)+50,Y:@LCV(L)+50
4000   
4001                                 TMP_PXL_TBL1
4002      Y:00023D Y:00023D                   DC      0
4003   
4004      Y:000270 Y:000270                   ORG     Y:@LCV(L)+50,Y:@LCV(L)+50
4005   
4006                                 TMP_PXL_TBL2
4007      Y:000270 Y:000270                   DC      0
4008   
4009      Y:0002A3 Y:0002A3                   ORG     Y:@LCV(L)+50,Y:@LCV(L)+50
4010   
4011                                 TMP_PXL_TBL3
4012      Y:0002A3 Y:0002A3                   DC      0
4013   
4014                                 END_APPLICATON_Y_MEMORY
4015      0002A4                              EQU     @LCV(L)
4016   
4017   
4018                                ; End of program
4019                                          END

0    Errors
0    Warnings


