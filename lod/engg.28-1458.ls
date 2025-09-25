Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  tim.asm  Page 1



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timhdr.asm  Page 2



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timhdr.asm  Page 3



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timhdr.asm  Page 4



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 5



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 6



309    
310                                 ; *******************  Command Processing  ******************
311    
312                                 ; Read the header and check it for self-consistency
313       P:000054 P:000054 609F00  START     MOVE              X:<IDL_ADR,R0
314       P:000055 P:000055 018FA0            JSET    #TIM_BIT,X:TCSR0,EXPOSING         ; If exposing go check the timer
                            0003D4
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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 7



362       P:000083 P:000083 302800            MOVE              #<COM_TBL,R0            ; Get the command table starting address
363       P:000084 P:000084 062F80            DO      #NUM_COM,END_COM                  ; Loop over the command table
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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 8



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 9



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 10



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 11



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 12



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 13



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 14



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 15



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 16



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 17



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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timboot.asm  Page 18



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
964       P:000233 P:000233 5EA400            MOVE                          Y:<NSRI,A   ; NSERIALS_READ = NSR
965       P:000234 P:000234 0A0085            JCLR    #SPLIT_S,X:STATUS,*+3
                            000237
966       P:000236 P:000236 200022            ASR     A                                 ; Split serials requires / 2
967       P:000237 P:000237 000000            NOP
968    
969       P:000238 P:000238 06CE00            DO      A,IDL1                            ; Loop over number of pixels per line
                            000245
970       P:00023A P:00023A 68F000            MOVE                          Y:SERIAL_IDLE,R0 ; Serial transfer on pixel
                            000015
971                                           CLOCK                                     ; Go to it
975       P:000240 P:000240 330700            MOVE              #COM_BUF,R3
976       P:000241 P:000241 0D00A5            JSR     <GET_RCV                          ; Check for FO or SSI commands
977       P:000242 P:000242 0E0245            JCC     <NO_COM                           ; Continue IDLE if no commands received
978       P:000243 P:000243 00008C            ENDDO
979       P:000244 P:000244 0C005D            JMP     <PRC_RCV                          ; Go process header and command
980       P:000245 P:000245 000000  NO_COM    NOP
981                                 IDL1
982       P:000246 P:000246 061E40            DO      Y:<N_PARALLEL_CLEARS,PAR
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
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  tim.asm  Page 19



996       P:00024E P:00024E 0B00AD            JSSET   #ST_SYNC,X:STATUS,SYNCH_CONTROLLER ; Sync up two controllers
                            000807
997    
998                                 ; Add user-input to built-in serial skip
999       P:000250 P:000250 4CAD00            MOVE                          Y:<NS_SKIP,X0
1000      P:000251 P:000251 5EB200            MOVE                          Y:<NSEXTENDED,A
1001      P:000252 P:000252 200040            ADD     X0,A
1002      P:000253 P:000253 000000            NOP
1003      P:000254 P:000254 5C2E00            MOVE                          A1,Y:<NS_SKP1 ; total number of serial pre-skips
1004      P:000255 P:000255 5C2F00            MOVE                          A1,Y:<NS_SKP2 ; total number of serial post-skips
1005   
1006                                ; Calculate some readout parameters
1007      P:000256 P:000256 5EB300            MOVE                          Y:<NBOXES,A ; NBOXES = 0 => full image readout
1008      P:000257 P:000257 000000            NOP
1009      P:000258 P:000258 200003            TST     A
1010      P:000259 P:000259 0E2262            JNE     <SUB_IMG
1011      P:00025A P:00025A 5C2C00            MOVE                          A1,Y:<NP_SKIP ; Zero these all out
1012      P:00025B P:00025B 5E8100            MOVE                          Y:<NSR,A    ; NSERIALS_READ = NSR
1013      P:00025C P:00025C 0A0085            JCLR    #SPLIT_S,X:STATUS,*+3
                            00025F
1014      P:00025E P:00025E 200022            ASR     A                                 ; Split serials requires / 2
1015      P:00025F P:00025F 000000            NOP
1016      P:000260 P:000260 5E0A00            MOVE                          A,Y:<NSERIALS_READ ; Number of columns in each subimage
1017      P:000261 P:000261 0C026A            JMP     <WT_CLK
1018   
1019                                ; Loop over the required number of subimage boxes
1020                                SUB_IMG
1021      P:000262 P:000262 67F400            MOVE              #BOI_TABLE,R7           ; Parameter table for subimage readout
                            000037
1022      P:000264 P:000264 063340            DO      Y:<NBOXES,L_NBOXES                ; Loop over number of boxes
                            0002D8
1023      P:000266 P:000266 4CDF00            MOVE                          Y:(R7)+,X0
1024      P:000267 P:000267 4C2C00            MOVE                          X0,Y:<NP_SKIP
1025      P:000268 P:000268 4CDF00            MOVE                          Y:(R7)+,X0
1026      P:000269 P:000269 4C3600            MOVE                          X0,Y:<NP_READ
1027   
1028                                ; Start the loop for parallel shifting desired number of lines
1029                                ;WT_CLK JSR     <GENERATE_SERIAL_WAVEFORM
1030      P:00026A P:00026A 000000  WT_CLK    NOP                                       ; not using the binning waveform generation 
at the moment.
1031      P:00026B P:00026B 0D0464            JSR     <WAIT_TO_FINISH_CLOCKING
1032   
1033                                ; Skip over the required number of rows for subimage readout
1034      P:00026C P:00026C 5EAC00            MOVE                          Y:<NP_SKIP,A ; Number of rows to skip
1035      P:00026D P:00026D 200003            TST     A
1036      P:00026E P:00026E 0EA276            JEQ     <CLR_SR
1037      P:00026F P:00026F 062C40            DO      Y:<NP_SKIP,L_PSKP
                            000275
1038      P:000271 P:000271 689000            MOVE                          Y:<PARALLEL,R0 ; Couldn't this be above the start of the
 do loop?
1039                                          CLOCK
1043                                L_PSKP
1044   
1045                                ; Clear out the accumulated charge from the serial shift register
1046                                CLR_SR
1047      P:000276 P:000276 060340            DO      Y:<NSCLR,L_CLRSR                  ; Loop over number of pixels to skip
                            00027C
1048      P:000278 P:000278 688F00            MOVE                          Y:<SERIAL_SKIP,R0
1049                                          CLOCK                                     ; Go clock out the CCD charge
1053                                L_CLRSR                                             ; Do loop restriction
1054   
1055                                ; Parallel shift the image into the serial shift register
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  tim.asm  Page 20



1056      P:00027D P:00027D 5E8200            MOVE                          Y:<NPR,A    ; Number of rows set by host computer
1057      P:00027E P:00027E 000000            NOP
1058      P:00027F P:00027F 5FB300            MOVE                          Y:<NBOXES,B ; NBOXES = 0 => full image readout
1059      P:000280 P:000280 20000B            TST     B
1060      P:000281 P:000281 0EA283            JEQ     *+2
1061      P:000282 P:000282 5EB600            MOVE                          Y:<NP_READ,A ; If NBOXES .NE. 0 use subimage table
1062      P:000283 P:000283 000000            NOP
1063   
1064      P:000284 P:000284 0A0086            JCLR    #SPLIT_P,X:STATUS,P_CLK
                            000289
1065      P:000286 P:000286 5E8200            MOVE                          Y:<NPR,A
1066      P:000287 P:000287 200022            ASR     A
1067      P:000288 P:000288 000000            NOP
1068   
1069                                ; This is the main loop over each line to be read out
1070   
1071      P:000289 P:000289 06CC00  P_CLK     DO      A1,LPR                            ; Number of rows to read out
                            0002D7
1072   
1073                                ; Exercise the parallel clocks, including binning if needed
1074      P:00028B P:00028B 060640            DO      Y:<NPBIN,L_PBIN
                            000291
1075      P:00028D P:00028D 689000            MOVE                          Y:<PARALLEL,R0
1076                                          CLOCK
1080                                L_PBIN
1081   
1082                                ; Check for a command once per line. Only the ABORT command should be issued.
1083      P:000292 P:000292 330700            MOVE              #COM_BUF,R3
1084      P:000293 P:000293 0D00A5            JSR     <GET_RCV                          ; Was a command received?
1085      P:000294 P:000294 0E029E            JCC     <CONTINUE_READ                    ; If no, continue reading out
1086      P:000295 P:000295 0C005D            JMP     <PRC_RCV                          ; If yes, go process it
1087   
1088                                ; Abort the readout currently underway
1089      P:000296 P:000296 0A0084  ABR_RDC   JCLR    #ST_RDC,X:<STATUS,ABORT_EXPOSURE
                            000412
1090      P:000298 P:000298 00008C            ENDDO                                     ; Properly terminate readout loop
1091      P:000299 P:000299 5EB300            MOVE                          Y:<NBOXES,A ; NBOXES = 0 => full image readout
1092      P:00029A P:00029A 200003            TST     A
1093      P:00029B P:00029B 0EA29D            JEQ     *+2
1094      P:00029C P:00029C 00008C            ENDDO                                     ; Properly terminate readout loop
1095      P:00029D P:00029D 0C0412            JMP     <ABORT_EXPOSURE
1096   
1097                                ; Skip over NS_SKP1 columns for subimage readout
1098                                CONTINUE_READ
1099      P:00029E P:00029E 5EAE00            MOVE                          Y:<NS_SKP1,A ; Number of columns to skip
1100      P:00029F P:00029F 200003            TST     A
1101      P:0002A0 P:0002A0 0EF2A8            JLE     <L_READ
1102      P:0002A1 P:0002A1 062E40            DO      Y:<NS_SKP1,L_SKP1                 ; Number of waveform entries total
                            0002A7
1103      P:0002A3 P:0002A3 688F00            MOVE                          Y:<SERIAL_SKIP,R0 ; Waveform table starting address
1104                                          CLOCK                                     ; Go clock out the CCD charge
1108                                L_SKP1
1109   
1110                                ; Finally read some real pixels
1111                                L_READ
1112      P:0002A8 P:0002A8 060140            DO      Y:<NSR,L_RD
                            0002B9
1113      P:0002AA P:0002AA 5E8500            MOVE                          Y:<NSBIN,A
1114      P:0002AB P:0002AB 0140C4            SUB     #>1,A
                            000001
1115      P:0002AD P:0002AD 000000            NOP
1116      P:0002AE P:0002AE 06CC00            DO      A1,SER_BIN_LOOP
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  tim.asm  Page 21



                            0002B4
1117      P:0002B0 P:0002B0 68A800            MOVE                          Y:<SERIAL_BIN,R0
1118                                          CLOCK
1122                                SER_BIN_LOOP
1123      P:0002B5 P:0002B5 688E00            MOVE                          Y:<SERIAL_READ,R0
1124                                          CLOCK                                     ; Go clock out the CCD charge
1128                                L_RD
1129   
1130                                ; Skip over NS_SKP2 columns if needed for subimage readout
1131      P:0002BA P:0002BA 5EAF00            MOVE                          Y:<NS_SKP2,A ; Number of columns to skip
1132      P:0002BB P:0002BB 200003            TST     A
1133      P:0002BC P:0002BC 0EF2C4            JLE     <L_BIAS
1134      P:0002BD P:0002BD 062F40            DO      Y:<NS_SKP2,L_SKP2
                            0002C3
1135      P:0002BF P:0002BF 688F00            MOVE                          Y:<SERIAL_SKIP,R0 ; Waveform table starting address
1136                                          CLOCK                                     ; Go clock out the CCD charge
1140                                L_SKP2
1141   
1142                                ; And read the bias pixels if in subimage readout mode
1143                                L_BIAS
1144      P:0002C4 P:0002C4 5EB300            MOVE                          Y:<NBOXES,A ; NBOXES = 0 => full image readout
1145      P:0002C5 P:0002C5 200003            TST     A
1146      P:0002C6 P:0002C6 0EF2D7            JLE     <END_ROW
1147      P:0002C7 P:0002C7 5EB400            MOVE                          Y:<NR_BIAS,A ; NR_BIAS = 0 => no bias pixels
1148      P:0002C8 P:0002C8 200003            TST     A
1149      P:0002C9 P:0002C9 0EF2D7            JLE     <END_ROW
1150      P:0002CA P:0002CA 0A0085            JCLR    #SPLIT_S,X:STATUS,*+3
                            0002CD
1151      P:0002CC P:0002CC 200022            ASR     A                                 ; Split serials require / 2
1152      P:0002CD P:0002CD 000000            NOP
1153      P:0002CE P:0002CE 06CC00            DO      A1,L_BRD                          ; Number of pixels to read out
                            0002D5
1154      P:0002D0 P:0002D0 60F400            MOVE              #PXL_TBL,R0
                            000285
1155                                          CLOCK                                     ; Go clock out the CCD charg
1159      P:0002D6 P:0002D6 000000  L_BRD     NOP
1160      P:0002D7 P:0002D7 000000  END_ROW   NOP
1161      P:0002D8 P:0002D8 000000  LPR       NOP                                       ; End of parallel loop
1162      P:0002D9 P:0002D9 000000  L_NBOXES  NOP                                       ; End of subimage boxes loop
1163   
1164                                ; Restore the controller to non-image data transfer and idling if necessary
1165      P:0002DA P:0002DA 0A0082  RDC_END   JCLR    #IDLMODE,X:<STATUS,NO_IDL         ; Don't idle after readout
                            0002E0
1166      P:0002DC P:0002DC 60F400            MOVE              #IDLE,R0
                            000233
1167      P:0002DE P:0002DE 601F00            MOVE              R0,X:<IDL_ADR
1168      P:0002DF P:0002DF 0C02E3            JMP     <RDC_E
1169      P:0002E0 P:0002E0 60F400  NO_IDL    MOVE              #NO_PAR_IDLE,R0
                            0003AD
1170      P:0002E2 P:0002E2 601F00            MOVE              R0,X:<IDL_ADR
1171      P:0002E3 P:0002E3 0D0464  RDC_E     JSR     <WAIT_TO_FINISH_CLOCKING
1172      P:0002E4 P:0002E4 0A0004            BCLR    #ST_RDC,X:<STATUS                 ; Set status to not reading out
1173      P:0002E5 P:0002E5 0C0054            JMP     <START
1174   
1175                                ; ******  Include many routines not directly needed for readout  *******
1176                                          INCLUDE "timCCDmisc.asm"
1177                                ; Miscellaneous CCD control routines, common to all detector types
1178   
1179                                ; test to see if the power is on on the backplane.
1180                                POWER_CHECK
1181                                CHECK_LVEN
1182      P:0002E6 P:0002E6 0A8982            JCLR    #LVEN,X:HDR,CHECK_HVEN
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 22



                            0002E8
1183                                CHECK_HVEN
1184      P:0002E8 P:0002E8 0A8983            JCLR    #HVEN,X:HDR,PWR_CHECK_OK
                            0002EC
1185      P:0002EA P:0002EA 270000            MOVE              #0,Y1                   ; send a 1 back if the power is on
1186      P:0002EB P:0002EB 0C0090            JMP     <FINISH1
1187                                                                                    ; jump here is power is ok
1188                                PWR_CHECK_OK
1189      P:0002EC P:0002EC 270100            MOVE              #1,Y1                   ; send a 1 back if the power is on
1190      P:0002ED P:0002ED 0C0090            JMP     <FINISH1
1191   
1192                                POWER_OFF
1193      P:0002EE P:0002EE 0D0337            JSR     <CLEAR_SWITCHES                   ; Clear all analog switches
1194      P:0002EF P:0002EF 0A8922            BSET    #LVEN,X:HDR
1195      P:0002F0 P:0002F0 0A8923            BSET    #HVEN,X:HDR
1196      P:0002F1 P:0002F1 0A000F            BCLR    #POWERST,X:<STATUS                ; Set the power state in the X: status word
1197      P:0002F2 P:0002F2 0C008F            JMP     <FINISH
1198   
1199                                ; Execute the power-on cycle, as a command
1200                                POWER_ON
1201      P:0002F3 P:0002F3 0D0337            JSR     <CLEAR_SWITCHES                   ; Clear all analog switches
1202      P:0002F4 P:0002F4 0D0309            JSR     <PON                              ; Turn on the power control board
1203      P:0002F5 P:0002F5 0A8980            JCLR    #PWROK,X:HDR,PWR_ERR              ; Test if the power turned on properly
                            000306
1204      P:0002F7 P:0002F7 0D031C            JSR     <SET_BIASES                       ; Turn on the DC bias supplies
1205      P:0002F8 P:0002F8 0D05A8            JSR     <SEL_OS                           ; Set up readout parameters
1206      P:0002F9 P:0002F9 60F400            MOVE              #IDLE,R0                ; Put controller in IDLE state
                            000233
1207                                ;       MOVE    #TST_RCV,R0             ; Put controller in non-IDLE state
1208      P:0002FB P:0002FB 601F00            MOVE              R0,X:<IDL_ADR
1209      P:0002FC P:0002FC 0A002F            BSET    #POWERST,X:<STATUS                ; Set the power state bit in the X: status w
ord
1210      P:0002FD P:0002FD 0A0002            BCLR    #IDLMODE,X:<STATUS                ; no idle after readout
1211   
1212                                                                                    ; get the gain setting and put it into the a
ppropriate place in Y memory
1213      P:0002FE P:0002FE 5EF000            MOVE                          Y:GAIN_SETTING,A
                            00027C
1214      P:000300 P:000300 240D00            MOVE              #$0D0000,X0
1215      P:000301 P:000301 200044            SUB     X0,A
1216      P:000302 P:000302 000000            NOP
1217      P:000303 P:000303 5E0000            MOVE                          A,Y:<GAIN
1218   
1219                                ; !!!   MOVE    #$1064,X0
1220                                ; !!!   MOVE    X0,X:<STATUS
1221                                ; !!!   JSR     <SEL_OS
1222      P:000304 P:000304 0A002F            BSET    #POWERST,X:<STATUS                ; Set the power state bit in the X: status w
ord
1223      P:000305 P:000305 0C008F            JMP     <FINISH
1224   
1225                                ; The power failed to turn on because of an error on the power control board
1226      P:000306 P:000306 0A8922  PWR_ERR   BSET    #LVEN,X:HDR                       ; Turn off the low voltage enable line
1227      P:000307 P:000307 0A8923            BSET    #HVEN,X:HDR                       ; Turn off the high voltage enable line
1228      P:000308 P:000308 0C008D            JMP     <ERROR
1229   
1230                                ; As a subroutine, turn on the low voltages (+/- 6.5V, +/- 16.5V) and delay
1231      P:000309 P:000309 0A8902  PON       BCLR    #LVEN,X:HDR                       ; Set these signals to DSP outputs
1232      P:00030A P:00030A 44F400            MOVE              #2000000,X0
                            1E8480
1233      P:00030C P:00030C 06C400            DO      X0,*+3                            ; Wait 20 millisec for settling
                            00030E
1234      P:00030E P:00030E 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 23



1235   
1236                                ; Turn on the high +36 volt power line and then delay
1237      P:00030F P:00030F 0A8903            BCLR    #HVEN,X:HDR                       ; HVEN = Low => Turn on +36V
1238      P:000310 P:000310 44F400            MOVE              #10000000,X0
                            989680
1239      P:000312 P:000312 06C400            DO      X0,*+3                            ; Wait 100 millisec for settling
                            000314
1240      P:000314 P:000314 000000            NOP
1241      P:000315 P:000315 00000C            RTS
1242   
1243   
1244                                RAW_COMMAND
1245      P:000316 P:000316 012F23            BSET    #3,X:PCRD                         ; Turn on the serial clock
1246   
1247      P:000317 P:000317 56DB00            MOVE              X:(R3)+,A               ; Get the command which should just be a wor
d
1248      P:000318 P:000318 0D020C            JSR     <XMIT_A_WORD                      ; Transmit A to TIM-A-STD
1249      P:000319 P:000319 0D0467            JSR     <PAL_DLY                          ; Wait for the number to be sent
1250      P:00031A P:00031A 012F03            BCLR    #3,X:PCRD                         ; Turn off the serial clock
1251      P:00031B P:00031B 0C008F            JMP     <FINISH
1252   
1253                                ; Set all the DC bias voltages and video processor offset values, reading
1254                                ;   them from the 'DACS' table
1255                                SET_BIASES
1256      P:00031C P:00031C 012F23            BSET    #3,X:PCRD                         ; Turn on the serial clock
1257      P:00031D P:00031D 0A0F01            BCLR    #1,X:<LATCH                       ; Separate updates of clock driver
1258      P:00031E P:00031E 0A0F20            BSET    #CDAC,X:<LATCH                    ; Disable clearing of DACs
1259      P:00031F P:00031F 0A0F22            BSET    #ENCK,X:<LATCH                    ; Enable clock and DAC output switches
1260      P:000320 P:000320 09F0B5            MOVEP             X:LATCH,Y:WRLATCH       ; Write it to the hardware
                            00000F
1261      P:000322 P:000322 0D0467            JSR     <PAL_DLY                          ; Delay for all this to happen
1262   
1263                                ; Read DAC values from a table, and write them to the DACs
1264      P:000323 P:000323 60F400            MOVE              #DACS,R0                ; Get starting address of DAC values
                            00023C
1265      P:000325 P:000325 000000            NOP
1266      P:000326 P:000326 000000            NOP
1267      P:000327 P:000327 065840            DO      Y:(R0)+,L_DAC                     ; Repeat Y:(R0)+ times
                            00032B
1268      P:000329 P:000329 5ED800            MOVE                          Y:(R0)+,A   ; Read the table entry
1269      P:00032A P:00032A 0D020C            JSR     <XMIT_A_WORD                      ; Transmit it to TIM-A-STD
1270      P:00032B P:00032B 000000            NOP
1271                                L_DAC
1272   
1273                                ; Let the DAC voltages all ramp up before exiting
1274      P:00032C P:00032C 44F400            MOVE              #400000,X0
                            061A80
1275      P:00032E P:00032E 06C400            DO      X0,*+3                            ; 4 millisec delay
                            000330
1276      P:000330 P:000330 000000            NOP
1277      P:000331 P:000331 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
1278      P:000332 P:000332 00000C            RTS
1279   
1280                                SET_BIAS_VOLTAGES
1281      P:000333 P:000333 0D031C            JSR     <SET_BIASES
1282      P:000334 P:000334 0C008F            JMP     <FINISH
1283   
1284      P:000335 P:000335 0D0337  CLR_SWS   JSR     <CLEAR_SWITCHES
1285      P:000336 P:000336 0C008F            JMP     <FINISH
1286   
1287                                ; Clear all video processor analog switches to lower their power dissipation
1288                                CLEAR_SWITCHES
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 24



1289      P:000337 P:000337 012F23            BSET    #3,X:PCRD                         ; Turn the serial clock on
1290      P:000338 P:000338 56F400            MOVE              #$0C3000,A              ; Value of integrate speed and gain switches
                            0C3000
1291      P:00033A P:00033A 20001B            CLR     B
1292      P:00033B P:00033B 241000            MOVE              #$100000,X0             ; Increment over board numbers for DAC write
s
1293      P:00033C P:00033C 45F400            MOVE              #$001000,X1             ; Increment over board numbers for WRSS writ
es
                            001000
1294      P:00033E P:00033E 060F80            DO      #15,L_VIDEO                       ; Fifteen video processor boards maximum
                            000345
1295      P:000340 P:000340 0D020C            JSR     <XMIT_A_WORD                      ; Transmit A to TIM-A-STD
1296      P:000341 P:000341 200040            ADD     X0,A
1297      P:000342 P:000342 5F7000            MOVE                          B,Y:WRSS
                            FFFFF3
1298      P:000344 P:000344 0D0467            JSR     <PAL_DLY                          ; Delay for the serial data transmission
1299      P:000345 P:000345 200068            ADD     X1,B
1300                                L_VIDEO
1301      P:000346 P:000346 0A0F00            BCLR    #CDAC,X:<LATCH                    ; Enable clearing of DACs
1302      P:000347 P:000347 0A0F02            BCLR    #ENCK,X:<LATCH                    ; Disable clock and DAC output switches
1303      P:000348 P:000348 09F0B5            MOVEP             X:LATCH,Y:WRLATCH       ; Execute these two operations
                            00000F
1304      P:00034A P:00034A 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
1305      P:00034B P:00034B 00000C            RTS
1306   
1307                                SET_SHUTTER_STATE
1308      P:00034C P:00034C 568F00            MOVE              X:LATCH,A
1309      P:00034D P:00034D 0140C6            AND     #$FFEF,A
                            00FFEF
1310      P:00034F P:00034F 200042            OR      X0,A
1311      P:000350 P:000350 000000            NOP
1312      P:000351 P:000351 540F00            MOVE              A1,X:LATCH
1313      P:000352 P:000352 09CC35            MOVEP             A1,Y:WRLATCH
1314      P:000353 P:000353 00000C            RTS
1315   
1316                                ; Open the shutter from the timing board, executed as a command
1317                                OPEN_SHUTTER
1318      P:000354 P:000354 0D0358            JSR     <OSHUT
1319      P:000355 P:000355 0C008F            JMP     <FINISH
1320   
1321                                ; Close the shutter from the timing board, executed as a command
1322                                CLOSE_SHUTTER
1323      P:000356 P:000356 0D035D            JSR     <CSHUT
1324      P:000357 P:000357 0C008F            JMP     <FINISH
1325   
1326                                ; Shutter subroutines
1327      P:000358 P:000358 0A0023  OSHUT     BSET    #ST_SHUT,X:<STATUS                ; Set status bit to mean shutter open
1328      P:000359 P:000359 44F400            MOVE              #>$10,X0
                            000010
1329      P:00035B P:00035B 0D034C            JSR     <SET_SHUTTER_STATE
1330      P:00035C P:00035C 00000C            RTS
1331   
1332      P:00035D P:00035D 0A0003  CSHUT     BCLR    #ST_SHUT,X:<STATUS                ; Clear status to mean shutter closed
1333      P:00035E P:00035E 240000            MOVE              #0,X0
1334      P:00035F P:00035F 0D034C            JSR     <SET_SHUTTER_STATE
1335      P:000360 P:000360 00000C            RTS
1336   
1337                                ; Clear the CCD, executed as a command
1338      P:000361 P:000361 0D0363  CLEAR     JSR     <CLR_CCD
1339      P:000362 P:000362 0C008F            JMP     <FINISH
1340   
1341                                ; Default clearing routine with serial clocks inactive
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 25



1342                                ; Fast clear image before each exposure, executed as a subroutine
1343      P:000363 P:000363 060440  CLR_CCD   DO      Y:<NPCLR,LPCLR2                   ; Loop over number of lines in image
                            000373
1344      P:000365 P:000365 689100            MOVE                          Y:<PARALLEL_CLEAR,R0 ; Address of parallel transfer wave
form
1345                                          CLOCK
1349      P:00036A P:00036A 0A8989            JCLR    #EF,X:HDR,LPCLR1                  ; Simple test for fast execution
                            000373
1350      P:00036C P:00036C 330700            MOVE              #COM_BUF,R3
1351      P:00036D P:00036D 0D00A5            JSR     <GET_RCV                          ; Check for FO command
1352      P:00036E P:00036E 0E0373            JCC     <LPCLR1                           ; Continue no commands received
1353   
1354      P:00036F P:00036F 60F400            MOVE              #LPCLR1,R0
                            000373
1355      P:000371 P:000371 601F00            MOVE              R0,X:<IDL_ADR
1356      P:000372 P:000372 0C005D            JMP     <PRC_RCV
1357      P:000373 P:000373 000000  LPCLR1    NOP
1358                                LPCLR2
1359      P:000374 P:000374 330700            MOVE              #COM_BUF,R3
1360      P:000375 P:000375 0D00A5            JSR     <GET_RCV                          ; Check for FO command
1361      P:000376 P:000376 00000C            RTS
1362   
1363                                ; Clear the frame store regions into the serial register, and then clear that.
1364                                ; Note that this does not use the inherent Frame Store feature of this device.
1365                                ;
1366                                CLR_FS                                              ; this version for calling by external 3-let
ter command
1367      P:000377 P:000377 0D0379            JSR     <DO_CLR_FS
1368      P:000378 P:000378 0C008F            JMP     <FINISH
1369                                DO_CLR_FS                                           ; this version for calling internally
1370      P:000379 P:000379 060740            DO      Y:<NPFS,FSCLR2                    ; loop over Number of Parallel rows in Frame
 Store area
                            000391
1371      P:00037B P:00037B 60F400            MOVE              #FS_CLEAR,R0
                            000012
1372                                          CLOCK
1376      P:000381 P:000381 060340            DO      Y:<NSCLR,L_CLRSR2                 ; Loop over number of pixels to skip
                            000387
1377      P:000383 P:000383 688F00            MOVE                          Y:<SERIAL_SKIP,R0
1378                                          CLOCK                                     ; Go clock out the CCD charge
1382                                L_CLRSR2                                            ; Do loop restriction
1383      P:000388 P:000388 0A8989            JCLR    #EF,X:HDR,FSCLR1
                            000391
1384      P:00038A P:00038A 330700            MOVE              #COM_BUF,R3
1385      P:00038B P:00038B 0D00A5            JSR     <GET_RCV
1386      P:00038C P:00038C 0E0391            JCC     <FSCLR1
1387   
1388      P:00038D P:00038D 60F400            MOVE              #FSCLR1,R0
                            000391
1389      P:00038F P:00038F 601F00            MOVE              R0,X:<IDL_ADR
1390      P:000390 P:000390 0C005D            JMP     <PRC_RCV
1391      P:000391 P:000391 000000  FSCLR1    NOP
1392                                FSCLR2
1393      P:000392 P:000392 330700            MOVE              #COM_BUF,R3
1394      P:000393 P:000393 0D00A5            JSR     <GET_RCV
1395      P:000394 P:000394 00000C            RTS
1396   
1397                                ; Perform the Frame Transfer.
1398                                ; Note that this does not use the inherent Frame Store feature of this device,
1399                                ; but instead treats one half or the other as frame store.  A Y:MEM flag is used
1400                                ; to indicate its running. This happens so fast it's probably not necessary.
1401                                ;
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 26



1402                                FRAME_TRANSFER
1403      P:000395 P:000395 56F400            MOVE              #>1,A
                            000001
1404      P:000397 P:000397 000000            NOP
1405      P:000398 P:000398 5C2500            MOVE                          A1,Y:<IN_FT ; set Y:IN_FT=1 to mean FRAME_TRANSFER runni
ng
1406                                ;       JSR     <DO_CLR_FS              ; clear FS area into serial and clear serial
1407      P:000399 P:000399 0D0464            JSR     <WAIT_TO_FINISH_CLOCKING
1408      P:00039A P:00039A 060740            DO      Y:<NPFS,END_FT                    ; transfer all rows of image area to frame s
tore area
                            0003A1
1409      P:00039C P:00039C 68F000            MOVE                          Y:PARALLEL_FT,R0 ; parallel waveforms for frame transfer
                            000026
1410                                          CLOCK
1414      P:0003A2 P:0003A2 000000  END_FT    NOP
1415      P:0003A3 P:0003A3 200013            CLR     A
1416      P:0003A4 P:0003A4 000000            NOP
1417      P:0003A5 P:0003A5 5C2500            MOVE                          A1,Y:<IN_FT ; clear Y:IN_FT=0 to mean FRAME_TRANSFER com
plete
1418      P:0003A6 P:0003A6 0C008F            JMP     <FINISH
1419   
1420                                STOP_PARALLEL_CLOCKING
1421      P:0003A7 P:0003A7 60F400            MOVE              #NO_PAR_IDLE,R0         ; Process commands during the exposure
                            0003AD
1422      P:0003A9 P:0003A9 000000            NOP
1423      P:0003AA P:0003AA 601F00            MOVE              R0,X:<IDL_ADR
1424      P:0003AB P:0003AB 0A0002            BCLR    #IDLMODE,X:<STATUS                ; Idle after readout
1425      P:0003AC P:0003AC 0C008F            JMP     <FINISH
1426   
1427                                ; Keep the CCD idling when not reading out
1428                                NO_PAR_IDLE
1429      P:0003AD P:0003AD 5EA400            MOVE                          Y:<NSRI,A   ; NSERIALS_READ = NSR
1430      P:0003AE P:0003AE 0A0085            JCLR    #SPLIT_S,X:STATUS,*+3
                            0003B1
1431      P:0003B0 P:0003B0 200022            ASR     A                                 ; Split serials requires / 2
1432      P:0003B1 P:0003B1 000000            NOP
1433   
1434      P:0003B2 P:0003B2 06CE00            DO      A,NP_IDL1                         ; Loop over number of pixels per line
                            0003BF
1435      P:0003B4 P:0003B4 68F000            MOVE                          Y:SERIAL_IDLE,R0 ; Serial transfer on pixel
                            000015
1436                                          CLOCK                                     ; Go to it
1440      P:0003BA P:0003BA 330700            MOVE              #COM_BUF,R3
1441      P:0003BB P:0003BB 0D00A5            JSR     <GET_RCV                          ; Check for FO or SSI commands
1442      P:0003BC P:0003BC 0E03BF            JCC     <NP_NO_COM                        ; Continue IDLE if no commands received
1443      P:0003BD P:0003BD 00008C            ENDDO
1444      P:0003BE P:0003BE 0C005D            JMP     <PRC_RCV                          ; Go process header and command
1445      P:0003BF P:0003BF 000000  NP_NO_COM NOP
1446                                NP_IDL1
1447      P:0003C0 P:0003C0 000000            NOP
1448                                NO_PAR
1449      P:0003C1 P:0003C1 0C03AD            JMP     <NO_PAR_IDLE
1450   
1451                                ; Start the exposure timer and monitor its progress
1452      P:0003C2 P:0003C2 0B00AD  EXPOSE    JSSET   #ST_SYNC,X:STATUS,SYNCH_CONTROLLER ; Sync up two controllers
                            000807
1453      P:0003C4 P:0003C4 07F40E            MOVEP             #0,X:TLR0               ; Load 0 into counter timer
                            000000
1454      P:0003C6 P:0003C6 240000            MOVE              #0,X0
1455      P:0003C7 P:0003C7 441100            MOVE              X0,X:<ELAPSED_TIME      ; Set elapsed exposure time to zero
1456      P:0003C8 P:0003C8 579000            MOVE              X:<EXPOSURE_TIME,B
1457      P:0003C9 P:0003C9 20000B            TST     B                                 ; Special test for zero exposure time
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 27



1458      P:0003CA P:0003CA 0EA3D6            JEQ     <END_EXP                          ; Don't even start an exposure
1459      P:0003CB P:0003CB 01418C            SUB     #1,B                              ; Timer counts from X:TCPR0+1 to zero
1460      P:0003CC P:0003CC 010F20            BSET    #TIM_BIT,X:TCSR0                  ; Enable the timer #0
1461      P:0003CD P:0003CD 577000            MOVE              B,X:TCPR0
                            FFFF8D
1462      P:0003CF P:0003CF 0A8989  CHK_RCV   JCLR    #EF,X:HDR,CHK_TIM                 ; Simple test for fast execution
                            0003D4
1463      P:0003D1 P:0003D1 330700            MOVE              #COM_BUF,R3             ; The beginning of the command buffer
1464      P:0003D2 P:0003D2 0D00A5            JSR     <GET_RCV                          ; Check for an incoming command
1465      P:0003D3 P:0003D3 0E805D            JCS     <PRC_RCV                          ; If command is received, go check it
1466      P:0003D4 P:0003D4 018F95  CHK_TIM   JCLR    #TCF,X:TCSR0,CHK_RCV              ; Wait for timer to equal compare value
                            0003CF
1467      P:0003D6 P:0003D6 010F00  END_EXP   BCLR    #TIM_BIT,X:TCSR0                  ; Disable the timer
1468      P:0003D7 P:0003D7 0AE780            JMP     (R7)                              ; This contains the return address
1469   
1470                                ; Start the exposure, operate the shutter, and initiate the CCD readout
1471                                START_READOUT                                       ; was START_EXPOSURE
1472      P:0003D8 P:0003D8 57F400            MOVE              #$020102,B
                            020102
1473      P:0003DA P:0003DA 0D00EB            JSR     <XMT_WRD
1474      P:0003DB P:0003DB 57F400            MOVE              #'IIA',B                ; Initialize the PCI image address
                            494941
1475      P:0003DD P:0003DD 0D00EB            JSR     <XMT_WRD
1476                                ;       JSCLR   #NOT_CLR,X:STATUS,CLR_CCD ; Jump to clear out routine if bit set
1477      P:0003DE P:0003DE 330700            MOVE              #COM_BUF,R3             ; The beginning of the command buffer
1478      P:0003DF P:0003DF 0D00A5            JSR     <GET_RCV                          ; Check for FO command
1479      P:0003E0 P:0003E0 0E805D            JCS     <PRC_RCV                          ; Process the command
1480      P:0003E1 P:0003E1 305A00            MOVE              #TST_RCV,R0             ; Process commands during the exposure
1481      P:0003E2 P:0003E2 601F00            MOVE              R0,X:<IDL_ADR
1482      P:0003E3 P:0003E3 0D0464            JSR     <WAIT_TO_FINISH_CLOCKING
1483   
1484                                ; remove shutter operation
1485                                ; Operate the shutter if needed and begin exposure
1486                                ;       JCLR    #SHUT,X:STATUS,L_SEX0
1487                                ;       JSR     <OSHUT                  ; Open the shutter if needed
1488                                ;L_SEX0 MOVE    #L_SEX1,R7              ; Return address at end of exposure
1489                                ;
1490                                ; delay to ensure controllers are synced
1491                                ;       MOVE    #100000,X0
1492                                ;       DO      #100,SHUTTER_SYNC0              ; Delay by Y:SHDEL milliseconds
1493                                ;       DO      X0,SHUTTER_SYNC1
1494                                ;       NOP
1495                                ;SHUTTER_SYNC1 NOP
1496                                ;SHUTTER_SYNC0  NOP
1497                                ;
1498                                ;       JMP     <EXPOSE                 ; Delay for specified exposure time
1499                                ;L_SEX1
1500   
1501                                ; Now we really start the CCD readout, alerting the PCI board, closing the
1502                                ;  shutter, waiting for it to close and then reading out
1503      P:0003E4 P:0003E4 0D0457  STR_RDC   JSR     <PCI_READ_IMAGE                   ; Get the PCI board reading the image
1504      P:0003E5 P:0003E5 0A0024            BSET    #ST_RDC,X:<STATUS                 ; Set status to reading out
1505      P:0003E6 P:0003E6 0A008B            JCLR    #SHUT,X:STATUS,TST_SYN
                            0003E9
1506      P:0003E8 P:0003E8 0D035D            JSR     <CSHUT                            ; Close the shutter if necessary
1507      P:0003E9 P:0003E9 0A00AA  TST_SYN   JSET    #TST_IMG,X:STATUS,SYNTHETIC_IMAGE
                            000422
1508   
1509                                ; Delay readout until the shutter has fully closed
1510                                ;       MOVE    Y:<SHDEL,A
1511                                ;       TST     A
1512                                ;       JLE     <S_DEL0
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 28



1513                                ;       MOVE    #100000,X0
1514                                ;       DO      A,S_DEL0                ; Delay by Y:SHDEL milliseconds
1515                                ;       DO      X0,S_DEL1
1516                                ;       NOP
1517                                ;S_DEL1 NOP
1518                                ;S_DEL0 NOP
1519   
1520      P:0003EB P:0003EB 0C024E            JMP     <RDCCD                            ; Finally, go read out the CCD
1521   
1522                                ;TEST_READ_AD
1523                                ;       DO Y:NPR,END_PARALLEL
1524                                ;       DO Y:NSR,END_SERIAL
1525                                ;       JSR <READ_AD
1526                                ;       NOP
1527                                ;END_SERIAL
1528                                ;        NOP
1529                                ;END_PARALLEL
1530   
1531                                ;RDC_ENDT       JCLR    #IDLMODE,X:<STATUS,NO_IDLT ; Don't idle after readout
1532                                ;       MOVE    #IDLE,R0
1533                                ;       MOVE    R0,X:<IDL_ADR
1534                                ;       JMP     <RDC_ET
1535                                ;NO_IDLT        MOVE    #TST_RCV,R0
1536                                ;       MOVE    R0,X:<IDL_ADR
1537                                ;RDC_ET JSR     <WAIT_TO_FINISH_CLOCKING
1538                                ;       BCLR    #ST_RDC,X:<STATUS       ; Set status to not reading out
1539                                ;        JMP     <START
1540   
1541                                TEST_AD
1542      P:0003EC P:0003EC 57F000            MOVE              X:(RDAD+1),B
                            010001
1543      P:0003EE P:0003EE 0C1DA1            ASL     #16,B,B
1544      P:0003EF P:0003EF 000000            NOP
1545      P:0003F0 P:0003F0 216500            MOVE              B2,X1
1546      P:0003F1 P:0003F1 0C1D91            ASL     #8,B,B
1547      P:0003F2 P:0003F2 000000            NOP
1548      P:0003F3 P:0003F3 216400            MOVE              B2,X0
1549      P:0003F4 P:0003F4 000000            NOP
1550      P:0003F5 P:0003F5 20A700            MOVE              X1,Y1
1551      P:0003F6 P:0003F6 208600            MOVE              X0,Y0
1552      P:0003F7 P:0003F7 4F1800            MOVE                          Y1,Y:<BYTE_1
1553      P:0003F8 P:0003F8 4E1900            MOVE                          Y0,Y:<BYTE_2
1554   
1555      P:0003F9 P:0003F9 0C008F            JMP     <FINISH
1556   
1557                                ; Set the desired exposure time
1558                                SET_EXPOSURE_TIME
1559      P:0003FA P:0003FA 46DB00            MOVE              X:(R3)+,Y0
1560      P:0003FB P:0003FB 461000            MOVE              Y0,X:EXPOSURE_TIME
1561      P:0003FC P:0003FC 04C68D            MOVEP             Y0,X:TCPR0
1562      P:0003FD P:0003FD 0C008F            JMP     <FINISH
1563   
1564                                ; Read the time remaining until the exposure ends
1565                                READ_EXPOSURE_TIME
1566      P:0003FE P:0003FE 018FA0            JSET    #TIM_BIT,X:TCSR0,RD_TIM           ; Read DSP timer if its running
                            000402
1567      P:000400 P:000400 479100            MOVE              X:<ELAPSED_TIME,Y1
1568      P:000401 P:000401 0C0090            JMP     <FINISH1
1569      P:000402 P:000402 47F000  RD_TIM    MOVE              X:TCR0,Y1               ; Read elapsed exposure time
                            FFFF8C
1570      P:000404 P:000404 0C0090            JMP     <FINISH1
1571   
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 29



1572                                ; Pause the exposure - close the shutter and stop the timer
1573                                PAUSE_EXPOSURE
1574      P:000405 P:000405 07700C            MOVEP             X:TCR0,X:ELAPSED_TIME   ; Save the elapsed exposure time
                            000011
1575      P:000407 P:000407 010F00            BCLR    #TIM_BIT,X:TCSR0                  ; Disable the DSP exposure timer
1576      P:000408 P:000408 0D035D            JSR     <CSHUT                            ; Close the shutter
1577      P:000409 P:000409 0C008F            JMP     <FINISH
1578   
1579                                ; Resume the exposure - open the shutter if needed and restart the timer
1580                                RESUME_EXPOSURE
1581      P:00040A P:00040A 010F29            BSET    #TRM,X:TCSR0                      ; To be sure it will load TLR0
1582      P:00040B P:00040B 07700C            MOVEP             X:TCR0,X:TLR0           ; Restore elapsed exposure time
                            FFFF8E
1583      P:00040D P:00040D 010F20            BSET    #TIM_BIT,X:TCSR0                  ; Re-enable the DSP exposure timer
1584      P:00040E P:00040E 0A008B            JCLR    #SHUT,X:STATUS,L_RES
                            000411
1585      P:000410 P:000410 0D0358            JSR     <OSHUT                            ; Open the shutter if necessary
1586      P:000411 P:000411 0C008F  L_RES     JMP     <FINISH
1587   
1588                                ; Abort exposure - close the shutter, stop the timer and resume idle mode
1589                                ABORT_EXPOSURE
1590      P:000412 P:000412 0D035D            JSR     <CSHUT                            ; Close the shutter
1591      P:000413 P:000413 010F00            BCLR    #TIM_BIT,X:TCSR0                  ; Disable the DSP exposure timer
1592      P:000414 P:000414 0A0082            JCLR    #IDLMODE,X:<STATUS,NO_IDL2        ; Don't idle after readout
                            00041A
1593      P:000416 P:000416 60F400            MOVE              #IDLE,R0
                            000233
1594      P:000418 P:000418 601F00            MOVE              R0,X:<IDL_ADR
1595      P:000419 P:000419 0C041C            JMP     <RDC_E2
1596      P:00041A P:00041A 305A00  NO_IDL2   MOVE              #TST_RCV,R0
1597      P:00041B P:00041B 601F00            MOVE              R0,X:<IDL_ADR
1598      P:00041C P:00041C 0D0464  RDC_E2    JSR     <WAIT_TO_FINISH_CLOCKING
1599      P:00041D P:00041D 0A0004            BCLR    #ST_RDC,X:<STATUS                 ; Set status to not reading out
1600      P:00041E P:00041E 06A08F            DO      #4000,*+3                         ; Wait 40 microsec for the fiber
                            000420
1601      P:000420 P:000420 000000            NOP                                       ;  optic to clear out
1602      P:000421 P:000421 0C008F            JMP     <FINISH
1603                                ; Generate a synthetic image by simply incrementing the pixel counts
1604                                SYNTHETIC_IMAGE
1605                                ;       JSR     <PCI_READ_IMAGE         ; Get the PCI board reading the image
1606      P:000422 P:000422 0A0024            BSET    #ST_RDC,X:<STATUS                 ; Set status to reading out
1607      P:000423 P:000423 0D0467            JSR     <PAL_DLY
1608      P:000424 P:000424 200013            CLR     A
1609      P:000425 P:000425 060240            DO      Y:<NPR,LPR_TST                    ; Loop over each line readout
                            000430
1610      P:000427 P:000427 060140            DO      Y:<NSR,LSR_TST                    ; Loop over number of pixels per line
                            00042F
1611      P:000429 P:000429 0614A0            REP     #20                               ; #20 => 1.0 microsec per pixel
1612      P:00042A P:00042A 000000            NOP
1613      P:00042B P:00042B 014180            ADD     #1,A                              ; Pixel data = Pixel data + 1
1614      P:00042C P:00042C 000000            NOP
1615      P:00042D P:00042D 21CF00            MOVE              A,B
1616      P:00042E P:00042E 0D0432            JSR     <XMT_PIX                          ;  transmit them
1617      P:00042F P:00042F 000000            NOP
1618                                LSR_TST
1619      P:000430 P:000430 000000            NOP
1620                                LPR_TST
1621      P:000431 P:000431 0C02DA            JMP     <RDC_END                          ; Normal exit
1622   
1623                                ; Transmit the 16-bit pixel datum in B1 to the host computer
1624      P:000432 P:000432 0C1DA1  XMT_PIX   ASL     #16,B,B
1625      P:000433 P:000433 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 30



1626      P:000434 P:000434 216500            MOVE              B2,X1
1627      P:000435 P:000435 0C1D91            ASL     #8,B,B
1628      P:000436 P:000436 000000            NOP
1629      P:000437 P:000437 216400            MOVE              B2,X0
1630      P:000438 P:000438 000000            NOP
1631      P:000439 P:000439 09C532            MOVEP             X1,Y:WRFO
1632      P:00043A P:00043A 09C432            MOVEP             X0,Y:WRFO
1633      P:00043B P:00043B 00000C            RTS
1634   
1635                                ; Test the hardware to read A/D values directly into the DSP instead
1636                                ;   of using the SXMIT option, A/Ds #2 and 3.
1637      P:00043C P:00043C 57F000  READ_AD   MOVE              X:(RDAD+2),B
                            010002
1638      P:00043E P:00043E 0C1DA1            ASL     #16,B,B
1639      P:00043F P:00043F 000000            NOP
1640      P:000440 P:000440 216500            MOVE              B2,X1
1641      P:000441 P:000441 0C1D91            ASL     #8,B,B
1642      P:000442 P:000442 000000            NOP
1643      P:000443 P:000443 216400            MOVE              B2,X0
1644      P:000444 P:000444 000000            NOP
1645      P:000445 P:000445 09C532            MOVEP             X1,Y:WRFO
1646      P:000446 P:000446 09C432            MOVEP             X0,Y:WRFO
1647      P:000447 P:000447 060AA0            REP     #10
1648      P:000448 P:000448 000000            NOP
1649      P:000449 P:000449 57F000            MOVE              X:(RDAD+3),B
                            010003
1650      P:00044B P:00044B 0C1DA1            ASL     #16,B,B
1651      P:00044C P:00044C 000000            NOP
1652      P:00044D P:00044D 216500            MOVE              B2,X1
1653      P:00044E P:00044E 0C1D91            ASL     #8,B,B
1654      P:00044F P:00044F 000000            NOP
1655      P:000450 P:000450 216400            MOVE              B2,X0
1656      P:000451 P:000451 000000            NOP
1657      P:000452 P:000452 09C532            MOVEP             X1,Y:WRFO
1658      P:000453 P:000453 09C432            MOVEP             X0,Y:WRFO
1659      P:000454 P:000454 060AA0            REP     #10
1660      P:000455 P:000455 000000            NOP
1661      P:000456 P:000456 00000C            RTS
1662   
1663                                ; Alert the PCI interface board that images are coming soon
1664                                PCI_READ_IMAGE
1665      P:000457 P:000457 57F400            MOVE              #$020104,B              ; Send header word to the FO xmtr
                            020104
1666      P:000459 P:000459 0D00EB            JSR     <XMT_WRD
1667      P:00045A P:00045A 57F400            MOVE              #'RDA',B
                            524441
1668      P:00045C P:00045C 0D00EB            JSR     <XMT_WRD
1669      P:00045D P:00045D 5FF000            MOVE                          Y:NSR,B     ; Number of columns to read
                            000001
1670      P:00045F P:00045F 0D00EB            JSR     <XMT_WRD
1671      P:000460 P:000460 5FF000            MOVE                          Y:NPR,B     ; Number of rows to read
                            000002
1672      P:000462 P:000462 0D00EB            JSR     <XMT_WRD
1673      P:000463 P:000463 00000C            RTS
1674   
1675                                ; Wait for the clocking to be complete before proceeding
1676                                WAIT_TO_FINISH_CLOCKING
1677      P:000464 P:000464 01ADA1            JSET    #SSFEF,X:PDRD,*                   ; Wait for the SS FIFO to be empty
                            000464
1678      P:000466 P:000466 00000C            RTS
1679   
1680                                ; Delay for serial writes to the PALs and DACs by 8 microsec
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 31



1681      P:000467 P:000467 062083  PAL_DLY   DO      #800,*+3                          ; Wait 8 usec for serial data xmit
                            000469
1682      P:000469 P:000469 000000            NOP
1683      P:00046A P:00046A 00000C            RTS
1684   
1685                                ; Let the host computer read the controller configuration
1686                                READ_CONTROLLER_CONFIGURATION
1687      P:00046B P:00046B 4F8900            MOVE                          Y:<CONFIG,Y1 ; Just transmit the configuration
1688      P:00046C P:00046C 0C0090            JMP     <FINISH1
1689   
1690                                ; Set the video processor gain and integrator speed for all video boards
1691                                ;                                         #SPEED = 0 for slow, 1 for fast
1692      P:00046D P:00046D 012F23  ST_GAIN   BSET    #3,X:PCRD                         ; Turn on the serial clock
1693      P:00046E P:00046E 56DB00            MOVE              X:(R3)+,A               ; Gain value (1,2,5 or 10)
1694      P:00046F P:00046F 44F400            MOVE              #>1,X0
                            000001
1695      P:000471 P:000471 20001B            CLR     B
1696   
1697      P:000472 P:000472 060F80            DO      #15,CHK_GAIN
                            000477
1698      P:000474 P:000474 200005            CMP     B,A
1699      P:000475 P:000475 0EA479            JEQ     <STG_A
1700      P:000476 P:000476 200048            ADD     X0,B
1701      P:000477 P:000477 000000            NOP
1702                                CHK_GAIN
1703      P:000478 P:000478 0C0483            JMP     <ERR_SGN
1704   
1705      P:000479 P:000479 5E0000  STG_A     MOVE                          A,Y:<GAIN   ; Store the GAIN value for later use
1706      P:00047A P:00047A 240D00            MOVE              #$0D0000,X0
1707      P:00047B P:00047B 200042            OR      X0,A
1708      P:00047C P:00047C 000000            NOP
1709   
1710                                ; Send this same value to 15 video processor boards whether they exist or not
1711                                ;       MOVE    #$100000,X0     ; Increment value
1712                                ;       DO      #15,STG_LOOP
1713                                ; DO NOT LOOP!  You will send out a word which is 0x2D000x where x is the
1714                                ; gain settings.  This will set one of the reset drain voltages to ~0V and
1715                                ; so mess up the CCD depletion.  You can't set the HV board address to get
1716                                ; around this...  If you end up with more than one video card in a box
1717                                ; then you'll have to manually set this.
1718      P:00047D P:00047D 0D020C            JSR     <XMIT_A_WORD                      ; Transmit A to TIM-A-STD
1719      P:00047E P:00047E 0D0467            JSR     <PAL_DLY                          ; Wait for SSI and PAL to be empty
1720      P:00047F P:00047F 200040            ADD     X0,A                              ; Increment the video processor board number
1721      P:000480 P:000480 000000            NOP
1722                                STG_LOOP
1723      P:000481 P:000481 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
1724      P:000482 P:000482 0C008F            JMP     <FINISH
1725   
1726      P:000483 P:000483 56DB00  ERR_SGN   MOVE              X:(R3)+,A
1727      P:000484 P:000484 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
1728      P:000485 P:000485 0C008D            JMP     <ERROR
1729   
1730                                ; Set binning parameters NPBIN, NP_SKIP, NSBIN, NS_SKIP
1731                                ;
1732                                SET_BIN_PARAMETERS
1733      P:000486 P:000486 56DB00            MOVE              X:(R3)+,A               ; first arg is NPBIN
1734      P:000487 P:000487 000000            NOP
1735      P:000488 P:000488 5E0600            MOVE                          A,Y:<NPBIN  ; parallel binning parameter
1736      P:000489 P:000489 56DB00            MOVE              X:(R3)+,A               ; second arg is NP_SKIP
1737      P:00048A P:00048A 000000            NOP
1738      P:00048B P:00048B 5E2C00            MOVE                          A,Y:<NP_SKIP ; number of rows to skip
1739      P:00048C P:00048C 56DB00            MOVE              X:(R3)+,A               ; third arg is NSBIN
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 32



1740      P:00048D P:00048D 000000            NOP
1741      P:00048E P:00048E 5E0500            MOVE                          A,Y:<NSBIN  ; serial binning parameter
1742      P:00048F P:00048F 56DB00            MOVE              X:(R3)+,A               ; fourth arg is NS_SKIP
1743      P:000490 P:000490 000000            NOP
1744      P:000491 P:000491 5E2D00            MOVE                          A,Y:<NS_SKIP ; number of columns to skip
1745      P:000492 P:000492 0C008F            JMP     <FINISH
1746   
1747                                ; Set geometry parameters: GEO <NPR> <NPBIAS> <NSR> <NSBIAS>
1748                                ;
1749                                GEOMETRY_PARAMETERS
1750      P:000493 P:000493 56DB00            MOVE              X:(R3)+,A               ; first arg is NPR
1751      P:000494 P:000494 000000            NOP
1752      P:000495 P:000495 5E0200            MOVE                          A,Y:<NPR    ; number of parallel reads
1753      P:000496 P:000496 56DB00            MOVE              X:(R3)+,A               ; second arg is NPBIAS
1754      P:000497 P:000497 000000            NOP
1755      P:000498 P:000498 5E3000            MOVE                          A,Y:<NPBIAS ; number of parallel bias (overscans)
1756      P:000499 P:000499 56DB00            MOVE              X:(R3)+,A               ; third arg is NSR
1757      P:00049A P:00049A 000000            NOP
1758      P:00049B P:00049B 5E0100            MOVE                          A,Y:<NSR    ; number of serial reads
1759      P:00049C P:00049C 56DB00            MOVE              X:(R3)+,A               ; fourth arg is NSBIAS
1760      P:00049D P:00049D 000000            NOP
1761      P:00049E P:00049E 5E3100            MOVE                          A,Y:<NSBIAS ; number of serial bias (overscans)
1762      P:00049F P:00049F 0C008F            JMP     <FINISH
1763   
1764                                ; Set a particular DAC numbers, for setting DC bias voltages, clock driver
1765                                ;   voltages and video processor offset
1766                                ; This is code for the ARC32 clock driver and ARC45 CCD video processor
1767                                ;
1768                                ; SBN  #BOARD  #DAC  ['CLK' or 'VID'] voltage
1769                                ;
1770                                ;                               #BOARD is from 0 to 15
1771                                ;                               #DAC number
1772                                ;                               #voltage is from 0 to 4095
1773   
1774                                SET_BIAS_NUMBER                                     ; Set bias number
1775      P:0004A0 P:0004A0 012F23            BSET    #3,X:PCRD                         ; Turn on the serial clock
1776      P:0004A1 P:0004A1 56DB00            MOVE              X:(R3)+,A               ; First argument is board number, 0 to 15
1777      P:0004A2 P:0004A2 0614A0            REP     #20
1778      P:0004A3 P:0004A3 200033            LSL     A
1779      P:0004A4 P:0004A4 000000            NOP
1780      P:0004A5 P:0004A5 21C500            MOVE              A,X1                    ; Save the board number
1781      P:0004A6 P:0004A6 56DB00            MOVE              X:(R3)+,A               ; Second argument is DAC number
1782      P:0004A7 P:0004A7 57E300            MOVE              X:(R3),B                ; Third argument is 'VID' or 'CLK' string
1783      P:0004A8 P:0004A8 0140CD            CMP     #'VID',B
                            564944
1784      P:0004AA P:0004AA 0E24B2            JNE     <CLK_DRV
1785      P:0004AB P:0004AB 060EA0            REP     #14
1786      P:0004AC P:0004AC 200033            LSL     A
1787      P:0004AD P:0004AD 000000            NOP
1788      P:0004AE P:0004AE 0ACC73            BSET    #19,A1                            ; Set bits to mean video processor DAC
1789      P:0004AF P:0004AF 000000            NOP
1790      P:0004B0 P:0004B0 0ACC72            BSET    #18,A1
1791      P:0004B1 P:0004B1 0C04DC            JMP     <BD_SET
1792      P:0004B2 P:0004B2 0140CD  CLK_DRV   CMP     #'CLK',B
                            434C4B
1793      P:0004B4 P:0004B4 0E24F1            JNE     <ERR_SBN
1794   
1795                                ; For ARC32 do some trickiness to set the chip select and address bits
1796      P:0004B5 P:0004B5 218F00            MOVE              A1,B
1797      P:0004B6 P:0004B6 060EA0            REP     #14
1798      P:0004B7 P:0004B7 200033            LSL     A
1799      P:0004B8 P:0004B8 240E00            MOVE              #$0E0000,X0
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 33



1800      P:0004B9 P:0004B9 200046            AND     X0,A
1801      P:0004BA P:0004BA 44F400            MOVE              #>7,X0
                            000007
1802      P:0004BC P:0004BC 20004E            AND     X0,B                              ; Get 3 least significant bits of clock #
1803      P:0004BD P:0004BD 01408D            CMP     #0,B
1804      P:0004BE P:0004BE 0E24C1            JNE     <CLK_1
1805      P:0004BF P:0004BF 0ACE68            BSET    #8,A
1806      P:0004C0 P:0004C0 0C04DC            JMP     <BD_SET
1807      P:0004C1 P:0004C1 01418D  CLK_1     CMP     #1,B
1808      P:0004C2 P:0004C2 0E24C5            JNE     <CLK_2
1809      P:0004C3 P:0004C3 0ACE69            BSET    #9,A
1810      P:0004C4 P:0004C4 0C04DC            JMP     <BD_SET
1811      P:0004C5 P:0004C5 01428D  CLK_2     CMP     #2,B
1812      P:0004C6 P:0004C6 0E24C9            JNE     <CLK_3
1813      P:0004C7 P:0004C7 0ACE6A            BSET    #10,A
1814      P:0004C8 P:0004C8 0C04DC            JMP     <BD_SET
1815      P:0004C9 P:0004C9 01438D  CLK_3     CMP     #3,B
1816      P:0004CA P:0004CA 0E24CD            JNE     <CLK_4
1817      P:0004CB P:0004CB 0ACE6B            BSET    #11,A
1818      P:0004CC P:0004CC 0C04DC            JMP     <BD_SET
1819      P:0004CD P:0004CD 01448D  CLK_4     CMP     #4,B
1820      P:0004CE P:0004CE 0E24D1            JNE     <CLK_5
1821      P:0004CF P:0004CF 0ACE6D            BSET    #13,A
1822      P:0004D0 P:0004D0 0C04DC            JMP     <BD_SET
1823      P:0004D1 P:0004D1 01458D  CLK_5     CMP     #5,B
1824      P:0004D2 P:0004D2 0E24D5            JNE     <CLK_6
1825      P:0004D3 P:0004D3 0ACE6E            BSET    #14,A
1826      P:0004D4 P:0004D4 0C04DC            JMP     <BD_SET
1827      P:0004D5 P:0004D5 01468D  CLK_6     CMP     #6,B
1828      P:0004D6 P:0004D6 0E24D9            JNE     <CLK_7
1829      P:0004D7 P:0004D7 0ACE6F            BSET    #15,A
1830      P:0004D8 P:0004D8 0C04DC            JMP     <BD_SET
1831      P:0004D9 P:0004D9 01478D  CLK_7     CMP     #7,B
1832      P:0004DA P:0004DA 0E24DC            JNE     <BD_SET
1833      P:0004DB P:0004DB 0ACE70            BSET    #16,A
1834   
1835      P:0004DC P:0004DC 200062  BD_SET    OR      X1,A                              ; Add on the board number
1836      P:0004DD P:0004DD 000000            NOP
1837      P:0004DE P:0004DE 21C400            MOVE              A,X0
1838      P:0004DF P:0004DF 57DB00            MOVE              X:(R3)+,B               ; Third argument (again) is 'VID' or 'CLK' s
tring
1839      P:0004E0 P:0004E0 0140CD            CMP     #'VID',B
                            564944
1840      P:0004E2 P:0004E2 0EA4EB            JEQ     <VID
1841      P:0004E3 P:0004E3 56DB00            MOVE              X:(R3)+,A               ; Fourth argument is voltage value, 0 to $ff
f
1842      P:0004E4 P:0004E4 0604A0            REP     #4
1843      P:0004E5 P:0004E5 200023            LSR     A                                 ; Convert 12 bits to 8 bits for ARC32
1844      P:0004E6 P:0004E6 46F400            MOVE              #>$FF,Y0                ; Mask off just 8 bits
                            0000FF
1845      P:0004E8 P:0004E8 200056            AND     Y0,A
1846      P:0004E9 P:0004E9 200042            OR      X0,A
1847      P:0004EA P:0004EA 0C04ED            JMP     <XMT_SBN
1848      P:0004EB P:0004EB 56DB00  VID       MOVE              X:(R3)+,A               ; Fourth argument is voltage value for ARC45
, 12 bits
1849      P:0004EC P:0004EC 200042            OR      X0,A
1850   
1851      P:0004ED P:0004ED 0D020C  XMT_SBN   JSR     <XMIT_A_WORD                      ; Transmit A to TIM-A-STD
1852      P:0004EE P:0004EE 0D0467            JSR     <PAL_DLY                          ; Wait for the number to be sent
1853      P:0004EF P:0004EF 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
1854      P:0004F0 P:0004F0 0C008F            JMP     <FINISH
1855      P:0004F1 P:0004F1 56DB00  ERR_SBN   MOVE              X:(R3)+,A               ; Read and discard the fourth argument
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 34



1856      P:0004F2 P:0004F2 012F03            BCLR    #3,X:PCRD                         ; Turn the serial clock off
1857      P:0004F3 P:0004F3 0C008D            JMP     <ERROR
1858   
1859   
1860                                ; Specify the MUX value to be output on the clock driver board
1861                                ; Command syntax is  SMX  #clock_driver_board #MUX1 #MUX2
1862                                ;                               #clock_driver_board from 0 to 15
1863                                ;                               #MUX1, #MUX2 from 0 to 23
1864   
1865      P:0004F4 P:0004F4 012F23  SET_MUX   BSET    #3,X:PCRD                         ; Turn on the serial clock
1866      P:0004F5 P:0004F5 56DB00            MOVE              X:(R3)+,A               ; Clock driver board number
1867      P:0004F6 P:0004F6 0614A0            REP     #20
1868      P:0004F7 P:0004F7 200033            LSL     A
1869      P:0004F8 P:0004F8 44F400            MOVE              #$003000,X0
                            003000
1870      P:0004FA P:0004FA 200042            OR      X0,A
1871      P:0004FB P:0004FB 000000            NOP
1872      P:0004FC P:0004FC 21C500            MOVE              A,X1                    ; Move here for storage
1873   
1874                                ; Get the first MUX number
1875      P:0004FD P:0004FD 56DB00            MOVE              X:(R3)+,A               ; Get the first MUX number
1876      P:0004FE P:0004FE 0AF0A9            JLT     ERR_SM1
                            000542
1877      P:000500 P:000500 44F400            MOVE              #>24,X0                 ; Check for argument less than 32
                            000018
1878      P:000502 P:000502 200045            CMP     X0,A
1879      P:000503 P:000503 0AF0A1            JGE     ERR_SM1
                            000542
1880      P:000505 P:000505 21CF00            MOVE              A,B
1881      P:000506 P:000506 44F400            MOVE              #>7,X0
                            000007
1882      P:000508 P:000508 20004E            AND     X0,B
1883      P:000509 P:000509 44F400            MOVE              #>$18,X0
                            000018
1884      P:00050B P:00050B 200046            AND     X0,A
1885      P:00050C P:00050C 0E250F            JNE     <SMX_1                            ; Test for 0 <= MUX number <= 7
1886      P:00050D P:00050D 0ACD63            BSET    #3,B1
1887      P:00050E P:00050E 0C051A            JMP     <SMX_A
1888      P:00050F P:00050F 44F400  SMX_1     MOVE              #>$08,X0
                            000008
1889      P:000511 P:000511 200045            CMP     X0,A                              ; Test for 8 <= MUX number <= 15
1890      P:000512 P:000512 0E2515            JNE     <SMX_2
1891      P:000513 P:000513 0ACD64            BSET    #4,B1
1892      P:000514 P:000514 0C051A            JMP     <SMX_A
1893      P:000515 P:000515 44F400  SMX_2     MOVE              #>$10,X0
                            000010
1894      P:000517 P:000517 200045            CMP     X0,A                              ; Test for 16 <= MUX number <= 23
1895      P:000518 P:000518 0E2542            JNE     <ERR_SM1
1896      P:000519 P:000519 0ACD65            BSET    #5,B1
1897      P:00051A P:00051A 20006A  SMX_A     OR      X1,B1                             ; Add prefix to MUX numbers
1898      P:00051B P:00051B 000000            NOP
1899      P:00051C P:00051C 21A700            MOVE              B1,Y1
1900   
1901                                ; Add on the second MUX number
1902      P:00051D P:00051D 56DB00            MOVE              X:(R3)+,A               ; Get the next MUX number
1903      P:00051E P:00051E 0E908D            JLT     <ERROR
1904      P:00051F P:00051F 44F400            MOVE              #>24,X0                 ; Check for argument less than 32
                            000018
1905      P:000521 P:000521 200045            CMP     X0,A
1906      P:000522 P:000522 0E108D            JGE     <ERROR
1907      P:000523 P:000523 0606A0            REP     #6
1908      P:000524 P:000524 200033            LSL     A
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 35



1909      P:000525 P:000525 000000            NOP
1910      P:000526 P:000526 21CF00            MOVE              A,B
1911      P:000527 P:000527 44F400            MOVE              #$1C0,X0
                            0001C0
1912      P:000529 P:000529 20004E            AND     X0,B
1913      P:00052A P:00052A 44F400            MOVE              #>$600,X0
                            000600
1914      P:00052C P:00052C 200046            AND     X0,A
1915      P:00052D P:00052D 0E2530            JNE     <SMX_3                            ; Test for 0 <= MUX number <= 7
1916      P:00052E P:00052E 0ACD69            BSET    #9,B1
1917      P:00052F P:00052F 0C053B            JMP     <SMX_B
1918      P:000530 P:000530 44F400  SMX_3     MOVE              #>$200,X0
                            000200
1919      P:000532 P:000532 200045            CMP     X0,A                              ; Test for 8 <= MUX number <= 15
1920      P:000533 P:000533 0E2536            JNE     <SMX_4
1921      P:000534 P:000534 0ACD6A            BSET    #10,B1
1922      P:000535 P:000535 0C053B            JMP     <SMX_B
1923      P:000536 P:000536 44F400  SMX_4     MOVE              #>$400,X0
                            000400
1924      P:000538 P:000538 200045            CMP     X0,A                              ; Test for 16 <= MUX number <= 23
1925      P:000539 P:000539 0E208D            JNE     <ERROR
1926      P:00053A P:00053A 0ACD6B            BSET    #11,B1
1927      P:00053B P:00053B 200078  SMX_B     ADD     Y1,B                              ; Add prefix to MUX numbers
1928      P:00053C P:00053C 000000            NOP
1929      P:00053D P:00053D 21AE00            MOVE              B1,A
1930      P:00053E P:00053E 0D020C            JSR     <XMIT_A_WORD                      ; Transmit A to TIM-A-STD
1931      P:00053F P:00053F 0D0467            JSR     <PAL_DLY                          ; Delay for all this to happen
1932      P:000540 P:000540 012F03            BCLR    #3,X:PCRD                         ; Turn off the serial clock
1933      P:000541 P:000541 0C008F            JMP     <FINISH
1934      P:000542 P:000542 56DB00  ERR_SM1   MOVE              X:(R3)+,A
1935      P:000543 P:000543 012F03            BCLR    #3,X:PCRD                         ; Turn off the serial clock
1936      P:000544 P:000544 0C008D            JMP     <ERROR
1937   
1938                                ; Specify subarray readout coordinates, one rectangle only
1939                                SET_SUBARRAY_SIZES
1940      P:000545 P:000545 200013            CLR     A
1941      P:000546 P:000546 000000            NOP
1942      P:000547 P:000547 5E3300            MOVE                          A,Y:<NBOXES ; Number of subimage boxes = 0 to start
1943      P:000548 P:000548 44DB00            MOVE              X:(R3)+,X0
1944      P:000549 P:000549 4C3400            MOVE                          X0,Y:<NR_BIAS ; Number of bias pixels to read
1945      P:00054A P:00054A 44DB00            MOVE              X:(R3)+,X0
1946      P:00054B P:00054B 4C3500            MOVE                          X0,Y:<NS_READ ; Number of columns in subimage read
1947      P:00054C P:00054C 44DB00            MOVE              X:(R3)+,X0
1948      P:00054D P:00054D 4C3600            MOVE                          X0,Y:<NP_READ ; Number of rows in subimage read
1949      P:00054E P:00054E 0C008F            JMP     <FINISH
1950   
1951                                ; Call this routine once for every band of interest row to be added to the table
1952                                BAND_OF_INTEREST
1953      P:00054F P:00054F 4CB300            MOVE                          Y:<NBOXES,X0
1954      P:000550 P:000550 459300            MOVE              X:<TWO,X1
1955      P:000551 P:000551 2000A0            MPY     X0,X1,A
1956      P:000552 P:000552 200022            ASR     A
1957      P:000553 P:000553 210C00            MOVE              A0,A1
1958      P:000554 P:000554 44F400            MOVE              #>10,X0
                            00000A
1959      P:000556 P:000556 200045            CMP     X0,A
1960      P:000557 P:000557 0E708D            JGT     <ERROR                            ; Error if number of boxes > 10
1961      P:000558 P:000558 44F400            MOVE              #BOI_TABLE,X0
                            000037
1962      P:00055A P:00055A 200040            ADD     X0,A
1963      P:00055B P:00055B 000000            NOP
1964      P:00055C P:00055C 219700            MOVE              A1,R7
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 36



1965      P:00055D P:00055D 44DB00            MOVE              X:(R3)+,X0
1966      P:00055E P:00055E 000000            NOP
1967      P:00055F P:00055F 000000            NOP
1968      P:000560 P:000560 4C5F00            MOVE                          X0,Y:(R7)+  ; Number of rows to skip
1969      P:000561 P:000561 44DB00            MOVE              X:(R3)+,X0
1970      P:000562 P:000562 4C5F00            MOVE                          X0,Y:(R7)+  ; Number of rows to read
1971      P:000563 P:000563 44DB00            MOVE              X:(R3)+,X0              ; last arg resets NBOXES when 0
1972      P:000564 P:000564 208E00            MOVE              X0,A
1973      P:000565 P:000565 200003            TST     A
1974      P:000566 P:000566 0EA56B            JEQ     <ZERO_NBOXES
1975      P:000567 P:000567 5EB300            MOVE                          Y:<NBOXES,A ;  the box readout
1976      P:000568 P:000568 449200            MOVE              X:<ONE,X0
1977      P:000569 P:000569 200040            ADD     X0,A
1978      P:00056A P:00056A 000000            NOP
1979                                ZERO_NBOXES
1980      P:00056B P:00056B 5E3300            MOVE                          A,Y:<NBOXES
1981      P:00056C P:00056C 0C008F            JMP     <FINISH
1982   
1983                                ; Generate the serial readout waveform table for the chosen
1984                                ;   value of readout and serial binning.
1985   
1986                                GENERATE_SERIAL_WAVEFORM
1987      P:00056D P:00056D 60F400            MOVE              #CCD_RESET,R0
                            00011B
1988      P:00056F P:00056F 61F400            MOVE              #(PXL_TBL+1),R1
                            000286
1989      P:000571 P:000571 5ED800            MOVE                          Y:(R0)+,A
1990      P:000572 P:000572 000000            NOP
1991      P:000573 P:000573 06CC00            DO      A1,L_CCD_RESET
                            000576
1992      P:000575 P:000575 4CD800            MOVE                          Y:(R0)+,X0
1993      P:000576 P:000576 4C5900            MOVE                          X0,Y:(R1)+
1994                                L_CCD_RESET
1995   
1996                                ; Generate the first set of clocks
1997      P:000577 P:000577 68F000            MOVE                          Y:FIRST_CLOCKS,R0
                            00000C
1998      P:000579 P:000579 000000            NOP
1999      P:00057A P:00057A 000000            NOP
2000      P:00057B P:00057B 5ED800            MOVE                          Y:(R0)+,A
2001      P:00057C P:00057C 000000            NOP
2002      P:00057D P:00057D 06CC00            DO      A1,L_FIRST_CLOCKS
                            000580
2003      P:00057F P:00057F 4CD800            MOVE                          Y:(R0)+,X0
2004      P:000580 P:000580 4C5900            MOVE                          X0,Y:(R1)+
2005                                L_FIRST_CLOCKS
2006   
2007   
2008                                ; Generate the binning waveforms if needed
2009      P:000581 P:000581 5E8500            MOVE                          Y:<NSBIN,A
2010      P:000582 P:000582 014184            SUB     #1,A
2011      P:000583 P:000583 0EF591            JLE     <GEN_VID
2012      P:000584 P:000584 06CC00            DO      A1,L_BIN
                            000590
2013      P:000586 P:000586 68F000            MOVE                          Y:CLOCK_LINE,R0
                            00000D
2014      P:000588 P:000588 000000            NOP
2015      P:000589 P:000589 000000            NOP
2016      P:00058A P:00058A 5ED800            MOVE                          Y:(R0)+,A
2017      P:00058B P:00058B 000000            NOP
2018      P:00058C P:00058C 06CC00            DO      A1,L_CLOCK_LINE
                            00058F
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 37



2019      P:00058E P:00058E 4CD800            MOVE                          Y:(R0)+,X0
2020      P:00058F P:00058F 4C5900            MOVE                          X0,Y:(R1)+
2021                                L_CLOCK_LINE
2022      P:000590 P:000590 000000            NOP
2023                                L_BIN
2024   
2025                                ; Generate the video processor waveforms
2026      P:000591 P:000591 60F400  GEN_VID   MOVE              #VIDEO_PROCESS,R0
                            000138
2027      P:000593 P:000593 000000            NOP
2028      P:000594 P:000594 000000            NOP
2029      P:000595 P:000595 5ED800            MOVE                          Y:(R0)+,A
2030      P:000596 P:000596 000000            NOP
2031      P:000597 P:000597 06CC00            DO      A1,L_VID
                            00059A
2032      P:000599 P:000599 4CD800            MOVE                          Y:(R0)+,X0
2033      P:00059A P:00059A 4C5900            MOVE                          X0,Y:(R1)+
2034                                L_VID
2035   
2036                                ; Finally, calculate the number of entries in the waveform table just generated
2037      P:00059B P:00059B 44F400            MOVE              #PXL_TBL,X0
                            000285
2038      P:00059D P:00059D 209000            MOVE              X0,R0
2039      P:00059E P:00059E 222E00            MOVE              R1,A
2040      P:00059F P:00059F 200044            SUB     X0,A
2041      P:0005A0 P:0005A0 014184            SUB     #1,A
2042      P:0005A1 P:0005A1 000000            NOP
2043      P:0005A2 P:0005A2 5C6000            MOVE                          A1,Y:(R0)
2044      P:0005A3 P:0005A3 00000C            RTS
2045   
2046                                ; Select which readouts to process
2047                                ;   'SOS'  Amplifier_name
2048                                ;       Amplifier_name = '__L', '__R', or '_LR'
2049   
2050                                SELECT_OUTPUT_SOURCE
2051      P:0005A4 P:0005A4 46DB00            MOVE              X:(R3)+,Y0
2052      P:0005A5 P:0005A5 4E0B00            MOVE                          Y0,Y:<OS
2053      P:0005A6 P:0005A6 0D05A8            JSR     <SEL_OS
2054      P:0005A7 P:0005A7 0C008F            JMP     <FINISH
2055   
2056      P:0005A8 P:0005A8 4E8B00  SEL_OS    MOVE                          Y:<OS,Y0
2057      P:0005A9 P:0005A9 56F400            MOVE              #'_U1',A
                            5F5531
2058      P:0005AB P:0005AB 200055            CMP     Y0,A
2059      P:0005AC P:0005AC 0E25CC            JNE     <COMP_U2
2060   
2061      P:0005AD P:0005AD 44F400            MOVE              #$F0C3,X0
                            00F0C3
2062      P:0005AF P:0005AF 4C7000            MOVE                          X0,Y:SXL
                            00014A
2063      P:0005B1 P:0005B1 44F400            MOVE              #PARALLEL_1,X0
                            000061
2064      P:0005B3 P:0005B3 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2065      P:0005B5 P:0005B5 44F400            MOVE              #SERIAL_READ_LEFT,X0
                            000141
2066      P:0005B7 P:0005B7 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2067      P:0005B9 P:0005B9 44F400            MOVE              #SERIAL_BIN_LEFT,X0
                            000153
2068      P:0005BB P:0005BB 4C7000            MOVE                          X0,Y:SERIAL_BIN
                            000028
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 38



2069      P:0005BD P:0005BD 44F400            MOVE              #SERIAL_SKIP_LEFT,X0
                            0001C5
2070      P:0005BF P:0005BF 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2071      P:0005C1 P:0005C1 44F400            MOVE              #SERIAL_IDLE_LEFT,X0
                            0000B1
2072      P:0005C3 P:0005C3 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2073      P:0005C5 P:0005C5 44F400            MOVE              #PARALLEL_CLEAR_1,X0
                            00006B
2074      P:0005C7 P:0005C7 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2075      P:0005C9 P:0005C9 0A0005            BCLR    #SPLIT_S,X:STATUS
2076      P:0005CA P:0005CA 0A0006            BCLR    #SPLIT_P,X:STATUS
2077      P:0005CB P:0005CB 00000C            RTS
2078   
2079      P:0005CC P:0005CC 56F400  COMP_U2   MOVE              #'_U2',A
                            5F5532
2080      P:0005CE P:0005CE 200055            CMP     Y0,A
2081      P:0005CF P:0005CF 0E25EF            JNE     <COMP_L1
2082   
2083      P:0005D0 P:0005D0 44F400            MOVE              #$F041,X0
                            00F041
2084      P:0005D2 P:0005D2 4C7000            MOVE                          X0,Y:SXR
                            0001B5
2085      P:0005D4 P:0005D4 44F400            MOVE              #PARALLEL_2,X0
                            000057
2086      P:0005D6 P:0005D6 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2087      P:0005D8 P:0005D8 44F400            MOVE              #SERIAL_READ_RIGHT,X0
                            0001AC
2088      P:0005DA P:0005DA 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2089      P:0005DC P:0005DC 44F400            MOVE              #SERIAL_BIN_RIGHT,X0
                            0001BE
2090      P:0005DE P:0005DE 4C7000            MOVE                          X0,Y:SERIAL_BIN
                            000028
2091      P:0005E0 P:0005E0 44F400            MOVE              #SERIAL_SKIP_RIGHT,X0
                            0001CE
2092      P:0005E2 P:0005E2 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2093      P:0005E4 P:0005E4 44F400            MOVE              #SERIAL_IDLE_RIGHT,X0
                            0000D3
2094      P:0005E6 P:0005E6 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2095      P:0005E8 P:0005E8 44F400            MOVE              #PARALLEL_CLEAR_2,X0
                            000075
2096      P:0005EA P:0005EA 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2097      P:0005EC P:0005EC 0A0005            BCLR    #SPLIT_S,X:STATUS
2098      P:0005ED P:0005ED 0A0006            BCLR    #SPLIT_P,X:STATUS
2099      P:0005EE P:0005EE 00000C            RTS
2100   
2101      P:0005EF P:0005EF 56F400  COMP_L1   MOVE              #'_L1',A
                            5F4C31
2102      P:0005F1 P:0005F1 200055            CMP     Y0,A
2103      P:0005F2 P:0005F2 0E260E            JNE     <COMP_L2
2104   
2105      P:0005F3 P:0005F3 44F400            MOVE              #$F000,X0
                            00F000
2106      P:0005F5 P:0005F5 4C7000            MOVE                          X0,Y:SXR
                            0001B5
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 39



2107      P:0005F7 P:0005F7 44F400            MOVE              #PARALLEL_1,X0
                            000061
2108      P:0005F9 P:0005F9 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2109      P:0005FB P:0005FB 44F400            MOVE              #SERIAL_READ_RIGHT,X0
                            0001AC
2110      P:0005FD P:0005FD 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2111      P:0005FF P:0005FF 44F400            MOVE              #SERIAL_SKIP_RIGHT,X0
                            0001CE
2112      P:000601 P:000601 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2113      P:000603 P:000603 44F400            MOVE              #SERIAL_IDLE_RIGHT,X0
                            0000D3
2114      P:000605 P:000605 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2115      P:000607 P:000607 44F400            MOVE              #PARALLEL_CLEAR_1,X0
                            00006B
2116      P:000609 P:000609 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2117      P:00060B P:00060B 0A0005            BCLR    #SPLIT_S,X:STATUS
2118      P:00060C P:00060C 0A0006            BCLR    #SPLIT_P,X:STATUS
2119      P:00060D P:00060D 00000C            RTS
2120   
2121      P:00060E P:00060E 56F400  COMP_L2   MOVE              #'_L2',A
                            5F4C32
2122      P:000610 P:000610 200055            CMP     Y0,A
2123      P:000611 P:000611 0E262D            JNE     <COMP_2
2124   
2125      P:000612 P:000612 44F400            MOVE              #$F082,X0
                            00F082
2126      P:000614 P:000614 4C7000            MOVE                          X0,Y:SXL
                            00014A
2127      P:000616 P:000616 44F400            MOVE              #PARALLEL_2,X0
                            000057
2128      P:000618 P:000618 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2129      P:00061A P:00061A 44F400            MOVE              #SERIAL_READ_LEFT,X0
                            000141
2130      P:00061C P:00061C 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2131      P:00061E P:00061E 44F400            MOVE              #SERIAL_SKIP_LEFT,X0
                            0001C5
2132      P:000620 P:000620 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2133      P:000622 P:000622 44F400            MOVE              #SERIAL_IDLE_LEFT,X0
                            0000B1
2134      P:000624 P:000624 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2135      P:000626 P:000626 44F400            MOVE              #PARALLEL_CLEAR_2,X0
                            000075
2136      P:000628 P:000628 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2137      P:00062A P:00062A 0A0005            BCLR    #SPLIT_S,X:STATUS
2138      P:00062B P:00062B 0A0006            BCLR    #SPLIT_P,X:STATUS
2139      P:00062C P:00062C 00000C            RTS
2140   
2141      P:00062D P:00062D 56F400  COMP_2    MOVE              #'__2',A
                            5F5F32
2142      P:00062F P:00062F 200055            CMP     Y0,A
2143      P:000630 P:000630 0E2648            JNE     <COMP_1
2144   
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 40



2145      P:000631 P:000631 44F400            MOVE              #PARALLEL_2,X0
                            000057
2146      P:000633 P:000633 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2147      P:000635 P:000635 44F400            MOVE              #SERIAL_READ_SPLIT_SPECIAL__2,X0
                            000184
2148      P:000637 P:000637 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2149      P:000639 P:000639 44F400            MOVE              #SERIAL_SKIP_SPLIT,X0
                            0001D7
2150      P:00063B P:00063B 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2151      P:00063D P:00063D 44F400            MOVE              #SERIAL_IDLE_SPLIT,X0
                            0000F7
2152      P:00063F P:00063F 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2153      P:000641 P:000641 44F400            MOVE              #PARALLEL_CLEAR_2,X0
                            000075
2154      P:000643 P:000643 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2155      P:000645 P:000645 0A0025            BSET    #SPLIT_S,X:STATUS
2156      P:000646 P:000646 0A0006            BCLR    #SPLIT_P,X:STATUS
2157      P:000647 P:000647 00000C            RTS
2158   
2159      P:000648 P:000648 56F400  COMP_1    MOVE              #'__1',A
                            5F5F31
2160      P:00064A P:00064A 200055            CMP     Y0,A
2161      P:00064B P:00064B 0E2663            JNE     <COMP_ALL
2162   
2163      P:00064C P:00064C 44F400            MOVE              #PARALLEL_1,X0
                            000061
2164      P:00064E P:00064E 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2165      P:000650 P:000650 44F400            MOVE              #SERIAL_READ_SPLIT_SPECIAL__1,X0
                            000198
2166      P:000652 P:000652 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2167      P:000654 P:000654 44F400            MOVE              #SERIAL_SKIP_SPLIT,X0
                            0001D7
2168      P:000656 P:000656 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2169      P:000658 P:000658 44F400            MOVE              #SERIAL_IDLE_SPLIT,X0
                            0000F7
2170      P:00065A P:00065A 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2171      P:00065C P:00065C 44F400            MOVE              #PARALLEL_CLEAR_1,X0
                            00006B
2172      P:00065E P:00065E 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2173      P:000660 P:000660 0A0025            BSET    #SPLIT_S,X:STATUS
2174      P:000661 P:000661 0A0006            BCLR    #SPLIT_P,X:STATUS
2175      P:000662 P:000662 00000C            RTS
2176   
2177                                COMP_ALL
2178      P:000663 P:000663 56F400            MOVE              #'ALL',A
                            414C4C
2179      P:000665 P:000665 200055            CMP     Y0,A
2180      P:000666 P:000666 0E267E            JNE     <COMP_FT2
2181   
2182      P:000667 P:000667 44F400            MOVE              #PARALLEL_SPLIT,X0
                            00004D
2183      P:000669 P:000669 4C7000            MOVE                          X0,Y:PARALLEL
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 41



                            000010
2184      P:00066B P:00066B 44F400            MOVE              #SERIAL_READ_SPLIT_SPECIAL_QUAD,X0
                            00016C
2185      P:00066D P:00066D 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2186      P:00066F P:00066F 44F400            MOVE              #SERIAL_SKIP_SPLIT,X0
                            0001D7
2187      P:000671 P:000671 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2188      P:000673 P:000673 44F400            MOVE              #SERIAL_IDLE_SPLIT,X0
                            0000F7
2189      P:000675 P:000675 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2190      P:000677 P:000677 44F400            MOVE              #PARALLEL_CLEAR_SPLIT,X0
                            00007F
2191      P:000679 P:000679 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2192      P:00067B P:00067B 0A0025            BSET    #SPLIT_S,X:STATUS
2193      P:00067C P:00067C 0A0026            BSET    #SPLIT_P,X:STATUS
2194      P:00067D P:00067D 00000C            RTS
2195   
2196                                ; Frame Transfer 1->2 read split2
2197                                COMP_FT2
2198      P:00067E P:00067E 56F400            MOVE              #'FT2',A
                            465432
2199      P:000680 P:000680 200055            CMP     Y0,A
2200      P:000681 P:000681 0E26A5            JNE     <COMP_FT1
2201   
2202      P:000682 P:000682 44F400            MOVE              #PARALLEL_FRAME_2,X0
                            000089
2203      P:000684 P:000684 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2204      P:000686 P:000686 44F400            MOVE              #PARALLEL_2,X0
                            000057
2205      P:000688 P:000688 4C7000            MOVE                          X0,Y:PARALLEL_FT
                            000026
2206      P:00068A P:00068A 44F400            MOVE              #SERIAL_READ_SPLIT_SPECIAL__2,X0
                            000184
2207      P:00068C P:00068C 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2208      P:00068E P:00068E 44F400            MOVE              #SERIAL_BIN_SPLIT,X0
                            0001DF
2209      P:000690 P:000690 4C7000            MOVE                          X0,Y:SERIAL_BIN
                            000028
2210      P:000692 P:000692 44F400            MOVE              #SERIAL_SKIP_SPLIT,X0
                            0001D7
2211      P:000694 P:000694 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2212      P:000696 P:000696 44F400            MOVE              #SERIAL_IDLE_SPLIT,X0
                            0000F7
2213      P:000698 P:000698 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2214      P:00069A P:00069A 44F400            MOVE              #PARALLEL_CLEAR_2,X0
                            000075
2215      P:00069C P:00069C 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2216      P:00069E P:00069E 44F400            MOVE              #FS_CLEAR_2,X0
                            000043
2217      P:0006A0 P:0006A0 4C7000            MOVE                          X0,Y:FS_CLEAR
                            000012
2218      P:0006A2 P:0006A2 0A0025            BSET    #SPLIT_S,X:STATUS
2219      P:0006A3 P:0006A3 0A0006            BCLR    #SPLIT_P,X:STATUS
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 42



2220      P:0006A4 P:0006A4 00000C            RTS
2221   
2222                                ; Frame Transfer 2->1 read split1
2223                                COMP_FT1
2224      P:0006A5 P:0006A5 56F400            MOVE              #'FT1',A
                            465431
2225      P:0006A7 P:0006A7 200055            CMP     Y0,A
2226      P:0006A8 P:0006A8 0E26CC            JNE     <COMP_TEMP
2227   
2228      P:0006A9 P:0006A9 44F400            MOVE              #PARALLEL_FRAME_1,X0
                            000093
2229      P:0006AB P:0006AB 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2230      P:0006AD P:0006AD 44F400            MOVE              #PARALLEL_1,X0
                            000061
2231      P:0006AF P:0006AF 4C7000            MOVE                          X0,Y:PARALLEL_FT
                            000026
2232      P:0006B1 P:0006B1 44F400            MOVE              #SERIAL_READ_SPLIT_SPECIAL__1,X0
                            000198
2233      P:0006B3 P:0006B3 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2234      P:0006B5 P:0006B5 44F400            MOVE              #SERIAL_BIN_SPLIT,X0
                            0001DF
2235      P:0006B7 P:0006B7 4C7000            MOVE                          X0,Y:SERIAL_BIN
                            000028
2236      P:0006B9 P:0006B9 44F400            MOVE              #SERIAL_SKIP_SPLIT,X0
                            0001D7
2237      P:0006BB P:0006BB 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2238      P:0006BD P:0006BD 44F400            MOVE              #SERIAL_IDLE_SPLIT,X0
                            0000F7
2239      P:0006BF P:0006BF 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2240      P:0006C1 P:0006C1 44F400            MOVE              #PARALLEL_CLEAR_1,X0
                            00006B
2241      P:0006C3 P:0006C3 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2242      P:0006C5 P:0006C5 44F400            MOVE              #FS_CLEAR_1,X0
                            000039
2243      P:0006C7 P:0006C7 4C7000            MOVE                          X0,Y:FS_CLEAR
                            000012
2244      P:0006C9 P:0006C9 0A0025            BSET    #SPLIT_S,X:STATUS
2245      P:0006CA P:0006CA 0A0006            BCLR    #SPLIT_P,X:STATUS
2246      P:0006CB P:0006CB 00000C            RTS
2247   
2248                                COMP_TEMP
2249      P:0006CC P:0006CC 56F400            MOVE              #>5,A
                            000005
2250      P:0006CE P:0006CE 200055            CMP     Y0,A
2251      P:0006CF P:0006CF 0E208D            JNE     <ERROR
2252   
2253      P:0006D0 P:0006D0 44F400            MOVE              #$F040,X0
                            00F040
2254      P:0006D2 P:0006D2 4C7000            MOVE                          X0,Y:SXR
                            0001B5
2255      P:0006D4 P:0006D4 44F400            MOVE              #PARALLEL_SPLIT,X0
                            00004D
2256      P:0006D6 P:0006D6 4C7000            MOVE                          X0,Y:PARALLEL
                            000010
2257      P:0006D8 P:0006D8 44F400            MOVE              #SERIAL_READ_RIGHT,X0
                            0001AC
2258      P:0006DA P:0006DA 4C7000            MOVE                          X0,Y:SERIAL_READ
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 43



                            00000E
2259      P:0006DC P:0006DC 44F400            MOVE              #SERIAL_SKIP_RIGHT,X0
                            0001CE
2260      P:0006DE P:0006DE 4C7000            MOVE                          X0,Y:SERIAL_SKIP
                            00000F
2261      P:0006E0 P:0006E0 44F400            MOVE              #SERIAL_IDLE_RIGHT,X0
                            0000D3
2262      P:0006E2 P:0006E2 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2263      P:0006E4 P:0006E4 44F400            MOVE              #PARALLEL_CLEAR_SPLIT,X0
                            00007F
2264      P:0006E6 P:0006E6 4C7000            MOVE                          X0,Y:PARALLEL_CLEAR
                            000011
2265      P:0006E8 P:0006E8 0A0005            BCLR    #SPLIT_S,X:STATUS
2266      P:0006E9 P:0006E9 0A0026            BSET    #SPLIT_P,X:STATUS
2267      P:0006EA P:0006EA 00000C            RTS
2268   
2269   
2270   
2271                                NO_POLARITY_SHIFT
2272      P:0006EB P:0006EB 4E8B00            MOVE                          Y:<OS,Y0
2273      P:0006EC P:0006EC 56F400            MOVE              #'_U1',A
                            5F5531
2274      P:0006EE P:0006EE 200055            CMP     Y0,A
2275      P:0006EF P:0006EF 0E26F5            JNE     <COMP_U2_NO_POL
2276   
2277      P:0006F0 P:0006F0 44F400            MOVE              #SERIAL_IDLE_LEFT_NO_POL,X0
                            0000C2
2278      P:0006F2 P:0006F2 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2279      P:0006F4 P:0006F4 0C008F            JMP     <FINISH
2280   
2281                                COMP_U2_NO_POL
2282      P:0006F5 P:0006F5 56F400            MOVE              #'_U2',A
                            5F5532
2283      P:0006F7 P:0006F7 200055            CMP     Y0,A
2284      P:0006F8 P:0006F8 0E26FE            JNE     <COMP_L1_NO_POL
2285   
2286      P:0006F9 P:0006F9 44F400            MOVE              #SERIAL_IDLE_RIGHT_NO_POL,X0
                            0000E5
2287      P:0006FB P:0006FB 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2288      P:0006FD P:0006FD 0C008F            JMP     <FINISH
2289   
2290                                COMP_L1_NO_POL
2291      P:0006FE P:0006FE 56F400            MOVE              #'_L1',A
                            5F4C31
2292      P:000700 P:000700 200055            CMP     Y0,A
2293      P:000701 P:000701 0E2707            JNE     <COMP_L2_NO_POL
2294   
2295      P:000702 P:000702 44F400            MOVE              #SERIAL_IDLE_RIGHT_NO_POL,X0
                            0000E5
2296      P:000704 P:000704 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2297      P:000706 P:000706 0C008F            JMP     <FINISH
2298   
2299                                COMP_L2_NO_POL
2300      P:000707 P:000707 56F400            MOVE              #'_L2',A
                            5F4C32
2301      P:000709 P:000709 200055            CMP     Y0,A
2302      P:00070A P:00070A 0E2710            JNE     <COMP_2_NO_POL
2303   
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 44



2304      P:00070B P:00070B 44F400            MOVE              #SERIAL_IDLE_LEFT_NO_POL,X0
                            0000C2
2305      P:00070D P:00070D 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2306      P:00070F P:00070F 0C008F            JMP     <FINISH
2307   
2308                                COMP_2_NO_POL
2309      P:000710 P:000710 56F400            MOVE              #'__2',A
                            5F5F32
2310      P:000712 P:000712 200055            CMP     Y0,A
2311      P:000713 P:000713 0E2719            JNE     <COMP_1_NO_POL
2312   
2313      P:000714 P:000714 44F400            MOVE              #SERIAL_IDLE_SPLIT_NO_POL,X0
                            000109
2314      P:000716 P:000716 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2315      P:000718 P:000718 0C008F            JMP     <FINISH
2316   
2317                                COMP_1_NO_POL
2318      P:000719 P:000719 56F400            MOVE              #'__1',A
                            5F5F31
2319      P:00071B P:00071B 200055            CMP     Y0,A
2320      P:00071C P:00071C 0E2722            JNE     <COMP_ALL_NO_POL
2321   
2322      P:00071D P:00071D 44F400            MOVE              #SERIAL_IDLE_SPLIT_NO_POL,X0
                            000109
2323      P:00071F P:00071F 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2324      P:000721 P:000721 0C008F            JMP     <FINISH
2325   
2326                                 COMP_ALL_NO_POL
2327      P:000722 P:000722 56F400            MOVE              #'ALL',A
                            414C4C
2328      P:000724 P:000724 200055            CMP     Y0,A
2329      P:000725 P:000725 0E208D            JNE     <ERROR
2330   
2331      P:000726 P:000726 44F400            MOVE              #SERIAL_IDLE_SPLIT_NO_POL,X0
                            000109
2332      P:000728 P:000728 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2333      P:00072A P:00072A 0C008F            JMP     <FINISH
2334   
2335   
2336   
2337   
2338      P:00072B P:00072B 44DB00  ERASE     MOVE              X:(R3)+,X0              ; Get TIME1 off the command buffer
2339      P:00072C P:00072C 4C2A00            MOVE                          X0,Y:<TIME1 ; Move it to the address in tim.asm
2340      P:00072D P:00072D 44DB00            MOVE              X:(R3)+,X0              ; Ditto for TIME2
2341      P:00072E P:00072E 4C2B00            MOVE                          X0,Y:<TIME2
2342   
2343                                                                                    ;TODO: Get rid of this horrible hack
2344                                                                                    ;Horrible hack to get over time out on comma
nd return
2345      P:00072F P:00072F 0D0766            JSR     <HACK_FINISH
2346      P:000730 P:000730 012F23            BSET    #3,X:PCRD                         ; Turn on the serial clock - this should alr
eady be the case
2347      P:000731 P:000731 0A0F01            BCLR    #1,X:<LATCH                       ; Separate updates of clock driver
2348      P:000732 P:000732 0A0F20            BSET    #CDAC,X:<LATCH                    ; Disable clearing of DACs
2349      P:000733 P:000733 0A0F22            BSET    #ENCK,X:<LATCH                    ; Enable clock and DAC output switches
2350      P:000734 P:000734 09F0B5            MOVEP             X:LATCH,Y:WRLATCH       ; Write it to the hardware
                            00000F
2351      P:000736 P:000736 0D0467            JSR     <PAL_DLY                          ; Delay for all this to happen
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 45



2352   
2353                                ; Read DAC values from a table, and write them to the DACs
2354      P:000737 P:000737 60F400            MOVE              #ERHI,R0                ; Get starting address of erase clock values
                            0001E8
2355      P:000739 P:000739 000000            NOP
2356      P:00073A P:00073A 000000            NOP
2357      P:00073B P:00073B 065840            DO      Y:(R0)+,E_DAC                     ; Repeat Y:(R0)+ times
                            00073F
2358      P:00073D P:00073D 5ED800            MOVE                          Y:(R0)+,A   ; Read the table entry
2359      P:00073E P:00073E 0D020C            JSR     <XMIT_A_WORD                      ; Transmit it to TIM-A-STD
2360      P:00073F P:00073F 000000            NOP
2361                                E_DAC
2362   
2363                                ; Let the DAC voltages all ramp up before exiting
2364      P:000740 P:000740 44F400            MOVE              #400000,X0
                            061A80
2365      P:000742 P:000742 06C400            DO      X0,*+3                            ; 4 millisec delay
                            000744
2366      P:000744 P:000744 000000            NOP
2367   
2368                                ; Start the delay loop
2369   
2370      P:000745 P:000745 062A40            DO      Y:<TIME1,ER_T1                    ; Delay TIME1 msec
                            000748
2371      P:000747 P:000747 0D0760            JSR     <LNG_DLY
2372      P:000748 P:000748 000000            NOP
2373   
2374      P:000749 P:000749 5EF000  ER_T1     MOVE                          Y:VSUBN,A   ; Reset the Vsub value
                            0001E7
2375      P:00074B P:00074B 0D020C            JSR     <XMIT_A_WORD
2376      P:00074C P:00074C 0D0467            JSR     <PAL_DLY                          ; Wait for SSI and PAL to empty
2377   
2378      P:00074D P:00074D 062B40            DO      Y:<TIME2,ER_T2                    ; Delay TIME2 msec
                            000750
2379      P:00074F P:00074F 0D0760            JSR     <LNG_DLY
2380      P:000750 P:000750 000000            NOP
2381   
2382      P:000751 P:000751 60F400  ER_T2     MOVE              #ERHI_END,R0            ; Get the original clock values back.
                            000202
2383                                ;        MOVE    #DACS,R0               ; This line would do the same job -- pointing back to th
e
2384                                                                                    ; original clocking values, but you'd be set
ting everything.
2385   
2386                                ; Read DAC values from a table, and write them to the DACs
2387      P:000753 P:000753 000000            NOP
2388      P:000754 P:000754 000000            NOP
2389      P:000755 P:000755 065840            DO      Y:(R0)+,END_DAC_RESTORE           ; Repeat Y:(R0)+ times
                            000759
2390      P:000757 P:000757 5ED800            MOVE                          Y:(R0)+,A   ; Read the table entry
2391      P:000758 P:000758 0D020C            JSR     <XMIT_A_WORD                      ; Transmit it to TIM-A-STD
2392      P:000759 P:000759 000000            NOP
2393                                END_DAC_RESTORE
2394   
2395                                ; Let the DAC voltages all ramp up before exiting
2396      P:00075A P:00075A 44F400            MOVE              #400000,X0
                            061A80
2397      P:00075C P:00075C 06C400            DO      X0,*+3                            ; 4 millisec delay
                            00075E
2398      P:00075E P:00075E 000000            NOP
2399      P:00075F P:00075F 0C0054            JMP     <START
2400   
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 46



2401                                ; Routine to delay for 1msec
2402                                ; 40ns for DO and NOP line.  1ms/80ns = 125000
2403   
2404      P:000760 P:000760 44F400  LNG_DLY   MOVE              #100000,X0
                            0186A0
2405      P:000762 P:000762 06C400            DO      X0,*+3                            ; Wait 1 millisec for settling
                            000764
2406      P:000764 P:000764 000000            NOP
2407      P:000765 P:000765 00000C            RTS
2408   
2409                                ; Hack finish subroutine to send 'DON' reply back to the PIC
2410                                ; card.  Some of this should be redundant -- such as jumping to the
2411                                ; command processing.
2412                                 HACK_FINISH
2413      P:000766 P:000766 479800            MOVE              X:<DONE,Y1              ; Send 'DON' as the reply
2414                                 HACK_FINISH1
2415      P:000767 P:000767 578500            MOVE              X:<HEADER,B             ; Get header of incoming command
2416      P:000768 P:000768 469C00            MOVE              X:<SMASK,Y0             ; This was the source byte, and is to
2417      P:000769 P:000769 330700            MOVE              #<COM_BUF,R3            ;     become the destination byte
2418      P:00076A P:00076A 46935E            AND     Y0,B      X:<TWO,Y0
2419      P:00076B P:00076B 0C1ED1            LSR     #8,B                              ; Shift right eight bytes, add it to the
2420      P:00076C P:00076C 460600            MOVE              Y0,X:<NWORDS            ;     header, and put 2 as the number
2421      P:00076D P:00076D 469958            ADD     Y0,B      X:<SBRD,Y0              ;     of words in the string
2422      P:00076E P:00076E 200058            ADD     Y0,B                              ; Add source board's header, set Y1 for abov
e
2423      P:00076F P:00076F 000000            NOP
2424      P:000770 P:000770 575B00            MOVE              B,X:(R3)+               ; Put the new header on the transmitter stac
k
2425      P:000771 P:000771 475B00            MOVE              Y1,X:(R3)+              ; Put the argument on the transmitter stack
2426      P:000772 P:000772 570500            MOVE              B,X:<HEADER
2427      P:000773 P:000773 330700            MOVE              #<COM_BUF,R3            ; Restore R3 = beginning of the command
2428   
2429                                ; Is this command for the timing board?
2430      P:000774 P:000774 448500            MOVE              X:<HEADER,X0
2431      P:000775 P:000775 579B00            MOVE              X:<DMASK,B
2432      P:000776 P:000776 459A4E            AND     X0,B      X:<TIM_DRB,X1           ; Extract destination byte
2433      P:000777 P:000777 20006D            CMP     X1,B                              ; Does header = timing board number?
2434      P:000778 P:000778 0EA080            JEQ     <COMMAND                          ; Yes, process it here
2435      P:000779 P:000779 0E977A            JLT     <HACK_FO_XMT                      ; Send it to fiber optic transmitter
2436                                 HACK_FO_XMT
2437      P:00077A P:00077A 330700            MOVE              #COM_BUF,R3
2438      P:00077B P:00077B 060600            DO      X:<NWORDS,HACK_DON_FFO            ; Transmit all the words in the command
                            00077F
2439      P:00077D P:00077D 57DB00            MOVE              X:(R3)+,B
2440      P:00077E P:00077E 0D00EB            JSR     <XMT_WRD
2441      P:00077F P:00077F 000000            NOP
2442                                 HACK_DON_FFO
2443      P:000780 P:000780 00000C            RTS
2444   
2445      P:000781 P:000781 44DB00  EPURGE    MOVE              X:(R3)+,X0              ; Get TIME1 off the command buffer
2446      P:000782 P:000782 4C2A00            MOVE                          X0,Y:<TIME1 ; Move it to the address in tim.asm
2447   
2448                                                                                    ;TODO: Get rid of this horrible hack
2449                                                                                    ;Horrible hack to get over time out on comma
nd return
2450      P:000783 P:000783 0D0766            JSR     <HACK_FINISH
2451      P:000784 P:000784 012F23            BSET    #3,X:PCRD                         ; Turn on the serial clock - this should alr
eady be the case
2452      P:000785 P:000785 0A0F01            BCLR    #1,X:<LATCH                       ; Separate updates of clock driver
2453      P:000786 P:000786 0A0F20            BSET    #CDAC,X:<LATCH                    ; Disable clearing of DACs
2454      P:000787 P:000787 0A0F22            BSET    #ENCK,X:<LATCH                    ; Enable clock and DAC output switches
2455      P:000788 P:000788 09F0B5            MOVEP             X:LATCH,Y:WRLATCH       ; Write it to the hardware
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 47



                            00000F
2456      P:00078A P:00078A 0D0467            JSR     <PAL_DLY                          ; Delay for all this to happen
2457   
2458                                ; Read DAC values from a table, and write them to the DACs
2459      P:00078B P:00078B 60F400            MOVE              #EPUR,R0                ; Get starting address of erase clock values
                            00021F
2460      P:00078D P:00078D 000000            NOP
2461      P:00078E P:00078E 000000            NOP
2462      P:00078F P:00078F 065840            DO      Y:(R0)+,E_DAC2                    ; Repeat Y:(R0)+ times
                            000793
2463      P:000791 P:000791 5ED800            MOVE                          Y:(R0)+,A   ; Read the table entry
2464      P:000792 P:000792 0D020C            JSR     <XMIT_A_WORD                      ; Transmit it to TIM-A-STD
2465      P:000793 P:000793 000000            NOP
2466                                E_DAC2
2467   
2468                                ; Let the DAC voltages all ramp before exiting
2469      P:000794 P:000794 44F400            MOVE              #400000,X0
                            061A80
2470      P:000796 P:000796 06C400            DO      X0,*+3                            ; 4 millisec delay
                            000798
2471      P:000798 P:000798 000000            NOP
2472   
2473                                ; Start the delay loop
2474   
2475      P:000799 P:000799 062A40            DO      Y:<TIME1,EPUR_T1                  ; Delay TIME1 msec
                            00079C
2476      P:00079B P:00079B 0D0760            JSR     <LNG_DLY
2477      P:00079C P:00079C 000000            NOP
2478   
2479      P:00079D P:00079D 60F400  EPUR_T1   MOVE              #ERHI_END,R0            ; Get the original clock values back.
                            000202
2480      P:00079F P:00079F 000000            NOP
2481      P:0007A0 P:0007A0 000000            NOP
2482      P:0007A1 P:0007A1 065840            DO      Y:(R0)+,END_DAC_RESTORE2          ; Repeat Y:(R0)+ times
                            0007A5
2483      P:0007A3 P:0007A3 5ED800            MOVE                          Y:(R0)+,A   ; Read the table entry
2484      P:0007A4 P:0007A4 0D020C            JSR     <XMIT_A_WORD                      ; Transmit it to TIM-A-STD
2485      P:0007A5 P:0007A5 000000            NOP
2486                                END_DAC_RESTORE2
2487   
2488                                ; Let the DAC voltages all ramp up before exiting
2489      P:0007A6 P:0007A6 44F400            MOVE              #400000,X0
                            061A80
2490      P:0007A8 P:0007A8 06C400            DO      X0,*+3                            ; 4 millisec delay
                            0007AA
2491      P:0007AA P:0007AA 000000            NOP
2492      P:0007AB P:0007AB 0C0054            JMP     <START
2493   
2494                                ; change the idling direction.  CID
2495                                CHANGE_IDLE_DIRECTION
2496      P:0007AC P:0007AC 46DB00            MOVE              X:(R3)+,Y0
2497      P:0007AD P:0007AD 0D07AF            JSR     <CHG_IDL
2498      P:0007AE P:0007AE 0C008F            JMP     <FINISH
2499   
2500      P:0007AF P:0007AF 56F400  CHG_IDL   MOVE              #'__L',A                ; Shift right
                            5F5F4C
2501      P:0007B1 P:0007B1 200055            CMP     Y0,A
2502      P:0007B2 P:0007B2 0E27B9            JNE     <COMP_IR
2503   
2504      P:0007B3 P:0007B3 44F400            MOVE              #SERIAL_IDLE_LEFT,X0
                            0000B1
2505      P:0007B5 P:0007B5 4C7000            MOVE                          X0,Y:SERIAL_IDLE
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 48



                            000015
2506      P:0007B7 P:0007B7 0A0005            BCLR    #SPLIT_S,X:STATUS
2507      P:0007B8 P:0007B8 00000C            RTS
2508   
2509      P:0007B9 P:0007B9 56F400  COMP_IR   MOVE              #'__R',A
                            5F5F52
2510      P:0007BB P:0007BB 200055            CMP     Y0,A
2511      P:0007BC P:0007BC 0E27C3            JNE     <COMP_IS
2512   
2513      P:0007BD P:0007BD 44F400            MOVE              #SERIAL_IDLE_RIGHT,X0
                            0000D3
2514      P:0007BF P:0007BF 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2515      P:0007C1 P:0007C1 0A0005            BCLR    #SPLIT_S,X:STATUS
2516      P:0007C2 P:0007C2 00000C            RTS
2517   
2518      P:0007C3 P:0007C3 56F400  COMP_IS   MOVE              #'__S',A
                            5F5F53
2519      P:0007C5 P:0007C5 200055            CMP     Y0,A
2520      P:0007C6 P:0007C6 0E208D            JNE     <ERROR
2521   
2522      P:0007C7 P:0007C7 44F400            MOVE              #SERIAL_IDLE_SPLIT,X0
                            0000F7
2523      P:0007C9 P:0007C9 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2524      P:0007CB P:0007CB 0A0025            BSET    #SPLIT_S,X:STATUS
2525      P:0007CC P:0007CC 00000C            RTS
2526   
2527                                CHANGE_NUMBER_PARALLEL_CLEARS
2528      P:0007CD P:0007CD 46DB00            MOVE              X:(R3)+,Y0
2529      P:0007CE P:0007CE 4E1E00            MOVE                          Y0,Y:<N_PARALLEL_CLEARS
2530      P:0007CF P:0007CF 0C008F            JMP     <FINISH
2531   
2532   
2533                                CHANGE_IDLE_READ_DIRECTION
2534      P:0007D0 P:0007D0 46DB00            MOVE              X:(R3)+,Y0
2535      P:0007D1 P:0007D1 0D07D3            JSR     <CHG_IDL_READ
2536      P:0007D2 P:0007D2 0C008F            JMP     <FINISH
2537   
2538                                CHG_IDL_READ
2539      P:0007D3 P:0007D3 56F400            MOVE              #'__L',A                ; Shift right
                            5F5F4C
2540      P:0007D5 P:0007D5 200055            CMP     Y0,A
2541      P:0007D6 P:0007D6 0E27E1            JNE     <COMP_I_R_R
2542   
2543      P:0007D7 P:0007D7 44F400            MOVE              #SERIAL_IDLE_LEFT,X0
                            0000B1
2544      P:0007D9 P:0007D9 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2545      P:0007DB P:0007DB 44F400            MOVE              #SERIAL_READ_LEFT,X0
                            000141
2546      P:0007DD P:0007DD 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2547      P:0007DF P:0007DF 0A0005            BCLR    #SPLIT_S,X:STATUS
2548      P:0007E0 P:0007E0 00000C            RTS
2549   
2550                                COMP_I_R_R
2551      P:0007E1 P:0007E1 56F400            MOVE              #'__R',A
                            5F5F52
2552      P:0007E3 P:0007E3 200055            CMP     Y0,A
2553      P:0007E4 P:0007E4 0E27C3            JNE     <COMP_IS
2554   
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 49



2555      P:0007E5 P:0007E5 44F400            MOVE              #SERIAL_IDLE_RIGHT,X0
                            0000D3
2556      P:0007E7 P:0007E7 4C7000            MOVE                          X0,Y:SERIAL_IDLE
                            000015
2557      P:0007E9 P:0007E9 44F400            MOVE              #SERIAL_READ_RIGHT,X0
                            0001AC
2558      P:0007EB P:0007EB 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2559      P:0007ED P:0007ED 0A0005            BCLR    #SPLIT_S,X:STATUS
2560      P:0007EE P:0007EE 00000C            RTS
2561   
2562                                COMP_I_R_S
2563      P:0007EF P:0007EF 56F400            MOVE              #'__S',A
                            5F5F53
2564      P:0007F1 P:0007F1 200055            CMP     Y0,A
2565      P:0007F2 P:0007F2 0E208D            JNE     <ERROR
2566   
2567      P:0007F3 P:0007F3 44F400            MOVE              #SERIAL_READ_SPLIT,X0
                            00015A
2568      P:0007F5 P:0007F5 4C7000            MOVE                          X0,Y:SERIAL_READ
                            00000E
2569      P:0007F7 P:0007F7 0A0025            BSET    #SPLIT_S,X:STATUS
2570      P:0007F8 P:0007F8 00000C            RTS
2571   
2572   
2573   
2574                                ; Set synchronization mode of two controllers either ON or OFF
2575                                SET_SYNC_MODE
2576      P:0007F9 P:0007F9 44DB00            MOVE              X:(R3)+,X0              ; =1 for SYNC yes, =0 for not
2577      P:0007FA P:0007FA 0AC400            JCLR    #0,X0,DONT_SYNC
                            0007FE
2578      P:0007FC P:0007FC 0A002D            BSET    #ST_SYNC,X:STATUS
2579      P:0007FD P:0007FD 0C008F            JMP     <FINISH
2580                                DONT_SYNC
2581      P:0007FE P:0007FE 0A000D            BCLR    #ST_SYNC,X:STATUS
2582      P:0007FF P:0007FF 0C008F            JMP     <FINISH
2583   
2584                                ;SYNC_2
2585   
2586                                ;***********************************************************
2587                                ; routine to synchronise the controllers
2588                                ; at least one controller must output the 25 MHz clk
2589                                ; this code sends a high pulse out then goes low
2590                                ; the controllers must determine if they are MASTER or SLAVE
2591                                ; by looking at the JP2 LINK - they then run a synch
2592                                ; routine depending on this input
2593   
2594                                SYNCH_DLY
2595      P:000800 P:000800 44F400            MOVE              #100000,X0              ; 1 ms delay
                            0186A0
2596      P:000802 P:000802 06C400            DO      X0,S_DLY1
                            000804
2597      P:000804 P:000804 000000            NOP
2598                                S_DLY1
2599      P:000805 P:000805 000000            NOP
2600      P:000806 P:000806 00000C            RTS
2601   
2602                                SYNCH_CONTROLLER
2603      P:000807 P:000807 01ADA0            JSET    #SLAVE,X:PDRD,SLAVE_SYSTEM
                            000827
2604   
2605                                MASTER_SYSTEM
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 50



2606      P:000809 P:000809 0D0800            JSR     <SYNCH_DLY                        ; wait so that the slave goes first
2607   
2608      P:00080A P:00080A 0A890C            BCLR    #EXT_OUT0,X:HDR                   ; clear SYNCH bit
2609      P:00080B P:00080B 06D0A7            REP     #2000                             ; wait 20 us
2610      P:00080C P:00080C 000000            NOP
2611                                ;       BSET    #EXT_OUT0,X:HDR         ; set SYNCH bit
2612                                ;       REP     #2000                   ; wait 20 us
2613                                ;       NOP
2614   
2615      P:00080D P:00080D 0A898A  CHK_1     JCLR    #EXT_IN0,X:HDR,CHK_1              ; Wait for the slave signal to be high
                            00080D
2616      P:00080F P:00080F 000000            NOP
2617      P:000810 P:000810 06D0A7            REP     #2000                             ; ensure that it wasn't noise
2618      P:000811 P:000811 000000            NOP
2619      P:000812 P:000812 0A89AA            JSET    #EXT_IN0,X:HDR,GO_LOW_PULSE
                            000815
2620      P:000814 P:000814 0C080D            JMP     <CHK_1                            ; invalid - look again
2621                                GO_LOW_PULSE
2622      P:000815 P:000815 06D0A7            REP     #2000                             ; wait 20 us
2623      P:000816 P:000816 000000            NOP
2624      P:000817 P:000817 0A892C            BSET    #EXT_OUT0,X:HDR                   ; set SYNCH bit
2625      P:000818 P:000818 06D0A7            REP     #2000                             ; wait 20 us
2626      P:000819 P:000819 000000            NOP
2627   
2628      P:00081A P:00081A 0A89AA  CHK_2     JSET    #EXT_IN0,X:HDR,*                  ; Wait for a low signal from the slave
                            00081A
2629      P:00081C P:00081C 000000            NOP
2630      P:00081D P:00081D 06C6A7            REP     #1990                             ; Let it be low for almost as long
2631      P:00081E P:00081E 000000            NOP                                       ;  as the slave keeps it low
2632      P:00081F P:00081F 0A898A            JCLR    #EXT_IN0,X:HDR,MASTER_GOT_IT
                            000822
2633      P:000821 P:000821 0C081A            JMP     <CHK_2                            ; invalid - look again
2634                                MASTER_GOT_IT
2635      P:000822 P:000822 060AA0            REP     #10
2636      P:000823 P:000823 000000            NOP
2637      P:000824 P:000824 0A890C            BCLR    #EXT_OUT0,X:HDR                   ; clear SYNCH bit
2638      P:000825 P:000825 000000            NOP
2639      P:000826 P:000826 0C0846            JMP     <SYNCHED_NOW
2640   
2641                                SLAVE_SYSTEM
2642                                ;       BCLR    #EXT_OUT0,X:HDR         ; clear SYNCH bit
2643                                ;       REP     #2000                   ; wait 20 us
2644                                ;       NOP
2645   
2646      P:000827 P:000827 0A892C            BSET    #EXT_OUT0,X:HDR                   ; set SYNCH bit
2647      P:000828 P:000828 06D0A7            REP     #2000                             ; wait 20 us
2648      P:000829 P:000829 000000            NOP
2649   
2650      P:00082A P:00082A 0A898A  CHK_3     JCLR    #EXT_IN0,X:HDR,CHK_3              ; keep checking
                            00082A
2651      P:00082C P:00082C 000000            NOP
2652      P:00082D P:00082D 06E8A3            REP     #1000                             ; ensure that it wasn't noise
2653      P:00082E P:00082E 000000            NOP
2654      P:00082F P:00082F 0A89AA            JSET    #EXT_IN0,X:HDR,GO_LOW_PULSE2
                            000832
2655      P:000831 P:000831 0C082A            JMP     <CHK_3                            ; invalid - look again
2656                                GO_LOW_PULSE2
2657      P:000832 P:000832 06E8A3            REP     #1000                             ; wait 10 us
2658      P:000833 P:000833 000000            NOP
2659      P:000834 P:000834 0A890C            BCLR    #EXT_OUT0,X:HDR                   ; clear SYNCH bit
2660      P:000835 P:000835 06D0A7            REP     #2000                             ; wait 20 us
2661      P:000836 P:000836 000000            NOP
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  timCCDmisc.asm  Page 51



2662      P:000837 P:000837 0A89AA  CHK_4     JSET    #EXT_IN0,X:HDR,CHK_4              ; keep checking
                            000837
2663      P:000839 P:000839 000000            NOP
2664      P:00083A P:00083A 06E8A3            REP     #1000                             ; ensure that it wasn't noise
2665      P:00083B P:00083B 000000            NOP
2666      P:00083C P:00083C 0A898A            JCLR    #EXT_IN0,X:HDR,SLAVE_GOT_IT
                            00083F
2667      P:00083E P:00083E 0C0837            JMP     <CHK_4                            ; invalid - look again
2668                                SLAVE_GOT_IT
2669      P:00083F P:00083F 44F400            MOVE              #1000,X0
                            0003E8
2670      P:000841 P:000841 06C400            DO      X0,*+3
                            000843
2671      P:000843 P:000843 000000            NOP
2672      P:000844 P:000844 0A890C            BCLR    #EXT_OUT0,X:HDR                   ; clear SYNCH bit
2673      P:000845 P:000845 000000            NOP
2674                                SYNCHED_NOW
2675   
2676                                ; Generate a recognizable pattern
2677   
2678      P:000846 P:000846 0A892C            BSET    #EXT_OUT0,X:HDR
2679      P:000847 P:000847 0664A0            REP     #100
2680      P:000848 P:000848 000000            NOP
2681      P:000849 P:000849 0A890C            BCLR    #EXT_OUT0,X:HDR
2682      P:00084A P:00084A 0664A0            REP     #100
2683      P:00084B P:00084B 000000            NOP
2684      P:00084C P:00084C 0A892C            BSET    #EXT_OUT0,X:HDR
2685      P:00084D P:00084D 0664A0            REP     #100
2686      P:00084E P:00084E 000000            NOP
2687      P:00084F P:00084F 0A890C            BCLR    #EXT_OUT0,X:HDR
2688      P:000850 P:000850 0664A0            REP     #100
2689      P:000851 P:000851 000000            NOP
2690   
2691      P:000852 P:000852 00000C            RTS
2692   
2693                                ;CHANGE_VSUB
2694                                ;  MOVE  X:(R3)+,Y0 ; put the Vsub voltage into Y0
2695                                ;  CLR   A
2696                                ;  MOVE  #$3F0000,X0 ; put base address of Vsub into A
2697                                ;  ADD   X0,A
2698                                ;  OR    Y0,A       ; or it with the voltage
2699                                ;  NOP
2700                                ;  MOVE  #VSUBN,X0  ; Get the address of Vsubn
2701                                ;  MOVE  A,X0       ; Put A into this
2702                                ;  JSR   <XMIT_A_WORD ; Send it out
2703                                ;  JMP   <FINISH     ; Finish
2704   
2705   
2706   
2707   
2708   
2709   
2710                                 TIMBOOT_X_MEMORY
2711      000853                              EQU     @LCV(L)
2712   
2713                                ;  ****************  Setup memory tables in X: space ********************
2714   
2715                                ; Define the address in P: space where the table of constants begins
2716   
2717                                          IF      @SCP("HOST","HOST")
2718      X:000036 X:000036                   ORG     X:END_COMMAND_TABLE,X:END_COMMAND_TABLE
2719                                          ENDIF
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  tim.asm  Page 52



2720   
2721                                          IF      @SCP("HOST","ROM")
2723                                          ENDIF
2724   
2725                                ; Application commands
2726      X:000036 X:000036                   DC      'PON',POWER_ON
2727      X:000038 X:000038                   DC      'POF',POWER_OFF
2728      X:00003A X:00003A                   DC      'SBV',SET_BIAS_VOLTAGES
2729      X:00003C X:00003C                   DC      'IDL',START_IDLE_CLOCKING
2730      X:00003E X:00003E                   DC      'OSH',OPEN_SHUTTER
2731      X:000040 X:000040                   DC      'CSH',CLOSE_SHUTTER
2732      X:000042 X:000042                   DC      'RDC',RDCCD                       ; Begin CCD readout
2733      X:000044 X:000044                   DC      'CLR',CLEAR                       ; Fast clear the CCD
2734   
2735                                ; Exposure and readout control routines
2736      X:000046 X:000046                   DC      'SET',SET_EXPOSURE_TIME
2737      X:000048 X:000048                   DC      'RET',READ_EXPOSURE_TIME
2738                                ;       DC      'SEX',START_EXPOSURE
2739      X:00004A X:00004A                   DC      'PEX',PAUSE_EXPOSURE
2740      X:00004C X:00004C                   DC      'REX',RESUME_EXPOSURE
2741      X:00004E X:00004E                   DC      'AEX',ABORT_EXPOSURE
2742      X:000050 X:000050                   DC      'ABR',ABR_RDC
2743      X:000052 X:000052                   DC      'CRD',CONTINUE_READ
2744      X:000054 X:000054                   DC      'WSI',SYNTHETIC_IMAGE
2745      X:000056 X:000056                   DC      'SSM',SET_SYNC_MODE
2746                                ;       DC      'STR',STR_RDC
2747                                ; New commands for NGPS
2748      X:000058 X:000058                   DC      'SRE',START_READOUT
2749      X:00005A X:00005A                   DC      'FRT',FRAME_TRANSFER
2750      X:00005C X:00005C                   DC      'SPC',STOP_PARALLEL_CLOCKING
2751      X:00005E X:00005E                   DC      'SBP',SET_BIN_PARAMETERS
2752      X:000060 X:000060                   DC      'GEO',GEOMETRY_PARAMETERS
2753      X:000062 X:000062                   DC      'BOI',BAND_OF_INTEREST
2754   
2755                                ; Support routines
2756      X:000064 X:000064                   DC      'SGN',ST_GAIN
2757      X:000066 X:000066                   DC      'SBN',SET_BIAS_NUMBER
2758      X:000068 X:000068                   DC      'SMX',SET_MUX
2759      X:00006A X:00006A                   DC      'CSW',CLR_SWS
2760      X:00006C X:00006C                   DC      'SOS',SELECT_OUTPUT_SOURCE
2761      X:00006E X:00006E                   DC      'SSS',SET_SUBARRAY_SIZES
2762      X:000070 X:000070                   DC      'RCC',READ_CONTROLLER_CONFIGURATION
2763      X:000072 X:000072                   DC      'RAW',RAW_COMMAND                 ; So I can set voltages as I please
2764      X:000074 X:000074                   DC      'ERS',ERASE                       ;Persistent image erase
2765      X:000076 X:000076                   DC      'EPG',EPURGE                      ; E-Purge procedure
2766      X:000078 X:000078                   DC      'TAD',TEST_AD
2767      X:00007A X:00007A                   DC      'CID',CHANGE_IDLE_DIRECTION
2768      X:00007C X:00007C                   DC      'CIR',CHANGE_IDLE_READ_DIRECTION
2769      X:00007E X:00007E                   DC      'CPC',CHANGE_NUMBER_PARALLEL_CLEARS
2770      X:000080 X:000080                   DC      'NPS',NO_POLARITY_SHIFT
2771      X:000082 X:000082                   DC      'CFS',CLR_FS
2772                                ;       DC      'CVS',CHANGE_VSUB
2773                                ;       DC      'FSC',FRAME_STORE_CLEAR
2774      X:000084 X:000084                   DC      'PCK',POWER_CHECK
2775   
2776                                 END_APPLICATION_COMMAND_TABLE
2777      000086                              EQU     @LCV(L)
2778   
2779                                          IF      @SCP("HOST","HOST")
2780      00002F                    NUM_COM   EQU     (@LCV(R)-COM_TBL_R)/2             ; Number of boot +
2781                                                                                    ;  application commands
2782      0003D4                    EXPOSING  EQU     CHK_TIM                           ; Address if exposing
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  tim.asm  Page 53



2783                                 CONTINUE_READING
2784      100000                              EQU     CONT_RD                           ; Address if reading out
2785                                          ENDIF
2786   
2787                                          IF      @SCP("HOST","ROM")
2789                                          ENDIF
2790   
2791                                ; Now let's go for the timing waveform tables
2792                                          IF      @SCP("HOST","HOST")
2793      Y:000000 Y:000000                   ORG     Y:0,Y:0
2794                                          ENDIF
2795   
2796      Y:000000 Y:000000         GAIN      DC      END_APPLICATON_Y_MEMORY-@LCV(L)-1
2797   
2798      Y:000001 Y:000001         NSR       DC      4200                              ; Number Serial Read, prescan + image + bias
2799      Y:000002 Y:000002         NPR       DC      2100                              ; Number Parallel Read
2800      Y:000003 Y:000003         NSCLR     DC      NS_CLR                            ; To clear the serial register
2801      Y:000004 Y:000004         NPCLR     DC      NP_CLR                            ; To clear the parallel register
2802      Y:000005 Y:000005         NSBIN     DC      1                                 ; Serial binning parameter
2803      Y:000006 Y:000006         NPBIN     DC      1                                 ; Parallel binning parameter
2804      Y:000007 Y:000007         NPFS      DC      NP_FS                             ; number of rows in frame store area
2805      Y:000008 Y:000008         SHDEL     DC      SH_DEL                            ; Delay in milliseconds between shutter clos
ing
2806                                                                                    ; and image readout
2807      Y:000009 Y:000009         CONFIG    DC      CC                                ; Controller configuration
2808                                 NSERIALS_READ
2809      Y:00000A Y:00000A                   DC      4200                              ; Number of serials to read
2810   
2811                                ; Waveform parameters
2812      Y:00000B Y:00000B         OS        DC      '__2'                             ; Output source
2813                                 FIRST_CLOCKS
2814      Y:00000C Y:00000C                   DC      0                                 ; Address of first clocks waveforms
2815      Y:00000D Y:00000D         CLOCK_LINE DC     0                                 ; Clock one complete line of charge
2816   
2817                                ; Readout peculiarity parameters. Default values are selected here.
2818                                 SERIAL_READ
2819      Y:00000E Y:00000E                   DC      SERIAL_READ_SPLIT                 ;14  ; Serial readout waveforms
2820                                 SERIAL_SKIP
2821      Y:00000F Y:00000F                   DC      SERIAL_SKIP_SPLIT                 ;15      ; Serial skipping waveforms
2822      Y:000010 Y:000010         PARALLEL  DC      PARALLEL_2                        ;16  ; Parallel shifting waveforms
2823                                 PARALLEL_CLEAR
2824      Y:000011 Y:000011                   DC      PARALLEL_CLEAR_2                  ;17
2825      Y:000012 Y:000012         FS_CLEAR  DC      FS_CLEAR_2                        ;18 Frame Store clearing waveforms
2826      Y:000013 Y:000013         PC_1      DC      PARALLEL_CLEAR_1                  ;19
2827      Y:000014 Y:000014         PC_2      DC      PARALLEL_CLEAR_2                  ;20
2828                                 SERIAL_IDLE
2829      Y:000015 Y:000015                   DC      SERIAL_IDLE_SPLIT                 ;21
2830      Y:000016 Y:000016         SI_L      DC      SERIAL_IDLE_LEFT                  ;22
2831      Y:000017 Y:000017         SI_R      DC      SERIAL_IDLE_RIGHT                 ;23
2832      Y:000018 Y:000018         BYTE_1    DC      0                                 ;24
2833      Y:000019 Y:000019         BYTE_2    DC      0                                 ;25
2834      Y:00001A Y:00001A         SR_L      DC      SERIAL_READ_LEFT                  ;26
2835      Y:00001B Y:00001B         SR_R      DC      SERIAL_READ_RIGHT                 ;27
2836      Y:00001C Y:00001C         P_1       DC      PARALLEL_1                        ;28
2837      Y:00001D Y:00001D         P_2       DC      PARALLEL_2                        ;29
2838                                 N_PARALLEL_CLEARS
2839      Y:00001E Y:00001E                   DC      1                                 ;30
2840      Y:00001F Y:00001F         SR_S      DC      SERIAL_READ_SPLIT                 ;31
2841      Y:000020 Y:000020         TMP_1     DC      TMP_PXL_TBL1                      ;32 stuff where I can load a idle without re
setting.
2842      Y:000021 Y:000021         TMP_2     DC      TMP_PXL_TBL2                      ;33
2843      Y:000022 Y:000022         TMP_3     DC      TMP_PXL_TBL3                      ;34
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  tim.asm  Page 54



2844      Y:000023 Y:000023         PC_S      DC      PARALLEL_CLEAR_SPLIT              ;35
2845      Y:000024 Y:000024         NSRI      DC      4200
2846      Y:000025 Y:000025         IN_FT     DC      0                                 ; 37 (0x25) InFrameTransfer: 0=no, 1=yes, 2=
pending
2847                                 PARALLEL_FT
2848      Y:000026 Y:000026                   DC      PARALLEL_FRAME_1                  ; 38 (0x26) parallel frame transfer waveform
2849   
2850                                 NSERIAL_BIN
2851      Y:000027 Y:000027                   DC      1                                 ; 0x27
2852      Y:000028 Y:000028         SERIAL_BIN DC     SERIAL_BIN_SPLIT
2853   
2854      Y:000029 Y:000029         INT_TIME  DC      0                                 ; 0x29
2855      Y:00002A Y:00002A         TIME1     DC      0
2856      Y:00002B Y:00002B         TIME2     DC      0
2857   
2858                                ; These three parameters are read from the BOI_TABLE when needed by the
2859                                ;   RDCCD routine as it loops through the required number of boxes
2860      Y:00002C Y:00002C         NP_SKIP   DC      0                                 ; user-selected number of rows to skip  0x2c
2861      Y:00002D Y:00002D         NS_SKIP   DC      0                                 ; user-selected number of cols to skip
2862      Y:00002E Y:00002E         NS_SKP1   DC      0                                 ; Number of serials to clear before read
2863      Y:00002F Y:00002F         NS_SKP2   DC      0                                 ; Number of serials to clear after read
2864      Y:000030 Y:000030         NPBIAS    DC      0                                 ; number of parallel bias (overscan) pixels
2865      Y:000031 Y:000031         NSBIAS    DC      0                                 ; number of serial bias (overscan) pixels
2866   
2867      Y:000032 Y:000032         NSEXTENDED DC     7                                 ; built-in number of serial extended pixels
2868   
2869                                ; Subimage readout parameters. Ten subimage boxes maximum.
2870      Y:000033 Y:000033         NBOXES    DC      0                                 ; Number of boxes to read                 30
2871      Y:000034 Y:000034         NR_BIAS   DC      0                                 ; Number of bias pixels to read           31
2872      Y:000035 Y:000035         NS_READ   DC      0                                 ; Number of columns in subimage read      32
2873      Y:000036 Y:000036         NP_READ   DC      0                                 ; Number of rows in subimage read         33
2874      Y:000037 Y:000037         BOI_TABLE DC      0,0                               ; #1=rows_to_skip, #2=rows_to_read  34,35
2875   
2876                                ; Include the waveform table for the designated type of CCD
2877                                          INCLUDE "SWIFT/engg/engg_48khz.waveforms.reverse" ; Readout and clocking waveform file
2878                                ; vim: syntax=asm
2879                                ; @file engg_48khz.waveforms.reverse
2880   
2881                                ; Waveform tables and definitions for the LBNL CCD
2882                                ; This is for a slow, low noise readout
2883                                ; In this version the reset encloses the three serial clocks
2884                                ;**********************************************************************
2885   
2886                                ; U2 and L2 working.
2887                                ; Parallel clocking also needs fixing as shifting to register 1 has
2888                                ; charge under different phases during integration and clocking
2889   
2890                                ; Miscellaneous definitions
2891   
2892      000000                    VIDEO     EQU     $000000                           ; Video processor board select
2893      000000                    VID0      EQU     $000000                           ; Video processor board select
2894      002000                    BIAS      EQU     $002000                           ; Bias Generator board select = 3
2895      003000                    HVBIAS    EQU     $003000                           ; Bias Generator board select = 3
2896      002000                    CLK2      EQU     $002000                           ; Clock driver board select = 2
2897      003000                    CLK3      EQU     $003000                           ; Clock driver board select = 3
2898                                ;CLK4      EQU $004000 ; Clock driver board select = 4
2899                                ;CLK5      EQU $005000 ; Clock driver board select = 5
2900   
2901                                 VIDEO_CONFIG
2902      0C000C                              EQU     $0C000C                           ; WARP = DAC_OUT = ON; H16B, Reset FIFOs
2903      0E0000                    DAC_ADDR  EQU     $0E0000                           ; DAC Channel Address
2904      0F4000                    DAC_RegM  EQU     $0F4000                           ; DAC m Register
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 55



2905      0F8000                    DAC_RegC  EQU     $0F8000                           ; DAC c Register
2906      0FC000                    DAC_RegD  EQU     $0FC000                           ; DAC X1 Register
2907   
2908      -5.000000E+000            VABG      EQU     -5.0                              ; Anti-blooming gate
2909      8.700000E+000             VRSV_MAX  EQU     8.70
2910      000D9A                    DAC_VRSV  EQU     @CVI(((VABG+VRSV_MAX)/VRSV_MAX)*8192-1) ; Bipolar
2911   
2912      000834                    NP_CLR    EQU     2100
2913      0003FC                    NP_FS     EQU     1020
2914      001068                    NS_CLR    EQU     4200
2915   
2916                                ; GenIII: if bit #23=1; 22-16 = # of 320 nanos cycles that bits #15-0 held at
2917                                ;         if bit #23=0; 22-16 = # of  40 nanos cycles that bits #15-0 held at
2918   
2919   
2920                                ;
2921                                ; Pixel time 20.56 us
2922                                ; Full frame readout (4200x2040) = 176.2s
2923                                ; Equiv speed: 48khz
2924                                ;
2925   
2926   
2927   
2928                                ; Delay numbers for clocking
2929                                ; 180000 is 1 mus, 310000 is 2 mus, 4a0000 is 3 mus, 630000 is 4 mus
2930   
2931                                ;I_DELAY       EQU $480000 ; 72 * 40ns + 40ns = 2920ns
2932                                ;I_DELAY       EQU $a40000 ; 36 * 320ns + 40ns = 11560ns
2933      930000                    I_DELAY   EQU     $930000                           ; 18 * 320ns + 40ns = 5800ns
2934   
2935                                ; Delay numbers for clocking
2936                                ; Extra 4*40ns (160ns) for commands in pattern
2937      980000                    P_DELAY   EQU     $980000                           ; 24 * 320ns + 40ns = 7720ns
2938   
2939      070000                    S_DELAY   EQU     $070000                           ; Serial register transfer delay
2940                                                                                    ; 7*40ns + 40ns = 320ns (six in pattern)
2941      180000                    SW_DELAY  EQU     $180000                           ; Sum_well clock delay
2942                                                                                    ; 24*40ns + 40ns = 1000ns
2943                                 PRE_SET_DLY
2944      8B0000                              EQU     $8B0000                           ; 11*320ns + 40ns = 3560ns
2945                                 POST_SET_DLY
2946      2D0000                              EQU     $2D0000                           ; 45*40ns + 40ns = 1840ns
2947                                 DCRST_DELAY
2948      0B0000                              EQU     $0b0000                           ; 11*40ns + 40ns = 480ns
2949   
2950                                ; TODO: Is this a waste of clearing in split readout mode?  Calculate.
2951                                ;NP_CLR  EQU     2048  ; 2040 parallel direction
2952                                ;This doesn't do anything
2953                                ;NP_CLR  EQU     2100  ; 2040 parallel direction
2954                                ;NS_CLR  EQU     4200    ; 4128 in serial direction
2955                                ;NS_CLR  EQU     4128    ; 4128 in serial direction
2956      000064                    SH_DEL    EQU     100
2957   
2958                                ; CHANGE ABOVE TO GENIII TIMING BOARD DELAYS
2959   
2960                                ; Macros to help getting from volts to bits.
2961                                ; The \ in front of NAME substitutes the value of NAME into the variable. (tx)
2962   
2963                                VDEF      MACRO   NAME,BRDTYP,BRDNUM,DAC,ALO,AHI
2964 m 
2965 m                              LO_\NAME  EQU     ALO
2966 m                              HI_\NAME  EQU     AHI
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 56



2967 m                              DAC_\NAME EQU     DAC
2968 m                               BRDNUM_\NAME
2969 m                                        EQU     BRDNUM
2970 m                                        IF      @SCP("BRDTYP",'VID')
2971 m                               BRDTYP_\NAME
2972 m                                        EQU     3
2973 m                                        ELSE
2974 m                               BRDTYP_\NAME
2975 m                                        EQU     0
2976 m                                        ENDIF
2977 m 
2978 m                              ;        MSG     'Defining voltage ',"NAME",' type ',"BRDTYP",' board ',"BRDNUM",' dac ',"DAC",'
 with limits ',"ALO",' ',"AHI"
2979 m                                        ENDM
2980   
2981                                VOLTS     MACRO   NAME,F
2982 m 
2983 m                              DUMMY     SET     @CVI(@MIN(4095,@MAX(0,(F-LO_\NAME)/(HI_\NAME-LO_\NAME)*4096.)))
2984 m                              DUMMY2    SET     @CVI((BRDNUM_\NAME<<20)|(BRDTYP_\NAME<<18)|(DAC_\NAME<<14)|DUMMY)
2985 m                                        DC      DUMMY2
2986 m                                        MSG     'Setting voltage ',"NAME ","F",'V ',DUMMY,' ',DUMMY2
2987 m                                        ENDM
2988   
2989                                ;*********************************************************************
2990                                ;
2991                                ; ; define bias board voltage symbols
2992                                          VDEF    VDDL2,VID,2,0,0.0,-25.0
3005                                          VDEF    VDDU2,VID,2,1,0.0,-25.0
3018                                          VDEF    VDDL1,VID,2,2,0.0,-25.0
3031                                          VDEF    VDDU1,VID,2,3,0.0,-25.0
3044                                          VDEF    VRL2,VID,2,4,0.0,-25.0
3057                                          VDEF    VRU2,VID,2,5,0.0,-25.0
3070                                          VDEF    VRL1,VID,2,6,0.0,-25.0
3083                                          VDEF    VRU1,VID,2,7,0.0,-25.0
3096                                          VDEF    VOGL2,VID,2,8,0.0,5
3109                                          VDEF    VOGU2,VID,2,9,0.0,5
3122                                          VDEF    VOGL1,VID,2,10,0.0,5
3135                                          VDEF    VOGU1,VID,2,11,0.0,5
3148                                          VDEF    VSUB,VID,2,12,0.0,80.0
3161                                          VDEF    RAMP,VID,2,13,0.0,10.0            ;  for ramping p.s.
3174                                ;
3175                                ; ; define clock board symbols bank0
3176                                ;
3177   
3178   
3179                                ; Output video offset parameters
3180                                ;OFFSET0 EQU $2750
3181                                ;OFFSET1 EQU $2500
3182                                ;OFFSET2 EQU $2600
3183                                ;OFFSET3 EQU $2470
3184   
3185                                ;OFFSET0 EQU $26f0
3186                                ;OFFSET1 EQU $2600
3187                                ;OFFSET2 EQU $2370
3188                                ;OFFSET3 EQU $24f0
3189   
3190                                ;OFFSET0 EQU $2000
3191                                ;OFFSET1 EQU $1f00
3192                                ;OFFSET2 EQU $1f00
3193                                ;OFFSET3 EQU $1ece
3194   
3195                                ; changed OFFSET0 on 2011-12-01 Mte
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 57



3196      001F79                    OFFSET0   EQU     $1f79
3197      001F00                    OFFSET1   EQU     $1f00
3198      001F00                    OFFSET2   EQU     $1f00
3199      001ECE                    OFFSET3   EQU     $1ece
3200   
3201                                ; CCD clock voltage
3202      0.000000E+000             ZERO      EQU     0.0                               ; Unused pins
3203      1.300000E+001             Vmax      EQU     +13.0                             ; Clock driver board rails
3204   
3205                                ;LBNL DEFINITIONS
3206      5.000000E+000             V1_HI     EQU     5.0                               ; Vertical High
3207      -3.000000E+000            V1_LO     EQU     -3.0                              ; Vertical Low
3208      5.000000E+000             V2_HI     EQU     5.0                               ; Vertical High
3209      -3.000000E+000            V2_LO     EQU     -3.0                              ; Vertical Low
3210      5.000000E+000             V3_HI     EQU     5.0                               ; Vertical High
3211      -3.000000E+000            V3_LO     EQU     -3.0                              ; Vertical Low
3212   
3213      5.000000E+000             FS1_HI    EQU     5.0                               ; Vertical High
3214      -3.000000E+000            FS1_LO    EQU     -3.0                              ; Vertical Low
3215      5.000000E+000             FS2_HI    EQU     5.0                               ; Vertical High
3216      -3.000000E+000            FS2_LO    EQU     -3.0                              ; Vertical Low
3217      5.000000E+000             FS3_HI    EQU     5.0                               ; Vertical High
3218      -3.000000E+000            FS3_LO    EQU     -3.0                              ; Vertical Low
3219   
3220      5.000000E+000             T2_HI     EQU     5.0                               ; Transfer gate High
3221      -3.000000E+000            T2_LO     EQU     -3.0                              ; Transfer gate Low
3222      5.000000E+000             T1_HI     EQU     5.0                               ; Transfer gate High
3223      -3.000000E+000            T1_LO     EQU     -3.0                              ; Transfer gate Low
3224   
3225      6.000000E+000             H1U1_L2_HI EQU    +6.0                              ; Horizontal High
3226      -3.900000E+000            H1U1_L2_LO EQU    -3.9                              ; Horizontal Low
3227      6.000000E+000             H2U1_L2_HI EQU    +6.0                              ; Horizontal High
3228      -3.900000E+000            H2U1_L2_LO EQU    -3.9                              ; Horizontal Low
3229      6.000000E+000             H3U1_L2_HI EQU    +6.0                              ; Horizontal High
3230      -3.900000E+000            H3U1_L2_LO EQU    -3.9                              ; Horizontal Low
3231      6.000000E+000             H1U2_L1_HI EQU    +6.0                              ; Horizontal High
3232      -3.900000E+000            H1U2_L1_LO EQU    -3.9                              ; Horizontal Low
3233      6.000000E+000             H2U2_L1_HI EQU    +6.0                              ; Horizontal High
3234      -3.900000E+000            H2U2_L1_LO EQU    -3.9                              ; Horizontal Low
3235      6.000000E+000             H3U2_L1_HI EQU    +6.0                              ; Horizontal High
3236      -3.900000E+000            H3U2_L1_LO EQU    -3.9                              ; Horizontal Low
3237   
3238                                ;H1U1_L2_HI  EQU +0.0 ; Horizontal High
3239                                ;H1U1_L2_LO  EQU -0.0 ; Horizontal Low
3240                                ;H2U1_L2_HI  EQU +0.0 ; HoVR2rizontal High
3241                                ;H2U1_L2_LO  EQU -0.0 ; Horizontal Low
3242                                ;H3U1_L2_HI  EQU +0.0 ; Horizontal High
3243                                ;H3U1_L2_LO  EQU -0.0 ; Horizontal Low
3244                                ;H1U2_L1_HI  EQU +0.0 ; Horizontal High
3245                                ;H1U2_L1_LO  EQU -0.0 ; Horizontal Low
3246                                ;H2U2_L1_HI  EQU +0.0 ; Horizontal High
3247                                ;H2U2_L1_LO  EQU -0.0 ; Horizontal Low
3248                                ;H3U2_L1_HI  EQU +0.0 ; Horizontal High
3249                                ;H3U2_L1_LO  EQU -0.0 ; Horizontal Low
3250                                ;
3251                                ;Put summing wells low for conduction channel
3252      5.000000E+000             SWU_HI    EQU     +5.0                              ; Summing Well High
3253      -5.000000E+000            SWU_LO    EQU     -5.0                              ; Summing Well Low
3254      5.000000E+000             SWL_HI    EQU     +5.0                              ; Summing Well High
3255      -5.000000E+000            SWL_LO    EQU     -5.0                              ; Summing Well Low
3256   
3257      -6.000000E+000            RU_HI     EQU     -6.0                              ; Reset ACTIVE wrong polarity....
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 58



3258      -1.000000E-001            RU_LO     EQU     -0.1                              ; Reset INACTIVE
3259      -6.000000E+000            RL_HI     EQU     -6.0                              ; Reset ACTIVE wrong polarity....
3260      -1.000000E-001            RL_LO     EQU     -0.1                              ; Reset INACTIVE
3261   
3262   
3263                                ; Bit defintions for bottom half of clock driver board, CLK2
3264                                ; Clock FS ando vertical regions together
3265                                ; 1,2,4,8,10,20,40,80,100,200,400,800
3266                                ;
3267                                ; Format is Vx_y[H|L] where x=phase and y=upper(2) or lower(1) half
3268                                ; and H|L is level high/low
3269                                ;
3270      000009                    V1_1H     EQU     1|8                               ; V1: Pin 1 -- FS1: pin 4
3271      000000                    V1_1L     EQU     0
3272   
3273      000012                    V2_1H     EQU     2|$10                             ; V2: Pin 2 -- FS2: pin 5
3274      000000                    V2_1L     EQU     0
3275   
3276      000024                    V3_1H     EQU     4|$20                             ; V3: pin 3 -- FS3: pin 6
3277      000000                    V3_1L     EQU     0
3278   
3279      000240                    V1_2H     EQU     $40|$200                          ; V1: pin 7 -- FS1: pin 10
3280      000000                    V1_2L     EQU     0
3281   
3282      000480                    V2_2H     EQU     $80|$400                          ; V2: pin 8 -- FS2: pin 11
3283      000000                    V2_2L     EQU     0
3284   
3285      000900                    V3_2H     EQU     $100|$800                         ; V3: pin 9 -- FS3: pin 12
3286      000000                    V3_2L     EQU     0
3287   
3288   
3289                                ; frame store definitions for clearing out FS region separately.
3290      000008                    FS1_1H    EQU     8
3291      000000                    FS1_1L    EQU     0
3292      000010                    FS2_1H    EQU     $10
3293      000000                    FS2_1L    EQU     0
3294      000020                    FS3_1H    EQU     $20
3295      000000                    FS3_1L    EQU     0
3296      000200                    FS1_2H    EQU     $200
3297      000000                    FS1_2L    EQU     0
3298      000400                    FS2_2H    EQU     $400
3299      000000                    FS2_2L    EQU     0
3300      000800                    FS3_2H    EQU     $800
3301      000000                    FS3_2L    EQU     0
3302   
3303                                ; Top bank
3304   
3305      000001                    H1_1H     EQU     1                                 ; Horizontal 1 Upper Pin 13
3306      000000                    H1_1L     EQU     0
3307   
3308      000002                    H1_2H     EQU     2                                 ; Horizontal 2 Upper, Pin 14
3309      000000                    H1_2L     EQU     0
3310   
3311      000004                    H1_3H     EQU     4                                 ; Horizontal 3 Upper, Pin 15
3312      000000                    H1_3L     EQU     0
3313   
3314      000008                    H2_1H     EQU     8                                 ; Horizontal 1 Lower, Pin 16
3315      000000                    H2_1L     EQU     0
3316   
3317      000010                    H2_2H     EQU     $10                               ; Horizontal 2 Lower, Pin 17
3318      000000                    H2_2L     EQU     0
3319   
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 59



3320      000020                    H2_3H     EQU     $20                               ; Horizontal 3 Lower, Pin 18
3321      000000                    H2_3L     EQU     0
3322   
3323      000040                    SWLH      EQU     $40                               ; Summing Well Upper, Pin 19
3324      000000                    SWLL      EQU     0
3325   
3326      000080                    SWUH      EQU     $80                               ; Summing Well Lower, Pin 33
3327      000000                    SWUL      EQU     0
3328   
3329      000100                    RLH       EQU     $100                              ; Reset Gate Upper, Pin 34
3330      000000                    RLL       EQU     0
3331   
3332      000200                    RUH       EQU     $200                              ; Reset Gate Lower, Pin 35
3333      000000                    RUL       EQU     0
3334   
3335      000400                    T1        EQU     $400                              ; Transfer Gate Upper, Pin 36
3336      000800                    T2        EQU     $800                              ; Transfer Gate Lower, Pin 37
3337   
3338                                ;Both summing wells;
3339                                ;SWUH+SWLH = $80+$100
3340                                ;This was wrong -- should be $40+$80.
3341      000000                    WL        EQU     $00
3342      0000C0                    WH        EQU     $c0
3343   
3344      000000                    RL        EQU     $000
3345      000300                    RH        EQU     $300
3346   
3347                                ;both transfer gates together
3348      000000                    TL        EQU     $000
3349      000C00                    TH        EQU     $c00
3350   
3351                                ; LBNL waveforms
3352   
3353                                ; Frame Store Clear bottom (1) half
3354                                ; Runs the V1 clocks down, holds the V2 clocks fixed
3355                                ;
3356      Y:000039 Y:000039         FS_CLEAR_1 DC     END_FS_CLEAR_1-1
3357      Y:00003A Y:00003A                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3358      Y:00003B Y:00003B                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2L
3359      Y:00003C Y:00003C                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2H|V2_2L|V3_2L
3360      Y:00003D Y:00003D                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2H|V2_2L|V3_2L
3361      Y:00003E Y:00003E                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2H|V2_2L|V3_2L
3362      Y:00003F Y:00003F                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2H|V2_2L|V3_2L
3363      Y:000040 Y:000040                   DC      CLK2|$000000|V1_1H|V2_1L|V3_1L|V1_2H|V2_2L|V3_2L
3364      Y:000041 Y:000041                   DC      CLK3|P_DELAY|RL|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH
3365      Y:000042 Y:000042                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3366                                END_FS_CLEAR_1
3367   
3368                                ; Frame Store Clear top (2) half
3369                                ; Runs the V2 clocks up, holds the V1 clocks fixed  <-- THESE HAVE NOT BEEN TESTED YET!
3370                                ;
3371      Y:000043 Y:000043         FS_CLEAR_2 DC     END_FS_CLEAR_2-1
3372      Y:000044 Y:000044                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3373      Y:000045 Y:000045                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3374      Y:000046 Y:000046                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2H|V2_2L|V3_2L
3375      Y:000047 Y:000047                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2H|V2_2H|V3_2L
3376      Y:000048 Y:000048                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2L|V2_2H|V3_2L
3377      Y:000049 Y:000049                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2L|V2_2H|V3_2H
3378      Y:00004A Y:00004A                   DC      CLK2|$000000|V1_1L|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H
3379      Y:00004B Y:00004B                   DC      CLK3|P_DELAY|RL|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH
3380      Y:00004C Y:00004C                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 60



k add DCRestore,StrtRstInt
3381                                END_FS_CLEAR_2
3382   
3383                                 PARALLEL_SPLIT
3384      Y:00004D Y:00004D                   DC      END_PARALLEL_SPLIT-PARALLEL_SPLIT-1
3385      Y:00004E Y:00004E                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3386      Y:00004F Y:00004F                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3387      Y:000050 Y:000050                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2H|V2_2L|V3_2L
3388      Y:000051 Y:000051                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2H|V2_2H|V3_2L
3389      Y:000052 Y:000052                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3390      Y:000053 Y:000053                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2L|V2_2H|V3_2H
3391      Y:000054 Y:000054                   DC      CLK2|$000000|V1_1H|V2_1L|V3_1L|V1_2L|V2_2L|V3_2H
3392      Y:000055 Y:000055                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3393      Y:000056 Y:000056                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3394   
3395                                END_PARALLEL_SPLIT
3396   
3397   
3398                                ;Shift towards register 2
3399      Y:000057 Y:000057         PARALLEL_2 DC     END_PARALLEL_2-PARALLEL_2-1
3400      Y:000058 Y:000058                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3401      Y:000059 Y:000059                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3402      Y:00005A Y:00005A                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1L|V1_2H|V2_2L|V3_2L
3403      Y:00005B Y:00005B                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2H|V2_2H|V3_2L
3404      Y:00005C Y:00005C                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3405      Y:00005D Y:00005D                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2L|V2_2H|V3_2H
3406      Y:00005E Y:00005E                   DC      CLK2|$000000|V1_1L|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H ; shut TG
3407      Y:00005F Y:00005F                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3408      Y:000060 Y:000060                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3409                                END_PARALLEL_2
3410   
3411                                ;Shift towards register 1
3412                                ;charge stored under 2&3.  Issue with switching between which register to go to.
3413      Y:000061 Y:000061         PARALLEL_1 DC     END_PARALLEL_1-PARALLEL_1-1
3414      Y:000062 Y:000062                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3415      Y:000063 Y:000063                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3416      Y:000064 Y:000064                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H
3417      Y:000065 Y:000065                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2L|V2_2H|V3_2H
3418      Y:000066 Y:000066                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3419      Y:000067 Y:000067                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2H|V2_2H|V3_2L
3420      Y:000068 Y:000068                   DC      CLK2|$000000|V1_1H|V2_1L|V3_1L|V1_2H|V2_2L|V3_2L
3421      Y:000069 Y:000069                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3422      Y:00006A Y:00006A                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3423                                END_PARALLEL_1
3424   
3425   
3426                                PARALLEL_CLEAR_1
3427      Y:00006B Y:00006B                   DC      END_PARALLEL_CLEAR_1-PARALLEL_CLEAR_1-1
3428      Y:00006C Y:00006C                   DC      CLK3|$000000|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3429   
3430      Y:00006D Y:00006D                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3431      Y:00006E Y:00006E                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H
3432      Y:00006F Y:00006F                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2L|V2_2H|V3_2H
3433      Y:000070 Y:000070                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3434      Y:000071 Y:000071                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2H|V2_2H|V3_2L
3435      Y:000072 Y:000072                   DC      CLK2|$000000|V1_1H|V2_1L|V3_1L|V1_2H|V2_2L|V3_2L
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 61



3436   
3437      Y:000073 Y:000073                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3438                                                                                    ; wfk -  Next line: Add DCRestore,StrtRstInt
 to machine state at end of CLEAR to stabilize baseline (04/04/07)
3439      Y:000074 Y:000074                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3440                                END_PARALLEL_CLEAR_1
3441   
3442                                PARALLEL_CLEAR_2
3443      Y:000075 Y:000075                   DC      END_PARALLEL_CLEAR_2-PARALLEL_CLEAR_2-1
3444      Y:000076 Y:000076                   DC      CLK3|$000000|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3445   
3446      Y:000077 Y:000077                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3447      Y:000078 Y:000078                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1L|V1_2H|V2_2L|V3_2L
3448      Y:000079 Y:000079                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2H|V2_2H|V3_2L
3449      Y:00007A Y:00007A                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3450      Y:00007B Y:00007B                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2L|V2_2H|V3_2H
3451      Y:00007C Y:00007C                   DC      CLK2|$000000|V1_1L|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H
3452      Y:00007D Y:00007D                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3453   
3454   
3455                                                                                    ; wfk -  Next line: Add DCRestore,StrtRstInt
 to machine state at end of CLEAR to stabilize baseline (04/04/07)
3456      Y:00007E Y:00007E                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3457                                END_PARALLEL_CLEAR_2
3458   
3459                                ; this parallel split mixes two central rows on the CCD.
3460                                PARALLEL_CLEAR_SPLIT
3461      Y:00007F Y:00007F                   DC      END_PARALLEL_CLEAR_SPLIT-PARALLEL_CLEAR_SPLIT-1
3462      Y:000080 Y:000080                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3463   
3464      Y:000081 Y:000081                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3465      Y:000082 Y:000082                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2H|V2_2L|V3_2L
3466      Y:000083 Y:000083                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2H|V2_2H|V3_2L
3467      Y:000084 Y:000084                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3468      Y:000085 Y:000085                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2L|V2_2H|V3_2H
3469      Y:000086 Y:000086                   DC      CLK2|$000000|V1_1H|V2_1L|V3_1L|V1_2L|V2_2L|V3_2H
3470   
3471      Y:000087 Y:000087                   DC      CLK3|P_DELAY|RL|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH
3472                                                                                    ; wfk -  Next line: Add DCRestore,StrtRstInt
 to machine state at end of CLEAR to stabilize baseline (04/04/07)
3473      Y:000088 Y:000088                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3474                                END_PARALLEL_CLEAR_SPLIT
3475   
3476   
3477                                ;Shift towards register 2 only the Vx_2 clocks
3478                                 PARALLEL_FRAME_2
3479      Y:000089 Y:000089                   DC      END_PARALLEL_FRAME_2-PARALLEL_FRAME_2-1
3480      Y:00008A Y:00008A                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3481      Y:00008B Y:00008B                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3482      Y:00008C Y:00008C                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1L|V1_2H|V2_2L|V3_2L
3483      Y:00008D Y:00008D                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1L|V1_2H|V2_2H|V3_2L
3484      Y:00008E Y:00008E                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1L|V1_2L|V2_2H|V3_2L
3485      Y:00008F Y:00008F                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1L|V1_2L|V2_2H|V3_2H
3486      Y:000090 Y:000090                   DC      CLK2|$000000|V1_1H|V2_1L|V3_1L|V1_2L|V2_2L|V3_2H ; shut TG
3487      Y:000091 Y:000091                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3488      Y:000092 Y:000092                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 62



k add DCRestore,StrtRstInt
3489                                END_PARALLEL_FRAME_2
3490   
3491                                ;Shift towards register 1
3492                                ;charge stored under 2&3.  Issue with switching between which register to go to.
3493                                 PARALLEL_FRAME_1
3494      Y:000093 Y:000093                   DC      END_PARALLEL_FRAME_1-PARALLEL_FRAME_1-1
3495      Y:000094 Y:000094                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3496      Y:000095 Y:000095                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2L
3497      Y:000096 Y:000096                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2H|V2_2L|V3_2L
3498      Y:000097 Y:000097                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2H|V2_2L|V3_2L
3499      Y:000098 Y:000098                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2H|V2_2L|V3_2L
3500      Y:000099 Y:000099                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2H|V2_2L|V3_2L
3501      Y:00009A Y:00009A                   DC      CLK2|$000000|V1_1H|V2_1L|V3_1L|V1_2H|V2_2L|V3_2L
3502      Y:00009B Y:00009B                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3503      Y:00009C Y:00009C                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3504                                END_PARALLEL_FRAME_1
3505   
3506                                PARALLEL_CLEAR_FRAME_1                              ; only the V*_1 clocks run, and all the V*_2
 clocks are high
3507      Y:00009D Y:00009D                   DC      END_PARALLEL_CLEAR_FRAME_1-PARALLEL_CLEAR_FRAME_1-1
3508      Y:00009E Y:00009E                   DC      CLK3|$000000|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3509      Y:00009F Y:00009F                   DC      CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2H|V3_2H
3510      Y:0000A0 Y:0000A0                   DC      CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2H|V2_2H|V3_2H
3511      Y:0000A1 Y:0000A1                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2H|V2_2H|V3_2H
3512      Y:0000A2 Y:0000A2                   DC      CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2H|V2_2H|V3_2H
3513      Y:0000A3 Y:0000A3                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2H|V2_2H|V3_2H
3514      Y:0000A4 Y:0000A4                   DC      CLK2|$000000|V1_1H|V2_1L|V3_1L|V1_2H|V2_2H|V3_2H
3515      Y:0000A5 Y:0000A5                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3516                                                                                    ; wfk -  Next line: Add DCRestore,StrtRstInt
 to machine state at end of CLEAR to stabilize baseline (04/04/07)
3517      Y:0000A6 Y:0000A6                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3518                                END_PARALLEL_CLEAR_FRAME_1
3519   
3520                                PARALLEL_CLEAR_FRAME_2                              ; only the V*_2 clocks run, and all the V*_1
 clocks are high
3521      Y:0000A7 Y:0000A7                   DC      END_PARALLEL_CLEAR_FRAME_2-PARALLEL_CLEAR_FRAME_2-1
3522      Y:0000A8 Y:0000A8                   DC      CLK3|$000000|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3523      Y:0000A9 Y:0000A9                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1H|V1_2H|V2_2L|V3_2H
3524      Y:0000AA Y:0000AA                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1H|V1_2H|V2_2L|V3_2L
3525      Y:0000AB Y:0000AB                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1H|V1_2H|V2_2H|V3_2L
3526      Y:0000AC Y:0000AC                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1H|V1_2L|V2_2H|V3_2L
3527      Y:0000AD Y:0000AD                   DC      CLK2|P_DELAY|V1_1H|V2_1H|V3_1H|V1_2L|V2_2H|V3_2H
3528      Y:0000AE Y:0000AE                   DC      CLK2|$000000|V1_1H|V2_1H|V3_1H|V1_2L|V2_2L|V3_2H
3529      Y:0000AF Y:0000AF                   DC      CLK3|P_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Shut the transfer g
ate
3530                                                                                    ; wfk -  Next line: Add DCRestore,StrtRstInt
 to machine state at end of CLEAR to stabilize baseline (04/04/07)
3531      Y:0000B0 Y:0000B0                   DC      VIDEO+$000000+%0011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wf
k add DCRestore,StrtRstInt
3532                                END_PARALLEL_CLEAR_FRAME_2
3533   
3535                                ;PARALLEL_CLEAR_SPLIT
3536                                ;  DC  END_PARALLEL_CLEAR_SPLIT-PARALLEL_CLEAR_SPLIT-1
3537                                ;  DC  CLK3|$000000|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TL ;SW->lo
3538                                ;
3539                                ;  DC  CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2H|V2_2L|V3_2H
3540                                ;  DC  CLK2|P_DELAY|V1_1L|V2_1L|V3_1H|V1_2H|V2_2L|V3_2L
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 63



3541                                ;  DC  CLK2|P_DELAY|V1_1L|V2_1H|V3_1H|V1_2H|V2_2H|V3_2L
3542                                ;  DC  CLK2|P_DELAY|V1_1L|V2_1H|V3_1L|V1_2L|V2_2H|V3_2L
3543                                ;  DC  CLK2|P_DELAY|V1_1H|V2_1H|V3_1L|V1_2L|V2_2H|V3_2H
3544                                ;  DC  CLK2|P_DELAY|V1_1H|V2_1L|V3_1L|V1_2L|V2_2L|V3_2H
3545                                ;  DC  CLK2|P_DELAY|V1_1H|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H
3546                                ;  DC  CLK2|$000000|V1_1L|V2_1L|V3_1H|V1_2L|V2_2L|V3_2H
3547                                ;
3548                                ;  DC  CLK3|P_DELAY|RL|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH
3549                                ;  ; wfk -  Next line: Add DCRestore,StrtRstInt to machine state at end of CLEAR to stabilize ba
seline (04/04/07)
3550                                ;  DC  VIDEO+$000000+%1111000  ; ADCLatch,NonInv,DCRestore,StrtRstInt. - wfk add DCRestore,StrtR
stInt
3551                                ;END_PARALLEL_CLEAR_SPLIT
3552   
3553                                ; ARC47:  |xfer|A/D|integ|polarity|not used|not used|rst| (1 => switch open)
3554                                SERIAL_IDLE_LEFT                                    ; Clock serial charge from both L and R ends
3555      Y:0000B1 Y:0000B1                   DC      END_SERIAL_IDLE_LEFT-SERIAL_IDLE_LEFT-1
3556      Y:0000B2 Y:0000B2                   DC      VIDEO+$000000+%1111000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3557                                ; L2 idle version
3558                                ; 2->3->1->2->3
3559      Y:0000B3 Y:0000B3                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3560      Y:0000B4 Y:0000B4                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3561      Y:0000B5 Y:0000B5                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3562      Y:0000B6 Y:0000B6                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3563      Y:0000B7 Y:0000B7                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3564      Y:0000B8 Y:0000B8                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3565      Y:0000B9 Y:0000B9                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3566   
3567   
3568      Y:0000BA Y:0000BA                   DC      CLK3|$0000000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;dummy for trans
mit delay
3569      Y:0000BB Y:0000BB                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3570      Y:0000BC Y:0000BC                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3571      Y:0000BD Y:0000BD                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3572      Y:0000BE Y:0000BE                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3573      Y:0000BF Y:0000BF                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3574      Y:0000C0 Y:0000C0                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3575      Y:0000C1 Y:0000C1                   DC      VIDEO+DCRST_DELAY+%0010001        ;  Now sit around for at least 520ns while t
he conversion happens
3576                                END_SERIAL_IDLE_LEFT
3577   
3578   
3579                                ; ARC47:  |xfer|A/D|integ|polarity|not used|not used|rst| (1 => switch open)
3580                                SERIAL_IDLE_LEFT_NO_POL                             ; Clock serial charge from both L and R ends
3581      Y:0000C2 Y:0000C2                   DC      END_SERIAL_IDLE_LEFT_NO_POL-SERIAL_IDLE_LEFT_NO_POL-1
3582      Y:0000C3 Y:0000C3                   DC      VIDEO+$000000+%1111000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3583   
3584                                ; L2 idle version
3585                                ; 2->3->1->2->3
3586      Y:0000C4 Y:0000C4                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3587      Y:0000C5 Y:0000C5                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3L|H2_1H|H2_2L|H2_3L|WL|TH ;h2->hi
3588      Y:0000C6 Y:0000C6                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2H|H1_3L|H2_1H|H2_2H|H2_3L|WL|TH ;h1->lo
3589      Y:0000C7 Y:0000C7                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3L|H2_1L|H2_2H|H2_3L|WL|TH ;h3->hi
3590      Y:0000C8 Y:0000C8                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3H|H2_1L|H2_2H|H2_3H|WL|TH ;h2->lo
3591      Y:0000C9 Y:0000C9                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2L|H1_3H|H2_1L|H2_2L|H2_3H|WL|TH ;h1->hi
3592   
3593      Y:0000CA Y:0000CA                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3594   
3595   
3596      Y:0000CB Y:0000CB                   DC      CLK3|$0000000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;dummy for trans
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 64



mit delay
3597      Y:0000CC Y:0000CC                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3598      Y:0000CD Y:0000CD                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3599      Y:0000CE Y:0000CE                   DC      VIDEO+$000000+%0011001            ; Stop Integrate and sel inverting int.
3600      Y:0000CF Y:0000CF                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi -- charge d
ump
3601   
3602                                                                                    ;SW going low here suggests that no charge w
ill leak over OG barrier onto sense node.
3603      Y:0000D0 Y:0000D0                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3604      Y:0000D1 Y:0000D1                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3605      Y:0000D2 Y:0000D2                   DC      VIDEO+DCRST_DELAY+%0011001        ; ,NonInv  ;mF to do ADC sampling before res
etting
3606   
3607                                ;  DC  CLK3|POST_SET_DLY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL ;SW->lo
3608                                END_SERIAL_IDLE_LEFT_NO_POL
3609   
3610                                ; ARC47:  |xfer|A/D|integ|polarity|not used|not used|rst| (1 => switch open)
3611                                SERIAL_IDLE_RIGHT                                   ; Clock serial charge from both L and R ends
3612      Y:0000D3 Y:0000D3                   DC      END_SERIAL_IDLE_RIGHT-SERIAL_IDLE_RIGHT-1
3613      Y:0000D4 Y:0000D4                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3614   
3615                                ; L2 read version
3616                                ; 2->3->1->2->3
3617      Y:0000D5 Y:0000D5                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h2->lo,SW->lo,Reset
_On
3618      Y:0000D6 Y:0000D6                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3619      Y:0000D7 Y:0000D7                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->hi
3620      Y:0000D8 Y:0000D8                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->l
3621      Y:0000D9 Y:0000D9                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->hi
3622      Y:0000DA Y:0000DA                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->lo
3623      Y:0000DB Y:0000DB                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->hi, Reset_O
ff|Delay
3624   
3625   
3626      Y:0000DC Y:0000DC                   DC      CLK3|$0000000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;dummy for trans
mit delay
3627      Y:0000DD Y:0000DD                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3628      Y:0000DE Y:0000DE                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3629      Y:0000DF Y:0000DF                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3630      Y:0000E0 Y:0000E0                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi -- charge d
ump
3631   
3632                                                                                    ;SW going low here suggests that no charge w
ill leak over OG barrier onto sense node.
3633      Y:0000E1 Y:0000E1                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3634      Y:0000E2 Y:0000E2                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3635      Y:0000E3 Y:0000E3                   DC      VIDEO+$000000+%0110001            ; StopIntegrator
3636      Y:0000E4 Y:0000E4                   DC      VIDEO+DCRST_DELAY+%0110001        ; ADCLatch,NonInv  ;mF to do ADC sampling be
fore resetting
3637   
3638                                ;  DC  CLK3|POST_SET_DLY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL ;SW->lo
3639   
3640                                END_SERIAL_IDLE_RIGHT
3641   
3642   
3643                                ; ARC47:  |xfer|A/D|integ|polarity|not used|not used|rst| (1 => switch open)
3644                                SERIAL_IDLE_RIGHT_NO_POL                            ; Clock serial charge from both L and R ends
3645      Y:0000E5 Y:0000E5                   DC      END_SERIAL_IDLE_RIGHT_NO_POL-SERIAL_IDLE_RIGHT_NO_POL-1
3646      Y:0000E6 Y:0000E6                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3647   
3648                                ; L2 read version
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 65



3649                                ; 2->3->1->2->3
3650      Y:0000E7 Y:0000E7                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h2->lo,SW->lo,Reset
_On
3651      Y:0000E8 Y:0000E8                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3652      Y:0000E9 Y:0000E9                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->hi
3653      Y:0000EA Y:0000EA                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->l
3654      Y:0000EB Y:0000EB                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->hi
3655      Y:0000EC Y:0000EC                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->lo
3656      Y:0000ED Y:0000ED                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->hi, Reset_O
ff|Delay
3657   
3658   
3659      Y:0000EE Y:0000EE                   DC      CLK3|$0000000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;dummy for trans
mit delay
3660      Y:0000EF Y:0000EF                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3661      Y:0000F0 Y:0000F0                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3662      Y:0000F1 Y:0000F1                   DC      VIDEO+$000000+%0011001            ; Stop Integrate and sel inverting int.
3663      Y:0000F2 Y:0000F2                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi -- charge d
ump
3664   
3665                                                                                    ;SW going low here suggests that no charge w
ill leak over OG barrier onto sense node.
3666      Y:0000F3 Y:0000F3                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3667      Y:0000F4 Y:0000F4                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3668      Y:0000F5 Y:0000F5                   DC      VIDEO+$000000+%0011001            ; StopIntegrator
3669      Y:0000F6 Y:0000F6                   DC      VIDEO+DCRST_DELAY+%0111001        ; ADCLatch,NonInv  ;mF to do ADC sampling be
fore resetting
3670   
3671                                ;  DC  CLK3|POST_SET_DLY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL ;SW->lo
3672   
3673                                END_SERIAL_IDLE_RIGHT_NO_POL
3674   
3675                                ; ARC47:  |xfer|A/D|integ|polarity|not used|not used|rst| (1 => switch open)
3676                                SERIAL_IDLE_SPLIT
3677      Y:0000F7 Y:0000F7                   DC      END_SERIAL_IDLE_SPLIT-SERIAL_IDLE_SPLIT-1
3678      Y:0000F8 Y:0000F8                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3679   
3680                                ; split read version:
3681      Y:0000F9 Y:0000F9                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3682   
3683                                ;  DC  CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3684                                ;  DC  CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3685                                ;  DC  CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3686                                ;  DC  CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3687                                ;  DC  CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3688   
3689      Y:0000FA Y:0000FA                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2L|H1_3H|H2_1H|H2_2L|H2_3L|WL|TH ;h2->hi
3690      Y:0000FB Y:0000FB                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3H|H2_1H|H2_2H|H2_3L|WL|TH ;h1->lo
3691      Y:0000FC Y:0000FC                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3L|H2_1L|H2_2H|H2_3L|WL|TH ;h3->hi
3692      Y:0000FD Y:0000FD                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2H|H1_3L|H2_1L|H2_2H|H2_3H|WL|TH ;h2->lo
3693      Y:0000FE Y:0000FE                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3L|H2_1L|H2_2L|H2_3H|WL|TH ;h1->hi
3694   
3695   
3696      Y:0000FF Y:0000FF                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3697   
3698      Y:000100 Y:000100                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;dummy for transmit 
delay
3699      Y:000101 Y:000101                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3700      Y:000102 Y:000102                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3701      Y:000103 Y:000103                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3702      Y:000104 Y:000104                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 66



3703      Y:000105 Y:000105                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3704      Y:000106 Y:000106                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3705      Y:000107 Y:000107                   DC      VIDEO+$000000+%0010001            ; StopIntegrator
3706      Y:000108 Y:000108                   DC      VIDEO+DCRST_DELAY+%0110001        ; ADCLatch,NonInv  ;mF to do ADC sampling be
fore resetting
3707                                END_SERIAL_IDLE_SPLIT
3708   
3709   
3710                                ; ARC47:  |xfer|A/D|integ|polarity|not used|not used|rst| (1 => switch open)
3711                                SERIAL_IDLE_SPLIT_NO_POL
3712      Y:000109 Y:000109                   DC      END_SERIAL_IDLE_SPLIT_NO_POL-SERIAL_IDLE_SPLIT_NO_POL-1
3713      Y:00010A Y:00010A                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3714   
3715                                ; split read version:
3716      Y:00010B Y:00010B                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3717      Y:00010C Y:00010C                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3718      Y:00010D Y:00010D                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3719      Y:00010E Y:00010E                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3720      Y:00010F Y:00010F                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3721      Y:000110 Y:000110                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3722      Y:000111 Y:000111                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3723   
3724      Y:000112 Y:000112                   DC      CLK3|$000000|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;dummy for transmit 
delay
3725      Y:000113 Y:000113                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3726      Y:000114 Y:000114                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3727      Y:000115 Y:000115                   DC      VIDEO+$000000+%0011001            ; Stop Integrate and sel inverting int.
3728      Y:000116 Y:000116                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3729      Y:000117 Y:000117                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3730      Y:000118 Y:000118                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3731      Y:000119 Y:000119                   DC      VIDEO+$000000+%0011001            ; StopIntegrator
3732      Y:00011A Y:00011A                   DC      VIDEO+DCRST_DELAY+%0111001        ; ADCLatch,NonInv  ;mF to do ADC sampling be
fore resetting
3733                                END_SERIAL_IDLE_SPLIT_NO_POL
3734   
3735   
3736                                ;start binning waveforms
3737                                CCD_RESET                                           ;Used for binning only
3738      Y:00011B Y:00011B                   DC      END_CCD_RESET-CCD_RESET-1
3739      Y:00011C Y:00011C                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3740                                END_CCD_RESET
3741   
3742                                SERIAL_CLOCK_L                                      ;"NORMAL" clocking
3743      Y:00011D Y:00011D                   DC      END_SERIAL_CLOCK_L-SERIAL_CLOCK_L-1
3744      Y:00011E Y:00011E                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3745      Y:00011F Y:00011F                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3746      Y:000120 Y:000120                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3747      Y:000121 Y:000121                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3748      Y:000122 Y:000122                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3749      Y:000123 Y:000123                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3750      Y:000124 Y:000124                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3751      Y:000125 Y:000125                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3752                                END_SERIAL_CLOCK_L
3753   
3754                                SERIAL_CLOCK_R                                      ;"REVERSE" clocking
3755      Y:000126 Y:000126                   DC      END_SERIAL_CLOCK_R-SERIAL_CLOCK_R-1
3756      Y:000127 Y:000127                   DC      VIDEO+$000000+%1011000            ; NonInv,DCRestore,StrtRstInt.
3757      Y:000128 Y:000128                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3758      Y:000129 Y:000129                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h2->hi
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 67



3759      Y:00012A Y:00012A                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h1->lo
3760      Y:00012B Y:00012B                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3761      Y:00012C Y:00012C                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h2->lo
3762      Y:00012D Y:00012D                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h1->hi
3763      Y:00012E Y:00012E                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3764                                END_SERIAL_CLOCK_R
3765   
3766                                SERIAL_CLOCK_SPLIT                                  ;"SPLIT" clocking
3767      Y:00012F Y:00012F                   DC      END_SERIAL_CLOCK_SPLIT-SERIAL_CLOCK_SPLIT-1
3768      Y:000130 Y:000130                   DC      VIDEO+$000000+%1011000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3769      Y:000131 Y:000131                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3770      Y:000132 Y:000132                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3771      Y:000133 Y:000133                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3772      Y:000134 Y:000134                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3773      Y:000135 Y:000135                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3774      Y:000136 Y:000136                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3775      Y:000137 Y:000137                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3776                                END_SERIAL_CLOCK_SPLIT
3777   
3778   
3779                                VIDEO_PROCESS
3780      Y:000138 Y:000138                   DC      END_VIDEO_PROCESS-VIDEO_PROCESS-1
3781                                ;SXMIT  DC  $00F000     ; Transmit A/D data to host
3782      Y:000139 Y:000139                   DC      VIDEO+$000000+%1011000            ; StopDCRestore and StopResetIntegrator
3783      Y:00013A Y:00013A                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3784      Y:00013B Y:00013B                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3785      Y:00013C Y:00013C                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3786      Y:00013D Y:00013D                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3787      Y:00013E Y:00013E                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3788      Y:00013F Y:00013F                   DC      VIDEO+$000000+%0010001            ; StopResetIntegrator
3789      Y:000140 Y:000140                   DC      VIDEO+DCRST_DELAY+%0110001        ; ADCLatch,NonInv  ;mF to do ADC sampeling b
evore resetting
3790                                END_VIDEO_PROCESS
3791                                ;end binning waveforms
3792   
3793   
3794                                ; Video processor bit definition
3795                                ;      xfer, A/D, integ, Pol+, Pol-, DCrestore, rst   (1 => switch open)
3796   
3797                                ; These are the three reading tables. Make sure they're all the same length
3798                                ; 2->3->1->2
3799                                SERIAL_READ_LEFT
3800      Y:000141 Y:000141                   DC      END_SERIAL_READ_LEFT-SERIAL_READ_LEFT-1
3801      Y:000142 Y:000142                   DC      VIDEO+$000000+%1111000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3802      Y:000143 Y:000143                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3803      Y:000144 Y:000144                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3804      Y:000145 Y:000145                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3805      Y:000146 Y:000146                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3806      Y:000147 Y:000147                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3807      Y:000148 Y:000148                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3808      Y:000149 Y:000149                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3809      Y:00014A Y:00014A         SXL       DC      $00F000                           ;Transmit a/d data to host
3810      Y:00014B Y:00014B                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3811      Y:00014C Y:00014C                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3812      Y:00014D Y:00014D                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3813      Y:00014E Y:00014E                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3814      Y:00014F Y:00014F                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3815      Y:000150 Y:000150                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3816      Y:000151 Y:000151                   DC      VIDEO+DCRST_DELAY+%0010001        ;  Now sit around for at least 520ns while t
he conversion happens
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 68



3817      Y:000152 Y:000152                   DC      VIDEO+$30000+%0010001             ;  Now sit around for at least 520ns while t
he conversion happens
3818                                END_SERIAL_READ_LEFT
3819   
3820                                SERIAL_BIN_LEFT
3821      Y:000153 Y:000153                   DC      END_SERIAL_BIN_LEFT-SERIAL_BIN_LEFT-1
3822                                ;  DC  VIDEO+$000000+%1111000  ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3823      Y:000154 Y:000154                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3824      Y:000155 Y:000155                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3825      Y:000156 Y:000156                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3826      Y:000157 Y:000157                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3827      Y:000158 Y:000158                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3828      Y:000159 Y:000159                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3829                                ;  DC  CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3830                                END_SERIAL_BIN_LEFT
3831   
3832                                SERIAL_READ_SPLIT
3833      Y:00015A Y:00015A                   DC      END_SERIAL_READ_SPLIT-SERIAL_READ_SPLIT-1
3834      Y:00015B Y:00015B                   DC      VIDEO+$000000+%1111000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3835      Y:00015C Y:00015C                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3836   
3837      Y:00015D Y:00015D                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2L|H1_3H|H2_1H|H2_2L|H2_3L|WL|TH ;h2->hi
3838      Y:00015E Y:00015E                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3H|H2_1H|H2_2H|H2_3L|WL|TH ;h1->lo
3839      Y:00015F Y:00015F                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3L|H2_1L|H2_2H|H2_3L|WL|TH ;h3->hi
3840      Y:000160 Y:000160                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2H|H1_3L|H2_1L|H2_2H|H2_3H|WL|TH ;h2->lo
3841      Y:000161 Y:000161                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3L|H2_1L|H2_2L|H2_3H|WL|TH ;h1->hi
3842   
3843                                ;  DC  CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3844                                ;  DC  CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3845                                ;  DC  CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3846                                ;  DC  CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3847                                ;  DC  CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3848   
3849   
3850      Y:000162 Y:000162                   DC      CLK3|PRE_SET_DLY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;Reset_Off|Delay
3851      Y:000163 Y:000163         SXRL      DC      $00F0C2                           ;Transmit a/d data to host
3852      Y:000164 Y:000164                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3853      Y:000165 Y:000165                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3854      Y:000166 Y:000166                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3855      Y:000167 Y:000167                   DC      CLK3|SW_DELAY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WH|TH ;SW->hi
3856      Y:000168 Y:000168                   DC      CLK3|POST_SET_DLY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;SW->lo
3857      Y:000169 Y:000169                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3858      Y:00016A Y:00016A                   DC      VIDEO+DCRST_DELAY+%0010001        ; Sit around whilst sampling.
3859      Y:00016B Y:00016B                   DC      VIDEO+$30000+%0010001             ;  Now sit around for at least 520ns while t
he conversion happens
3860                                END_SERIAL_READ_SPLIT
3861   
3862   
3863                                ; SERIAL_READ_SPLIT_SPECIAL_QUAD
3864                                ; for QUAD "ALL"
3865                                ; transmits L1 | U1 | U2 | L2
3866                                SERIAL_READ_SPLIT_SPECIAL_QUAD
3867      Y:00016C Y:00016C                   DC      END_SERIAL_READ_SPLIT_SPECIAL_QUAD-SERIAL_READ_SPLIT_SPECIAL_QUAD-1
3868      Y:00016D Y:00016D                   DC      VIDEO+$000000+%1111000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3869      Y:00016E Y:00016E                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3870   
3871      Y:00016F Y:00016F                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2L|H1_3H|H2_1H|H2_2L|H2_3L|WL|TH ;h2->hi
3872      Y:000170 Y:000170                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3H|H2_1H|H2_2H|H2_3L|WL|TH ;h1->lo
3873      Y:000171 Y:000171                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3L|H2_1L|H2_2H|H2_3L|WL|TH ;h3->hi
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 69



3874      Y:000172 Y:000172                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2H|H1_3L|H2_1L|H2_2H|H2_3H|WL|TH ;h2->lo
3875      Y:000173 Y:000173                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3L|H2_1L|H2_2L|H2_3H|WL|TH ;h1->hi
3876   
3877      Y:000174 Y:000174                   DC      CLK3|PRE_SET_DLY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;Reset_Off|Delay
3878      Y:000175 Y:000175                   DC      $00F000                           ; Transmit A/D 0 data to host (L1)
3879      Y:000176 Y:000176                   DC      CLK3|$20000|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;Reset_Off|Delay
3880      Y:000177 Y:000177                   DC      $00F0C3                           ; Transmit A/D 3 data to host (U1)
3881      Y:000178 Y:000178                   DC      CLK3|$20000|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;Reset_Off|Delay
3882      Y:000179 Y:000179                   DC      $00F041                           ; Transmit A/D 1 data to host (U2)
3883      Y:00017A Y:00017A                   DC      CLK3|$20000|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;Reset_Off|Delay
3884      Y:00017B Y:00017B                   DC      $00F082                           ; Transmit A/D 2 data to host (L2)
3885      Y:00017C Y:00017C                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3886      Y:00017D Y:00017D                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3887      Y:00017E Y:00017E                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3888      Y:00017F Y:00017F                   DC      CLK3|SW_DELAY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WH|TH ;SW->hi
3889      Y:000180 Y:000180                   DC      CLK3|POST_SET_DLY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;SW->lo
3890      Y:000181 Y:000181                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3891      Y:000182 Y:000182                   DC      VIDEO+DCRST_DELAY+%0010001        ; Sit around whilst sampling.
3892      Y:000183 Y:000183                   DC      VIDEO+$50000+%0010001             ;  Now sit around for at least 520ns while t
he conversion happens
3893                                END_SERIAL_READ_SPLIT_SPECIAL_QUAD
3894   
3895   
3896                                ; SERIAL_READ_SPLIT_SPECIAL__2
3897                                ; for SPLIT2 "__2"
3898                                ; transmits L2 | U2
3899                                SERIAL_READ_SPLIT_SPECIAL__2
3900      Y:000184 Y:000184                   DC      END_SERIAL_READ_SPLIT_SPECIAL__2-SERIAL_READ_SPLIT_SPECIAL__2-1
3901      Y:000185 Y:000185                   DC      VIDEO+$000000+%1111000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3902      Y:000186 Y:000186                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3903   
3904      Y:000187 Y:000187                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2L|H1_3H|H2_1H|H2_2L|H2_3L|WL|TH ;h2->hi
3905      Y:000188 Y:000188                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3H|H2_1H|H2_2H|H2_3L|WL|TH ;h1->lo
3906      Y:000189 Y:000189                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3L|H2_1L|H2_2H|H2_3L|WL|TH ;h3->hi
3907      Y:00018A Y:00018A                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2H|H1_3L|H2_1L|H2_2H|H2_3H|WL|TH ;h2->lo
3908      Y:00018B Y:00018B                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3L|H2_1L|H2_2L|H2_3H|WL|TH ;h1->hi
3909   
3910      Y:00018C Y:00018C                   DC      CLK3|PRE_SET_DLY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;Reset_Off|Delay
3911      Y:00018D Y:00018D                   DC      $00F082                           ; Transmit A/D 2 data to host (L2)
3912      Y:00018E Y:00018E                   DC      CLK3|$20000|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;Reset_Off|Delay
3913      Y:00018F Y:00018F                   DC      $00F041                           ; Transmit A/D 1 data to host (U2)
3914      Y:000190 Y:000190                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3915      Y:000191 Y:000191                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3916      Y:000192 Y:000192                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3917      Y:000193 Y:000193                   DC      CLK3|SW_DELAY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WH|TH ;SW->hi
3918      Y:000194 Y:000194                   DC      CLK3|POST_SET_DLY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;SW->lo
3919      Y:000195 Y:000195                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3920      Y:000196 Y:000196                   DC      VIDEO+DCRST_DELAY+%0010001        ; Sit around whilst sampling.
3921      Y:000197 Y:000197                   DC      VIDEO+$50000+%0010001             ;  Now sit around for at least 520ns while t
he conversion happens
3922                                END_SERIAL_READ_SPLIT_SPECIAL__2
3923   
3924   
3925                                ; SERIAL_READ_SPLIT_SPECIAL__1
3926                                ; for SPLIT1 "__1"
3927                                ; transmits L1 | U1
3928                                SERIAL_READ_SPLIT_SPECIAL__1
3929      Y:000198 Y:000198                   DC      END_SERIAL_READ_SPLIT_SPECIAL__1-SERIAL_READ_SPLIT_SPECIAL__1-1
3930      Y:000199 Y:000199                   DC      VIDEO+$000000+%1111000            ; ADCLatch,NonInv,DCRestore,StrtRstInt.
3931      Y:00019A Y:00019A                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 70



3932   
3933      Y:00019B Y:00019B                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2L|H1_3H|H2_1H|H2_2L|H2_3L|WL|TH ;h2->hi
3934      Y:00019C Y:00019C                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3H|H2_1H|H2_2H|H2_3L|WL|TH ;h1->lo
3935      Y:00019D Y:00019D                   DC      CLK3|S_DELAY|RH|H1_1L|H1_2H|H1_3L|H2_1L|H2_2H|H2_3L|WL|TH ;h3->hi
3936      Y:00019E Y:00019E                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2H|H1_3L|H2_1L|H2_2H|H2_3H|WL|TH ;h2->lo
3937      Y:00019F Y:00019F                   DC      CLK3|S_DELAY|RH|H1_1H|H1_2L|H1_3L|H2_1L|H2_2L|H2_3H|WL|TH ;h1->hi
3938   
3939      Y:0001A0 Y:0001A0                   DC      CLK3|PRE_SET_DLY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;Reset_Off|Delay
3940      Y:0001A1 Y:0001A1                   DC      $00F000                           ; Transmit A/D 0 data to host (L1)
3941      Y:0001A2 Y:0001A2                   DC      CLK3|$20000|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;Reset_Off|Delay
3942      Y:0001A3 Y:0001A3                   DC      $00F0C3                           ; Transmit A/D 3 data to host (U1)
3943      Y:0001A4 Y:0001A4                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3944      Y:0001A5 Y:0001A5                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3945      Y:0001A6 Y:0001A6                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3946      Y:0001A7 Y:0001A7                   DC      CLK3|SW_DELAY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WH|TH ;SW->hi
3947      Y:0001A8 Y:0001A8                   DC      CLK3|POST_SET_DLY|RL|H1_1H|H1_2L|H1_3H|H2_1H|H2_2L|H2_3H|WL|TH ;SW->lo
3948      Y:0001A9 Y:0001A9                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3949      Y:0001AA Y:0001AA                   DC      VIDEO+DCRST_DELAY+%0010001        ; Sit around whilst sampling.
3950      Y:0001AB Y:0001AB                   DC      VIDEO+$50000+%0010001             ;  Now sit around for at least 520ns while t
he conversion happens
3951                                END_SERIAL_READ_SPLIT_SPECIAL__1
3952   
3953   
3954                                ; 2->1->3->2
3955                                SERIAL_READ_RIGHT
3956      Y:0001AC Y:0001AC                   DC      END_SERIAL_READ_RIGHT-SERIAL_READ_RIGHT-1
3957      Y:0001AD Y:0001AD                   DC      VIDEO+$000000+%1111000            ; NonInv,DCRestore,StrtRstInt.
3958      Y:0001AE Y:0001AE                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3959      Y:0001AF Y:0001AF                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h2->hi
3960      Y:0001B0 Y:0001B0                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h1->lo
3961      Y:0001B1 Y:0001B1                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3962      Y:0001B2 Y:0001B2                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h2->lo
3963      Y:0001B3 Y:0001B3                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h1->hi
3964      Y:0001B4 Y:0001B4                   DC      CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3965      Y:0001B5 Y:0001B5         SXR       DC      $00F041                           ;Transmit a/d data to host
3966      Y:0001B6 Y:0001B6                   DC      VIDEO+$000000+%0011001            ; StopDCRestore and StopResetIntegrator
3967      Y:0001B7 Y:0001B7                   DC      VIDEO+I_DELAY+%0001001            ; Integrate for I_DELAY microsec
3968      Y:0001B8 Y:0001B8                   DC      VIDEO+$000000+%0010001            ; Stop Integrate and sel inverting int.
3969      Y:0001B9 Y:0001B9                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
3970      Y:0001BA Y:0001BA                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
3971      Y:0001BB Y:0001BB                   DC      VIDEO+I_DELAY+%0000001            ; Integrate for I_DELAY microsec
3972      Y:0001BC Y:0001BC                   DC      VIDEO+DCRST_DELAY+%0010001        ; Wait for sampling
3973      Y:0001BD Y:0001BD                   DC      VIDEO+$30000+%0010001             ;
3974                                END_SERIAL_READ_RIGHT
3975   
3976                                SERIAL_BIN_RIGHT
3977      Y:0001BE Y:0001BE                   DC      END_SERIAL_BIN_RIGHT-SERIAL_BIN_RIGHT-1
3978                                ; ARC45:  |XFER|A/D|INTEGRATE|POL+|POL-|DCRESTORE|RESET|
3979                                ;          _|   _|     _       _    _      _        _     <== active LO-HI transition or level
3980                                ; DC  VIDEO+$000000+%1111000  ; NonInv,DCRestore,StrtRstInt.
3981      Y:0001BF Y:0001BF                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3982      Y:0001C0 Y:0001C0                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h2->hi
3983      Y:0001C1 Y:0001C1                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h1->lo
3984      Y:0001C2 Y:0001C2                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3985      Y:0001C3 Y:0001C3                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h2->lo
3986      Y:0001C4 Y:0001C4                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h1->hi
3987                                ; DC  CLK3|PRE_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
3988                                END_SERIAL_BIN_RIGHT
3989   
3990                                ; These are the three skipping tables. Make sure they're all the same length
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 71



3991                                SERIAL_SKIP_LEFT                                    ; Serial clocking waveform for skipping left
3992      Y:0001C5 Y:0001C5                   DC      END_SERIAL_SKIP_LEFT-SERIAL_SKIP_LEFT-1
3993      Y:0001C6 Y:0001C6                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
3994      Y:0001C7 Y:0001C7                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h2->hi
3995      Y:0001C8 Y:0001C8                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h1->lo
3996      Y:0001C9 Y:0001C9                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
3997      Y:0001CA Y:0001CA                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h2->lo
3998      Y:0001CB Y:0001CB                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h1->hi
3999      Y:0001CC Y:0001CC                   DC      CLK3|SW_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
4000      Y:0001CD Y:0001CD                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
4001                                END_SERIAL_SKIP_LEFT
4002   
4003                                SERIAL_SKIP_RIGHT                                   ; Serial clocking waveform for skipping righ
t
4004      Y:0001CE Y:0001CE                   DC      END_SERIAL_SKIP_RIGHT-SERIAL_SKIP_RIGHT-1
4005      Y:0001CF Y:0001CF                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;h3->lo,SW->lo,Reset
_On
4006      Y:0001D0 Y:0001D0                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1L|H1_2L|H1_3H|WL|TH ;h2->hi
4007      Y:0001D1 Y:0001D1                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1L|H1_2H|H1_3H|WL|TH ;h1->lo
4008      Y:0001D2 Y:0001D2                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
4009      Y:0001D3 Y:0001D3                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1H|H1_2H|H1_3L|WL|TH ;h2->lo
4010      Y:0001D4 Y:0001D4                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1H|H1_2L|H1_3L|WL|TH ;h1->hi
4011      Y:0001D5 Y:0001D5                   DC      CLK3|SW_DELAY|RH|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
4012      Y:0001D6 Y:0001D6                   DC      CLK3|POST_SET_DLY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;SW->lo
4013                                END_SERIAL_SKIP_RIGHT
4014   
4015                                SERIAL_SKIP_SPLIT                                   ; Serial clocking waveform for skipping both
 ends
4016      Y:0001D7 Y:0001D7                   DC      END_SERIAL_SKIP_SPLIT-SERIAL_SKIP_SPLIT-1
4017      Y:0001D8 Y:0001D8                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1L|H1_2L|H1_3H|WL|TH ;h2->hi
4018      Y:0001D9 Y:0001D9                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1L|H1_2H|H1_3H|WL|TH ;h1->lo
4019      Y:0001DA Y:0001DA                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
4020      Y:0001DB Y:0001DB                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1H|H1_2H|H1_3L|WL|TH ;h2->lo
4021      Y:0001DC Y:0001DC                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3L|WL|TH ;h1->hi
4022      Y:0001DD Y:0001DD                   DC      CLK3|S_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
4023      Y:0001DE Y:0001DE                   DC      CLK3|SW_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WH|TH ;SW->hi
4024                                END_SERIAL_SKIP_SPLIT
4025   
4026                                SERIAL_BIN_SPLIT                                    ; Serial clocking waveform for skipping both
 ends
4027      Y:0001DF Y:0001DF                   DC      END_SERIAL_BIN_SPLIT-SERIAL_BIN_SPLIT-1
4028      Y:0001E0 Y:0001E0                   DC      VIDEO+$000000+%1011000            ; Change nearly everything
4029      Y:0001E1 Y:0001E1                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2L|H2_3L|H1_1L|H1_2L|H1_3H|WL|TH ;h2->hi
4030      Y:0001E2 Y:0001E2                   DC      CLK3|S_DELAY|RH|H2_1H|H2_2H|H2_3L|H1_1L|H1_2H|H1_3H|WL|TH ;h1->lo
4031      Y:0001E3 Y:0001E3                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3L|H1_1L|H1_2H|H1_3L|WL|TH ;h3->hi
4032      Y:0001E4 Y:0001E4                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2H|H2_3H|H1_1H|H1_2H|H1_3L|WL|TH ;h2->lo
4033      Y:0001E5 Y:0001E5                   DC      CLK3|S_DELAY|RH|H2_1L|H2_2L|H2_3H|H1_1H|H1_2L|H1_3L|WL|TH ;h1->hi
4034      Y:0001E6 Y:0001E6                   DC      CLK3|S_DELAY|RL|H2_1H|H2_2L|H2_3H|H1_1H|H1_2L|H1_3H|WL|TH ;Reset_Off|Delay
4035                                END_SERIAL_BIN_SPLIT
4036   
4038                                ; ORG Y:$1C0,Y:$1C0   ; Download address
4040                                VSUBN
4041                                          VOLTS   VSUB,25.0                         ; Vsub  0.0 140 V, pin #
**** 4046 [SWIFT/engg/engg_48khz.waveforms.reverse 986]: Setting voltage VSUB 25.0V 1280 3081472
4047      Y:0001E8 Y:0001E8         ERHI      DC      ERHI_END-ERHI-1
4048                                          VOLTS   VSUB,0                            ; Vsub  0.0 140 V, pin #
**** 4053 [SWIFT/engg/engg_48khz.waveforms.reverse 988]: Setting voltage VSUB 0V 0 3080192
4054                                ; VOLTS V1_HI,9   ; Vertical High
4055                                ; VOLTS V1_LO,9   ; Vertical Low
4056                                ; VOLTS V2_HI,9   ; Vertical High
4057                                ; VOLTS V2_LO,9   ; Vertical Low
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 72



4058                                ; VOLTS V3_HI,9   ; Vertical High
4059                                ; VOLTS V3_LO,9   ; Vertical Low
4060                                ; VOLTS FS1_HI,9    ; Vertical High
4061                                ; VOLTS FS1_LO,9    ; Vertical Low
4062                                ; VOLTS FS2_HI,9    ; Vertical High
4063                                ; VOLTS FS2_LO,9    ; Vertical Low
4064                                ; VOLTS FS3_HI,9    ; Vertical High
4065                                ; VOLTS FS3_LO,9    ; Vertical Low
4066      Y:0001EA Y:0001EA                   DC      $200100+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #1, Vertical Clock 1
4067      Y:0001EB Y:0001EB                   DC      $200200+@CVI((9+Vmax)/(2*Vmax)*255)
4068      Y:0001EC Y:0001EC                   DC      $200400+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #2, Vertical Clock 2
4069      Y:0001ED Y:0001ED                   DC      $200800+@CVI((9+Vmax)/(2*Vmax)*255)
4070      Y:0001EE Y:0001EE                   DC      $202000+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #3, Vertical Clock 3
4071      Y:0001EF Y:0001EF                   DC      $204000+@CVI((9+Vmax)/(2*Vmax)*255)
4072      Y:0001F0 Y:0001F0                   DC      $208000+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #4, Frame Store 1
4073      Y:0001F1 Y:0001F1                   DC      $210000+@CVI((9+Vmax)/(2*Vmax)*255)
4074      Y:0001F2 Y:0001F2                   DC      $220100+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #5, Frame Store 2
4075      Y:0001F3 Y:0001F3                   DC      $220200+@CVI((9+Vmax)/(2*Vmax)*255)
4076      Y:0001F4 Y:0001F4                   DC      $220400+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #6, Frame Store 3
4077      Y:0001F5 Y:0001F5                   DC      $220800+@CVI((9+Vmax)/(2*Vmax)*255)
4078      Y:0001F6 Y:0001F6                   DC      $222000+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #7, Transfer Gate 2
4079      Y:0001F7 Y:0001F7                   DC      $224000+@CVI((9+Vmax)/(2*Vmax)*255)
4080      Y:0001F8 Y:0001F8                   DC      $228000+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #8, Transger Gate 1
4081      Y:0001F9 Y:0001F9                   DC      $230000+@CVI((9+Vmax)/(2*Vmax)*255)
4082      Y:0001FA Y:0001FA                   DC      $240100+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #9, Unused
4083      Y:0001FB Y:0001FB                   DC      $240200+@CVI((9+Vmax)/(2*Vmax)*255)
4084      Y:0001FC Y:0001FC                   DC      $240400+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #10, Unused
4085      Y:0001FD Y:0001FD                   DC      $240800+@CVI((9+Vmax)/(2*Vmax)*255)
4086      Y:0001FE Y:0001FE                   DC      $242000+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #11, Unused
4087      Y:0001FF Y:0001FF                   DC      $244000+@CVI((9+Vmax)/(2*Vmax)*255)
4088      Y:000200 Y:000200                   DC      $248000+@CVI((9+Vmax)/(2*Vmax)*255) ; Pin #12, Unused
4089      Y:000201 Y:000201                   DC      $250000+@CVI((9+Vmax)/(2*Vmax)*255)
4090      Y:000202 Y:000202         ERHI_END  DC      EPUR-ERHI_END-1
4091                                ; VOLTS V1_HI,5.0 ; Vertical High
4092                                ; VOLTS V1_LO,-3.0  ; Vertical Low
4093                                ; VOLTS V2_HI,5.0 ; Vertical High
4094                                ; VOLTS V2_LO,-3.0  ; Vertical Low
4095                                ; VOLTS V3_HI,5.0 ; Vertical High
4096                                ; VOLTS V3_LO,-3.0  ; Vertical Low
4097                                ; VOLTS FS1_HI,5.0  ; Vertical High
4098                                ; VOLTS FS1_LO,-3.0 ; Vertical Low
4099                                ; VOLTS FS2_HI,5.0  ; Vertical High
4100                                ; VOLTS FS2_LO,-3.0 ; Vertical Low
4101                                ; VOLTS FS3_HI,5.0  ; Vertical High
4102                                ; VOLTS FS3_LO,-3.0 ; Vertical Low
4103                                ;Return to normal voltages
4104      Y:000203 Y:000203                   DC      $200100+@CVI((V1_HI+Vmax)/(2*Vmax)*255) ; Pin #1, Vertical Clock 1
4105      Y:000204 Y:000204                   DC      $200200+@CVI((V1_LO+Vmax)/(2*Vmax)*255)
4106      Y:000205 Y:000205                   DC      $200400+@CVI((V2_HI+Vmax)/(2*Vmax)*255) ; Pin #2, Vertical Clock 2
4107      Y:000206 Y:000206                   DC      $200800+@CVI((V2_LO+Vmax)/(2*Vmax)*255)
4108      Y:000207 Y:000207                   DC      $202000+@CVI((V3_HI+Vmax)/(2*Vmax)*255) ; Pin #3, Vertical Clock 3
4109      Y:000208 Y:000208                   DC      $204000+@CVI((V3_LO+Vmax)/(2*Vmax)*255)
4110      Y:000209 Y:000209                   DC      $208000+@CVI((V1_HI+Vmax)/(2*Vmax)*255) ; Pin #4, Frame Store 1
4111      Y:00020A Y:00020A                   DC      $210000+@CVI((V1_LO+Vmax)/(2*Vmax)*255)
4112      Y:00020B Y:00020B                   DC      $220100+@CVI((V2_HI+Vmax)/(2*Vmax)*255) ; Pin #5, Frame Store 2
4113      Y:00020C Y:00020C                   DC      $220200+@CVI((V2_LO+Vmax)/(2*Vmax)*255)
4114      Y:00020D Y:00020D                   DC      $220400+@CVI((V3_HI+Vmax)/(2*Vmax)*255) ; Pin #6, Frame Store 3
4115      Y:00020E Y:00020E                   DC      $220800+@CVI((V3_LO+Vmax)/(2*Vmax)*255)
4116      Y:00020F Y:00020F                   DC      $222000+@CVI((V1_HI+Vmax)/(2*Vmax)*255) ; Pin #7, Transfer Gate 2
4117      Y:000210 Y:000210                   DC      $224000+@CVI((V1_LO+Vmax)/(2*Vmax)*255)
4118      Y:000211 Y:000211                   DC      $228000+@CVI((V2_HI+Vmax)/(2*Vmax)*255) ; Pin #8, Transger Gate 1
4119      Y:000212 Y:000212                   DC      $230000+@CVI((V2_LO+Vmax)/(2*Vmax)*255)
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 73



4120      Y:000213 Y:000213                   DC      $240100+@CVI((V3_HI+Vmax)/(2*Vmax)*255) ; Pin #9, Unused
4121      Y:000214 Y:000214                   DC      $240200+@CVI((V3_LO+Vmax)/(2*Vmax)*255)
4122      Y:000215 Y:000215                   DC      $240400+@CVI((FS1_HI+Vmax)/(2*Vmax)*255) ; Pin #10, Unused
4123      Y:000216 Y:000216                   DC      $240800+@CVI((FS1_LO+Vmax)/(2*Vmax)*255)
4124      Y:000217 Y:000217                   DC      $242000+@CVI((FS2_HI+Vmax)/(2*Vmax)*255) ; Pin #11, Unused
4125      Y:000218 Y:000218                   DC      $244000+@CVI((FS2_LO+Vmax)/(2*Vmax)*255)
4126      Y:000219 Y:000219                   DC      $248000+@CVI((FS3_HI+Vmax)/(2*Vmax)*255) ; Pin #12, Unused
4127      Y:00021A Y:00021A                   DC      $250000+@CVI((FS3_LO+Vmax)/(2*Vmax)*255)
4128   
4129      Y:00021B Y:00021B                   DC      $2A0100+@CVI((RL_HI+Vmax)/(2*Vmax)*255) ; Pin #34, Reset Gate Upper
4130      Y:00021C Y:00021C                   DC      $2A0200+@CVI((RL_LO+Vmax)/(2*Vmax)*255)
4131      Y:00021D Y:00021D                   DC      $2A0400+@CVI((RU_HI+Vmax)/(2*Vmax)*255) ; Pin #35, Reset Gate Lower
4132      Y:00021E Y:00021E                   DC      $2A0800+@CVI((RU_LO+Vmax)/(2*Vmax)*255)
4133   
4134      Y:00021F Y:00021F         EPUR      DC      EPUR_END-EPUR-1
4135      Y:000220 Y:000220                   DC      $200100+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #1, Vertical Clock 1
4136      Y:000221 Y:000221                   DC      $200200+@CVI((-9+Vmax)/(2*Vmax)*255)
4137      Y:000222 Y:000222                   DC      $200400+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #2, Vertical Clock 2
4138      Y:000223 Y:000223                   DC      $200800+@CVI((-9+Vmax)/(2*Vmax)*255)
4139      Y:000224 Y:000224                   DC      $202000+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #3, Vertical Clock 3
4140      Y:000225 Y:000225                   DC      $204000+@CVI((-9+Vmax)/(2*Vmax)*255)
4141      Y:000226 Y:000226                   DC      $208000+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #4, Frame Store 1
4142      Y:000227 Y:000227                   DC      $210000+@CVI((-9+Vmax)/(2*Vmax)*255)
4143      Y:000228 Y:000228                   DC      $220100+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #5, Frame Store 2
4144      Y:000229 Y:000229                   DC      $220200+@CVI((-9+Vmax)/(2*Vmax)*255)
4145      Y:00022A Y:00022A                   DC      $220400+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #6, Frame Store 3
4146      Y:00022B Y:00022B                   DC      $220800+@CVI((-9+Vmax)/(2*Vmax)*255)
4147      Y:00022C Y:00022C                   DC      $222000+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #7, Transfer Gate 2
4148      Y:00022D Y:00022D                   DC      $224000+@CVI((-9+Vmax)/(2*Vmax)*255)
4149      Y:00022E Y:00022E                   DC      $228000+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #8, Transger Gate 1
4150      Y:00022F Y:00022F                   DC      $230000+@CVI((-9+Vmax)/(2*Vmax)*255)
4151      Y:000230 Y:000230                   DC      $240100+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #9, Unused
4152      Y:000231 Y:000231                   DC      $240200+@CVI((-9+Vmax)/(2*Vmax)*255)
4153      Y:000232 Y:000232                   DC      $240400+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #10, Unused
4154      Y:000233 Y:000233                   DC      $240800+@CVI((-9+Vmax)/(2*Vmax)*255)
4155      Y:000234 Y:000234                   DC      $242000+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #11, Unused
4156      Y:000235 Y:000235                   DC      $244000+@CVI((-9+Vmax)/(2*Vmax)*255)
4157      Y:000236 Y:000236                   DC      $248000+@CVI((-9+Vmax)/(2*Vmax)*255) ; Pin #12, Unused
4158      Y:000237 Y:000237                   DC      $250000+@CVI((-9+Vmax)/(2*Vmax)*255)
4159   
4160      Y:000238 Y:000238                   DC      $2A0100+@CVI((-6+Vmax)/(2*Vmax)*255) ; Pin #34, Reset Gate Upper
4161      Y:000239 Y:000239                   DC      $2A0200+@CVI((-6+Vmax)/(2*Vmax)*255)
4162      Y:00023A Y:00023A                   DC      $2A0400+@CVI((-6+Vmax)/(2*Vmax)*255) ; Pin #35, Reset Gate Lower
4163      Y:00023B Y:00023B                   DC      $2A0800+@CVI((-6+Vmax)/(2*Vmax)*255)
4164   
4165                                EPUR_END
4166   
4167                                ; Code for ARC32 = universal clock driver board
4168      Y:00023C Y:00023C         DACS      DC      END_DACS-DACS-1
4169      Y:00023D Y:00023D                   DC      $2A0080                           ; DAC = unbuffered mode
4170   
4171      Y:00023E Y:00023E                   DC      $200100+@CVI((V1_HI+Vmax)/(2*Vmax)*255) ; Pin #1, Vertical Clock 1
4172      Y:00023F Y:00023F                   DC      $200200+@CVI((V1_LO+Vmax)/(2*Vmax)*255)
4173      Y:000240 Y:000240                   DC      $200400+@CVI((V2_HI+Vmax)/(2*Vmax)*255) ; Pin #2, Vertical Clock 2
4174      Y:000241 Y:000241                   DC      $200800+@CVI((V2_LO+Vmax)/(2*Vmax)*255)
4175      Y:000242 Y:000242                   DC      $202000+@CVI((V3_HI+Vmax)/(2*Vmax)*255) ; Pin #3, Vertical Clock 3
4176      Y:000243 Y:000243                   DC      $204000+@CVI((V3_LO+Vmax)/(2*Vmax)*255)
4177      Y:000244 Y:000244                   DC      $208000+@CVI((V1_HI+Vmax)/(2*Vmax)*255) ; Pin #4, Frame Store 1
4178      Y:000245 Y:000245                   DC      $210000+@CVI((V1_LO+Vmax)/(2*Vmax)*255)
4179      Y:000246 Y:000246                   DC      $220100+@CVI((V2_HI+Vmax)/(2*Vmax)*255) ; Pin #5, Frame Store 2
4180      Y:000247 Y:000247                   DC      $220200+@CVI((V2_LO+Vmax)/(2*Vmax)*255)
4181      Y:000248 Y:000248                   DC      $220400+@CVI((V3_HI+Vmax)/(2*Vmax)*255) ; Pin #6, Frame Store 3
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 74



4182      Y:000249 Y:000249                   DC      $220800+@CVI((V3_LO+Vmax)/(2*Vmax)*255)
4183      Y:00024A Y:00024A                   DC      $222000+@CVI((V1_HI+Vmax)/(2*Vmax)*255) ; Pin #7, Transfer Gate 2
4184      Y:00024B Y:00024B                   DC      $224000+@CVI((V1_LO+Vmax)/(2*Vmax)*255)
4185      Y:00024C Y:00024C                   DC      $228000+@CVI((V2_HI+Vmax)/(2*Vmax)*255) ; Pin #8, Transger Gate 1
4186      Y:00024D Y:00024D                   DC      $230000+@CVI((V2_LO+Vmax)/(2*Vmax)*255)
4187   
4188      Y:00024E Y:00024E                   DC      $240100+@CVI((V3_HI+Vmax)/(2*Vmax)*255) ; Pin #9, Unused
4189      Y:00024F Y:00024F                   DC      $240200+@CVI((V3_LO+Vmax)/(2*Vmax)*255)
4190      Y:000250 Y:000250                   DC      $240400+@CVI((FS1_HI+Vmax)/(2*Vmax)*255) ; Pin #10, Unused
4191      Y:000251 Y:000251                   DC      $240800+@CVI((FS1_LO+Vmax)/(2*Vmax)*255)
4192      Y:000252 Y:000252                   DC      $242000+@CVI((FS2_HI+Vmax)/(2*Vmax)*255) ; Pin #11, Unused
4193      Y:000253 Y:000253                   DC      $244000+@CVI((FS2_LO+Vmax)/(2*Vmax)*255)
4194      Y:000254 Y:000254                   DC      $248000+@CVI((FS3_HI+Vmax)/(2*Vmax)*255) ; Pin #12, Unused
4195      Y:000255 Y:000255                   DC      $250000+@CVI((FS3_LO+Vmax)/(2*Vmax)*255)
4196   
4197      Y:000256 Y:000256                   DC      $260100+@CVI((H1U2_L1_HI+Vmax)/(2*Vmax)*255) ; Pin #13, Horizontal 1 Upper
4198      Y:000257 Y:000257                   DC      $260200+@CVI((H1U2_L1_LO+Vmax)/(2*Vmax)*255)
4199      Y:000258 Y:000258                   DC      $260400+@CVI((H2U2_L1_HI+Vmax)/(2*Vmax)*255) ; Pin #14, Horizontal 2 Upper
4200      Y:000259 Y:000259                   DC      $260800+@CVI((H2U2_L1_LO+Vmax)/(2*Vmax)*255)
4201      Y:00025A Y:00025A                   DC      $262000+@CVI((H3U2_L1_HI+Vmax)/(2*Vmax)*255) ; Pin #15, Horizontal 3 Upper
4202      Y:00025B Y:00025B                   DC      $264000+@CVI((H3U2_L1_LO+Vmax)/(2*Vmax)*255)
4203      Y:00025C Y:00025C                   DC      $268000+@CVI((H1U1_L2_HI+Vmax)/(2*Vmax)*255) ; Pin #16, Horizontal 1 Lower
4204      Y:00025D Y:00025D                   DC      $270000+@CVI((H1U1_L2_LO+Vmax)/(2*Vmax)*255)
4205      Y:00025E Y:00025E                   DC      $280100+@CVI((H2U1_L2_HI+Vmax)/(2*Vmax)*255) ; Pin #17, Horizontal 2 Lower
4206      Y:00025F Y:00025F                   DC      $280200+@CVI((H2U1_L2_LO+Vmax)/(2*Vmax)*255)
4207      Y:000260 Y:000260                   DC      $280400+@CVI((H3U1_L2_HI+Vmax)/(2*Vmax)*255) ; Pin #18, Horizontal 3 Lower
4208      Y:000261 Y:000261                   DC      $280800+@CVI((H3U1_L2_LO+Vmax)/(2*Vmax)*255)
4209      Y:000262 Y:000262                   DC      $282000+@CVI((SWL_HI+Vmax)/(2*Vmax)*255) ; Pin #19, Summing Well Upper
4210      Y:000263 Y:000263                   DC      $284000+@CVI((SWL_LO+Vmax)/(2*Vmax)*255)
4211      Y:000264 Y:000264                   DC      $288000+@CVI((SWU_HI+Vmax)/(2*Vmax)*255) ; Pin #33, Summing Well Lower
4212      Y:000265 Y:000265                   DC      $290000+@CVI((SWU_LO+Vmax)/(2*Vmax)*255)
4213      Y:000266 Y:000266                   DC      $2A0100+@CVI((RL_HI+Vmax)/(2*Vmax)*255) ; Pin #34, Reset Gate Upper
4214      Y:000267 Y:000267                   DC      $2A0200+@CVI((RL_LO+Vmax)/(2*Vmax)*255)
4215      Y:000268 Y:000268                   DC      $2A0400+@CVI((RU_HI+Vmax)/(2*Vmax)*255) ; Pin #35, Reset Gate Lower
4216      Y:000269 Y:000269                   DC      $2A0800+@CVI((RU_LO+Vmax)/(2*Vmax)*255)
4217      Y:00026A Y:00026A                   DC      $2A2000+@CVI((T1_HI+Vmax)/(2*Vmax)*255) ; Pin #36, Unused
4218      Y:00026B Y:00026B                   DC      $2A4000+@CVI((T1_LO+Vmax)/(2*Vmax)*255)
4219      Y:00026C Y:00026C                   DC      $2A8000+@CVI((T2_HI+Vmax)/(2*Vmax)*255) ; Pin #37, Unused
4220      Y:00026D Y:00026D                   DC      $2B0000+@CVI((T2_LO+Vmax)/(2*Vmax)*255)
4221   
4222   
4223                                ; DC bias voltages for the LBL CCD chip
4224                                          VOLTS   VSUB,25.0                         ; Vsub  0.0 140 V
**** 4229 [SWIFT/engg/engg_48khz.waveforms.reverse 1159]: Setting voltage VSUB 25.0V 1280 3081472
4230                                          VOLTS   RAMP,5.0                          ; Vsub  AVG RAMP RATE
**** 4235 [SWIFT/engg/engg_48khz.waveforms.reverse 1160]: Setting voltage RAMP 5.0V 2048 3098624
4236                                          VOLTS   VDDL2,0.0                         ; Vdd  -5.1 -25V
**** 4241 [SWIFT/engg/engg_48khz.waveforms.reverse 1161]: Setting voltage VDDL2 0.0V 0 2883584
4242                                          VOLTS   VDDU2,0.0                         ; Vdd  -5.1 -25V
**** 4247 [SWIFT/engg/engg_48khz.waveforms.reverse 1162]: Setting voltage VDDU2 0.0V 0 2899968
4248                                          VOLTS   VDDL1,-22.0                       ; Vdd  -5.1 -25V
**** 4253 [SWIFT/engg/engg_48khz.waveforms.reverse 1163]: Setting voltage VDDL1 -22.0V 3604 2919956
4254                                          VOLTS   VDDU1,-22.0                       ; Vdd  -5.1 -25V
**** 4259 [SWIFT/engg/engg_48khz.waveforms.reverse 1164]: Setting voltage VDDU1 -22.0V 3604 2936340
4260                                          VOLTS   VRL2,-12.5                        ; Vr   -5.1 -25V
**** 4265 [SWIFT/engg/engg_48khz.waveforms.reverse 1165]: Setting voltage VRL2 -12.5V 2048 2951168
4266                                          VOLTS   VRU2,-12.5                        ; Vr   -5.1 -25V
**** 4271 [SWIFT/engg/engg_48khz.waveforms.reverse 1166]: Setting voltage VRU2 -12.5V 2048 2967552
4272                                          VOLTS   VRL1,-12.5                        ; Vr   -5.1 -25V
**** 4277 [SWIFT/engg/engg_48khz.waveforms.reverse 1167]: Setting voltage VRL1 -12.5V 2048 2983936
4278                                          VOLTS   VRU1,-12.5                        ; Vr   -5.1 -25V
**** 4283 [SWIFT/engg/engg_48khz.waveforms.reverse 1168]: Setting voltage VRU1 -12.5V 2048 3000320
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  SWIFT/engg/engg_48khz.waveforms.reverse  Page 75



4284                                          VOLTS   VOGL2,2.50                        ; Vopg  -10  10 V
**** 4289 [SWIFT/engg/engg_48khz.waveforms.reverse 1169]: Setting voltage VOGL2 2.50V 2048 3016704
4290                                          VOLTS   VOGU2,2.50                        ; Vopg  -10  10 V
**** 4295 [SWIFT/engg/engg_48khz.waveforms.reverse 1170]: Setting voltage VOGU2 2.50V 2048 3033088
4296                                          VOLTS   VOGL1,2.50                        ; Vopg  -10  10 V
**** 4301 [SWIFT/engg/engg_48khz.waveforms.reverse 1171]: Setting voltage VOGL1 2.50V 2048 3049472
4302                                          VOLTS   VOGU1,2.50                        ; Vopg  -10  10 V
**** 4307 [SWIFT/engg/engg_48khz.waveforms.reverse 1172]: Setting voltage VOGU1 2.50V 2048 3065856
4308   
4309   
4310                                 GAIN_SETTING
4311      Y:00027C Y:00027C                   DC      VID0+$0D000E                      ; Gain of 0.25 (from 0 to $F,one of 16 possi
ble values)
4312   
4313                                ;Initialize the ARC-47 DAC for video offsets, board #0
4314      Y:00027D Y:00027D                   DC      VID0+DAC_ADDR+$000014
4315      Y:00027E Y:00027E                   DC      VID0+DAC_RegD+OFFSET0
4316      Y:00027F Y:00027F                   DC      VID0+DAC_ADDR+$000015
4317      Y:000280 Y:000280                   DC      VID0+DAC_RegD+OFFSET1
4318      Y:000281 Y:000281                   DC      VID0+DAC_ADDR+$000016
4319      Y:000282 Y:000282                   DC      VID0+DAC_RegD+OFFSET2
4320      Y:000283 Y:000283                   DC      VID0+DAC_ADDR+$000017
4321      Y:000284 Y:000284                   DC      VID0+DAC_RegD+OFFSET3
4322   
4323   
4324                                ;  DC   VID0+DAC_ADDR+$00000C           ; Vabg,pin 5
4325                                ;  DC   VID0+DAC_RegD+DAC_VRSV
4326                                ;  DC   VID0+DAC_ADDR+$00000D           ; Vrsv1,pin 47
4327                                ;  DC   VID0+DAC_RegD+DAC_VRSV
4328                                ;  DC   VID0+DAC_ADDR+$00000E           ; Vrsv2,pin 27
4329                                ;  DC   VID0+DAC_RegD+DAC_VRSV
4330                                ;  DC   VID0+DAC_ADDR+$00000F           ; Vrsv3,pin 6
4331                                ;  DC   VID0+DAC_RegD+DAC_VRSV
4332   
4333   
4334                                END_DACS
4335   
4336   
4337                                ; Pixel table generated in "timCCD.asm"
4338      Y:000285 Y:000285         PXL_TBL   DC      0
4339   
4340   
4341   
4342      Y:0002B8 Y:0002B8                   ORG     Y:@LCV(L)+50,Y:@LCV(L)+50
4343   
4344                                 TMP_PXL_TBL1
4345      Y:0002B8 Y:0002B8                   DC      0
4346   
4347      Y:0002EB Y:0002EB                   ORG     Y:@LCV(L)+50,Y:@LCV(L)+50
4348   
4349                                 TMP_PXL_TBL2
4350      Y:0002EB Y:0002EB                   DC      0
4351   
4352      Y:00031E Y:00031E                   ORG     Y:@LCV(L)+50,Y:@LCV(L)+50
4353   
4354                                 TMP_PXL_TBL3
4355      Y:00031E Y:00031E                   DC      0
4356   
4357                                 END_APPLICATON_Y_MEMORY
4358      00031F                              EQU     @LCV(L)
4359   
4360   
Motorola DSP56300 Assembler  Version 6.3.4   25-09-25  13:37:40  tim.asm  Page 76



4361                                ; End of program
4362                                          END

0    Errors
0    Warnings


