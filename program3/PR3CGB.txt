       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PR3CGB.
       AUTHOR.  GARRETT BURNS.
       
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *
      * INPUT:
      *    INPUT RECORD LAYOUT CONTAINS THE FOLLOWING DATA FOR EACH RECORD:
      *        1. WAREHOUSE ID
      *        2. VENDOR ID
      *        3. CANDY ID
      *        4. CANDY DATA (MAX OF 5)
      *
      * RESTRICTIONS :
      *    THE FOLLOWING RESTRICTIONS MATCH WITH THE LAYOUT FROM INPUT:
      *        1. N/A
      *        2.
      *            A - ATOMIC SWEETS
      *            B - BOOZIE SWEETS
      *            N - NELLIES SWEET SHOP
      *            T - TIGERTREATS
      *        3. C01 - C10
      *        4. CANDY NAME                 X(15)
      *           CANDY BOX SIZE             A(L - LARGE, M - MEDIUM, S - SMALL,
      *                                        G - GIFT, X - SAMPLE)
      *           CANDY TYPE                 A(SF - SUGAR FREE, SU - SUGAR)
      *           NUMBER OF CASES IN STOCK   S9(4)
      *           PURCHASE PRICE             S999V99
      *
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *
      * OUTPUT:
      *    THE OUTPUT REPORT CONTAINS THE FOLLOWING INFORMATION:
      * * * * * *
      *    PER WAREHOUSE:
      *        CONTAINS INDIVIDUAL VENDOR INFORMATION FOR CANDY DATA
      *    PER VENDOR:
      *        CONTAINS INDIVIDUAL CANDY INFORMATION
      *    PER CANDY:
      *        CONTAINS ALL INFORMATION FOR EACH INDIVIDUAL CANDY PER PROGRAM REQUEST::
      *            CANDY NAME
      *            CANDY SIZE
      *            CANDY TYPE
      *            NUMBER OF CANDY IN STOCK
      *            TOTAL COST
      *
      * CALCULATIONS:
      *    MULTIPLY NUMBER OF CANDY IN STOCK BY TOTALCOST PER BOX, FOR TOTAL COST OF EACH INDIVIDUAL CANDY
      *    DISPLAY TOTAL FOR ALL CANDIES PER GIVEN CANDY ID
      *    DISPLAY TOTAL FOR ALL VENDORS PER GIVEN VENDOR ID
      *    DISPLAY TOTAL FOR ALL WAREHOUSES PER GIVEN WAREHOUSE ID
      *    GATHER GRAND TOTAL FOR ALL CANDIES CALCULATED
      *
      * * * * * * * * * * * * * * * * * * * * * * * * *

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   LAPTOP-U5VKK9JE.
       OBJECT-COMPUTER.   LAPTOP-U5VKK9JE.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-RECORDS
               ASSIGN TO 'PR3FA19.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-REPORT
               ASSIGN TO PRINTER 'PR3-OUT'.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD  INPUT-RECORDS
           RECORD CONTAINS 143 CHARACTERS.
       
       01  INFORMATION.
           05  I-WAREHOUSE-ID           PIC X(4).
           05  I-VENDOR-ID              PIC X(1).
           05  I-CANDY-ID               PIC X(3).
           
           05  I-CANDY-ARRAY OCCURS 5 TIMES.
               10  I-CANDY-NAME         PIC X(15).
               10  I-CANDY-BOX-SIZE     PIC A(1).
               10  I-CANDY-TYPE         PIC A(2).
               10  I-NUM-OF-CASES       PIC 9(4).
               10  I-PURCHASE-PRICE     PIC 999V99.
      
       FD  OUTPUT-REPORT
           RECORD CONTAINS 65 CHARACTERS.
           
       01  RECORD-REPORT                PIC X(65).
       
       
       WORKING-STORAGE SECTION.
       
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                 PIC X          VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.
               88 MORE-RECORDS                         VALUE 'Y'.
           05  FIRST-RECORD             PIC X(3)       VALUE 'YES'.
                  
       01  HOLDING-FIELDS.
           05  HF-WAREHOUSE-HOLD        PIC X(4).
           05  HF-VENDOR-HOLD           PIC X(1).
           05  HF-VENDOR-HOLD-2         PIC X(18).
           05  HF-CANDY-HOLD            PIC X(3).
           05  HF-PURCHASE-PRICE-HOLD   PIC S9(7)V9(2).
           05  HF-BOX-SIZE-HOLD         PIC X(6).
           05  HF-NUM-OF-CASES-HOLD     PIC S999.
           
       01  STRING-HOLDING.
           05  SH-WAREHOUSE-HOLD        PIC X(4).
           05  SH-VENDOR-HOLD           PIC X(1).
           05  SH-CANDY-HOLD            PIC X(3).
       
       01  SPACE-TEMP                   PIC X.
           
       01  REPORT-FIELDS.
           05  PROPER-SPACING           PIC S9         VALUE +1.
           05  PAGE-NO                  PIC S9(2)      VALUE +0.
           05  VENDOR-NAME              PIC X(18).
           05  CANDY-BOX-SIZE           PIC X(6).
           05  SUB                      PIC 9          VALUE 1.
           05  CANDY-ERR                PIC X(6)       VALUE 'BAD - '.
           05  VENDOR-ERR               PIC X(10)    VALUE 'INVALID - '.
           
       01  TOTAL-FIELDS.
           05  TF-CANDY-TOTAL           PIC S9(7)V9(2).
           05  TF-VENDOR-TOTAL          PIC S9(7)V9(2).
           05  TF-WAREHOUSE-TOTAL       PIC S9(8)V9(2).
           05  TF-GRAND-TOTAL           PIC S9(8)V9(2).
      
      * * * * * * * OUTPUT AREA * * * * * * *
       
       01  HEADING-ONE.
           05                    PIC X(31).
           05                    PIC X(18)   VALUE 'GLENCOE CANDY, LTD'.
           05                    PIC X(16).
           
       01  HEADING-TWO.
           05                    PIC X(6).
           05  H1-DATE           PIC 9999/99/99.

           05                    PIC X(15).
           05                    PIC X(16)   VALUE 'INVENTORY REPORT'.
           05                    PIC X(9).
           05                    PIC X(6)    VALUE 'PAGE: '.
           05  H1-PAGE-NO        PIC Z9.
       
       01  WAREHOUSE-LINE.
           05                    PIC X(2).
           05                    PIC X(11)  VALUE 'WAREHOUSE: '.
           05  WL-WAREHOUSE-ID   PIC X(4).
           05                    PIC X(48).
           
       01  VENDOR-LINE.
           05                    PIC X(5).
           05                    PIC X(8)  VALUE 'VENDOR: '.
           05  VL-VENDOR-NAME    PIC X(18).
           05                    PIC X(34).
           
       01  CANDY-LINE.
           05                    PIC X(6).
           05                    PIC X(7)  VALUE 'CANDY: '.
           05  CL-CANDY-ID       PIC X(3).
           05                    PIC X(49).
       
       01  HEADING-THREE.
           05                    PIC X(10).
           05                    PIC X(4)    VALUE 'NAME'.
           05                    PIC X(10).
           05                    PIC X(4)    VALUE 'SIZE'.
           05                    PIC X(8).
           05                    PIC X(4)    VALUE 'TYPE'.
           05                    PIC X(3).
           05                    PIC X(8)    VALUE 'IN STOCK'.
           05                    PIC X(4).
           05                    PIC X(10)   VALUE 'TOTAL COST'.
           
       01  DETAIL-LINE.
           05                    PIC X(5).
           05  DL-CANDY-NAME     PIC X(13).
           05                    PIC X(4).
           05  DL-CANDY-BOX-SIZE PIC X(10).
           05                    PIC X(5).
           05  DL-CANDY-TYPE     PIC X(2).
           05                    PIC X(6).
           05  DL-NUM-OF-CASES   PIC Z999.
           05                    PIC X(6).
           05  DL-PURCHASE-PRICE PIC $ZZ,ZZ9.99.
           
       01  CANDY-GROUP-LINE.
           05                    PIC X(19).
           05                    PIC X(13)  VALUE 'TOTAL CANDY: '.
           05  CGL-CANDY-NAME    PIC X(13).
           05                    PIC X(8).
           05  CGL-TOTAL-COST    PIC $BZZZ,999.99.
           
       01  VENDOR-GROUP-LINE.
           05                    PIC X(14).
           05                    PIC X(18)  VALUE 'TOTAL FOR VENDOR: '.
           05  VGL-VENDOR-NAME   PIC X(18).
           05                    PIC X(3).
           05  VGL-TOTAL-COST    PIC $BZZZ,999.99.
       
       01  WAREHOUSE-GROUP-LINE.
           05                    PIC X(11).
           05                   PIC X(21) VALUE 'TOTAL FOR WAREHOUSE: '.
           05  WGL-WAREHOUSE-ID  PIC X(4).
           05                    PIC X(16).
           05  WGL-TOTAL-COST    PIC $Z,ZZZ,999.99.
           
       01  GRAND-TOTAL.
           05                    PIC X(19).
           05                    PIC X(12)  VALUE 'GRAND TOTAL:'.
           05                    PIC X(20).
           05  GT-TOTAL-COST     PIC $ZZ,ZZZ,999.99.
           
           
       PROCEDURE DIVISION.
       
       10-CONTROL-MODULE.
       
           PERFORM 20-HOUSEKEEPING-ROUTINE
           PERFORM 30-READ-FILE
           PERFORM 700-EOJ-ROUTINE
           PERFORM 800-PRINT-GRAND-TOTALS
           PERFORM 900-EOF-ROUTINE
       .
           
       20-HOUSEKEEPING-ROUTINE.
       
           OPEN INPUT INPUT-RECORDS
               OUTPUT OUTPUT-REPORT
           ACCEPT H1-DATE FROM DATE YYYYMMDD
           
           PERFORM 40-HEADER-ROUTINE
       .
       
       30-READ-FILE.
       
           PERFORM UNTIL NO-MORE-DATA
               READ INPUT-RECORDS
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 100-PROCESS-DATA
               END-READ
           END-PERFORM
       .
           
       40-HEADER-ROUTINE.
           
           ADD 1 TO PAGE-NO
           MOVE PAGE-NO TO H1-PAGE-NO
           
           WRITE RECORD-REPORT FROM HEADING-ONE
               AFTER ADVANCING PROPER-SPACING
               
           MOVE HEADING-TWO TO RECORD-REPORT
           WRITE RECORD-REPORT FROM HEADING-TWO
               AFTER ADVANCING 1 LINE
           MOVE 1 TO PROPER-SPACING
       .
           
       45-PRINT-WAREHOUSE-HEADER.
           MOVE I-WAREHOUSE-ID TO WL-WAREHOUSE-ID
           WRITE RECORD-REPORT FROM WAREHOUSE-LINE
               AFTER ADVANCING 2 LINES
       .
       
       50-PRINT-VENDOR-HEADER.
           STRING
               I-VENDOR-ID DELIMITED BY ' '
               INTO SH-VENDOR-HOLD
               END-STRING
           
           EVALUATE SH-VENDOR-HOLD
               WHEN 'A'
                   MOVE 'ATOMIC SWEETS' TO VL-VENDOR-NAME, 
                                           VGL-VENDOR-NAME
               WHEN 'B'
                   MOVE 'BOOZIE SWEETS' TO VL-VENDOR-NAME,
                                           VGL-VENDOR-NAME
               WHEN 'N'
                   MOVE 'NELLIES SWEET SHOP' TO VL-VENDOR-NAME,
                                                VGL-VENDOR-NAME
               WHEN 'T'
                   MOVE 'TIGER TREATS' TO VL-VENDOR-NAME,
                                          VGL-VENDOR-NAME
               WHEN OTHER
                   STRING
                       VENDOR-ERR DELIMITED BY ' '
                           ' - ' DELIMITED BY SIZE
                       VGL-VENDOR-NAME DELIMITED BY ' '
                       INTO HF-VENDOR-HOLD-2
                   END-STRING
                   MOVE HF-VENDOR-HOLD-2 TO VL-VENDOR-NAME,
                                            VGL-VENDOR-NAME
           END-EVALUATE
           
           
           WRITE RECORD-REPORT FROM VENDOR-LINE
               AFTER ADVANCING 2 LINES
       .
       
       55-PRINT-CANDY-HEADER.
           STRING
               I-CANDY-ID DELIMITED BY ' '
               INTO SH-CANDY-HOLD
               END-STRING
               
           MOVE I-CANDY-ID TO CL-CANDY-ID
           WRITE RECORD-REPORT FROM CANDY-LINE
               AFTER ADVANCING 2 LINES
               
           WRITE RECORD-REPORT FROM HEADING-THREE
               AFTER ADVANCING 2 LINES
       .
       
       100-PROCESS-DATA.
           EVALUATE TRUE
               WHEN FIRST-RECORD = 'YES'
                   MOVE 'NO' TO FIRST-RECORD
                   MOVE I-WAREHOUSE-ID TO HF-WAREHOUSE-HOLD
                   MOVE I-VENDOR-ID TO HF-VENDOR-HOLD
                   MOVE I-CANDY-ID TO HF-CANDY-HOLD
                   
                   PERFORM 45-PRINT-WAREHOUSE-HEADER
                   PERFORM 50-PRINT-VENDOR-HEADER
                   PERFORM 55-PRINT-CANDY-HEADER
               
               WHEN I-WAREHOUSE-ID NOT = HF-WAREHOUSE-HOLD
                   PERFORM 300-WAREHOUSE-BREAK
                   PERFORM 40-HEADER-ROUTINE
                   PERFORM 45-PRINT-WAREHOUSE-HEADER
                   PERFORM 50-PRINT-VENDOR-HEADER
                   PERFORM 55-PRINT-CANDY-HEADER
               WHEN I-VENDOR-ID NOT = HF-VENDOR-HOLD
                   PERFORM 400-VENDOR-BREAK
                   PERFORM 50-PRINT-VENDOR-HEADER
                   PERFORM 55-PRINT-CANDY-HEADER
               WHEN I-CANDY-ID NOT = HF-CANDY-HOLD
                   PERFORM 500-CANDY-BREAK
                   PERFORM 55-PRINT-CANDY-HEADER
           END-EVALUATE
           
                      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               EVALUATE TRUE
                   WHEN SUB = 1
                       MOVE I-CANDY-NAME(SUB) TO DL-CANDY-NAME,
                                                 CGL-CANDY-NAME
                   WHEN OTHER
                       MOVE SPACES TO DL-CANDY-NAME
               END-EVALUATE
               MOVE I-CANDY-BOX-SIZE(SUB) TO DL-CANDY-BOX-SIZE
               MOVE I-CANDY-TYPE(SUB) TO DL-CANDY-TYPE
               MOVE I-NUM-OF-CASES(SUB) TO DL-NUM-OF-CASES
               
               
               EVALUATE DL-CANDY-BOX-SIZE
                   WHEN 'L'
                       MOVE 'LARGE' TO DL-CANDY-BOX-SIZE
                   WHEN 'M'
                       MOVE 'MEDIUM' TO DL-CANDY-BOX-SIZE
                   WHEN 'S'
                       MOVE 'SMALL' TO DL-CANDY-BOX-SIZE
                   WHEN 'G'
                       MOVE 'GIFT' TO DL-CANDY-BOX-SIZE
                   WHEN 'X'
                       MOVE 'SAMPLE' TO DL-CANDY-BOX-SIZE
                   WHEN SPACES
                       MOVE SPACES TO DL-CANDY-BOX-SIZE, DL-CANDY-NAME
                       MOVE ZEROS TO I-PURCHASE-PRICE(SUB),
                                     HF-PURCHASE-PRICE-HOLD
                   WHEN OTHER
      *                CONCATENATION OF CANDY BOX SIZEERROR
                       STRING
                           CANDY-ERR DELIMITED BY ' '
                               '- ' DELIMITED BY SIZE
                           DL-CANDY-BOX-SIZE DELIMITED BY ' '
                           INTO HF-BOX-SIZE-HOLD
                       END-STRING
                       MOVE HF-BOX-SIZE-HOLD TO DL-CANDY-BOX-SIZE
               END-EVALUATE
               
               
               STRING
                   DL-NUM-OF-CASES DELIMITED BY ' '
                   INTO HF-NUM-OF-CASES-HOLD
               END-STRING
               
               EVALUATE HF-NUM-OF-CASES-HOLD
                   WHEN NOT NUMERIC
                       MOVE ZEROS TO DL-NUM-OF-CASES,
                                     I-PURCHASE-PRICE(SUB)
                   WHEN NUMERIC
                       CONTINUE
               END-EVALUATE
               
               MOVE ZEROS TO HF-NUM-OF-CASES-HOLD
               
               
      *        CHECK FOR NON-NUMERIC IN PURCHASE PRICE
               IF I-PURCHASE-PRICE(SUB) NOT NUMERIC
                   MOVE ZEROS TO I-PURCHASE-PRICE(SUB)
               END-IF
               
               
      *        GETS THE TOTAL COST FOR EACH CANDY AND PLACES IT INTO A HOLDING CELL
               COMPUTE HF-PURCHASE-PRICE-HOLD = I-NUM-OF-CASES(SUB) * 
                                                I-PURCHASE-PRICE(SUB)
      *        TOTAL CANDY: $
               COMPUTE TF-CANDY-TOTAL = HF-PURCHASE-PRICE-HOLD
                                        + TF-CANDY-TOTAL
      *        TOTAL FOR VENDOR: $
               COMPUTE TF-VENDOR-TOTAL = HF-PURCHASE-PRICE-HOLD +
                                         TF-VENDOR-TOTAL
      *        TOTAL FOR WAREHOUSE: $
               COMPUTE TF-WAREHOUSE-TOTAL = HF-PURCHASE-PRICE-HOLD +
                                            TF-WAREHOUSE-TOTAL
      *        GRAND TOTAL: $
               COMPUTE TF-GRAND-TOTAL = HF-PURCHASE-PRICE-HOLD
                                        + TF-GRAND-TOTAL
               
               MOVE HF-PURCHASE-PRICE-HOLD TO DL-PURCHASE-PRICE

      
           WRITE RECORD-REPORT FROM DETAIL-LINE AFTER
               ADVANCING PROPER-SPACING
       .
       
       200-WRITE-A-LINE.
           WRITE RECORD-REPORT
               AFTER ADVANCING PROPER-SPACING
       .
       
       300-WAREHOUSE-BREAK.
           PERFORM 400-VENDOR-BREAK
           
           MOVE HF-WAREHOUSE-HOLD TO WGL-WAREHOUSE-ID
           MOVE TF-WAREHOUSE-TOTAL TO WGL-TOTAL-COST
           MOVE WAREHOUSE-GROUP-LINE TO RECORD-REPORT
           MOVE 2 TO PROPER-SPACING
           PERFORM 200-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING
           MOVE ZEROS TO WGL-TOTAL-COST
           MOVE ZEROS TO TF-WAREHOUSE-TOTAL
           
           MOVE ZEROS TO WL-WAREHOUSE-ID
           MOVE I-WAREHOUSE-ID TO HF-WAREHOUSE-HOLD
           MOVE 2 TO PROPER-SPACING
       .
       
       400-VENDOR-BREAK.
           PERFORM 500-CANDY-BREAK
           
           MOVE TF-VENDOR-TOTAL TO VGL-TOTAL-COST
           MOVE VENDOR-GROUP-LINE TO RECORD-REPORT
           MOVE 2 TO PROPER-SPACING
           PERFORM 200-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING
           MOVE ZEROS TO VGL-TOTAL-COST
           MOVE ZEROS TO TF-VENDOR-TOTAL
           
           MOVE ZEROS TO VL-VENDOR-NAME
           MOVE I-VENDOR-ID TO HF-VENDOR-HOLD
       .
       
       500-CANDY-BREAK.
           
           
           MOVE TF-CANDY-TOTAL TO CGL-TOTAL-COST
           MOVE CANDY-GROUP-LINE TO RECORD-REPORT
           MOVE 2 TO PROPER-SPACING
           PERFORM 200-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING
           MOVE ZEROS TO CGL-TOTAL-COST
           MOVE ZEROS TO TF-CANDY-TOTAL
           
           MOVE I-CANDY-ID TO HF-CANDY-HOLD
       .
       
       700-EOJ-ROUTINE.
       
           PERFORM 300-WAREHOUSE-BREAK
       .
       
       800-PRINT-GRAND-TOTALS.
           MOVE TF-GRAND-TOTAL TO GT-TOTAL-COST
           MOVE GRAND-TOTAL TO RECORD-REPORT
           MOVE 3 TO PROPER-SPACING
           PERFORM 200-WRITE-A-LINE
       .
           
       900-EOF-ROUTINE.    
       
           CLOSE INPUT-RECORDS
               OUTPUT-REPORT
           STOP RUN
       .
