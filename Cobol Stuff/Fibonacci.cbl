      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 FIB-TERM PIC 9(38) VALUE 0.
       01 FIB-LAST PIC 9(38) VALUE 0.
       01 FIB-CURRENT PIC 9(38) VALUE 1.
       01 FIB-TEMP PIC 9(38).
       PROCEDURE DIVISION.
       FIB.
           DISPLAY " " FIB-CURRENT
           SET FIB-TEMP TO FIB-LAST

           SET FIB-LAST TO FIB-CURRENT

           COMPUTE FIB-CURRENT = FIB-CURRENT + FIB-TEMP
           ADD 1 To FIB-TERM
       PERFORM FIB UNTIL FIB-TERM=50
       STOP RUN.
       END PROGRAM FIBONACCI.
