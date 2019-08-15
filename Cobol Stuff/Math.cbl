      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATH.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 CIRCLE-RADIUS PIC 9(5).
       01 CIRCLE-AREA PIC 9(5).
       01 SQUARE PIC 9(5).
       01 IS-EVEN PIC 9(5).
       01 MOD2 PIC 9(1).

       PROCEDURE DIVISION.
           DISPLAY "Input a circle radius."
           ACCEPT CIRCLE-RADIUS.

           COMPUTE CIRCLE-AREA = 3.141 * CIRCLE-RADIUS * CIRCLE-RADIUS.
           DISPLAY "Circle Area " CIRCLE-AREA

           DISPLAY "Input number to square root."
           ACCEPT SQUARE
           DISPLAY "Sqrt(" SQUARE ") = " FUNCTION SQRT(SQUARE).

           DISPLAY "Is this number even?"
           ACCEPT IS-EVEN
           COMPUTE MOD2 = FUNCTION MOD(IS-EVEN, 2).
           IF MOD2 = 0
               DISPLAY "Even Number."
           ELSE
               DISPLAY "Odd Number."
           END-IF

           STOP RUN.
       END PROGRAM MATH.
