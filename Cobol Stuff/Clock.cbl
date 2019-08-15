      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLOCK.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-TIME PIC 9(6).
       01 WS-DATE PIC 9(8).
       PROCEDURE DIVISION.
      *2018 06 14 09:46:31:79
       ACCEPT WS-TIME FROM TIME.
       ACCEPT WS-DATE FROM DATE.
       DISPLAY "The time is " WS-TIME.
       DISPLAY "The date is " WS-DATE.
       DISPLAY "The current time is " FUNCTION CURRENT-DATE.
       GOBACK.

       END PROGRAM CLOCK.
