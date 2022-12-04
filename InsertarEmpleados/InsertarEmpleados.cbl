       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERTAR-EMPLEADOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT EMPLOYEEFILE ASSIGN TO
            "C:\Users\dburg\Downloads\05_01\EMPLEADO-ARCH.DAT"
           FILE STATUS IS FS-EMPLEADO-ARCH
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT TRANSFILE ASSIGN TO
            "C:\Users\dburg\Downloads\05_01\EMPTRANS.DAT"
           FILE STATUS IS FS-EMPTRANS
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT NEWEMPFILE ASSIGN TO
            "C:\Users\dburg\Downloads\05_01\EMP-ARCH-NUEVO.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT OUTPUTFILE ASSIGN TO
            "C:\Users\dburg\Downloads\05_01\ERRORREPORT.DAT"
            ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEEFILE.
       01 EMPDETAILS.
            88 EMP-EOF VALUE HIGH-VALUES.
            02 EMP-ID          PIC 9(7).
            02 EMP-NOMBRE      PIC X(20).
            02 FILLER          PIC X(48).

       FD TRANSFILE.
       01 EMPTRANS.
            88 TRANS-EOF VALUE HIGH-VALUES.
            02 NEWEMPID       PIC 9(7).
            02 FILLER        PIC X(68).

       FD NEWEMPFILE.
       01 NEWEMPRECORD       PIC X(75).

       FD OUTPUTFILE.
       01 PRINTLINE.
          02 FILLER          PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-CONTADORES.
           05 WS-CONT-ACT     PIC 99 VALUE ZERO.
           05 WS-CONT-ERR    PIC 99 VALUE ZERO.

       01  WS-WORKING-STORAGE.
           05 FILLER      PIC X(27) VALUE
              'WORKING STORAGE STARTS HERE'.

       01  WS-DATE.
           05  WS-YEAR            PIC 99.
           05  WS-MONTH           PIC 99.
           05  WS-DAY             PIC 99.

       01  WS-WORK-AREAS.
           05  WS-SALARIO-TOTAL    PIC 9(12) VALUE ZERO.
           05  FS-EMPLEADO-ARCH    PIC X(02).
           05  FS-EMPTRANS         PIC X(02).

       01  HEADING-LINE.
           05 H-DIA              PIC 99     VALUE ZERO.
           05 FILLER             PIC X      VALUE '/'.
           05 H-MES              PIC 99     VALUE ZERO.
           05 FILLER             PIC X      VALUE '/'.
           05 FILLER             PIC 99     VALUE 20.
           05 H-ANIO             PIC 99     VALUE ZERO.
           05 FILLER             PIC X(43)  VALUE
               '  REPORTE DE ERROR POR EMPLEADO DUPLICADO: '.
           05 FILLER            PIC X(79)  VALUE SPACES.


       01  DETAIL-LINE.
           05 FILLER           PIC X(04) VALUE 'ID: '.
           05 DET-EMP-ID       PIC 9(07) VALUE ZERO.
           05 FILLER           PIC X(02) VALUE SPACES.
           05 FILLER           PIC X(08) VALUE 'NOMBRE: '.
           05 DET-EMP-NOMBRE   PIC X(20) VALUE SPACES.
           05 FILLER           PIC X(91) VALUE SPACES.


       PROCEDURE DIVISION.
       MAIN.
           PERFORM 100-INICIO  THRU 100-F.
           PERFORM 200-PROCESO THRU 200-F UNTIL
              (EMP-EOF) AND (TRANS-EOF).
           PERFORM 300-FIN     THRU 300-F.





           100-INICIO.
             ACCEPT WS-DATE FROM DATE

             MOVE WS-DAY   TO H-DIA
             MOVE WS-MONTH TO H-MES
             MOVE WS-YEAR  TO H-ANIO

             PERFORM 150-OPEN-FILES THRU 150-END
             PERFORM 170-READ-EMPLOYEES THRU 170-END.

           100-F. EXIT.

       150-OPEN-FILES.

           OPEN INPUT EMPLOYEEFILE
           IF FS-EMPLEADO-ARCH NOT = "00"
              DISPLAY "ERROR AL ABRIR EL ARCHIVO. FILE STATUS #",
                 FS-EMPLEADO-ARCH
              GO TO 300-FIN
           END-IF.
           OPEN INPUT TRANSFILE
           IF FS-EMPTRANS NOT = "00"
              DISPLAY "ERROR AL ABRIR EL ARCHIVO. FILE STATUS #",
                 FS-EMPTRANS
           END-IF.
           OPEN OUTPUT OUTPUTFILE.
           WRITE PRINTLINE FROM HEADING-LINE

           OPEN OUTPUT NEWEMPFILE.

       150-END.

       170-READ-EMPLOYEES.

           READ EMPLOYEEFILE
            AT END SET EMP-EOF TO TRUE
            END-READ.

           READ TRANSFILE
            AT END SET TRANS-EOF TO TRUE
            END-READ.

       170-END.

       200-PROCESO.
            EVALUATE TRUE
              WHEN(EMP-ID<NEWEMPID)
                ADD 1 TO WS-CONT-ACT
                WRITE NEWEMPRECORD FROM EMPDETAILS
                  READ EMPLOYEEFILE
                    AT END SET EMP-EOF TO TRUE
                  END-READ

              WHEN (EMP-ID>NEWEMPID)
                ADD 1 TO WS-CONT-ACT
                WRITE NEWEMPRECORD FROM EMPTRANS
                  READ TRANSFILE
                    AT END SET TRANS-EOF TO TRUE
                  END-READ

              WHEN (EMP-ID = NEWEMPID)
                 ADD 1 TO WS-CONT-ERR
                 MOVE EMP-ID     TO DET-EMP-ID
                 MOVE EMP-NOMBRE TO DET-EMP-NOMBRE
                    WRITE PRINTLINE FROM DETAIL-LINE AFTER
                       ADVANCING 1 LINE
                    READ TRANSFILE
                      AT END SET TRANS-EOF TO TRUE
                    END-READ
            END-EVALUATE.
       200-F. EXIT.

       300-FIN.
           DISPLAY '           RESUMEN DE ACTUALIZACION'.
           DISPLAY 'CANTIDAD DE REGISTROS INSERTADOS: ' WS-CONT-ACT.
           DISPLAY 'CANTIDAD DE REGISTROS ERRORNEOS:  ' WS-CONT-ERR.
           CLOSE EMPLOYEEFILE
                 TRANSFILE
                 NEWEMPFILE
                 OUTPUTFILE.
           STOP RUN.
       300-F. EXIT.
