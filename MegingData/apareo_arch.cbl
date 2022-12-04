      ******************************************************************
      * Author: Burgio
      * Date: 15/11/22
      * Purpose: Challenge nro. 5 de Cobol Essential Training
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMAPAR05.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT FUSESINC ASSIGN TO
           "C:\Users\dburg\Downloads\05_05\FUSESINC.DAT"
           FILE STATUS IS FS-FUSESINC
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT NEW-FUSEINC ASSIGN TO
           "C:\Users\dburg\Downloads\05_05\ACME.DAT"
           FILE STATUS IS FS-ACME
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT ARCH-ORDENADO ASSIGN TO
           "C:\Users\dburg\Downloads\05_05\ORDENADA.NEW"
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT WORKFILE ASSIGN TO
           "C:\Users\dburg\Downloads\05_05\WORK.TMP".

       SELECT EMP-RPT ASSIGN TO
       "C:\Users\dburg\Downloads\05_05\EMP-RPT.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
          FD FUSESINC.
           01 EMP-DETALLES     PIC X(48).

       FD NEW-FUSEINC.
           01 NEW-EMP-DETALLES PIC X(48).

       FD ARCH-ORDENADO.
           01 ARCH-ORDEN.
             88 ENDOFSALESFILE VALUE HIGH-VALUES.
             02 SD-ID            PIC 9(09).
             02 SD-APELLIDO      PIC X(10).
             02 SD-NOMBRE        PIC X(10).
             02 SD-FECHA         PIC X(08).
             02 SD-SALDO         PIC 9(09).
             02 SD-GENERO        PIC X.

       FD EMP-RPT.
           01 EMPLEADOS-RPT    PIC X(82).

       SD WORKFILE.
       01 WORKREC              PIC X(48).


       WORKING-STORAGE SECTION.
       01  WS-CONT-ERRORES   PIC 99 VALUE ZERO.
       01  WS-WORK-AREAS.
           05  FS-FUSESINC   PIC X(02).
           05  FS-ACME       PIC X(02).

       05  WS-FECHA.
             07  WS-FECHA-AA      PIC 99            VALUE ZEROS.
             07  WS-FECHA-MM      PIC 99            VALUE ZEROS.
             07  WS-FECHA-DD      PIC 99            VALUE ZEROS.

       01  WS-TITULO.
           05  WS-DD            PIC 99            VALUE ZEROS.
           05 FILLER            PIC X(01)         VALUE '/'.
           05  WS-MM            PIC 99            VALUE ZEROS.
           05 FILLER            PIC X(01)         VALUE '/'.
           05  WS-AA            PIC 99            VALUE ZEROS.
           05 FILLER            PIC X(13) VALUE SPACES.
           05 FILLER            PIC X(32)
                          VALUE "REPORTE ACTUALIZADO DE EMPLEADOS".
           05 FILLER            PIC X(20) VALUE SPACES.

       01  WS-CABECERA.
           05 FILLER            PIC X(08) VALUE 'NRO. ID'.
           05 FILLER            PIC X(03) VALUE SPACES.
           05 FILLER            PIC X(10) VALUE 'NOMBRE'.
           05 FILLER            PIC X(02) VALUE SPACES.
           05 FILLER            PIC X(10) VALUE 'APELLIDO'.
           05 FILLER            PIC X(04) VALUE SPACES.
           05 FILLER            PIC X(10) VALUE 'FECHA'.
           05 FILLER            PIC X(05) VALUE SPACES.
           05 FILLER            PIC X(10) VALUE 'SALDO'.
           05 FILLER            PIC X(02) VALUE SPACES.
           05 FILLER            PIC X(10) VALUE 'GENERO'.

       01  WS-SEPARADOR         PIC X(72) VALUE ALL '*'.

       01  WS-DETALLES.
           05 DET-ID            PIC X(09) VALUE ZERO.
           05 FILLER            PIC X(03) VALUE SPACES.
           05 DET-NOMBRE        PIC X(10) VALUE SPACES.
           05 FILLER            PIC X(02) VALUE SPACES.
           05 DET-APELLIDO      PIC X(10) VALUE SPACES.
           05 FILLER            PIC X(02) VALUE SPACES.
           05 DET-FECHA         PIC X(10) VALUE SPACES.
           05 FILLER            PIC X(02) VALUE SPACES.
           05 DET-SALDO         PIC $Z,ZZZ,ZZ9.99-.
           05 FILLER            PIC X(04) VALUE SPACES.
           05 DET-GENERO        PIC X     VALUE SPACE.



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 0100-ORDENAR-EMPLEADOS THRU 0100-FIN.
            PERFORM 0200-LEER-ARCHIVO THRU 0200-FIN.
            PERFORM 0900-FIN-PROGRAMA THRU 0900-FIN.
       END-MAIN.

       0100-ORDENAR-EMPLEADOS.

           PERFORM 0110-TOMAR-FECHA THRU 0110-FIN.

           OPEN INPUT FUSESINC, NEW-FUSEINC.

           IF (FS-FUSESINC EQUAL ZERO) AND (FS-ACME EQUAL ZERO)
             MERGE WORKFILE ON ASCENDING KEY
             SD-ID
             USING FUSESINC NEW-FUSEINC
             GIVING ARCH-ORDENADO
           END-IF.

       0100-FIN. EXIT.

       0110-TOMAR-FECHA.
           ACCEPT WS-FECHA FROM DATE.
           MOVE WS-FECHA-AA TO WS-AA.
           MOVE WS-FECHA-DD TO WS-DD.
           MOVE WS-FECHA-MM TO WS-MM.
       0110-FIN. EXIT.

       0200-LEER-ARCHIVO.
           OPEN INPUT ARCH-ORDENADO
                OUTPUT EMP-RPT.
            READ ARCH-ORDENADO
              AT END SET ENDOFSALESFILE TO TRUE
              END-READ.


           WRITE EMPLEADOS-RPT FROM WS-SEPARADOR AFTER 1 LINE.
           WRITE EMPLEADOS-RPT FROM WS-TITULO    AFTER 1 LINE.
           WRITE EMPLEADOS-RPT FROM WS-SEPARADOR AFTER 1 LINE.
           WRITE EMPLEADOS-RPT FROM WS-CABECERA  AFTER 2 LINE.
           PERFORM 0300-PROCESAR-EMPLEADOS THRU
                        0300-FIN UNTIL ENDOFSALESFILE.
       0200-FIN. EXIT.

       0300-PROCESAR-EMPLEADOS.
           MOVE SD-ID       TO DET-ID
           MOVE SD-APELLIDO TO DET-APELLIDO
           MOVE SD-NOMBRE   TO DET-NOMBRE
           MOVE SD-FECHA    TO DET-FECHA
           MOVE SD-SALDO    TO DET-SALDO
           IF SD-GENERO EQUAL 'M' OR 'F'
             MOVE SD-GENERO   TO DET-GENERO

           ELSE
             ADD 1 TO WS-CONT-ERRORES
             DISPLAY ARCH-ORDEN
           END-IF

           WRITE EMPLEADOS-RPT FROM WS-DETALLES AFTER 1 LINE

           READ ARCH-ORDENADO
              AT END SET ENDOFSALESFILE TO TRUE
              END-READ.
       0300-FIN. EXIT.


       0900-FIN-PROGRAMA.
           DISPLAY 'CANTIDAD DE ERRORES: 'WS-CONT-ERRORES.
           CLOSE FUSESINC
                 NEW-FUSEINC
                 ARCH-ORDENADO
                 EMP-RPT.
           STOP RUN.

       0900-FIN. EXIT.
