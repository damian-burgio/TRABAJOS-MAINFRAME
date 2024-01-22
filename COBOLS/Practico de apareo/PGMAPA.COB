      **************************************
      *                                    *
      *  PGMAPA  : APAREO SECUENCIAL       *
      *  CON ACTUALIZACION DE SALDO Y      *
      *  ALTAS, BAJAS Y MODIFICACIONES     *
      *  DE SOCIOS.                        *
      *                                    *
      **************************************
      *      MANTENIMIENTO DE PROGRAMA     *
      **************************************
      *  FECHA   *    DETALLE        * COD *
      **************************************
      *          *                   *     *
      *   HOY    * APAREO SECUENCIAL *     *
      *          *                   *     *
      **************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMAPA.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.  IBM-370 WITH DEBUGGING MODE.
      * OBJECT-COMPUTER.  IBM-370.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS
           ASSIGN TO "..\SOCIOS.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SOCIOS.
      *
           SELECT NOVEDADES
           ASSIGN TO "..\NOVEDADES.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-NOVEDADES.
      *
           SELECT LOG-ERRORES
           ASSIGN TO "..\LOG-ERRORES.txt"
           FILE STATUS IS FS-LOG-ERRORES.
      *
           SELECT SOCIOS-ACT
           ASSIGN TO "..\SOCIOS-ACT.txt"
           FILE STATUS IS FS-SOCIOS-ACT.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  SOCIOS.
       01  SOC-REG.
           88 ENDOFFILE VALUE HIGH-VALUES.
           03 SOC-SOCIO                     PIC 9(04).
           03 SOC-NOMBRE                    PIC X(20).
           03 SOC-SALDO                     PIC S9(08)V99.
      *
       FD  NOVEDADES.
       01  NOV-REG.
           88 ENDOFFILE2 VALUE HIGH-VALUES.
           03 NOV-SOCIO                     PIC 9(04).
           03 NOV-MOV                       PIC X.
           03 NOV-NOMBRE                    PIC X(20).
           03 NOV-IMPORTE                   PIC S9(7)V99.
      *
       FD  LOG-ERRORES.
       01  LOG-REG.
           03 LOG-SOCIO                     PIC 9(04).
           03 LOG-MOV                       PIC X.
           03 LOG-IMPORTE                   PIC S9(07)V99.
           03 LOG-ERROR                     PIC X(40).
      *
       FD  SOCIOS-ACT.
       01  SOCACT-REG.
           03 SOCACT-SOCIO                  PIC 9(04).
           03 SOCACT-NOMBRE                 PIC X(20).
           03 SOCACT-SALDO                  PIC S9(08)V99.
      *
      *
       WORKING-STORAGE SECTION.
      *
       01 FS-FILE-STATUS.
          05 FS-SOCIOS                      PIC X(02).
          05 FS-NOVEDADES                   PIC X(02).
          05 FS-LOG-ERRORES                 PIC X(02).
          05 FS-SOCIOS-ACT                  PIC X(02).
      *
      *
       01 CT-CONTADORES.
          05 CT-LEIDOS-SOCIOS               PIC 9(02).
          05 CT-LEIDOS-NOVEDADES            PIC 9(02).
          05 CT-ERRORES                     PIC 9(02).
      *
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-I-INICIO
              THRU 1000-F-INICIO.
      *
           PERFORM 2000-I-PROCESO
              THRU 2000-F-PROCESO
              UNTIL ENDOFFILE OR ENDOFFILE2
      *
           PERFORM 9999-I-FINAL
              THRU 9999-F-FINAL.
      *
       MAIN-PROGRAM-FINAL. GOBACK.


       1000-I-INICIO.
      D    DISPLAY "1000-I-INICIO."
      * ----       APERTURA DE LOS ARCHIVOS       ---- *
           OPEN INPUT SOCIOS
           IF FS-SOCIOS NOT EQUAL '00'
              DISPLAY '* ERROR EN OPEN ENTRADA INICIO = ' FS-SOCIOS
           END-IF.
      *
           OPEN INPUT NOVEDADES
           IF FS-NOVEDADES NOT EQUAL '00'
              DISPLAY '* ERROR EN OPEN ENTRADA INICIO = ' FS-NOVEDADES
           END-IF.
      *
           OPEN OUTPUT LOG-ERRORES
           IF FS-LOG-ERRORES NOT EQUAL '00'
              DISPLAY '* ERROR EN OPEN ENTRADA INICIO = ' FS-LOG-ERRORES
           END-IF.
      *
           OPEN OUTPUT SOCIOS-ACT
           IF FS-SOCIOS-ACT NOT EQUAL '00'
              DISPLAY '* ERROR EN OPEN ENTRADA INICIO = ' FS-SOCIOS-ACT
           END-IF.
      *
      * ---- PRIMERA LECTURA DE ARCHIVOS DE APAREO ---- *
           PERFORM 3000-I-LEER-NOVEDADES
              THRU 3000-F-LEER-NOVEDADES.
      *
           PERFORM 4000-I-LEER-SOCIO
              THRU 4000-F-LEER-SOCIO.
      *
       1000-F-INICIO. EXIT.
      *
      *
       2000-I-PROCESO.
      D    DISPLAY "2000-I-PROCESO."
           PERFORM 5000-I-CHEQUEO-ERROR
              THRU 5000-F-CHEQUEO-ERROR

           IF SOC-SOCIO = NOV-SOCIO
              EVALUATE NOV-MOV
                 WHEN 'A'
                    MOVE 'SE QUIERE DAR DE ALTA USUARIO EXISTENTE'
                      TO LOG-ERROR
                    PERFORM 6000-I-IMPRESION-ERROR
                       THRU 6000-F-IMPRESION-ERROR
                 WHEN 'B'
                    CONTINUE
                 WHEN 'M'
                    COMPUTE SOC-SALDO = SOC-SALDO + NOV-IMPORTE
                    MOVE SOC-REG TO SOCACT-REG
                    WRITE SOCACT-REG AFTER ADVANCING 1 LINE
              END-EVALUATE
      *
              PERFORM 4000-I-LEER-SOCIO
                 THRU 4000-F-LEER-SOCIO
      *
              PERFORM 3000-I-LEER-NOVEDADES
                 THRU 3000-F-LEER-NOVEDADES
           ELSE
              IF SOC-SOCIO < NOV-SOCIO
                 MOVE SOC-REG TO SOCACT-REG
                 WRITE SOCACT-REG AFTER ADVANCING 1 LINE
                 PERFORM 4000-I-LEER-SOCIO
                    THRU 4000-F-LEER-SOCIO
           ELSE
              IF SOC-SOCIO > NOV-SOCIO
                 IF NOV-MOV = 'A'
                    MOVE NOV-SOCIO   TO SOCACT-SOCIO
                    MOVE NOV-NOMBRE  TO SOCACT-NOMBRE
                    MOVE NOV-IMPORTE TO SOC-SALDO
                    WRITE SOCACT-REG AFTER ADVANCING 1 LINE
                 END-IF
                 PERFORM 3000-I-LEER-NOVEDADES
                    THRU 3000-F-LEER-NOVEDADES
           END-IF.
       2000-F-PROCESO. EXIT.
      *
      *
       3000-I-LEER-NOVEDADES.
      D     DISPLAY "3000-I-LEER-NOVEDADES."
      *
           READ NOVEDADES
              AT END SET ENDOFFILE2 TO TRUE
           END-READ
      *
           EVALUATE FS-NOVEDADES
              WHEN '00'
                 CONTINUE
              WHEN '10'
                 CONTINUE
              WHEN OTHER
                 DISPLAY '* ERROR EN LECTURA NOVEDADES = ' FS-NOVEDADES
           END-EVALUATE
      *
           ADD 1          TO CT-LEIDOS-NOVEDADES.
      *
       3000-F-LEER-NOVEDADES. EXIT.
      *
      *
       4000-I-LEER-SOCIO.
      D     DISPLAY "4000-I-LEER-SOCIO."
      *
           READ SOCIOS
              AT END SET ENDOFFILE TO TRUE
           END-READ
      *
           EVALUATE FS-SOCIOS
              WHEN '00'
                 CONTINUE
              WHEN '10'
                 CONTINUE
              WHEN OTHER
                 DISPLAY '* ERROR EN LECTURA NOVEDADES = ' FS-SOCIOS
           END-EVALUATE
      *
           ADD 1          TO CT-LEIDOS-SOCIOS.
      *
       4000-F-LEER-SOCIO. EXIT.
      *
      *
       5000-I-CHEQUEO-ERROR.
      D    DISPLAY "5000-I-CHEQUEO-ERROR."
           IF (NOV-SOCIO NOT NUMERIC) OR (NOV-IMPORTE NOT NUMERIC)
             MOVE "Valores no numericos en SOCIO o IMPORTE."
               TO LOG-ERROR
             PERFORM 6000-I-IMPRESION-ERROR
                THRU 6000-F-IMPRESION-ERROR
           END-IF
      *
           IF (NOV-MOV NOT EQUAL 'A')
              IF (NOV-MOV NOT EQUAL 'B')
                 IF (NOV-MOV NOT EQUAL 'M')
                    MOVE "Valor distinto de A, B o M."
                      TO LOG-ERROR
                    PERFORM 6000-I-IMPRESION-ERROR
                       THRU 6000-F-IMPRESION-ERROR
                 END-IF
              END-IF
           END-IF
      *
           IF NOV-NOMBRE EQUAL SPACES OR NOV-NOMBRE NOT ALPHABETIC
              MOVE "Campo de NOMBRE vacio o no alfabetico."
                TO LOG-ERROR
              PERFORM 6000-I-IMPRESION-ERROR
                 THRU 6000-F-IMPRESION-ERROR
           END-IF.
       5000-F-CHEQUEO-ERROR. EXIT.
      *
      *
       6000-I-IMPRESION-ERROR.
      D    DISPLAY "6000-I-IMPRESION-ERROR."
           MOVE NOV-SOCIO   TO LOG-SOCIO
           MOVE NOV-MOV     TO LOG-MOV
           MOVE NOV-IMPORTE TO LOG-IMPORTE
           WRITE LOG-REG AFTER ADVANCING 1 LINE
           ADD 1 TO CT-ERRORES.
       6000-F-IMPRESION-ERROR. EXIT.
      *
      *
       9999-I-FINAL.
      D     DISPLAY "9999-I-FINAL."
      *
           CLOSE SOCIOS
           IF FS-SOCIOS IS NOT EQUAL "00"
              DISPLAY "* ERROR EN CLOSE CLIENTE = " FS-SOCIOS
              MOVE 9999 TO RETURN-CODE
           END-IF
      *
           CLOSE NOVEDADES
           IF FS-NOVEDADES IS NOT EQUAL "00"
              DISPLAY "* ERROR EN CLOSE CLIENTE = " FS-NOVEDADES
              MOVE 9999 TO RETURN-CODE
           END-IF
      *
           CLOSE LOG-ERRORES
           IF FS-LOG-ERRORES IS NOT EQUAL "00"
              DISPLAY "* ERROR EN CLOSE CLIENTE = " FS-LOG-ERRORES
              MOVE 9999 TO RETURN-CODE
            END-IF
      *
           CLOSE SOCIOS-ACT
           IF FS-SOCIOS-ACT IS NOT EQUAL "00"
              DISPLAY "* ERROR EN CLOSE CLIENTE = " FS-SOCIOS-ACT
              MOVE 9999 TO RETURN-CODE
            END-IF.
      *
           DISPLAY '---------------------------------------'
           DISPLAY '         TOTALES DE CONTROL'
           DISPLAY '---------------------------------------'
           DISPLAY ' '
           DISPLAY '           REGISTROS LEIDOS'
           DISPLAY '           ----------------'
           DISPLAY 'REGISTROS CON ERRORES        : ' CT-ERRORES
           DISPLAY 'REGISTROS LEIDOS DE NOVEDADES: ' CT-LEIDOS-NOVEDADES
           DISPLAY 'REGISTROS LEIDOS DE SOCIOS   : ' CT-LEIDOS-SOCIOS
           DISPLAY ' '
           DISPLAY '---------------------------------------'.
      *
       9999-F-FINAL. EXIT.
