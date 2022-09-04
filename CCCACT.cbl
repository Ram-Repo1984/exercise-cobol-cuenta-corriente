      ******************************************************************
      * Author: Ramiro Angulo Lagraña
      * Date: Agosto - Septiembre 2022
      * Purpose: leer un fichero de entrada y crear un fichero de SALIDA
      *          donde se simplifique la visualizacion e interpretacion
      *          de los datos del fichero
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCCACT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA ASSIGN TO "ENTRADATXT.TXT"
           FILE STATUS IS FS-ENTRADA.

           SELECT SALIDA ASSIGN TO "SALIDA-CCCACT.TXT"
           FILE STATUS IS FS-SALIDA.
       I-O-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       FD ENTRADA
          RECORDING MODE IS F
          RECORD CONTAINS 52 CHARACTERS.

       01 REG-CCCACT.
           05 CCCACT-CLIENTE                 PIC X(10).
           05 CCCACT-SUCURSAL                PIC 9(06).
           05 CCCACT-CODIGO                  PIC X(03).
           05 CCCACT-FECHA.
               10 CCCACT-ANIO                PIC 9(04).
               10 CCCACT-MES                 PIC 9(02).
               10 CCCACT-DIA                 PIC 9(02).
           05 CCCACT-HORA.
               10 CCCACT-HH                  PIC 9(02).
               10 CCCACT-MM                  PIC 9(02).
               10 CCCACT-SS                  PIC 9(02).
           05 CCCACT-IMPORTE                 PIC S9(12)V9(04).
           05 CCCACT-TIPO                    PIC X(03).

       FD SALIDA
          RECORDING MODE F.
       01 REG-SALIDA                         PIC X(184).



       WORKING-STORAGE SECTION.
      *---------------------------------------------
      * VARIABLES PARA CONTROL DE LOS FILE STATUS.
      *---------------------------------------------

       01 WS-FILE-STATUS.
           05 FS-ENTRADA                    PIC X(02).
             88 ENTRADA-OK                      VALUE "00".
             88 EOF-ENTRADA                     VALUE "10".
           05 FS-SALIDA                     PIC X(02).
             88 LISTADO-OK                      VALUE "00".

      *---------------------------------------------
      * VARIABLES PARA AUXILIARES DATOS DE ENTRADA.
      *---------------------------------------------

       01 AUX-CCCACT.
           05 AUX-CCCACT-CLIENTE            PIC X(10).
           05 AUX-CCCACT-SUCURSAL           PIC 9(06).
           05 AUX-CCCACT-CODIGO             PIC X(03).
           05 AUX-CCCACT-FECHA.
               10 AUX-CCCACT-ANIO           PIC 9(04).
               10 AUX-CCCACT-MES            PIC 9(02).
               10 AUX-CCCACT-DIA            PIC 9(02).
           05 AUX-CCCACT-HORA.
               10 AUX-CCCACT-HH             PIC 9(02).
               10 AUX-CCCACT-MM             PIC 9(02).
               10 AUX-CCCACT-SS             PIC 9(02).
           05 AUX-CCCACT-IMPORTE            PIC S9(12)V9(04).
           05 AUX-CCCACT-TIPO               PIC X(03).
      *-------- 52

       01 WS-FECHA-PROCESO-SISTEMA.
           05 WS-PROCESO-ANIO               PIC 9(04).
           05 WS-PROCESO-MES                PIC 9(02).
           05 WS-PROCESO-DIA                PIC 9(02).

      *--------------------------------------------------
      * ARMADO DE CABECERAS, TITULOS Y COLUMNAS DE DATOS.
      *--------------------------------------------------

       01 WS-CABECERA-PRINCIPAL.
           05 FILLER                       PIC X(45) VALUE SPACES.
           05 WS-TITULO                    PIC X(41)
                VALUE "LISTADO DE CUENTAS CORRIENTES ACTUALIZADO".
           05 FILLER                       PIC X(45) VALUE SPACES.


       01 WS-SUBTITULOS.
           05 FILLER                       PIC X(16) VALUE SPACES.
           05 WS-NOMBRE-BANCO              PIC X(14)
                   VALUE "BANCO: GALICIA".
           05 FILLER                       PIC X(90) VALUE SPACES.
           05 WS-FECHA-SISTEMA.
               10 WS-DIA                   PIC 9(02) VALUE ZEROES.
               10 FILLER                   PIC X(01) VALUE "/".
               10 WS-MES                   PIC 9(02) VALUE ZEROES.
               10 FILLER                   PIC X(01) VALUE "/".
               10 WS-ANIO                  PIC 9(04) VALUE ZEROES.


       01 WS-COLUMNAS.
           05 FILLER                       PIC X(02).
           05 COLUMNA-CLIENTE              PIC X(11)
                 VALUE "COD-CLIENTE".
           05 FILLER                       PIC X(05).
           05 COLUMNA-SUCURSAL             PIC X(08)
                 VALUE "SUCURSAL".
           05 FILLER                       PIC X(10).
           05 COLUMNA-CODIGO               PIC X(06)
                 VALUE "CODIGO".
           05 FILLER                       PIC X(10).
           05 COLUMNA-FECHA                PIC X(12)
                 VALUE "FECHA Y HORA".
           05 FILLER                       PIC X(15).
           05 COLUMNA-IMPORTE              PIC X(07)
                 VALUE "IMPORTE".
           05 FILLER                       PIC X(10).
           05 COLUMNA-TIPO                 PIC X(18)
                 VALUE "TIPO DE MOVIMIENTO".
           05 FILLER                       PIC X(14).


       01 WS-DATOS-CCCACT.
           05 FILLER                       PIC X(03).
           05 WS-DATO-CLIENTE              PIC 9(10).
           05 FILLER                       PIC X(08).
           05 WS-DATO-SUCURSAL             PIC 9(06).
           05 FILLER                       PIC X(18).
           05 WS-DATO-CODIGO               PIC X(03).
           05 FILLER                       PIC X(10).
           05 WS-DATO-FECHA.
               10 DATO-DIA                 PIC 9(02).
               10 FILLER                   PIC X(01) VALUE "/".
               10 DATO-MES                 PIC 9(02).
               10 FILLER                   PIC X(01) VALUE "/".
               10 DATO-ANIO                PIC 9(04).
           05 FILLER                       PIC X(02).
           05 WS-DATO-HORA.
               10 DATO-HH                  PIC 9(02).
               10 FILLER                   PIC X(01) VALUE ":".
               10 DATO-MM                  PIC 9(02).
               10 FILLER                   PIC X(01) VALUE ":".
               10 DATO-SS                  PIC 9(02).
           05 FILLER                       PIC X(02).
           05 WS-DATO-IMPORTE              PIC $ZZZ.ZZZ.ZZZ.Z99,9999.
           05 FILLER                       PIC X(15).
           05 WS-DATO-TIPO                 PIC X(03).
           05 FILLER                       PIC X(70) VALUE SPACES.
      *------ 130
      *--------------------------><--------><---------------------------

       01 LINEA-TITULO-PRINCIPAL           PIC X(133) VALUE SPACES.


       PROCEDURE DIVISION.

       100000-PRINCIPAL.

           INITIALIZE AUX-CCCACT

           ACCEPT WS-FECHA-PROCESO-SISTEMA FROM DATE YYYYMMDD.
           MOVE WS-PROCESO-ANIO            TO WS-ANIO
           MOVE WS-PROCESO-MES             TO WS-MES
           MOVE WS-PROCESO-DIA             TO WS-DIA


           PERFORM 10000-INICIO
           THRU 10000-F-INICIO.

           PERFORM 20000-PROCESO
           THRU 20000-F-PROCESO
           UNTIL EOF-ENTRADA.

           PERFORM 30000-FIN-PGM
           THRU 30000-F-FIN-PGM.

           STOP RUN.

       10000-INICIO.
           PERFORM 11000-ABRIR-FICHERO
           THRU 11000-F-ABRIR-FICHERO.

           PERFORM 50000-IMPRIMIR-CABECERAS
           THRU 50000-F-IMPRIMIR-CABECERAS.

           PERFORM 12000-PRIMER-LECTURA
           THRU 12000-F-PRIMER-LECTURA.

           PERFORM 13000-MOVER-DETALLES
           THRU 13000-F-MOVER-DETALLES.

           PERFORM 51000-IMPRIMIR-DATOS
           THRU 51000-F-IMPRIMIR-DATOS.

       10000-F-INICIO.
           EXIT.

       11000-ABRIR-FICHERO.
           OPEN INPUT ENTRADA
                OUTPUT SALIDA.

           IF ENTRADA-OK THEN
               DISPLAY "APERTURA DE FICHERO SIN ERRORES"
               DISPLAY "-- " FS-ENTRADA
           END-IF.

           IF NOT LISTADO-OK THEN
               DISPLAY "LISTADO DE SALIDAD CON ERRORES."
               DISPLAY "-- " FS-SALIDA
           END-IF.
       11000-F-ABRIR-FICHERO.
           EXIT.

       12000-PRIMER-LECTURA.
           READ ENTRADA INTO AUX-CCCACT.
       12000-F-PRIMER-LECTURA.
           EXIT.

       13000-MOVER-DETALLES.

           MOVE AUX-CCCACT-CLIENTE TO WS-DATO-CLIENTE.
           MOVE AUX-CCCACT-SUCURSAL TO WS-DATO-SUCURSAL.
           MOVE AUX-CCCACT-CODIGO TO WS-DATO-CODIGO.

           MOVE AUX-CCCACT-DIA TO DATO-DIA.
           MOVE AUX-CCCACT-MES TO DATO-MES.
           MOVE AUX-CCCACT-ANIO TO DATO-ANIO.

           MOVE AUX-CCCACT-HH TO DATO-HH.
           MOVE AUX-CCCACT-MM TO DATO-MM.
           MOVE AUX-CCCACT-SS TO DATO-SS.

           MOVE AUX-CCCACT-IMPORTE TO WS-DATO-IMPORTE.
           MOVE AUX-CCCACT-TIPO TO WS-DATO-TIPO.

       13000-F-MOVER-DETALLES.
           EXIT.

       20000-PROCESO.

           PERFORM 12000-PRIMER-LECTURA
           THRU 12000-F-PRIMER-LECTURA.

           PERFORM 13000-MOVER-DETALLES
           THRU 13000-F-MOVER-DETALLES.

           PERFORM 51000-IMPRIMIR-DATOS
           THRU 51000-F-IMPRIMIR-DATOS.

       20000-F-PROCESO.
           EXIT.

       30000-FIN-PGM.
           CLOSE ENTRADA SALIDA.
       30000-F-FIN-PGM.
           EXIT.

       50000-IMPRIMIR-CABECERAS.

           MOVE WS-CABECERA-PRINCIPAL TO LINEA-TITULO-PRINCIPAL.
           WRITE REG-SALIDA FROM LINEA-TITULO-PRINCIPAL
           AFTER ADVANCING 1 LINES.

           MOVE WS-SUBTITULOS TO LINEA-TITULO-PRINCIPAL.
           WRITE REG-SALIDA FROM LINEA-TITULO-PRINCIPAL
           AFTER ADVANCING 1 LINES.

           MOVE WS-COLUMNAS TO LINEA-TITULO-PRINCIPAL.
           WRITE REG-SALIDA FROM LINEA-TITULO-PRINCIPAL
           AFTER ADVANCING 1 LINES.

       50000-F-IMPRIMIR-CABECERAS.
           EXIT.

       51000-IMPRIMIR-DATOS.
           MOVE WS-DATOS-CCCACT TO LINEA-TITULO-PRINCIPAL.
           WRITE REG-SALIDA FROM LINEA-TITULO-PRINCIPAL
           AFTER ADVANCING 2 LINES.
       51000-F-IMPRIMIR-DATOS.
           EXIT.




       END PROGRAM CCCACT.
