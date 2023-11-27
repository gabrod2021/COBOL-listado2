 ******************************************************************
      * Author:Gabriela Cristina Rodriguez
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGCVEM2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
      * Variables generales.
       01 WS-VARIABLES.
          02 WS-NOM-CANDIDATO          PIC X(99) VALUE SPACE.
          02 WS-ESTADO                 PIC X(10) VALUE SPACE.
          02 WS-PUNTOS                 PIC 9(01) VALUE ZERO.


       01 WS-CV.
          02 WS-ESTUDIANTE             PIC X(01) VALUE SPACE.
             88  WS-ESTUDIANTE-SI                VALUE 'S'.
             88  WS-ESTUDIANTE-NO                VALUE 'N'.
             88  WS-ESTUDIANTE-A                 VALUE 'A'.
          02 WS-EXPERIENCIA            PIC X(01) VALUE SPACE.
             88  WS-EXPERIENCIA-SI               VALUE 'S'.
             88  WS-EXPERIENCIA-NO               VALUE 'N'.
          02 WS-COBOL                  PIC X(01) VALUE SPACE.
             88  WS-COBOL-SI                     VALUE 'S'.
             88  WS-COBOL-NO                     VALUE 'N'.
          02 WS-INGLES                 PIC X(01) VALUE SPACE.
             88  WS-INGLES-SI                    VALUE 'S'.
             88  WS-INGLES-NO                    VALUE 'N'.



       01 WS-PLANILLA.
          05 WS-CABECERA.
             07 WS-SEPARADOR-1            PIC X(01) VALUE '|'.
             07 WS-NOMBRE-CAB             PIC X(30) VALUE '    Nombre'.
             07 WS-SEPARADOR-2            PIC X(01) VALUE '|'.
             07 WS-ESTADO-CAB             PIC X(30) VALUE '    Estado'.
             07 WS-SEPARADOR-3            PIC X(01) VALUE '|'.
             07 WS-PUNTOS-CAB             PIC X(30) VALUE '    Puntos'.
             07 WS-SEPARADOR-4            PIC X(01) VALUE '|'.
          05 WS-LINEA                     PIC X(94) VALUE ALL '-'.
          05 WS-REGISTRO.
             07 WS-SEPARADOR-1            PIC X(01) VALUE '|'.
             07 FILLER                    PIC X(04) VALUE SPACES.
             07 WS-NOMBRE-REG             PIC X(26) VALUE 'Nombre'.
             07 WS-SEPARADOR-2            PIC X(01) VALUE '|'.
             07 FILLER                    PIC X(04) VALUE SPACES.
             07 WS-ESTADO-REG             PIC X(26) VALUE 'Estado'.
             07 WS-SEPARADOR-3            PIC X(01) VALUE '|'.
             07 FILLER                    PIC X(04) VALUE SPACES.
             07 WS-PUNTOS-REG             PIC X(26) VALUE 'Puntos'.
             07 WS-SEPARADOR-4            PIC X(01) VALUE '|'.
       PROCEDURE DIVISION.
       0000-PROCESO-PRINCIPAL.

           PERFORM 1000-CARGAR-DATOS
              THRU 1000-CARGAR-DATOS-FIN

           PERFORM 2000-VERIFICAR-DATOS
              THRU 2000-VERIFICAR-DATOS-FIN

           PERFORM 3000-PROCESAR-DATOS
              THRU 3000-PROCESAR-DATOS-EXIT.

           PERFORM 4000-IMPRIMIR
              THRU 4000-IMPRIMIR-EXIT.

           STOP RUN.
       0000-PROCESO-PRINCIPAL-FIN.
           EXIT.

      * Carga de datos
       1000-CARGAR-DATOS.
           DISPLAY "Ingresar Nombre del candidato: "
           ACCEPT WS-NOM-CANDIDATO

           DISPLAY "Esta estudiando una carreta universitaria?(S/N/A) "
           ACCEPT WS-ESTUDIANTE

           DISPLAY "Tiene experiencia en el area de IT?(S/N) "
           ACCEPT WS-EXPERIENCIA

           DISPLAY "Sabe programar en leguanje cobol?(S/N) "
           ACCEPT WS-COBOL

           DISPLAY "Sabe hablar en ingles?(S/N) "
           ACCEPT WS-INGLES.

       1000-CARGAR-DATOS-FIN.
           EXIT.

      * Verificar datos ingresados
       2000-VERIFICAR-DATOS.

      *    Verifica si esta estudiando una carrera universitaria
           IF WS-ESTUDIANTE EQUAL 'S' OR 's' THEN
              ADD 2 TO  WS-PUNTOS
           ELSE IF WS-ESTUDIANTE EQUAL 'A' OR 'a' THEN
              ADD 1 TO WS-PUNTOS
           ELSE
              ADD 0 TO WS-PUNTOS
           END-IF

      *    Verifica si tien experiencia en el area de IT
           IF WS-EXPERIENCIA EQUAL 'S' OR 's' THEN
              ADD 1 TO  WS-PUNTOS
           END-IF

      *    Verifica si sabe programar en COBOL
           IF WS-COBOL EQUAL 'S' OR 's' THEN
              ADD 1 TO  WS-PUNTOS
           END-IF

      *    Verifica si sabe hablar ingles
           IF WS-INGLES EQUAL 'S' OR 's' THEN
              ADD 1 TO  WS-PUNTOS
           END-IF.

       2000-VERIFICAR-DATOS-FIN.
           EXIT.

      * Procesar datos
       3000-PROCESAR-DATOS.

      *    Comprobar si supero la entrevista  con 3 o mas puntos
           IF WS-PUNTOS  > 2 THEN
      *    IF WS-PUNTOS  >= 3 THEN
      *    IF WS-PUNTOS  GREATER 2 THEN
      *    IF WS-PUNTOS  IS NOT LESS 2 THEN
               MOVE 'APROBADO'   TO WS-ESTADO
           ELSE
               MOVE 'REPROBADO'  TO WS-ESTADO
           END-IF.

       3000-PROCESAR-DATOS-EXIT.
           EXIT.


      * Imprimir listado
       4000-IMPRIMIR.

      *    Mover variables en el  listado
           MOVE  WS-NOM-CANDIDATO  TO WS-NOMBRE-REG
           MOVE  WS-ESTADO         TO WS-ESTADO-REG
           MOVE  WS-PUNTOS         TO WS-PUNTOS-REG.

      *    Imprimir planilla
           DISPLAY WS-LINEA.
           DISPLAY WS-CABECERA.
           DISPLAY WS-LINEA.
           DISPLAY WS-REGISTRO.

       4000-IMPRIMIR-EXIT.
           EXIT.



       END PROGRAM PRGCVEM2.
