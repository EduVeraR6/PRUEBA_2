      ******************************************************************
      * Author:Eduardo Vera Romero
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-OPCION PIC 9.
       01 WS-SI-NO PIC X(2).
       01 WS-CONTADOR PIC 9(2) VALUE ZERO.
       01 WS-SALARIO-NETO-BUSQUEDA PIC 9(7)V99.
       01 WS-NIVEL-EDUCATIVO-BUSQUEDA PIC X(25).
       01 WS-BANDERA PIC X VALUE 'N'.
       01 WS-INDICE PIC 9(2) VALUE ZERO.
       01 WS-NUMERO-CEDULA-BUSQUEDA PIC X(10).
       01  EMPLEADO-TABLE.
           05 RECORD-EMPLEADO OCCURS 20 TIMES INDEXED BY IDX_EMPLEADO.
               10 ID-EMPLEADO PIC X(10).
               10 NOMBRE PIC X(20).
               10 SALARIO-BRUTO PIC 9(5)V99.
               10 DEDUCCION-IMPUESTOS PIC 9(5)V99.
               10 DEDUCCION-SEGURO PIC 9(5)V99.
               10 SALARIO-NETO PIC 9(7)V99.
               10 DATOS-SOCIOECONOMICOS.
                   15 NIVEL-EDUCATIVO PIC X(25).
                   15 TIPO-VIVIENDA PIC X(25).
                   15 NUMERO-DORMITORIOS PIC 9(2).
                   15 NUMERO-VEHICULOS PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

       1000-MENU-PROGRAMA.
            DISPLAY "***************************"
            DISPLAY "BIENVENIDO!"
            DISPLAY "ELIJA UNA DE LAS OPCIONES: "
            DISPLAY "1.- INGRESAR EMPLEADO"
            DISPLAY "2.- CALCULAR SALARIO"
            DISPLAY "3.- MOSTRAR INFORMACION EMPLEADO"
            DISPLAY "4.- FILTRAR"
            DISPLAY "5.- SALIR"

            DISPLAY SPACES
            ACCEPT WS-OPCION

            EVALUATE WS-OPCION
            WHEN 1
                 PERFORM 2000-INGRESAR-EMPLEADO
            WHEN 2
                 PERFORM 3000-CALCULAR-SALARIO
            WHEN 3
                 PERFORM 4000-MOSTRAR-INFO
            WHEN 4
                 PERFORM 5000-FILTRAR-SUBMENU
            WHEN 5
                 STOP RUN
            WHEN OTHER
                 DISPLAY "OPCION ERRONEA, VUELVA A INTENTARLO"
                 PERFORM 1000-MENU-PROGRAMA
            END-EVALUATE.


       2000-INGRESAR-EMPLEADO.

            IF WS-CONTADOR < 20 THEN

             MOVE 'N' TO WS-BANDERA

      * VALIDACION INGRESO CEDULA
               PERFORM UNTIL WS-BANDERA = 'S'
                  DISPLAY "INGRESE LA CEDULA DEL EMPLEADO:"
                  ACCEPT ID-EMPLEADO(IDX_EMPLEADO)

                  IF ID-EMPLEADO(IDX_EMPLEADO) IS NUMERIC AND
                     FUNCTION LENGTH(ID-EMPLEADO(IDX_EMPLEADO)) = 10
                     THEN
                       MOVE 'S' TO WS-BANDERA
                  ELSE
                      DISPLAY SPACE
                      DISPLAY "CEDULA NO VALIDA"
                      DISPLAY SPACE
                  END-IF
               END-PERFORM

               MOVE 'N' TO WS-BANDERA

      * VALIDACION INGRESO NOMBRES
              PERFORM UNTIL WS-BANDERA = 'S'
                   DISPLAY "INGRESE EL NOMBRE DEL EMPLEADO:"
                   ACCEPT NOMBRE(IDX_EMPLEADO)

                   IF NOMBRE(IDX_EMPLEADO) IS ALPHABETIC AND
                      FUNCTION LENGTH(NOMBRE(IDX_EMPLEADO)) > 10 AND
                      NOMBRE(IDX_EMPLEADO) NOT = SPACES
                      THEN
                      MOVE 'S' TO WS-BANDERA
                   ELSE
                      DISPLAY SPACE
                      DISPLAY "NOMBRES NO VALIDOS"
                      DISPLAY SPACE
                   END-IF
              END-PERFORM

               MOVE 'N' TO WS-BANDERA

      * VALIDACION INGRESO SALARIO-BRUTO
               PERFORM UNTIL WS-BANDERA = 'S'
                   DISPLAY "INGRESE EL SALARIO QUE RECIBE:"
                   ACCEPT SALARIO-BRUTO(IDX_EMPLEADO)

                   IF SALARIO-BRUTO(IDX_EMPLEADO) IS NUMERIC AND
                      SALARIO-BRUTO(IDX_EMPLEADO) > 10 THEN
                      MOVE 'S' TO WS-BANDERA
                   ELSE
                      DISPLAY SPACE
                      DISPLAY "SALARIO NO VALIDO"
                      DISPLAY SPACE
                   END-IF
               END-PERFORM

              MOVE 'N' TO WS-BANDERA

      * VALIDACION INGRESO DEDUCCION-IMPUESTOS
           PERFORM UNTIL WS-BANDERA = 'S'
               DISPLAY "INGRESE LA DEDUCCION IMPUESTOS:"
                ACCEPT DEDUCCION-IMPUESTOS(IDX_EMPLEADO)

                 IF DEDUCCION-IMPUESTOS(IDX_EMPLEADO) IS NUMERIC AND
                    DEDUCCION-IMPUESTOS(IDX_EMPLEADO) > 0 THEN
                    MOVE 'S' TO WS-BANDERA
                 ELSE
                     DISPLAY SPACE
                     DISPLAY "DEDUCCION DE IMPUESTOS INVALIDA"
                     DISPLAY SPACE
                 END-IF
           END-PERFORM

               MOVE 'N' TO WS-BANDERA

      * VALIDACION INGRESO DEDUCCION-SEGURO
           PERFORM UNTIL WS-BANDERA = 'S'
               DISPLAY "INGRESE LA DEDUCCION SEGURO:"
                ACCEPT DEDUCCION-SEGURO(IDX_EMPLEADO)

                 IF DEDUCCION-SEGURO(IDX_EMPLEADO) IS NUMERIC AND
                    DEDUCCION-SEGURO(IDX_EMPLEADO) > 0 THEN
                    MOVE 'S' TO WS-BANDERA
                 ELSE
                     DISPLAY SPACE
                     DISPLAY "DEDUCCION DE SEGURO INVALIDA"
                     DISPLAY SPACE
                 END-IF
           END-PERFORM



           MOVE 'N' TO WS-BANDERA

      * VALIDACION INGRESO NIVEL EDUCATIVO
           PERFORM UNTIL WS-BANDERA = 'S'

               DISPLAY "INGRESE SU NIVEL-EDUCATIVO"
               ACCEPT NIVEL-EDUCATIVO(IDX_EMPLEADO)

               IF NIVEL-EDUCATIVO(IDX_EMPLEADO) IS ALPHABETIC AND
                   NIVEL-EDUCATIVO(IDX_EMPLEADO) NOT = SPACES
                   THEN
                   MOVE 'S' TO WS-BANDERA
               ELSE
                   MOVE 'NA' TO NIVEL-EDUCATIVO(IDX_EMPLEADO)
                    MOVE 'S' TO WS-BANDERA
               END-IF

           END-PERFORM


           MOVE 'N' TO WS-BANDERA

      * VALIDACION INGRESO TIPO VIVIENDA
           PERFORM UNTIL WS-BANDERA = 'S'

               DISPLAY "INGRESE EL TIPO DE VIVIENDA:"
               ACCEPT TIPO-VIVIENDA(IDX_EMPLEADO)

               IF TIPO-VIVIENDA(IDX_EMPLEADO) IS ALPHABETIC AND
                   TIPO-VIVIENDA(IDX_EMPLEADO) NOT = SPACES
                   THEN
                   MOVE 'S' TO WS-BANDERA
               ELSE
                   MOVE 'NA' TO TIPO-VIVIENDA(IDX_EMPLEADO)
                    MOVE 'S' TO WS-BANDERA
               END-IF
           END-PERFORM

           MOVE 'N' TO WS-BANDERA

      * VALIDACION INGRESO NUMERO-DORMITORIOS
           PERFORM UNTIL WS-BANDERA = 'S'

               DISPLAY "INGRESE EL NUMERO DE DORMITORIOS"
               ACCEPT NUMERO-DORMITORIOS(IDX_EMPLEADO)

               IF NUMERO-DORMITORIOS(IDX_EMPLEADO) IS NUMERIC AND
                   NUMERO-DORMITORIOS(IDX_EMPLEADO) > 0 THEN
                       MOVE 'S' TO WS-BANDERA
               ELSE
                     MOVE 0 TO NUMERO-DORMITORIOS(IDX_EMPLEADO)
                     MOVE 'S' TO WS-BANDERA
               END-IF
           END-PERFORM

           MOVE 'N' TO WS-BANDERA

      * VALIDACION INGRESO NUMERO-VEHICULOS
           PERFORM UNTIL WS-BANDERA = 'S'

               DISPLAY "INGRESE EL NUMERO DE VEHICULOS"
               ACCEPT NUMERO-VEHICULOS(IDX_EMPLEADO)

               IF NUMERO-VEHICULOS(IDX_EMPLEADO) IS NUMERIC AND
                   NUMERO-VEHICULOS(IDX_EMPLEADO) > 0 THEN
                       MOVE 'S' TO WS-BANDERA
               ELSE
                  MOVE 0 TO NUMERO-VEHICULOS(IDX_EMPLEADO)
                  MOVE 'S' TO WS-BANDERA
               END-IF
           END-PERFORM

      * ADD 1 AL CONTADOR
           ADD 1 TO WS-CONTADOR
           DISPLAY " "
           DISPLAY "EMPLEADO REGISTRADO"
           DISPLAY " "
           PERFORM 1000-MENU-PROGRAMA
            ELSE
                 DISPLAY SPACE
                 DISPLAY
                 "SE ALCANZO EL LIMITE DE REGISTROS, REGRESANDO AL MENU"
                 DISPLAY SPACE
                 PERFORM 1000-MENU-PROGRAMA
            END-IF.

       3000-CALCULAR-SALARIO.
           DISPLAY "CALCULAR SALARIO NETO"

            IF WS-CONTADOR > 0 THEN

             MOVE 'N' TO WS-BANDERA
           *> BUSQUEDA POR NUMERO DE CEDULA
            PERFORM UNTIL WS-BANDERA = 'S'
                  DISPLAY "INGRESE LA CEDULA DEL EMPLEADO:"
                  ACCEPT WS-NUMERO-CEDULA-BUSQUEDA

                  IF WS-NUMERO-CEDULA-BUSQUEDA IS NUMERIC AND
                     FUNCTION LENGTH(WS-NUMERO-CEDULA-BUSQUEDA) = 10
                     THEN
                       MOVE 'S' TO WS-BANDERA
                  ELSE
                      DISPLAY SPACE
                      DISPLAY "CEDULA NO VALIDA"
                      DISPLAY SPACE
                  END-IF
            END-PERFORM

            MOVE 'N' TO WS-BANDERA

            PERFORM UNTIL WS-INDICE > WS-CONTADOR OR WS-BANDERA = 'S'

            IF ID-EMPLEADO(IDX_EMPLEADO) = WS-NUMERO-CEDULA-BUSQUEDA
              THEN

             *> CALCULANDO SALARIO-NETO
            COMPUTE SALARIO-NETO(IDX_EMPLEADO) =
                   SALARIO-BRUTO(IDX_EMPLEADO) -
                   (DEDUCCION-IMPUESTOS(IDX_EMPLEADO) +
                   DEDUCCION-SEGURO(IDX_EMPLEADO))

            MOVE 'S' TO WS-BANDERA

             DISPLAY
             "DESEA CALCULAR EL SALARIO DE OTRO EMPLEADO ? : SI o NO "
             ACCEPT WS-SI-NO

             EVALUATE WS-SI-NO
             WHEN 'SI'
                 PERFORM 3000-CALCULAR-SALARIO
             WHEN 'NO'
                 PERFORM 1000-MENU-PROGRAMA
             WHEN OTHER
                 DISPLAY SPACE
                 DISPLAY "OPCION NO VALIDA, VOLVIENDO AL MENU PRINCIPAL"
                 DISPLAY SPACE
                 PERFORM 1000-MENU-PROGRAMA
             END-EVALUATE

            END-PERFORM
            ELSE
                 DISPLAY SPACE
                 DISPLAY
                 "NO HAY REGISTROS GUARDADOS, REGRESANDO AL MENU"
                 DISPLAY SPACE
                 PERFORM 1000-MENU-PROGRAMA
           END-IF.


       4000-MOSTRAR-INFO.
            DISPLAY "INFORMACION EMPLEADO".

            IF WS-CONTADOR > 0 THEN

             MOVE 'N' TO WS-BANDERA
           *> BUSQUEDA POR NUMERO DE CEDULA
            PERFORM UNTIL WS-BANDERA = 'S'
                  DISPLAY "INGRESE LA CEDULA DEL EMPLEADO:"
                  ACCEPT WS-NUMERO-CEDULA-BUSQUEDA

                  IF WS-NUMERO-CEDULA-BUSQUEDA IS NUMERIC AND
                     FUNCTION LENGTH(WS-NUMERO-CEDULA-BUSQUEDA) = 10
                     THEN
                       MOVE 'S' TO WS-BANDERA
                  ELSE
                      DISPLAY SPACE
                      DISPLAY "CEDULA NO VALIDA"
                      DISPLAY SPACE
                  END-IF
            END-PERFORM

            MOVE 'N' TO WS-BANDERA

            PERFORM UNTIL WS-INDICE > WS-CONTADOR OR WS-BANDERA = 'S'

            IF ID-EMPLEADO(IDX_EMPLEADO) = WS-NUMERO-CEDULA-BUSQUEDA
              THEN
                  DISPLAY "NOMBRES : " NOMBRE(IDX_EMPLEADO)
                  DISPLAY "CEDULA : " ID-EMPLEADO(IDX_EMPLEADO)
                  DISPLAY "SALARIO-BRUTO :" SALARIO-BRUTO(IDX_EMPLEADO)
                  DISPLAY "SALARIO-NETO :" SALARIO-NETO(IDX_EMPLEADO)
            DISPLAY
            "DEDUCCION-IMPUESTOS :"DEDUCCION-IMPUESTOS(IDX_EMPLEADO)
            DISPLAY "DEDUCCION-SEGURO :" DEDUCCION-SEGURO(IDX_EMPLEADO)
            DISPLAY "NIVEL-EDUCATIVO :" NIVEL-EDUCATIVO(IDX_EMPLEADO)
            DISPLAY "TIPO-VIVIENDA :" TIPO-VIVIENDA(IDX_EMPLEADO)
            DISPLAY
            "NUMERO-DORMITORIOS :" NUMERO-DORMITORIOS(IDX_EMPLEADO)
             DISPLAY
            "NUMERO-VEHICULOS :" NUMERO-VEHICULOS(IDX_EMPLEADO)
            MOVE 'S' TO WS-BANDERA

             DISPLAY "DESEA BUSCAR OTRO EMPLEADO ? : SI o NO "
             ACCEPT WS-SI-NO

             EVALUATE WS-SI-NO
             WHEN 'SI'
                 PERFORM 4000-MOSTRAR-INFO
             WHEN 'NO'
                 PERFORM 1000-MENU-PROGRAMA
             WHEN OTHER
                 DISPLAY SPACE
                 DISPLAY "OPCION NO VALIDA, VOLVIENDO AL MENU PRINCIPAL"
                 DISPLAY SPACE
                 PERFORM 1000-MENU-PROGRAMA
             END-EVALUATE

            END-PERFORM
            ELSE
                 DISPLAY SPACE
                 DISPLAY
                 "NO HAY REGISTROS GUARDADOS, REGRESANDO AL MENU"
                 DISPLAY SPACE
                 PERFORM 1000-MENU-PROGRAMA
           END-IF.



       5000-FILTRAR-SUBMENU.
            DISPLAY "FILTRAR POR : "
            DISPLAY "1.- SALARIO NETO"
            DISPLAY "2.- NIVEL EDUCATIVO"
            DISPLAY "3.- SALARIO O NIVEL EDUCATIVO"
            DISPLAY "4.- VOLVER AL MENU PRINCIPAL"
            DISPLAY SPACE

            ACCEPT WS-OPCION

            EVALUATE WS-OPCION
            WHEN 1
                 PERFORM 6000-FILTRO-SALARIO
            WHEN 2
                 PERFORM 7000-FILTRO-NIVEL-EDUCATIVO
            WHEN 3
                 PERFORM 8000-FILTRO-AMBOS
            WHEN 4
                 PERFORM 1000-MENU-PROGRAMA
            WHEN OTHER
                 DISPLAY SPACE
                 DISPLAY "OPCION DE FILTRO INCORRECTA"
                 DISPLAY SPACE
                 PERFORM 5000-FILTRAR-SUBMENU
            END-EVALUATE.


       6000-FILTRO-SALARIO.
            IF WS-CONTADOR > 0 THEN

                  MOVE 'N' TO WS-BANDERA

      * VALIDACION INGRESO SALARIO-NETO
               PERFORM UNTIL WS-BANDERA = 'S'
                   DISPLAY "INGRESE EL SALARIO NETO QUE RECIBE:"
                   ACCEPT WS-SALARIO-NETO-BUSQUEDA

                   IF WS-SALARIO-NETO-BUSQUEDA IS NUMERIC AND
                      WS-SALARIO-NETO-BUSQUEDA > 10 THEN
                      MOVE 'S' TO WS-BANDERA
                   ELSE
                      DISPLAY SPACE
                      DISPLAY "SALARIO NO VALIDO"
                      DISPLAY SPACE
                   END-IF
               END-PERFORM

            MOVE 'N' TO WS-BANDERA

            PERFORM UNTIL WS-INDICE > WS-CONTADOR OR WS-BANDERA = 'S'

            IF SALARIO-NETO(IDX_EMPLEADO) = WS-SALARIO-NETO-BUSQUEDA
              THEN
                  DISPLAY "NOMBRES : " NOMBRE(IDX_EMPLEADO)
                  DISPLAY "CEDULA : " ID-EMPLEADO(IDX_EMPLEADO)
                  DISPLAY "SALARIO-BRUTO :" SALARIO-BRUTO(IDX_EMPLEADO)
                  DISPLAY "SALARIO-NETO :" SALARIO-NETO(IDX_EMPLEADO)
            DISPLAY
            "DEDUCCION-IMPUESTOS :"DEDUCCION-IMPUESTOS(IDX_EMPLEADO)
            DISPLAY "DEDUCCION-SEGURO :" DEDUCCION-SEGURO(IDX_EMPLEADO)
            DISPLAY "NIVEL-EDUCATIVO :" NIVEL-EDUCATIVO(IDX_EMPLEADO)
            DISPLAY "TIPO-VIVIENDA :" TIPO-VIVIENDA(IDX_EMPLEADO)
            DISPLAY
            "NUMERO-DORMITORIOS :" NUMERO-DORMITORIOS(IDX_EMPLEADO)
             DISPLAY
            "NUMERO-VEHICULOS :" NUMERO-VEHICULOS(IDX_EMPLEADO)

            ADD 1 TO WS-INDICE

            IF WS-INDICE = 20 THEN
                 MOVE 'S' TO WS-BANDERA

             DISPLAY "DESEA BUSCAR OTRO EMPLEADO ? : SI o NO "
             ACCEPT WS-SI-NO

             EVALUATE WS-SI-NO
             WHEN 'SI'
                 PERFORM 6000-FILTRO-SALARIO
             WHEN 'NO'
                 PERFORM 1000-MENU-PROGRAMA
             WHEN OTHER
                 DISPLAY SPACE
                 DISPLAY "OPCION NO VALIDA, VOLVIENDO AL MENU PRINCIPAL"
                 DISPLAY SPACE
                 PERFORM 1000-MENU-PROGRAMA
             END-EVALUATE
            END-IF

            END-PERFORM
            ELSE
                 DISPLAY SPACE
                 DISPLAY
                 "NO HAY REGISTROS GUARDADOS, REGRESANDO AL MENU"
                 DISPLAY SPACE
                 PERFORM 1000-MENU-PROGRAMA
           END-IF.

       7000-FILTRO-NIVEL-EDUCATIVO.
                       IF WS-CONTADOR > 0 THEN

                  MOVE 'N' TO WS-BANDERA

      * VALIDACION INGRESO SALARIO-NETO
               PERFORM UNTIL WS-BANDERA = 'S'
                   DISPLAY "INGRESE EL SALARIO NETO QUE RECIBE:"
                   ACCEPT WS-SALARIO-NETO-BUSQUEDA

                   IF WS-SALARIO-NETO-BUSQUEDA IS NUMERIC AND
                      WS-SALARIO-NETO-BUSQUEDA > 10 THEN
                      MOVE 'S' TO WS-BANDERA
                   ELSE
                      DISPLAY SPACE
                      DISPLAY "SALARIO NO VALIDO"
                      DISPLAY SPACE
                   END-IF
               END-PERFORM

            MOVE 'N' TO WS-BANDERA

            PERFORM UNTIL WS-INDICE > WS-CONTADOR OR WS-BANDERA = 'S'

            IF SALARIO-NETO(IDX_EMPLEADO) = WS-SALARIO-NETO-BUSQUEDA
              THEN
                  DISPLAY "NOMBRES : " NOMBRE(IDX_EMPLEADO)
                  DISPLAY "CEDULA : " ID-EMPLEADO(IDX_EMPLEADO)
                  DISPLAY "SALARIO-BRUTO :" SALARIO-BRUTO(IDX_EMPLEADO)
                  DISPLAY "SALARIO-NETO :" SALARIO-NETO(IDX_EMPLEADO)
            DISPLAY
            "DEDUCCION-IMPUESTOS :"DEDUCCION-IMPUESTOS(IDX_EMPLEADO)
            DISPLAY "DEDUCCION-SEGURO :" DEDUCCION-SEGURO(IDX_EMPLEADO)
            DISPLAY "NIVEL-EDUCATIVO :" NIVEL-EDUCATIVO(IDX_EMPLEADO)
            DISPLAY "TIPO-VIVIENDA :" TIPO-VIVIENDA(IDX_EMPLEADO)
            DISPLAY
            "NUMERO-DORMITORIOS :" NUMERO-DORMITORIOS(IDX_EMPLEADO)
             DISPLAY
            "NUMERO-VEHICULOS :" NUMERO-VEHICULOS(IDX_EMPLEADO)

            ADD 1 TO WS-INDICE

            IF WS-INDICE = 20 THEN
                 MOVE 'S' TO WS-BANDERA

             DISPLAY "DESEA BUSCAR OTRO EMPLEADO ? : SI o NO "
             ACCEPT WS-SI-NO

             EVALUATE WS-SI-NO
             WHEN 'SI'
                 PERFORM 6000-FILTRO-SALARIO
             WHEN 'NO'
                 PERFORM 1000-MENU-PROGRAMA
             WHEN OTHER
                 DISPLAY SPACE
                 DISPLAY "OPCION NO VALIDA, VOLVIENDO AL MENU PRINCIPAL"
                 DISPLAY SPACE
                 PERFORM 1000-MENU-PROGRAMA
             END-EVALUATE
            END-IF

            END-PERFORM
            ELSE
                 DISPLAY SPACE
                 DISPLAY
                 "NO HAY REGISTROS GUARDADOS, REGRESANDO AL MENU"
                 DISPLAY SPACE
                 PERFORM 1000-MENU-PROGRAMA
           END-IF.


       8000-FILTRO-AMBOS.
            DISPLAY "FILTRO AMBOS".            IF WS-CONTADOR > 0 THEN

                  MOVE 'N' TO WS-BANDERA

      * VALIDACION INGRESO SALARIO-NETO
               PERFORM UNTIL WS-BANDERA = 'S'
                   DISPLAY "INGRESE EL SALARIO NETO QUE RECIBE:"
                   ACCEPT WS-SALARIO-NETO-BUSQUEDA

                   IF WS-SALARIO-NETO-BUSQUEDA IS NUMERIC AND
                      WS-SALARIO-NETO-BUSQUEDA > 10 THEN
                      MOVE 'S' TO WS-BANDERA
                   ELSE
                      DISPLAY SPACE
                      DISPLAY "SALARIO NO VALIDO"
                      DISPLAY SPACE
                   END-IF
               END-PERFORM

            MOVE 'N' TO WS-BANDERA

            PERFORM UNTIL WS-INDICE > WS-CONTADOR OR WS-BANDERA = 'S'

            IF SALARIO-NETO(IDX_EMPLEADO) = WS-SALARIO-NETO-BUSQUEDA
              THEN
                  DISPLAY "NOMBRES : " NOMBRE(IDX_EMPLEADO)
                  DISPLAY "CEDULA : " ID-EMPLEADO(IDX_EMPLEADO)
                  DISPLAY "SALARIO-BRUTO :" SALARIO-BRUTO(IDX_EMPLEADO)
                  DISPLAY "SALARIO-NETO :" SALARIO-NETO(IDX_EMPLEADO)
            DISPLAY
            "DEDUCCION-IMPUESTOS :"DEDUCCION-IMPUESTOS(IDX_EMPLEADO)
            DISPLAY "DEDUCCION-SEGURO :" DEDUCCION-SEGURO(IDX_EMPLEADO)
            DISPLAY "NIVEL-EDUCATIVO :" NIVEL-EDUCATIVO(IDX_EMPLEADO)
            DISPLAY "TIPO-VIVIENDA :" TIPO-VIVIENDA(IDX_EMPLEADO)
            DISPLAY
            "NUMERO-DORMITORIOS :" NUMERO-DORMITORIOS(IDX_EMPLEADO)
             DISPLAY
            "NUMERO-VEHICULOS :" NUMERO-VEHICULOS(IDX_EMPLEADO)

            ADD 1 TO WS-INDICE

            IF WS-INDICE = 20 THEN
                 MOVE 'S' TO WS-BANDERA

             DISPLAY "DESEA BUSCAR OTRO EMPLEADO ? : SI o NO "
             ACCEPT WS-SI-NO

             EVALUATE WS-SI-NO
             WHEN 'SI'
                 PERFORM 6000-FILTRO-SALARIO
             WHEN 'NO'
                 PERFORM 1000-MENU-PROGRAMA
             WHEN OTHER
                 DISPLAY SPACE
                 DISPLAY "OPCION NO VALIDA, VOLVIENDO AL MENU PRINCIPAL"
                 DISPLAY SPACE
                 PERFORM 1000-MENU-PROGRAMA
             END-EVALUATE
            END-IF

            END-PERFORM
            ELSE
                 DISPLAY SPACE
                 DISPLAY
                 "NO HAY REGISTROS GUARDADOS, REGRESANDO AL MENU"
                 DISPLAY SPACE
                 PERFORM 1000-MENU-PROGRAMA
           END-IF.








       END PROGRAM YOUR-PROGRAM-NAME.
