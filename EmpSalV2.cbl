      ******************************************************************
      * Author: Luis Angel Baez Nieto
      * Date: 24/01/24
      * Purpose: Learning project number 2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 3EMP-SAL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEES ASSIGN TO DISK.
           SELECT EMPSAL ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEES.
       01  EMP-REG.
           02 EMP-NOMI PIC 9(06).
           02 EMP-NOMB PIC X(20).
           02 EMP-DEPT PIC X(20).
           02 EMP-PERC PIC 9(05)V99.
           02 EMP-DEDU PIC 9(05).
       FD  EMPSAL.
       01  EMS-REG.
           02 EMS-NOMI PIC 9(06).
           02 EMS-NOMB PIC X(20).
           02 EMS-DEPT PIC X(20).
           02 EMS-PERC PIC 9(05)V99.
           02 EMS-DEDU PIC 9(05).
           02 EMS-SALA PIC S9(05)V99.
       WORKING-STORAGE SECTION.
       77  CONT-EMP PIC 9(3).
       77  EMP-EOF PIC 9.
       77  TOTAL-SALA PIC S9(7)V99.
       77  I PIC 9.
       01  DEPS-SALA-TAB.
           02 DEPTS-SALA-ROW OCCURS 6 TIMES.
               03 DEPT-SAL PIC S9(05)V99.
       01  DEPTS-NAME-TAB.
           02 DEPTS-NAMES-ROW OCCURS 6 TIMES.
               03 DEPT-NAME PIC X(03).
       PROCEDURE DIVISION.
      *------------------------ Main procedure ------------------------
       MAIN-PROCEDURE.
           PERFORM CARGAR-NOMBRES.
           OPEN INPUT EMPLOYEES.
           OPEN OUTPUT EMPSAL.
           PERFORM LEE-EMPLEADO.
           PERFORM GENERA-SALARIO UNTIL EMP-EOF = 1.
      *     PERFORM DISP-DEPT-SAL VARYING I FROM 1 BY 1 UNTIL I > 6.
           PERFORM DISP-DEPT-SAL-V2.
           DISPLAY "TOTAL DE EMPLEADOS PROCESADOS: ", CONT-EMP.
           DISPLAY "SUELDO TOTAL DE LA EMPRESA: ", TOTAL-SALA.

           CLOSE EMPLOYEES.
           CLOSE EMPSAL.
           STOP RUN.



      *------------------------ Load names ------------------------
       CARGAR-NOMBRES.
           MOVE "ADM" TO DEPT-NAME(1).
           MOVE "CON" TO DEPT-NAME(2).
           MOVE "MER" TO DEPT-NAME(3).
           MOVE "SIS" TO DEPT-NAME(4).
           MOVE "RH" TO DEPT-NAME(5).
           MOVE "TEC" TO DEPT-NAME(6).

      *------------------------ Read data ------------------------
       LEE-EMPLEADO.
           READ EMPLOYEES AT END MOVE 1 TO EMP-EOF.

      *---------------------- Calculo de salario ----------------------
       GENERA-SALARIO.
           MOVE EMP-REG TO EMS-REG.
           COMPUTE EMS-SALA = EMS-PERC - EMS-DEDU.
           PERFORM SALARIO-DEPT VARYING I FROM 1 BY 1 UNTIL I > 6.
           WRITE EMS-REG.
           ADD EMS-SALA TO TOTAL-SALA.
           ADD 1 TO CONT-EMP.
           PERFORM LEE-EMPLEADO.

      *------------------------ Add salary ------------------------
       SALARIO-DEPT.
           IF EMS-DEPT = DEPT-NAME(I)
               ADD EMS-SALA TO DEPT-SAL(I).

      *------------------------ DISP-SALARIO ------------------------
       DISP-DEPT-SAL.
           DISPLAY "SALARIO EN ", DEPT-NAME(I), ": ", DEPT-SAL(I).

      *------------------------ DISP-SALARIO V2 ------------------------
       DISP-DEPT-SAL-V2.
           DISPLAY "SALARIOS POR DEPARTAMENTOS:"
           DISPLAY "  ADMIN   |   CONTA   |   MERCA   |   SISTE",
                                        "   |   RR.HH   |   TECNO".
           DISPLAY DEPT-SAL(1)," | ",DEPT-SAL(2)," | ",DEPT-SAL(3)," | "
                                    ,DEPT-SAL(4)," | ",DEPT-SAL(5)," | "
                                    ,DEPT-SAL(6).
           DISPLAY " ".

       END PROGRAM 3EMP-SAL.
