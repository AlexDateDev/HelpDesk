&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          nomi             PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&SCOPED-DEFINE tb   N1272

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES N1272 N1263 N1270 N1271 N1261 N1279 N1260

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 N1272.N1272-COD N1272.N1263-COD ~
N1263.N1263-NOM N1272.N1270-COD N1270.N1270-NOM N1272.N1271-COD ~
N1261.N1261-NOM N1272.N1279-COD N1260.N1260-NOM N1272.N1272-DCR ~
N1272.N1272-DCI N1272.N1267-COD N1272.M1008-CRE N1272.M1008-NULO ~
N1272.N1268-CODFIN N1272.N1268-ESTFIN N1272.N1272-DFI N1272.N1272-DIN ~
N1272.N1272-NULO 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH N1272 NO-LOCK, ~
      EACH N1263 OF N1272 NO-LOCK, ~
      EACH N1270 OF N1272 NO-LOCK, ~
      EACH N1271 OF N1272 NO-LOCK, ~
      EACH N1261 OF N1271 NO-LOCK, ~
      EACH N1279 OF N1272 NO-LOCK, ~
      EACH N1260 OF N1279 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH N1272 NO-LOCK, ~
      EACH N1263 OF N1272 NO-LOCK, ~
      EACH N1270 OF N1272 NO-LOCK, ~
      EACH N1271 OF N1272 NO-LOCK, ~
      EACH N1261 OF N1271 NO-LOCK, ~
      EACH N1279 OF N1272 NO-LOCK, ~
      EACH N1260 OF N1279 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 N1272 N1263 N1270 N1271 N1261 N1279 ~
N1260
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 N1272
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 N1263
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-1 N1270
&Scoped-define FOURTH-TABLE-IN-QUERY-BROWSE-1 N1271
&Scoped-define FIFTH-TABLE-IN-QUERY-BROWSE-1 N1261
&Scoped-define SIXTH-TABLE-IN-QUERY-BROWSE-1 N1279
&Scoped-define SEVENTH-TABLE-IN-QUERY-BROWSE-1 N1260


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS wM1008-NULO wN1271-COD wN1268-ESTFIN ~
wN1272-COD BtnBuscar wN1272-DCR BtnHow-3 wN1279-COD wN1272-DIN BtnHow ~
wN1263-COD BtrnFiltrar wN1272-DCI BtnHow-4 wN1267-COD wN1272-DFI BtnHow-2 ~
wN1270-COD BtrnFiltrar-2 wN1268-CODFIN wM1008-CRE wN1272-NULO BROWSE-1 ~
BtnCrear BtnActualizar BtnLimpiar BtnReacargae BtnEliminar BtnEditar ~
BtnIrPrimero BtnIrSiguiente BtnIrAnterior BtnIrUltimo 
&Scoped-Define DISPLAYED-OBJECTS wM1008-NULO wN1271-COD wN1268-ESTFIN ~
wN1272-COD wN1272-DCR wN1279-COD wN1272-DIN wN1263-COD wN1272-DCI ~
wN1267-COD wN1272-DFI wN1270-COD wN1268-CODFIN wM1008-CRE wN1272-NULO 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnActualizar 
     LABEL "Actualizar" 
     SIZE 13 BY 1.86.

DEFINE BUTTON BtnBuscar 
     LABEL "Buscar" 
     SIZE 13 BY 1.

DEFINE BUTTON BtnCrear 
     LABEL "Crear" 
     SIZE 13 BY 1.86.

DEFINE BUTTON BtnEditar 
     LABEL "Editar" 
     SIZE 13 BY 1.86.

DEFINE BUTTON BtnEliminar 
     LABEL "Eliminar" 
     SIZE 13 BY 1.86.

DEFINE BUTTON BtnHow 
     LABEL "Hoy" 
     SIZE 8 BY 1.14.

DEFINE BUTTON BtnHow-2 
     LABEL "Hoy" 
     SIZE 8 BY 1.14.

DEFINE BUTTON BtnHow-3 
     LABEL "Hoy" 
     SIZE 8 BY 1.14.

DEFINE BUTTON BtnHow-4 
     LABEL "Hoy" 
     SIZE 8 BY 1.14.

DEFINE BUTTON BtnIrAnterior 
     LABEL "Anterior" 
     SIZE 13 BY 1.86.

DEFINE BUTTON BtnIrPrimero 
     LABEL "Primero" 
     SIZE 13 BY 1.86.

DEFINE BUTTON BtnIrSiguiente 
     LABEL "Siguiente" 
     SIZE 13 BY 1.86.

DEFINE BUTTON BtnIrUltimo 
     LABEL "Último" 
     SIZE 13 BY 1.86.

DEFINE BUTTON BtnLimpiar 
     LABEL "Limpiar" 
     SIZE 13 BY 1.86.

DEFINE BUTTON BtnReacargae 
     LABEL "Recargar" 
     SIZE 13 BY 1.86.

DEFINE BUTTON BtrnFiltrar 
     LABEL "Filtrar por contrtata" 
     SIZE 20 BY 1.

DEFINE BUTTON BtrnFiltrar-2 
     LABEL "Filtrar por solicitud" 
     SIZE 20 BY 1.

DEFINE VARIABLE wM1008-CRE AS CHARACTER FORMAT "X(26)":U 
     LABEL "M1008-CRE (Creador)" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE wM1008-NULO AS CHARACTER FORMAT "X(26)":U 
     LABEL "M1008-NULO (Anulador)" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE wN1263-COD AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "N1263-COD (Contrata)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE wN1267-COD AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "wN1267-COD (Requisito)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE wN1268-CODFIN AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "wN1268-CODFIN" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE wN1268-ESTFIN AS CHARACTER FORMAT "X(256)":U 
     LABEL "wN1268-ESTFIN" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE wN1270-COD AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "wN1270-COD (Solicitud)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE wN1271-COD AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "wN1271-COD (Riesgo Sol)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE wN1272-COD AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "N1272-COD (Código)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE wN1272-DCI AS DATETIME FORMAT "99/99/9999 HH:MM:SS":U 
     LABEL "wN1272-DCI (F. Cierre)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE wN1272-DCR AS DATETIME FORMAT "99/99/9999 HH:MM:SS":U 
     LABEL "wN1272-DCR (F. Creación)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE wN1272-DFI AS DATETIME FORMAT "99/99/9999 HH:MM:SS":U 
     LABEL "wN1272-DFI (F. fin)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE wN1272-DIN AS DATETIME FORMAT "99/99/9999 HH:MM:SS":U 
     LABEL "wN1272-DIN (F. inicio)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE wN1279-COD AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "wN1279-COD (Equipo Sol)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE wN1272-NULO AS LOGICAL INITIAL no 
     LABEL "N1272-NULO" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      N1272, 
      N1263, 
      N1270, 
      N1271, 
      N1261, 
      N1279, 
      N1260 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      N1272.N1272-COD COLUMN-LABEL "COD" FORMAT ">>>>>>9":U WIDTH 5.2
      N1272.N1263-COD COLUMN-LABEL "N1263" FORMAT ">>>>>>9":U WIDTH 6.2
      N1263.N1263-NOM COLUMN-LABEL "Contrata" FORMAT "x(80)":U
            WIDTH 20.2
      N1272.N1270-COD COLUMN-LABEL "N1270" FORMAT ">>>>>>9":U WIDTH 6.2
      N1270.N1270-NOM COLUMN-LABEL "Solciitud" FORMAT "x(80)":U
            WIDTH 30.2
      N1272.N1271-COD COLUMN-LABEL "N1271" FORMAT ">>>>>>9":U WIDTH 6.2
      N1261.N1261-NOM COLUMN-LABEL "Actividad" FORMAT "x(80)":U
            WIDTH 27.2
      N1272.N1279-COD COLUMN-LABEL "N1279" FORMAT ">>>>>>9":U WIDTH 6.2
      N1260.N1260-NOM COLUMN-LABEL "Equipo" FORMAT "x(80)":U WIDTH 23.2
      N1272.N1272-DCR FORMAT "99/99/9999 HH:MM:SS":U WIDTH 14.6
      N1272.N1272-DCI FORMAT "99/99/9999 HH:MM:SS":U WIDTH 10.6
      N1272.N1267-COD COLUMN-LABEL "N1267" FORMAT ">>>>>9":U WIDTH 6.4
      N1272.M1008-CRE FORMAT "x(80)":U WIDTH 14.2
      N1272.M1008-NULO FORMAT "x(80)":U WIDTH 12.2
      N1272.N1268-CODFIN FORMAT ">>>>>>9":U WIDTH 10.2
      N1272.N1268-ESTFIN FORMAT "x(80)":U WIDTH 13.2
      N1272.N1272-DFI FORMAT "99/99/9999 HH:MM:SS":U WIDTH 14.2
      N1272.N1272-DIN FORMAT "99/99/9999 HH:MM:SS":U WIDTH 13.2
      N1272.N1272-NULO FORMAT "yes/no":U WIDTH 5.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE SEPARATORS NO-VALIDATE NO-TAB-STOP SIZE 264 BY 26.67 ROW-HEIGHT-CHARS .65 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     wM1008-NULO AT ROW 1.71 COL 223 COLON-ALIGNED WIDGET-ID 108
     wN1271-COD AT ROW 1.95 COL 97 COLON-ALIGNED WIDGET-ID 94
     wN1268-ESTFIN AT ROW 1.95 COL 149 COLON-ALIGNED WIDGET-ID 102
     wN1272-COD AT ROW 2.19 COL 30 COLON-ALIGNED WIDGET-ID 2
     BtnBuscar AT ROW 2.19 COL 49 WIDGET-ID 8
     wN1272-DCR AT ROW 3.14 COL 223 COLON-ALIGNED WIDGET-ID 116
     BtnHow-3 AT ROW 3.14 COL 256 WIDGET-ID 110
     wN1279-COD AT ROW 3.38 COL 97 COLON-ALIGNED WIDGET-ID 96
     wN1272-DIN AT ROW 3.38 COL 149 COLON-ALIGNED WIDGET-ID 18
     BtnHow AT ROW 3.38 COL 182 WIDGET-ID 42
     wN1263-COD AT ROW 3.62 COL 30 COLON-ALIGNED WIDGET-ID 56
     BtrnFiltrar AT ROW 3.62 COL 49 WIDGET-ID 84
     wN1272-DCI AT ROW 4.57 COL 223 COLON-ALIGNED WIDGET-ID 114
     BtnHow-4 AT ROW 4.57 COL 256 WIDGET-ID 112
     wN1267-COD AT ROW 4.81 COL 97 COLON-ALIGNED WIDGET-ID 98
     wN1272-DFI AT ROW 4.81 COL 149 COLON-ALIGNED WIDGET-ID 106
     BtnHow-2 AT ROW 4.81 COL 182 WIDGET-ID 104
     wN1270-COD AT ROW 5.05 COL 30 COLON-ALIGNED WIDGET-ID 90
     BtrnFiltrar-2 AT ROW 5.05 COL 49 WIDGET-ID 88
     wN1268-CODFIN AT ROW 6.24 COL 97 COLON-ALIGNED WIDGET-ID 100
     wM1008-CRE AT ROW 6.24 COL 149 COLON-ALIGNED WIDGET-ID 22
     wN1272-NULO AT ROW 6.24 COL 225 WIDGET-ID 26
     BROWSE-1 AT ROW 7.67 COL 6 WIDGET-ID 200
     BtnCrear AT ROW 35.29 COL 8 WIDGET-ID 10
     BtnActualizar AT ROW 35.29 COL 25 WIDGET-ID 28
     BtnLimpiar AT ROW 35.29 COL 42 WIDGET-ID 44
     BtnReacargae AT ROW 35.29 COL 59 WIDGET-ID 86
     BtnEliminar AT ROW 35.29 COL 97 WIDGET-ID 40
     BtnEditar AT ROW 35.29 COL 139 WIDGET-ID 38
     BtnIrPrimero AT ROW 35.29 COL 203 WIDGET-ID 30
     BtnIrSiguiente AT ROW 35.29 COL 220 WIDGET-ID 32
     BtnIrAnterior AT ROW 35.29 COL 237 WIDGET-ID 36
     BtnIrUltimo AT ROW 35.29 COL 254 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.1
         SIZE 271.8 BY 37.14 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "N1272 - Coodinaciones"
         HEIGHT             = 36.95
         WIDTH              = 272.2
         MAX-HEIGHT         = 38.1
         MAX-WIDTH          = 280.6
         VIRTUAL-HEIGHT     = 38.1
         VIRTUAL-WIDTH      = 280.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 wN1272-NULO DEFAULT-FRAME */
ASSIGN 
       BROWSE-1:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE
       BROWSE-1:COLUMN-MOVABLE IN FRAME DEFAULT-FRAME         = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "nomi.N1272,nomi.N1263 OF nomi.N1272,nomi.N1270 OF nomi.N1272,nomi.N1271 OF nomi.N1272,nomi.N1261 OF nomi.N1271,nomi.N1279 OF nomi.N1272,nomi.N1260 OF nomi.N1279"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > nomi.N1272.N1272-COD
"N1272.N1272-COD" "COD" ? "integer" ? ? ? ? ? ? no ? no no "5.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > nomi.N1272.N1263-COD
"N1272.N1263-COD" "N1263" ? "integer" ? ? ? ? ? ? no ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > nomi.N1263.N1263-NOM
"N1263.N1263-NOM" "Contrata" ? "character" ? ? ? ? ? ? no ? no no "20.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > nomi.N1272.N1270-COD
"N1272.N1270-COD" "N1270" ? "integer" ? ? ? ? ? ? no ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > nomi.N1270.N1270-NOM
"N1270.N1270-NOM" "Solciitud" ? "character" ? ? ? ? ? ? no ? no no "30.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > nomi.N1272.N1271-COD
"N1272.N1271-COD" "N1271" ? "integer" ? ? ? ? ? ? no ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > nomi.N1261.N1261-NOM
"N1261.N1261-NOM" "Actividad" ? "character" ? ? ? ? ? ? no ? no no "27.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > nomi.N1272.N1279-COD
"N1272.N1279-COD" "N1279" ? "integer" ? ? ? ? ? ? no ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > nomi.N1260.N1260-NOM
"N1260.N1260-NOM" "Equipo" ? "character" ? ? ? ? ? ? no ? no no "23.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > nomi.N1272.N1272-DCR
"N1272.N1272-DCR" ? ? "datetime" ? ? ? ? ? ? no ? no no "14.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > nomi.N1272.N1272-DCI
"N1272.N1272-DCI" ? ? "datetime" ? ? ? ? ? ? no ? no no "10.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > nomi.N1272.N1267-COD
"N1272.N1267-COD" "N1267" ? "integer" ? ? ? ? ? ? no ? no no "6.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > nomi.N1272.M1008-CRE
"N1272.M1008-CRE" ? ? "character" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > nomi.N1272.M1008-NULO
"N1272.M1008-NULO" ? ? "character" ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > nomi.N1272.N1268-CODFIN
"N1272.N1268-CODFIN" ? ? "integer" ? ? ? ? ? ? no ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > nomi.N1272.N1268-ESTFIN
"N1272.N1268-ESTFIN" ? ? "character" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > nomi.N1272.N1272-DFI
"N1272.N1272-DFI" ? ? "datetime" ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > nomi.N1272.N1272-DIN
"N1272.N1272-DIN" ? ? "datetime" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > nomi.N1272.N1272-NULO
"N1272.N1272-NULO" ? ? "logical" ? ? ? ? ? ? no ? no no "5.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* N1272 - Coodinaciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* N1272 - Coodinaciones */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnActualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnActualizar C-Win
ON CHOOSE OF BtnActualizar IN FRAME DEFAULT-FRAME /* Actualizar */
DO:
  DEFINE VARIABLE r-rowid AS ROWID       NO-UNDO.
  IF AVAIL {&tb} THEN DO:
    r-rowid = ROWID( {&tb} ).      
  END.
  
  RUN DoActualizar.
    RUN acc_limpiar_campos.  
  RUN Acc_cargar_query.
  
  
  IF r-rowid <> ? THEN DO:
    FIND FIRST {&tb} WHERE ROWID({&tb}) = r-rowid NO-LOCK 
                   NO-ERROR. 
                           
    IF AVAIL {&tb} THEN DO:
        RUN Acc_posicionar_grid.        
    END.
    
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnBuscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnBuscar C-Win
ON CHOOSE OF BtnBuscar IN FRAME DEFAULT-FRAME /* Buscar */
DO:
  RUN DoBuscar.
  RUN acc_posicionar_grid.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCrear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCrear C-Win
ON CHOOSE OF BtnCrear IN FRAME DEFAULT-FRAME /* Crear */
DO:
  RUN DoCrear.
  
  DEFINE VARIABLE r AS ROWID       NO-UNDO.
  IF AVAIL {&tb} THEN DO:
       r= ROWID({&tb}).              
  END.
  RUN acc_cargar_query.
  RUN acc_limpiar_campos.
  RUN acc_display_campos.
  
  FIND FIRST {&tb} WHERE ROWID( {&tb}) = r 
                 NO-LOCK 
                 NO-ERROR. 
                          
  IF AVAIL {&tb} THEN DO:
    RUN acc_posicionar_grid.
  END.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnEditar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnEditar C-Win
ON CHOOSE OF BtnEditar IN FRAME DEFAULT-FRAME /* Editar */
DO:
  RUN DoEditar.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnEliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnEliminar C-Win
ON CHOOSE OF BtnEliminar IN FRAME DEFAULT-FRAME /* Eliminar */
DO:
  
  
  DEFINE VARIABLE r-rowid AS ROWID       NO-UNDO.
  IF AVAIL {&tb} THEN DO:
    r-rowid = ROWID( {&tb} ).      
  END.
  
  IF r-rowid = ? THEN DO:
    MESSAGE "No hay registro seleccionado" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.      
    RETURN.
  END.
  
  FIND FIRST {&tb}  WHERE ROWID({&tb}) = r-rowid
                    EXCLUSIVE-LOCK 
                    NO-ERROR
                    NO-WAIT.
                          
  IF AVAIL {&tb} THEN DO:
  
  // Pregunta
    MESSAGE "¿Eliminar registro?" VIEW-AS ALERT-BOX 
        QUESTION BUTTONS YES-NO
        TITLE "Eliminar" 
        UPDATE lChoice AS LOGICAL.
    
    CASE lChoice:
        WHEN TRUE THEN /* Yes */ DO:
            DELETE {&tb}.
            MESSAGE "OK" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.            //
            RUN acc_limpiar_campos.
            RUN Acc_cargar_query.            
        END.
        WHEN FALSE THEN /* No */ DO:                    
                //
            RETURN.
        END.
    END CASE.
    
    
  END.
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnHow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnHow C-Win
ON CHOOSE OF BtnHow IN FRAME DEFAULT-FRAME /* Hoy */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN  wN1272-DIN.
    
    wN1272-DIN = NOW.
    
    DISPLAY wN1272-DIN.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnHow-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnHow-2 C-Win
ON CHOOSE OF BtnHow-2 IN FRAME DEFAULT-FRAME /* Hoy */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN  wN1272-DFI.
    
    wN1272-DFI = NOW.
    
    DISPLAY wN1272-DFI.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnHow-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnHow-3 C-Win
ON CHOOSE OF BtnHow-3 IN FRAME DEFAULT-FRAME /* Hoy */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN  wN1272-DCR.
    
    wN1272-DCR = NOW.
    
    DISPLAY wN1272-DCR.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnHow-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnHow-4 C-Win
ON CHOOSE OF BtnHow-4 IN FRAME DEFAULT-FRAME /* Hoy */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN  wN1272-DCI.
    
    wN1272-DCI = NOW.
    
    DISPLAY wN1272-DCI.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnIrAnterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnIrAnterior C-Win
ON CHOOSE OF BtnIrAnterior IN FRAME DEFAULT-FRAME /* Anterior */
DO:
  RUN DoIrAnterior.
  
    RUN Acc_posicionar_grid.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnIrPrimero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnIrPrimero C-Win
ON CHOOSE OF BtnIrPrimero IN FRAME DEFAULT-FRAME /* Primero */
DO:
  RUN DoIrPrimero.

  RUN Acc_posicionar_grid.
   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnIrSiguiente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnIrSiguiente C-Win
ON CHOOSE OF BtnIrSiguiente IN FRAME DEFAULT-FRAME /* Siguiente */
DO:
    RUN DoIrSiguiente.
  
    RUN Acc_posicionar_grid.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnIrUltimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnIrUltimo C-Win
ON CHOOSE OF BtnIrUltimo IN FRAME DEFAULT-FRAME /* Último */
DO:
  RUN DoIrUltimo.
    RUN Acc_posicionar_grid.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnLimpiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnLimpiar C-Win
ON CHOOSE OF BtnLimpiar IN FRAME DEFAULT-FRAME /* Limpiar */
DO:
  RUN acc_limpiar_campos.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnReacargae
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnReacargae C-Win
ON CHOOSE OF BtnReacargae IN FRAME DEFAULT-FRAME /* Recargar */
DO:
  RUN acc_cargar_query.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtrnFiltrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtrnFiltrar C-Win
ON CHOOSE OF BtrnFiltrar IN FRAME DEFAULT-FRAME /* Filtrar por contrtata */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      
            
      RUN acc_cargar_query.
      
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtrnFiltrar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtrnFiltrar-2 C-Win
ON CHOOSE OF BtrnFiltrar-2 IN FRAME DEFAULT-FRAME /* Filtrar por solicitud */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      
            
      RUN acc_cargar_query.
      
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN Acc_Limpiar_campos.
  RUN Acc_cargar_query.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE acc_asignar_campos C-Win 
PROCEDURE acc_asignar_campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


DO WITH FRAME {&FRAME-NAME}:
    
    /* Solo asignamos los campos de la ventana a moria*/
    
    ASSIGN  w{&tb}-COD
            wN1263-COD            
            wN1270-COD
            wN1271-COD
            wN1279-COD
            wN1267-COD
            wN1268-CODFIN            
            wN1268-ESTFIN
            wN1268-ESTFIN
            wN1272-DIN
            wN1272-DFI
            wM1008-CRE
            wM1008-NULO
            wN1272-DCR
            wN1272-DCI
            wN1272-nulo
            .
            
            
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE acc_cargar_campos_de_la_db C-Win 
PROCEDURE acc_cargar_campos_de_la_db :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
    
    /* De la base de datos a los campos */
    
    
    ASSIGN  w{&tb}-COD  =   {&tb}.{&tb}-cod      
            wN1263-COD      =   {&tb}.n1263-cod
            wN1270-COD      =   {&tb}.n1270-cod
            wN1271-COD      =   {&tb}.n1271-cod
            wN1279-COD      =   {&tb}.n1279-cod
            wN1267-COD      =   {&tb}.n1267-cod
            wN1268-CODFIN   =   {&tb}.n1268-codfin
            wN1268-ESTFIN   =   {&tb}.n1268-estfin
            wN1272-DIN      =   {&tb}.n1272-din
            wN1272-DFI      =   {&tb}.n1272-dfi
            wM1008-CRE      =   {&tb}.m1008-cre
            wM1008-NULO     =   {&tb}.m1008-nulo
            wN1272-DCR      =   {&tb}.n1272-dcr
            wN1272-DCI      =   {&tb}.n1272-dci
            wN1272-NULO     =   {&tb}.n1272-nulo
            .          
                
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE acc_cargar_query C-Win 
PROCEDURE acc_cargar_query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:

ASSIGN  wN1263-COD 
            wN1270-COD 
            .
    OPEN QUERY {&browse-name} 
        FOR EACH {&tb}
        WHERE ({&tb}.n1263-cod = IF wN1263-COD = 0 THEN {&tb}.n1263-cod ELSE wN1263-COD)
        AND ({&tb}.n1270-cod = IF wN1270-COD = 0 THEN {&tb}.n1270-cod ELSE wN1270-COD)
        NO-LOCK,
        FIRST N1263 WHERE N1263.n1263-cod = {&tb}.n1263-cod OUTER-JOIN NO-LOCK,
        FIRST N1270 WHERE N1270.n1270-cod = {&tb}.n1270-cod OUTER-JOIN NO-LOCK,
        FIRST N1271 WHERE N1271.n1271-cod = {&tb}.n1271-cod OUTER-JOIN NO-LOCK,
        FIRST N1261 WHERE N1261.n1261-cod = N1271.n1261-cod OUTER-JOIN NO-LOCK,
        FIRST N1279 WHERE N1279.n1279-cod = {&tb}.n1279-cod OUTER-JOIN NO-LOCK,
        FIRST N1260 WHERE N1260.n1260-cod = N1279.n1260-cod OUTER-JOIN NO-LOCK
                            
        BY {&tb}.{&tb}-cod.
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE acc_display_campos C-Win 
PROCEDURE acc_display_campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
    
    /* Hacer display de todos los campos */
    
    DISPLAY w{&tb}-COD
            wN1263-COD            
            wN1270-COD
            wN1271-COD
            wN1279-COD            
            wN1267-COD
            wN1268-CODFIN            
            wN1268-ESTFIN
            wN1268-ESTFIN
            wN1272-DIN
            wN1272-DFI
            wM1008-CRE
            wM1008-NULO
            wN1272-DCR
            wN1272-DCI
            wN1272-NULO.
                        .
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE acc_guardar_campos_a_db C-Win 
PROCEDURE acc_guardar_campos_a_db :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

        
            
    ASSIGN  {&tb}.{&tb}-cod     = w{&tb}-COD 
            {&tb}.n1263-cod     = wN1263-COD
            {&tb}.n1270-cod     = wN1270-COD
            {&tb}.n1271-cod     = wN1271-COD
            {&tb}.n1279-cod     = wN1279-COD
            {&tb}.n1267-cod      = wN1267-COD
            {&tb}.n1268-codfin  = wN1268-CODFIN
            {&tb}.n1268-estfin  = wN1268-ESTFIN
            {&tb}.n1272-din     = wN1272-DIN
            {&tb}.n1272-dfi     = wN1272-DFI
            {&tb}.m1008-cre     = wM1008-CRE
            {&tb}.m1008-nulo    = wM1008-NULO
            {&tb}.n1272-dcr     = wN1272-DCR
            {&tb}.n1272-dci     = wN1272-DCI   
            {&tb}.n1272-nulo    =  wN1272-NULO 
            
            
            .
          
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE acc_limpiar_campos C-Win 
PROCEDURE acc_limpiar_campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/



DO WITH FRAME {&FRAME-NAME}:
    
    /* Solo asignamos los campos de la ventana a moria*/
    
    ASSIGN  w{&tb}-COD      = 0 
            wN1263-COD      = 0
            wN1270-COD      = 0
            wN1271-COD      = 0
            wN1279-COD      = 0
            wN1267-COD      = 0
            wN1268-CODFIN   = 0
            wN1268-ESTFIN   = ""
            wN1272-DIN      = ?
            wN1272-DFI      = ?
            wM1008-CRE      = ""
            wM1008-NULO     = ""
            wN1272-DCR      = ?
            wN1272-DCI      = ?
            wN1272-NULO = FALSE
            
            .
            
    RUN acc_display_campos.            
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE acc_posicionar_grid C-Win 
PROCEDURE acc_posicionar_grid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL {&tb} THEN DO:
        REPOSITION {&browse-name } TO ROWID (ROWID({&tb})) NO-ERROR.    
    END.
    
END.
    
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoActualizar C-Win 
PROCEDURE DoActualizar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
    
    RUN Acc_Asignar_campos.
    
    IF  w{&tb}-COD = 0 THEN DO:
        MESSAGE "Falta código" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.        
        RETURN.
    END.
    
    FIND FIRST {&tb}    WHERE   {&tb}.{&tb}-cod = w{&tb}-cod 
                        EXCLUSIVE-LOCK 
                        NO-ERROR
                        NO-WAIT.
                            
    IF AVAIL {&tb} THEN DO:
        RUN Acc_Guardar_campos_a_db.
        MESSAGE "OK" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    ELSE DO:    
        MESSAGE  "Registro no encontrado"  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    
    RELEASE {&tb}.
    
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoBuscar C-Win 
PROCEDURE DoBuscar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN  w{&tb}-cod.
    
    IF w{&tb}-cod = 0 THEN DO:
        MESSAGE "No existe código" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.        
        RETURN.
    END.
    
    FIND FIRST {&tb}    WHERE {&tb}.{&tb}-cod = w{&tb}-cod
                        NO-LOCK 
                        NO-ERROR. 
                        
    IF AVAIL {&tb} THEN DO:
        RUN Acc_Cargar_campos_de_la_db.
        RUN Acc_Display_campos. 
        MESSAGE "Ok" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    ELSE DO:
        MESSAGE  "Registro no encontrado" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.
    END.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoCrear C-Win 
PROCEDURE DoCrear :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
    
    RUN acc_asignar_campos.
    
    IF w{&tb}-COD = 0  THEN DO:
        MESSAGE "Falta código" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.        
        RETURN.
    END.
    
    FIND FIRST {&tb}    WHERE {&tb}.{&tb}-cod = w{&tb}-cod
                        NO-LOCK 
                        NO-ERROR. 
                            
    IF AVAIL {&tb} THEN DO:
        MESSAGE "Ya existe el registro con código " + STRING(w{&tb}-cod) VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.            
    END.
        
    CREATE  {&tb}.
    RUN acc_guardar_campos_a_db.
    
    
    MESSAGE "OK" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RUN acc_limpiar_campos.
    
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoEditar C-Win 
PROCEDURE DoEditar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Recorrer seleccionadas */
DEFINE VARIABLE n-i AS INTEGER     NO-UNDO.
DEFINE VARIABLE n-cod AS INT       NO-UNDO.
DO n-i = 1 TO BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS:
        BROWSE {&BROWSE-NAME}:FETCH-SELECTED-ROW(n-i).     
        FIND CURRENT {&tb} NO-LOCK NO-ERROR.
        n-cod = {&tb}.{&tb}-cod.
END.

IF n-cod <> 0 THEN DO:
    RUN acc_cargar_campos_de_la_db.
    RUN acc_display_campos.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoIrAnterior C-Win 
PROCEDURE DoIrAnterior :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND PREV {&tb} NO-LOCK NO-ERROR. 
                        
IF AVAIL {&tb} THEN DO:
    RUN Acc_Cargar_campos_de_la_db.
    RUN Acc_Display_campos.
END.
ELSE DO:
    MESSAGE "No hay mas registros" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoIrPrimero C-Win 
PROCEDURE DoIrPrimero :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST {&tb}    NO-LOCK 
                        NO-ERROR. 

    IF AVAIL {&tb} THEN DO:
        RUN Acc_Cargar_campos_de_la_db.
        RUN Acc_Display_campos.    
    END.
    ELSE DO:
        MESSAGE "No hay registro"  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    
    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoIrSiguiente C-Win 
PROCEDURE DoIrSiguiente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND NEXT {&tb} NO-LOCK NO-ERROR. 
                        
IF AVAIL {&tb} THEN DO:
    RUN Acc_Cargar_campos_de_la_db.
    RUN Acc_Display_campos.
END.
ELSE DO:
    MESSAGE "No hay mas registros" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoIrUltimo C-Win 
PROCEDURE DoIrUltimo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND LAST {&tb}    NO-LOCK 
                        NO-ERROR. 

    IF AVAIL {&tb} THEN DO:
        RUN Acc_Cargar_campos_de_la_db.
        RUN Acc_Display_campos.    
    END.
    ELSE DO:
        MESSAGE "No hay registro"  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY wM1008-NULO wN1271-COD wN1268-ESTFIN wN1272-COD wN1272-DCR wN1279-COD 
          wN1272-DIN wN1263-COD wN1272-DCI wN1267-COD wN1272-DFI wN1270-COD 
          wN1268-CODFIN wM1008-CRE wN1272-NULO 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE wM1008-NULO wN1271-COD wN1268-ESTFIN wN1272-COD BtnBuscar wN1272-DCR 
         BtnHow-3 wN1279-COD wN1272-DIN BtnHow wN1263-COD BtrnFiltrar 
         wN1272-DCI BtnHow-4 wN1267-COD wN1272-DFI BtnHow-2 wN1270-COD 
         BtrnFiltrar-2 wN1268-CODFIN wM1008-CRE wN1272-NULO BROWSE-1 BtnCrear 
         BtnActualizar BtnLimpiar BtnReacargae BtnEliminar BtnEditar 
         BtnIrPrimero BtnIrSiguiente BtnIrAnterior BtnIrUltimo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

