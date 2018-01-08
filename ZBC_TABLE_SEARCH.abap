REPORT  zcs_table_search LINE-SIZE 1023.

TYPE-POOLS: abap.

TABLES: hier_mess,
        dd02vv.

TYPES: tt_indexes TYPE TABLE OF i.


CONSTANTS: gc_show_progress_step    TYPE i VALUE 1322.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
SELECT-OPTIONS: gt_tabn  FOR dd02vv-tabname DEFAULT 'Z*' OPTION CP,
                gt_cflg  FOR dd02vv-contflag.
SELECTION-SCREEN END OF BLOCK b01.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.
SELECT-OPTIONS: gt_text  FOR hier_mess-msgtxt.
PARAMETERS:     gv_maxr  TYPE i,
                gv_casi  AS CHECKBOX USER-COMMAND chn.
SELECTION-SCREEN END OF BLOCK b02.

INITIALIZATION.
  gt_cflg-sign   = 'I'.
  gt_cflg-option = 'EQ'.
  gt_cflg-low    = 'C'.
  APPEND gt_cflg.
  gt_cflg-low    = 'E'.
  APPEND gt_cflg.


AT SELECTION-SCREEN OUTPUT.
  PERFORM translate_text_if_casi.

START-OF-SELECTION.

  CHECK gt_text IS NOT INITIAL.

  PERFORM start.



*&---------------------------------------------------------------------*
*&      Form  start
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM start.
  DATA: lv_first          TYPE abap_bool,
        lv_text           TYPE char200,
        lv_compn          TYPE i,
        lv_length_total   TYPE i,
        lv_length_temp    TYPE i,
        lv_count_total    TYPE i,
        lv_count_found    TYPE i,
        lt_indexes        TYPE tt_indexes,
        lt_tables         TYPE TABLE OF dd02vv,
        lrs_table         TYPE REF TO data.

  FIELD-SYMBOLS: <fss_tables>       LIKE LINE OF lt_tables,
                 <fss_table>        TYPE any,
                 <fsv_table_comp>   TYPE any.

  DEFINE lm_unassign.
    if &1 is assigned.
      unassign &1.
    endif.
  END-OF-DEFINITION.

  DEFINE lm_check_in_range. " &struct 'COMPNAME' &range
    if &3 is not initial.
      lm_unassign <fsv_table_comp>.
      assign component &2 of structure &1 to <fsv_table_comp>.
      check <fsv_table_comp> is assigned.
      check <fsv_table_comp> in &3[].
    endif.
  END-OF-DEFINITION.

  PERFORM translate_text_if_casi.

  SELECT *
    FROM dd02vv
    INTO TABLE lt_tables
   WHERE tabname IN gt_tabn
     AND as4local = 'A'
     AND tabclass = 'TRANSP'
     AND contflag IN gt_cflg.

  CLEAR: lv_count_total, lv_count_found.

  SORT lt_tables BY tabname.
  DELETE ADJACENT DUPLICATES FROM lt_tables COMPARING tabname.

  LOOP AT lt_tables ASSIGNING <fss_tables>.
    CREATE DATA lrs_table TYPE (<fss_tables>-tabname).

    ASSIGN lrs_table->* TO <fss_table>.

    CREATE DATA lrs_table TYPE (<fss_tables>-tabname).
    ASSIGN lrs_table->* TO <fss_table>.

    lv_first = abap_true.

    SELECT *
      FROM (<fss_tables>-tabname)
      INTO <fss_table>.

      lv_count_total = lv_count_total + 1.

      lv_compn = lv_count_total MOD gc_show_progress_step.
      IF lv_compn IS INITIAL.
        MESSAGE s102(zcs) WITH <fss_tables>-tabname lv_count_found lv_count_total INTO lv_text.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = 100
            text       = lv_text.
      ENDIF.


      PERFORM find_text USING <fss_table>
                     CHANGING lt_indexes.

      CHECK lt_indexes IS NOT INITIAL.

      IF lv_first = abap_true.
        PERFORM calculate_total_length USING <fss_table> CHANGING lv_length_total.

        lv_length_temp = lv_length_total.

        IF lv_length_temp < 120.
          lv_length_temp = 120.
        ENDIF.

        FORMAT COLOR COL_GROUP.
        ULINE AT (lv_length_temp).
        NEW-LINE.
        WRITE: sy-vline, <fss_tables>-tabname, <fss_tables>-ddtext, AT lv_length_temp sy-vline.
        NEW-LINE.
        ULINE AT (lv_length_temp).

        PERFORM show_heading USING <fss_tables>-tabname <fss_table> lv_length_total.

        NEW-LINE.
        ULINE AT (lv_length_total).

        lv_first = abap_false.
      ENDIF.

      NEW-LINE.
      lv_compn = 1.
      DO.
        ASSIGN COMPONENT lv_compn OF STRUCTURE <fss_table> TO <fsv_table_comp>.
        IF sy-subrc IS NOT INITIAL.
          EXIT.
        ENDIF.

        READ TABLE lt_indexes TRANSPORTING NO FIELDS WITH KEY table_line = lv_compn.
        IF sy-subrc IS INITIAL.
          FORMAT COLOR COL_POSITIVE.
        ELSE.
          FORMAT COLOR COL_NORMAL.
        ENDIF.

        IF lv_compn = 1.
          WRITE sy-vline.
        ENDIF.

        WRITE: <fsv_table_comp>, sy-vline.

        lv_compn = lv_compn + 1.
      ENDDO.

      lv_count_found = lv_count_found + 1.

      IF gv_maxr IS NOT INITIAL AND lv_count_found >= gv_maxr.
        EXIT.
      ENDIF.

    ENDSELECT.

    IF lv_first = abap_false.
      NEW-LINE.
      ULINE AT (lv_length_total).
      SKIP 3.
    ENDIF.

    IF gv_maxr IS NOT INITIAL AND lv_count_found >= gv_maxr.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "start


*&---------------------------------------------------------------------*
*&      Form  show_heading
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IS_STRUCT  text
*----------------------------------------------------------------------*
FORM show_heading USING iv_struct_name  TYPE tabname
                        is_struct       TYPE any
                        iv_length_total TYPE i.
  DATA: lv_length        TYPE i,
        lv_fname(1024)   TYPE c,
        lt_fcata         TYPE lvc_t_fcat.

  FIELD-SYMBOLS: <fss_fcata>        LIKE LINE OF lt_fcata,
                 <fsv_struct_comp>  TYPE any.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = iv_struct_name
      i_client_never_display = 'X'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_fcata
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  FORMAT COLOR COL_HEADING.
  NEW-LINE.
  LOOP AT lt_fcata ASSIGNING <fss_fcata>.

    ASSIGN COMPONENT sy-tabix OF STRUCTURE is_struct TO <fsv_struct_comp>.

    AT FIRST.
      WRITE sy-vline.
    ENDAT.

    lv_fname  = <fss_fcata>-fieldname.
    DESCRIBE FIELD <fsv_struct_comp> OUTPUT-LENGTH lv_length.

    WRITE: lv_fname(lv_length) QUICKINFO <fss_fcata>-reptext, sy-vline.

  ENDLOOP.
  NEW-LINE.

ENDFORM.                    "show_heading


*&---------------------------------------------------------------------*
*&      Form  calculate_total_length
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IS_STRUCT     text
*      -->CV_TOTAL_LEN  text
*----------------------------------------------------------------------*
FORM calculate_total_length USING is_struct         TYPE any
                         CHANGING cv_length_total   TYPE i.
  DATA:          lv_compi  TYPE i,
                 lv_length TYPE i.
  FIELD-SYMBOLS: <fsv_struct_comp>  TYPE any.

  lv_compi = 1.
  cv_length_total = 1.
  DO.
    ASSIGN COMPONENT lv_compi OF STRUCTURE is_struct TO <fsv_struct_comp>.
    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ENDIF.
    DESCRIBE FIELD <fsv_struct_comp> OUTPUT-LENGTH lv_length.
    cv_length_total = cv_length_total + lv_length + 3.
    lv_compi = lv_compi + 1.
  ENDDO.
ENDFORM.                    "calculate_total_length


*&---------------------------------------------------------------------*
*&      Form  find_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IS_STRUCT  text
*      -->CT_INDEXES text
*----------------------------------------------------------------------*
FORM find_text USING is_struct   TYPE any
            CHANGING ct_indexes  TYPE tt_indexes.
  DATA: lv_compn       TYPE i,
        lv_contn(4096) TYPE c.

  FIELD-SYMBOLS: <fsv_table_comp> TYPE any.

  REFRESH ct_indexes.

  lv_compn = 1.
  DO.
    ASSIGN COMPONENT lv_compn OF STRUCTURE is_struct TO <fsv_table_comp>.
    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ENDIF.

    WRITE <fsv_table_comp> TO lv_contn.
    CONDENSE lv_contn.

    IF gv_casi IS NOT INITIAL.
      TRANSLATE lv_contn TO UPPER CASE.
    ENDIF.

    FORMAT COLOR COL_NORMAL.
    IF lv_contn IN gt_text. "CP gv_text OR lv_contn EQ gv_text.
      INSERT lv_compn INTO TABLE ct_indexes.
    ENDIF.
    lv_compn = lv_compn + 1.
  ENDDO.

ENDFORM.                    "find_text


*&---------------------------------------------------------------------*
*&      Form  translate_text_if_casi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM translate_text_if_casi.
  IF gv_casi IS INITIAL.
    LOOP AT gt_text.
      TRANSLATE gt_text-low TO UPPER CASE.
      TRANSLATE gt_text-high TO UPPER CASE.
      MODIFY gt_text.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "translate_text_if_casi
