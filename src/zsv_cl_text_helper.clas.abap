class ZSV_CL_TEXT_HELPER definition
  public
  final
  create public .

public section.






  types: BEGIN OF ty_s_text,
      rollname            type string,
      short_description   TYPE string,
      short_field_label   TYPE string,
      medium_field_label  TYPE string,
      long_field_label    TYPE string,
      heading_field_label TYPE string,
    END OF ty_s_text,
    ty_t_texts type STANDARD TABLE OF ty_s_text with EMPTY KEY.

  class-data:
    mt_texts TYPE ty_t_texts.
*  class-data:
*    mt_t100 TYPE STANDARD TABLE OF t100 WITH EMPTY KEY .
*  class-data:
*    MT_dd02t TYPE STANDARD TABLE OF dd02t WITH EMPTY KEY .

  class-methods GET_DD04T_L
    importing
      !IV_ROLLNAME type string
      !IV_LANGU type SY-LANGU optional
    returning
      value(RESULT) type STRING .
  class-methods GET_DD04T
    importing
      !IV_TYPE type string default 'S'
      !IV_ROLLNAME type string
      !IV_LANGU type SY-LANGU optional
    returning
      value(RESULT) type STRING .
  class-methods GET_DD02T
    importing
      !IV_TABNAME type STRING
      !IV_LANGU type SY-LANGU optional
    returning
      value(RESULT) type STRING .
  class-methods GET_T100
    importing
      !IV_ARBGB type STRING
      !IV_MSGNR type STRING
      !IV_LANGU type SY-LANGU optional
    returning
      value(RESULT) type STRING .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZSV_CL_TEXT_HELPER IMPLEMENTATION.


  METHOD GET_DD02T.

*    IF iv_langu IS INITIAL.
*      DATA(langu) = sy-langu.
*    ENDIF.
*
*    READ TABLE mt_dd02t INTO DATA(dd02t)
*             WITH KEY tabname   = iv_tabname
*                      ddlanguage = langu.
*    IF sy-subrc NE 0.
*
*      SELECT SINGLE * FROM dd02t INTO dd02t
*                        WHERE tabname   = iv_tabname
*                        AND   ddlanguage = langu.
*      IF sy-subrc = 0.
*        APPEND dd02t TO mt_dd02t.
*
*      ELSE.
*
*        SELECT SINGLE * FROM dd02t INTO dd02t
*                        WHERE tabname   = iv_tabname
*                        AND   ddlanguage = 'E'.
*        IF sy-subrc = 0.
*          APPEND dd02t TO mt_dd02t.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
*
*    IF dd02t IS NOT INITIAL.
*      result = dd02t-ddtext.
*    ELSE.
*      result = iv_tabname.
*    ENDIF.


  ENDMETHOD.


  METHOD get_dd04t.

    READ TABLE mt_texts
             WITH KEY rollname   = iv_rollname
             INTO DATA(text).

    IF sy-subrc NE 0.

      TRY.
          cl_abap_typedescr=>describe_by_name( 'DD04T' ).

          DATA r_dd04t TYPE REF TO data.
          FIELD-SYMBOLS <any>  TYPE STANDARD TABLE.

          DATA(tabletype) = 'DD04T'.
          DATA(fb)        = 'DD_DTEL_GET'.

          CREATE DATA r_dd04t TYPE STANDARD TABLE OF (tabletype) .
          ASSIGN r_dd04t->* TO <any> ."CASTING TYPE (tabletype) .


          CALL FUNCTION fb
            EXPORTING
              langu         = sy-langu
              roll_name     = 'MATNR'
            TABLES
              dd04t_tab_a   = <any>
            EXCEPTIONS
              illegal_value = 1.


          READ TABLE <any> ASSIGNING FIELD-SYMBOL(<line>) INDEX 1.
          IF <line> IS ASSIGNED.

            ASSIGN COMPONENT 'DDTEXT'    OF STRUCTURE <line> TO FIELD-SYMBOL(<ddtext>).
            ASSIGN COMPONENT 'REPTEXT'   OF STRUCTURE <line> TO FIELD-SYMBOL(<reptext>).
            ASSIGN COMPONENT 'SCRTEXT_S' OF STRUCTURE <line> TO FIELD-SYMBOL(<scrtext_s>).
            ASSIGN COMPONENT 'SCRTEXT_M' OF STRUCTURE <line> TO FIELD-SYMBOL(<scrtext_m>).
            ASSIGN COMPONENT 'SCRTEXT_L' OF STRUCTURE <line> TO FIELD-SYMBOL(<scrtext_l>).

            IF  <ddtext>    IS ASSIGNED
            AND <reptext>   IS ASSIGNED
            AND <scrtext_s> IS ASSIGNED
            AND <scrtext_m> IS ASSIGNED
            AND <scrtext_l> IS ASSIGNED.

              text-rollname = iv_rollname.
              text-short_field_label   = <scrtext_s>.
              text-medium_field_label  = <scrtext_m>.
              text-long_field_label    = <scrtext_l>.
              text-heading_field_label = <reptext>.
              text-short_description   = <ddtext>.

              APPEND text TO mt_texts.

            ENDIF.
          ENDIF.

        CATCH cx_root.


          TRY.

              DATA(ele) = xco_cp_abap_dictionary=>data_element( iv_name = CONV #( iv_rollname ) ).

              DATA(cont) = ele->content(  ).

              DATA(content) = cont->get( ).

              text-rollname = iv_rollname.
              text-short_field_label = content-short_field_label-text.
              text-medium_field_label = content-medium_field_label-text.
              text-long_field_label = content-long_field_label-text.
              text-heading_field_label = content-heading_field_label-text.
              text-short_description = content-short_description.

              APPEND text TO mt_texts.

            CATCH cx_root.
          ENDTRY.

      ENDTRY.



    ENDIF.

    IF text IS NOT INITIAL.
      CASE iv_type.
        WHEN 'S'."Feldbezeichner kurz
          result = TEXT-short_field_label.
        WHEN 'M'."Feldbezeichner mittel
          result = TEXT-medium_field_label.
        WHEN 'L'."Feldbezeichner lang
          result = TEXT-long_field_label.
        WHEN 'R'."Überschrift
          result = TEXT-heading_field_label.
        WHEN 'D'."Kurzbeschreibung von Repository-Objekten
          result = TEXT-short_description.
        WHEN OTHERS.
          result = TEXT-short_field_label.
      ENDCASE.
    ENDIF.

    IF result IS INITIAL.
      result = iv_rollname.
    ENDIF.


  ENDMETHOD.


  METHOD GET_DD04T_L.

    ZSV_CL_TEXT_HELPER=>get_dd04t(
      EXPORTING
        iv_type     = 'L'
        iv_rollname = iv_rollname
        iv_langu    = iv_langu
      RECEIVING
        result      = result
    ).

  ENDMETHOD.


  METHOD GET_T100.

*      IF iv_langu IS INITIAL.
*      DATA(langu) = sy-langu.
*    ENDIF.
*
*    READ TABLE mt_t100 INTO DATA(t100)
*             WITH KEY arbgb   = iv_arbgb
*                      msgnr   = iv_msgnr
*                      SPRSL   = langu.
*    IF sy-subrc NE 0.
*
*      SELECT SINGLE * FROM t100 INTO t100
*                        WHERE arbgb    = iv_arbgb
*                        and   msgnr    = iv_msgnr
*                        AND   SPRSL    = langu.
*      IF sy-subrc = 0.
*        APPEND t100 TO mt_t100.
*
*      ELSE.
*
*        SELECT SINGLE * FROM t100 INTO t100
*                        WHERE arbgb   = iv_arbgb
*                        and   msgnr   = iv_msgnr
*                        AND   SPRSL   = 'E'.
*        IF sy-subrc = 0.
*          APPEND t100 TO mt_t100.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
*
*      result = t100-text.


  ENDMETHOD.
ENDCLASS.
