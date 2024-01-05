class ZSV_CL_TEXT_HELPER definition
  public
  final
  create public .

public section.

  class-data:
    mt_dd04t TYPE STANDARD TABLE OF dd04t WITH EMPTY KEY .
  class-data:
    mt_t100 TYPE STANDARD TABLE OF t100 WITH EMPTY KEY .
  class-data:
    MT_dd02t TYPE STANDARD TABLE OF dd02t WITH EMPTY KEY .

  class-methods GET_DD04T_L
    importing
      !IV_ROLLNAME type ROLLNAME
      !IV_LANGU type SY-LANGU optional
    returning
      value(RESULT) type STRING .
  class-methods GET_DD04T
    importing
      !IV_TYPE type CHAR1 default 'S'
      !IV_ROLLNAME type ROLLNAME
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

    IF iv_langu IS INITIAL.
      DATA(langu) = sy-langu.
    ENDIF.

    READ TABLE mt_dd02t INTO DATA(dd02t)
             WITH KEY tabname   = iv_tabname
                      ddlanguage = langu.
    IF sy-subrc NE 0.

      SELECT SINGLE * FROM dd02t INTO dd02t
                        WHERE tabname   = iv_tabname
                        AND   ddlanguage = langu.
      IF sy-subrc = 0.
        APPEND dd02t TO mt_dd02t.

      ELSE.

        SELECT SINGLE * FROM dd02t INTO dd02t
                        WHERE tabname   = iv_tabname
                        AND   ddlanguage = 'E'.
        IF sy-subrc = 0.
          APPEND dd02t TO mt_dd02t.
        ENDIF.
      ENDIF.

    ENDIF.

    IF dd02t IS NOT INITIAL.
      result = dd02t-ddtext.
    ELSE.
      result = iv_tabname.
    ENDIF.


  ENDMETHOD.


  METHOD GET_DD04T.


    IF iv_langu IS INITIAL.
      DATA(langu) = sy-langu.
    ENDIF.


    READ TABLE mt_dd04t INTO DATA(dd04t)
             WITH KEY rollname   = iv_rollname
                      ddlanguage = langu.
    IF sy-subrc NE 0.

      SELECT SINGLE * FROM dd04t INTO dd04t
                        WHERE rollname   = iv_rollname
                        AND   ddlanguage = langu.
      IF sy-subrc = 0.
        APPEND dd04t TO mt_dd04t.

      ELSE.

        SELECT SINGLE * FROM dd04t INTO dd04t
                        WHERE rollname   = iv_rollname
                        AND   ddlanguage = 'E'.
        IF sy-subrc = 0.
          APPEND dd04t TO mt_dd04t.
        ENDIF.
      ENDIF.

    ENDIF.

    IF dd04t IS NOT INITIAL.
      CASE iv_type.
        WHEN 'S'."Feldbezeichner kurz
          result = dd04t-scrtext_s.
        WHEN 'M'."Feldbezeichner mittel
          result = dd04t-scrtext_m.
        WHEN 'L'."Feldbezeichner lang
          result = dd04t-scrtext_l.
        WHEN 'R'."Überschrift
          result = dd04t-reptext.
        WHEN 'D'."Kurzbeschreibung von Repository-Objekten
          result = dd04t-ddtext.
        WHEN ''.
          result = dd04t-scrtext_s.
        WHEN OTHERS.
          result = dd04t-scrtext_s.
      ENDCASE.
    ENDIF.

    IF result IS INITIAL.
      result = iv_rollname.
    ENDIF.

  ENDMETHOD.


  METHOD GET_DD04T_L.

    zcl_text_helper=>get_dd04t(
      EXPORTING
        iv_type     = 'L'
        iv_rollname = iv_rollname
        iv_langu    = iv_langu
      RECEIVING
        result      = result
    ).

  ENDMETHOD.


  METHOD GET_T100.

      IF iv_langu IS INITIAL.
      DATA(langu) = sy-langu.
    ENDIF.

    READ TABLE mt_t100 INTO DATA(t100)
             WITH KEY arbgb   = iv_arbgb
                      msgnr   = iv_msgnr
                      SPRSL   = langu.
    IF sy-subrc NE 0.

      SELECT SINGLE * FROM t100 INTO t100
                        WHERE arbgb    = iv_arbgb
                        and   msgnr    = iv_msgnr
                        AND   SPRSL    = langu.
      IF sy-subrc = 0.
        APPEND t100 TO mt_t100.

      ELSE.

        SELECT SINGLE * FROM t100 INTO t100
                        WHERE arbgb   = iv_arbgb
                        and   msgnr   = iv_msgnr
                        AND   SPRSL   = 'E'.
        IF sy-subrc = 0.
          APPEND t100 TO mt_t100.
        ENDIF.
      ENDIF.

    ENDIF.

      result = t100-text.


  ENDMETHOD.
ENDCLASS.
