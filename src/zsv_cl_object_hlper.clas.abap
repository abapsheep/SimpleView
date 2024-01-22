CLASS ZSV_cl_object_hlper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


    TYPES:
      BEGIN OF fixvalue,
        low        TYPE string,
        high       TYPE string,
        option     TYPE string,
        ddlanguage TYPE string,
        ddtext     TYPE string,
      END OF fixvalue .
    TYPES:
      fixvalues TYPE STANDARD TABLE OF fixvalue WITH DEFAULT KEY .

    types:
      begin of ty_s_dfies,
  tabname     type string,
  fieldname   type string,
  offset      type string,
  domname     type string,
  rollname    type string,
  checktable  type string,
  leng        type string,
  intlen      type string,
  outputlen   type string,
  decimals    type string,
  datatype    type string,
  inttype     type string,
  reftable    type string,
  reffield    type string,
  convexit    type string,
  fieldtext   type string,
  reptext     type string,
  scrtext_s   type string,
  scrtext_m   type string,
  scrtext_l   type string,
  keyflag     type string,
      end of ty_s_dfies,
      ty_t_dfies type standard table of ty_s_dfies with empty key.




    CLASS-METHODS get_dfies_of_table
      IMPORTING !table        TYPE string
      RETURNING VALUE(result) TYPE ty_t_dfies.

    CLASS-METHODS get_comp_of_table
      IMPORTING !table        TYPE string
      RETURNING VALUE(result) TYPE  cl_abap_structdescr=>component_table.

    CLASS-METHODS get_relative_name_of_table
      IMPORTING !table        TYPE any
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS get_fix_values
      IMPORTING
                !rollname     TYPE string
      RETURNING VALUE(result) TYPE fixvalues.
PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZSV_cl_object_hlper IMPLEMENTATION.

  METHOD get_dfies_of_table.



    DATA tabname     TYPE c LENGTH 16.
    DATA fields      TYPE REF TO object.
    DATA type        TYPE REF TO object.
    DATA db          TYPE REF TO object.
    DATA element     TYPE REF TO object.
    DATA content     TYPE REF TO object.
    DATA t_param     TYPE abap_parmbind_tab.
    DATA field       TYPE REF TO object.
    DATA fiel        TYPE REF TO object.
    DATA structdescr TYPE REF TO cl_abap_structdescr.
    DATA r_content   TYPE REF TO data.
    DATA r_names     TYPE REF TO data.

    FIELD-SYMBOLS <any>   TYPE any.
    FIELD-SYMBOLS <name>  TYPE any.
    FIELD-SYMBOLS <fiel>  TYPE REF TO object.
    FIELD-SYMBOLS <names> TYPE STANDARD TABLE.



    " convert to correct type,
    tabname = table.



*        DATA(a) = xco_cp_abap_dictionary=>database_table( iv_name = tabname ).
*        a->field(
*          EXPORTING
*            iv_name  = 'LAYOUT'
*          RECEIVING
*            ro_field = DATA(b1)   ).
*        b1->content(
*          RECEIVING
*            ro_content    = DATA(c1) ).
*        c1->get(
*          RECEIVING
*            rs_content = DATA(d1) ).
*        DATA(foreingkey) = b1->foreign_key.
*        DATA(searchhelp) = b1->search_help.
*        foreingkey->content(
*          RECEIVING
*            ro_content    = DATA(c2) ).
*        TRY.
*            c2->get(
*              RECEIVING
*                rs_content = DATA(d2)
*            ).
*          CATCH cx_root.
*        ENDTRY.
*        searchhelp->content(
*          RECEIVING
*            ro_content    = DATA(c3) ).
*        DATA(v1) = d1-type->get_built_in_type( ).
*        DATA(v2) = d1-type->get_data_element( ).
*DATA(name) = v2->if_xco_ad_object~name.



        TRY.
            cl_abap_typedescr=>describe_by_name( 'T100' ).

            TRY.
                structdescr ?= cl_abap_structdescr=>describe_by_name( tabname ).

                DATA(ddict) =  structdescr->get_ddic_field_list(  ).

                MOVE-CORRESPONDING ddict TO result.

                Loop at result ASSIGNING FIELD-SYMBOL(<result>) where rollname is INITIAL.
                <result>-rollname = <result>-fieldname.
                ENDLOOP.


              CATCH cx_root.
            ENDTRY.

          CATCH cx_root.

            CALL METHOD ('XCO_CP_ABAP_DICTIONARY')=>database_table
              EXPORTING
                iv_name           = tabname
              RECEIVING
                ro_database_table = db.

            ASSIGN db->('IF_XCO_DATABASE_TABLE~FIELDS->IF_XCO_DBT_FIELDS_FACTORY~ALL') TO <any>.

            IF sy-subrc  <> 0.
              " fallback to RTTI, KEY field does not exist in S/4 2020
              RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_class.
            ENDIF.

            fields = <any>.

            CREATE DATA r_names TYPE ('SXCO_T_AD_FIELD_NAMES').
            ASSIGN r_names->* TO <Names>.

            CALL METHOD fields->('IF_XCO_DBT_FIELDS~GET_NAMES')
              RECEIVING
                rt_names = <Names>.

            LOOP AT <Names> ASSIGNING <name>.

              CLEAR: t_param.

              " Befüllung der Tabelle
              INSERT VALUE #( name = 'IV_NAME' kind = cl_abap_objectdescr=>exporting value = REF #( <name> ) ) INTO TABLE t_param.
              INSERT VALUE #( name = 'RO_FIELD' kind = cl_abap_objectdescr=>receiving value = REF #( field ) ) INTO TABLE t_param.

              " Dynamischer Aufruf

              CALL METHOD db->(`IF_XCO_DATABASE_TABLE~FIELD`)
                PARAMETER-TABLE t_param.

              READ TABLE t_param ASSIGNING FIELD-SYMBOL(<line>) WITH KEY name = 'RO_FIELD'.
              ASSIGN <line>-value->* TO <fiel>.
*          fiel = t_param[ name = 'RO_FIELD' ]-value->*.

              CALL METHOD <fiel>->('IF_XCO_DBT_FIELD~CONTENT')
                RECEIVING
                  ro_content = content.

              CREATE DATA r_content TYPE ('IF_XCO_DBT_FIELD_CONTENT=>TS_CONTENT').
              ASSIGN r_content->* TO FIELD-SYMBOL(<Content>) CASTING TYPE ('IF_XCO_DBT_FIELD_CONTENT=>TS_CONTENT').

              CALL METHOD content->('IF_XCO_DBT_FIELD_CONTENT~GET')
                RECEIVING
                  rs_content = <Content>.

              ASSIGN COMPONENT 'KEY_INDICATOR'     OF STRUCTURE <content> TO FIELD-SYMBOL(<key>).
              ASSIGN COMPONENT 'SHORT_DESCRIPTION' OF STRUCTURE <content> TO FIELD-SYMBOL(<text>).
              ASSIGN COMPONENT 'TYPE'              OF STRUCTURE <content> TO FIELD-SYMBOL(<type>).

              CHECK <key> IS ASSIGNED AND <text> IS ASSIGNED AND <type> IS ASSIGNED.

              type = <type>.

              " Dict type
              CALL METHOD type->('IF_XCO_DBT_FIELD_TYPE~GET_DATA_ELEMENT')
                RECEIVING
                  ro_data_element = element.


              IF <text> IS INITIAL.
                <text> = <name>.
              ENDIF.

              ASSIGN element->('IF_XCO_AD_OBJECT~NAME') TO FIELD-SYMBOL(<rname>).

              IF sy-subrc = 0.
                result = VALUE #(  BASE result ( fieldname = <name> keyflag = <key> tabname = tabname scrtext_s = <text> rollname = <rname> ) ).
              ELSE.
                result = VALUE #(  BASE result ( fieldname = <name> keyflag = <key> tabname = tabname scrtext_s = <text> rollname = <name> ) ).
              ENDIF.

              UNASSIGN <Content>.
              UNASSIGN <key>.
              UNASSIGN <Text>.
              UNASSIGN <type>.
              UNASSIGN <rname>.

            ENDLOOP.


        ENDTRY.


      ENDMETHOD.

  METHOD get_comp_of_table.

    TRY.

        cl_abap_typedescr=>describe_by_name(
          EXPORTING
            p_name         = table
          RECEIVING
            p_descr_ref    = DATA(typedesc)
          EXCEPTIONS
            type_not_found = 1
            OTHERS         = 2 ).
        IF sy-subrc <> 0.

        ENDIF.

        DATA(structdesc) = CAST cl_abap_structdescr( typedesc ).
        result = structdesc->get_components( ).


      CATCH cx_root into DATA(root).

    ENDTRY.


  ENDMETHOD.

  METHOD get_relative_name_of_table.

FIELD-SYMBOLS <table> type any.

    TRY.
        DATA(typedesc) = cl_abap_typedescr=>describe_by_data( table ).

        CASE typedesc->kind.

          WHEN cl_abap_typedescr=>kind_table.
            DATA(tabledesc) = CAST cl_abap_tabledescr( typedesc ).
            DATA(structdesc) = CAST cl_abap_structdescr( tabledesc->get_table_line_type( ) ).
            result = structdesc->get_relative_name( ).
            RETURN.

          WHEN typedesc->kind_ref.

            ASSIGN table->* to <table>.
            result = get_relative_name_of_table( <table> ).

        ENDCASE.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.



 METHOD get_fix_values.

   CHECK rollname IS NOT INITIAL.

   TRY.

       CALL METHOD cl_abap_typedescr=>describe_by_name
         EXPORTING
           p_name         = rollname
         RECEIVING
           p_descr_ref    = DATA(typedescr)
         EXCEPTIONS
           type_not_found = 1
           OTHERS         = 2.
       IF sy-subrc <> 0.
         RETURN.
       ENDIF.

       DATA(elemdescr) =  CAST cl_abap_elemdescr( typedescr ).

       elemdescr->get_ddic_fixed_values(
         EXPORTING
           p_langu        = sy-langu
         RECEIVING
           p_fixed_values = DATA(fixval)
         EXCEPTIONS
           not_found      = 1
           no_ddic_type   = 2
           OTHERS         = 3
       ).

       MOVE-CORRESPONDING fixval TO result.

     CATCH cx_root.
   ENDTRY.

 ENDMETHOD.

ENDCLASS.
