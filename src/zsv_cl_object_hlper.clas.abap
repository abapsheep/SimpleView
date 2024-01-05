CLASS ZSV_cl_object_hlper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_comp_of_table
      IMPORTING !table        TYPE string
      RETURNING VALUE(result) TYPE  cl_abap_structdescr=>component_table.

    CLASS-METHODS get_dfies_of_table
      IMPORTING !table        TYPE string
      RETURNING VALUE(result) TYPE  ddfields .

    CLASS-METHODS get_relative_name_of_table
      IMPORTING !table        TYPE any
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS get_fix_values
      IMPORTING
                !rollname     TYPE string
      RETURNING VALUE(result) TYPE cl_abap_elemdescr=>fixvalues.
PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZSV_cl_object_hlper IMPLEMENTATION.


  METHOD get_comp_of_table.

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

  ENDMETHOD.



  METHOD get_dfies_of_table.

    DATA structdescr TYPE REF TO cl_abap_structdescr.

    TRY.

        structdescr ?= cl_abap_structdescr=>describe_by_name( CONV ddobjname( table ) ).

        result =  structdescr->get_ddic_field_list( ).

      CATCH cx_root.
    ENDTRY.


  ENDMETHOD.

  METHOD get_relative_name_of_table.

    TRY.
        DATA(typedesc) = cl_abap_typedescr=>describe_by_data( table ).

        CASE typedesc->kind.

          WHEN cl_abap_typedescr=>kind_table.
            DATA(tabledesc) = CAST cl_abap_tabledescr( typedesc ).
            DATA(structdesc) = CAST cl_abap_structdescr( tabledesc->get_table_line_type( ) ).
            result = structdesc->get_relative_name( ).
            RETURN.

          WHEN typedesc->kind_ref.

            result = get_relative_name_of_table( table->* ).

        ENDCASE.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.



 METHOD get_fix_values.

   TRY.
       DATA(typedescr) = cl_abap_typedescr=>describe_by_name( rollname ).
       DATA(elemdescr) =  CAST cl_abap_elemdescr( typedescr ).

       elemdescr->get_ddic_fixed_values(
         EXPORTING
           p_langu        = sy-langu
         RECEIVING
           p_fixed_values = result
         EXCEPTIONS
           not_found      = 1
           no_ddic_type   = 2
           OTHERS         = 3
       ).

     CATCH cx_root.
   ENDTRY.

 ENDMETHOD.

ENDCLASS.
