CLASS zsv_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  METHODS do.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zsv_test IMPLEMENTATION.
  METHOD do.


DATA(lo_database_table) = xco_cp_abap_dictionary=>database_table( 'USR01' ).




if 1 = 2.

endif.


  ENDMETHOD.

ENDCLASS.
