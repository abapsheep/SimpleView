CLASS ZSV_cl_app_004 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_serializable_object .
    INTERFACES z2ui5_if_app .

    DATA mt_DATA          type ref to data.
    DATA ms_DATA_row      type ref to data.
    DATA ms_layout        TYPE REF TO data.

    DATA mv_table         TYPE string.
    DATA mv_field         TYPE string.
    DATA mv_value         TYPE string.
    DATA mv_return_value  type string.
    DATA mv_rows      type numc4 VALUE '50'.
    DATA mt_dfies     type STANDARD TABLE OF dfies.

       CLASS-METHODS factory
      IMPORTING
        i_table          TYPE string
        i_field          TYPE string
        i_value          TYPE string
      RETURNING
        VALUE(result)   TYPE REF TO ZSV_cl_app_004.

PROTECTED SECTION.

  DATA client             TYPE REF TO z2ui5_if_client.
  DATA mv_init            TYPE abap_bool.
  DATA mv_check_tab_field TYPE string.
  DATA mv_check_tab       TYPE string.

  METHODS get_dfies.

  METHODS on_init.

  METHODS render_view.

  METHODS on_event.

  METHODS set_row_id.

  METHODS get_txt
    IMPORTING roll          TYPE string
              type          TYPE char1 OPTIONAL
    RETURNING VALUE(result) TYPE string.

  METHODS get_txt_l
    IMPORTING roll          TYPE string
    RETURNING VALUE(result) TYPE string.

  METHODS get_data
    IMPORTING
      where TYPE rsds_twhere.

  METHODS get_where_tab
    RETURNING
      VALUE(result) TYPE rsds_twhere.

  METHODS prefill_inputs.

  METHODS on_after_layout.

  METHODS get_layout.

PRIVATE SECTION.
    METHODS create_objects.

ENDCLASS.



CLASS ZSV_cl_app_004 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

  me->client = client.

    IF mv_init = abap_false.
      mv_init = abap_true.

      on_init( ).

      render_view( ).

    ENDIF.

    on_event( ).

    on_after_LAYOUT( ).

    client->view_model_update(  ).

  ENDMETHOD.

  METHOD on_init.

    get_dfies(  ).

    get_layout(   ).

    create_objects(  ).

    prefill_inputs( ).

    get_data( get_where_tab( ) ).

  ENDMETHOD.

  METHOD get_where_tab.

    DATA t_selopt TYPE rsds_frange_t.
    FIELD-SYMBOLS <struc> type any.

    " Gehe über alle Comps
    LOOP AT mt_dfies REFERENCE INTO DATA(dfies).

      CHECK dfies->keyflag = abap_true OR dfies->fieldname = mv_check_tab_field.

      assign ms_data_row->* to <struc>.
      ASSIGN COMPONENT dfies->fieldname of STRUCTURE <struc> TO FIELD-SYMBOL(<val>).

      CHECK <val> IS NOT INITIAL.
      t_selopt = VALUE #( BASE t_selopt ( fieldname = dfies->fieldname selopt_t = VALUE #( ( sign = 'I' option = 'CP' low = `*` && <val> && `*`  high = '' ) ) ) ).

    ENDLOOP.

    DATA(range)  = VALUE rsds_trange( ( tablename = mv_check_tab frange_t = t_selopt ) ).

    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING
        field_ranges  = range
      IMPORTING
        where_clauses = result.

  ENDMETHOD.


METHOD create_objects.

  DATA lo_struct          TYPE REF TO cl_abap_structdescr.
  DATA lo_tab             TYPE REF TO cl_abap_tabledescr.
  DATA index              TYPE int4.

  FIELD-SYMBOLS <table>   TYPE STANDARD TABLE.

  TRY.

      DATA(comp) = VALUE cl_abap_structdescr=>component_table(  ( name = 'ROW_ID'
                                                                  type = CAST #( cl_abap_datadescr=>describe_by_data( index ) ) ) ).

      APPEND LINES OF ZSV_cl_object_hlper=>get_comp_of_table( table = mv_check_tab  ) TO comp.

      DATA(New_struct_desc) = cl_abap_structdescr=>create( comp ).

      DATA(new_table_desc) = cl_abap_tabledescr=>create(
        p_line_type  = new_struct_desc
        p_table_kind = cl_abap_tabledescr=>tablekind_std ).

      CREATE DATA mt_data     TYPE HANDLE new_table_desc.
      CREATE DATA ms_data_row TYPE HANDLE new_struct_desc.

    CATCH cx_root.

  ENDTRY.

ENDMETHOD.


  METHOD get_data.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    TRY.

        ASSIGN mt_data->* to <table>.

        DATA(range) = VALUE #( where[ 1 ]-where_tab OPTIONAL ).

        SELECT * FROM (mv_check_tab) INTO CORRESPONDING FIELDS OF TABLE @<table> UP TO @mv_rows ROWS
        WHERE (range).

        set_row_id( ).

      CATCH cx_root.

    ENDTRY.

  ENDMETHOD.




  METHOD render_view.

    FIELD-SYMBOLS <struc> TYPE ZSV_cl_app_009=>ty_s_layout.
    FIELD-SYMBOLS <row> TYPE any.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( client ).

    DATA(simple_form) = popup->dialog( title = 'F4-Help' contentWidth = '90%' afterclose = client->_event( 'F4_CLOSE' )
          )->simple_form(
          title     = 'F4-Help'
          layout    = 'ResponsiveGridLayout'
          editable  = abap_true
          )->content( ns = 'form'
          ).

    " Gehe über alle Comps
    LOOP AT mt_dfies REFERENCE INTO DATA(dfies).

      CHECK dfies->fieldname NE `MANDT`.
      CHECK dfies->keyflag = abap_true OR dfies->fieldname = mv_check_tab_field.

      ASSIGN ms_data_row->* TO <row>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).

      simple_form->label( text = get_txt( CONV #( dfies->rollname ) ) ).

      simple_form->input( value         = client->_bind_edit( <val> )
                          showvaluehelp = abap_false
                          submit        = client->_event( 'F4_INPUT_DONE' ) ).

    ENDLOOP.

    simple_form->label( text = get_txt( 'SYST_TABIX' ) ).

    simple_form->input( value         = client->_bind_edit( mv_rows )
                        showvaluehelp = abap_false
                        submit        = client->_event( 'F4_INPUT_DONE' )
                        maxLength     = '3' ).

    DATA(table) = popup->get_child( )->table(
                   growing    ='true'
                   width      ='auto'
                   items      = client->_bind( val = mt_DATA->* )
                   headerText = zcl_text_helper=>get_dd02t( mv_check_tab ) ).

    DATA(headder) = table->header_toolbar(
                 )->overflow_toolbar(
                 )->Title(   text = zcl_text_helper=>get_dd02t( mv_check_tab )
                 )->toolbar_spacer( ).


    headder = ZSV_cl_app_009=>render_layout_function( xml    = headder
                                                      client = client ).


    DATA(columns) = table->columns( ).

    LOOP AT mt_dfies REFERENCE INTO dfies.

      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE ms_layout->* TO <struc>.
      CHECK <struc> IS ASSIGNED.

      columns->column( visible = <struc>-visible )->text( get_txt(  CONV #( dfies->rollname ) ) ).

    ENDLOOP.


    DATA(cells) = columns->get_parent( )->items(
                                       )->column_list_item( vAlign = 'Middle'
                                                            type   ='Navigation'
                                                            press  = client->_event( val   = 'F4_ROW_SELECT'
                                                                                     t_arg = VALUE #( ( `${ROW_ID}`  ) ) )
                                       )->cells( ).

    LOOP AT mt_dfies REFERENCE INTO dfies.


      DATA(text)  = COND #( WHEN dfies->keyflag = abap_false THEN '{' && dfies->fieldname && '}' ELSE '' ).
      DATA(title) = COND #( WHEN dfies->keyflag = abap_true  THEN '{' && dfies->fieldname && '}' ELSE '' ).


      cells->object_identifier( text  = text
                                title = title ).


    ENDLOOP.


    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.


  METHOD on_event.

    DATA(event) = client->get( )-event.

    CASE client->get( )-event.

      WHEN `F4_CLOSE`.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN `F4_ROW_SELECT`.

        DATA(lt_arg) = client->get( )-t_event_arg.

        FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
        ASSIGN mt_DATA->* TO <tab>.

        READ TABLE <tab> ASSIGNING FIELD-SYMBOL(<row>) INDEX lt_arg[ 1 ].

        ASSIGN COMPONENT mv_check_tab_field OF STRUCTURE <row> TO FIELD-SYMBOL(<value>).

        mv_return_value = <value>.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'F4_INPUT_DONE'.

        get_data( get_where_tab( ) ).

        render_view( ).


      WHEN OTHERS.

        client = ZSV_cl_app_009=>on_event_layout(
          client = client
          layout = ms_layout ).

    ENDCASE.

  ENDMETHOD.




  METHOD set_row_id.

      FIELD-SYMBOLS <line> TYPE any.
      FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
      ASSIGN mt_DATA->* TO <tab>.

      LOOP AT <tab> ASSIGNING <line>.

        ASSIGN COMPONENT 'ROW_ID' OF STRUCTURE <line> TO FIELD-SYMBOL(<row>).
        IF <row> IS ASSIGNED.
          <row> = sy-tabix.
        ENDIF.
      ENDLOOP.

  ENDMETHOD.


  METHOD factory.

    result = NEW #( ).

    result->mv_table = i_table.
    result->mv_field = i_field.
    result->mv_value = i_value.

  ENDMETHOD.


  METHOD get_txt.

    ZSV_cl_text_helper=>get_dd04t(
      EXPORTING
        iv_rollname = CONV #( roll )
      RECEIVING
        result      = result ).

  ENDMETHOD.


  METHOD get_txt_l.

    ZSV_cl_text_helper=>get_dd04t(
      EXPORTING
        iv_rollname = CONV #( roll )
        iv_type     = 'L'
      RECEIVING
        result      = result ).

  ENDMETHOD.


  METHOD get_dfies.

   DATA(t_dfies) = ZSV_cl_object_hlper=>get_dfies_of_table( table = mv_table ).

    READ TABLE t_dfies REFERENCE INTO DATA(dfies) WITH KEY fieldname = mv_field.

    CHECK  dfies->checktable IS NOT INITIAL.

    mt_dfies   = ZSV_cl_object_hlper=>get_dfies_of_table( CONV #( dfies->checktable ) ).
*
    " ZUORDNUNG --- ggf ist das nicht zu 100% sicher ... :(
    mv_check_tab_field = VALUE #( mt_dfies[ rollname = dfies->rollname ]-fieldname ).
    mv_check_tab       = dfies->checktable.



  ENDMETHOD.


  METHOD prefill_inputs.

FIELD-SYMBOLS <row> type any.

    " Gehe über alle Comps
    LOOP AT mt_dfies REFERENCE INTO DATA(dfies).

      CHECK dfies->keyflag = abap_true OR dfies->fieldname = mv_check_tab_field.

      ASSIGN ms_data_row->* to <row>.
      ASSIGN COMPONENT dfies->fieldname of STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      check <val> is ASSIGNED.

      If dfies->fieldname = mv_check_tab_field.

        <val> = mv_value.

      endif.

    ENDLOOP.


  ENDMETHOD.


  METHOD on_after_layout.

    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_true.

      TRY.
          " War es das Layout?
          DATA(app) = CAST ZSV_cl_app_009( client->get_app( client->get( )-s_draft-id_prev_app ) ).

          ms_layout = app->ms_layout.

          render_view( ).

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD get_layout.

    DATA(class)   = cl_abap_classdescr=>get_class_name( me  ).
    DATA(app)     = z2ui5_cl_util_func=>url_param_get( val = 'app' url = client->get( )-s_config-search ).

    " Lagernumemr ermitteln
    GET PARAMETER ID '/SCWM/LGN' FIELD DATA(lgnum).

    ms_layout = ZSV_cl_app_009=>init_layout(
                  table = mv_table
                  app   = ''
                  class = conv #( class )
                  lgnum = conv #( lgnum ) ).

  ENDMETHOD.

ENDCLASS.
