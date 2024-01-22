CLASS zsv_cl_app_001 DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_serializable_object .
    INTERFACES z2ui5_if_app .
    INTERFACES zsv_if_000 .

    ALIASES mo_parent_view
      FOR zsv_if_000~mo_parent_view.
    ALIASES mv_view_display
      FOR zsv_if_000~mv_view_display.
    ALIASES set_main_app_data
      FOR zsv_if_000~set_app_data.


    DATA ms_layout       TYPE REF TO data.
    DATA ms_fixval       TYPE REF TO data.
    DATA mv_search_value TYPE string .
    DATA mt_table        TYPE REF TO data .
    DATA mt_table_tmp    TYPE REF TO data .
    DATA ms_table_row    TYPE REF TO data .
    DATA mt_table_del    TYPE REF TO data .

    DATA mv_table        TYPE string .
    DATA mt_dfies        TYPE zsv_cl_object_hlper=>ty_t_dfies."STANDARD TABLE OF dfies .
    DATA mv_activ_row    TYPE string .
    DATA mv_edit         TYPE abap_bool .

    TYPES: fixvalue type ZSV_CL_OBJECT_HLPER=>fixvalue,
      fixvalues TYPE STANDARD TABLE OF fixvalue WITH empty KEY.


  PROTECTED SECTION.


    DATA client            TYPE REF TO z2ui5_if_client.
    DATA check_initialized TYPE abap_bool.
    DATA mv_f4_fieldname   TYPE string.

    METHODS on_init.
    METHODS on_event.
    METHODS search.
    METHODS render_popup.
    METHODS get_data.
    METHODS set_row_id.
    METHODS POPUP_add.
    METHODS button_popup_delete.
    METHODS get_DFIES.
    METHODS row_select.
    METHODS Render_main.

    METHODS Data_to_table
      CHANGING
        row TYPE any.
    METHODS POPUP_edit.

    METHODS get_txt
      IMPORTING
                roll          TYPE string
                type          TYPE string OPTIONAL
      RETURNING VALUE(result) TYPE string.

    METHODS get_txt_l
      IMPORTING
                roll          TYPE string
      RETURNING VALUE(result) TYPE string.

    METHODS button_save.

    METHODS get_comp
      RETURNING
        VALUE(result) TYPE abap_component_tab.

    METHODS Render_popup_footer
      IMPORTING
        xml           TYPE REF TO z2ui5_cl_xml_view
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_xml_view.

    METHODS Render_main_footer
      IMPORTING
        page TYPE REF TO z2ui5_cl_xml_view.

    METHODS Render_main_head
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_xml_view.

    METHODS prefill_popup_values.

    METHODS on_event_main.

    METHODS on_event_popup.

    METHODS on_event_layout.

    METHODS popup_f4.

    METHODS on_after_f4.

    METHODS get_table_name.

    METHODS on_after_layout.

    METHODS get_layout.

    METHODS get_fixval.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZSV_CL_APP_001 IMPLEMENTATION.


  METHOD get_txt.

    ZSV_cl_text_helper=>get_dd04t(
      EXPORTING
        iv_rollname =  roll
      RECEIVING
        result      = result ).

  ENDMETHOD.


  METHOD get_txt_l.

    ZSV_cl_text_helper=>get_dd04t(
      EXPORTING
        iv_rollname =  roll
        iv_type     = 'L'
      RECEIVING
        result      = result ).

  ENDMETHOD.


  METHOD button_popup_delete.

    FIELD-SYMBOLS <del> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    ASSIGN mt_table->* TO <tab>.
    ASSIGN mt_table_del->* TO <del>.

    DATA(t_arg) = client->get( )-t_event_arg.

    READ TABLE <tab> ASSIGNING FIELD-SYMBOL(<line>) INDEX t_arg[ 1 ].
    IF sy-subrc = 0.

      APPEND <line> TO <del>.
      DELETE <tab> INDEX t_arg[ 1 ].

    ENDIF.

    set_row_id(  ).

  ENDMETHOD.


  METHOD button_save.


    DATA t_table     TYPE REF TO data.
    DATA t_table_del TYPE REF TO data.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_org> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <del> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <del_org> TYPE STANDARD TABLE.

    ASSIGN mt_table->* TO <tab>.
    ASSIGN mt_table_del->* TO <del>.

    DATA(comp) = ZSV_cl_object_hlper=>get_comp_of_table( table = mv_table ).

    TRY.

        DATA(struct_desc) = cl_abap_structdescr=>create( comp ).

        DATA(table_desc) = cl_abap_tabledescr=>create(
          p_line_type  = struct_desc
          p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA t_table     TYPE HANDLE table_desc.
        CREATE DATA t_table_del TYPE HANDLE table_desc.

        ASSIGN t_table->* TO <tab_org>.
        ASSIGN t_table_del->* TO <del_org>.
        MOVE-CORRESPONDING <tab> TO <tab_org>.
        IF <del> IS ASSIGNED.
          MOVE-CORRESPONDING <del> TO <del_org>.
        ENDIF.
      CATCH cx_root.

    ENDTRY.



    TRY.

        IF <del> IS ASSIGNED.

          DELETE (mv_table) FROM TABLE @<del_org>.
          IF sy-subrc = 0.
            COMMIT WORK AND WAIT.
            CLEAR: mt_table_del.
          ENDIF.

        ENDIF.

        MODIFY (mv_table) FROM TABLE @<tab_org>.

        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
          client->message_toast_display( zsv_cl_text_helper=>get_t100(
                                           iv_arbgb = '/SCWM/IT_DEVKIT'
                                           iv_msgnr = '012'   ) ).
        ENDIF.

        client->view_model_update( ).

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD data_to_table.

    FIELD-SYMBOLS <row> TYPE any.

    LOOP AT mt_dfies INTO DATA(dfies).

      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE row TO FIELD-SYMBOL(<value_tab>).
      ASSIGN ms_table_row->* TO <row>.
      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<value_struc>).

      IF <value_tab> IS ASSIGNED AND <value_struc> IS ASSIGNED.

        IF <value_tab> NE <value_struc>.

          <value_tab> = <value_struc>.

          client->view_model_update( ).

        ENDIF.

      ENDIF.

    ENDLOOP.

    set_row_id( ).

  ENDMETHOD.


  METHOD get_comp.

    TRY.

        DATA index TYPE int4.

        DATA(comp) = ZSV_cl_object_hlper=>get_comp_of_table( table = mv_table ).

        result = VALUE cl_abap_structdescr=>component_table( ( name = 'ROW_ID'
                                                               type = CAST #( cl_abap_datadescr=>describe_by_data( index ) ) ) ).

        APPEND LINES OF comp TO REsult.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD get_data.

    DATA lo_struct          TYPE REF TO cl_abap_structdescr.
    DATA lo_tab             TYPE REF TO cl_abap_tabledescr.
    DATA selkz              TYPE abap_bool.

    FIELD-SYMBOLS:  <table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS:  <table2> TYPE STANDARD TABLE.

    DATA(t_comp) = get_comp( ).

    TRY.

        DATA(New_struct_desc) = cl_abap_structdescr=>create( t_comp ).

        DATA(new_table_desc) = cl_abap_tabledescr=>create(
          p_line_type  = new_struct_desc
          p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA mt_table     TYPE HANDLE new_table_desc.
        CREATE DATA ms_table_row TYPE HANDLE new_struct_desc.

        ASSIGN mt_table->* TO <table>.

        SELECT * FROM (mv_table) INTO CORRESPONDING FIELDS OF TABLE @<table>.

      CATCH cx_root.

    ENDTRY.

    set_row_id( ).

    ASSIGN mt_table->* TO <table>.
    CREATE DATA mt_table_tmp LIKE <table>.
    ASSIGN mt_table_tmp->* TO <table2>.
    <table2> = <table>.


  ENDMETHOD.


  METHOD get_dfies.

    mt_dfies    =  ZSV_cl_object_hlper=>get_dfies_of_table( mv_table ) .

  ENDMETHOD.


  METHOD on_event.

    on_event_main( ).

    on_event_popup( ).

    on_event_LAYOUT( ).

  ENDMETHOD.


  METHOD on_init.

    get_table_name( ).

    get_data(  ).

    get_DFIES( ).

    get_layout( ).

    get_fixval( ).

  ENDMETHOD.


  METHOD get_table_name.

    IF mv_table IS INITIAL.

      " TABLE per Paramter?
      DATA(tab) = z2ui5_cl_util_func=>url_param_get(
        val = 'TABLE'
        url = client->get( )-s_config-search ).

      tab = to_upper( tab ).

      IF  tab IS INITIAL.
        mv_table = 'USR01'. " FALLBACK
      ELSE.
        mv_table = tab.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD POPUP_add.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN mt_table->* TO <tab>.

    APPEND INITIAL LINE TO <tab> ASSIGNING FIELD-SYMBOL(<row>).

    Data_to_table( CHANGING row = <row> ).

    client->popup_destroy( ).

  ENDMETHOD.


  METHOD POPUP_edit.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN mt_table->* TO <tab>.

    READ TABLE <tab> ASSIGNING FIELD-SYMBOL(<row>) INDEX mv_activ_row.

    Data_to_table( CHANGING row = <row> ).

    client->popup_destroy( ).

  ENDMETHOD.


  METHOD render_main.

    FIELD-SYMBOLS <struc>  TYPE ZSV_cl_app_009=>ty_s_layout.
    FIELD-SYMBOLS <table>  type ANY TABLE.
    FIELD-SYMBOLS <layout> type any.

    DATA(page) = Render_main_head( ).

    ASSIGN mt_table->* to <table>.

    DATA(table) = page->table(
      growing    = 'true'
      width      = 'auto'
      items      = client->_bind( val = <table> )
      headerText = mv_table
    ).


    DATA(headder) =  table->header_toolbar(
               )->overflow_toolbar(
                 )->Title(   text =  mv_table
                 )->toolbar_spacer(
                 )->search_field(
                              value  = client->_bind_edit( mv_search_value )
                              search = client->_event( 'BUTTON_SEARCH' )
                              change = client->_event( 'BUTTON_SEARCH' )
                              id     = `SEARCH`
                              width  = '17.5rem' ).

    headder = zsv_cl_app_009=>render_layout_function( xml    = headder
                                                      client = client ).

    DATA(columns) = table->columns( ).

    LOOP AT mt_dfies REFERENCE INTO DATA(dfies).

      ASSIGN ms_layout->* to <layout>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <layout> TO <struc>.
      CHECK <struc> IS ASSIGNED.

      columns->column( visible         = client->_bind( <struc>-visible )
                       halign          = client->_bind( <struc>-halign )
                       importance      = client->_bind( <struc>-importance )
                       mergeduplicates = client->_bind( <struc>-merge )
                       minscreenwidth  = client->_bind( <struc>-width )
       )->text( get_txt( dfies->rollname ) ).

    ENDLOOP.


    DATA(cells) = columns->get_parent( )->items(
                                       )->column_list_item( vAlign = 'Middle'
                                                            type   = 'Navigation'
                                                            press  = client->_event( val   = 'ROW_SELECT'
                                                                                     t_arg = VALUE #( ( `${ROW_ID}`  ) ) )
                                       )->cells( ).

    LOOP AT mt_dfies REFERENCE INTO dfies.


      DATA(text)  = COND #( WHEN dfies->keyflag = abap_false THEN '{' && dfies->fieldname && '}' ELSE '' ).
      DATA(title) = COND #( WHEN dfies->keyflag = abap_true  THEN '{' && dfies->fieldname && '}' ELSE '' ).


      cells->object_identifier( text  = text
                                title = title ).


    ENDLOOP.

    Render_main_footer( page ).

  ENDMETHOD.


  METHOD Render_main_head.

    IF mo_parent_view IS INITIAL.

      DATA(view)  = z2ui5_cl_xml_view=>factory( client )."->shell( ).


      result = view->page( title          = mv_table
                           navbuttonpress = client->_event( 'BACK' )
                           shownavbutton  = abap_true
                           class          = 'sapUiContentPadding' ).

    ELSE.

      result = mo_parent_view->get( `Page` ).


    ENDIF.

    result->header_content( )->scroll_container( height = '70%' vertical = abap_true ).

  ENDMETHOD.


  METHOD Render_main_footer.

    page->footer( )->overflow_toolbar(
                    )->toolbar_spacer(
                    )->button(
                        icon    = 'sap-icon://add'
                        text    = 'Add'
                        press   = client->_event( 'BUTTON_ADD' )
                        type    = 'Default'
                     )->button(
                        icon    = 'sap-icon://refresh'
                        text    = 'Refresh'
                        press   = client->_event( 'BUTTON_REFRESH' )
                        type    = 'Default'
                     )->button(
                        text    = 'Save'
                        press   = client->_event( 'BUTTON_SAVE' )
                        type    = 'Success' ).

    IF mo_parent_view IS INITIAL.

      client->view_display( page->stringify( ) ).

    ELSE.

      mv_view_display = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD render_popup.

    DATA index TYPE int4.
    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <fixval> TYPE any.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( client ).

    DATA(title) = COND #( WHEN mv_edit = abap_true THEN get_txt( 'CRMST_UIU_EDIT' ) ELSE get_txt( 'RSLPO_GUI_ADDPART' ) ).

    DATA(simple_form) =  popup->dialog( title = title contentWidth = '60%'
          )->simple_form(
          title     = ''
          layout    = 'ResponsiveGridLayout'
          editable  = abap_true
          )->content( ns = 'form'
          ).

    " Gehe über alle Comps wenn wir im Edit sind dann sind keyfelder nicht eingabebereit.
    LOOP AT mt_dfies REFERENCE INTO DATA(dfies).

      DATA(enabled) = COND #( WHEN dfies->keyflag = abap_true AND mv_edit = abap_true THEN abap_false ELSE abap_true ).

      IF dfies->fieldname = 'MANDT'.
        enabled = abap_false.
      ENDIF.

      ASSIGN ms_table_row->* TO <row>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      IF <val> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      simple_form->label( text = get_txt( dfies->rollname  ) ).

      ASSIGN ms_fixval->* TO <fixval>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <fixval> TO FIELD-SYMBOL(<struc>).
      CHECK <struc> IS ASSIGNED.


      IF <struc> IS INITIAL.

        IF dfies->checktable IS NOT INITIAL.

          simple_form->input( value            = client->_bind_edit( <val> )
                              showvaluehelp    = abap_true
                              enabled          = enabled
                              valuehelprequest = client->_event( val = 'POPUP_F4' t_arg = VALUE #( (  dfies->fieldname ) ) ) ).

        ELSE.

          simple_form->input( value         = client->_bind_edit( <val> )
                              showvaluehelp = abap_false
                              enabled       = enabled ).

        ENDIF.
      ELSE.

        simple_form->combobox(
                  enabled       = enabled
                  selectedkey = client->_bind_edit( <val> )
                  items       = client->_bind( <struc> )
                      )->item(
                          key = '{LOW}'
                          text = '{LOW} - {DDTEXT}' ).
      ENDIF.

    ENDLOOP.

    simple_form = Render_popup_footer( simple_form ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.


  METHOD render_popup_footer.

    DATA(event) =  COND #( WHEN mv_edit = abap_true THEN `POPUP_EDIT` ELSE `POPUP_ADD` ).

    DATA(toolbar) = xml->get_root( )->get_child(
         )->footer(
         )->overflow_toolbar( ).
    toolbar->toolbar_spacer(
     ).

    toolbar->button(
      text  = 'Back'
                icon  = 'sap-icon://nav-back'
      press = client->_event( 'POPUP_CLOSE' )
    ).

    IF mv_edit = abap_true.
      toolbar->button(
        text  = 'Delete'
        type  = 'Reject'
        icon  = 'sap-icon://delete'
        press = client->_event( val = 'BUTTON_POPUP_DELETE' t_arg = VALUE #( ( mv_activ_row ) ) ) ).
    ENDIF.

    toolbar->button(
      text  = 'Okay'
      press = client->_event( event )
      type  = 'Emphasized'
    ).

    result = xml.

  ENDMETHOD.


  METHOD row_select.

    mv_edit = abap_true.

    DATA(lt_arg) = client->get( )-t_event_arg.
    READ TABLE lt_arg INTO DATA(ls_arg) INDEX 1.

    CHECK  sy-subrc = 0.

    mv_activ_row = ls_arg.

    prefill_popup_values(  ).

    render_popup(  ).

  ENDMETHOD.


  METHOD prefill_popup_values.


    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <table_row> TYPE any.

    ASSIGN mt_table->* TO <tab>.

    READ TABLE <tab> ASSIGNING FIELD-SYMBOL(<row>) INDEX mv_activ_row.

    CHECK  sy-subrc = 0.

    LOOP AT mt_dfies INTO DATA(dfies).

      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<value_tab>).
      ASSIGN ms_table_row->* TO <table_row>.
      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <table_row> TO FIELD-SYMBOL(<value_struc>).

      IF <value_tab> IS ASSIGNED AND <value_struc> IS ASSIGNED.
        <value_struc> = <value_tab>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD search.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_tmp> TYPE STANDARD TABLE.
    ASSIGN mt_table->* TO <tab>.
    ASSIGN mt_table_tmp->* TO <tab_tmp>.

    IF <tab_tmp> IS NOT INITIAL.
      <tab> = <tab_tmp>.
    ENDIF.

    IF mv_search_value IS NOT INITIAL.

      LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<f_row>).
        DATA(lv_row) = ``.
        DATA(lv_index) = 1.
        DO.
          ASSIGN COMPONENT lv_index OF STRUCTURE <f_row> TO FIELD-SYMBOL(<field>).
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          lv_row = lv_row && <field>.
          lv_index = lv_index + 1.
        ENDDO.

        IF lv_row NS mv_search_value.
          DELETE <tab>.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD set_row_id.


    FIELD-SYMBOLS <line> TYPE any.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN mt_table->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.

      ASSIGN COMPONENT 'ROW_ID' OF STRUCTURE <line> TO FIELD-SYMBOL(<row>).
      IF <row> IS ASSIGNED.
        <row> = sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF check_initialized = abap_false.
      check_initialized = abap_true.

      on_init( ).

      Render_main( ).

    ENDIF.

    on_after_f4( ).

    on_after_LAYOUT( ).

    on_event( ).

  ENDMETHOD.


  METHOD on_after_f4.

    FIELD-SYMBOLS <row> TYPE any.

    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_true.

      TRY.
          " War es die F4 Hilfe?
          DATA(app) = CAST ZSV_cl_app_004( client->get_app( client->get( )-s_draft-id_prev_app ) ).

          IF app->mv_return_value IS NOT INITIAL.

            READ TABLE mt_dfies INTO DATA(dfies) WITH KEY fieldname = mv_f4_fieldname.

            ASSIGN ms_table_row->* TO <row>.
            ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<value_struc>).

            IF <value_struc> IS ASSIGNED.
              <value_struc> = app->mv_return_value.
            ENDIF.

          ENDIF.

          "Main und Popup neu rendern
          render_main( ).
          render_popup(  ).

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD on_event_main.


    CASE client->get( )-event.

      WHEN 'BACK'.

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'BUTTON_REFRESH'.

        get_data( ).
        client->view_model_update( ).

      WHEN 'BUTTON_SEARCH'.

        search( ).
        client->view_model_update( ).

      WHEN 'BUTTON_SAVE'.

        button_save( ).

      WHEN 'BUTTON_ADD'.

        mv_edit = abap_false.
        render_popup(  ).

      WHEN 'ROW_SELECT'.

        row_select( ).

    ENDCASE.


  ENDMETHOD.


  METHOD on_event_popup.


    CASE client->get( )-event.

      WHEN 'BUTTON_POPUP_DELETE'.

        button_popup_delete( ).
        client->popup_destroy( ).

      WHEN 'POPUP_ADD'.

        POPUP_add( ).

      WHEN 'POPUP_EDIT'.

        POPUP_edit( ).

      WHEN 'POPUP_INPUT_SCREEN'.

      WHEN 'POPUP_CLOSE'.

        client->popup_destroy( ).

      WHEN 'BUTTON_LAYOUT_CLOSE'.

        client->popup_destroy( ).
        render_main( ).

      WHEN 'POPUP_F4'.

        popup_f4( ).

    ENDCASE.

  ENDMETHOD.


  METHOD popup_f4.

    FIELD-SYMBOLS <row> TYPE any.

    DATA(lt_arg) = client->get( )-t_event_arg.

    mv_f4_fieldname = VALUE string( lt_arg[ 1 ] ).

    READ TABLE mt_dfies INTO DATA(dfies) WITH KEY fieldname = mv_f4_fieldname.

    ASSIGN ms_table_row->* TO <row>.
    ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<value_struc>).

    client->nav_app_call( ZSV_cl_app_004=>factory(
        i_table = mv_table
        i_field = mv_f4_fieldname
        i_value = CONV #( <value_struc> )
      ) ).

  ENDMETHOD.


  METHOD ZSV_if_000~set_app_data.

    mv_table = data.

  ENDMETHOD.


  METHOD on_event_layout.

    client = ZSV_cl_app_009=>on_event_layout(
      client = client
      layout = ms_layout ).

  ENDMETHOD.


  METHOD on_after_layout.

    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_true.

      TRY.
          " War es das Layout?
          DATA(app) = CAST ZSV_cl_app_009( client->get_app( client->get( )-s_draft-id_prev_app ) ).

          ms_layout = app->ms_layout.

          render_main( ).

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD get_layout.

    DATA(class)   = cl_abap_classdescr=>get_class_name( me ).
    DATA(app)     = z2ui5_cl_util_func=>url_param_get( val = 'app' url = client->get( )-s_config-search ).


*    " Lagernumemr ermitteln
*    GET PARAMETER ID '/SCWM/LGN' FIELD DATA(lgnum).

    ms_layout = ZSV_cl_app_009=>init_layout(
      table = mv_table
      app   = app
      class = CONV #( class ) ).
*      lgnum = CONV #( lgnum )

  ENDMETHOD.


  METHOD get_fixval.


    DATA comp        TYPE cl_abap_structdescr=>component_table.
    DATA structdescr TYPE REF TO cl_abap_structdescr.
    DATA lt_fixval   TYPE fixvalues.
    FIELD-SYMBOLS <s_fixval> TYPE any.

    LOOP AT mt_dfies REFERENCE INTO DATA(dfies).

      comp = VALUE cl_abap_structdescr=>component_table( BASE comp ( name = dfies->fieldname
                                                                     type = CAST #( cl_abap_datadescr=>describe_by_data( lt_fixval ) ) ) ).
    ENDLOOP.

    structdescr = cl_abap_structdescr=>create( comp ).

    CREATE DATA ms_fixval TYPE HANDLE structdescr.

    LOOP AT mt_dfies REFERENCE INTO dfies.

      ASSIGN ms_fixval->* TO <s_fixval>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <s_fixval> TO FIELD-SYMBOL(<fixval>).

      CHECK <fixval> IS ASSIGNED.

      <fixval> = ZSV_cl_object_hlper=>get_fix_values( dfies->rollname ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
