class ZSV_CL_APP_009 definition
  public
  final
  create public .

PUBLIC SECTION.

  INTERFACES if_serializable_object .
  INTERFACES z2ui5_if_app .

 types: fixvalue type ZSV_CL_OBJECT_HLPER=>fixvalue,
    fixvalues type standard table of fixvalue with empty key .


  TYPES: ty_s_layout TYPE ZSV_t005,
         ty_t_layout type STANDARD TABLE OF ty_s_layout with EMPTY KEY.
  TYPES: ty_s_header TYPE ZSV_t004.

  TYPES:
    BEGIN OF ty_s_t004.
      INCLUDE TYPE ZSV_t004.
  TYPES: selkz TYPE abap_bool,
    END OF ty_s_t004 .
  TYPES:
    ty_t_t004 TYPE STANDARD TABLE OF ty_s_t004 WITH EMPTY KEY .

  CLASS-DATA layout_headder TYPE string VALUE 'LAYOUT_SETTING' ##NO_TEXT.

  DATA ms_layout TYPE REF TO data .

  CLASS-METHODS on_event_layout
    IMPORTING client        TYPE REF TO z2ui5_if_client
              layout        TYPE REF TO data
    RETURNING VALUE(result) TYPE REF TO z2ui5_if_client.

  DATA mt_t004 TYPE ty_t_t004.

  DATA mv_descr  TYPE string.
  DATA mv_layout TYPE string.
  DATA mv_def    TYPE abap_bool.
  DATA mv_usr    TYPE abap_bool.
  DATA mv_lgn    TYPE abap_bool.

  DATA mv_open   TYPE abap_bool.
  DATA MV_delete TYPE abap_bool.
  DATA mv_extended_layout TYPE abap_bool.

  CLASS-METHODS create_layout
    IMPORTING
      !table        TYPE string
    RETURNING
      VALUE(result) TYPE REF TO data.
  CLASS-METHODS render_layout_function
    IMPORTING
      !xml          TYPE REF TO z2ui5_cl_xml_view
      !client       TYPE REF TO z2ui5_if_client
    RETURNING
      VALUE(result) TYPE REF TO z2ui5_cl_xml_view.

  CLASS-METHODS init_layout
    IMPORTING
      !table        TYPE string
      !app          TYPE string
      !class        TYPE string
      !lgnum        TYPE string OPTIONAL
    RETURNING
      VALUE(result) TYPE REF TO data.

  CLASS-METHODS factory
    IMPORTING
      !layout          TYPE REF TO data
      !open_layout     TYPE abap_bool OPTIONAL
      !delete_LAYOUT   TYPE abap_bool OPTIONAL
      !extended_LAYOUT TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(result)    TYPE REF TO ZSV_cl_app_009.
  CLASS-METHODS select_layouts
    IMPORTING
      !app          TYPE string
      !class        TYPE string
      !tab          TYPE string
    RETURNING
      VALUE(result) TYPE ty_t_t004.
PROTECTED SECTION.

  DATA client        TYPE REF TO z2ui5_if_client.
  DATA mv_init       TYPE abap_bool.

  DATA mt_halign     TYPE fixvalues.
  DATA mt_importance TYPE fixvalues.

  METHODS on_init.
  METHODS render_edit.
  METHODS on_event.

  METHODS get_txt
    IMPORTING roll          TYPE string
    RETURNING VALUE(result) TYPE string.

  METHODS get_txt_l
    IMPORTING roll          TYPE string
    RETURNING VALUE(result) TYPE string.

  METHODS get_fixvalues
    IMPORTING
      rollname      TYPE string
    RETURNING
      VALUE(result) TYPE fixvalues .

  METHODS render_save.

  METHODS save_layout.
  METHODS render_open.
  METHODS get_selected_layout.

  CLASS-METHODS set_layout_Settings
    IMPORTING
      layout TYPE string
      descr  TYPE string
      class  TYPE string
      app    TYPE string
      lgnum  TYPE string
      def    TYPE string
      uname  TYPE string
      tab    TYPE string
    CHANGING
      result TYPE REF TO data.

  METHODS get_layouts.

  METHODS init_edit.

  METHODS render_delete.

PRIVATE SECTION.




ENDCLASS.



CLASS ZSV_CL_APP_009 IMPLEMENTATION.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF mv_init = abap_false.
      mv_init = abap_true.

      on_init( ).

      CASE abap_true.
        WHEN mv_open.

          get_layouts( ).

          render_open( ).

        WHEN mv_delete.

          get_layouts( ).

          render_delete( ).

        WHEN OTHERS.

          init_edit( ).

          render_edit( ).

      ENDCASE.

      client->view_model_update(  ).

    ENDIF.

    on_event( ).

  ENDMETHOD.


  METHOD on_init.

    MT_halign     = get_fixvalues( 'ZSV_ALIGN' ).
    MT_importance = get_fixvalues( 'ZSV_IMPORTANCE' ).

  ENDMETHOD.


  METHOD render_edit.


    FIELD-SYMBOLS <line> TYPE ty_s_layout.
    FIELD-SYMBOLS <layout> type any.

    DATA(popup) = Z2UI5_cl_xml_view=>factory_popup( client ).

    DATA(dialog) = popup->dialog( title         = 'Layout'
                                  afterclose    = client->_event( 'CLOSE' ) ).

    DATA(form) = dialog->simple_form( title           = 'Settings'
                                      editable        = abap_true
                                      labelSpanXL     = `4`
                                      labelSpanL      = `4`
                                      labelSpanM      = `4`
                                      labelSpanS      = `4`
                                      adjustLabelSpan = abap_false
                                      emptySpanXL     = `0`
                                      emptySpanL      = `0`
                                      emptySpanM      = `0`
                                      emptySpanS      = `0`
                                      columnsXL       = `1`
                                      columnsL        = `1`
                                      columnsM        = `1`

                                      ).
    DATA(index) = 1.

    DO.

      index = index + 1.

      ASSIGN ms_layout->* to <layout>.
      ASSIGN COMPONENT index OF STRUCTURE <layout> TO <Line>.
      IF <line> IS ASSIGNED.

        form->Toolbar(
                )->Title( text = get_txt( conv #( <Line>-rollname ) )
                          level = 'H2'
                )->text(  `- ` && <Line>-Fname ).

        form->content(  ns = 'form'
                     )->label( text   = 'Visible'
                     )->switch( type  = 'AcceptReject'
                                state = client->_bind_edit( <Line>-visible ) ).

        IF mv_extended_layout = abap_true.

          form->content(  ns = 'form'
                       )->label( text = get_txt( `ZSV_ALIGN` )
                       )->combobox(
          selectedkey = client->_bind_edit( <Line>-halign )
          items       = client->_bind_local( mt_halign )
              )->item(
                  key = '{LOW}'
                  text = '{LOW} - {DDTEXT}' ).

          form->content(  ns = 'form'
                       )->label( text = get_txt( `ZSV_IMPORTANCE` )
                       )->combobox(
          selectedkey = client->_bind_edit( <Line>-importance )
          items       = client->_bind_local( mt_importance )
              )->item(
                  key = '{LOW}'
                  text = '{LOW} - {DDTEXT}' ).

          form->content(  ns = 'form'
                       )->label( text   = 'Merge Duplicates'
                       )->switch( type  = 'AcceptReject'
                                  state = client->_bind_edit( <Line>-merge ) ).


          form->content(  ns = 'form'
                       )->label( text = 'min Screen width in px'
                       )->input( value = client->_bind_edit( <Line>-width )
                    ).

        ENDIF.

      ELSE.
        EXIT.
      ENDIF.

      UNASSIGN <line>.

    ENDDO.

    dialog->footer( )->overflow_toolbar(
          )->toolbar_spacer(
          )->Button(
                text  = 'Back'
                icon  = 'sap-icon://nav-back'
                press = client->_event( 'CLOSE' )
          )->button(
                text  = 'Save'
                press = client->_event( 'EDIT_SAVE' )
                icon  = 'sap-icon://save'
                type  = 'Emphasized'
          ).


    client->popup_display( popup->get_root( )->xml_get( ) ).


  ENDMETHOD.


  METHOD on_event.

    DATA(event) = client->get( )-event.

    CASE client->get( )-event.

      WHEN 'CLOSE'.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

     WHEN 'EDIT_SAVE'.

        render_SAVE( ).

      WHEN 'SAVE_CLOSE'.

        client->popup_destroy( ).

        render_edit(  ).

      WHEN 'SAVE_SAVE'.

        save_layout( ).

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'OPEN_SELECT'.

        get_selected_layout( ).

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'DELETE_SELECT'.

        get_selected_layout( ).

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).
    ENDCASE.

  ENDMETHOD.


  METHOD factory.

    FIELD-SYMBOLS <i_layout> TYPE any.
    FIELD-SYMBOLS <layout> TYPE any.


    result = NEW #( ).

    ASSIGN layout->* TO <I_layout>.

    CREATE DATA result->ms_layout LIKE <I_layout>.

    ASSIGN result->ms_layout->* TO <layout>.

    <layout>                   = <I_layout>.
    result->mv_open            = open_layout.
    result->mv_delete          = delete_layout.
    result->mv_extended_layout = extended_layout.

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


  METHOD create_layout.

    DATA ls_layout   TYPE ty_s_layout.
    DATA ls_setting  TYPE ty_s_header.
    DATA comp        TYPE cl_abap_structdescr=>component_table.
    DATA structdescr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS <struc> type ty_s_layout.


     comp =  VALUE cl_abap_structdescr=>component_table( BASE comp ( name = ZSV_CL_APP_009=>layout_headder
                                                              type = CAST #( cl_abap_datadescr=>describe_by_data( ls_setting ) ) ) ).




    DATA(t_dfies) = ZSV_cl_object_hlper=>get_dfies_of_table( table ).

    LOOP AT t_dfies REFERENCE INTO DATA(dfies).
      comp = VALUE cl_abap_structdescr=>component_table( BASE comp ( name = dfies->fieldname
                                                                     type = CAST #( cl_abap_datadescr=>describe_by_data( ls_layout ) ) ) ).
    ENDLOOP.


    structdescr = cl_abap_structdescr=>create( comp ).

    CREATE DATA result TYPE HANDLE structdescr.


  ENDMETHOD.


  METHOD render_layout_function.

    result = xml.

    result->Overflow_Toolbar_Menu_Button( tooltip = 'Export' icon = 'sap-icon://action-settings'
       )->_generic( `menu`
          )->_generic( `Menu`
             )->Menu_Item( text =  'Change Layout'
                           icon = 'sap-icon://edit'
                           press = client->_event( val = 'LAYOUT_EDIT' )
             )->Menu_Item( text =  'Choose Layout'
                           icon = 'sap-icon://open-folder'
                           press = client->_event( val = 'LAYOUT_OPEN' )
             )->Menu_Item( text = 'Manage Layouts'
                           icon = 'sap-icon://delete'
                           press = client->_event( val = 'LAYOUT_DELETE' ) ).


  ENDMETHOD.


  METHOD get_fixvalues.

result = ZSV_cl_object_hlper=>get_fix_values( rollname ).



*        TRY.
*            " FIXED Values Lesen
*            DATA(type) = cl_abap_typedescr=>describe_by_name( rollname ).
*            DATA(ele) =  CAST cl_abap_elemdescr( type ).
*
*            ele->get_ddic_fixed_values(
*              EXPORTING
*                p_langu        = sy-langu
*              RECEIVING
*                p_fixed_values = result
*              EXCEPTIONS
*                not_found      = 1
*                no_ddic_type   = 2
*                OTHERS         = 3
*            ).
*
*          CATCH cx_root.
*        ENDTRY.

  ENDMETHOD.


  METHOD render_save.


    DATA(popup) = Z2UI5_cl_xml_view=>factory_popup( client ).

    DATA(dialog) = popup->dialog( title      = 'Save'
                                  afterclose = client->_event( 'SAVE_CLOSE' ) ).


    DATA(form) = dialog->simple_form( title           = 'Layout'
                                      editable        = abap_true
                                      labelSpanXL     = `4`
                                      labelSpanL      = `4`
                                      labelSpanM      = `4`
                                      labelSpanS      = `4`
                                      adjustLabelSpan = abap_false
                                      ).

    form->Toolbar( )->Title( text = 'Layout' ).

    form->content(  ns = 'form'
                           )->label( 'Layout'
                           )->input( value = client->_bind_edit( mv_layout )
                           )->label( 'Description'
                           )->input( value = client->_bind_edit( mv_descr ) ).

    form->Toolbar( )->Title( text = `` ).

    form->content(  ns = 'form'
                           )->label( 'Default Layout'
                           )->switch( type = 'AcceptReject' state = client->_bind_edit( mv_def )
                           )->label( 'User specific'
                           )->switch( type = 'AcceptReject' state = client->_bind_edit( mv_usr )
                           )->label( 'Wharehousenumber specific'
                           )->switch( type = 'AcceptReject' state = client->_bind_edit( mv_lgn )
                           ).

    dialog->footer( )->overflow_toolbar(
          )->toolbar_spacer(
          )->Button(
                text  = 'Back'
                icon  = 'sap-icon://nav-back'
                press = client->_event( 'SAVE_CLOSE' )
          )->button(
                text  = 'Save'
                press = client->_event( 'SAVE_SAVE' )
                type  = 'Success'
                icon  = 'sap-icon://save' ).

    client->popup_display( popup->get_root( )->xml_get( ) ).

  ENDMETHOD.


  METHOD save_layout.

    DATA t005 TYPE ty_s_layout.
    DATA t_t005 TYPE STANDARD TABLE OF ty_s_layout.
    FIELD-SYMBOLS <line> TYPE ty_s_layout.
    FIELD-SYMBOLS <setting> TYPE ZSV_cl_app_009=>ty_s_header.

    ASSIGN COMPONENT ZSV_cl_app_009=>layout_headder OF STRUCTURE ms_layout->* TO <setting>.

    IF mv_layout IS INITIAL.
      client->message_toast_display( ZSV_CL_TEXT_HELPER=>get_t100(
                                    iv_arbgb = '0K'
                                    iv_msgnr = '535'   ) ).
      RETURN.
    ENDIF.

    IF mv_lgn = abap_true.
*      GET PARAMETER ID '/SCWM/LGN' FIELD DATA(lgnum).
    ENDIF.

    IF mv_usr = abap_true.
      DATA(user) = sy-uname.
    ENDIF.

    DATA(t004) = VALUE ZSV_t004( layout  = mv_layout
                                        class   = <setting>-class
                                        app     = <setting>-app
*                                        lgnum   = lgnum
                                        descr   = mv_descr
                                        def     = mv_def
                                        uname   = user
                                        tab     = <setting>-tab ).

    " gibt es das alyout schon?
    SELECT SINGLE layout FROM ZSV_t004
    WHERE layout = @t004-layout
    AND   tab    = @<setting>-tab
    INTO @t004-layout.

    IF sy-subrc = 0.


      " postionen lesen und löschen
      SELECT * FROM ZSV_t005
      WHERE layout = @t004-layout
      AND   tab    = @<setting>-tab
      INTO TABLE @DATA(del).

      IF sy-subrc = 0.
        DELETE ZSV_t005 FROM TABLE @del.
        COMMIT WORK AND WAIT.
      ENDIF.

    ENDIF.

    MODIFY ZSV_t004 FROM @t004.

    IF sy-subrc = 0.

      DATA(index) = 1.

      DO.

        index += 1.

        ASSIGN COMPONENT index OF STRUCTURE ms_layout->* TO <line>.
        IF <line> IS ASSIGNED.

          CLEAR: t005.
          MOVE-CORRESPONDING <line> TO t005.
          t005-layout = t004-layout.
          APPEND t005 TO t_t005.

        ELSE.
          EXIT.
        ENDIF.

        UNASSIGN <line>.

      ENDDO.

      MODIFY ZSV_t005 FROM TABLE @t_t005.

      IF sy-subrc = 0.

        COMMIT WORK AND WAIT.

        client->message_toast_display( zsv_cl_text_helper=>get_t100(
                                         iv_arbgb = '/SCWM/IT_DEVKIT'
                                         iv_msgnr = '012'   ) ).
      ENDIF.

    ENDIF.

    set_layout_settings(
      EXPORTING
        layout = CONV #( t004-layout )
        descr  = CONV #( t004-descr )
        class  = CONV #( t004-class )
        app    = CONV #( t004-app )
        lgnum  = CONV #( t004-lgnum )
        def    = CONV #( t004-def )
        uname  = CONV #( t004-uname )
        tab    = CONV #( t004-tab )
      CHANGING
        result = ms_layout ).

  ENDMETHOD.


  METHOD render_delete.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( client ).

   DATA(dialog) = popup->dialog( title = 'Layout'
                  afterclose   = client->_event( 'CLOSE' ) ).

    dialog->table(
                headertext = 'Layout'
                mode = 'SingleSelectLeft'
                items = client->_bind_edit( mt_t004 )
                )->columns(
                    )->column( )->text( 'Layout' )->get_parent(
                    )->column( )->text( get_txt_L( 'ZSV_DESCR' ) )->get_parent(
                    )->column( )->text( get_txt_L( '/SCWM/LGNUM' )

                    )->get_parent( )->get_parent(
                )->items(
                    )->column_list_item( selected = '{SELKZ}'
                        )->cells(
                            )->text( '{LAYOUT}'
                            )->text( '{DESCR}'
                            )->text( '{LGNUM}' ).

    dialog->footer( )->overflow_toolbar(
          )->toolbar_spacer(
          )->Button(
                text  = 'Back'
                icon  = 'sap-icon://nav-back'
                press = client->_event( 'CLOSE' )
          )->button(
                text  = 'Delete'
                press = client->_event( 'DELETE_SELECT' )
                type  = 'Reject'
                icon  = 'sap-icon://delete' ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.


  METHOD render_open.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( client ).

   DATA(dialog) = popup->dialog( title        = 'Layout'
                  afterclose   = client->_event( 'CLOSE' ) ).

    dialog->table(
                headertext = 'Layout'
                mode = 'SingleSelectLeft'
                items = client->_bind_edit( mt_t004 )
                )->columns(
                    )->column( )->text( 'Layout' )->get_parent(
                    )->column( )->text( get_txt_L( 'ZSV_DESCR' ) )->get_parent(
                    )->column( )->text( get_txt_L( '/SCWM/LGNUM' )

                    )->get_parent( )->get_parent(
                )->items(
                    )->column_list_item( selected = '{SELKZ}'
                        )->cells(
                            )->text( '{LAYOUT}'
                            )->text( '{DESCR}'
                            )->text( '{LGNUM}' ).

    dialog->footer( )->overflow_toolbar(
          )->toolbar_spacer(
          )->Button(
                text  = 'Back'
                icon  = 'sap-icon://nav-back'
                press = client->_event( 'CLOSE' )
          )->button(
                text  = 'Open'
                icon  = 'sap-icon://accept'
                press = client->_event( 'OPEN_SELECT' )
                type  = 'Emphasized' ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.


  METHOD select_layouts.

    SELECT  * FROM ZSV_t004
    WHERE class   = @class
    AND   app     = @app
    and   tab     = @tab
    INTO CORRESPONDING FIELDS OF TABLE @result.

  ENDMETHOD.


  METHOD get_selected_layout.

    FIELD-SYMBOLS <struc> TYPE ty_s_layout.

    DATA(t004) = VALUE #( mt_t004[ selkz = abaP_true ] OPTIONAL ).

    CHECK t004 IS NOT INITIAL.

    SELECT SINGLE * FROM ZSV_t004
    WHERE layout = @t004-layout
    and   tab    = @t004-tab
    INTO @t004.

    SELECT * FROM ZSV_t005
    WHERE layout = @t004-layout
    and   tab    = @t004-tab
    INTO TABLE @DATA(t_t005).

    CHECK sy-subrc = 0.

    DELETE  ZSV_t004 FROM @t004.
    DELETE  ZSV_t005 FROM TABLE @t_t005.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD init_layout.

    FIELD-SYMBOLS <struc> TYPE ty_s_layout.
    FIELD-SYMBOLS <setting> TYPE ty_s_header.

    result = create_layout( table ).

    DATA(t004) = select_layouts( app   = app
                                 class = class
                                 tab   = table ).

    " DEFAULT LGNUM AND USER
    DATA(default) = VALUE #( t004[ app = app class = class tab = table lgnum = CONV #( lgnum ) def = abap_true uname = sy-uname ] OPTIONAL ).

    IF default IS INITIAL.
      " DEFAULT USER
      default = VALUE #( t004[ app = app class = class tab = table def = abap_true uname = sy-uname ] OPTIONAL ).
      IF default IS INITIAL.
        " DEFAULT LGNUM
        default  = VALUE #( t004[ app = app class = class tab = table lgnum = CONV #( lgnum ) def = abap_true ] OPTIONAL ).
        IF default IS INITIAL.
          " DEFAULT
          default  = VALUE #( t004[ app = app class = class tab = table def = abap_true ] OPTIONAL ).
        ENDIF.
      ENDIF.
    ENDIF.


    DATA(t_dfies) = ZSV_cl_object_hlper=>get_dfies_of_table( table ).

    IF default-layout IS NOT INITIAL.

      SELECT * FROM ZSV_t005
      WHERE layout = @default-layout
      AND   tab    = @default-tab
      INTO TABLE @DATA(t_t005).


      LOOP AT t_dfies REFERENCE INTO DATA(dfies).

        ASSIGN COMPONENT dfies->fieldname OF STRUCTURE result->* TO <struc>.
        CHECK sy-subrc = 0.

        READ TABLE t_t005 REFERENCE INTO DATA(t005) WITH KEY fname = dfies->fieldname.

        IF sy-subrc = 0.
          MOVE-CORRESPONDING t005->* TO <struc>.
        ELSE.
          <struc>-layout     = 'Default'.
          <struc>-halign     = 'Initial'.
          <struc>-importance = 'None'.
          <struc>-rollname   = dfies->rollname.
          <struc>-fname      = dfies->fieldname.
          <struc>-tab        = table.
        ENDIF.

      ENDLOOP.

      ASSIGN COMPONENT ZSV_cl_app_009=>layout_headder OF STRUCTURE result->* TO <setting>.
      CHECK <setting> IS ASSIGNED.
      MOVE-CORRESPONDING default TO <setting>.
      RETURN.

    ENDIF.

" Default Layout

    DATA(index) = 0.

    LOOP AT t_dfies REFERENCE INTO dfies.

      index += 1.

      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE result->* TO <struc>.

      CHECK <struc> IS ASSIGNED.

      IF index <= 10.
        <struc>-visible = abap_true.
      ENDIF.

      IF dfies->fieldname = 'MANDT'.
        <struc>-visible = abap_false.
      ENDIF.

      <struc>-layout     = 'Default'.
      <struc>-halign     = 'Initial'.
      <struc>-importance = 'None'.
      <struc>-rollname   = dfies->rollname.
      <struc>-fname      = dfies->fieldname.
      <struc>-tab        = table.

    ENDLOOP.

    ASSIGN COMPONENT ZSV_cl_app_009=>layout_headder OF STRUCTURE result->* TO <setting>.
    CHECK <setting> IS ASSIGNED.
    <setting>-layout = 'Default'.
    <setting>-descr  = 'System generated Layout'.
    <setting>-def    = abap_true.
    <setting>-class  = class.
    <setting>-app    = app.
    <setting>-tab    = table.

  ENDMETHOD.


  METHOD set_layout_Settings.

    FIELD-SYMBOLS <setting> TYPE ZSV_cl_app_009=>ty_s_header.

    ASSIGN COMPONENT ZSV_cl_app_009=>layout_headder OF STRUCTURE result->* TO <setting>.

    <setting>-LAYOUT = LAYOUt.
    <setting>-DESCR  = DESCR.
    <setting>-CLASS  = CLASS.
    <setting>-APP    = APP.
    <setting>-LGNUM  = LGNUM.
    <setting>-DEF    = DEF.
    <setting>-UNAME  = UNAME.
    <setting>-tab    = tab.

  ENDMETHOD.


  METHOD get_layouts.

    FIELD-SYMBOLS <setting> TYPE ZSV_cl_app_009=>ty_s_header.

    ASSIGN COMPONENT ZSV_cl_app_009=>layout_headder OF STRUCTURE ms_layout->* TO <setting>.

    mt_t004 = select_layouts(
      app   = CONV #( <setting>-app )
      class = CONV #( <setting>-class )
      tab   = CONV #( <setting>-tab ) ).


    CHECK mt_t004 IS NOT INITIAL.

    DATA(selkz) = REF #( mt_t004[ 1 ]-selkz OPTIONAL ).
    selkz->* = abaP_true.

  ENDMETHOD.


  METHOD init_edit.

    FIELD-SYMBOLS <setting> TYPE ZSV_cl_app_009=>ty_s_header.

    ASSIGN COMPONENT ZSV_cl_app_009=>layout_headder OF STRUCTURE ms_layout->* TO <setting>.

    mv_layout = <setting>-layout.
    mv_descr  = <setting>-descr.
    mv_def    = <setting>-def.

    mv_lgn    = cond #( when <setting>-lgnum is not INITIAL then abap_true ).
    mv_usr    = cond #( when <setting>-uname is not INITIAL then abap_true ).

  ENDMETHOD.


  METHOD on_event_layout.

    result = client.

    CASE result->get( )-event.

      WHEN 'LAYOUT_OPEN'.
   client->view_destroy( ).
        result->nav_app_call( ZSV_cl_app_009=>factory( layout      = layout
                                                              open_layout = abap_true   ) ).

      WHEN 'LAYOUT_EDIT'.
   client->view_destroy( ).
        result->nav_app_call( ZSV_cl_app_009=>factory( layout = layout
                                                              extended_layout = abap_true
                                                               ) ).


      WHEN 'LAYOUT_DELETE'.
   client->view_destroy( ).
        result->nav_app_call( ZSV_cl_app_009=>factory( layout = layout
                                                              delete_layout = abap_true ) ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
