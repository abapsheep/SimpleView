interface ZSV_IF_000
  public .


  data MV_VIEW_DISPLAY type ABAP_BOOLEAN .   " rerender if Sub App
  data MO_PARENT_VIEW type ref to Z2UI5_CL_XML_VIEW .

  methods set_app_data
    importing
      !DATA type STRING .
endinterface.
