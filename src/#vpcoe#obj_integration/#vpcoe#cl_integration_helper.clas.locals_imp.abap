*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_rfc_read_table DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_rfc_read_table.
ENDCLASS.

CLASS lcl_rfc_read_table IMPLEMENTATION.
  METHOD lif_rfc_read_table~call_fuba_rfc_read_table.
* isolated coding
* read table content
* with rfc destination
    DATA:lv_rfc_message TYPE /vpcoe/rfc_message,
         lv_tablename   TYPE char40.

    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION iv_rfc_destination
      EXPORTING
        query_table           = iv_tablename
        delimiter             = '|'
*** Begin of correction 2696821 - 17.10.2018
        rowcount              = iv_rowcount
        rowskips              = iv_rowskips
*** End of correction 2696821 - 17.10.2018
      TABLES
        options               = it_selopt
        fields                = ct_fields
        data                  = ct_content
      EXCEPTIONS
        table_not_available   = 1
        table_without_data    = 2
        option_not_valid      = 3
        field_not_valid       = 4
        not_authorized        = 5
        data_buffer_exceeded  = 6
        system_failure        = 7 MESSAGE lv_rfc_message
        communication_failure = 8 MESSAGE lv_rfc_message
        OTHERS                = 9.

    ev_subrc = sy-subrc.
    ev_rfc_message = lv_rfc_message.

  ENDMETHOD.
ENDCLASS.
