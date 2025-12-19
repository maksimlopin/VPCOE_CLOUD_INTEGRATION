*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
INTERFACE lif_rfc_read_table.
  METHODS call_fuba_rfc_read_table
    IMPORTING VALUE(iv_tablename)        TYPE tabname
              VALUE(iv_struct_tablename) TYPE tabname
              VALUE(iv_rfc_destination)  TYPE rfc_dest
              VALUE(it_selopt)           TYPE /vpcoe/t_sel_options
*** Begin of correction 2696821 - 17.10.2018
              VALUE(iv_rowcount)         TYPE i OPTIONAL
              VALUE(iv_rowskips)         TYPE i OPTIONAL
*** End of correction 2696821 - 17.10.2018
    EXPORTING VALUE(ev_subrc)            TYPE sy-subrc
              VALUE(ev_rfc_message)      TYPE /vpcoe/rfc_message
    CHANGING  VALUE(ct_fields)           TYPE /vpcoe/t_db_fields
              VALUE(ct_content)          TYPE STANDARD TABLE.
ENDINTERFACE.
