CLASS /vpcoe/cl_ehfnd_ehssub_proxy DEFINITION
 PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /vpcoe/if_ehfnd_ehssub_proxy.

    ALIASES get_next_subid
      FOR /vpcoe/if_ehfnd_ehssub_proxy~get_next_subid.

*"* public components of class CL_EHFND_EHSSUB_PROXY
*"* do not include other source files here!!!
    CONSTANTS gc_report_status_released TYPE string VALUE 'W7' ##NO_TEXT.
    CONSTANTS gc_report_status_historic TYPE string VALUE 'WD' ##NO_TEXT.

    CLASS-METHODS get_environment_parameter
      IMPORTING
        !iv_parameter TYPE char30
      EXPORTING
        !ev_value     TYPE text132 .
    CLASS-METHODS check_cas_number
      IMPORTING
        !iv_cas_number     TYPE char20
      EXPORTING
        !ev_new_cas_number TYPE char20
        !ev_format_ok      TYPE abap_bool .
    CLASS-METHODS check_ec_number
      IMPORTING
        !iv_ec_number     TYPE char9
      EXPORTING
        !ev_new_ec_number TYPE char9
        !et_msg           TYPE /vpcoe/t_t100_message .
    CLASS-METHODS check_einecs_number
      IMPORTING
        !iv_einecs_number     TYPE char9
      EXPORTING
        !ev_new_einecs_number TYPE char9
        !ev_format_ok         TYPE abap_bool .
    CLASS-METHODS check_identifier
      IMPORTING
        !iv_ident        TYPE text132
        !iv_check_method TYPE char30
      EXPORTING
        !ev_new_ident    TYPE text132
      CHANGING
        !ct_message      TYPE ehcsmt_spc_msg .
    CLASS-METHODS check_identifier_by_type_cat
      IMPORTING
        !iv_idtype    TYPE eseidtype
        !iv_idcat     TYPE eseidcat
        !iv_ident     TYPE eseident
      EXPORTING
        !ev_new_ident TYPE eseident
      CHANGING
        !ct_message   TYPE ehcsmt_spc_msg .
    CLASS-METHODS delete_specifications_via_api
      IMPORTING
        !is_scenario           TYPE /vpcoe/s_ehs_scenario_type
      EXPORTING
        !et_error              TYPE /vpcoe/t_ehs_api_msg
        !ev_flg_internal_error TYPE esp1_boolean
        !ev_flg_error          TYPE esp1_boolean
        !ev_flg_warning        TYPE esp1_boolean
      CHANGING
        !ct_sub_header         TYPE /vpcoe/t_ehs_api_head OPTIONAL
        !ct_material           TYPE /vpcoe/t_ehs_api_material OPTIONAL
        !ct_identifier         TYPE /vpcoe/t_ehs_api_ide OPTIONAL .
    CLASS-METHODS extract_phrase_id
      IMPORTING
        !iv_phrase_key      TYPE csequence
      RETURNING
        VALUE(rv_phrase_id) TYPE string .
    CLASS-METHODS find_pure_sub_by_ident_recn
      IMPORTING
        !it_idents          TYPE esprh_apiri_tab_type
        !i_flg_fuzzy        TYPE eseboole OPTIONAL
        !iv_pure_sub_subcat TYPE esesubcat
      CHANGING
        !ct_recns           TYPE /vpcoe/t_key_recn
      RAISING
        cx_abap_invalid_name
        cx_abap_invalid_value .
    CLASS-METHODS get_idents_at_pos
      IMPORTING
        !it_ident_listing   TYPE /vpcoe/t_ident_listin
        !its_ident_all      TYPE /vpcoe/t_bapi1077ri
        !iv_spec_recno_root TYPE eserecn
        !iv_position        TYPE i
      EXPORTING
        !et_ident_at_pos    TYPE /vpcoe/t_bapi1077ri .
    CLASS-METHODS get_last_lock_error_from_api
      EXPORTING
        VALUE(ev_lock_info) TYPE c .
    CLASS-METHODS get_mime_type
      IMPORTING
        !iv_dappl           TYPE dappl
        !iv_filename        TYPE sdok_filnm
      RETURNING
        VALUE(rv_mime_type) TYPE w3conttype .
    CLASS-METHODS get_subcat_by_recn_subid
      IMPORTING
        !iv_subid        TYPE /vpcoe/substance_id OPTIONAL
        !iv_spc_hdr_recn TYPE eserecn OPTIONAL
        !iv_spc_hdr_actn TYPE eseactn OPTIONAL
        !iv_key_date     TYPE rmsae_gpdate DEFAULT sy-datum
        !iv_change_nr    TYPE eseaennr OPTIONAL
        !iv_langu        TYPE langu DEFAULT sy-langu
      EXPORTING
        !ev_subcat_name  TYPE esesubnam
        !ev_subcat       TYPE esesubcat .
    CLASS-METHODS is_subid_valid
      IMPORTING
        !iv_spec_type      TYPE esesubcat OPTIONAL
        !iv_subid          TYPE esesubid
      EXPORTING
        !ev_subid_is_valid TYPE boole_d
        !ev_error_ind      TYPE boole_d
        !es_error_msg      TYPE symsg
        !ev_nrobj          TYPE nrobj
        !ev_nrnr_external  TYPE nrnr
        !ev_nrnr_internal  TYPE nrnr .
    CLASS-METHODS modify_specifications_via_api
      IMPORTING
        !is_scenario            TYPE /vpcoe/s_ehs_scenario_type
        !iv_flg_create_mode     TYPE boolean OPTIONAL
      EXPORTING
        !et_error               TYPE /vpcoe/t_ehs_api_msg
        !ev_flg_internal_error  TYPE esp1_boolean
        !ev_flg_error           TYPE esp1_boolean
        !ev_flg_warning         TYPE esp1_boolean
      CHANGING
        !ct_identifier          TYPE /vpcoe/t_ehs_api_ide OPTIONAL
        !ct_identifier_longtext TYPE /vpcoe/t_ehs_api_ident_lt OPTIONAL
        !ct_sub_header          TYPE /vpcoe/t_ehs_api_head OPTIONAL
        !ct_material            TYPE /vpcoe/t_ehs_api_material OPTIONAL .
    CLASS-METHODS read_specifications_via_api
      IMPORTING
        !is_scenario            TYPE /vpcoe/s_ehs_scenario_type
        !iv_flg_with_inh_data   TYPE boolean DEFAULT abap_true
      EXPORTING
        !et_identifier_longtext TYPE /vpcoe/t_ehs_api_ident_lt
        !et_error               TYPE /vpcoe/t_ehs_api_msg
        !ev_flg_internal_error  TYPE esp1_boolean
        !ev_flg_error           TYPE esp1_boolean
        !ev_flg_warning         TYPE esp1_boolean
      CHANGING
        !ct_sub_header          TYPE /vpcoe/t_ehs_api_head
        !ct_material            TYPE /vpcoe/t_ehs_api_material OPTIONAL
        !ct_identifier          TYPE /vpcoe/t_ehs_api_ide OPTIONAL .
    CLASS-METHODS recns_to_subids
      CHANGING
        !ct_recn_subid TYPE /vpcoe/t_spc_recn_subid .
    CLASS-METHODS recn_get_next
      RETURNING
        VALUE(rv_recn) TYPE eserecn .
    CLASS-METHODS recn_to_subid
      IMPORTING
        !iv_recn          TYPE eserecn
      EXPORTING
        !ev_subid         TYPE /vpcoe/substance_id
        !ev_not_found_ind TYPE boole_d .
    CLASS-METHODS reset_spec_buffers .
    CLASS-METHODS save_spec_to_db_for_api .
    CLASS-METHODS subids_to_recns
      CHANGING
        !ct_recn_subid TYPE /vpcoe/t_spc_recn_subid .
    CLASS-METHODS subid_to_recn
      IMPORTING
        !iv_subid         TYPE /vpcoe/substance_id
      EXPORTING
        !ev_recn          TYPE eserecn
        !ev_not_found_ind TYPE boole_d .
    METHODS add_message_to_msgtable
      IMPORTING
        !iv_msgtype  TYPE symsgty DEFAULT 'E'
        !iv_msgid    TYPE symsgid DEFAULT 'CM_EHFND_EHSSUB_PRXY'
        !iv_msgno    TYPE symsgno
        !iv_msgv1    TYPE any OPTIONAL
        !iv_msgv2    TYPE any OPTIONAL
        !iv_msgv3    TYPE any OPTIONAL
        !iv_msgv4    TYPE any OPTIONAL
      CHANGING
        !xt_messages TYPE bapiret2_t .
    METHODS constructor .
    METHODS get_documents_by_substance
      IMPORTING
        !iv_rfc_dest     TYPE rfc_dest
        !iv_external_src TYPE char40
        !iv_sbgvid       TYPE eseldepid DEFAULT 'IBD_MSDS'
      EXPORTING
        !et_files        TYPE cvapi_tbl_doc_files
        !et_content      TYPE dms_tbl_drao
        !et_return       TYPE bapiret2_t .
    METHODS get_document_headers
      IMPORTING
        !iv_rfc_dest       TYPE rfc_dest
        !it_doc_parameters TYPE /vpcoe/t_doc_param
        !it_sub_header     TYPE /vpcoe/t_bapi1077rh
      EXPORTING
        !ets_doc_header    TYPE /vpcoe/t_doc_header
        !et_return         TYPE bapiret2_t .
    METHODS get_ident_listings_chm_subst
      IMPORTING
        !iv_rfc_dest      TYPE rfc_dest
        !iv_idlid         TYPE /vpcoe/sub_ident_listing
      EXPORTING
        !et_ident_listing TYPE /vpcoe/t_ident_listin
        !et_ident_header  TYPE /vpcoe/t_bapi1077ri .
    METHODS get_ident_listings_lsub_subst
      IMPORTING
        !iv_rfc_dest      TYPE rfc_dest
        !iv_idlid         TYPE /vpcoe/sub_ident_listing
      EXPORTING
        !et_ident_listing TYPE /vpcoe/t_ident_listin
        !et_ident_header  TYPE /vpcoe/t_bapi1077ri .
    METHODS get_keytab_from_hitlist
      IMPORTING
        !iv_rfc_dest     TYPE rfc_dest
        !iv_hit_grpid    TYPE /vpcoe/hit_grpid
        !iv_hit_grpobjid TYPE /vpcoe/hit_grpobjid
      EXPORTING
        !et_substances   TYPE /vpcoe/t_ehs_hitpos
        !et_messages     TYPE bapiret2_t .
    METHODS read_ehs_phrases
      IMPORTING
        !iv_rfc_dest          TYPE rfc_dest
        !iv_selection_set     TYPE esephrsel
      EXPORTING
        !et_return            TYPE bapirettab
        !et_phrase_list       TYPE /vpcoe/t_bapiphr_01
        !et_selectionset_list TYPE /vpcoe/t_bapiphr_02 .
    METHODS read_ehs_phrase_details
      IMPORTING
        !iv_rfc_dest             TYPE rfc_dest
        !iv_scenario             TYPE bapistdtyp-scenario
        !iv_flg_phrase_header    TYPE bapistdtyp-boolean
        !iv_flg_phrase_text      TYPE bapistdtyp-boolean
        !iv_flg_phrase_longtext  TYPE bapistdtyp-boolean OPTIONAL
      EXPORTING
        !et_phrase_text_itab     TYPE /vpcoe/t_bapi1091pp
        !et_phrase_longtext_itab TYPE /vpcoe/t_bapi1091plt
      CHANGING
        !xt_phrase_header_itab   TYPE /vpcoe/t_bapi1091ph .
    METHODS read_ehs_substances
      IMPORTING
        !iv_rfc_dest                TYPE rfc_dest
        !iv_scenario                TYPE bapistdtyp-scenario DEFAULT '01'
        !iv_key_date                TYPE rcgaddinf-valdat DEFAULT sy-datum
        !iv_flg_header              TYPE abap_bool OPTIONAL
        !iv_flg_header_usage        TYPE abap_bool OPTIONAL
        !iv_flg_refsubs             TYPE abap_bool OPTIONAL
        !iv_flg_ident               TYPE abap_bool OPTIONAL
        !iv_flg_ident_longtext      TYPE abap_bool OPTIONAL
        !iv_flg_ident_usage         TYPE abap_bool OPTIONAL
        !iv_flg_matjoin             TYPE abap_bool OPTIONAL
        !iv_flg_tplrel              TYPE abap_bool OPTIONAL
        !iv_flg_properties          TYPE abap_bool OPTIONAL
        !iv_flg_prop_data           TYPE abap_bool OPTIONAL
        !iv_flg_prop_details        TYPE abap_bool OPTIONAL
        !iv_flg_prop_ftext_longtext TYPE abap_bool OPTIONAL
      EXPORTING
        !ev_flg_abort_on_error      TYPE abap_bool
        !et_return                  TYPE bapirettab
      CHANGING
        !xt_sub_header              TYPE esy_tt_bapi1077rh OPTIONAL
        !xt_refsubs                 TYPE /vpcoe/t_bapi1077rr OPTIONAL
        !xt_ident_header            TYPE esy_tt_bapi1077ri OPTIONAL
        !xt_ident_longtext          TYPE esy_tt_bapi1077il OPTIONAL
        !xt_ident_sublist           TYPE /vpcoe/t_bapi1077rl OPTIONAL
        !xt_matjoin                 TYPE /vpcoe/t_bapi1077mj OPTIONAL
        !xt_tplrel                  TYPE /vpcoe/t_bapi1077tplrel OPTIONAL
        !xt_prop_header             TYPE esy_tt_bapi1077vh OPTIONAL
        !xt_prop_val                TYPE esy_tt_bapi1077va OPTIONAL
        !xt_prop_data               TYPE esy_tt_bapi1077pr OPTIONAL
        !xt_prop_component          TYPE esy_tt_bapi1077vp OPTIONAL
        !xt_prop_usage              TYPE esy_tt_bapi1077du OPTIONAL
        !xt_prop_reliability        TYPE /vpcoe/t_bapi1077dr OPTIONAL
        !xt_prop_source             TYPE /vpcoe/t_bapi1077ds OPTIONAL
        !xt_prop_ftext              TYPE esy_tt_bapi1077df OPTIONAL
        !xt_prop_ftext_longtext     TYPE esy_tt_bapi1077fl OPTIONAL .
    METHODS shlp_exit_substance_id_sel
      IMPORTING
        !iv_validity_date   TYPE uxx_validity_date
        !iv_substance_type  TYPE /vpcoe/substance_type OPTIONAL
        !iv_substance_id    TYPE /vpcoe/substance_id
        !iv_identifier_cat  TYPE /vpcoe/identifier_cat OPTIONAL
        !iv_identifier_type TYPE /vpcoe/identifier_type OPTIONAL
        !iv_identifier      TYPE /vpcoe/substance_identifier OPTIONAL
        !iv_ident_listing   TYPE /vpcoe/sub_ident_listing OPTIONAL
      EXPORTING
        !et_substance       TYPE /vpcoe/t_substance .
    METHODS convert_subid_to_int
      IMPORTING
        !iv_subid       TYPE esesubid
      RETURNING
        VALUE(rv_subid) TYPE esesubid .
    CLASS-METHODS is_spec_conv_active
      RETURNING
        VALUE(rv_is_active) TYPE boole_d .
  PROTECTED SECTION.
*"* protected components of class CL_EHFND_EHSSUB_PROXY
*"* do not include other source files here!!!
  PRIVATE SECTION.

    TYPES:
*"* private components of class CL_EHFND_EHSSUB_PROXY
*"* do not include other source files here!!!
      BEGIN OF lty_recn_subid,
        recn  TYPE eserecn,
        subid TYPE esesubid,
        tabix TYPE sytabix,
      END OF lty_recn_subid .

    DATA go_table_reader TYPE REF TO /vpcoe/cl_integration_helper .
    CONSTANTS:
      BEGIN OF gc_tabname,
        genvar      TYPE tabname VALUE 'ESTLP',
        genvar_desc TYPE tabname VALUE 'ESTLR',
        rephead     TYPE tabname VALUE 'ESTDH',             "#EC NOTEXT
        tcghit      TYPE tabname VALUE 'TCGHIT',
        tcghitpos   TYPE tabname VALUE 'TCGHITPOS',
        idlisting   TYPE tabname VALUE 'TCG28',
      END OF gc_tabname .

    METHODS extract_identifier
      IMPORTING
        !iv_ident_listing    TYPE /vpcoe/sub_ident_listing
        !it_ident_header     TYPE esy_tt_bapi1077ri
        !iv_recn             TYPE eserecn
      RETURNING
        VALUE(rv_identifier) TYPE /vpcoe/substance_identifier .
    METHODS get_ident_listing_for_subst
      IMPORTING
        !iv_rfc_dest            TYPE rfc_dest
        !iv_idlid               TYPE /vpcoe/sub_ident_listing
      RETURNING
        VALUE(rt_ident_listing) TYPE /vpcoe/t_ident_listin
      RAISING
        cx_abap_invalid_name .
ENDCLASS.



CLASS /VPCOE/CL_EHFND_EHSSUB_PROXY IMPLEMENTATION.


  METHOD /vpcoe/if_ehfnd_ehssub_proxy~get_next_subid.
* Purpose: Get the next internal number from internal number range

    DATA: lv_flg_determine_number TYPE boole_d,
          lv_error                TYPE boole_d,
          lv_returncode           TYPE inri-returncode.

* Method body -----------------------------------------------------

    CLEAR: ev_subid,
           ev_error_ind,
           es_error_msg.

    IF ( iv_spec_type IS NOT INITIAL ).
      CALL FUNCTION 'C14I_SUBID_DRAW_CUSTOMIZED'
        EXPORTING
          i_subcat                  = iv_spec_type
          i_flg_with_message        = abap_false
        IMPORTING
          e_subid                   = ev_subid
        EXCEPTIONS
          tcg31_not_complete        = 1
          number_range_not_found    = 2
          number_range_not_internal = 3
          interval_not_found        = 4
          interval_overflow         = 5
          quantity_is_0             = 6
          internal_error            = 7
          OTHERS                    = 8.
      IF ( sy-subrc = 1 ).
        lv_flg_determine_number = abap_true.
      ELSEIF ( sy-subrc <> 0 ).
        lv_error = abap_true.
      ENDIF.
    ELSE.
      lv_flg_determine_number  = abap_true.
    ENDIF.

    "Check whether the system is a cloud system
    TRY.
        /vpcoe/cl_integration_helper=>is_s4h_cloud( RECEIVING rv_is_s4h_cloud = DATA(lv_is_s4h_cloud) ).
      CATCH cx_root.
        lv_is_s4h_cloud = abap_false.
    ENDTRY.

    "Determine the internal number -> number range ESN_SUBID
    IF ( lv_is_s4h_cloud = abap_true )
       AND ( lv_flg_determine_number = abap_true ).
      CALL FUNCTION 'C149_NUMBER_DRAW_FROM_NUMRANGE'
        EXPORTING
          i_numrange_object       = 'ESN_SUBID'
          i_numrange_interval     = '0I'
        IMPORTING
          e_number                = ev_subid
        EXCEPTIONS
          number_range_not_found  = 1
          number_range_not_intern = 2
          interval_not_found      = 3
          interval_overflow       = 4
          quantity_is_0           = 5
          OTHERS                  = 6.
      IF ( sy-subrc <> 0 ).
        lv_error = abap_true.
      ENDIF.
    ELSEIF ( ( iv_spec_type IS INITIAL )
             AND ( lv_is_s4h_cloud = abap_false ) )
           OR ( ( iv_spec_type IS NOT INITIAL )
                AND ( lv_is_s4h_cloud = abap_false )
                AND ( lv_flg_determine_number = abap_true ) ).
      lv_error = abap_true.
    ENDIF.

    "Error handling
    IF ( lv_error = abap_true ).
      ev_error_ind = abap_true.
      "Internal number could not checked.
      "Internal number could not determined.
      MESSAGE e105(cm_ehfnd_ehssub_prxy)
        WITH iv_spec_type
        INTO DATA(lv_msg).
      es_error_msg = /vpcoe/cl_integration_helper=>get_msg_from_sys_fields( ).
    ENDIF.

  ENDMETHOD.


  METHOD add_message_to_msgtable.
*----------------------------------------------------------------------*
* Local Data
*----------------------------------------------------------------------*
    DATA ls_message       LIKE LINE OF xt_messages.


*----------------------------------------------------------------------*
* Function Body
*----------------------------------------------------------------------*
* NOTE: do not refresh XT_MESSAGES

    CLEAR ls_message.
    ls_message-type       = iv_msgtype.
    ls_message-id         = iv_msgid.
    ls_message-number     = iv_msgno.
    ls_message-message_v1 = iv_msgv1.
    ls_message-message_v2 = iv_msgv2.
    ls_message-message_v3 = iv_msgv3.
    ls_message-message_v4 = iv_msgv4.

    APPEND ls_message TO xt_messages.

  ENDMETHOD.


  METHOD check_cas_number.

    DATA:
      lv_cas_ident           TYPE /vpcoe/substance_identifier,
      lv_cas_ident_new_value TYPE /vpcoe/substance_identifier,
      lt_msg                 TYPE ehcsmt_spc_msg.


    " check the identifier value for the EINECS number
    lv_cas_ident = iv_cas_number.

    check_identifier(
      EXPORTING
        iv_check_method  = 'C142_CASNO'
        iv_ident     = lv_cas_ident                                " Identifier
      IMPORTING
        ev_new_ident = lv_cas_ident_new_value                      " Identifier
      CHANGING
        ct_message   = lt_msg                                         " Messages Table Type
    ).

    " error message
    IF lt_msg IS INITIAL.
      ev_format_ok = abap_true.
    ELSE.
      ev_format_ok = abap_false.
    ENDIF.



  ENDMETHOD.


  METHOD check_ec_number.
* Begin Correction 3051352 29.04.2021 ********************************

    DATA:
      lv_ec_ident           TYPE /vpcoe/substance_identifier,
      lv_ec_ident_new_value TYPE /vpcoe/substance_identifier,
      lt_msg                TYPE ehcsmt_spc_msg,
      ls_msg                TYPE ehcsms_spc_msg,
      ls_ehfnd_msg          TYPE /vpcoe/s_t100_message.

    " check the identifier value for the EC number
    lv_ec_ident = iv_ec_number.

    check_identifier(
      EXPORTING
        iv_check_method  = 'C142_ECNO'
        iv_ident     = lv_ec_ident                                    " Identifier
      IMPORTING
        ev_new_ident = lv_ec_ident_new_value                          " Identifier
      CHANGING
        ct_message   = lt_msg                                         " Messages Table Type
    ).

*   mapping of EHS Classic error messages to EHSM error messages
    REFRESH et_msg.
    LOOP AT lt_msg INTO ls_msg.
      CLEAR ls_ehfnd_msg.
      ls_ehfnd_msg-msgid       = ls_msg-msgid.
      ls_ehfnd_msg-msgno       = ls_msg-msgno.
      ls_ehfnd_msg-severity    = ls_msg-msgty.
      ls_ehfnd_msg-parameter_1 = ls_msg-msgv1.
      ls_ehfnd_msg-parameter_2 = ls_msg-msgv2.
      ls_ehfnd_msg-parameter_3 = ls_msg-msgv3.
      ls_ehfnd_msg-parameter_4 = ls_msg-msgv4.
      APPEND ls_ehfnd_msg TO et_msg.
    ENDLOOP.

* End Correction 3051352 29.04.2021 **********************************
  ENDMETHOD.


  METHOD check_einecs_number.

    DATA:
      lv_einecs_ident           TYPE /vpcoe/substance_identifier,
      lv_einecs_ident_new_value TYPE /vpcoe/substance_identifier,
      lt_msg                    TYPE ehcsmt_spc_msg.


    " check the identifier value for the EINECS number
    lv_einecs_ident = iv_einecs_number.

    check_identifier(
      EXPORTING
        iv_check_method  = 'C142_EINECSNO'
        iv_ident     = lv_einecs_ident                                " Identifier
      IMPORTING
        ev_new_ident = lv_einecs_ident_new_value                      " Identifier
      CHANGING
        ct_message   = lt_msg                                         " Messages Table Type
    ).

    " error message
    IF lt_msg IS INITIAL.
      ev_format_ok = abap_true.
    ELSE.
      ev_format_ok = abap_false.
    ENDIF.



  ENDMETHOD.


  METHOD check_identifier.
*--------------------------------------------------------------------*
* Declarations
*--------------------------------------------------------------------*
    DATA:
      lr_msg           TYPE REF TO ehcsms_spc_msg,
      lv_tabix         TYPE sytabix,
      ls_msg           TYPE symsg,
      ls_add_params    TYPE esp7_tcg23_add_params_type,
      ls_char120       TYPE char120,
      ls_tcg21         TYPE tcg21,                          "#EC NEEDED
      lv_dummy         TYPE char1,                          "#EC NEEDED
      lv_status        TYPE i,
      lv_msgv1         TYPE sy-msgv1,
      lv_msgv2         TYPE sy-msgv2,
      lv_msgv3         TYPE sy-msgv3,
      lv_msg_text(255) TYPE c,
      lv_msg_line      TYPE symsgv,
      lv_msg_rest(255) TYPE c,
      lv_msgv_size     TYPE i,
      lv_loop_count    TYPE i.

*--------------------------------------------------------------------*
* Method Implementation
*--------------------------------------------------------------------*

    CLEAR ev_new_ident.

    " suppress dialogs
    ls_add_params-flg_dialog = abap_false.

    CALL FUNCTION iv_check_method
      EXPORTING
        i_ident      = iv_ident
      IMPORTING
        e_ident      = ev_new_ident
        e_status     = lv_status
        e_mesg       = ls_char120
      CHANGING
        x_add_params = ls_add_params.


    IF ( lv_status = 0 OR lv_status = 1 ) AND ls_char120 IS NOT INITIAL.
      APPEND INITIAL LINE TO ct_message REFERENCE INTO lr_msg.
      MOVE-CORRESPONDING ls_msg TO lr_msg->*.
      lr_msg->msgty     = if_ehcsm_spc_const_c=>gc_msg_warning.
      lr_msg->msgid     = 'C$'.
      lr_msg->msgno     = 246.
      lr_msg->fieldname = 'IDENT'.                          "#EC NEEDED
      lr_msg->msg_index = lv_tabix.
      lr_msg->msgv1     = iv_ident.
      lr_msg->msgv2     = ls_char120.
      lr_msg->msgv3     = ev_new_ident.
* Begin Correction 3051352 29.04.2021 ********************************
      IF iv_ident = ev_new_ident.
*       check digit not unique. Set variable 4 of message table so that
*       we can later decide whether there should be a warning or error
*       message set
        lr_msg->msgv4     = 'CHECK_DIGIT_NOT_UNIQUE'.
      ENDIF.
* End Correction 3051352 29.04.2021 **********************************
      MESSAGE w246(c$) WITH iv_ident ls_char120 ev_new_ident INTO lv_dummy.


*     In case of an error the error message returned in e_mesg could be longer than
*     one single message parameter. So in that case we have to split the message
*     into parts that each fit into one message parameter. Then we can display
*     the error message text via message 204(C$).
    ELSEIF lv_status = -1 AND ls_char120 IS NOT INITIAL.

      CLEAR: lv_msgv1,
             lv_msgv2,
             lv_msgv3,
             lv_msgv_size,
             lv_loop_count,
             lv_msg_text,
             lv_msg_line,
             lv_msg_rest.

      DESCRIBE FIELD lv_msg_line LENGTH lv_msgv_size IN CHARACTER MODE.

*       Check if the error message is longer than one message paraemter
      IF ( strlen( ls_char120 ) > lv_msgv_size ).
        lv_msg_text = ls_char120.

*         We split the message into up to 3 parts used for up to 3 message parameters
        DO 3 TIMES.
          lv_loop_count = sy-index.

*           Get the next part of the message that fits into one message parameter
*           Note, this function module takes spaces into account and does not split
*           within a word like cl_message_helper=>set_msg_vars_for_clike does.
          CALL FUNCTION 'TEXT_SPLIT'
            EXPORTING
              length = lv_msgv_size
              text   = lv_msg_text
            IMPORTING
              line   = lv_msg_line
              rest   = lv_msg_rest.

*           Transfer to message variable
          CASE lv_loop_count.
            WHEN 1. lv_msgv1 = lv_msg_line.
            WHEN 2. lv_msgv2 = lv_msg_line.
            WHEN 3. lv_msgv3 = lv_msg_line.
          ENDCASE.

*           Exit the loop when there is no text left to be processed
          IF ( lv_msg_rest IS INITIAL ).
            EXIT.
          ELSE.
            lv_msg_text = lv_msg_rest.
          ENDIF.

        ENDDO.
      ELSE.
*         The error message is short enough to fit into one message parameter
        lv_msgv1 = ls_char120.
      ENDIF.

      APPEND INITIAL LINE TO ct_message REFERENCE INTO lr_msg.
      MOVE-CORRESPONDING ls_msg TO lr_msg->*.
      lr_msg->msgty     = if_ehcsm_spc_const_c=>gc_msg_warning.
      lr_msg->msgid     = 'C$'.
      lr_msg->msgno     = 204.
      lr_msg->fieldname = 'IDENT'.                          "#EC NEEDED
      lr_msg->msg_index = lv_tabix.
      lr_msg->msgv1     = lv_msgv1.
* Begin Correction 3051352 29.04.2021 ********************************
      lr_msg->msgv2     = lv_msgv2.
      lr_msg->msgv3     = lv_msgv3.
* End Correction 3051352 29.04.2021 **********************************
      MESSAGE w204(c$) WITH lv_msgv1 lv_msgv2 lv_msgv3 INTO lv_dummy.
    ENDIF.


  ENDMETHOD.


  METHOD check_identifier_by_type_cat.
*--------------------------------------------------------------------*
* Declarations
*--------------------------------------------------------------------*
    DATA:
      ls_tcg23             TYPE tcg23.                      "#EC NEEDED

*--------------------------------------------------------------------*
* Method Implementation
*--------------------------------------------------------------------*

    CLEAR ev_new_ident.

    SELECT SINGLE checkf FROM tcg23 INTO CORRESPONDING FIELDS OF ls_tcg23
                               WHERE idtype = iv_idtype
                               AND   idcat  = iv_idcat.
    IF sy-subrc = 0.
      " ---------------------------------------
      " combination is valid; now let's call the check function
      " module assigned to the identification type.
      " This is necessary since the backend API is returning
      " not that readable messages than that function modules
      " that are unfortunately only called in Dynpro.
      " ---------------------------------------

      " in EHSM 4.0 some code was moved to the method:
      " CL_EHFND_EHSSUB_PROXY=>CHECK_IDENTIFIER
      IF ls_tcg23-checkf IS NOT INITIAL.

        check_identifier(
          EXPORTING
            iv_ident        = iv_ident         " Identifier
            iv_check_method = ls_tcg23-checkf  " 30 Characters
          IMPORTING
            ev_new_ident    = ev_new_ident     " Identifier
          CHANGING
            ct_message      = ct_message       " Messages Table Type
        ).

      ENDIF. " IF ls_tcg23-checkf
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
*----------------------------------------------------------------------*
* Local Data
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Function Body
*----------------------------------------------------------------------*
    CREATE OBJECT go_table_reader.


  ENDMETHOD.


  METHOD convert_subid_to_int.
* Purpose: Convert specification id to internal id

    CALL FUNCTION 'CONVERSION_EXIT_SPEC1_INPUT'
      EXPORTING
        input  = iv_subid
      IMPORTING
        output = rv_subid.

  ENDMETHOD.


  METHOD delete_specifications_via_api.

    DATA: ls_addinf TYPE rcgaddinf.
    DATA: ls_scenario TYPE espap_new_scenario_type.
    DATA: lt_error_api TYPE espap_exterror_tab_type.
    DATA: lt_sub_header_api TYPE esprh_apirh_tab_type.
    DATA: lt_material_api TYPE esprh_apimj_tab_type.
    DATA: lt_identifier_api TYPE esprh_apiri_tab_type.
    DATA: ls_sub_header_api TYPE esprh_apirh_wa_type.
    DATA: ls_material_api TYPE esprh_apimj_wa_type.
    DATA: ls_identifier_api TYPE esprh_apiri_wa_type.
    DATA: ls_error TYPE /vpcoe/s_ehs_api_msg.  ##NEEDED

    REFRESH: et_error.

    MOVE-CORRESPONDING is_scenario TO ls_scenario.
    ls_addinf-valdat = sy-datum.

    " map EHFND structures to API structures to avoid issues with customer enhancements
    LOOP AT ct_sub_header INTO DATA(ls_sub_header).
      MOVE-CORRESPONDING ls_sub_header TO ls_sub_header_api.
      INSERT ls_sub_header_api INTO TABLE lt_sub_header_api.
    ENDLOOP.
    LOOP AT ct_material INTO DATA(ls_material).
      MOVE-CORRESPONDING ls_material TO ls_material_api.
      INSERT ls_material_api INTO TABLE lt_material_api.
    ENDLOOP.
    LOOP AT ct_identifier INTO DATA(ls_identifier).
      MOVE-CORRESPONDING ls_identifier TO ls_identifier_api.
      INSERT ls_identifier_api INTO TABLE lt_identifier_api.
    ENDLOOP.

    CALL FUNCTION 'C1F5_SPECIFICATIONS_DELETE'
      EXPORTING
        i_scenario           = ls_scenario
        i_addinf             = ls_addinf
      IMPORTING
        e_flg_internal_error = ev_flg_internal_error
        e_flg_error          = ev_flg_error
        e_flg_warning        = ev_flg_warning
      TABLES
        x_spec_head_tab      = lt_sub_header_api
        x_material_tab       = lt_material_api
        x_identifier_tab     = lt_identifier_api
        e_error_tab          = lt_error_api.

    " map API structures back to EHFND structures
    CLEAR: ct_sub_header, ct_material, ct_identifier.
    LOOP AT lt_sub_header_api INTO ls_sub_header_api.
      MOVE-CORRESPONDING ls_sub_header_api TO ls_sub_header.
      INSERT ls_sub_header INTO TABLE ct_sub_header.
    ENDLOOP.
    LOOP AT lt_material_api INTO ls_material_api.
      MOVE-CORRESPONDING ls_material_api TO ls_material.
      INSERT ls_material INTO TABLE ct_material.
    ENDLOOP.
    LOOP AT lt_identifier_api INTO ls_identifier_api.
      MOVE-CORRESPONDING ls_identifier_api TO ls_identifier.
      INSERT ls_identifier INTO TABLE ct_identifier.
    ENDLOOP.
    LOOP AT lt_error_api INTO DATA(ls_error_api).
      MOVE-CORRESPONDING ls_error_api TO ls_error.
      INSERT ls_error INTO TABLE et_error.
    ENDLOOP.


  ENDMETHOD.


  METHOD extract_identifier.

    DATA: lr_s_ident_header TYPE REF TO bapi1077ri,
          lt_tcg28          TYPE STANDARD TABLE OF tcg28,
          lr_s_tcg28        TYPE REF TO tcg28.



* Try to find ident. listing profile for substances
    SELECT * FROM tcg28 INTO TABLE lt_tcg28
      WHERE idlid   = iv_ident_listing
        AND objtype = 'SUBSTANCE'.

    IF lines( lt_tcg28 ) = 0.
*   Try to find ident. listing profile for all types
      SELECT * FROM tcg28 INTO TABLE lt_tcg28
        WHERE idlid   = iv_ident_listing
          AND objtype = ''.
    ENDIF.

* Sort by position / priority
    SORT lt_tcg28 BY idlpos idlprio.

* Loop the profile
    LOOP AT lt_tcg28 REFERENCE INTO lr_s_tcg28.

*   Try the user language
      READ TABLE it_ident_header REFERENCE INTO lr_s_ident_header
        WITH KEY recno_root = iv_recn
                 id_categry = lr_s_tcg28->idcat
                 id_type    = lr_s_tcg28->idtype
                 langu      = sy-langu
        BINARY SEARCH.
      IF sy-subrc = 0.
        rv_identifier = lr_s_ident_header->identifier.
        EXIT.
      ENDIF.

*   Try the default language
      READ TABLE it_ident_header REFERENCE INTO lr_s_ident_header
        WITH KEY recno_root = iv_recn
                 id_type    = lr_s_tcg28->idtype
                 id_categry = lr_s_tcg28->idcat
                 langu      = /vpcoe/cl_config_access=>get_default_language( )
        BINARY SEARCH.
      IF sy-subrc = 0.
        rv_identifier = lr_s_ident_header->identifier.
        EXIT.
      ENDIF.

*   Try empty language
      READ TABLE it_ident_header REFERENCE INTO lr_s_ident_header
        WITH KEY recno_root = iv_recn
                 id_type    = lr_s_tcg28->idtype
                 id_categry = lr_s_tcg28->idcat
                 langu      = ''
        BINARY SEARCH.
      IF sy-subrc = 0.
        rv_identifier = lr_s_ident_header->identifier.
        EXIT.
      ENDIF.

    ENDLOOP.



  ENDMETHOD.


  METHOD extract_phrase_id.

    DATA lv_phrase_key TYPE esephrkey.
    DATA lv_phrase_id  TYPE esephrid.

    lv_phrase_key = iv_phrase_key.

    CALL FUNCTION 'C146_PHRASE_KEY_SPLIT'
      EXPORTING
        i_phrkey = lv_phrase_key
      IMPORTING
        e_phrid  = lv_phrase_id.

    rv_phrase_id = lv_phrase_id.

  ENDMETHOD.


  METHOD find_pure_sub_by_ident_recn.
* Begin Correction Note 1889966 *******************
    TYPES: BEGIN OF recn_ident_wa_type,
             recn  TYPE eserecn,
             ident TYPE eseident,
           END OF recn_ident_wa_type.
    TYPES: recn_ident_tab_type TYPE TABLE OF recn_ident_wa_type.

* Local deklarations ---------------------------------------------------
    CONSTANTS: lc_rh      TYPE char3 VALUE 'rh~',
               lc_ri      TYPE char3 VALUE 'ri~',
               lc_and     TYPE char3 VALUE 'AND',
               lc_valfr   TYPE char5 VALUE 'valfr',
               lc_valto   TYPE char5 VALUE 'valto',
               lc_delflg  TYPE char6 VALUE 'delflg',
               lc_idtype  TYPE char6 VALUE 'idtype',
               lc_idcat   TYPE char5 VALUE 'idcat',
               lc_langu   TYPE char5 VALUE 'langu',
               lc_identnm TYPE char7 VALUE 'identnm',
               lc_le      TYPE char2 VALUE '<=',
               lc_ge      TYPE char2 VALUE '>=',
               lc_ne      TYPE char2 VALUE '<>',
               lc_eq      TYPE char1 VALUE '=',
               lc_like(4) TYPE c VALUE 'LIKE',
               lc_apo     TYPE char1 VALUE ''''.

* Begin Correction 2059564 ***********************
    DATA: lt_wherecond_rh       TYPE TABLE OF string,
          lt_wherecond          TYPE TABLE OF string,
          ls_wherecond          TYPE string,
          l_temp_cond           TYPE string,
          l_temp_value          TYPE string,
* End Correction 2059564 *************************
          l_ident_norm          TYPE eseidentnm,
          l_ident_norm_str      TYPE string, " Note 2260499 11.01.2016
          lt_idents_found       TYPE recn_ident_tab_type,
          ls_ident_found        LIKE LINE OF lt_idents_found,
          lv_valdat             TYPE esediagvd,
          lv_idtype_checked     TYPE eseidtype,
          lv_idcat_checked      TYPE eseidcat,
          lv_langu_checked      TYPE eselangu,
          lv_ident_norm_checked TYPE eseidentnm.


    DATA:
      ls_idents         TYPE esprh_apiri_wa_type,
      l_flg_keep_jokers TYPE esp1_boolean.


* Functional body ------------------------------------------------------

* ----------------------------------------------------------------------
* (0) Parameter checks
* ----------------------------------------------------------------------
    lv_valdat = sy-datum.
* ----------------------------------------------------------------------
* (1) Create condition table for where condition (fix conditions)
* ----------------------------------------------------------------------
* (1a) delflg estrh
    CLEAR: ls_wherecond, l_temp_cond, l_temp_value.
    CONCATENATE lc_rh lc_delflg INTO ls_wherecond.
    CONCATENATE lc_apo esp1_true lc_apo INTO l_temp_value.
    CONCATENATE ls_wherecond lc_ne l_temp_value
                INTO ls_wherecond SEPARATED BY space.
    APPEND ls_wherecond TO lt_wherecond_rh.

* (1b) valfr estrh
    CLEAR: ls_wherecond, l_temp_cond, l_temp_value.
    CONCATENATE lc_rh lc_valfr INTO l_temp_cond.
    CONCATENATE lc_apo lv_valdat lc_apo INTO l_temp_value.  "1042993
    CONCATENATE lc_and l_temp_cond lc_le l_temp_value
                INTO ls_wherecond SEPARATED BY space.
    APPEND ls_wherecond TO lt_wherecond_rh.

* (1c) valto estrh
    CLEAR: ls_wherecond, l_temp_cond, l_temp_value.
    CONCATENATE lc_rh lc_valto INTO l_temp_cond.
    CONCATENATE lc_apo lv_valdat lc_apo INTO l_temp_value.  "1042993
    CONCATENATE lc_and l_temp_cond lc_ge l_temp_value
                INTO ls_wherecond SEPARATED BY space.
    APPEND ls_wherecond TO lt_wherecond_rh.

* (2a) delflg estri
    CLEAR: ls_wherecond, l_temp_cond, l_temp_value.
    CONCATENATE lc_ri lc_delflg INTO l_temp_cond.
    CONCATENATE lc_apo esp1_true lc_apo INTO l_temp_value.
    CONCATENATE lc_and l_temp_cond lc_ne l_temp_value
                INTO ls_wherecond SEPARATED BY space.
    APPEND ls_wherecond TO lt_wherecond_rh.

* (2b) valfr estri
    CLEAR: ls_wherecond, l_temp_cond, l_temp_value.
    CONCATENATE lc_ri lc_valfr INTO l_temp_cond.
    CONCATENATE lc_apo lv_valdat lc_apo INTO l_temp_value.  "1042993
    CONCATENATE lc_and l_temp_cond lc_le l_temp_value
                INTO ls_wherecond SEPARATED BY space.
    APPEND ls_wherecond TO lt_wherecond_rh.

* (2c) valto estri
    CLEAR: ls_wherecond, l_temp_cond, l_temp_value.
    CONCATENATE lc_ri lc_valto INTO l_temp_cond.
    CONCATENATE lc_apo lv_valdat lc_apo INTO l_temp_value.  "1042993
    CONCATENATE lc_and l_temp_cond lc_ge l_temp_value
                INTO ls_wherecond SEPARATED BY space.
    APPEND ls_wherecond TO lt_wherecond_rh.

* ----------------------------------------------------------------------
* (2) Create condition table for where condition (variable conditions)
* ----------------------------------------------------------------------

    LOOP AT it_idents INTO ls_idents.
      lt_wherecond = lt_wherecond_rh.

*   (2d) idtype estri
      IF NOT ls_idents-idtype IS INITIAL.
        CLEAR: ls_wherecond, l_temp_cond, l_temp_value.

        lv_idtype_checked = cl_abap_dyn_prg=>check_variable_name( ls_idents-idtype ).

        CONCATENATE lc_ri lc_idtype INTO l_temp_cond.
        CONCATENATE lc_apo lv_idtype_checked lc_apo INTO l_temp_value.
        CONCATENATE lc_and l_temp_cond lc_eq l_temp_value
                      INTO ls_wherecond SEPARATED BY space.
        APPEND ls_wherecond TO lt_wherecond.
      ENDIF.

*   (2e) idcat estri
      IF NOT ls_idents-idcat IS INITIAL.
        CLEAR: ls_wherecond, l_temp_cond, l_temp_value.

*     check variable name for security issues
        lv_idcat_checked = cl_abap_dyn_prg=>check_variable_name( ls_idents-idcat ).

        CONCATENATE lc_ri lc_idcat INTO l_temp_cond.
        CONCATENATE lc_apo lv_idcat_checked lc_apo INTO l_temp_value.
        CONCATENATE lc_and l_temp_cond lc_eq l_temp_value
                    INTO ls_wherecond SEPARATED BY space.
        APPEND ls_wherecond TO lt_wherecond.
      ENDIF.

*   (2f) langu estri
      IF NOT ls_idents-langu IS INITIAL.
        CLEAR: ls_wherecond, l_temp_cond, l_temp_value.

*     check variable name for security issues
        lv_langu_checked = cl_abap_dyn_prg=>check_variable_name( ls_idents-langu ).

        CONCATENATE lc_ri lc_langu INTO l_temp_cond.
        CONCATENATE lc_apo lv_langu_checked lc_apo INTO l_temp_value.
        CONCATENATE lc_and l_temp_cond lc_eq l_temp_value
                    INTO ls_wherecond SEPARATED BY space.
        APPEND ls_wherecond TO lt_wherecond.
      ENDIF.

*   (2g) ident estri
      IF NOT ls_idents-ident IS INITIAL.

*     We use the normalized identifier because there is an index
*     on that field. After the select statement we check the
*     exact identifier and return only the exactly matching records.

        l_ident_norm = ls_idents-ident.
        IF i_flg_fuzzy = esp1_true.
*       For a pattern based search over normalized identifiers
*       we will replace + with *. Except if it is masked with a
*       preceeding #
          l_flg_keep_jokers = esp1_true.
*        PERFORM l_replace_special_chars
*          CHANGING l_ident_norm.
* replace all occurences of escaped characters as these have to be
* treated as exact search for one of the chaarcters #, * or +
          REPLACE ALL OCCURRENCES OF '##' IN l_ident_norm WITH ''.
          REPLACE ALL OCCURRENCES OF '#*' IN l_ident_norm WITH ''.
          REPLACE ALL OCCURRENCES OF '#+' IN l_ident_norm WITH ''.

* Normalized identifier data model does not allow to use + as wild
* card (any character). We will use the * wildcard (none or any
* sequence of characters) for the SQL select. The pattern based
* comparison will be done with an IN operator in ABAP
          REPLACE ALL OCCURRENCES OF '+'  IN l_ident_norm WITH '*'.


        ELSE.
*       For an exact search we will remove SQL jokers * and +. This
*       is necessary as there might be MPN numbers like PT+3/4X20
*       or SMF*28E+.
*       This would imply a pattern based search although this is
*       not the case!
          l_flg_keep_jokers = esp1_false.
        ENDIF.

        CALL FUNCTION 'C147_STRING_NORMALIZE_GENERIC'
          EXPORTING
            i_string             = l_ident_norm
            i_flg_keep_sapjokers = l_flg_keep_jokers
            i_flg_keep_sqljokers = abap_false
          IMPORTING
            e_string_norm        = l_ident_norm.

*     Translate * to % and + to _
        CALL FUNCTION 'C147_JOKER_SAP_TO_SQL'
          EXPORTING
            i_srcstr = l_ident_norm
          IMPORTING
            e_dststr = l_ident_norm.

        CLEAR: ls_wherecond, l_temp_cond, l_temp_value.

* Begin Correction Note 2260499 11.01.2016 **********************************************************************
        CONCATENATE lc_ri lc_identnm INTO l_temp_cond.

        l_ident_norm_str = l_ident_norm.
        CONCATENATE lc_apo l_ident_norm_str lc_apo INTO l_temp_value.
        lv_ident_norm_checked = cl_abap_dyn_prg=>check_char_literal( l_temp_value ).
* End Correction Note 2260499 11.01.2016 **********************************************************************

        IF lv_ident_norm_checked CA '%_'.
          CONCATENATE lc_and l_temp_cond lc_like lv_ident_norm_checked " Note 2260499 11.01.2016
                        INTO ls_wherecond SEPARATED BY space.
        ELSE.
          CONCATENATE lc_and l_temp_cond lc_eq lv_ident_norm_checked " Note 2260499 11.01.2016
                        INTO ls_wherecond SEPARATED BY space.
        ENDIF.

        APPEND ls_wherecond TO lt_wherecond.

      ENDIF.

* ----------------------------------------------------------------------
* (3) Get Recns from DB
* ----------------------------------------------------------------------
      IF ct_recns IS NOT INITIAL.
        SELECT rh~recn ri~ident
               INTO TABLE lt_idents_found
               FROM estrh AS rh
               JOIN estri AS ri
               ON rh~recn = ri~recnroot
               FOR ALL ENTRIES IN ct_recns
               WHERE rh~recn = ct_recns-recn
                 AND (lt_wherecond)
                 AND rh~subcat = iv_pure_sub_subcat. " Note 2025684 03.06.2014
      ELSE.
        SELECT rh~recn ri~ident
               INTO TABLE lt_idents_found
               FROM estrh AS rh
               JOIN estri AS ri
               ON rh~recn = ri~recnroot
               WHERE (lt_wherecond)
               AND rh~subcat = iv_pure_sub_subcat. " Note 2025684 03.06.2014
      ENDIF.

      REFRESH ct_recns.

*   Copy only those RECNs to the result table whose identifier match
*   exactly the requested one. This is necessary because we have
*   used the normalized identifier for the select statement and could
*   have found too many records.
      IF ( NOT ls_idents-ident IS INITIAL ).
        TRANSLATE ls_idents-ident TO UPPER CASE.         "#EC TRANSLANG
        LOOP AT lt_idents_found INTO ls_ident_found.

          TRANSLATE ls_ident_found-ident
                         TO UPPER CASE.                  "#EC TRANSLANG

          IF i_flg_fuzzy = esp1_false AND
             ls_ident_found-ident = ls_idents-ident.
            APPEND ls_ident_found-recn TO ct_recns.
          ELSEIF i_flg_fuzzy = esp1_true AND
                 ( ls_ident_found-ident EQ ls_idents-ident OR
                   ls_ident_found-ident CP ls_idents-ident ).
            APPEND ls_ident_found-recn TO ct_recns.
          ENDIF.

        ENDLOOP.
      ELSE.
        LOOP AT lt_idents_found INTO ls_ident_found.
          APPEND ls_ident_found-recn TO ct_recns.
        ENDLOOP.
      ENDIF.

      SORT ct_recns.
      DELETE ADJACENT DUPLICATES FROM ct_recns.

*   Each subsequent select in this loop reduces the number of hits. So
*   when et_recns is empty here after at least one select we haven't
*   found anything.
      IF ( ct_recns[] IS INITIAL ).
        EXIT.
      ENDIF.

    ENDLOOP.
* End Correction Note 1889966 *******************
  ENDMETHOD.


  METHOD get_documents_by_substance.
*----------------------------------------------------------------------*
* .... inspired by
* .........system:   TC1/011
* .........func mod: /TDAG/I_RF03_DMS_DOC_GET
*----------------------------------------------------------------------*
* Local Data
*----------------------------------------------------------------------*
    DATA:
      lr_selopt TYPE REF TO rfc_db_opt,
      lt_selopt TYPE /vpcoe/t_sel_options.

    DATA:
      lr_sub_header TYPE REF TO bapi1077rh,
      lr_estdh      TYPE REF TO estdh,

      lt_sub_header TYPE /vpcoe/t_bapi1077rh,
      lt_estdh      TYPE TABLE OF estdh.

    DATA:
      ls_doc_key TYPE bapi_doc_keys,
      lt_files   TYPE cvapi_tbl_doc_files,
      lt_content TYPE dms_tbl_drao.

*----------------------------------------------------------------------*
* Function Body
*----------------------------------------------------------------------*
* Init
    CLEAR: et_files, et_content, et_return.

*------------------------------------------
* 0. Get Substance ID for external source key
    APPEND INITIAL LINE TO lt_sub_header REFERENCE INTO lr_sub_header.
    lr_sub_header->record_no = iv_external_src.

    read_ehs_substances(
      EXPORTING
        iv_rfc_dest   = iv_rfc_dest
        iv_flg_header = abap_true
      IMPORTING
        et_return     = et_return
      CHANGING
        xt_sub_header = lt_sub_header ).

    READ TABLE lt_sub_header REFERENCE INTO lr_sub_header INDEX 1.

*------------------------------------------
* 1. remote table read on ESTDH
    APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_selopt.
    CONCATENATE '( SUBID = ' '''' lr_sub_header->substance '''' ' )'
           INTO lr_selopt->* RESPECTING BLANKS.

    APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_selopt.
    lr_selopt->* = 'AND'.

    APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_selopt.
    CONCATENATE '( SBGVID = ' '''' iv_sbgvid '''' ' )'
           INTO lr_selopt->* RESPECTING BLANKS.

    go_table_reader->get_remote_table_content(
      EXPORTING
        iv_tablename       = gc_tabname-rephead
        iv_rfc_destination = iv_rfc_dest
        it_selopt          = lt_selopt
      CHANGING
        ct_content         = lt_estdh ).

    LOOP AT lt_estdh REFERENCE INTO lr_estdh.

      CLEAR: ls_doc_key, lt_files, lt_content.

      ls_doc_key = lr_estdh->dmskey.

*  ------------------------------------------
*   2. Get File details of Document
      CALL FUNCTION 'CVAPI_DOC_GETDETAIL'
        DESTINATION iv_rfc_dest
        EXPORTING
          pf_batchmode = abap_true
          pf_dokar     = ls_doc_key-documenttype
          pf_doknr     = ls_doc_key-documentnumber
          pf_dokvr     = ls_doc_key-documentversion
          pf_doktl     = ls_doc_key-documentpart
        TABLES
          pt_files     = lt_files                             "#EC ENHOK
        EXCEPTIONS
          not_found    = 1
          no_auth      = 2
          error        = 3
          OTHERS       = 4.
      IF ( sy-subrc <> 0 ).
        CONTINUE.
      ENDIF.

*  ------------------------------------------
*   3. Read Binary Contents from Documents
      CALL FUNCTION 'CVAPI_DOC_CHECKOUTVIEW'
        DESTINATION iv_rfc_dest
        EXPORTING
          pf_dokar           = ls_doc_key-documenttype
          pf_doknr           = ls_doc_key-documentnumber
          pf_dokvr           = ls_doc_key-documentversion
          pf_doktl           = ls_doc_key-documentpart
          pf_content_provide = 'TBL'
        TABLES
          pt_files           = lt_files                       "#EC ENHOK
          ptx_content        = lt_content.                    "#EC ENHOK

      APPEND LINES OF lt_files   TO et_files.
      APPEND LINES OF lt_content TO et_content.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_document_headers.

    DATA lr_s_doc_parameters TYPE REF TO /vpcoe/s_doc_param.
    DATA lr_s_selopt         TYPE REF TO rfc_db_opt.
    DATA lt_selopt           TYPE /vpcoe/t_sel_options.
    DATA lt_selopt_subst     TYPE /vpcoe/t_sel_options.
    DATA lt_gen_var          TYPE TABLE OF estlp.
    DATA lr_s_gen_var        TYPE REF TO estlp.
    DATA lv_gen_var_count    TYPE i.
    DATA lt_gen_var_desc     TYPE TABLE OF estlr.
    DATA lr_s_gen_var_desc   TYPE REF TO estlr.
    DATA lt_estdh            TYPE TABLE OF estdh.
    DATA lt_estdh_all        TYPE TABLE OF estdh.
    DATA lr_s_estdh          TYPE REF TO estdh.
    DATA lr_s_sub_header     TYPE REF TO bapi1077rh.
    DATA ls_doc_header       TYPE /vpcoe/s_doc_header.
    DATA lts_gen_var         TYPE SORTED TABLE OF estlp WITH NON-UNIQUE KEY rvlid reptype.
    DATA lth_gen_var         TYPE HASHED TABLE OF estlp WITH UNIQUE KEY ldepid.
    DATA lv_default_language TYPE langu.

    CLEAR ets_doc_header.
    CLEAR et_return.

    " We need the document parameters and any substances to select the documents
    IF ( lines( it_doc_parameters ) = 0 OR lines( it_sub_header ) = 0 ).
      RETURN.
    ENDIF.

    " Create select conditions from document parameters. The generation variants
    " can be identifierd via document category and validity area.
    LOOP AT it_doc_parameters REFERENCE INTO lr_s_doc_parameters.
      IF ( lines( lt_selopt ) > 0 ).
        APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_s_selopt.
        lr_s_selopt->text = 'OR'.
      ENDIF.

      APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_s_selopt.
      lr_s_selopt->text = |(( RVLID = '{ lr_s_doc_parameters->val_area }' )|.

      APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_s_selopt.
      lr_s_selopt->text = 'AND'.

      APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_s_selopt.
      lr_s_selopt->text = |( REPTYPE = '{ lr_s_doc_parameters->category }' ))|.
    ENDLOOP.

    " Determine the generation variant according to the document parameters
    go_table_reader->get_remote_table_content(
      EXPORTING
        iv_tablename       = gc_tabname-genvar
        iv_rfc_destination = iv_rfc_dest
        it_selopt          = lt_selopt
      CHANGING
        ct_content         = lt_gen_var ).

    " Create a select-option table to select the descriptions of the generation variants.
    CLEAR lt_selopt.
    LOOP AT lt_gen_var REFERENCE INTO lr_s_gen_var.
      APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_s_selopt.
      IF ( lines( lt_selopt ) > 1 ).
        lr_s_selopt->text = 'OR'.
      ENDIF.
      lr_s_selopt->text = |{ lr_s_selopt->text } RECNROOT = '{ lr_s_gen_var->recn }'|.
    ENDLOOP.

    " Determine the generation variant according to the document parameters
    go_table_reader->get_remote_table_content(
      EXPORTING
        iv_tablename       = gc_tabname-genvar_desc
        iv_rfc_destination = iv_rfc_dest
        it_selopt          = lt_selopt
      CHANGING
        ct_content         = lt_gen_var_desc  ).

    " Sort for later access
    lts_gen_var = lt_gen_var.

    " Create a select option table for all substances that is used to
    " search for the documents.
    APPEND INITIAL LINE TO lt_selopt_subst REFERENCE INTO lr_s_selopt.
    lr_s_selopt->text = '('.
    LOOP AT  it_sub_header REFERENCE INTO lr_s_sub_header.
      IF ( lines( lt_selopt_subst ) > 1 ).
        APPEND INITIAL LINE TO lt_selopt_subst REFERENCE INTO lr_s_selopt.
        lr_s_selopt->text = 'OR'.
      ENDIF.
      APPEND INITIAL LINE TO lt_selopt_subst REFERENCE INTO lr_s_selopt.
      lr_s_selopt->text = |SUBID = '{ lr_s_sub_header->substance }'|.
    ENDLOOP.
    APPEND INITIAL LINE TO lt_selopt_subst REFERENCE INTO lr_s_selopt.
    lr_s_selopt->text = ')'.

    " Select the documents via the substance IDs above and the
    " conditions from the document parameters.
    " We select only released documents here.
    CLEAR lt_selopt.
    LOOP AT it_doc_parameters REFERENCE INTO lr_s_doc_parameters.

      " Start with the conditions for all substances
      lt_selopt = lt_selopt_subst.

      " only released and historic documents
      APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_s_selopt.
      lr_s_selopt->text = |AND (REPSTATUS = '{ gc_report_status_released }' OR REPSTATUS = '{ gc_report_status_historic }')|.

      APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_s_selopt.
      lr_s_selopt->text = |AND REPTYPE = '{ lr_s_doc_parameters->category }'|.

      APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_s_selopt.
      lr_s_selopt->text = |AND LANGU = '{ lr_s_doc_parameters->language }'|.

      lv_gen_var_count = 0.
      " Get all generation variants corresponding to the document parameters
      LOOP AT lts_gen_var REFERENCE INTO lr_s_gen_var
        WHERE rvlid = lr_s_doc_parameters->val_area
          AND reptype = lr_s_doc_parameters->category.
        APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_s_selopt.
        IF ( lv_gen_var_count = 0 ).
          lr_s_selopt->text = 'AND ('.
        ELSE.
          lr_s_selopt->text = 'OR'.
        ENDIF.
        lr_s_selopt->text = |{ lr_s_selopt->text } SBGVID = '{ lr_s_gen_var->ldepid }'|.
        ADD 1 TO lv_gen_var_count.
      ENDLOOP.

      IF ( lv_gen_var_count > 0 ).
        lr_s_selopt->text = |{ lr_s_selopt->text })|.

        " Determine the documents
        CLEAR lt_estdh.
        go_table_reader->get_remote_table_content(
          EXPORTING
            iv_tablename       = gc_tabname-rephead
            iv_rfc_destination = iv_rfc_dest
            it_selopt          = lt_selopt
          CHANGING
            ct_content         = lt_estdh ).

        APPEND LINES OF lt_estdh TO lt_estdh_all.
      ENDIF.

    ENDLOOP.

    lth_gen_var = lt_gen_var.
    SORT lt_gen_var_desc BY recnroot langu.

    lv_default_language = /vpcoe/cl_config_access=>get_default_language( ).

    LOOP AT lt_estdh_all REFERENCE INTO lr_s_estdh.
      " Get the validity area of the generation variant
      READ TABLE lth_gen_var REFERENCE INTO lr_s_gen_var
        WITH TABLE KEY ldepid = lr_s_estdh->sbgvid.
      ASSERT ( sy-subrc = 0 ).

      CLEAR lr_s_gen_var_desc.
      " Determine the description of the generation variant in the document language
      READ TABLE lt_gen_var_desc REFERENCE INTO lr_s_gen_var_desc
        WITH KEY recnroot = lr_s_gen_var->recn
                 langu = lr_s_estdh->langu
        BINARY SEARCH.
      IF ( sy-subrc <> 0 ).
        " There is no description in the document language so use the default language
        READ TABLE lt_gen_var_desc REFERENCE INTO lr_s_gen_var_desc
          WITH KEY recnroot = lr_s_gen_var->recn
                   langu = lv_default_language
          BINARY SEARCH.
        IF ( sy-subrc <> 0 ).
          " There is no description in the default language so use English
          READ TABLE lt_gen_var_desc REFERENCE INTO lr_s_gen_var_desc
            WITH KEY recnroot = lr_s_gen_var->recn
                     langu = 'E'
            BINARY SEARCH.
          IF ( sy-subrc <> 0 ).
            " There is no description in English so use the first one
            READ TABLE lt_gen_var_desc REFERENCE INTO lr_s_gen_var_desc
              WITH KEY recnroot = lr_s_gen_var->recn
              BINARY SEARCH.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR ls_doc_header.
      ls_doc_header-recn       = lr_s_estdh->recn.
      ls_doc_header-subid      = lr_s_estdh->subid.
      ls_doc_header-reptype    = lr_s_estdh->reptype.
      ls_doc_header-val_area   = lr_s_gen_var->rvlid.
      ls_doc_header-language   = lr_s_estdh->langu.
      ls_doc_header-gen_var    = lr_s_estdh->sbgvid.
      ls_doc_header-version    = lr_s_estdh->version.
      ls_doc_header-subversion = lr_s_estdh->subversion.
      ls_doc_header-val_date   = lr_s_estdh->valdat.
      " If available also return the description of the generation variant
      IF ( lr_s_gen_var_desc IS NOT INITIAL ).
        ls_doc_header-gen_var_desc = lr_s_gen_var_desc->ldepnam.
      ENDIF.

      INSERT ls_doc_header INTO TABLE ets_doc_header.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_environment_parameter.

    " generic call to get an environment parameter
    CALL FUNCTION 'C1I0_TCGENV_READ'
      EXPORTING
        i_param         = iv_parameter " 'INH_JOB_PLANNED'
      IMPORTING
        e_value         = ev_value
      EXCEPTIONS
        param_not_found = 0
        OTHERS          = 0.


  ENDMETHOD.


  METHOD get_idents_at_pos.

    DATA lts_ident_listing  TYPE SORTED TABLE OF /vpcoe/s_ident_listin
                            WITH UNIQUE KEY idlpos idlprio.
    DATA lr_s_ident_listing TYPE REF TO /vpcoe/s_ident_listin.
    DATA lr_s_ident         TYPE REF TO bapi1077ri.
    DATA lv_position        TYPE eseidlpos.


    " We assume it_ident_listing contains just one listing (one IDLID) and one object type


    CLEAR et_ident_at_pos.

    " Sort by position and priority
    lts_ident_listing = it_ident_listing.

    lv_position = iv_position.

    " Check all priorities of the given position
    LOOP AT  lts_ident_listing REFERENCE INTO lr_s_ident_listing WHERE idlpos = lv_position.

      " Try to find any identifiers with the current priority
      LOOP AT its_ident_all REFERENCE INTO lr_s_ident
        WHERE recno_root = iv_spec_recno_root
          AND id_type    = lr_s_ident_listing->idtype
          AND id_categry = lr_s_ident_listing->idcat.
        APPEND lr_s_ident->* TO et_ident_at_pos.
      ENDLOOP.

      " As soon as we have found at least one identifier at the requested position we are done
      IF ( lines( et_ident_at_pos ) > 0 ).
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_ident_listings_chm_subst.
*----------------------------------------------------------------------*
* Local Data
*----------------------------------------------------------------------*
    DATA:
      lr_ident_listing TYPE REF TO /vpcoe/s_ident_listin,
      lr_ident_header  TYPE REF TO bapi1077ri.

*----------------------------------------------------------------------*
* Function Body
*----------------------------------------------------------------------*
* Init
    CLEAR: et_ident_listing, et_ident_header.


    TRY .
        et_ident_listing = get_ident_listing_for_subst(
                             iv_rfc_dest = iv_rfc_dest
                             iv_idlid    = iv_idlid
        ).

        DELETE et_ident_listing WHERE objtype <> 'SUBSTANCE' AND objtype IS NOT INITIAL.


*     Add restrictions to objects to be read
        LOOP AT et_ident_listing REFERENCE INTO lr_ident_listing.
          APPEND INITIAL LINE TO et_ident_header REFERENCE INTO lr_ident_header.
          lr_ident_header->id_categry = lr_ident_listing->idcat.
          lr_ident_header->id_type    = lr_ident_listing->idtype.
        ENDLOOP.

      CATCH cx_abap_invalid_name.
*     nothing realy to do
        CLEAR: et_ident_listing, et_ident_header.

    ENDTRY.

  ENDMETHOD.


  METHOD get_ident_listings_lsub_subst.
*----------------------------------------------------------------------*
* Local Data
*----------------------------------------------------------------------*
    DATA:
      lr_ident_listing TYPE REF TO /vpcoe/s_ident_listin,
      lr_ident_header  TYPE REF TO bapi1077ri.

*----------------------------------------------------------------------*
* Function Body
*----------------------------------------------------------------------*
* Init
    CLEAR: et_ident_listing, et_ident_header.


    TRY .
        et_ident_listing = get_ident_listing_for_subst(
                             iv_rfc_dest = iv_rfc_dest
                             iv_idlid    = iv_idlid
        ).


*     Add restrictions to objects to be read
        LOOP AT et_ident_listing REFERENCE INTO lr_ident_listing.
          APPEND INITIAL LINE TO et_ident_header REFERENCE INTO lr_ident_header.
          lr_ident_header->id_categry = lr_ident_listing->idcat.
          lr_ident_header->id_type    = lr_ident_listing->idtype.
        ENDLOOP.


      CATCH cx_abap_invalid_name.
        CLEAR: et_ident_listing, et_ident_header.

    ENDTRY.
  ENDMETHOD.


  METHOD get_ident_listing_for_subst.
*----------------------------------------------------------------------*
* Local Data
*----------------------------------------------------------------------*
    DATA:
      lr_selopt TYPE REF TO rfc_db_opt,
      lt_selopt TYPE /vpcoe/t_sel_options.

*----------------------------------------------------------------------*
* Function Body
*----------------------------------------------------------------------*
* Init
    CLEAR: rt_ident_listing.


* reduce selection request to given identifier listing
    IF ( iv_idlid IS NOT INITIAL ).
      APPEND INITIAL LINE TO lt_selopt REFERENCE INTO lr_selopt.
      CONCATENATE '( IDLID = ' '''' iv_idlid '''' ' )'
                  INTO lr_selopt->* RESPECTING BLANKS.
    ENDIF.


* read table for identification listing (TCG28) via RFC
    go_table_reader->get_remote_table_content(
      EXPORTING
        iv_tablename       = gc_tabname-idlisting
        iv_rfc_destination = iv_rfc_dest
        it_selopt          = lt_selopt
      CHANGING
        ct_content         = rt_ident_listing ).


    SORT rt_ident_listing BY idlid   ASCENDING
                             objtype DESCENDING
                             idlpos  ASCENDING
                             idlprio ASCENDING.

  ENDMETHOD.


  METHOD get_keytab_from_hitlist.
*----------------------------------------------------------------------*
* Local Data
*----------------------------------------------------------------------*
* tables to be read (with work areas)
    DATA:
      lt_tcghit    TYPE TABLE OF tcghit,
      lt_tcghitpos TYPE TABLE OF tcghitpos,
      lr_tcghit    TYPE REF TO tcghit,
      lr_tcghitpos TYPE REF TO tcghitpos.

* select options / where clause
    DATA:
      ls_selopt TYPE rfc_db_opt,
      lt_selopt TYPE /vpcoe/t_sel_options.

* further variables
    DATA:
      lr_substances       TYPE REF TO /vpcoe/s_ehs_hitpos.

*----------------------------------------------------------------------*
* Function Body
*----------------------------------------------------------------------*
* Init
    CLEAR: et_substances, et_messages.

*=====================================================================================
* check input
*=====================================================================================
    IF ( iv_hit_grpid IS INITIAL    ) OR
       ( iv_hit_grpobjid IS INITIAL ).

      add_message_to_msgtable(
        EXPORTING
*       iv_msgtype  = IF_EHFND_MSG_SEVERITY_C=>GC_SEVERITY_ERROR                   " Message Type
*       iv_msgid    = IF_EHFND_EHSSUB_PROXY_IMPL_C=>GC_EHSSUB_PROXY_MESSAGE_CLASS  " Message Class
          iv_msgno    = '101'              " Message Number
          iv_msgv1    = iv_hit_grpid       " Message Variable
          iv_msgv2    = iv_hit_grpobjid    " Message Variable
*       iv_msgv3    =                    " Message Variable
*       iv_msgv4    =                    " Message Variable
        CHANGING
          xt_messages = et_messages        " Return parameter table
      ).
      RETURN.
    ENDIF.


*=====================================================================================
* get group key of relevant result list for filling chemicals from specifications
*=====================================================================================
* build WHERE clause for table read:
    REFRESH lt_selopt.
* - append defined group (GRPID)
    CONCATENATE '( GRPID = ' '''' iv_hit_grpid '''' ' )' INTO ls_selopt RESPECTING BLANKS.
    APPEND ls_selopt TO lt_selopt.

* - append defined group entry (OBJID)
    IF ( lt_selopt IS NOT INITIAL ).
      ls_selopt = 'AND'.
      APPEND ls_selopt TO lt_selopt.
    ENDIF.
    CONCATENATE '( OBJID = ' '''' iv_hit_grpobjid '''' ' )' INTO ls_selopt RESPECTING BLANKS.
    APPEND ls_selopt TO lt_selopt.


* read table TCGHIT via RFC
    REFRESH lt_tcghit.
    go_table_reader->get_remote_table_content(
      EXPORTING
        iv_tablename       = gc_tabname-tcghit
        iv_rfc_destination = iv_rfc_dest
        it_selopt          = lt_selopt
      CHANGING
        ct_content         = lt_tcghit  ).

* we expect exactly one hit for the selection
    CASE lines( lt_tcghit ).
      WHEN 0.
        add_message_to_msgtable(
          EXPORTING
            iv_msgno    = '103'
            iv_msgv1    = iv_hit_grpid
          CHANGING
            xt_messages = et_messages ).
      WHEN 1.
        "OK
      WHEN OTHERS.
        add_message_to_msgtable(
          EXPORTING
            iv_msgno    = '104'
            iv_msgv1    = iv_hit_grpid
            iv_msgv2    = lines( lt_tcghit )
          CHANGING
            xt_messages = et_messages        " Return parameter table
        ).
    ENDCASE.


*=====================================================================================
* get relevant group entries for filling chemicals from specifications
*=====================================================================================
    READ TABLE lt_tcghit REFERENCE INTO lr_tcghit
         INDEX 1.
    IF ( sy-subrc = 0 ).

*   build WHERE clause for table read:
      REFRESH lt_selopt.
*   - append determined group key (RECN_GRP)
      CONCATENATE '( ' 'RECN_GRP ' '= ' '''' lr_tcghit->recn_grp '''' ' )' INTO ls_selopt RESPECTING BLANKS.
      APPEND ls_selopt TO lt_selopt.


*   read table TCGHITPOS via RFC
      REFRESH lt_tcghitpos.
      go_table_reader->get_remote_table_content(
        EXPORTING
          iv_tablename        = gc_tabname-tcghitpos
          iv_rfc_destination  = iv_rfc_dest
          it_selopt           = lt_selopt                " RFC Table Read: Select Options / WHERE Clause
        CHANGING
          ct_content          = lt_tcghitpos ).

    ENDIF.

*=====================================================================================
* fill key table for filling chemicals from specifications
*=====================================================================================
    LOOP AT lt_tcghitpos REFERENCE INTO lr_tcghitpos.
      APPEND INITIAL LINE TO et_substances REFERENCE INTO lr_substances.
      MOVE-CORRESPONDING lr_tcghitpos->* TO lr_substances->*. "#EC ENHOK
    ENDLOOP.


  ENDMETHOD.


  METHOD get_last_lock_error_from_api.

    CALL FUNCTION 'EHSB_LAST_LOCK_ERROR_GET_SET'
      EXPORTING
        i_flg_get   = esp1_true
      IMPORTING
        e_lock_info = ev_lock_info.

  ENDMETHOD.


  METHOD get_mime_type.

*   retrive document mime code (using filename and search for it in the customzing tables)
    CALL FUNCTION 'CV120_GET_MIME_TYPE'
      EXPORTING
        pf_dappl     = iv_dappl
        pf_file      = iv_filename
      IMPORTING
        pfx_mimetype = rv_mime_type.

  ENDMETHOD.


  METHOD get_subcat_by_recn_subid.

* Purpose: Get the specification type of a given specification

    cl_ehcsm_spc_utility=>get_spec_type_by_recn_subid(
      EXPORTING
        iv_subid         = iv_subid
        iv_spc_hdr_recn  = iv_spc_hdr_recn
        iv_spc_hdr_actn  = iv_spc_hdr_actn
        iv_key_date      = iv_key_date
        iv_change_nr     = iv_change_nr
        iv_langu         = iv_langu
      IMPORTING
        ev_spc_type_name = ev_subcat_name
        ev_spc_type      = ev_subcat
    ).
  ENDMETHOD.


  METHOD is_spec_conv_active.
* Purpose: Check whether ehs classic is active

    DATA: lv_flg_spec1_active TYPE eseboole.

    CALL FUNCTION 'EHS001_SPEC1_ACTIVE_CHECK'
      IMPORTING
        ev_flg_spec1_active = lv_flg_spec1_active.

    rv_is_active = lv_flg_spec1_active.

  ENDMETHOD.


  METHOD is_subid_valid.
* Purpose: Checks whether the internal number is valid (check against external number range)

    DATA: lv_flg_check_numrange TYPE boole_d,
          lv_error              TYPE boole_d,
          lv_returncode         TYPE inri-returncode.

* Method body ---------------------------------------------------------------------------

    CLEAR: ev_error_ind,
           es_error_msg,
           ev_subid_is_valid,
           ev_nrobj,
           ev_nrnr_external.

    IF iv_subid IS INITIAL.
      RETURN.
    ENDIF.

    "Check internal number in external number range
    IF ( iv_spec_type IS NOT INITIAL ).
      CALL FUNCTION 'C14I_SUBID_IN_CORRECT_NUMRANGE'
        EXPORTING
          i_subcat                  = iv_spec_type
          i_subid                   = iv_subid
          i_flg_with_message        = abap_false
        IMPORTING
          e_flg_within              = ev_subid_is_valid
        EXCEPTIONS
          tcg31_not_complete        = 1
          number_range_not_found    = 2
          number_range_not_external = 3
          interval_not_found        = 4
          subcat_not_found          = 5
          OTHERS                    = 6.
      IF ( sy-subrc = 5 )
         OR ( sy-subrc = 1 ).
        lv_flg_check_numrange = abap_true.
      ELSEIF ( sy-subrc <> 0 ).
        lv_error = abap_true.
      ENDIF.
    ELSE.
      lv_flg_check_numrange = abap_true.
    ENDIF.

    "Check whether the system is a cloud system
    TRY.
        /vpcoe/cl_integration_helper=>is_s4h_cloud( RECEIVING rv_is_s4h_cloud = DATA(lv_is_s4h_cloud) ).
      CATCH cx_root.
        lv_is_s4h_cloud = abap_false.
    ENDTRY.

    "Check the internal number if specification type is not known -> number range ESN_SUBID
    IF ( lv_is_s4h_cloud = abap_true )
       AND ( lv_flg_check_numrange = abap_true ).
      CALL FUNCTION 'NUMBER_CHECK'
        EXPORTING
          nr_range_nr             = '0E'
          number                  = iv_subid
          object                  = 'ESN_SUBID'
        IMPORTING
          returncode              = lv_returncode
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_extern = 2
          object_not_found        = 3
          OTHERS                  = 4.
*      test-seam no_numberrange_subid_valid.
*      END-TEST-SEAM.
      IF ( sy-subrc <> 0 ).
        lv_error = abap_true.
      ELSE.
        IF ( lv_returncode = abap_false ).
          ev_subid_is_valid = abap_true.
        ENDIF.
      ENDIF.
    ELSEIF ( ( iv_spec_type IS INITIAL )
             AND ( lv_is_s4h_cloud = abap_false ) )
           OR ( ( iv_spec_type IS NOT INITIAL )
                AND ( lv_is_s4h_cloud = abap_false )
                AND ( lv_flg_check_numrange = abap_true ) ).
      lv_error = abap_true.
    ENDIF.

    "Error handling
    IF ( lv_error = abap_true ).
      ev_error_ind = abap_true.
      "Internal number could not checked.
      MESSAGE e000(/vpcoe/common) WITH `Internal number could not checked ` iv_spec_type `` `` INTO DATA(lv_msg).
      es_error_msg = /vpcoe/cl_integration_helper=>get_msg_from_sys_fields( ).
    ENDIF.

    "Remember the number range and external number range
    IF ev_nrobj IS REQUESTED
      OR ev_nrnr_external IS REQUESTED
      OR ev_nrnr_internal IS REQUESTED.
      SELECT SINGLE numrnge, numinte, numinti
        FROM tcg31
        WHERE subcat = @iv_spec_type
        INTO @DATA(ls_tcg31).
      IF sy-subrc = 0.
        ev_nrobj         = ls_tcg31-numrnge.
        ev_nrnr_external = ls_tcg31-numinte.
        ev_nrnr_internal = ls_tcg31-numinti.
      ELSE.
        ev_nrobj         = 'ESN_SUBID'.
        ev_nrnr_external = '0E'.
        ev_nrnr_internal = '0I'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD modify_specifications_via_api.

    DATA: ls_addinf TYPE rcgaddinf.
    DATA: ls_scenario TYPE espap_new_scenario_type.
    DATA: lt_error_api TYPE espap_exterror_tab_type.
    DATA: lt_sub_header_api TYPE esprh_apirh_tab_type.
    DATA: lt_material_api TYPE esprh_apimj_tab_type.
    DATA: lt_identifier_api TYPE esprh_apiri_tab_type.
    DATA: lt_identifier_longtext_api TYPE esprh_apiil_tab_type.
    DATA: ls_sub_header_api TYPE esprh_apirh_wa_type.
    DATA: ls_material_api TYPE esprh_apimj_wa_type.
    DATA: ls_identifier_api TYPE esprh_apiri_wa_type.
    DATA: ls_identifier_longtext_api TYPE esprh_apiil_wa_type.
    DATA: ls_error TYPE /vpcoe/s_ehs_api_msg.  ##NEEDED

    REFRESH: et_error.

    MOVE-CORRESPONDING is_scenario TO ls_scenario.
    ls_addinf-valdat = sy-datum.

    " map EHFND structures to API structures to avoid issues with customer enhancements
    LOOP AT ct_sub_header INTO DATA(ls_sub_header).
      MOVE-CORRESPONDING ls_sub_header TO ls_sub_header_api.
      INSERT ls_sub_header_api INTO TABLE lt_sub_header_api.
    ENDLOOP.
    LOOP AT ct_material INTO DATA(ls_material).
      MOVE-CORRESPONDING ls_material TO ls_material_api.
      INSERT ls_material_api INTO TABLE lt_material_api.
    ENDLOOP.
    LOOP AT ct_identifier INTO DATA(ls_identifier).
      MOVE-CORRESPONDING ls_identifier TO ls_identifier_api.
      INSERT ls_identifier_api INTO TABLE lt_identifier_api.
    ENDLOOP.
    LOOP AT ct_identifier_longtext INTO DATA(ls_identifier_longtext).
      MOVE-CORRESPONDING ls_identifier_longtext TO ls_identifier_longtext_api.
      INSERT ls_identifier_longtext_api INTO TABLE lt_identifier_longtext_api.
    ENDLOOP.

    CALL FUNCTION 'C1F5_SPECIFICATIONS_MODIFY'
      EXPORTING
        i_scenario           = ls_scenario
        i_addinf             = ls_addinf
        i_flg_create_mode    = iv_flg_create_mode
      IMPORTING
        e_flg_internal_error = ev_flg_internal_error
        e_flg_error          = ev_flg_error
        e_flg_warning        = ev_flg_warning
      TABLES
        x_spec_head_tab      = lt_sub_header_api
        x_material_tab       = lt_material_api
        x_identifier_tab     = lt_identifier_api
        x_ident_longtext_tab = lt_identifier_longtext_api
        e_error_tab          = lt_error_api.

    " map API structures back to EHFND structures
    CLEAR: ct_sub_header, ct_material, ct_identifier, ct_identifier_longtext.
    LOOP AT lt_sub_header_api INTO ls_sub_header_api.
      MOVE-CORRESPONDING ls_sub_header_api TO ls_sub_header.
      INSERT ls_sub_header INTO TABLE ct_sub_header.
    ENDLOOP.
    LOOP AT lt_material_api INTO ls_material_api.
      MOVE-CORRESPONDING ls_material_api TO ls_material.
      INSERT ls_material INTO TABLE ct_material.
    ENDLOOP.
    LOOP AT lt_identifier_api INTO ls_identifier_api.
      MOVE-CORRESPONDING ls_identifier_api TO ls_identifier.
      INSERT ls_identifier INTO TABLE ct_identifier.
    ENDLOOP.
    LOOP AT lt_identifier_longtext_api INTO ls_identifier_longtext_api.
      MOVE-CORRESPONDING ls_identifier_longtext_api TO ls_identifier_longtext.
      INSERT ls_identifier_longtext INTO TABLE ct_identifier_longtext.
    ENDLOOP.
    LOOP AT lt_error_api INTO DATA(ls_error_api).
      MOVE-CORRESPONDING ls_error_api TO ls_error.
      INSERT ls_error INTO TABLE et_error.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_ehs_phrases.

* read the phrases for the PHRASESEL
    CALL FUNCTION 'BAPI_BUS1091_GETLIST'
      DESTINATION iv_rfc_dest
      EXPORTING
        selection_set         = iv_selection_set
      TABLES
        return                = et_return
        phrase_list           = et_phrase_list            "#EC ENHOK
        selectionset_list     = et_selectionset_list      "#EC ENHOK
      EXCEPTIONS ##FM_SUBRC_OK
        system_failure        = 7
        communication_failure = 8
        OTHERS                = 9.

  ENDMETHOD.


  METHOD read_ehs_phrase_details.

* Begin correction 2713015 05.11.2018 *********************************
* With SAP note 2713015: Possibility to get longtext of phrase

    CALL FUNCTION 'BAPI_BUS1091_GETDETAIL'
      DESTINATION iv_rfc_dest
      EXPORTING
        scenario            = iv_scenario
        flg_phrase_header   = iv_flg_phrase_header
        flg_phrase_text     = iv_flg_phrase_text
        flg_phrase_longtext = iv_flg_phrase_longtext
      TABLES
        phrase_header       = xt_phrase_header_itab     "#EC ENHOK
        phrase_text         = et_phrase_text_itab       "#EC ENHOK
        phrase_longtext     = et_phrase_longtext_itab.  "#EC ENHOK
* End correction 2713015 05.11.2018 ***********************************
  ENDMETHOD.


  METHOD read_ehs_substances.
* encapsulated reading of the EHS specification (BAPI_BUS1077_GETDETAIL)
*----------------------------------------------------------------------*
* Local Data
*----------------------------------------------------------------------*
* import parameter for the BAPI_BUS1077_GETDETAIL
    DATA lv_scenario                  TYPE bapistdtyp-scenario.
    DATA lv_key_date                  TYPE rcgaddinf-valdat.
    DATA lv_flg_header                TYPE bapistdtyp-boolean.
    DATA lv_flg_header_usage          TYPE bapistdtyp-boolean.
    DATA lv_flg_refsubs               TYPE bapistdtyp-boolean.
    DATA lv_flg_ident                 TYPE bapistdtyp-boolean.
    DATA lv_flg_ident_longtext        TYPE bapistdtyp-boolean.
    DATA lv_flg_ident_usage           TYPE bapistdtyp-boolean.
    DATA lv_flg_matjoin               TYPE bapistdtyp-boolean.
    DATA lv_flg_tplrel                TYPE bapistdtyp-boolean.
    DATA lv_flg_properties            TYPE bapistdtyp-boolean.
    DATA lv_flg_prop_data             TYPE bapistdtyp-boolean.
    DATA lv_flg_prop_details          TYPE bapistdtyp-boolean.
    DATA lv_flg_prop_ftext_longtext   TYPE bapistdtyp-boolean.

* export parameter for the BAPI_BUS1077_GETDETAIL
    DATA lv_flg_abort_on_error        TYPE bapistdtyp-boolean.

* tables parameter for the BAPI_BUS1077_GETDETAIL
    DATA:
      lt_return              TYPE STANDARD TABLE OF bapiret2,
      lt_sub_header          TYPE STANDARD TABLE OF bapi1077rh,
      lt_refsubs             TYPE STANDARD TABLE OF bapi1077rr,
      lt_ident_header        TYPE STANDARD TABLE OF bapi1077ri,
      lt_ident_longtext      TYPE STANDARD TABLE OF bapi1077il,
      lt_ident_sublist       TYPE STANDARD TABLE OF bapi1077rl,
      lt_matjoin             TYPE STANDARD TABLE OF bapi1077mj,
      lt_tplrel              TYPE STANDARD TABLE OF bapi1077tplrel,
      lt_prop_header         TYPE STANDARD TABLE OF bapi1077vh,
      lt_prop_val            TYPE STANDARD TABLE OF bapi1077va,
      lt_prop_data           TYPE STANDARD TABLE OF bapi1077pr,
      lt_prop_component      TYPE STANDARD TABLE OF bapi1077vp,
      lt_prop_usage          TYPE STANDARD TABLE OF bapi1077du,
      lt_prop_reliability    TYPE STANDARD TABLE OF bapi1077dr,
      lt_prop_source         TYPE STANDARD TABLE OF bapi1077ds,
      lt_prop_ftext          TYPE STANDARD TABLE OF bapi1077df,
      lt_prop_ftext_longtext TYPE STANDARD TABLE OF bapi1077fl.


* workareas for the internal and external tables
    DATA:
      lr_return              TYPE REF TO bapiret2,
      lr_sub_header          TYPE REF TO bapi1077rh,
      lr_refsubs             TYPE REF TO bapi1077rr,
      lr_ident_header        TYPE REF TO bapi1077ri,
      lr_ident_longtext      TYPE REF TO bapi1077il,
      lr_ident_sublist       TYPE REF TO bapi1077rl,
      lr_matjoin             TYPE REF TO bapi1077mj,
      lr_tplrel              TYPE REF TO bapi1077tplrel,
      lr_prop_header         TYPE REF TO bapi1077vh,
      lr_prop_val            TYPE REF TO bapi1077va,
      lr_prop_data           TYPE REF TO bapi1077pr,
      lr_prop_component      TYPE REF TO bapi1077vp,
      lr_prop_usage          TYPE REF TO bapi1077du,
      lr_prop_reliability    TYPE REF TO bapi1077dr,
      lr_prop_source         TYPE REF TO bapi1077ds,
      lr_prop_ftext          TYPE REF TO bapi1077df,
      lr_prop_ftext_longtext TYPE REF TO bapi1077fl.

    DATA:
      lr_ehfnd_return              TYPE REF TO bapiret2,
      lr_ehfnd_sub_header          TYPE REF TO bapi1077rh,
      lr_ehfnd_refsubs             TYPE REF TO bapi1077rr,
      lr_ehfnd_ident_header        TYPE REF TO bapi1077ri,
      lr_ehfnd_ident_longtext      TYPE REF TO bapi1077il,
      lr_ehfnd_ident_sublist       TYPE REF TO bapi1077rl,
      lr_ehfnd_matjoin             TYPE REF TO bapi1077mj,
      lr_ehfnd_tplrel              TYPE REF TO bapi1077tplrel,
      lr_ehfnd_prop_header         TYPE REF TO bapi1077vh,
      lr_ehfnd_prop_val            TYPE REF TO bapi1077va,
      lr_ehfnd_prop_data           TYPE REF TO bapi1077pr,
      lr_ehfnd_prop_component      TYPE REF TO bapi1077vp,
      lr_ehfnd_prop_usage          TYPE REF TO bapi1077du,
      lr_ehfnd_prop_reliability    TYPE REF TO bapi1077dr,
      lr_ehfnd_prop_source         TYPE REF TO bapi1077ds,
      lr_ehfnd_prop_ftext          TYPE REF TO bapi1077df,
      lr_ehfnd_prop_ftext_longtext TYPE REF TO bapi1077fl.

* Begin Correction 31.07.2018 2672935 **********************************
    DATA: ls_prop_val   TYPE bapi1077va,
          ls_prop_usage TYPE bapi1077du.
* End Correction 31.07.2018 2672935 ************************************
* Begin Correction 10.01.2019 2739292 **********************************
    DATA: ls_prop_val_tmp TYPE bapi1077va,
          lv_tabix        LIKE sy-tabix.
* End Correction 10.01.2019 2739292 ************************************

*----------------------------------------------------------------------*
* Function Body
*----------------------------------------------------------------------*
* Init
    CLEAR: ev_flg_abort_on_error, lv_flg_abort_on_error.
    REFRESH: et_return, lt_return.


* =====================================================================
* mapping / casting from teh input and exchange parameters to local
* =====================================================================

* mapping / casting of the import parameters
    lv_scenario                = iv_scenario.
    lv_key_date                = iv_key_date.
    lv_flg_header              = iv_flg_header.
    lv_flg_header_usage        = iv_flg_header_usage.
    lv_flg_refsubs             = iv_flg_refsubs.
    lv_flg_ident               = iv_flg_ident.
    lv_flg_ident_longtext      = iv_flg_ident_longtext.
    lv_flg_ident_usage         = iv_flg_ident_usage.
    lv_flg_matjoin             = iv_flg_matjoin.
    lv_flg_tplrel              = iv_flg_tplrel.
    lv_flg_properties          = iv_flg_properties.
    lv_flg_prop_data           = iv_flg_prop_data.
    lv_flg_prop_details        = iv_flg_prop_details.
    lv_flg_prop_ftext_longtext = iv_flg_prop_ftext_longtext.


* mapping / casting of the tables for import or exchange:
* the sub_header
    REFRESH lt_sub_header.
    LOOP AT xt_sub_header REFERENCE INTO lr_ehfnd_sub_header.
      APPEND INITIAL LINE TO lt_sub_header REFERENCE INTO lr_sub_header.
      MOVE-CORRESPONDING lr_ehfnd_sub_header->* TO lr_sub_header->*.
    ENDLOOP.

* the refsubs
    REFRESH lt_refsubs.
    LOOP AT xt_refsubs REFERENCE INTO lr_ehfnd_refsubs.
      APPEND INITIAL LINE TO lt_refsubs REFERENCE INTO lr_refsubs.
      MOVE-CORRESPONDING lr_ehfnd_refsubs->* TO lr_refsubs->*. "#EC ENHOK
    ENDLOOP.

* the ident_header
    REFRESH lt_ident_header.
    LOOP AT xt_ident_header REFERENCE INTO lr_ehfnd_ident_header.
      APPEND INITIAL LINE TO lt_ident_header REFERENCE INTO lr_ident_header.
      MOVE-CORRESPONDING lr_ehfnd_ident_header->* TO lr_ident_header->*. "#EC ENHOK
    ENDLOOP.

* the ident_longtext
    REFRESH lt_ident_longtext.
    LOOP AT xt_ident_longtext REFERENCE INTO lr_ehfnd_ident_longtext.
      APPEND INITIAL LINE TO lt_ident_longtext REFERENCE INTO lr_ident_longtext.
      MOVE-CORRESPONDING lr_ehfnd_ident_longtext->* TO lr_ident_longtext->*.
    ENDLOOP.

* ident_sublist
    REFRESH lt_ident_sublist.
    LOOP AT xt_ident_sublist REFERENCE INTO lr_ehfnd_ident_sublist.
      APPEND INITIAL LINE TO lt_ident_sublist REFERENCE INTO lr_ident_sublist.
      MOVE-CORRESPONDING lr_ehfnd_ident_sublist->* TO lr_ident_sublist->*.
    ENDLOOP.

* matjoin
    REFRESH lt_matjoin.
    LOOP AT xt_matjoin REFERENCE INTO lr_ehfnd_matjoin.
      APPEND INITIAL LINE TO lt_matjoin REFERENCE INTO lr_matjoin.
      MOVE-CORRESPONDING lr_ehfnd_matjoin->* TO lr_matjoin->*.
      " domain extension MATNR
*        cl_ehs_matnr_chk_mapper=>convert_on_output(
*          EXPORTING
*            iv_matnr40   = lr_ehfnd_matjoin->material
*          IMPORTING
*            ev_matnr18   = lr_matjoin->material
*            ev_matnr40   = lr_matjoin->material_long
*            ev_version   = lr_matjoin->material_version
*            ev_guid      = lr_matjoin->material_guid
*            ev_matnr_ext = lr_matjoin->material_external
*        ).
    ENDLOOP.

* tplrel
    REFRESH lt_tplrel.
    LOOP AT xt_tplrel REFERENCE INTO lr_ehfnd_tplrel.
      APPEND INITIAL LINE TO lt_tplrel REFERENCE INTO lr_tplrel.
      MOVE-CORRESPONDING lr_ehfnd_tplrel->* TO lr_tplrel->*. "#EC ENHOK
    ENDLOOP.

* prop_header
    REFRESH lt_prop_header.
    LOOP AT xt_prop_header REFERENCE INTO lr_ehfnd_prop_header.
      APPEND INITIAL LINE TO lt_prop_header REFERENCE INTO lr_prop_header.
      MOVE-CORRESPONDING lr_ehfnd_prop_header->* TO lr_prop_header->*.
    ENDLOOP.

* prop_val
    REFRESH lt_prop_val.
    LOOP AT xt_prop_val REFERENCE INTO lr_ehfnd_prop_val.
      APPEND INITIAL LINE TO lt_prop_val REFERENCE INTO lr_prop_val.
      MOVE-CORRESPONDING lr_ehfnd_prop_val->* TO lr_prop_val->*.
    ENDLOOP.

* prop_data
    REFRESH lt_prop_data.
    LOOP AT xt_prop_data REFERENCE INTO lr_ehfnd_prop_data.
      APPEND INITIAL LINE TO lt_prop_data REFERENCE INTO lr_prop_data.
      MOVE-CORRESPONDING lr_ehfnd_prop_data->* TO lr_prop_data->*.
      " domain extension ATWRT
      lr_prop_data->char_value = lr_ehfnd_prop_data->char_value.
      lr_prop_data->descr_cval = lr_ehfnd_prop_data->descr_cval.
    ENDLOOP.

* prop_component
    REFRESH lt_prop_component.
    LOOP AT xt_prop_component REFERENCE INTO lr_ehfnd_prop_component.
      APPEND INITIAL LINE TO lt_prop_component REFERENCE INTO lr_prop_component.
      MOVE-CORRESPONDING lr_ehfnd_prop_component->* TO lr_prop_component->*.
    ENDLOOP.

* prop_usage
    REFRESH lt_prop_usage.
    LOOP AT xt_prop_usage REFERENCE INTO lr_ehfnd_prop_usage.
      APPEND INITIAL LINE TO lt_prop_usage REFERENCE INTO lr_prop_usage.
      MOVE-CORRESPONDING lr_ehfnd_prop_usage->* TO lr_prop_usage->*.
    ENDLOOP.

* prop_reliability
    REFRESH lt_prop_reliability.
    LOOP AT xt_prop_reliability REFERENCE INTO lr_ehfnd_prop_reliability.
      APPEND INITIAL LINE TO lt_prop_reliability REFERENCE INTO lr_prop_reliability.
      MOVE-CORRESPONDING lr_ehfnd_prop_reliability->* TO lr_prop_reliability->*.
    ENDLOOP.

* prop_source
    REFRESH lt_prop_source.
    LOOP AT xt_prop_source REFERENCE INTO lr_ehfnd_prop_source.
      APPEND INITIAL LINE TO lt_prop_source REFERENCE INTO lr_prop_source.
      MOVE-CORRESPONDING lr_ehfnd_prop_source->* TO lr_prop_source->*.
    ENDLOOP.

* prop_ftext
    REFRESH lt_prop_ftext.
    LOOP AT xt_prop_ftext REFERENCE INTO lr_ehfnd_prop_ftext.
      APPEND INITIAL LINE TO lt_prop_ftext REFERENCE INTO lr_prop_ftext.
      MOVE-CORRESPONDING lr_ehfnd_prop_ftext->* TO lr_prop_ftext->*. "#EC ENHOK
    ENDLOOP.

* prop_ftext_longtext
    REFRESH lt_prop_ftext_longtext.
    LOOP AT xt_prop_ftext_longtext REFERENCE INTO lr_ehfnd_prop_ftext_longtext.
      APPEND INITIAL LINE TO lt_prop_ftext_longtext REFERENCE INTO lr_prop_ftext_longtext.
      MOVE-CORRESPONDING lr_ehfnd_prop_ftext_longtext->* TO lr_prop_ftext_longtext->*.
    ENDLOOP.



* =====================================================================
* call the RFC BAPI to read specifications
* =====================================================================
    CALL FUNCTION 'BAPI_BUS1077_GETDETAIL'
      DESTINATION iv_rfc_dest
      EXPORTING
        scenario                = lv_scenario
        key_date                = lv_key_date
*       CHANGE_NUMBER           =
*       VALFR                   =
*       VALTO                   =
*       FLG_KEY_DATE_SWITCH_ONLY        =
*       FLG_RETURN_WHOLE_INTERVAL       =
*       MULTLANGU_PARAMS        =
        flg_header              = lv_flg_header
        flg_header_usage        = lv_flg_header_usage
        flg_refsubs             = lv_flg_refsubs     "Reference Relationship
        flg_ident               = lv_flg_ident
        flg_ident_longtext      = lv_flg_ident_longtext
        flg_ident_usage         = lv_flg_ident_usage
        flg_matjoin             = lv_flg_matjoin
        flg_tplrel              = lv_flg_tplrel      "Inheritance Relationship
*       FLG_APPLSCP             =
        flg_properties          = lv_flg_properties
        flg_prop_data           = lv_flg_prop_data
        flg_prop_details        = lv_flg_prop_details
        flg_prop_ftext_longtext = lv_flg_prop_ftext_longtext
*       FLG_SORT_PROP_DATA      =
*       FLG_READ_ALL_CHARACT    =
*       FLG_READ_WITHOUT_REF    =
*       FLG_DANG_GOOD_DATA      =
*       FLG_SHOW_LOCAL_INH_REC  =
*       FLG_SKIP_PROP_PACK      =
*       FLG_SKIP_PROP_SPROV     =
*       FLG_SKIP_PROP_CARR      =
      IMPORTING
        flg_abort_on_error      = lv_flg_abort_on_error
      TABLES
        return                  = lt_return
        sub_header              = lt_sub_header
        refsubs                 = lt_refsubs          "#EC ENHOK
        ident_header            = lt_ident_header
        ident_longtext          = lt_ident_longtext
        ident_sublist           = lt_ident_sublist
        matjoin                 = lt_matjoin
        tplrel                  = lt_tplrel
*       APPLSCP                 =
        prop_header             = lt_prop_header
        prop_val                = lt_prop_val
        prop_data               = lt_prop_data
        prop_component          = lt_prop_component
        prop_usage              = lt_prop_usage
        prop_reliability        = lt_prop_reliability
        prop_source             = lt_prop_source
        prop_ftext              = lt_prop_ftext
        prop_ftext_longtext     = lt_prop_ftext_longtext
*       PROP_TAB07              =
*       PROP_TAB0B              =
*       PROP_TAB0D              =
*       PROP_TAB0F              =
*       PROP_PACK               =
*       PROP_SPROV              =
*       PROP_CARR               =
      . "'BAPI_BUS1077_GETDETAIL'



* =====================================================================
* mapping / casting from the local to export and exchange parameters
* =====================================================================

* mapping / casting to the export parameters:
    ev_flg_abort_on_error = lv_flg_abort_on_error.


* mapping / casting of tables for export or exchange:
* return
    REFRESH et_return.
    LOOP AT lt_return REFERENCE INTO lr_return.
      APPEND INITIAL LINE TO et_return REFERENCE INTO lr_ehfnd_return.
      MOVE-CORRESPONDING lr_return->* TO lr_ehfnd_return->*.
    ENDLOOP.

* sub_header
    REFRESH xt_sub_header.
    LOOP AT lt_sub_header REFERENCE INTO lr_sub_header.
      APPEND INITIAL LINE TO xt_sub_header REFERENCE INTO lr_ehfnd_sub_header.
      MOVE-CORRESPONDING lr_sub_header->* TO lr_ehfnd_sub_header->*.
    ENDLOOP.

* refsubs
    REFRESH xt_refsubs.
    LOOP AT lt_refsubs REFERENCE INTO lr_refsubs.
      APPEND INITIAL LINE TO xt_refsubs REFERENCE INTO lr_ehfnd_refsubs.
      MOVE-CORRESPONDING lr_refsubs->* TO lr_ehfnd_refsubs->*. "#EC ENHOK
    ENDLOOP.

* ident_header
    REFRESH xt_ident_header.
    LOOP AT lt_ident_header REFERENCE INTO lr_ident_header.
      APPEND INITIAL LINE TO xt_ident_header REFERENCE INTO lr_ehfnd_ident_header.
      MOVE-CORRESPONDING lr_ident_header->* TO lr_ehfnd_ident_header->*. "#EC ENHOK
    ENDLOOP.

* ident_longtext
    REFRESH xt_ident_longtext.
    LOOP AT lt_ident_longtext REFERENCE INTO lr_ident_longtext.
      APPEND INITIAL LINE TO xt_ident_longtext REFERENCE INTO lr_ehfnd_ident_longtext.
      MOVE-CORRESPONDING lr_ident_longtext->* TO lr_ehfnd_ident_longtext->*.
    ENDLOOP.

* ident_sublist
    REFRESH xt_ident_sublist.
    LOOP AT lt_ident_sublist REFERENCE INTO lr_ident_sublist.
      APPEND INITIAL LINE TO xt_ident_sublist REFERENCE INTO lr_ehfnd_ident_sublist.
      MOVE-CORRESPONDING lr_ident_sublist->* TO lr_ehfnd_ident_sublist->*.
    ENDLOOP.

* matjoin
    REFRESH xt_matjoin.
    LOOP AT lt_matjoin REFERENCE INTO lr_matjoin.
      APPEND INITIAL LINE TO xt_matjoin REFERENCE INTO lr_ehfnd_matjoin.
      MOVE-CORRESPONDING lr_matjoin->* TO lr_ehfnd_matjoin->*.
      " domain extension MATNR
*        cl_ehs_matnr_chk_mapper=>convert_on_input(
*          EXPORTING
*            iv_matnr18    = lr_matjoin->material
*            iv_guid       = lr_matjoin->material_guid
*            iv_version    = lr_matjoin->material_version
*            iv_matnr40    = lr_matjoin->material_long
*            iv_matnr_ext  = lr_matjoin->material_external
*          IMPORTING
*            ev_matnr40    = lr_ehfnd_matjoin->material
*          EXCEPTIONS
*            OTHERS        = 0
*        ).
    ENDLOOP.

* tplrel
    REFRESH xt_tplrel.
    LOOP AT lt_tplrel REFERENCE INTO lr_tplrel.
      APPEND INITIAL LINE TO xt_tplrel REFERENCE INTO lr_ehfnd_tplrel.
      MOVE-CORRESPONDING lr_tplrel->* TO lr_ehfnd_tplrel->*. "#EC ENHOK
    ENDLOOP.

* prop_header
    REFRESH xt_prop_header.
    LOOP AT lt_prop_header REFERENCE INTO lr_prop_header.
      APPEND INITIAL LINE TO xt_prop_header REFERENCE INTO lr_ehfnd_prop_header.
      MOVE-CORRESPONDING lr_prop_header->* TO lr_ehfnd_prop_header->*.
    ENDLOOP.

* prop_val
* Begin Correction 31.07.2018 2672935 ************************************
* eleminate inactive instances of specification
* search for the active usages (ESTDU)
* Begin Correction 07.09.2018 2691461 ************************************
* Sort table lt_prop_val
    SORT lt_prop_val BY record_no.
* End Correction 07.09.2018 2691461 **************************************
    LOOP AT lt_prop_usage INTO ls_prop_usage WHERE act_ind = abap_true.
      READ TABLE lt_prop_val INTO ls_prop_val
        WITH KEY record_no = ls_prop_usage-ref_recn
        BINARY SEARCH.
      IF sy-subrc = 0.
*     mark the current instance as active in the internal table
*     for that use the non-used indicator park-ind
* Begin Correction 10.01.2019 2739292 ************************************
*     due to inheritance scenario several identical entries might be in
*     lt_prop_val. So, a single MODIFY is not sufficient and we have to
*     loop over the entries
        lv_tabix = sy-tabix.
        LOOP AT lt_prop_val FROM lv_tabix INTO ls_prop_val_tmp.
          IF ls_prop_val_tmp-record_no <> ls_prop_usage-ref_recn. EXIT. ENDIF.
          ls_prop_val_tmp-park_ind = abap_true.
          MODIFY lt_prop_val FROM ls_prop_val_tmp TRANSPORTING park_ind.
        ENDLOOP.
* End Correction 10.01.2019 2739292 **************************************
      ENDIF.
    ENDLOOP.
* delete the inactive instances from the internal ESTVA tab
* and reset the park_ind for the active instances
    LOOP AT lt_prop_val INTO ls_prop_val.
      IF ls_prop_val-park_ind = abap_false.
        DELETE lt_prop_val.
      ELSE.
        ls_prop_val-park_ind = abap_false.
        MODIFY lt_prop_val INDEX sy-tabix FROM ls_prop_val
          TRANSPORTING park_ind.
      ENDIF.
    ENDLOOP.
* End Correction 31.07.2018 2672935 **************************************
    REFRESH xt_prop_val.
    LOOP AT lt_prop_val REFERENCE INTO lr_prop_val.
      APPEND INITIAL LINE TO xt_prop_val REFERENCE INTO lr_ehfnd_prop_val.
      MOVE-CORRESPONDING lr_prop_val->* TO lr_ehfnd_prop_val->*.
    ENDLOOP.

* prop_data
    REFRESH xt_prop_data.
    LOOP AT lt_prop_data REFERENCE INTO lr_prop_data.
      APPEND INITIAL LINE TO xt_prop_data REFERENCE INTO lr_ehfnd_prop_data.
      MOVE-CORRESPONDING lr_prop_data->* TO lr_ehfnd_prop_data->*.
      " domain extension ATWRT
      IF lr_prop_data->char_value IS NOT INITIAL.
        lr_ehfnd_prop_data->char_value = lr_prop_data->char_value.
      ENDIF.
      IF lr_prop_data->descr_cval IS NOT INITIAL.
        lr_ehfnd_prop_data->descr_cval = lr_prop_data->descr_cval.
      ENDIF.
    ENDLOOP.

* prop_component
    REFRESH xt_prop_component.
    LOOP AT lt_prop_component REFERENCE INTO lr_prop_component.
      APPEND INITIAL LINE TO xt_prop_component REFERENCE INTO lr_ehfnd_prop_component.
      MOVE-CORRESPONDING lr_prop_component->* TO lr_ehfnd_prop_component->*.
    ENDLOOP.

* prop_usage
    REFRESH xt_prop_usage.
    LOOP AT lt_prop_usage REFERENCE INTO lr_prop_usage.
      APPEND INITIAL LINE TO xt_prop_usage REFERENCE INTO lr_ehfnd_prop_usage.
      MOVE-CORRESPONDING lr_prop_usage->* TO lr_ehfnd_prop_usage->*.
    ENDLOOP.

* prop_reliability
    REFRESH xt_prop_reliability.
    LOOP AT lt_prop_reliability REFERENCE INTO lr_prop_reliability.
      APPEND INITIAL LINE TO xt_prop_reliability REFERENCE INTO lr_ehfnd_prop_reliability.
      MOVE-CORRESPONDING lr_prop_reliability->* TO lr_ehfnd_prop_reliability->*.
    ENDLOOP.

* prop_source
    REFRESH xt_prop_source.
    LOOP AT lt_prop_source REFERENCE INTO lr_prop_source.
      APPEND INITIAL LINE TO xt_prop_source REFERENCE INTO lr_ehfnd_prop_source.
      MOVE-CORRESPONDING lr_prop_source->* TO lr_ehfnd_prop_source->*.
    ENDLOOP.

* prop_ftext
    REFRESH xt_prop_ftext.
    LOOP AT lt_prop_ftext REFERENCE INTO lr_prop_ftext.
      APPEND INITIAL LINE TO xt_prop_ftext REFERENCE INTO lr_ehfnd_prop_ftext.
      MOVE-CORRESPONDING lr_prop_ftext->* TO lr_ehfnd_prop_ftext->*. "#EC ENHOK
    ENDLOOP.

* prop_ftext_longtext
    REFRESH xt_prop_ftext_longtext.
    LOOP AT lt_prop_ftext_longtext REFERENCE INTO lr_prop_ftext_longtext.
      APPEND INITIAL LINE TO xt_prop_ftext_longtext REFERENCE INTO lr_ehfnd_prop_ftext_longtext.
      MOVE-CORRESPONDING lr_prop_ftext_longtext->* TO lr_ehfnd_prop_ftext_longtext->*. "#EC ENHOK
    ENDLOOP.



* =====================================================================
* the final actions
* =====================================================================

    SORT xt_ident_header BY langu id_type id_categry.

  ENDMETHOD.


  METHOD read_specifications_via_api.

    DATA: ls_addinf TYPE rcgaddinf.
    DATA: ls_scenario TYPE espap_new_scenario_type.
    DATA: lt_error_api TYPE espap_exterror_tab_type.
    DATA: lt_sub_header_api TYPE esprh_apirh_tab_type.
    DATA: lt_material_api TYPE esprh_apimj_tab_type.
    DATA: lt_identifier_api TYPE esprh_apiri_tab_type.
    DATA: lt_identifier_longtext_api TYPE esprh_apiil_tab_type.
    DATA: ls_sub_header_api TYPE esprh_apirh_wa_type.
    DATA: ls_material_api TYPE esprh_apimj_wa_type.
    DATA: ls_identifier_api TYPE esprh_apiri_wa_type.
    DATA: ls_identifier_longtext TYPE /vpcoe/s_ehs_api_ident_lt.
    DATA: ls_error TYPE /vpcoe/s_ehs_api_msg.  ##NEEDED
    DATA: lv_flg_with_inh_data TYPE boolean.

    REFRESH: et_error, et_identifier_longtext.

    MOVE-CORRESPONDING is_scenario TO ls_scenario.
    ls_addinf-valdat = sy-datum.

    " map EHFND structures to API structures to avoid issues with customer enhancements
    LOOP AT ct_sub_header INTO DATA(ls_sub_header).
      MOVE-CORRESPONDING ls_sub_header TO ls_sub_header_api.
      INSERT ls_sub_header_api INTO TABLE lt_sub_header_api.
    ENDLOOP.
    LOOP AT ct_material INTO DATA(ls_material).
      MOVE-CORRESPONDING ls_material TO ls_material_api.
      INSERT ls_material_api INTO TABLE lt_material_api.
    ENDLOOP.
    LOOP AT ct_identifier INTO DATA(ls_identifier).
      MOVE-CORRESPONDING ls_identifier TO ls_identifier_api.
      INSERT ls_identifier_api INTO TABLE lt_identifier_api.
    ENDLOOP.

    lv_flg_with_inh_data = abap_true.
    IF ( iv_flg_with_inh_data IS SUPPLIED ).
      lv_flg_with_inh_data = iv_flg_with_inh_data.
    ENDIF.

    CALL FUNCTION 'C1F5_SPECIFICATIONS_READ'
      EXPORTING
        i_scenario           = ls_scenario
        i_addinf             = ls_addinf
        i_flg_with_inh_data  = lv_flg_with_inh_data
      IMPORTING
        e_flg_internal_error = ev_flg_internal_error
        e_flg_error          = ev_flg_error
        e_flg_warning        = ev_flg_warning
      TABLES
        x_spec_head_tab      = lt_sub_header_api
        x_material_tab       = lt_material_api
        x_identifier_tab     = lt_identifier_api
        x_ident_longtext_tab = lt_identifier_longtext_api
        e_error_tab          = lt_error_api.

    " map API structures back to EHFND structures
    CLEAR: ct_sub_header, ct_material, ct_identifier.
    LOOP AT lt_sub_header_api INTO ls_sub_header_api.
      MOVE-CORRESPONDING ls_sub_header_api TO ls_sub_header.
      INSERT ls_sub_header INTO TABLE ct_sub_header.
    ENDLOOP.
    LOOP AT lt_material_api INTO ls_material_api.
      MOVE-CORRESPONDING ls_material_api TO ls_material.
      INSERT ls_material INTO TABLE ct_material.
    ENDLOOP.
    LOOP AT lt_identifier_api INTO ls_identifier_api.
      MOVE-CORRESPONDING ls_identifier_api TO ls_identifier.
      INSERT ls_identifier INTO TABLE ct_identifier.
    ENDLOOP.
    LOOP AT lt_identifier_longtext_api INTO DATA(ls_identifier_longtext_api).
      MOVE-CORRESPONDING ls_identifier_longtext_api TO ls_identifier_longtext.
      INSERT ls_identifier_longtext INTO TABLE et_identifier_longtext.
    ENDLOOP.
    LOOP AT lt_error_api INTO DATA(ls_error_api).
      MOVE-CORRESPONDING ls_error_api TO ls_error.
      INSERT ls_error INTO TABLE et_error.
    ENDLOOP.

  ENDMETHOD.


  METHOD recns_to_subids.

* Converts substance RECNs into the according SUBIDs

*--------------------------------------------------------------------*
* Declarations
*--------------------------------------------------------------------*

    DATA lr_s_subid      TYPE REF TO lty_recn_subid.
    DATA lt_subid        TYPE STANDARD TABLE OF lty_recn_subid.
    DATA lr_s_recn_subid TYPE REF TO /vpcoe/s_spc_recn_subid.

*--------------------------------------------------------------------*
* Method Implementation
*--------------------------------------------------------------------*

    IF ct_recn_subid IS INITIAL.
      RETURN.
    ENDIF.

* Fill internal structure
    LOOP AT ct_recn_subid REFERENCE INTO lr_s_recn_subid.
      APPEND INITIAL LINE TO lt_subid REFERENCE INTO lr_s_subid.
      lr_s_subid->recn  = lr_s_recn_subid->recn.
    ENDLOOP.

* Determine the subids
    CALL FUNCTION 'C1A0_TRANSLATE_RECNS_TO_SUBIDS'
      EXPORTING
        i_valdat    = sy-datum
      TABLES
        x_subid_tab = lt_subid.

    SORT lt_subid BY recn.

* Update the table
*--------------------------------------------------------------------*
    LOOP AT ct_recn_subid REFERENCE INTO lr_s_recn_subid.
      READ TABLE lt_subid REFERENCE INTO lr_s_subid WITH KEY recn = lr_s_recn_subid->recn BINARY SEARCH.
      IF sy-subrc = 0.
        lr_s_recn_subid->subid = lr_s_subid->subid.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD recn_get_next.

    CALL FUNCTION 'C149_RECN_GET_NEXT'
      IMPORTING
        number = rv_recn.

  ENDMETHOD.


  METHOD recn_to_subid.

* Determine the Substance ID for a given RECN

    CLEAR: ev_subid, ev_not_found_ind.

    CALL FUNCTION 'C1A0_TRANSLATE_RECN_TO_SUBID'
      EXPORTING
        i_recn               = iv_recn
        i_valdat             = sy-datum
        i_flg_by_buf_read    = esp1_true
        i_flg_no_exist_check = esp1_false
        i_flg_date_indep     = esp1_false
      IMPORTING
        e_subid              = ev_subid
      EXCEPTIONS
        subid_not_found      = 1
        internal_error       = 2
        OTHERS               = 3.


    IF sy-subrc <> 0.
*   substance not found
      ev_not_found_ind = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD reset_spec_buffers.

    CALL FUNCTION 'C148_RESET_SUBSTANCE_BUFFERS'.

  ENDMETHOD.


  METHOD save_spec_to_db_for_api.

    CALL FUNCTION 'C1F2_SUBSTANCES_SAVE_TO_DB' .

  ENDMETHOD.


  METHOD shlp_exit_substance_id_sel.

* ----------------------------------------------------------------------
* Local data
* ----------------------------------------------------------------------
    CONSTANTS: lc_info_only_scenario    TYPE esescen VALUE '01'.
    DATA: lv_validity_date         TYPE uxx_validity_date.
    DATA: lv_substance_id          TYPE /vpcoe/substance_id.
    DATA: ls_spec_data             TYPE estrh.
    DATA: ls_substancelist         TYPE bapisub_01.
    DATA: lt_substancelist         TYPE TABLE OF bapisub_01.
    DATA: ls_sub_header            TYPE bapi1077rh.
    DATA: lt_sub_header            TYPE TABLE OF bapi1077rh.
    DATA: lt_ident_header          TYPE TABLE OF bapi1077ri.
    DATA: ls_substance             TYPE /vpcoe/s_substance.
    DATA: lt_return                TYPE TABLE OF bapiret2.
    DATA: lv_rfc_destination       TYPE rfc_dest.
    DATA: ls_int_dest              TYPE /vpcoe/s_int_dest.
    DATA: lv_flg_identifier        TYPE abap_bool.

* ----------------------------------------------------------------------
* Function body
* ----------------------------------------------------------------------
* init
    CLEAR lv_validity_date.
    CLEAR lv_substance_id.
    CLEAR ls_spec_data.
    CLEAR ls_substancelist.
    REFRESH lt_substancelist.
    REFRESH lt_sub_header.
    CLEAR ls_sub_header.
    CLEAR ls_substance.
    REFRESH lt_return.

* read RFC Destination
*    CALL METHOD cl_ehfnd_int_dest=>read_int_dest
*      EXPORTING
*        iv_dest_type = cl_ehfnd_int_dest=>gc_destination_ehs
*      IMPORTING
*        es_int_dest  = ls_int_dest.

    lv_rfc_destination = 'VPCOE_RDP_PLM'.
*    lv_rfc_destination = ls_int_dest-rfc_destination.

*----------------------------------------------------------------------
*   Step A: Move imported values to local values
*----------------------------------------------------------------------
    lv_validity_date     = iv_validity_date.
    lv_substance_id      = iv_substance_id.
    ls_spec_data-subcat  = iv_substance_type.

* if no search criteria given then search for all using wildcard
    IF ( ls_spec_data IS INITIAL ) AND
       ( lv_substance_id IS INITIAL ).
      lv_substance_id = '*'.
    ENDIF.




*----------------------------------------------------------------------
*   Step B: Search by substance information
*----------------------------------------------------------------------
* search for substances
    CALL FUNCTION 'BAPI_BUS1077_GETLIST'
      DESTINATION lv_rfc_destination
      EXPORTING
        key_date            = lv_validity_date
        substance           = lv_substance_id
        ##ENH_OK spec_data  = ls_spec_data
        identifier_category = iv_identifier_type " Category and Type are the other
        identifier_type     = iv_identifier_cat  " way round in the BAPI
        identifier          = iv_identifier
      TABLES
        return              = lt_return
        substancelist       = lt_substancelist.

* if there is an identifier listing requested, we need to read
* the identifier table
    IF iv_ident_listing IS NOT INITIAL.
      lv_flg_identifier = abap_true.
    ENDIF.

    IF ( lt_return IS INITIAL ) AND
       ( NOT lt_substancelist IS INITIAL ).

      LOOP AT lt_substancelist INTO ls_substancelist.
        ls_sub_header-substance = ls_substancelist-substance.
        APPEND ls_sub_header TO lt_sub_header.
      ENDLOOP.
*   read details
      CALL FUNCTION 'BAPI_BUS1077_GETDETAIL'
        DESTINATION lv_rfc_destination
        EXPORTING
          scenario     = lc_info_only_scenario
          key_date     = lv_validity_date
          flg_header   = abap_true
          flg_ident    = lv_flg_identifier
        TABLES
          return       = lt_return
          sub_header   = lt_sub_header
          ident_header = lt_ident_header.  "#EC ENHOK

*   sort the table for BINARY SEARCH  access in the extract method
      SORT lt_ident_header BY recno_root id_type id_categry langu.

      IF ( lt_return IS INITIAL ) AND
         ( NOT lt_sub_header IS INITIAL ).
        LOOP AT lt_sub_header INTO ls_sub_header.
          ls_substance-substance_type = ls_sub_header-subcategry.
          ls_substance-substance_id = ls_sub_header-substance.
          IF iv_ident_listing IS INITIAL.
            ls_substance-identifier = ls_substancelist-identifier.
          ELSE.
            ls_substance-identifier = extract_identifier(
              iv_recn = ls_sub_header-recno_root
              it_ident_header = lt_ident_header
              iv_ident_listing = iv_ident_listing
            ).
          ENDIF.
          APPEND ls_substance TO et_substance.
        ENDLOOP.
      ELSE.
        RETURN.
      ENDIF.

    ELSE.

      RETURN.

    ENDIF.

  ENDMETHOD.


  METHOD subids_to_recns.

* Converts substance IDs into the according recns

*--------------------------------------------------------------------*
* Declarations
*--------------------------------------------------------------------*

    DATA lr_s_subid      TYPE REF TO lty_recn_subid.
    DATA lt_subid        TYPE STANDARD TABLE OF lty_recn_subid.
    DATA lr_s_recn_subid TYPE REF TO /vpcoe/s_spc_recn_subid.

*--------------------------------------------------------------------*
* Method Implementation
*--------------------------------------------------------------------*

    IF ct_recn_subid IS INITIAL.
      RETURN.
    ENDIF.

* Fill internal structure
    LOOP AT ct_recn_subid REFERENCE INTO lr_s_recn_subid.
      APPEND INITIAL LINE TO lt_subid REFERENCE INTO lr_s_subid.
      lr_s_subid->subid = lr_s_recn_subid->subid.
    ENDLOOP.

    CALL FUNCTION 'C1A0_TRANSLATE_SUBIDS_TO_RECNS'
      EXPORTING
        i_valdat                      = sy-datum
        i_flg_db_search_at_big_buffer = abap_false
      TABLES
        x_subid_tab                   = lt_subid.

    SORT lt_subid BY subid.

* Update the table
*--------------------------------------------------------------------*
    LOOP AT ct_recn_subid REFERENCE INTO lr_s_recn_subid.
      READ TABLE lt_subid REFERENCE INTO lr_s_subid WITH KEY subid = lr_s_recn_subid->subid BINARY SEARCH.
      IF sy-subrc = 0.
        lr_s_recn_subid->recn  = lr_s_subid->recn.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD subid_to_recn.

* Determine the RECN for a given substance ID

    CLEAR: ev_recn, ev_not_found_ind.

    CALL FUNCTION 'C148_TRANSLATE_SUBID_TO_RECN_1'
      EXPORTING
        i_subid  = iv_subid
        i_valdat = sy-datum
      IMPORTING
        e_recn   = ev_recn.


    IF ev_recn IS INITIAL.
      ev_not_found_ind = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
