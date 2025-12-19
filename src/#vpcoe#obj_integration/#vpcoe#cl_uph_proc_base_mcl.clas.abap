class /VPCOE/CL_UPH_PROC_BASE_MCL definition
  public
  inheriting from /VPCOE/CL_UPH_PROC_BASE
  abstract
  create public .

public section.

  interfaces /VPCOE/IF_UPH_ENTITY_MCL_PROC .

  methods CONSTRUCTOR .

  methods /VPCOE/IF_UPH_ENTITY_PROC~DESERIALIZE_SELECTION_PARAMS
    redefinition .
  methods /VPCOE/IF_UPH_ENTITY_PROC~INIT_PROCESSOR
    redefinition .
  methods /VPCOE/IF_UPH_ENTITY_PROC~PREPARE_PROCESS
    redefinition .
  methods /VPCOE/IF_UPH_ENTITY_PROC~PROCESS_PACKAGE
    redefinition .
  methods /VPCOE/IF_UPH_ENTITY_PROC~GET_PARAMETERS
    redefinition .
protected section.

  data MT_MAT_CLAS_PARAMS type /VPCOE/IF_UPH_ENTITY_MCL_PROC=>GTY_T_MAT_CLASS_PARAM .
  data MS_PARAMETERS type /VPCOE/S_PCKG_MATCLAS_INPUT .
  data MT_RELVNT_MAT_CLAS type /VPCOE/IF_UPH_ENTITY_MCL_PROC=>GTY_T_MAT_CLASS .

  methods DETERMINE_MAT_CLAS_PARAMS .
  methods CALL_MAT_CLAS_BAPI
    importing
      !IS_MAT_CLAS_PARAMS type /VPCOE/IF_UPH_ENTITY_MCL_PROC=>GTY_MAT_CLASS_PARAMETER
    exporting
      !ET_VALUES_NUM type TT_BAPI1003_ALLOC_VALUES_NUM
      !ET_VALUES_CHAR type TT_BAPI1003_ALLOC_VALUES_CHAR
      !ET_VALUES_CURR type TT_BAPI1003_ALLOC_VALUES_CURR .
  methods DETERMINE_DELTA_RELEVANT_PARAM
    importing
      !IR_PROTOCOL_DELTA type ref to /VPCOE/UPH_PROT .
ENDCLASS.



CLASS /VPCOE/CL_UPH_PROC_BASE_MCL IMPLEMENTATION.


  method /VPCOE/IF_UPH_ENTITY_MCL_PROC~GET_RELEVANT_MAT_CLAS.
  endmethod.


  METHOD /vpcoe/if_uph_entity_mcl_proc~map_mat_clas_data.

  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_mcl_proc~retrieve_mat_clas_data.
    DATA(lv_mat_clas_map) = NEW lcl_surdp_uph_wrap_mcl_map(  ).

    LOOP AT it_mat_clas_params INTO DATA(ls_mat_clas_params).

      call_mat_clas_bapi( EXPORTING is_mat_clas_params = ls_mat_clas_params
                          IMPORTING et_values_char = DATA(lt_character_value)
                                    et_values_num  = DATA(lt_number_value)
                                    et_values_curr = DATA(lt_currency_value) ).
      DATA(lo_wrap_mcl) = NEW /vpcoe/cl_uph_wrap_mcl_version( is_mcl_header    = ls_mat_clas_params
                                                             it_mcl_character_data = lt_character_value
                                                             it_mcl_currency_data = lt_currency_value
                                                             it_mcl_number_data = lt_number_value ).
      lv_mat_clas_map->add_version( lo_wrap_mcl  ).

    ENDLOOP.

    rt_mat_clas_data = lv_mat_clas_map->get_mat_class_wrapper(  ).
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~deserialize_selection_params.
    DATA ls_selection_params TYPE /vpcoe/s_pckg_matclas_input.

    CLEAR es_selection_params.

    /vpcoe/cl_plm_helper=>deserialize_json( EXPORTING iv_json = iv_json_str
                                            CHANGING  cs_data = ls_selection_params ).

    es_selection_params = ls_selection_params.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~get_parameters.
    rv_result = REF #( ms_parameters ).
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~init_processor.
    super->/vpcoe/if_uph_entity_proc~init_processor( iv_upload_entity = iv_upload_entity iv_upload_mode = iv_upload_mode ).

    "store the input parameter only in case it is a full load
    CLEAR ms_parameters.

    IF is_parameters IS NOT INITIAL AND mv_upload_mode = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.

      MOVE-CORRESPONDING is_parameters TO ms_parameters.

    ENDIF.

    mv_initialized = abap_true.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~prepare_process.
    "This method is used to determine the relevant specifications to retrieve according the current
    "upload mode and the given parameters for the upload entity
    DATA: lv_params_string         TYPE string,
          ls_input_params_frm_prot TYPE /vpcoe/s_pckg_matclas_input,
          lt_messages              TYPE /vpcoe/t_uph_msg,
          lt_sub_keys_tmp          TYPE /vpcoe/if_uph_entity_mcl_proc~gty_t_mat_class_param,
          lr_protocol_delta        TYPE REF TO /vpcoe/uph_prot, "surdpd_uph_prot,
          lr_protocol_selection    TYPE REF TO /vpcoe/uph_prot. "surdpd_uph_prot.

    IF mv_upload_mode = /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta.
      DATA(lt_protocol)  =  /vpcoe/if_uph_entity_proc~read_from_protocol( iv_upload_entity = mv_upload_entity
                                                                          iv_only_success  = abap_true ).
      SORT lt_protocol BY start_timestamp DESCENDING.

      "get last successful protocol entry, might be a full or delta load: relevant for delta timestamp
      READ TABLE lt_protocol INDEX 1 REFERENCE INTO lr_protocol_delta.

      "get last successful full load protocol: relevant for selection
      READ TABLE lt_protocol REFERENCE INTO lr_protocol_selection WITH KEY upload_mode = /vpcoe/if_uph_entity_proc~gc_upload_mode-gc_upload_mode_full.

      IF lr_protocol_selection IS INITIAL.

        MESSAGE s018(/vpcoe/plm).
        APPEND INITIAL LINE TO lt_messages ASSIGNING FIELD-SYMBOL(<fs_messages>).
        IF <fs_messages> IS ASSIGNED.
          <fs_messages>-msgty = /vpcoe/cl_uph_exec_load_wrap=>gc_msgty_w.
          <fs_messages>-msgid = /vpcoe/cl_uph_exec_load_wrap=>gc_default_msgclass.
          <fs_messages>-msgno = 018.
        ENDIF.
        mo_logger->add_messages( it_messages = lt_messages ).
        RETURN.

      ELSE.

        "deserialize parameter
        lv_params_string = lr_protocol_selection->selection.
        /vpcoe/if_uph_entity_proc~deserialize_selection_params( EXPORTING  iv_json_str = lv_params_string
                                                                IMPORTING  es_selection_params = ls_input_params_frm_prot ).
        ms_parameters = ls_input_params_frm_prot.
      ENDIF.

    ENDIF.

    "Retrieve with filter criteria:
    determine_mat_clas_params( ).

    CASE mv_upload_mode.
      WHEN /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.

      WHEN /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta.
        determine_delta_relevant_param( lr_protocol_delta ).
    ENDCASE.

    mv_prepared = abap_true.

    rv_record_cnt = lines( mt_mat_clas_params ).
    MESSAGE s039(/vpcoe/plm) WITH rv_record_cnt INTO DATA(lv_msg_str).
    mo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).

  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~process_package.
    DATA: lv_top             TYPE i,
          lt_mat_clas_params TYPE /vpcoe/if_uph_entity_mcl_proc~gty_t_mat_class_param.

    "check if preparation did already take place
    IF mv_prepared = abap_false.
      /vpcoe/if_uph_entity_proc~prepare_process( ).
    ENDIF.

    "collect relevant keys for the current package
    lv_top = ( iv_act_package - 1 ) * iv_package_size + 1.

    DO iv_package_size TIMES.
      IF lv_top <= lines( mt_mat_clas_params ) .

        READ TABLE mt_mat_clas_params INDEX lv_top REFERENCE INTO DATA(lr_mat_clas_params).
        APPEND INITIAL LINE TO lt_mat_clas_params REFERENCE INTO DATA(lr_mat_clas_pckg).
        MOVE-CORRESPONDING lr_mat_clas_params->* TO lr_mat_clas_pckg->*.

        ADD 1 TO lv_top.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    DATA(lt_entity_data) = /vpcoe/if_uph_entity_mcl_proc~map_mat_clas_data( /vpcoe/if_uph_entity_mcl_proc~retrieve_mat_clas_data( lt_mat_clas_params ) ).

    rt_entity_data = lt_entity_data.

    "log result
    DATA(lv_rec_cnt) = lines( lt_mat_clas_params ).
    DATA(lv_mat_clas_count) = lines( rt_entity_data ).
    DATA(lv_upload_entity_dval) = VALUE dd07v(  ).
    DATA(lv_domvalue) = VALUE domvalue_l(  ).
    lv_domvalue = mv_upload_entity.
    " Get the upload entity text
    CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
      EXPORTING
        domname  = '/VPCOE/UPLOAD_ENTITY'
        value    = lv_domvalue
      IMPORTING
        dd07v_wa = lv_upload_entity_dval.

    MESSAGE s040(/vpcoe/plm) WITH lv_mat_clas_count lv_mat_clas_count lv_upload_entity_dval-ddtext INTO DATA(lv_msg_str).
    mo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 ) ) ).
  ENDMETHOD.


  METHOD call_mat_clas_bapi.

    DATA: lt_return TYPE TABLE OF bapiret2 .
    DATA: lv_status TYPE  clstatus .
    DATA: lv_standardcls TYPE  stdclass .

    CLEAR: et_values_num,
           et_values_char,
           et_values_curr.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey        = is_mat_clas_params-objek
        objecttable      = 'MARA'
        classnum         = is_mat_clas_params-class
        classtype        = '001'
        keydate          = is_mat_clas_params-datuv
        unvaluated_chars = 'X'
      IMPORTING
        status           = lv_status
        standardclass    = lv_standardcls
      TABLES
        allocvaluesnum   = et_values_num
        allocvalueschar  = et_values_char
        allocvaluescurr  = et_values_curr
        return           = lt_return.


  ENDMETHOD.


  METHOD constructor.

    super->constructor(  ).

  ENDMETHOD.


  METHOD determine_delta_relevant_param.

    IF mt_mat_clas_params IS NOT INITIAL.

      SELECT objek, datub, utime FROM kssk
        LEFT JOIN mara ON kssk~objek = mara~matnr
        LEFT JOIN cdhdr ON kssk~objek = cdhdr~objectid
        FOR ALL ENTRIES IN @mt_mat_clas_params
        WHERE objek = @mt_mat_clas_params-objek
          AND klart = '001'
          AND mafid = 'O'
        INTO TABLE @DATA(lt_objek_tsmp_for_delta).

      SORT lt_objek_tsmp_for_delta BY datub.

      CONVERT TIME STAMP ir_protocol_delta->start_timestamp TIME ZONE ' ' INTO
                  DATE DATA(lv_prot_date) TIME DATA(lv_prot_time).

      DATA lv_chng_date TYPE date.
      DATA lv_chng_time TYPE tims.

      LOOP AT lt_objek_tsmp_for_delta INTO DATA(ls_objek_tsmp_for_delta).

        lv_chng_date = ls_objek_tsmp_for_delta-datub.
        lv_chng_time = ls_objek_tsmp_for_delta-utime.

        IF ( lv_chng_date < lv_prot_date ) OR ( lv_chng_date = lv_prot_date AND lv_chng_time < lv_prot_time ).
          DELETE lt_objek_tsmp_for_delta INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

      LOOP AT mt_mat_clas_params INTO DATA(ls_mat_clas_params).
        IF NOT line_exists( lt_objek_tsmp_for_delta[ objek = ls_mat_clas_params-objek ] ).
          DELETE TABLE mt_mat_clas_params FROM ls_mat_clas_params.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD determine_mat_clas_params.

    DATA: lv_valid_from          TYPE datuv,
          lv_valid_to            TYPE datub,
          lt_internal_matnr      TYPE STANDARD TABLE OF ausp,
          lt_matnr               TYPE STANDARD TABLE OF ausp,
          lt_selpar_matnr        TYPE RANGE OF matnr_d,
          lv_lines_count         TYPE i VALUE `500`,
          lt_relevant_clas_range TYPE RANGE OF klasse_d,
          lt_inob                TYPE STANDARD TABLE OF inob,
          lt_relevant_clas       TYPE /vpcoe/if_uph_entity_mcl_proc~gty_t_mat_class.

    CLEAR mt_mat_clas_params.

    lv_valid_from = ms_parameters-valfromdate.
    lv_valid_to = ms_parameters-valtodate.

    IF lv_valid_from IS INITIAL.
      "in case there is not from date -> init with current date
      IF lv_valid_to IS INITIAL.
        lv_valid_from = sy-datum.
        lv_valid_to   = sy-datum.
      ELSE.
        lv_valid_from = /vpcoe/if_uph_entity_proc~gc_date_bot.
      ENDIF.
    ELSE.
      "use dynamic to-date
      IF lv_valid_to IS INITIAL.
        IF sy-datum >= lv_valid_from.
          lv_valid_to = sy-datum.
        ELSE.
          lv_valid_to = lv_valid_from.
        ENDIF.
      ENDIF.
    ENDIF.

    lt_relevant_clas = /vpcoe/if_uph_entity_mcl_proc~get_relevant_mat_clas( ).

    IF lt_relevant_clas IS INITIAL.
      MESSAGE s042(/vpcoe/plm) INTO DATA(lv_msg_str).
      mo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 ) ) ).
      RETURN.
    ENDIF.

    LOOP AT lt_relevant_clas ASSIGNING FIELD-SYMBOL(<ls_relevant_clas>).
      lt_relevant_clas_range = VALUE #(
                               BASE lt_relevant_clas_range ( sign   = 'I'
                                                             option = 'EQ'
                                                             high   = ' '
                                                             low    = <ls_relevant_clas>-class_number ) ).
    ENDLOOP.

    " Since material classification uses '00000000' as BOT we have to set lv_valid_from to initial
    IF lv_valid_from = '00010101'.
      CLEAR lv_valid_from.
    ENDIF.

    LOOP AT ms_parameters-material INTO DATA(lr_material) GROUP BY ( sy-tabix - 1 ) DIV lv_lines_count + 1.
      CLEAR lt_selpar_matnr.
      LOOP AT GROUP lr_material ASSIGNING FIELD-SYMBOL(<ls_r_material>).
        lt_selpar_matnr = VALUE #( BASE lt_selpar_matnr ( <ls_r_material> ) ).
      ENDLOOP.

      SELECT DISTINCT matnr AS objek
        FROM mara
         APPENDING CORRESPONDING FIELDS OF TABLE @lt_matnr
       WHERE matnr IN @lt_selpar_matnr.
    ENDLOOP.

    "Check if multiple objects is active
    SELECT SINGLE multobj FROM tcla WHERE klart = '001' INTO @DATA(lv_result).

    IF lv_result = abap_true.
      " Range ms_parameters-material may contains huge amount of entries and it will leads to the dump in Select
      " This is replacement from `where` to `For all entries`
      SELECT DISTINCT cuobj AS objek
        FROM inob
         APPENDING CORRESPONDING FIELDS OF TABLE @lt_internal_matnr
        FOR ALL ENTRIES IN @lt_matnr
       WHERE klart = '001'
         AND obtab = 'MARA'
         AND objek = @lt_matnr-objek.


      "Determine relevant characteristic values joining over the following entities:
      "ausp: characteristic values
      "kssk: assignment of class to objects with change no and validity range
      "ksml: assignment of characteristics to class
      "klah: class header data
      SELECT DISTINCT ausp~objek, mara~matnr, ausp~datuv, klah~class, mara~lvorm
        FROM ausp INNER JOIN mara ON ausp~objek = mara~matnr
                  INNER JOIN kssk ON kssk~objek = mara~matnr AND kssk~mafid = 'O' AND kssk~klart = '001'
                  INNER JOIN ksml ON ausp~atinn = ksml~imerk AND ksml~lkenz = ''
                  INNER JOIN klah ON ksml~clint = klah~clint AND klah~clint = kssk~clint
        FOR ALL ENTRIES IN @lt_internal_matnr
        WHERE ausp~klart = '001'
          AND ausp~lkenz = ''
          AND mara~lvorm = ''
          AND ausp~objek = @lt_internal_matnr-objek
          AND klah~class IN @lt_relevant_clas_range
          AND mara~matkl IN @ms_parameters-mat_group
          AND mara~mtart IN @ms_parameters-mat_type
          AND mara~laeda IN @ms_parameters-changedate
          AND ( ( ausp~datuv >= @lv_valid_from AND ausp~datuv <= @lv_valid_to AND ausp~datub >= @lv_valid_to )
             OR ( ausp~datub >= @lv_valid_from AND ausp~datub <= @lv_valid_to AND ausp~datuv <= @lv_valid_from )
             OR ( ausp~datuv <= @lv_valid_from AND ausp~datub >= @lv_valid_to )
             OR ( ausp~datuv > @lv_valid_from AND ausp~datub < @lv_valid_to ) )
        INTO CORRESPONDING FIELDS OF TABLE @mt_mat_clas_params.

    ELSE.
      "Determine relevant characteristic values joining over the following entities:
      "ausp: characteristic values
      "kssk: assignment of class to objects with change no and validity range
      "ksml: assignment of characteristics to class
      "klah: class header data
      SELECT DISTINCT ausp~objek, mara~matnr, ausp~datuv, klah~class, mara~lvorm
        FROM ausp INNER JOIN mara ON ausp~objek = mara~matnr
                  INNER JOIN kssk ON kssk~objek = mara~matnr AND kssk~mafid = 'O' AND kssk~klart = '001'
                  INNER JOIN ksml ON ausp~atinn = ksml~imerk AND ksml~lkenz = ''
                  INNER JOIN klah ON ksml~clint = klah~clint AND klah~clint = kssk~clint
        WHERE ausp~klart = '001'
          AND ausp~lkenz = ''
          AND mara~lvorm = ''
          AND ausp~objek IN @ms_parameters-material
          AND klah~class IN @lt_relevant_clas_range
          AND mara~matkl IN @ms_parameters-mat_group
          AND mara~mtart IN @ms_parameters-mat_type
          AND mara~laeda IN @ms_parameters-changedate
          AND ( ( ausp~datuv >= @lv_valid_from AND ausp~datuv <= @lv_valid_to AND ausp~datub >= @lv_valid_to )
             OR ( ausp~datub >= @lv_valid_from AND ausp~datub <= @lv_valid_to AND ausp~datuv <= @lv_valid_from )
             OR ( ausp~datuv <= @lv_valid_from AND ausp~datub >= @lv_valid_to )
             OR ( ausp~datuv > @lv_valid_from AND ausp~datub < @lv_valid_to ) )
        INTO CORRESPONDING FIELDS OF TABLE @mt_mat_clas_params.

    ENDIF.

    SORT mt_mat_clas_params BY objek datuv.

  ENDMETHOD.
ENDCLASS.
