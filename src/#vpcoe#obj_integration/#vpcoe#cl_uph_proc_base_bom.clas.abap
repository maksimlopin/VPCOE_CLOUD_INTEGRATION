CLASS /vpcoe/cl_uph_proc_base_bom DEFINITION
  PUBLIC
  INHERITING FROM /vpcoe/cl_uph_proc_base
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /vpcoe/if_uph_entity_bom_proc .

    METHODS constructor .
    "! Setter injection for external modules wrapper
    METHODS set_ext_modules_wrapper
      IMPORTING
        !io_bom_expl_wrapper TYPE REF TO object .

    METHODS /vpcoe/if_uph_entity_proc~deserialize_selection_params
         REDEFINITION .
    METHODS /vpcoe/if_uph_entity_proc~init_processor
         REDEFINITION .
    METHODS /vpcoe/if_uph_entity_proc~prepare_process
         REDEFINITION .
    METHODS /vpcoe/if_uph_entity_proc~process_package
         REDEFINITION .
    METHODS /vpcoe/if_uph_entity_proc~get_parameters
         REDEFINITION .
protected section.

  data MS_PARAMETERS type /VPCOE/S_PCKG_BOM_INPUT .
  data MT_BOM_KEYS type /VPCOE/T_UPH_BOM_KEY .
  constants MC_CLSFN_BTCH_OBJECTTABLE type TABELLE value 'STPO' ##NO_TEXT.
  constants MC_CLSFN_BTCH_CLASSTYPE type KLASSENART value '023' ##NO_TEXT.

  methods DETERMINE_BOM_HEADER_BY_PARAMS
    exporting
      !ET_BOM_KEY type /VPCOE/T_UPH_BOM_KEY .
  methods DETERMINE_BOM_WHERE_USED
    importing
      !IT_BOM_KEY type /VPCOE/T_UPH_BOM_KEY
    changing
      !CT_BOM_KEY type /VPCOE/T_UPH_BOM_KEY .
  methods RETRIEVE_ITEM_CLASSIFICATION
    importing
      !IR_WRAP_BOM_ITEM type ref to /VPCOE/CL_UPH_WRAP_BOM_ITEM
      !IV_KEYDATE type DATUV .
private section.

  data MO_EXT_MODULES_WRAPPER type ref to LIF_EXT_MODULES_WRAPPER .
  data:
    mc_end_of_time_date TYPE c LENGTH 8 value '99991231' ##NO_TEXT.
ENDCLASS.



CLASS /VPCOE/CL_UPH_PROC_BASE_BOM IMPLEMENTATION.


  METHOD /vpcoe/if_uph_entity_bom_proc~map_bom_data.
    RETURN.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_bom_proc~retrieve_bom_data.

    TYPES:
      BEGIN OF lty_node_buffer,
        node_material TYPE matnr,
        items         TYPE REF TO /vpcoe/uph_wrap_bom_item,
      END OF lty_node_buffer,
      ltty_node_buffer TYPE HASHED TABLE OF lty_node_buffer WITH UNIQUE KEY node_material.

    DATA: lt_node_puffer    TYPE ltty_node_buffer,
          lt_exp_bom_items  TYPE TABLE OF stpox,
          lt_exp_bom_nodes  TYPE TABLE OF cscmat,
          ls_exp_bom_hdr    TYPE cstmat,
          lr_wrap_bom_items TYPE REF TO /vpcoe/uph_wrap_bom_item,
          lo_wrap_bom_item  TYPE REF TO /vpcoe/cl_uph_wrap_bom_item.

    LOOP AT it_bom_key REFERENCE INTO DATA(lr_bom_key).

      CLEAR: lt_node_puffer, ls_exp_bom_hdr, lt_exp_bom_items, lt_exp_bom_nodes,
             lr_wrap_bom_items, lo_wrap_bom_item.

      mo_ext_modules_wrapper->call_bom_explosion(
          EXPORTING
            ir_bom_key       = lr_bom_key
            is_parameters    = ms_parameters
            io_logger        = mo_logger
          IMPORTING
            es_exp_bom_hdr   = ls_exp_bom_hdr
            et_exp_bom_items = lt_exp_bom_items
            et_exp_bom_nodes = lt_exp_bom_nodes
      ).

      IF ls_exp_bom_hdr IS NOT INITIAL.

        ls_exp_bom_hdr-datuv = lr_bom_key->valfr.
        ls_exp_bom_hdr-datub = lr_bom_key->valto.

        mo_ext_modules_wrapper->get_material_detail(
           EXPORTING
             iv_material = ls_exp_bom_hdr-matnr
             iv_plant = ls_exp_bom_hdr-werks
             io_logger = mo_logger
           IMPORTING
             es_material_general = DATA(ls_material_general)
             es_material_plant = DATA(ls_material_plant)
             es_material_valuation = DATA(ls_material_valuation)
        ).

        lr_wrap_bom_items = NEW #( ).
        DATA(lo_wrap_bom_hdr) = NEW /vpcoe/cl_uph_wrap_bom_hdr(
              is_header_material   = ls_exp_bom_hdr
              ir_items              = lr_wrap_bom_items
              is_material_general   = ls_material_general
              is_material_plant     = ls_material_plant
              is_material_valuation = ls_material_valuation
        ).
        INSERT VALUE #( node_material = ls_exp_bom_hdr-matnr
                        items         = lr_wrap_bom_items ) INTO TABLE lt_node_puffer.

        INSERT lo_wrap_bom_hdr INTO TABLE rt_bom_data.

        " Loop the items from BOM explosion
        LOOP AT lt_exp_bom_items REFERENCE INTO DATA(lr_exp_bom_items).

          lo_wrap_bom_item = NEW /vpcoe/cl_uph_wrap_bom_item( is_bom_item_data = lr_exp_bom_items->* ).

          " Is item a node -> add items table
          READ TABLE lt_exp_bom_nodes WITH KEY matnr = lo_wrap_bom_item->get_material( ) TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            lr_wrap_bom_items = NEW #( ).
            lo_wrap_bom_item->set_items( lr_wrap_bom_items ).
            INSERT VALUE #( node_material = lo_wrap_bom_item->get_material( ) items = lr_wrap_bom_items ) INTO TABLE lt_node_puffer.
          ENDIF.

          " Lookup parent material
          READ TABLE lt_exp_bom_nodes INDEX lr_exp_bom_items->ttidx REFERENCE INTO DATA(lr_exp_bom_nodes).
          IF sy-subrc = 0.

            " Lookup parent item table for bom item
            READ TABLE lt_node_puffer WITH KEY node_material = lr_exp_bom_nodes->matnr REFERENCE INTO DATA(lr_parent_items).
            IF sy-subrc = 0.
              APPEND lo_wrap_bom_item TO lr_parent_items->items->*.
            ENDIF.
          ENDIF.

          "Retrieve item batch classification
          retrieve_item_classification( ir_wrap_bom_item = lo_wrap_bom_item iv_keydate = lr_bom_key->valfr ).

        ENDLOOP.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~deserialize_selection_params.
    CLEAR es_selection_params.

    DATA ls_selection_params TYPE /vpcoe/s_pckg_bom_input.

    /vpcoe/cl_plm_helper=>deserialize_json(
      EXPORTING
        iv_json   = iv_json_str
      CHANGING
        cs_data   = ls_selection_params ).

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

    "This method is used to determine the relevant BOMs to retrieve according the current
    "upload mode and the given parameters for the upload entity

    DATA : lv_params_string         TYPE string,
           ls_input_params_frm_prot TYPE /vpcoe/s_pckg_bom_input,
           lt_messages              TYPE /vpcoe/t_uph_msg,
           lt_bom_keys_tmp          TYPE /vpcoe/t_uph_bom_key,
           lr_protocol_delta        TYPE REF TO /vpcoe/uph_prot,
           lr_protocol_selection    TYPE REF TO /vpcoe/uph_prot.

    CLEAR: mt_bom_keys, mv_prepared.

    " When the Delta load then get the last successful full load selection screen parameters from Protocol
    IF mv_upload_mode EQ /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta .

      "retrieve last protocol entries
      DATA(lt_protocol)  =  /vpcoe/if_uph_entity_proc~read_from_protocol(
        iv_upload_entity = mv_upload_entity
        iv_only_success  = abap_true
        ).

      SORT lt_protocol BY start_timestamp DESCENDING.

      "get last successful protocol entry, might be a full or delta load: relevant for delta timestamp
      READ TABLE lt_protocol INDEX 1 REFERENCE INTO lr_protocol_delta.

      "get last successful full load protocol: relevant for selection
      READ TABLE lt_protocol REFERENCE INTO lr_protocol_selection WITH KEY upload_mode = /vpcoe/if_uph_entity_proc~gc_upload_mode-gc_upload_mode_full.

      IF lr_protocol_selection IS INITIAL.

        MESSAGE s053(/vpcoe/common).
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
        /vpcoe/if_uph_entity_proc~deserialize_selection_params( EXPORTING iv_json_str = lv_params_string IMPORTING es_selection_params = ls_input_params_frm_prot ).
        ms_parameters = ls_input_params_frm_prot.
      ENDIF.

    ENDIF.

    "Retrieve with filter criteria:
    determine_bom_header_by_params( IMPORTING et_bom_key = DATA(lt_bom_keys)  ).

    "finally fill key table
    mt_bom_keys = lt_bom_keys.

    "filter according upload mode
    CASE mv_upload_mode.
      WHEN /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full.
        "all specifications according the parameters are relevant

      WHEN /vpcoe/if_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta.
        "retrieve all BOMs which were changed since the last delta/full run for the current upload entity
        IF mt_bom_keys IS NOT INITIAL.

          " get the records which changed after the successful full load
          CLEAR lt_bom_keys_tmp.

          LOOP AT lt_bom_keys INTO DATA(ls_bom_keys)
             WHERE upddats GE lr_protocol_delta->start_timestamp.

            INSERT ls_bom_keys INTO TABLE lt_bom_keys_tmp.
          ENDLOOP.

          mt_bom_keys = lt_bom_keys_tmp.

          "reverse lookup of all usages of lt_bom_keys_tmp and filter according parameters
          DATA(lt_bom_used_keys) = VALUE /vpcoe/t_uph_bom_key( ).

          "reverse lookup of all usages of lt_bom_keys_tmp and filter according parameters
          determine_bom_where_used(
            EXPORTING
              it_bom_key = lt_bom_keys_tmp
            CHANGING
              ct_bom_key = lt_bom_used_keys
          ).

          "filter usage results against parameters
          IF ms_parameters-material IS NOT INITIAL.
            DELETE lt_bom_used_keys WHERE material NOT IN ms_parameters-material.
          ENDIF.
          IF ms_parameters-mat_type IS NOT INITIAL.
            DELETE lt_bom_used_keys WHERE mat_type NOT IN ms_parameters-mat_type.
          ENDIF.
          IF ms_parameters-bom_usage IS NOT INITIAL.
            DELETE lt_bom_used_keys WHERE stlan NOT IN ms_parameters-bom_usage.
          ENDIF.
          IF ms_parameters-bom_alter IS NOT INITIAL.
            DELETE lt_bom_used_keys WHERE stlal NOT IN ms_parameters-bom_alter.
          ENDIF.
          IF ms_parameters-bom_status IS NOT INITIAL.
            DELETE lt_bom_used_keys WHERE stlst <> ms_parameters-bom_status.
          ENDIF.

          INSERT LINES OF lt_bom_used_keys INTO TABLE mt_bom_keys.

          SORT mt_bom_keys BY stlnr stlal stlan bom_versn valfr valto.
          DELETE ADJACENT DUPLICATES FROM mt_bom_keys COMPARING stlnr stlal stlan bom_versn valfr valto.

        ENDIF.

    ENDCASE.

    "return record count and set as prepared
    rv_record_cnt = lines( mt_bom_keys ).

    mv_prepared = abap_true.

    "log record count
    MESSAGE s054(/vpcoe/common) WITH rv_record_cnt INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    mo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 ) ) ).

  ENDMETHOD.


  METHOD /vpcoe/if_uph_entity_proc~process_package.
    DATA: lv_top     TYPE i,
          lt_bom_key TYPE /vpcoe/t_uph_bom_key.

    "check if preparation did already take place
    IF mv_prepared = abap_false.
      /vpcoe/if_uph_entity_proc~prepare_process( ).
    ENDIF.

    "collect relevant keys for the current package
    lv_top = ( iv_act_package - 1 ) * iv_package_size + 1.

    DO iv_package_size TIMES.
      IF lv_top <= lines( mt_bom_keys ) .

        READ TABLE mt_bom_keys INDEX lv_top REFERENCE INTO DATA(lr_bom_keys).
        APPEND INITIAL LINE TO lt_bom_key REFERENCE INTO DATA(lr_bom_key).
        MOVE-CORRESPONDING lr_bom_keys->* TO lr_bom_key->*.

        ADD 1 TO lv_top.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    "retrieve and map specification data
    DATA(lt_entity_data) = /vpcoe/if_uph_entity_bom_proc~map_bom_data( it_bom_data = /vpcoe/if_uph_entity_bom_proc~retrieve_bom_data( it_bom_key = lt_bom_key )  ).

    rt_entity_data = lt_entity_data.

    "log result
    DATA(lv_rec_cnt) = lines( lt_bom_key ).
    DATA(lv_entity_cnt) = lines( lt_entity_data ).
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

    MESSAGE s055(/vpcoe/common) WITH lv_entity_cnt lv_rec_cnt lv_upload_entity_dval-ddtext INTO /vpcoe/cl_rdp_log=>sv_msg_text.
    mo_logger->add_messages( it_messages = VALUE #( ( msgid = sy-msgid msgno = sy-msgno msgty = sy-msgty msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 ) ) ).
  ENDMETHOD.


  METHOD constructor.

    super->constructor(  ).

    mo_ext_modules_wrapper = NEW lcl_bom_expl_wrapper(  ).

  ENDMETHOD.


  METHOD determine_bom_header_by_params.

    TYPES:
      ltty_range_bom_status  TYPE RANGE OF stlst,
      ltty_range_valfromdate TYPE RANGE OF datuv.

    TYPES: BEGIN OF data_d,
             tims TYPE tims,
           END OF data_d.

    DATA: lv_valid_from  TYPE datuv,
          lv_mat_count   TYPE i VALUE `500`,
          lt_materials   TYPE STANDARD TABLE OF mara,
          lr_t_material  TYPE RANGE OF matnr,
          ls_bom_key     LIKE LINE OF et_bom_key,
          lt_valfromdate TYPE ltty_range_valfromdate,
          lt_bom_status  TYPE ltty_range_bom_status,
          lv_utc         TYPE tzonref-tzone VALUE 'UTC',
          lt_bom_key     TYPE SORTED TABLE OF /vpcoe/s_uph_bom_key WITH NON-UNIQUE KEY stlnr stlal stlan bom_versn valfrdats.

    CLEAR et_bom_key.

    lv_valid_from = ms_parameters-valfromdate.

    IF lv_valid_from IS INITIAL.
      "in case there is not from date -> init with current date
      lv_valid_from = sy-datum.
    ENDIF.

    IF ms_parameters-valfromdate IS NOT INITIAL.
      INSERT VALUE #( sign = 'I' option = 'LE' low = ms_parameters-valfromdate ) INTO TABLE lt_valfromdate.
    ENDIF.

    IF ms_parameters-bom_status IS NOT INITIAL.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = ms_parameters-bom_status ) INTO TABLE lt_bom_status.
    ENDIF.

* In case of huge amount of Materials in Selection Parameter replace the logic to "For All Entries"
    IF lines( ms_parameters-material ) < lv_mat_count.
      SELECT DISTINCT stko~stlnr,
                      stko~stlal,
                      stzu~stlan,
                      stko~wrkan,
                      mast~matnr AS material,
                      mara~mtart AS mat_type,
                      mast~werks AS plant,
                      stko~datuv AS valfr,
                      stko~valid_to AS valto,
                      stpo~datuv AS posvalfr,
                      stpo~valid_to AS posvalto,
                      stko~aennr,
                      stpo~aennr AS posaennr,
                      stko~stlst AS stlst,
                      stzu~stldt,
                      stzu~stltm,
                      @lv_valid_from AS valfrdats,
                      stas~lkenz,
                      stas~andat
        FROM stko INNER JOIN stzu ON stko~stlty = stzu~stlty
                                 AND stko~stlnr = stzu~stlnr
                                 AND stko~stlty = @/vpcoe/if_uph_entity_bom_proc=>gc_stlty_mat_bom
                  LEFT OUTER JOIN mast ON mast~stlnr = stko~stlnr
                                      AND mast~stlal = stko~stlal
                                      AND stzu~stlan = mast~stlan
                  INNER JOIN mara ON mast~matnr = mara~matnr and mara~lvorm = ''
                  LEFT OUTER JOIN stpo ON stpo~stlnr = stko~stlnr
                                      AND stpo~stlty = @/vpcoe/if_uph_entity_bom_proc=>gc_stlty_mat_bom AND stpo~lkenz = ''
                  INNER JOIN stas ON stpo~stlty = stas~stlty
                                 AND stpo~stlnr = stas~stlnr
                                 AND stpo~stlkn = stas~stlkn
                                 AND stas~stlal = stko~stlal
                                 AND stas~lkenz = ''
        WHERE  mast~matnr IN @ms_parameters-material "bom material
           AND mast~werks IN @ms_parameters-plant "plant
           AND stko~datuv IN @lt_valfromdate  "header valid from
           AND stpo~datuv IN @lt_valfromdate "bom item valid from
           AND stzu~stldt IN @ms_parameters-bom_chgdon "last change
           AND mast~stlan IN @ms_parameters-bom_usage "bom usage
           AND mast~stlal IN @ms_parameters-bom_alter "bom alternative
           AND mara~mtart IN @ms_parameters-mat_type "material type
           AND stko~stlst IN @lt_bom_status "bom status
           AND stko~lkenz = '' " bom deleted
        ORDER BY stko~stlnr, stko~stlal, stzu~stlan, stko~wrkan, valfr
        INTO TABLE @DATA(lt_bom_key_tmp).
    ELSE.

      LOOP AT ms_parameters-material INTO DATA(lr_material) GROUP BY ( sy-tabix - 1 ) DIV lv_mat_count + 1.
        LOOP AT GROUP lr_material ASSIGNING FIELD-SYMBOL(<lr_material>).
          lr_t_material = VALUE #( BASE lr_t_material ( <lr_material> ) ).
        ENDLOOP.

        SELECT DISTINCT matnr
          FROM mast
          APPENDING CORRESPONDING FIELDS OF TABLE lt_materials
           WHERE matnr IN lr_t_material.

      ENDLOOP.

      IF lt_materials IS NOT INITIAL.
        SELECT DISTINCT stko~stlnr,
                        stko~stlal,
                        stzu~stlan,
                        stko~wrkan,
                        mast~matnr AS material,
                        mara~mtart AS mat_type,
                        mast~werks AS plant,
                        stko~datuv AS valfr,
                        stko~valid_to AS valto,
                        stpo~datuv AS posvalfr,
                        stpo~valid_to AS posvalto,
                        stko~aennr,
                        stpo~aennr AS posaennr,
                        stko~stlst AS stlst,
                        stzu~stldt,
                        stzu~stltm,
*                        @lv_valid_from AS valfrdats,
                        stas~lkenz,
                        stas~andat
          FROM stko INNER JOIN stzu ON stko~stlty = stzu~stlty
                                   AND stko~stlnr = stzu~stlnr
                                   AND stko~stlty = @/vpcoe/if_uph_entity_bom_proc=>gc_stlty_mat_bom
                    LEFT OUTER JOIN mast ON mast~stlnr = stko~stlnr
                                        AND mast~stlal = stko~stlal
                                        AND stzu~stlan = mast~stlan
                    INNER JOIN mara ON mast~matnr = mara~matnr and mara~lvorm = ''
                    LEFT OUTER JOIN stpo ON stpo~stlnr = stko~stlnr
                                        AND stpo~stlty = @/vpcoe/if_uph_entity_bom_proc=>gc_stlty_mat_bom AND stpo~lkenz = ''
                    INNER JOIN stas ON stpo~stlty = stas~stlty
                                   AND stpo~stlnr = stas~stlnr
                                   AND stpo~stlkn = stas~stlkn
                                   AND stas~stlal = stko~stlal
                                   AND stas~lkenz = ''
          FOR ALL ENTRIES IN @lt_materials
          WHERE  mast~matnr = @lt_materials-matnr "bom material
             AND mast~werks IN @ms_parameters-plant "plant
             AND stko~datuv IN @lt_valfromdate  "header valid from
             AND stpo~datuv IN @lt_valfromdate "bom item valid from
             AND stzu~stldt IN @ms_parameters-bom_chgdon "last change
             AND mast~stlan IN @ms_parameters-bom_usage "bom usage
             AND mast~stlal IN @ms_parameters-bom_alter "bom alternative
             AND mara~mtart IN @ms_parameters-mat_type "material type
             AND stko~stlst IN @lt_bom_status "bom status
             AND stko~lkenz = '' " bom deleted
          INTO CORRESPONDING FIELDS OF TABLE @lt_bom_key_tmp.

        SORT lt_bom_key_tmp BY stlnr stlal stlan wrkan valfr.
      ENDIF.
    ENDIF.

    LOOP AT lt_bom_key_tmp ASSIGNING FIELD-SYMBOL(<ls_bom_key_tmp>).

      <ls_bom_key_tmp>-valfrdats = lv_valid_from. " since it can be done with for all entries in select above

      CONVERT DATE <ls_bom_key_tmp>-stldt TIME <ls_bom_key_tmp>-stltm DAYLIGHT SAVING TIME 'X'
        INTO TIME STAMP DATA(lv_time_stamp) TIME ZONE lv_utc.

      CLEAR ls_bom_key.

      ls_bom_key-material  = <ls_bom_key_tmp>-material.
      ls_bom_key-mat_type  = <ls_bom_key_tmp>-mat_type.
      ls_bom_key-plant     = <ls_bom_key_tmp>-plant.
      ls_bom_key-stlnr     = <ls_bom_key_tmp>-stlnr.
      ls_bom_key-stlal     = <ls_bom_key_tmp>-stlal.
      ls_bom_key-stlan     = <ls_bom_key_tmp>-stlan.
      ls_bom_key-bom_versn = <ls_bom_key_tmp>-wrkan.
      ls_bom_key-stlst     = <ls_bom_key_tmp>-stlst.
      ls_bom_key-upddats   = lv_time_stamp.

      IF NOT line_exists( lt_bom_key[ stlnr     = <ls_bom_key_tmp>-stlnr
                                      plant     = <ls_bom_key_tmp>-plant
                                      valfrdats = <ls_bom_key_tmp>-valfr
                                      stlal     = <ls_bom_key_tmp>-stlal
                                      stlan     = <ls_bom_key_tmp>-stlan
                                      bom_versn = <ls_bom_key_tmp>-wrkan ] ).
        ls_bom_key-valfr     = <ls_bom_key_tmp>-valfr.
        ls_bom_key-valto     = <ls_bom_key_tmp>-valto.
        ls_bom_key-valfrdats = <ls_bom_key_tmp>-valfr.
        ls_bom_key-aennr     = <ls_bom_key_tmp>-aennr.
        INSERT ls_bom_key INTO TABLE lt_bom_key.
      ENDIF.

      " item valid from
      IF <ls_bom_key_tmp>-posvalfr IS NOT INITIAL.
        IF NOT line_exists( lt_bom_key[ stlnr     = <ls_bom_key_tmp>-stlnr
                                        plant     = <ls_bom_key_tmp>-plant
                                        valfrdats = <ls_bom_key_tmp>-posvalfr
                                        stlal     = <ls_bom_key_tmp>-stlal
                                        stlan     = <ls_bom_key_tmp>-stlan
                                        bom_versn = <ls_bom_key_tmp>-wrkan ] ).
          ls_bom_key-valfr     = <ls_bom_key_tmp>-posvalfr.
          ls_bom_key-valto     = <ls_bom_key_tmp>-posvalto.
          ls_bom_key-valfrdats = <ls_bom_key_tmp>-posvalfr.
          ls_bom_key-aennr     = <ls_bom_key_tmp>-posaennr.
          INSERT ls_bom_key INTO TABLE lt_bom_key.
        ENDIF.
      ENDIF.

      " item valid to
      IF <ls_bom_key_tmp>-posvalto <> mc_end_of_time_date AND <ls_bom_key_tmp>-posvalto IS NOT INITIAL.
        IF NOT line_exists( lt_bom_key[ stlnr     = <ls_bom_key_tmp>-stlnr
                                        plant     = <ls_bom_key_tmp>-plant
                                        valfrdats = <ls_bom_key_tmp>-posvalto
                                        stlal     = <ls_bom_key_tmp>-stlal
                                        stlan     = <ls_bom_key_tmp>-stlan
                                        bom_versn = <ls_bom_key_tmp>-wrkan ] ).
          ls_bom_key-valfr     = <ls_bom_key_tmp>-posvalto.
          ls_bom_key-valto     = mc_end_of_time_date.
          ls_bom_key-valfrdats = <ls_bom_key_tmp>-posvalto.
          ls_bom_key-aennr     = <ls_bom_key_tmp>-posaennr.
          INSERT ls_bom_key INTO TABLE lt_bom_key.
        ENDIF.
      ENDIF.

    ENDLOOP.

    " calculate bom valid to
    LOOP AT lt_bom_key REFERENCE INTO DATA(lr_bom_key).
      " find next valid from
      READ TABLE lt_bom_key INTO DATA(ls_next_entry) INDEX sy-tabix + 1.

      IF sy-subrc = 0 AND ls_next_entry-stlnr = lr_bom_key->stlnr AND ls_next_entry-plant = lr_bom_key->plant
        AND ls_next_entry-stlal = lr_bom_key->stlal AND ls_next_entry-stlan = lr_bom_key->stlan AND ls_next_entry-bom_versn = lr_bom_key->bom_versn.
        lr_bom_key->valto = ls_next_entry-valfr - 1.
      ELSE.
        lr_bom_key->valto = mc_end_of_time_date.
      ENDIF.
    ENDLOOP.

    et_bom_key = lt_bom_key.
  ENDMETHOD.


  METHOD determine_bom_where_used.
    DATA lt_bom_key TYPE /vpcoe/t_uph_bom_key.
    DATA lt_tmp_matnr TYPE TABLE OF matnr.

    "add self references
    LOOP AT it_bom_key REFERENCE INTO DATA(lr_bom_key) WHERE material IS NOT INITIAL AND plant IS NOT INITIAL.
      IF NOT line_exists( ct_bom_key[ stlnr     = lr_bom_key->stlnr
                                      stlal     = lr_bom_key->stlal
                                      bom_versn = lr_bom_key->bom_versn
                                      stlan     = lr_bom_key->stlan
                                      valfr     = lr_bom_key->valfr
                                      valto     = lr_bom_key->valto ] ).
        INSERT lr_bom_key->* INTO TABLE ct_bom_key.
      ENDIF.

    ENDLOOP.

    LOOP AT it_bom_key REFERENCE INTO lr_bom_key WHERE material IS NOT INITIAL AND plant IS NOT INITIAL..

      mo_ext_modules_wrapper->call_bom_where_used(
        EXPORTING
          iv_valfr     = lr_bom_key->valfr
          iv_valto     = lr_bom_key->valto
          iv_material  = lr_bom_key->material
          iv_plant     = lr_bom_key->plant
          iv_stlan     = lr_bom_key->stlan
        IMPORTING
          et_stpo_used = DATA(lt_stpo_used) ).

      LOOP AT lt_stpo_used REFERENCE INTO DATA(lr_stpo_used) WHERE loekz IS INITIAL AND matnr IS NOT INITIAL.
        APPEND lr_stpo_used->matnr TO lt_tmp_matnr.
      ENDLOOP.

      IF lt_tmp_matnr IS NOT INITIAL.

        SELECT matnr, mtart
          FROM mara
          FOR ALL ENTRIES IN @lt_tmp_matnr
                     WHERE matnr = @lt_tmp_matnr-table_line
          INTO TABLE @DATA(lt_mara_type).

        SORT lt_mara_type BY table_line.
      ENDIF.

      LOOP AT lt_stpo_used REFERENCE INTO lr_stpo_used WHERE loekz IS INITIAL
                                                         AND matnr IS NOT INITIAL
                                                         AND werks IS NOT INITIAL.

        IF NOT line_exists( ct_bom_key[ stlnr     = lr_stpo_used->stlnr
                                        stlal     = COND #( WHEN lr_stpo_used->vwalt IS INITIAL THEN '01' ELSE lr_stpo_used->vwalt )
                                        bom_versn = lr_stpo_used->stpoz
                                        stlan     = lr_stpo_used->stlan
                                        valfr     = lr_stpo_used->datuv
                                        valto = lr_stpo_used->datub ] ).

          DATA(ls_bom_key) = VALUE /vpcoe/s_uph_bom_key(
                            material = lr_stpo_used->matnr
                            mat_type = lt_mara_type[ matnr = lr_stpo_used->matnr ]-mtart
                            plant    = lr_stpo_used->werks
                            stlnr    = lr_stpo_used->stlnr
                            stlal    = COND #( WHEN lr_stpo_used->vwalt IS INITIAL THEN '01' ELSE lr_stpo_used->vwalt )
                            stlan     = lr_stpo_used->stlan
                            bom_versn = lr_stpo_used->stpoz
                            stlst     = lr_stpo_used->stlst
                            valfr     = lr_stpo_used->datuv
                            valto     = lr_stpo_used->datub
                            aennr     = lr_stpo_used->aennr
                            upddats   = COND #( WHEN lr_stpo_used->aedat IS INITIAL THEN lr_stpo_used->andat ELSE lr_stpo_used->aedat )
                            valfrdats = lr_stpo_used->datuv ).

          INSERT ls_bom_key INTO TABLE ct_bom_key.

          determine_bom_where_used(
            EXPORTING
              it_bom_key = VALUE #( ( ls_bom_key ) )
            CHANGING
              ct_bom_key = ct_bom_key ).

        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD retrieve_item_classification.
    DATA:
      lv_classnum     TYPE klasse_d,
      lt_mcl_versions TYPE /vpcoe/uph_wrap_mcl_version.

    DATA(ls_exp_bom_item) = ir_wrap_bom_item->get_internal_data(  ).

    IF ls_exp_bom_item-clszu IS NOT INITIAL.

      DATA(lv_object_key_long) = VALUE cuobn90( ).
      lv_object_key_long = |{ ls_exp_bom_item-bmtyp }{ ls_exp_bom_item-stlnr }{ ls_exp_bom_item-clszu }| .

      mo_ext_modules_wrapper->call_clsfn_get_classes(
        EXPORTING
          iv_objectkey_long = lv_object_key_long
          iv_objecttable    = mc_clsfn_btch_objecttable
          iv_keydate        = iv_keydate
          iv_classtype      = mc_clsfn_btch_classtype
          io_logger         = mo_logger
        IMPORTING
          et_alloc_list     = DATA(lt_alloc_list)
      ).

      IF lt_alloc_list IS NOT INITIAL.

        CLEAR lt_mcl_versions.

        LOOP AT lt_alloc_list INTO DATA(ls_alloc_list).

          lv_classnum = ls_alloc_list-classnum.

          mo_ext_modules_wrapper->call_clsfn_get_detail(
            EXPORTING
              iv_objectkey_long = lv_object_key_long
              iv_objecttable    = mc_clsfn_btch_objecttable
              iv_keydate        = iv_keydate
              iv_classnum       = lv_classnum
              iv_classtype      = mc_clsfn_btch_classtype
              io_logger         = mo_logger
            IMPORTING
              et_values_num     = DATA(lt_number_value)
              et_values_char    = DATA(lt_character_value)
              et_values_curr    = DATA(lt_currency_value)
          ).

          APPEND INITIAL LINE TO lt_mcl_versions REFERENCE INTO DATA(lo_wrap_mcl_version).

          lo_wrap_mcl_version->* = NEW /vpcoe/cl_uph_wrap_mcl_version( is_mcl_header = VALUE #(
                                                                     objek = lv_object_key_long matnr = ls_exp_bom_item-idnrk
                                                                     datuv = iv_keydate class = lv_classnum
                                                                  )
                                                                  it_mcl_character_data = lt_character_value
                                                                  it_mcl_currency_data = lt_currency_value
                                                                  it_mcl_number_data = lt_number_value ).


        ENDLOOP.

        DATA(lr_batch_clfn) = NEW /vpcoe/cl_uph_wrap_mcl(
          iv_objek        = CONV #( lv_object_key_long )
          iv_matnr        = ls_exp_bom_item-idnrk
          it_mcl_versions = lt_mcl_versions
        ).

        ir_wrap_bom_item->set_batch_clfn_data( ir_batch_clfn = lr_batch_clfn ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD set_ext_modules_wrapper.
* Setter injector: Set wrapper for BOM explosion FM call - only used by tests
    IF io_bom_expl_wrapper IS BOUND.

      mo_ext_modules_wrapper ?= io_bom_expl_wrapper.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
