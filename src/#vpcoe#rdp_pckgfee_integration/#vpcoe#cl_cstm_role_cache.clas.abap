class /VPCOE/CL_CSTM_ROLE_CACHE definition
  public
  create public .

public section.

  interfaces /VPCOE/IF_PCKF_CACHE .

  methods CONSTRUCTOR .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_gzip_util TYPE REF TO object.

    METHODS get_gzip_util
      RETURNING VALUE(ro_gzip_util) TYPE REF TO object.

ENDCLASS.



CLASS /VPCOE/CL_CSTM_ROLE_CACHE IMPLEMENTATION.


  METHOD /VPCOE/IF_PCKF_CACHE~CLEAR.

    CHECK iv_entity_type IS NOT INITIAL.

    IF iv_ref_prot_uuid IS SUPPLIED AND iv_ref_prot_uuid IS NOT INITIAL.
      DELETE FROM /vpcoe/cstm_ench WHERE entity_type = @iv_entity_type AND ref_prot_uuid = @iv_ref_prot_uuid.
    ELSE.
      DELETE FROM /vpcoe/cstm_ench WHERE entity_type = @iv_entity_type.
    ENDIF.

  ENDMETHOD.


  METHOD /VPCOE/IF_PCKF_CACHE~DELETE_ENTITY.

    CHECK iv_uuid IS NOT INITIAL.

    DELETE FROM /vpcoe/cstm_ench WHERE uuid = @iv_uuid.

  ENDMETHOD.


  METHOD /VPCOE/IF_PCKF_CACHE~GET_ENTITIES.

    DATA: lt_rep_config_id TYPE RANGE OF /vpcoe/pckf_report_config_id,
          lt_ref_prot_uuid TYPE RANGE OF /vpcoe/uph_prot_uuid,
          lt_failed        TYPE RANGE OF /vpcoe/pckf_failed,
          lt_entities      TYPE /vpcoe/t_pckf_entity_data,
          lt_cache_entries TYPE STANDARD TABLE OF /vpcoe/pckf_ench,
          lv_json_raw      TYPE xstring.

    CHECK iv_entity_type IS NOT INITIAL.

    IF iv_report_config_id IS SUPPLIED AND iv_report_config_id IS NOT INITIAL.

      APPEND INITIAL LINE TO lt_rep_config_id REFERENCE INTO DATA(lr_rep_config_id).
      lr_rep_config_id->sign = 'I'.
      lr_rep_config_id->option = 'EQ'.
      lr_rep_config_id->low = iv_report_config_id.

    ENDIF.

    IF iv_ref_prot_uuid IS SUPPLIED AND iv_ref_prot_uuid IS NOT INITIAL.

      APPEND INITIAL LINE TO lt_ref_prot_uuid REFERENCE INTO DATA(lr_ref_prot_uuid).
      lr_ref_prot_uuid->sign = 'I'.
      lr_ref_prot_uuid->option = 'EQ'.
      lr_ref_prot_uuid->low = iv_ref_prot_uuid.

    ENDIF.

    IF iv_failed_only IS SUPPLIED AND iv_failed_only = abap_true.

      APPEND INITIAL LINE TO lt_failed REFERENCE INTO DATA(lr_failed).
      lr_failed->sign = 'I'.
      lr_failed->option = 'EQ'.
      lr_failed->low = iv_failed_only.

    ENDIF.

    "retrieve data from cache table, FIFO order
    SELECT uuid, entity_type, reportconfigid, jsondata, failed, creationdatetime FROM /vpcoe/cstm_ench
      WHERE entity_type = @iv_entity_type
            AND reportconfigid IN @lt_rep_config_id
            AND ref_prot_uuid IN @lt_ref_prot_uuid
            AND failed IN @lt_failed
       ORDER BY creationdatetime ASCENDING
       INTO CORRESPONDING FIELDS OF TABLE @lt_cache_entries.

    LOOP AT lt_cache_entries REFERENCE INTO DATA(lr_cache_entry).

      IF mo_gzip_util IS BOUND.

        CALL METHOD mo_gzip_util->('DECOMPRESS_BINARY_WITH_HEADER')
          EXPORTING
            gzip_in = lr_cache_entry->jsondata
          IMPORTING
            raw_out = lv_json_raw.
      ELSE.
        lv_json_raw = lr_cache_entry->jsondata.
      ENDIF.

      DATA(lv_json_str) = NEW cl_abap_codepage( )->convert_from( source = lv_json_raw codepage = 'UTF-8' ).

      CASE lr_cache_entry->entity_type.
        WHEN /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_role.

          /vpcoe/cl_pckf_ent_cstm_role=>/vpcoe/if_pckf_entity_data~json_to_entity(
            EXPORTING
              iv_json = lv_json_str
            IMPORTING
              et_entity_data = lt_entities
           ).

      ENDCASE.

      "write back uuid reference and add to result
      IF lt_entities IS NOT INITIAL AND lines( lt_entities ) = 1.

        DATA(lr_entity) = lt_entities[ 1 ].

        lr_entity->set_uuid( lr_cache_entry->uuid ).
        lr_entity->set_failed( lr_cache_entry->failed ).

        APPEND lr_entity TO rv_result.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD /VPCOE/IF_PCKF_CACHE~GET_REFERENCES.

    DATA: lt_references TYPE /vpcoe/t_pckf_cache_reference.

    CHECK iv_entity_type IS NOT INITIAL.

    "retrieve references from cache
    SELECT entity_type, ref_prot_uuid, COUNT( * ) AS rec_cnt FROM /vpcoe/cstm_ench
      WHERE entity_type = @iv_entity_type
       GROUP BY entity_type, ref_prot_uuid
       INTO CORRESPONDING FIELDS OF TABLE @lt_references.

    "order references by creation date ascending (FIFO order)
    LOOP AT lt_references REFERENCE INTO DATA(lr_reference).

      SELECT SINGLE MIN( creationdatetime ) FROM /vpcoe/cstm_ench WHERE entity_type = @lr_reference->entity_type AND ref_prot_uuid = @lr_reference->ref_prot_uuid INTO @lr_reference->creationdatetime.
      SELECT COUNT( * ) FROM /vpcoe/cstm_ench WHERE entity_type = @lr_reference->entity_type AND ref_prot_uuid = @lr_reference->ref_prot_uuid AND failed = @abap_true INTO @lr_reference->failed_cnt.

    ENDLOOP.

    SORT lt_references BY creationdatetime ASCENDING.

    rt_result = lt_references.

  ENDMETHOD.


  METHOD /VPCOE/IF_PCKF_CACHE~SET_ENTITIES.

    DATA: lt_update_tab TYPE STANDARD TABLE OF /vpcoe/cstm_ench,
          lt_insert_tab TYPE STANDARD TABLE OF /vpcoe/cstm_ench,
          ls_row_exists TYPE /vpcoe/cstm_ench,
          lv_json_gzip  TYPE xstring.

    LOOP AT it_entities INTO DATA(lr_entity).

      DATA(lv_act_uuid) = lr_entity->get_uuid( ).

      SELECT SINGLE FOR UPDATE uuid, failed, creationdatetime FROM /vpcoe/cstm_ench WHERE uuid = @lv_act_uuid INTO CORRESPONDING FIELDS OF @ls_row_exists.

      IF sy-subrc = 0.

        APPEND INITIAL LINE TO lt_update_tab REFERENCE INTO DATA(lr_cu_row).
        MOVE-CORRESPONDING ls_row_exists TO lr_cu_row->*.

      ELSE.

        APPEND INITIAL LINE TO lt_insert_tab REFERENCE INTO lr_cu_row.
        lr_cu_row->uuid = lr_entity->get_uuid( ).
        lr_cu_row->entity_type = lr_entity->get_entity_type( ).
        GET TIME STAMP FIELD lr_cu_row->creationdatetime.

      ENDIF.

      DATA(lv_json_raw) = NEW cl_abap_codepage( )->convert_to( source = lr_entity->entity_to_json( ) codepage = 'UTF-8' ).

      IF mo_gzip_util IS BOUND.

        CALL METHOD mo_gzip_util->('COMPRESS_BINARY_WITH_HEADER')
          EXPORTING
            raw_in   = lv_json_raw
          IMPORTING
            gzip_out = lv_json_gzip.

        lr_cu_row->jsondata = lv_json_gzip.
      ELSE.
        lr_cu_row->jsondata = lv_json_raw.
      ENDIF.

      lr_cu_row->reportconfigid = lr_entity->get_report_config_id( ).

      IF iv_ref_prot_uuid IS SUPPLIED.
        lr_cu_row->ref_prot_uuid = iv_ref_prot_uuid.
      ENDIF.

      GET TIME STAMP FIELD lr_cu_row->lastchangedatetime.

    ENDLOOP.

    IF lt_update_tab IS NOT INITIAL.

      UPDATE /vpcoe/cstm_ench FROM TABLE lt_update_tab.

    ENDIF.

    IF lt_insert_tab IS NOT INITIAL.

      INSERT /vpcoe/cstm_ench FROM TABLE lt_insert_tab.
    ENDIF.

  ENDMETHOD.


  METHOD /VPCOE/IF_PCKF_CACHE~SET_FAILED.

    CHECK iv_uuid IS NOT INITIAL.

    UPDATE /vpcoe/cstm_ench SET failed = @abap_true WHERE uuid = @iv_uuid.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    IF mo_gzip_util IS NOT BOUND.
      mo_gzip_util = get_gzip_util( ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_GZIP_UTIL.

    CLEAR: ro_gzip_util.

    SELECT SINGLE *
      INTO @DATA(ls_sap_basis)
        FROM cvers
          WHERE component = 'SAP_BASIS'.

    IF sy-subrc = 0.
      IF   ls_sap_basis-release = 700 AND ls_sap_basis-extrelease >= 35 OR ls_sap_basis-release = 701 AND ls_sap_basis-extrelease >= 20 OR ls_sap_basis-release = 702 AND ls_sap_basis-extrelease >= 20
        OR ls_sap_basis-release = 710 AND ls_sap_basis-extrelease >= 22 OR ls_sap_basis-release = 711 AND ls_sap_basis-extrelease >= 17 OR ls_sap_basis-release = 730 AND ls_sap_basis-extrelease >= 18
        OR ls_sap_basis-release = 731 AND ls_sap_basis-extrelease >= 21 OR ls_sap_basis-release = 740 AND ls_sap_basis-extrelease >= 18 OR ls_sap_basis-release = 750 AND ls_sap_basis-extrelease >= 09
        OR ls_sap_basis-release = 751 AND ls_sap_basis-extrelease >= 04.
        ro_gzip_util = NEW cl_abap_gzip( ).
      ELSE.
        ro_gzip_util = NEW /vpcoe/cl_abap_gzip( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
