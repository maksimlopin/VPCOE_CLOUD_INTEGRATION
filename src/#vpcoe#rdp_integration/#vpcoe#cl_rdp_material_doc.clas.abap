CLASS /vpcoe/cl_rdp_material_doc DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      gty_t_movement_types TYPE RANGE OF /vpcoe/movement_type-type .
    TYPES:
      BEGIN OF gty_s_sel_opt,
        country       TYPE RANGE OF land1,
        post_date     TYPE RANGE OF mseg-budat_mkpf,
        mtrl_doc_year TYPE RANGE OF mseg-mjahr,
        chng_date     TYPE RANGE OF mkpf-aedat,
      END OF gty_s_sel_opt .
    TYPES:
      BEGIN OF gty_s_db_json,
        api_type           TYPE /vpcoe/jsn_cloud-api_type,
        session_id         TYPE /vpcoe/jsn_cloud-session_id,
        session_item       TYPE /vpcoe/jsn_cloud-session_item,
        failed             TYPE /vpcoe/jsn_cloud-failed,
        lines_count        TYPE /vpcoe/jsn_cloud-lines_count,
        processing_started TYPE xfeld,
        processing_ended   TYPE xfeld,
        do_wait            TYPE xfeld,
        task_id            TYPE text40,
        response_status    TYPE /vpcoe/jsn_cloud-response_status,
        response_message   TYPE /vpcoe/jsn_cloud-response_message,
      END OF gty_s_db_json .
    TYPES:
      gty_t_cpi TYPE STANDARD TABLE OF bdicpident .
    TYPES:
      gty_r_country TYPE RANGE OF land1 .                          "Country
    TYPES:
      gty_r_pst_date TYPE RANGE OF mseg-budat_mkpf .                          "Posting Date
    TYPES:
      gty_r_mtrl_doc_year TYPE RANGE OF mseg-mjahr .                          "Material Document Year
    TYPES:
      gty_r_chng_date TYPE RANGE OF mkpf-aedat .                          "Changed on
    TYPES:
      BEGIN OF gty_s_texts,
        code        TYPE char10,
        name        TYPE char255,
        description TYPE string,
      END OF gty_s_texts .
    TYPES:
      gty_t_texts TYPE SORTED TABLE OF gty_s_texts WITH NON-UNIQUE KEY code .
    TYPES gty_s_material TYPE /vpcoe/str_material .
    TYPES:
      gty_t_material TYPE SORTED TABLE OF gty_s_material WITH NON-UNIQUE KEY id .
    TYPES gty_s_material_json TYPE /vpcoe/str_material_json .
    TYPES:
      BEGIN OF gty_s_material_data,
        supplying_or_issuing_plant   TYPE reswk,
        suppl_or_issuing_plant_cntr  TYPE land1,
        suppl_or_issuing_plant_regio TYPE regio,
        suppl_vendor                 TYPE ekpo-emlif,
        address2                     TYPE ekpo-adrn2,
        customer_po                  TYPE ekpo-kunnr,
        deb_cred_ind                 TYPE mseg-shkzg,
        ebelp                        TYPE ekpo-ebelp,
        text_id                      TYPE sgtxt.
            INCLUDE TYPE /vpcoe/str_material.
    TYPES: END OF gty_s_material_data .
    TYPES:
      gty_t_material_data TYPE SORTED TABLE OF gty_s_material_data WITH NON-UNIQUE KEY id .
    TYPES:
      gty_t_twlad TYPE STANDARD TABLE OF twlad .
    TYPES gty_t_adrc TYPE /vpcoe/t_adrc .

    CONSTANTS:
      BEGIN OF sc_variants,
        gr_prod   TYPE /vpcoe/de_mat_doc_variants VALUE 'GR_PROD',
        gi_prod   TYPE /vpcoe/de_mat_doc_variants VALUE 'GI_PROD',
        gr_st_imp TYPE /vpcoe/de_mat_doc_variants VALUE 'GR_ST_IMP',
        gi_st_exp TYPE /vpcoe/de_mat_doc_variants VALUE 'GI_ST_EXP',
        gr_po_imp TYPE /vpcoe/de_mat_doc_variants VALUE 'GR_PO_IMP',
        gr_po     TYPE /vpcoe/de_mat_doc_variants VALUE 'GR_PO',
        gi_so_exp TYPE /vpcoe/de_mat_doc_variants VALUE 'GI_SO_EXP',
        gi_so     TYPE /vpcoe/de_mat_doc_variants VALUE 'GI_SO',
        gr_st     TYPE /vpcoe/de_mat_doc_variants VALUE 'GR_ST',
        gi_st     TYPE /vpcoe/de_mat_doc_variants VALUE 'GI_ST',
      END OF sc_variants .

    METHODS adjust_header
      IMPORTING
        !is_sel_opt           TYPE /vpcoe/s_selopt_mat_doc
        !iv_code              TYPE /vpcoe/de_mat_doc_variants
      CHANGING
        !ct_material_document TYPE /vpcoe/cl_rdp_material_doc=>gty_t_material_data .
    METHODS cancellation_check
      CHANGING
        !ct_material_document TYPE /vpcoe/cl_rdp_material_doc=>gty_t_material_data .
    METHODS constructor
      IMPORTING
        !iv_package_size TYPE /vpcoe/de_package_size
        !iv_source       TYPE string
        !iv_api_type     TYPE /vpcoe/de_api_type .
    METHODS define_countries
      IMPORTING
        !iv_code              TYPE /vpcoe/de_mat_doc_variants
      CHANGING
        !ct_material_document TYPE /vpcoe/cl_rdp_material_doc=>gty_t_material_data .
    METHODS download_excel
      IMPORTING
        !iv_file_path   TYPE string
        !it_mat_doc     TYPE /vpcoe/t_material
        !iv_code        TYPE /vpcoe/de_mat_doc_variants
      CHANGING
        !co_xls_handler TYPE REF TO /vpcoe/cl_xls_handler .
    METHODS get_material_document
      IMPORTING
        !iv_send                TYPE abap_bool OPTIONAL
        !io_cust                TYPE REF TO /vpcoe/cl_rdp_helper
        !iv_code                TYPE /vpcoe/de_mat_doc_variants OPTIONAL
        !is_sel_opt             TYPE /vpcoe/s_selopt_mat_doc
        !io_log_supp_inv        TYPE REF TO /vpcoe/cl_rdp_log OPTIONAL
        !io_log_billdoc         TYPE REF TO /vpcoe/cl_rdp_log OPTIONAL
        !io_log                 TYPE REF TO /vpcoe/cl_rdp_log
        !iv_mode                TYPE /vpcoe/de_mode DEFAULT /vpcoe/cl_rdp_helper=>sc_mode-send
        !iv_send_mat_doc        TYPE abap_bool OPTIONAL
        !iv_send_bil_doc        TYPE abap_bool OPTIONAL
        !iv_send_sup_inv        TYPE abap_bool OPTIONAL
      EXPORTING
        !et_material_document   TYPE /vpcoe/t_material
        !et_supinvi             TYPE /vpcoe/t_supinvi_data
        !et_billdi              TYPE /vpcoe/t_billdi_data
        !ev_failed              TYPE abap_bool
        !ev_no_data             TYPE abap_bool
      CHANGING
        !ct_total_count         TYPE /vpcoe/tt_log_sum
        !ct_total_count_billdoc TYPE /vpcoe/tt_log_sum
        !ct_total_count_suppinv TYPE /vpcoe/tt_log_sum .
protected section.

  data MO_LOG type ref to /VPCOE/CL_RDP_LOG .
  data MT_ADRC type /VPCOE/T_ADRC .
  data MT_KNA1 type /VPCOE/T_KNA1 .
  data MT_LFA1 type /VPCOE/T_LFA1 .
  data MT_MOVEMENT_TYPE type GTY_T_MOVEMENT_TYPES .
  data MT_R_WERKS type /VPCOE/TT_R_WERKS_D .
  data MT_STOCK_CHANGE_CAT type GTY_T_MOVEMENT_TYPES .
  data MT_T001W type /VPCOE/T_T001W .
  data MT_TWLAD type GTY_T_TWLAD .
  data MV_API_TYPE type /VPCOE/DE_API_TYPE .
  data MV_MODE type /VPCOE/DE_MODE .
  data MV_PACKAGE_SIZE type /VPCOE/DE_PACKAGE_SIZE .
  data MV_SESSION_ID type RAW16 .
  data MV_SESSION_ITEM type INT4 .
  data MV_SOURCE type STRING .

  methods ADD_GI_PROD
    importing
      !IT_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA
      !IT_R_COUNTRY type GTY_R_COUNTRY
    exporting
      !ET_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA .
  methods ADD_GI_SO
    importing
      !IT_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA
      !IT_R_COUNTRY type GTY_R_COUNTRY
    exporting
      !ET_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA .
  methods ADD_GI_SO_EXP
    importing
      !IT_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA
      !IT_R_COUNTRY type GTY_R_COUNTRY
    exporting
      !ET_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA .
  methods ADD_GI_ST
    importing
      !IT_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA
      !IT_R_COUNTRY type GTY_R_COUNTRY
    exporting
      !ET_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA .
  methods ADD_GI_ST_EXP
    importing
      !IT_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA
      !IT_R_COUNTRY type GTY_R_COUNTRY
    exporting
      !ET_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA .
  methods ADD_GR_PO
    importing
      !IT_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA
      !IT_R_COUNTRY type GTY_R_COUNTRY
    exporting
      !ET_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA .
  methods ADD_GR_PO_IMP
    importing
      !IT_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA
      !IT_R_COUNTRY type GTY_R_COUNTRY
    exporting
      !ET_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA .
  methods ADD_GR_PROD
    importing
      !IT_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA
      !IT_R_COUNTRY type GTY_R_COUNTRY
    exporting
      !ET_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA .
  methods ADD_GR_ST
    importing
      !IT_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA
      !IT_R_COUNTRY type GTY_R_COUNTRY
    exporting
      !ET_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA .
  methods ADD_GR_ST_IMP
    importing
      !IT_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA
      !IT_R_COUNTRY type GTY_R_COUNTRY
    exporting
      !ET_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA .
  methods BUILD_WHERE_FOR_VARIANT
    importing
      !IV_CODE type /VPCOE/DE_MAT_DOC_VARIANTS
      !IS_SEL_OPT type /VPCOE/S_SELOPT_MAT_DOC
    returning
      value(RV_WHERE) type STRING .
  methods GET_HEADER_XLS
    importing
      !IV_CODE type /VPCOE/DE_MAT_DOC_VARIANTS
    exporting
      !ET_HEADER type /VPCOE/CL_XLS_HANDLER=>GTY_T_TITLE .
  methods STORE_JSON
    importing
      !IT_MATERIAL_DOCUMENT type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA
      !IO_CUST type ref to /VPCOE/CL_RDP_HELPER
    exporting
      !ES_JSON type /VPCOE/CL_RDP_HTTP=>GTY_S_JSON .
  methods HANDLE_BILLDI_SUPINVI
    importing
      !IT_MAT_DOC type /VPCOE/CL_RDP_MATERIAL_DOC=>GTY_T_MATERIAL_DATA
      !IS_SEL_OPT type /VPCOE/S_SELOPT_MAT_DOC
    returning
      value(RT_SESION_ID) type /VPCOE/CL_RDP_PAYLOAD_HANDLER=>GTY_R_SESION_ID .
  methods DEFINE_MOVEMENT_TYPES
    importing
      !IV_CODE type /VPCOE/DE_MAT_DOC_VARIANTS .
private section.
ENDCLASS.



CLASS /VPCOE/CL_RDP_MATERIAL_DOC IMPLEMENTATION.


  METHOD add_gi_prod.
    CLEAR: et_material_document.

    LOOP AT it_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>).
      IF <ls_material_document>-production_order <> ' ' AND <ls_material_document>-product <> ' ' AND <ls_material_document>-issuing_or_receiving_plant = ' '
        AND <ls_material_document>-supplier = ' ' AND <ls_material_document>-customer = ' ' AND <ls_material_document>-stock_change_category = 'GI'
        AND <ls_material_document>-ship_to_country IN it_r_country.

        INSERT <ls_material_document> INTO TABLE et_material_document.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_gi_so.
    CLEAR: et_material_document.

    LOOP AT it_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>).
      IF <ls_material_document>-sales_order <> ' ' AND <ls_material_document>-customer <> ' ' AND <ls_material_document>-product <> ' '
        AND <ls_material_document>-stock_change_category = 'GI' AND <ls_material_document>-plant_country IN it_r_country
        AND <ls_material_document>-customer_country IN it_r_country AND <ls_material_document>-plant_country = <ls_material_document>-customer_country.
        INSERT <ls_material_document> INTO TABLE et_material_document.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_gi_so_exp.
    CLEAR: et_material_document.

    LOOP AT it_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>).
      IF <ls_material_document>-sales_order <> ' ' AND <ls_material_document>-customer <> ' ' AND <ls_material_document>-product <> ' '
        AND <ls_material_document>-stock_change_category = 'GI' AND <ls_material_document>-plant_country IN it_r_country
        AND <ls_material_document>-customer_country <> <ls_material_document>-plant_country.
        INSERT <ls_material_document> INTO TABLE et_material_document.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_gi_st.

    CLEAR: et_material_document.

    IF it_material_document IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_material_document) = it_material_document.

    LOOP AT lt_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>).
      IF <ls_material_document>-issuing_or_receiving_plant <> <ls_material_document>-plant."New
        <ls_material_document>-is_cross_plant_transfer = 'T'.
      ENDIF.
      IF <ls_material_document>-product <> ' ' AND <ls_material_document>-issuing_or_receiving_plant <> ' '
        AND NOT ( <ls_material_document>-issuing_or_receiving_plant IS INITIAL AND <ls_material_document>-issuing_or_receiving_plant <> <ls_material_document>-plant ) "new
        AND <ls_material_document>-is_cross_plant_transfer = 'T' AND <ls_material_document>-stock_change_category = 'GI'
        AND <ls_material_document>-ship_to_country IN it_r_country AND <ls_material_document>-ship_to_country = <ls_material_document>-ship_from_country.
        INSERT <ls_material_document> INTO TABLE et_material_document.
      ENDIF.
    ENDLOOP.
*    LOOP AT it_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>).
*      IF <ls_material_document>-product <> ' ' AND <ls_material_document>-issuing_or_receiving_plant <> ' '
*        AND <ls_material_document>-is_cross_plant_transfer = 'T' AND <ls_material_document>-stock_change_category = 'GI'
*        AND <ls_material_document>-ship_to_country IN it_r_country AND <ls_material_document>-ship_to_country = <ls_material_document>-ship_from_country.
*        INSERT <ls_material_document> INTO TABLE et_material_document.
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.


  METHOD add_gi_st_exp.
    CLEAR: et_material_document.

    IF it_material_document IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_material_document) = it_material_document.

    LOOP AT lt_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>).
      IF <ls_material_document>-issuing_or_receiving_plant <> <ls_material_document>-plant.
        <ls_material_document>-is_cross_plant_transfer = 'T'.
      ENDIF.
      "For Customer = ' ':
      "potentially to be deleted after confirmation with RDP team
      "because this condition does not allow intercompany drop shipping
      "(641 and 643 mvmnt types with Customer maintained in the shipping tab of STO)
      IF <ls_material_document>-product <> ' ' AND <ls_material_document>-is_cross_plant_transfer = 'T'
        AND <ls_material_document>-stock_change_category = 'GI' AND <ls_material_document>-ship_from_country IN it_r_country
        AND <ls_material_document>-ship_from_country <> <ls_material_document>-ship_to_country AND <ls_material_document>-ship_from_country <> ' '.

        INSERT <ls_material_document> INTO TABLE et_material_document.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD add_gr_po.
    CLEAR: et_material_document.

    LOOP AT it_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>).

      IF <ls_material_document>-purchase_order <> ' ' AND <ls_material_document>-supplier <> ' ' AND <ls_material_document>-product <> ' '
        AND <ls_material_document>-issuing_or_receiving_plant = ' ' AND <ls_material_document>-stock_change_category = 'GR'
        AND <ls_material_document>-ship_to_country IN it_r_country AND <ls_material_document>-ship_from_country IN it_r_country
        AND <ls_material_document>-ship_to_country = <ls_material_document>-ship_from_country AND NOT line_exists( me->mt_t001w[ KEY lifnr COMPONENTS lifnr = <ls_material_document>-supplier ] ).

        INSERT <ls_material_document> INTO TABLE et_material_document.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD add_gr_po_imp.
    TYPES: BEGIN OF lt_s_gr_po_imp,
             purchase_order              TYPE mseg-ebeln,
             supplying_or_issuing_plant  TYPE ekko-reswk,
             suppl_or_issuing_plant_cntr TYPE t001w-land1,
           END OF lt_s_gr_po_imp.

    DATA: lt_si_data TYPE SORTED TABLE OF lt_s_gr_po_imp WITH NON-UNIQUE KEY purchase_order.

    CLEAR et_material_document.

    IF it_material_document IS INITIAL.
      RETURN.
    ENDIF.

    SELECT DISTINCT mseg~ebeln AS purchase_order,
                    ekko~reswk AS supplying_or_issuing_plant,
                    t2~land1   AS suppl_or_issuing_plant_cntr
   INTO CORRESPONDING FIELDS OF TABLE @lt_si_data
     FROM mseg LEFT JOIN ekko ON mseg~ebeln = ekko~ebeln
               LEFT JOIN t156  ON mseg~bwart = t156~bwart
               LEFT JOIN t001w AS t1 ON mseg~werks = t1~werks
               LEFT JOIN t001w AS t2 ON mseg~umwrk = t2~werks
     FOR ALL ENTRIES IN @it_material_document
         WHERE mseg~ebeln <> ''
           AND mseg~matnr <> ' '
           AND mseg~umwrk <> mseg~werks
           AND mseg~ebeln = @it_material_document-purchase_order.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR: et_material_document.

    LOOP AT it_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>).
      IF <ls_material_document>-purchase_order <> ' ' AND <ls_material_document>-supplier <> ' ' AND <ls_material_document>-product <> ' '
        AND <ls_material_document>-issuing_or_receiving_plant = ' ' AND <ls_material_document>-stock_change_category = 'GR'
        AND <ls_material_document>-ship_to_country IN it_r_country AND <ls_material_document>-ship_to_country <> <ls_material_document>-ship_from_country.
        DATA(lv_issuing_plant) = VALUE #( lt_si_data[ purchase_order = <ls_material_document>-purchase_order ]-supplying_or_issuing_plant OPTIONAL ).
        IF lv_issuing_plant IS INITIAL.
          INSERT <ls_material_document> INTO TABLE et_material_document.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_gr_prod.
    CLEAR: et_material_document.

    LOOP AT it_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>).
      IF <ls_material_document>-production_order <> ' ' AND <ls_material_document>-product <> ' '
        AND <ls_material_document>-issuing_or_receiving_plant = ' ' AND <ls_material_document>-supplier = ' '
        AND <ls_material_document>-customer = ' ' AND <ls_material_document>-stock_change_category = 'GR'
        AND <ls_material_document>-ship_to_country IN it_r_country.

        INSERT <ls_material_document> INTO TABLE et_material_document.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_gr_st.
    TYPES: BEGIN OF lty_s_gr_st,
             purchase_order             TYPE mseg-ebeln,
             supplying_or_issuing_plant TYPE ekko-reswk,
             text_id                    TYPE mseg-sgtxt,
             id                         TYPE mseg-mblnr,
             plant                      TYPE mseg-werks,
           END OF lty_s_gr_st.

    TYPES: lty_t_gr_st TYPE SORTED TABLE OF lty_s_gr_st WITH NON-UNIQUE KEY purchase_order
                                                        WITH NON-UNIQUE SORTED KEY text_id COMPONENTS text_id.

    DATA: lt_si_data     TYPE lty_t_gr_st,
          lt_mat_doc_po  TYPE /vpcoe/cl_rdp_material_doc=>gty_t_material_data,
          lt_mat_doc_txt TYPE /vpcoe/cl_rdp_material_doc=>gty_t_material_data.

*
    CLEAR: et_material_document.

    DATA(lt_material_document) = it_material_document.

    IF lt_material_document IS INITIAL.
      RETURN.
    ENDIF.

    lt_mat_doc_po = VALUE #( FOR <ls_f_mat_doc> IN lt_material_document WHERE ( purchase_order NE ' ' ) ( <ls_f_mat_doc> ) ).
    lt_mat_doc_txt = VALUE #( FOR <ls_f_mat_doc> IN lt_material_document WHERE ( text_id NE ' ' ) ( <ls_f_mat_doc> ) ).

    IF lt_mat_doc_po IS NOT INITIAL.
      SELECT mseg~ebeln AS purchase_order,
             ekko~reswk AS supplying_or_issuing_plant,
             mseg~sgtxt AS text_id,
             mseg~mblnr AS id,
             mseg~werks AS plant
       INTO CORRESPONDING FIELDS OF TABLE @lt_si_data
         FROM mseg LEFT JOIN ekko ON mseg~ebeln = ekko~ebeln
                   LEFT JOIN t001w AS t1 ON mseg~werks = t1~werks
                   LEFT JOIN t156  ON mseg~bwart = t156~bwart
         FOR ALL ENTRIES IN @lt_mat_doc_po
             WHERE mseg~matnr <> ' '
               AND mseg~umwrk <> mseg~werks
               AND t1~land1 = @lt_mat_doc_po-ship_to_country
               AND (    ( mseg~shkzg = 'H' AND t156~xstbw = ' ' AND t156~kzdru <> '2' )
                     OR ( mseg~shkzg = 'S' AND t156~xstbw = 'X' AND mseg~smbln = ' ' ) )
               AND mseg~ebeln = @lt_mat_doc_po-purchase_order.
    ENDIF.

    IF lt_mat_doc_txt IS NOT INITIAL.
      SELECT mseg~ebeln AS purchase_order,
             ekko~reswk AS supplying_or_issuing_plant,
             mseg~sgtxt AS text_id,
             mseg~mblnr AS id,
             mseg~werks AS plant
       APPENDING CORRESPONDING FIELDS OF TABLE @lt_si_data
         FROM mseg LEFT JOIN ekko ON mseg~ebeln = ekko~ebeln
                   LEFT JOIN t001w AS t1 ON mseg~werks = t1~werks
                   LEFT JOIN t156  ON mseg~bwart = t156~bwart
         FOR ALL ENTRIES IN @lt_mat_doc_txt
             WHERE mseg~matnr <> ' '
               AND mseg~umwrk <> mseg~werks
               AND t1~land1 = @lt_mat_doc_txt-ship_to_country
               AND (    ( mseg~shkzg = 'H' AND t156~xstbw = ' ' AND t156~kzdru <> '2' )
                     OR ( mseg~shkzg = 'S' AND t156~xstbw = 'X' AND mseg~smbln = ' ' ) )
               AND mseg~sgtxt = @lt_mat_doc_txt-text_id.
    ENDIF.

    IF sy-subrc <> 0.
      CLEAR lt_si_data.
    ENDIF.

    DELETE lt_material_document WHERE stock_change_category = 'GI' AND goods_movement_type NOT IN mt_movement_type.

    LOOP AT lt_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>).
      IF <ls_material_document>-goods_movement_type IN mt_stock_change_cat AND <ls_material_document>-stock_change_category = 'GI'
        OR <ls_material_document>-goods_movement_type = '302' AND <ls_material_document>-deb_cred_ind = 'H'.
        <ls_material_document>-stock_change_category = 'GR'.
      ENDIF.
      IF <ls_material_document>-product <> ' ' AND <ls_material_document>-stock_change_category = 'GR' AND <ls_material_document>-ship_to_country IN it_r_country
         AND <ls_material_document>-ship_from_country = <ls_material_document>-ship_to_country.
        IF <ls_material_document>-issuing_or_receiving_plant = ' '.
          <ls_material_document>-issuing_or_receiving_plant = COND #( WHEN <ls_material_document>-purchase_order <> ''
                                                                       THEN VALUE #( lt_si_data[ purchase_order = <ls_material_document>-purchase_order ]-supplying_or_issuing_plant OPTIONAL )
                                                                      WHEN <ls_material_document>-text_id <> '' "new
                                                                       THEN VALUE #( lt_si_data[ KEY text_id COMPONENTS text_id = <ls_material_document>-text_id ]-plant OPTIONAL ) ).
        ENDIF.
        IF <ls_material_document>-issuing_or_receiving_plant IS NOT INITIAL AND <ls_material_document>-issuing_or_receiving_plant <> <ls_material_document>-plant.
          <ls_material_document>-is_cross_plant_transfer = 'T'.
          INSERT <ls_material_document> INTO TABLE et_material_document.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD add_gr_st_imp.
    TYPES: BEGIN OF lty_s_gr_st_imp,
             purchase_order              TYPE  mseg-ebeln,
             supplying_or_issuing_plant  TYPE  ekko-reswk,
             suppl_or_issuing_plant_cntr TYPE  t001w-land1,
           END OF lty_s_gr_st_imp.

    DATA: lt_si_data           TYPE SORTED TABLE OF lty_s_gr_st_imp WITH NON-UNIQUE KEY purchase_order,
          lt_material_document TYPE SORTED TABLE OF gty_s_material_data WITH NON-UNIQUE KEY stock_change_category.

    FIELD-SYMBOLS: <ls_mat_doc> TYPE lty_s_gr_st_imp.

*
    CLEAR: et_material_document.

    IF it_material_document IS INITIAL.
      RETURN.
    ENDIF.

    lt_material_document = it_material_document.
    DELETE lt_material_document WHERE stock_change_category = 'GI'.

    SELECT DISTINCT mseg~ebeln AS purchase_order,
                    ekko~reswk AS supplying_or_issuing_plant,
                    t2~land1   AS suppl_or_issuing_plant_cntr
       INTO CORRESPONDING FIELDS OF TABLE @lt_si_data
         FROM mseg LEFT JOIN ekko ON mseg~ebeln = ekko~ebeln
                   LEFT JOIN t156  ON mseg~bwart = t156~bwart
                   LEFT JOIN t001w AS t1 ON mseg~werks = t1~werks
                   LEFT JOIN t001w AS t2 ON mseg~umwrk = t2~werks
         FOR ALL ENTRIES IN @lt_material_document
             WHERE mseg~ebeln <> ''
               AND mseg~matnr <> ' '
               AND mseg~umwrk <> mseg~werks
               AND mseg~ebeln = @lt_material_document-purchase_order.

    IF sy-subrc <> 0.
      CLEAR lt_si_data.
    ENDIF.

    LOOP AT lt_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>).
      IF <ls_material_document>-product <> ' ' AND <ls_material_document>-customer = ' '
         AND <ls_material_document>-stock_change_category = 'GR' AND <ls_material_document>-ship_to_country IN it_r_country
        AND <ls_material_document>-ship_to_country <> <ls_material_document>-ship_from_country.

        IF <ls_material_document>-issuing_or_receiving_plnt_cntr = ' ' AND <ls_material_document>-issuing_or_receiving_plant = ' '
           AND line_exists( lt_si_data[ purchase_order = <ls_material_document>-purchase_order ] ).

          ASSIGN lt_si_data[ purchase_order = <ls_material_document>-purchase_order ] TO <ls_mat_doc>.
          IF <ls_mat_doc>-supplying_or_issuing_plant IS NOT INITIAL AND <ls_mat_doc>-supplying_or_issuing_plant <> <ls_material_document>-plant.
            <ls_material_document>-is_cross_plant_transfer = 'T'.
            <ls_material_document>-issuing_or_receiving_plant = <ls_mat_doc>-supplying_or_issuing_plant.
            <ls_material_document>-issuing_or_receiving_plnt_cntr = <ls_mat_doc>-suppl_or_issuing_plant_cntr.

            INSERT <ls_material_document> INTO TABLE et_material_document.
          ENDIF.
        ELSEIF <ls_material_document>-issuing_or_receiving_plnt_cntr <> ' ' AND <ls_material_document>-issuing_or_receiving_plant <> ' '
        AND <ls_material_document>-is_cross_plant_transfer <> 'F'.

          INSERT <ls_material_document> INTO TABLE et_material_document.
        ENDIF.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD adjust_header.
    TYPES: BEGIN OF lty_s_si_data,
             purchase_order               TYPE mseg-ebeln,
             supplying_or_issuing_plant   TYPE ekpo-werks,
             suppl_or_issuing_plant_cntr  TYPE t001w-land1,
             suppl_or_issuing_plant_regio TYPE t001w-regio,
             storage_location             TYPE lgort_d, "New
           END OF lty_s_si_data.

    DATA: lo_badi                TYPE REF TO /vpcoe/adjust_data_retrieval,
          lt_material_doc_wo_var TYPE /vpcoe/cl_rdp_material_doc=>gty_t_material_data,
          lt_si_data             TYPE SORTED TABLE OF lty_s_si_data WITH NON-UNIQUE KEY purchase_order,
          lv_skip                TYPE abap_bool.

    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-adjust_hdr
        iv_code     = iv_code
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    IF lv_skip = abap_false.
      IF iv_code = me->sc_variants-gi_st_exp OR iv_code = me->sc_variants-gi_st.
        " itercompany stock transfer GI: in this case umwrk is empty
        " therefor we take issuuing/receiving value from po item.
        " Not from GR mat dc because it might not have been posted yet
        SELECT DISTINCT mseg~ebeln AS purchase_order, "+ lgort for ship-to/from country.
                        ekpo~werks AS supplying_or_issuing_plant,
                        ekpo~lgort AS storage_location,
                        t1~land1   AS suppl_or_issuing_plant_cntr,
                        t1~regio   AS suppl_or_issuing_plant_regio
          INTO CORRESPONDING FIELDS OF TABLE @lt_si_data
          FROM mseg LEFT JOIN ekpo ON mseg~ebeln = ekpo~ebeln AND mseg~ebelp = ekpo~ebelp
                 LEFT JOIN t001w AS t1 ON ekpo~werks = t1~werks
            FOR ALL ENTRIES IN @ct_material_document
              WHERE mseg~ebeln <> ' '
                AND mseg~matnr <> ' '
                AND mseg~umwrk <> mseg~werks
                AND mseg~ebeln = @ct_material_document-purchase_order
                AND mseg~ebelp = @ct_material_document-ebelp.
        IF sy-subrc <> 0.
          CLEAR lt_si_data.
        ENDIF.
      ENDIF.

      LOOP AT ct_material_document ASSIGNING FIELD-SYMBOL(<ls_mat_doc>).
        IF ( iv_code = me->sc_variants-gi_st_exp OR iv_code = me->sc_variants-gi_st ) AND <ls_mat_doc>-issuing_or_receiving_plant = ' '."new
          ASSIGN lt_si_data[ purchase_order = <ls_mat_doc>-purchase_order ] TO FIELD-SYMBOL(<ls_si_data>).
          IF sy-subrc = 0.
            <ls_mat_doc>-storage_location               = <ls_si_data>-storage_location."new
            <ls_mat_doc>-issuing_or_receiving_plant     = <ls_si_data>-supplying_or_issuing_plant.
            <ls_mat_doc>-issuing_or_receiving_plnt_cntr = <ls_si_data>-suppl_or_issuing_plant_cntr.
            <ls_mat_doc>-issuing_or_receiving_plnt_reg  = <ls_si_data>-suppl_or_issuing_plant_regio.
          ENDIF.
        ELSE.
          ASSIGN me->mt_t001w[ werks = <ls_mat_doc>-plant ] TO FIELD-SYMBOL(<ls_t001>).
          IF sy-subrc = 0.
            <ls_mat_doc>-plant_country = <ls_t001>-land1.
            <ls_mat_doc>-plant_region = <ls_t001>-regio.
          ENDIF.

          ASSIGN me->mt_t001w[ werks = <ls_mat_doc>-issuing_or_receiving_plant ] TO <ls_t001>.
          IF sy-subrc = 0.
            <ls_mat_doc>-issuing_or_receiving_plnt_cntr = <ls_t001>-land1.
            <ls_mat_doc>-issuing_or_receiving_plnt_reg = <ls_t001>-regio.
          ENDIF.

          <ls_mat_doc>-supplier_country = VALUE #( mt_lfa1[ lifnr = <ls_mat_doc>-supplier ]-land1 OPTIONAL ).

          ASSIGN me->mt_kna1[ kunnr = <ls_mat_doc>-customer ] TO FIELD-SYMBOL(<ls_kna1>).
          IF sy-subrc = 0.
            <ls_mat_doc>-customer_country = <ls_kna1>-land1.
            <ls_mat_doc>-customer_region = <ls_kna1>-regio.
          ENDIF.
        ENDIF.
      ENDLOOP.

      lt_material_doc_wo_var = ct_material_document.

      me->define_countries( EXPORTING iv_code              = iv_code
                            CHANGING  ct_material_document = lt_material_doc_wo_var ).

      CASE iv_code.
        WHEN me->sc_variants-gr_prod .
          me->add_gr_prod(
            EXPORTING
              it_material_document = lt_material_doc_wo_var
              it_r_country         = is_sel_opt-country
            IMPORTING
              et_material_document = ct_material_document ).

        WHEN me->sc_variants-gi_prod.
          me->add_gi_prod(
            EXPORTING
              it_material_document = lt_material_doc_wo_var
              it_r_country         = is_sel_opt-country
            IMPORTING
              et_material_document = ct_material_document ).

        WHEN me->sc_variants-gr_st_imp.
          me->add_gr_st_imp(
            EXPORTING
              it_material_document = lt_material_doc_wo_var
              it_r_country         = is_sel_opt-country
            IMPORTING
              et_material_document = ct_material_document ).

        WHEN me->sc_variants-gi_st_exp.
          me->add_gi_st_exp(
           EXPORTING
             it_material_document = lt_material_doc_wo_var
             it_r_country         = is_sel_opt-country
           IMPORTING
             et_material_document = ct_material_document ).

        WHEN me->sc_variants-gr_po_imp.
          me->add_gr_po_imp(
            EXPORTING
              it_material_document = lt_material_doc_wo_var
              it_r_country         = is_sel_opt-country
            IMPORTING
              et_material_document = ct_material_document ).

        WHEN me->sc_variants-gr_po.
          me->add_gr_po(
            EXPORTING
              it_material_document = lt_material_doc_wo_var
              it_r_country         = is_sel_opt-country
            IMPORTING
              et_material_document = ct_material_document ).

        WHEN me->sc_variants-gi_so_exp.
          me->add_gi_so_exp(
            EXPORTING
              it_material_document = lt_material_doc_wo_var
              it_r_country         = is_sel_opt-country
            IMPORTING
              et_material_document = ct_material_document ).

        WHEN me->sc_variants-gi_so.
          me->add_gi_so(
            EXPORTING
              it_material_document = lt_material_doc_wo_var
              it_r_country         = is_sel_opt-country
            IMPORTING
              et_material_document = ct_material_document ).

        WHEN me->sc_variants-gi_st.
          me->add_gi_st(
            EXPORTING
              it_material_document = lt_material_doc_wo_var
              it_r_country         = is_sel_opt-country
            IMPORTING
              et_material_document = ct_material_document ).

        WHEN me->sc_variants-gr_st.
          me->add_gr_st(
            EXPORTING
              it_material_document = lt_material_doc_wo_var
              it_r_country         = is_sel_opt-country
            IMPORTING
              et_material_document = ct_material_document ).

      ENDCASE.

    ENDIF.

    CALL BADI lo_badi->adjust_data_retrieval
      EXPORTING
        iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-adjust_hdr
        is_sel_opt  = is_sel_opt
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
        iv_code     = iv_code
      CHANGING
        ct_data     = ct_material_document.
  ENDMETHOD.


  METHOD build_where_for_variant.
    DATA: lo_badi       TYPE REF TO /vpcoe/adjust_data_retrieval.

    CLEAR: me->mt_r_werks, me->mt_r_werks.

    LOOP AT is_sel_opt-werks ASSIGNING FIELD-SYMBOL(<ls_sel_opt_werks>).
      INSERT <ls_sel_opt_werks> INTO TABLE me->mt_r_werks.
    ENDLOOP.

    me->define_movement_types( EXPORTING iv_code = iv_code ).

    CASE iv_code.
      WHEN me->sc_variants-gr_prod .

        rv_where = `ms1~aufnr <> ' ' ` &&
               `AND ms1~matnr <> ' ' ` &&
               `AND ms1~umwrk = ' ' ` &&
               `AND ms1~lifnr = ' ' ` &&
               `AND ms1~kunnr = ' ' ` &&
               `AND ( ` &&
                   `( ms1~shkzg = 'S' AND ( ( t156~xstbw = ' ' AND t156~kzdru <> ' ' ) OR ( t156~xstbw = 'X' AND t156~kzdru = '1' ) ) ) ` &&
                   `OR ` &&
                   `( ms1~shkzg = 'H' AND ( ( t156~xstbw = 'X' AND t156~kzdru <> ' ' ) OR ( t156~xstbw = ' ' AND t156~kzdru = '2' ) ) ) ` &&
                   `)`.

      WHEN me->sc_variants-gi_prod.

        rv_where = `ms1~aufnr <> ' ' ` &&
               `AND ms1~matnr <> ' ' ` &&
               `AND ms1~umwrk = ' ' ` &&
               `AND ms1~lifnr = ' ' ` &&
               `AND ms1~kunnr = ' ' ` &&
               `AND ( ` &&
                   `( ms1~shkzg = 'S' AND ( ( t156~xstbw = ' ' AND t156~kzdru = ' ' ) OR ( t156~xstbw = 'X' AND t156~kzdru <> '1' ) ) ) ` &&
                   `OR ` &&
                   `( ms1~shkzg = 'H' AND ( ( t156~xstbw = 'X' AND t156~kzdru = ' ' ) OR ( t156~xstbw = ' ' AND t156~kzdru <> '2' ) ) ) ` &&
                   `)`.

      WHEN me->sc_variants-gr_st_imp.

        rv_where = `( ` &&
                   `( ms1~ebeln <> ' ' AND ms1~xauto <> 'X' ) ` &&
                   `OR ` &&
                   `( ms1~ebeln = ' ' AND ms1~bwart IN @mt_movement_type ) ` &&
                   `) ` &&
                   `AND ms1~matnr <> ' ' ` &&
                   `AND ` &&
           `          ( ` &&
                     `( ms1~shkzg = 'S' AND ( ( t156~xstbw = ' ' AND t156~kzdru <> ' ' ) OR ( t156~xstbw = 'X' AND t156~kzdru = '1' ) ) ) ` &&
                     `OR ` &&
                     `( ms1~shkzg = 'H' AND ( ( t156~xstbw = 'X' AND t156~kzdru <> ' ' ) OR ( t156~xstbw = ' ' AND t156~kzdru = '2' ) ) ) ` &&
                     `)`.

      WHEN me->sc_variants-gi_st_exp.

        rv_where = `( ` &&
                   `( ms1~ebeln <> ' ' AND ekpo~pstyp NOT IN ('3', 'L') )  ` &&
                   `OR  ` &&
                   `( ms1~ebeln = ' ' AND ms1~bwart IN @mt_movement_type ) ` &&
                   `)  ` &&
                   `AND ms1~matnr <> ' ' ` &&
                   `AND ms1~xauto <> 'X' ` &&
                   `AND ( ` &&
                       `( ms1~shkzg = 'S' AND ( ( t156~xstbw = ' ' AND t156~kzdru = ' ' ) OR ( t156~xstbw = 'X' AND t156~kzdru <> '1' ) ) ) ` &&
                       `OR ` &&
                       `( ms1~shkzg = 'H' AND ( ( t156~xstbw = 'X' AND t156~kzdru = ' ' ) OR ( t156~xstbw = ' ' AND t156~kzdru <> '2' ) ) ) ` &&
                       `) `.
*                   `` .

      WHEN me->sc_variants-gr_po_imp.

        rv_where = `ms1~ebeln <> ' ' ` &&
               `AND ms1~lifnr <> ' ' ` &&
               `AND ms1~matnr <> ' ' ` &&
               `AND ms1~umwrk = ' ' ` &&
               `AND ( ` &&
                   `( ms1~shkzg = 'S' AND ( ( t156~xstbw = ' ' AND t156~kzdru <> ' ' ) OR ( t156~xstbw = 'X' AND t156~kzdru = '1' ) ) ) ` &&
                   `OR ` &&
                   `( ms1~shkzg = 'H' AND ( ( t156~xstbw = 'X' AND t156~kzdru <> ' ' ) OR ( t156~xstbw = ' ' AND t156~kzdru = '2' ) ) ) ` &&
                   `) `.
      WHEN me->sc_variants-gr_po.

        rv_where = `ms1~ebeln <> ' ' ` &&
               `AND ms1~lifnr <> ' ' ` &&
               `AND ms1~matnr <> ' ' ` &&
               `AND ms1~umwrk = ' ' ` &&
               `AND ( ` &&
                     `( ms1~shkzg = 'S' AND ( ( t156~xstbw = ' ' AND t156~kzdru <> ' ' ) OR ( t156~xstbw = 'X' AND t156~kzdru = '1' ) ) ) ` &&
                     `OR ` &&
                     `( ms1~shkzg = 'H' AND ( ( t156~xstbw = 'X' AND t156~kzdru <> ' ' ) OR ( t156~xstbw = ' ' AND t156~kzdru = '2' ) ) ) ` &&
                    `) `.

      WHEN me->sc_variants-gi_so_exp.

        rv_where = `ms1~kdauf <> ' ' ` &&
               `AND ms1~kunnr <> ' ' ` &&
               `AND ms1~matnr <> ' ' ` &&
               `AND ( ` &&
                       `( ms1~shkzg = 'S' AND ( ( t156~xstbw = ' ' AND t156~kzdru = ' ' ) OR ( t156~xstbw = 'X' AND t156~kzdru <> '1' ) ) ) ` &&
                      `OR ` &&
                       `( ms1~shkzg = 'H' AND ( ( t156~xstbw = 'X' AND t156~kzdru = ' ' ) OR ( t156~xstbw = ' ' AND t156~kzdru <> '2' ) ) ) ` &&
                    `) `.

      WHEN me->sc_variants-gi_so.

        rv_where = `ms1~kdauf <> ' ' ` &&
               `AND ms1~kunnr <> ' ' ` &&
               `AND ms1~matnr <> ' ' ` &&
               `AND ( ` &&
                     `( ms1~shkzg = 'S' AND ( ( t156~xstbw = ' ' AND t156~kzdru = ' ' ) OR ( t156~xstbw = 'X' AND t156~kzdru <> '1' ) ) ) ` &&
                   `OR ` &&
                     `( ms1~shkzg = 'H' AND ( ( t156~xstbw = 'X' AND t156~kzdru = ' ' ) OR ( t156~xstbw = ' ' AND t156~kzdru <> '2' ) ) ) ` &&
                    `) `.

      WHEN me->sc_variants-gi_st.
        rv_where = `ms1~matnr <> ' ' ` &&
*               `AND ms1~umwrk <> ' ' ` &&"New
*               `AND NOT ( ms1~umwrk = ' ' AND ms1~umwrk <> ms1~werks ) ` &&"New
               `AND ( ` &&
                     `( ms1~shkzg = 'S' AND ( ( t156~xstbw = ' ' AND t156~kzdru = ' ' ) OR ( t156~xstbw = 'X' AND t156~kzdru <> '1' ) ) ) ` &&
                    `OR ` &&
                     `( ms1~shkzg = 'H' AND ( ( t156~xstbw = 'X' AND t156~kzdru = ' ' ) OR ( t156~xstbw = ' ' AND t156~kzdru <> '2' ) ) ) ` &&
                    `) ` &&
*               `AND ms1~umwrk IN @me->mt_r_umwrk AND ms1~xauto <> 'X' ` &&
               `AND ms1~xauto <> 'X' ` &&
               `AND ( ` &&
                      `( ms1~ebeln <> ' ' AND ekpo~pstyp NOT IN ('3', 'L') ) ` &&
                    `OR ` &&
                      `( ms1~ebeln = ' ' AND ms1~bwart IN @mt_movement_type ) ` &&
                    `) `.

      WHEN me->sc_variants-gr_st.

        rv_where = `ms1~matnr <> ' ' ` &&
                `AND ( ` &&
                      `( ms1~ebeln <> ' ' AND ms1~xauto <> 'X' ) ` &&
                     `OR ` &&
                      `( ms1~ebeln = ' ' AND ms1~bwart IN @mt_movement_type ) ` &&
                     `) `.
    ENDCASE.

    " Adjust where condition
    GET BADI lo_badi.
    CALL BADI lo_badi->adjust_build_where_for_variant
      EXPORTING
        iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc
        iv_code     = iv_code
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_where    = rv_where.

  ENDMETHOD.


  METHOD cancellation_check.
    DATA: lt_mat_doc TYPE SORTED TABLE OF gty_s_material_data WITH NON-UNIQUE KEY cancellation_id
                                                                                  mat_doc_item
                                                                                  is_reversal_movement_type.

    lt_mat_doc = ct_material_document.

    LOOP AT ct_material_document ASSIGNING FIELD-SYMBOL(<ls_material_doc>).
      IF line_exists( lt_mat_doc[ cancellation_id           = <ls_material_doc>-id
                                  mat_doc_item              = <ls_material_doc>-item
                                  is_reversal_movement_type = abap_true ] ).

        <ls_material_doc>-is_cancelled = abap_true.
      ELSE.
        <ls_material_doc>-is_cancelled = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    me->mv_api_type     = iv_api_type.
    me->mv_package_size = iv_package_size.
    me->mv_source       = iv_source.

    SELECT *
      INTO TABLE me->mt_twlad
         FROM twlad.

    IF sy-subrc = 0.
      SELECT DISTINCT addrnumber, country, region
        INTO CORRESPONDING FIELDS OF TABLE @mt_adrc
          FROM adrc
            FOR ALL ENTRIES IN @me->mt_twlad
              WHERE addrnumber = @me->mt_twlad-adrnr
                AND addrnumber <> ' '.

      IF sy-subrc <> 0.
        CLEAR me->mt_adrc.
      ENDIF.
    ENDIF.

    SELECT werks, land1, regio, lifnr
      INTO CORRESPONDING FIELDS OF TABLE @me->mt_t001w
        FROM t001w.
    IF sy-subrc <> 0.
      CLEAR me->mt_t001w.
    ENDIF.

    SELECT lifnr, land1
      INTO CORRESPONDING FIELDS OF TABLE @me->mt_lfa1
        FROM lfa1.
    IF sy-subrc <> 0.
      CLEAR me->mt_lfa1.
    ENDIF.

    SELECT kunnr, land1, regio
      INTO CORRESPONDING FIELDS OF TABLE @me->mt_kna1
        FROM kna1.
    IF sy-subrc <> 0.
      CLEAR me->mt_kna1.
    ENDIF.

  ENDMETHOD.


  METHOD define_countries.
    TYPES: BEGIN OF lty_s_lfa1,
             lifnr TYPE lfa1-lifnr,
             land1 TYPE lfa1-land1,
             regio TYPE lfa1-regio,
           END OF lty_s_lfa1,
           BEGIN OF lty_s_mseg,
             ebeln TYPE mseg-ebeln,
             lgort TYPE mseg-lgort,
             werks TYPE mseg-werks,
             shkzg TYPE shkzg,
             umwrk TYPE umwrk,
             sgtxt TYPE sgtxt,
           END OF lty_s_mseg,
           BEGIN OF lty_s_ekko,
             ebeln TYPE ekpo-ebeln,
             ebelp TYPE ekpo-ebelp,
             retpo TYPE ekpo-retpo,
             reswk TYPE ekko-reswk,
           END OF lty_s_ekko,
           BEGIN OF lty_s_lifnr,
             lifnr TYPE lfa1-lifnr,
           END OF lty_s_lifnr.

    DATA: lo_badi        TYPE REF TO /vpcoe/enh_rdp_material_doc,
          lt_lfa1        TYPE SORTED TABLE OF lty_s_lfa1 WITH NON-UNIQUE KEY lifnr,
          lt_lifnr       TYPE TABLE OF lty_s_lifnr,
          lt_adrc        TYPE /vpcoe/t_adrc,
          lt_adrnr       TYPE STANDARD TABLE OF /vpcoe/s_adrc,
          lt_lfa2        TYPE SORTED TABLE OF lty_s_lfa1 WITH NON-UNIQUE KEY lifnr,
          lt_mseg        TYPE SORTED TABLE OF lty_s_mseg WITH NON-UNIQUE KEY ebeln,
          lt_ekko        TYPE SORTED TABLE OF lty_s_ekko WITH NON-UNIQUE KEY ebeln,
          lt_mat_doc_po  TYPE /vpcoe/cl_rdp_material_doc=>gty_t_material_data,
          lt_mat_doc_txt TYPE /vpcoe/cl_rdp_material_doc=>gty_t_material_data,
          lv_adrnr_to    TYPE ad_addrnum,
          lv_adrnr_from  TYPE ad_addrnum.

    FIELD-SYMBOLS: <ls_adrc> TYPE LINE OF gty_t_adrc.

    IF ct_material_document IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ct_material_document ASSIGNING FIELD-SYMBOL(<ls_mat_doc>).
      "Collect Lifnr
      IF iv_code = me->sc_variants-gr_po_imp OR iv_code = me->sc_variants-gr_po.
        IF <ls_mat_doc>-goods_supplier IS NOT INITIAL.
          INSERT VALUE #( lifnr = <ls_mat_doc>-goods_supplier ) INTO TABLE lt_lifnr.
        ENDIF.
        IF <ls_mat_doc>-vendor_acc_number IS NOT INITIAL.
          INSERT VALUE #( lifnr = <ls_mat_doc>-vendor_acc_number ) INTO TABLE lt_lifnr.
        ENDIF.
        IF <ls_mat_doc>-suppl_vendor IS NOT INITIAL.
          INSERT VALUE #( lifnr = <ls_mat_doc>-suppl_vendor ) INTO TABLE lt_lifnr.
        ENDIF.
      ENDIF.

      "Collect Address Number
      IF <ls_mat_doc>-address IS NOT INITIAL.
        INSERT VALUE #( addrnumber = <ls_mat_doc>-address ) INTO TABLE lt_adrnr.
      ENDIF.

      IF <ls_mat_doc>-address2 IS NOT INITIAL.
        INSERT VALUE #( addrnumber = <ls_mat_doc>-address2 ) INTO TABLE lt_adrnr.
      ENDIF.
    ENDLOOP.

    lt_adrc = me->mt_adrc.

    IF lt_adrnr IS NOT INITIAL.
      SORT lt_adrnr BY addrnumber.
      DELETE ADJACENT DUPLICATES FROM lt_adrnr COMPARING addrnumber.

      SELECT DISTINCT addrnumber, country, region
        APPENDING CORRESPONDING FIELDS OF TABLE @lt_adrc
          FROM adrc
            FOR ALL ENTRIES IN @lt_adrnr
              WHERE addrnumber = @lt_adrnr-addrnumber.

      IF sy-subrc = 0.
        DELETE ADJACENT DUPLICATES FROM lt_adrc COMPARING addrnumber.
      ENDIF.
    ENDIF.

    " Get LIFNRs
    IF iv_code = me->sc_variants-gr_po_imp OR iv_code = me->sc_variants-gr_po.
      SORT lt_lifnr BY lifnr.
      DELETE ADJACENT DUPLICATES FROM lt_lifnr COMPARING lifnr.

      SELECT DISTINCT lifnr, land1, regio
        INTO CORRESPONDING FIELDS OF TABLE @lt_lfa1
          FROM lfa1
            FOR ALL ENTRIES IN @lt_lifnr
              WHERE lifnr = @lt_lifnr-lifnr.
      IF sy-subrc <> 0.
        CLEAR lt_lfa1.
      ENDIF.

    ENDIF.

    IF iv_code = me->sc_variants-gr_st_imp OR iv_code =  me->sc_variants-gr_st.
      lt_mat_doc_po = VALUE #( FOR <ls_f_mat_doc> IN ct_material_document WHERE ( purchase_order NE ' ' ) ( <ls_f_mat_doc> ) ).
      lt_mat_doc_txt = VALUE #( FOR <ls_f_mat_doc> IN ct_material_document WHERE ( text_id NE ' ' ) ( <ls_f_mat_doc> ) ).

      IF lt_mat_doc_po IS NOT INITIAL.
        SELECT DISTINCT ebeln, lgort, werks, shkzg, umwrk, sgtxt
          INTO CORRESPONDING FIELDS OF TABLE @lt_mseg
           FROM mseg
             FOR ALL ENTRIES IN @lt_mat_doc_po
              WHERE ebeln = @lt_mat_doc_po-purchase_order
                AND mseg~umwrk <> ''
                AND mseg~xauto <> 'X'.
      ENDIF.

      IF lt_mat_doc_txt IS NOT INITIAL.
        SELECT DISTINCT ebeln, lgort, werks, shkzg, umwrk, sgtxt
          APPENDING CORRESPONDING FIELDS OF TABLE @lt_mseg
           FROM mseg
             FOR ALL ENTRIES IN @lt_mat_doc_txt
              WHERE mseg~sgtxt = @lt_mat_doc_txt-text_id
                AND mseg~umwrk <> ''
                AND mseg~xauto <> 'X'.
      ENDIF.

      IF sy-subrc <> 0.
        CLEAR lt_mseg.
      ENDIF.

      SELECT DISTINCT ekpo~ebeln, ekpo~ebelp, ekpo~retpo, ekko~reswk
        INTO CORRESPONDING FIELDS OF TABLE @lt_ekko
         FROM ekpo
          LEFT JOIN ekko ON ekpo~ebeln = ekko~ebeln
           FOR ALL ENTRIES IN @ct_material_document
            WHERE ekpo~ebeln = @ct_material_document-purchase_order
              AND ekpo~ebelp = @ct_material_document-ebelp
              AND ekko~reswk <> ' '.
      IF sy-subrc <> 0.
        CLEAR lt_ekko.
      ENDIF.
    ENDIF.

    LOOP AT ct_material_document ASSIGNING <ls_mat_doc>.
      CLEAR: lv_adrnr_from,lv_adrnr_to, <ls_mat_doc>-ship_from_country, <ls_mat_doc>-ship_to_country, <ls_mat_doc>-ship_to_region.

      CASE iv_code.
*GR with purchase order reference imported in specific country
*GR with purchase order reference in specific country (domestic)
        WHEN me->sc_variants-gr_po_imp OR me->sc_variants-gr_po.

          IF <ls_mat_doc>-goods_supplier IS NOT INITIAL." Prio 1
            <ls_mat_doc>-ship_from_country = VALUE #( lt_lfa1[ lifnr = <ls_mat_doc>-goods_supplier ]-land1 OPTIONAL ).
          ELSE."Prio 2
            <ls_mat_doc>-ship_from_country = <ls_mat_doc>-supplier_country.
          ENDIF.

          IF <ls_mat_doc>-address IS NOT INITIAL."Prio 1
            ASSIGN lt_adrc[ addrnumber = <ls_mat_doc>-address ] TO <ls_adrc>.
            IF sy-subrc = 0 AND <ls_adrc>-country IS NOT INITIAL.
              <ls_mat_doc>-ship_to_country = <ls_adrc>-country.
              <ls_mat_doc>-ship_to_region = <ls_adrc>-region.
              CONTINUE.
            ENDIF.
          ELSE."Prio 2
            IF <ls_mat_doc>-address2 IS NOT INITIAL.
              ASSIGN lt_adrc[ addrnumber = <ls_mat_doc>-address2 ] TO <ls_adrc>.
              IF sy-subrc = 0 AND <ls_adrc>-country IS NOT INITIAL.
                <ls_mat_doc>-ship_to_country = <ls_adrc>-country.
                <ls_mat_doc>-ship_to_region = <ls_adrc>-region.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
          IF <ls_mat_doc>-customer_po IS NOT INITIAL."Prio 3
            ASSIGN me->mt_kna1[ kunnr = <ls_mat_doc>-customer_po ] TO FIELD-SYMBOL(<ls_kna1>).
            IF sy-subrc = 0 AND <ls_kna1>-land1 IS NOT INITIAL.
              <ls_mat_doc>-ship_to_country = <ls_kna1>-land1.
              <ls_mat_doc>-ship_to_region = <ls_kna1>-regio.
              CONTINUE.
            ENDIF.
          ENDIF.
          IF <ls_mat_doc>-suppl_vendor IS NOT INITIAL."Prio 4
            ASSIGN lt_lfa1[ lifnr = <ls_mat_doc>-suppl_vendor ] TO FIELD-SYMBOL(<ls_lfa1>).
            IF sy-subrc = 0 AND <ls_lfa1>-land1 IS NOT INITIAL.
              <ls_mat_doc>-ship_to_country = <ls_lfa1>-land1.
              <ls_mat_doc>-ship_to_region = <ls_lfa1>-regio.
              CONTINUE.
            ENDIF.
          ENDIF.
          IF <ls_mat_doc>-lgort IS NOT INITIAL."Prio 5
            lv_adrnr_to = VALUE #( me->mt_twlad[ lgort = <ls_mat_doc>-lgort werks = <ls_mat_doc>-plant ]-adrnr OPTIONAL ).
            IF lv_adrnr_to IS NOT INITIAL.
              ASSIGN lt_adrc[ addrnumber = lv_adrnr_to ] TO <ls_adrc>.
              IF sy-subrc = 0 AND <ls_adrc>-country IS NOT INITIAL.
                <ls_mat_doc>-ship_to_country = <ls_adrc>-country.
                <ls_mat_doc>-ship_to_region = <ls_adrc>-region.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
          IF <ls_mat_doc>-plant IS NOT INITIAL."Prio 6
            <ls_mat_doc>-ship_to_country = <ls_mat_doc>-plant_country.
            <ls_mat_doc>-ship_to_region = <ls_mat_doc>-plant_region.
          ENDIF.

*GI with Stock Transfer exported from Specific country
*GI with Stock Transfer reference in specific country (domestic)
        WHEN me->sc_variants-gi_st_exp OR me->sc_variants-gi_st.
          IF <ls_mat_doc>-lgort IS NOT INITIAL."Prio 1
            lv_adrnr_from = VALUE #( me->mt_twlad[ werks = <ls_mat_doc>-plant lgort = <ls_mat_doc>-lgort ]-adrnr OPTIONAL ).
            IF lv_adrnr_from IS INITIAL.
              <ls_mat_doc>-ship_from_country = <ls_mat_doc>-plant_country.
            ELSE.
              <ls_mat_doc>-ship_from_country = VALUE #( lt_adrc[ addrnumber = lv_adrnr_from ]-country OPTIONAL ).
              IF <ls_mat_doc>-ship_from_country IS INITIAL.
                <ls_mat_doc>-ship_from_country = <ls_mat_doc>-plant_country.
              ENDIF.
            ENDIF.
          ELSEIF <ls_mat_doc>-plant IS NOT INITIAL."Prio 2
            <ls_mat_doc>-ship_from_country = <ls_mat_doc>-plant_country.
          ENDIF.

          IF <ls_mat_doc>-storage_location IS NOT INITIAL."Prio 1
            lv_adrnr_to = VALUE #( me->mt_twlad[ werks = <ls_mat_doc>-issuing_or_receiving_plant lgort = <ls_mat_doc>-storage_location ]-adrnr OPTIONAL ).
            IF lv_adrnr_to IS NOT INITIAL.
              ASSIGN lt_adrc[ addrnumber = lv_adrnr_to ] TO <ls_adrc>.
              IF sy-subrc = 0 AND <ls_adrc>-country IS NOT INITIAL.
                <ls_mat_doc>-ship_to_country = <ls_adrc>-country.
                <ls_mat_doc>-ship_to_region = <ls_adrc>-region.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.

          IF <ls_mat_doc>-issuing_or_receiving_plant IS NOT INITIAL."Prio 2
            <ls_mat_doc>-ship_to_country = <ls_mat_doc>-issuing_or_receiving_plnt_cntr.
            <ls_mat_doc>-ship_to_region = <ls_mat_doc>-issuing_or_receiving_plnt_reg.
          ENDIF.

*GR with Stock Transfer reference imported in specific country
*GR with Stock Transfer reference in specific country (domestic)
        WHEN me->sc_variants-gr_st_imp OR me->sc_variants-gr_st.
          IF <ls_mat_doc>-goods_movement_type = '301' OR <ls_mat_doc>-goods_movement_type = '302'.
            IF <ls_mat_doc>-storage_location IS NOT INITIAL.
              lv_adrnr_from = VALUE #( me->mt_twlad[ werks = <ls_mat_doc>-issuing_or_receiving_plant lgort = <ls_mat_doc>-storage_location ]-adrnr OPTIONAL )."Prio 1
              IF lv_adrnr_from IS NOT INITIAL.
                <ls_mat_doc>-ship_from_country = VALUE #( lt_adrc[ addrnumber = lv_adrnr_from ]-country OPTIONAL ).
              ENDIF.
              IF <ls_mat_doc>-ship_from_country IS INITIAL.
                <ls_mat_doc>-ship_from_country = VALUE #( me->mt_t001w[ werks = <ls_mat_doc>-issuing_or_receiving_plant ]-land1 OPTIONAL )." Prio 2
              ENDIF.
            ELSE.
              <ls_mat_doc>-ship_from_country = VALUE #( me->mt_t001w[ werks = <ls_mat_doc>-issuing_or_receiving_plant ]-land1 OPTIONAL )." Prio 2
            ENDIF.
          ELSE.
            "New Prio
            ASSIGN lt_ekko[ ebeln = <ls_mat_doc>-purchase_order ebelp = <ls_mat_doc>-ebelp ] TO FIELD-SYMBOL(<ls_ekko>).
            IF sy-subrc = 0.
              <ls_mat_doc>-ship_from_country = VALUE #( me->mt_t001w[ werks = <ls_ekko>-reswk ]-land1 OPTIONAL ).
            ELSE.
              IF <ls_mat_doc>-purchase_order IS NOT INITIAL .
                ASSIGN lt_mseg[ ebeln = <ls_mat_doc>-purchase_order ] TO FIELD-SYMBOL(<ls_mseg>).
              ELSE.
                ASSIGN lt_mseg[ sgtxt = <ls_mat_doc>-text_id ] TO <ls_mseg>.
              ENDIF.
              IF sy-subrc = 0.
                IF <ls_mseg>-lgort IS INITIAL "Prio 2
                      OR REDUCE i( INIT lv_count = 0 FOR <ls_line> IN lt_mseg WHERE ( ebeln = <ls_mat_doc>-purchase_order )
                                                                              NEXT lv_count = lv_count + 1 ) > 1.

                  <ls_mat_doc>-ship_from_country = VALUE #( me->mt_t001w[ werks = <ls_mseg>-werks ]-land1 OPTIONAL ).
                ELSE.
                  lv_adrnr_from = VALUE #( me->mt_twlad[ werks = <ls_mseg>-werks lgort = <ls_mseg>-lgort ]-adrnr OPTIONAL )."Prio 1
                  IF lv_adrnr_from IS NOT INITIAL.
                    <ls_mat_doc>-ship_from_country = VALUE #( lt_adrc[ addrnumber = lv_adrnr_from ]-country OPTIONAL ).
                  ENDIF.
                  IF <ls_mat_doc>-ship_from_country IS INITIAL.
                    <ls_mat_doc>-ship_from_country = VALUE #( me->mt_t001w[ werks = <ls_mseg>-werks ]-land1 OPTIONAL ).
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          IF <ls_mat_doc>-lgort IS NOT INITIAL."Prio 1
            lv_adrnr_to = VALUE #( me->mt_twlad[ werks = <ls_mat_doc>-plant lgort = <ls_mat_doc>-lgort ]-adrnr OPTIONAL ).
            IF lv_adrnr_to IS NOT INITIAL.
              ASSIGN lt_adrc[ addrnumber = lv_adrnr_to ] TO <ls_adrc>.
              IF sy-subrc = 0 AND <ls_adrc>-country IS NOT INITIAL.
                <ls_mat_doc>-ship_to_country = <ls_adrc>-country.
                <ls_mat_doc>-ship_to_region = <ls_adrc>-region.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
          <ls_mat_doc>-ship_to_country = <ls_mat_doc>-plant_country.
          <ls_mat_doc>-ship_to_region  = <ls_mat_doc>-plant_region.

*GR with production Order reference in specific country
        WHEN me->sc_variants-gr_prod.

          IF <ls_mat_doc>-lgort IS NOT INITIAL."Prio 1
            lv_adrnr_to = VALUE #( me->mt_twlad[ werks = <ls_mat_doc>-plant lgort = <ls_mat_doc>-lgort ]-adrnr OPTIONAL ).
            IF lv_adrnr_to IS NOT INITIAL.
              ASSIGN lt_adrc[ addrnumber = lv_adrnr_to ] TO <ls_adrc>.
              IF sy-subrc = 0 AND  <ls_adrc>-country IS NOT INITIAL.
                <ls_mat_doc>-ship_to_country = <ls_adrc>-country.
                <ls_mat_doc>-ship_to_region = <ls_adrc>-region.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
          IF <ls_mat_doc>-plant IS NOT INITIAL."Prio 2
            <ls_mat_doc>-ship_to_country = <ls_mat_doc>-plant_country.
            <ls_mat_doc>-ship_to_region = <ls_mat_doc>-plant_region.
          ENDIF.

      ENDCASE.

    ENDLOOP.

    GET BADI lo_badi.
    CALL BADI lo_badi->define_countries
      EXPORTING
        iv_code              = iv_code
      CHANGING
        ct_material_document = ct_material_document.

  ENDMETHOD.


  METHOD define_movement_types.
    DATA: lo_badi          TYPE REF TO /vpcoe/adjust_data_retrieval.

    CLEAR: mt_movement_type, mt_stock_change_cat.

    CASE iv_code.
      WHEN me->sc_variants-gr_st_imp.
        mt_movement_type = VALUE #( ( sign = 'I' option = 'EQ' low = '305' )
                                    ( sign = 'I' option = 'EQ' low = '301' )
                                    ( sign = 'I' option = 'EQ' low = '302' )
                                    ( sign = 'I' option = 'EQ' low = '306' ) ).

      WHEN me->sc_variants-gr_st.
        mt_movement_type = VALUE #( ( sign = 'I' option = 'EQ' low = '305' )
                                    ( sign = 'I' option = 'EQ' low = '301' )
                                    ( sign = 'I' option = 'EQ' low = '302' )
                                    ( sign = 'I' option = 'EQ' low = '306' ) ).

        mt_stock_change_cat = VALUE #( ( sign = 'I' option = 'EQ' low = '305' ) ).

      WHEN me->sc_variants-gi_st.
        mt_movement_type = VALUE #( ( sign = 'I' option = 'EQ' low = '304' )
                                    ( sign = 'I' option = 'EQ' low = '303' )
                                    ( sign = 'I' option = 'EQ' low = '301' )
                                    ( sign = 'I' option = 'EQ' low = '302' ) ).

      WHEN me->sc_variants-gi_st_exp.
        mt_movement_type = VALUE #( ( sign = 'I' option = 'EQ' low = '304' )
                                    ( sign = 'I' option = 'EQ' low = '303' )
                                    ( sign = 'I' option = 'EQ' low = '301' )
                                    ( sign = 'I' option = 'EQ' low = '302' ) ).
    ENDCASE.

    GET BADI lo_badi.
    CALL BADI lo_badi->define_movement_type
      EXPORTING
        iv_code             = iv_code
      CHANGING
        ct_movement_types   = mt_movement_type
        ct_stock_change_cat = mt_stock_change_cat.

  ENDMETHOD.


  METHOD download_excel.

    DATA: lv_sheet_name  TYPE char30,
          lt_mat_doc     TYPE /vpcoe/t_material_xls,
          ls_mat_doc_xls TYPE /vpcoe/str_material_xls,
          lv_value(14)   TYPE c.

    CASE iv_code.
      WHEN sc_variants-gr_prod.
        lv_sheet_name = 'GR Production Order (domestic)'.
      WHEN sc_variants-gi_prod.
        lv_sheet_name = 'GI Production Order (domestic)'.
      WHEN sc_variants-gr_st_imp.
        lv_sheet_name = 'GR Stock Transfer (import)'.
      WHEN sc_variants-gi_st_exp.
        lv_sheet_name = 'GI for Stock Transfer (export)'.
      WHEN sc_variants-gr_po_imp.
        lv_sheet_name = 'GR Purchase Order (import)'.
      WHEN sc_variants-gr_po.
        lv_sheet_name = 'GR Purchase Order (domestic)'.
      WHEN sc_variants-gi_so_exp.
        lv_sheet_name = 'GI Sales Order (export)'.
      WHEN sc_variants-gi_so.
        lv_sheet_name = 'GI Sales Order (domestic)'.
      WHEN sc_variants-gr_st.
        lv_sheet_name = 'GR Stock Transfer (domestic)'.
      WHEN sc_variants-gi_st.
        lv_sheet_name = 'GI Stock Transfer (domestic)'.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    me->get_header_xls( EXPORTING iv_code   = iv_code
                        IMPORTING et_header = DATA(lt_header) ).

    LOOP AT it_mat_doc ASSIGNING FIELD-SYMBOL(<ls_mat_doc>).
      CLEAR lv_value.
      MOVE-CORRESPONDING <ls_mat_doc> TO ls_mat_doc_xls .
      lv_value = <ls_mat_doc>-materialstockchangeqtybaseunit.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = lv_value.

      ls_mat_doc_xls-materialstockchangeqtybaseunit = lv_value.

      APPEND ls_mat_doc_xls TO lt_mat_doc.
    ENDLOOP.
    co_xls_handler->execute( iv_name = CONV #( lv_sheet_name ) ).
    co_xls_handler->fill_data( it_item_tab = lt_mat_doc
                               it_title    = lt_header ).

  ENDMETHOD.


  METHOD get_header_xls.
    DATA: lt_header TYPE /vpcoe/cl_xls_handler=>gty_t_title,
          ls_header TYPE LINE OF /vpcoe/cl_xls_handler=>gty_t_title.

    "create excel header template
    lt_header = VALUE #(
                         ( description      = 'Material Document Key1'
                           internal_name    = 'id1'
                           data_type        = 'CHAR(8)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.KEY1 MSEG.MJAHR'
                           vpcoe_attribute  = 'YEAR'
                           is_key           = abap_true )
                         ( description      = 'Material Document Key2'
                           internal_name    = 'id2'
                           data_type        = 'CHAR(8)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.KEY2 MSEG.ZEILE'
                           vpcoe_attribute  = 'ITEM'
                           is_key           = abap_true )
                         ( description      = 'Material Document Key3'
                           internal_name    = 'id3'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.KEY3 MSEG.MBLNR'
                           vpcoe_attribute  = 'ID'
                           is_key           = abap_true )
                         ( description      = 'Material Document Key4'
                           internal_name    = 'id4'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.KEY4 n/a'
                           vpcoe_attribute  = 'COLOR'
                           is_key           = abap_true )
                         ( description      = 'Material Document Key5'
                           internal_name    = 'id5'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.KEY5 n/a'
                           vpcoe_attribute  = 'COLOR'
                           is_key           = abap_true )
                         ( description      = 'Material Document Key6'
                           internal_name    = 'id6'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.KEY6 n/a'
                           vpcoe_attribute  = 'COLOR'
                           is_key           = abap_true )
                         ( description      = 'Material Document Number'
                           internal_name    = 'number'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.MBLNR MSEG.MBLNR'
                           vpcoe_attribute  = 'ID' )
                         ( description      = 'Material Document Item'
                           internal_name    = 'item'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.ZEILE MSEG.ZEILE'
                           vpcoe_attribute  = 'ITEM' )
                         ( description      = 'Material Document Line'
                           internal_name    = 'line'
                           data_type        = 'CHAR(6)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.LINE_ID MSEG.LINE_ID'
                           vpcoe_attribute  = 'LINE')
                         ( description      = 'Material Document Year'
                           internal_name    = 'year'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.MJAHR MSEG.MJAHR'
                           vpcoe_attribute  = 'YEAR')
                         ( description      = 'Posting Date'
                           internal_name    = 'postingDate'
                           data_type        = 'DATE'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.BUDAT MSEG.BUDAT_MKPF'
                           vpcoe_attribute  = 'POSTING_DATE')
                         ( description      = 'Creation Date'
                           internal_name    = 'creationDate'
                           data_type        = 'DATE'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.CPUDT MSEG.CPUDT_MKPF'
                           vpcoe_attribute  = 'CREATION_DATE')
                         ( description      = 'Creation Time'
                           internal_name    = 'creationTime'
                           data_type        = 'TIME'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.CPUTM MSEG.CPUTM_MKPF'
                           vpcoe_attribute  = 'CREATION_TIME')
                         ( description      = 'Plant'
                           internal_name    = 'plant'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.WERKS MSEG.WERKS'
                           vpcoe_attribute  = 'PLANT')
                         ( description      = 'Plant'
                           internal_name    = 'issuingOrReceivingPlant'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.UMWRK MSEG.UMWRK'
                           vpcoe_attribute  = 'ISSUING_OR_RECEIVING_PLANT')
                         ( description      = 'Stock Identifying Material'
                           internal_name    = 'stockMaterial'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.MATBF MSEG.MATBF'
                           vpcoe_attribute  = 'STOCK_MATERIAL')
                         ( description      = 'Inventory Special Stock Type'
                           internal_name    = 'inventoryStockType'
                           data_type        = 'CHAR(1)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.SOBKZ MSEG.SOBKZ'
                           vpcoe_attribute  = 'INVENTORY_STOCK_TYPE')
                         ( description      = 'Company Code'
                           internal_name    = 'companyCode'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.BUKRS MSEG.BUKRS'
                           vpcoe_attribute  = 'COMPANY_CODE')
                         ( description      = 'Product Base Unit'
                           internal_name    = 'baseUnitOfMeasure'
                           data_type        = 'CHAR(4)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.MEINS MSEG.MEINS'
                           vpcoe_attribute  = 'BASE_UNIT_OF_MEASURE')
                         ( description      = 'Quantity In Base Unit'
                           internal_name    = 'quantityInBaseUnit'
                           data_type        = 'QUAN(13,3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.MENGE MSEG.MENGE'
                           vpcoe_attribute  = 'QUANTITY_IN_BASE_UNIT')
                         ( description      = 'Product Stock Change Qty in Base Unit'
                           internal_name    = 'materialStockChangeQtyInBaseUnit'
                           data_type        = 'S4: QUAN(31,14) or ECC:CHAR(1)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.SOBKZ MSEG.SOBKZ'
                           vpcoe_attribute  = 'MATERIALSTOCKCHANGEQTYBASEUNIT')
                         ( description      = 'Entry Unit'
                           internal_name    = 'entryUnit'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.ERFME MSEG.ERFME'
                           vpcoe_attribute  = 'ENTRY_UNIT')
                         ( description      = 'Quantity in Entry Unit'
                           internal_name    = 'quantityInEntryUnit'
                           data_type        = 'QUAN(13,3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.ERFMG MSEG.ERFMG'
                           vpcoe_attribute  = 'QUANTITY_IN_ENTRY_UNIT')
                         ( description      = 'Inventory Management Reference Document'
                           internal_name    = 'inventoryManagementReferenceDocument'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.LFBNR MSEG.LFBNR'
                           vpcoe_attribute  = 'INVTRY_MGMT_REFERENCE_DOCUMENT')
                         ( description      = 'Reference Document'
                           internal_name    = 'referenceDocument'
                           data_type        = 'CHAR(16)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.XBLNR MSEG.XBLNR'
                           vpcoe_attribute  = 'REFERENCE_DOCUMENT')
                         ( description      = 'Sales Order'
                           internal_name    = 'salesOrder'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.KDAUF MSEG.KDAUF'
                           vpcoe_attribute  = 'SALES_ORDER')
                         ( description      = 'Purchase Order'
                           internal_name    = 'purchaseOrder'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.EBELN MSEG.EBELN'
                           vpcoe_attribute  = 'PURCHASE_ORDER')
                         ( description      = 'Purchase Order Item'
                           internal_name    = 'purchaseOrderItem'
                           data_type        = 'CHAR(5)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.EBELP MSEG.EBELP'
                           vpcoe_attribute  = 'PURCHASE_ORDER_ITEM')
                         ( description      = 'Production Order'
                           internal_name    = 'productionOrder'
                           data_type        = 'CHAR(12)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.AUFNR MSEG.AUFNR'
                           vpcoe_attribute  = 'PRODUCTION_ORDER')
                         ( description      = 'Delivery Document'
                           internal_name    = 'delivery'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.VBELN_IM MSEG.VBELN_IM'
                           vpcoe_attribute  = 'DELIVERY')
                         ( description      = 'Delivery Document Item'
                           internal_name    = 'deliveryDocumentItem'
                           data_type        = 'CHAR(6)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.VBELP_IM MSEG.VBELP_IM'
                           vpcoe_attribute  = 'DELIVERY_DOCUMENT_ITEM')
                         ( description      = 'Goods Movement is Cancelled'
                           internal_name    = 'isCanceled'
                           data_type        = 'Boolean'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.CANCELLED n/a'
                           vpcoe_attribute  = 'COLOR')
                         ( description      = 'Is Reversal Movement Type'
                           internal_name    = 'isReversalMovementType'
                           data_type        = 'Boolean'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.REVERSAL_MOVEMENT T156.XSTBW'
                           vpcoe_attribute  = 'IS_REVERSAL_MOVEMENT_TYPE')
                         ( description      = 'Goods Movement Type'
                           internal_name    = 'goodsMovementType'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.BWART MSEG.BWART'
                           vpcoe_attribute  = 'GOODS_MOVEMENT_TYPE')
                         ( description      = 'Product'
                           internal_name    = 'product'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory
                           s4hana_attribute = 'MATDOC.MATNR MSEG.MATNR'
                           vpcoe_attribute  = 'PRODUCT')
                         ( description      = 'Issuing or Receiving Product'
                           internal_name    = 'issuingOrReceivingProduct'
                           data_type        = 'CHAR(40)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.UMMAT MSEG.UMMAT'
                           vpcoe_attribute  = 'ISSG_OR_RCVG_PRODUCT_ID')
                         ( description      = 'Batch'
                           internal_name    = 'batch'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.CHARG MSEG.CHARG'
                           vpcoe_attribute  = 'BATCH')
                         ( description      = 'Manufacture Date'
                           internal_name    = 'manufactureDate'
                           data_type        = 'DATE'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.HSDAT MSEG.HSDAT'
                           vpcoe_attribute  = 'MANUFACTURE_DATE')
                         ( description      = 'Issuing or Receiving Batch'
                           internal_name    = 'issuingOrReceivingBatch'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.UMCHA MSEG.UMCHA'
                           vpcoe_attribute  = 'ISSG_OR_RCVG_BATCH_ID')
                         ( description      = 'Supplier'
                           internal_name    = 'supplier'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.LIFNR MSEG.LIFNR'
                           vpcoe_attribute  = 'SUPPLIER')
                           ( description      = 'Goods Supplier'
                           internal_name    = 'goodsSupplier'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.LIFNR MSEG.LIFNR'
                           vpcoe_attribute  = 'GOODS_SUPPLIER')
                         ( description      = 'Customer'
                           internal_name    = 'customer'
                           data_type        = 'CHAR(10)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.KUNNR MSEG.KUNNR'
                           vpcoe_attribute  = 'CUSTOMER')
                         ( description      = 'Goods Recipient Name'
                           internal_name    = 'goodsRecipientName'
                           data_type        = 'CHAR(12)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.WEMPF MSEG.WEMPF'
                           vpcoe_attribute  = 'GOODS_RECIPIENT_NAME')
                         ( description      = 'Goods Movement Ref Doc Type'
                           internal_name    = 'referenceDocumentType'
                           data_type        = 'CHAR(1)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.KZBEW MSEG.KZBEW'
                           vpcoe_attribute  = 'REFERENCE_DOCUMENT_TYPE')
                         ( description      = 'Ship-From Country/Region'
                           internal_name    = 'shipFromCountry'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'COUNTRY'
                           vpcoe_attribute  = 'SHIP_FROM_COUNTRY')
                           ( description      = 'Ship-To Country/Region'
                           internal_name    = 'shipToCountry'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'COUNTRY'
                           vpcoe_attribute  = 'SHIP_TO_COUNTRY')
                           ( description      = 'Ship-To State/Province'
                           internal_name    = 'shipToRegion'
                           data_type        = 'CHAR(3)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'REGION'
                           vpcoe_attribute  = 'SHIP_TO_REGION')
                         ( description      = 'Accounting Document Type'
                           internal_name    = 'accountingDocumentType'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.BLART MSEG.BLART'
                           vpcoe_attribute  = 'ACCOUNTING_DOCUMENT_TYPE')
                         ( description      = 'Inventory Transaction Type'
                           internal_name    = 'transactionType '
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'MATDOC.VGART MSEG.VGART_MKPF'
                           vpcoe_attribute  = 'TRANSACTION_TYPE')
                         ( description      = 'stock Change Category'
                           internal_name    = 'stockChangeCategory'
                           data_type        = 'CHAR(2)'
                           mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                           s4hana_attribute = 'n/a n/a'
                           vpcoe_attribute  = 'STOCK_CHANGE_CATEGORY') ).

    CASE iv_code.
      WHEN sc_variants-gr_prod. "GR Production Order (domestic)

        DELETE lt_header WHERE description = 'Ship-From Country/Region'.

      WHEN sc_variants-gr_st_imp OR sc_variants-gi_st_exp."GR Stock Transfer (import)

        ls_header = VALUE #(  description      = 'Incoterms'
                              internal_name    = 'incoterms'
                              data_type        = 'CHAR(3)'
                              mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                              s4hana_attribute = 'EKPO.INCO1 EKPO.INCO1'
                              vpcoe_attribute  = 'INCOTERMS'  ).

        APPEND ls_header TO lt_header.

        lt_header[ internal_name = 'issuingOrReceivingPlant' ]-mandatory = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory.

      WHEN sc_variants-gr_po_imp."GR Purchase Order (import)

        ls_header = VALUE #(  description      = 'Incoterms'
                      internal_name    = 'incoterms'
                      data_type        = 'CHAR(3)'
                      mandatory        = /vpcoe/cl_xls_handler=>sc_mandatory-optional
                      s4hana_attribute = 'EKPO.INCO1 EKPO.INCO1'
                      vpcoe_attribute  = 'INCOTERMS'  ).

        APPEND ls_header TO lt_header.

        lt_header[ internal_name = 'supplier' ]-mandatory = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory.

      WHEN sc_variants-gr_po."GR Purchase Order (domestic)

        lt_header[ internal_name = 'supplier' ]-mandatory = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory.

      WHEN sc_variants-gr_st OR sc_variants-gi_st."GR Stock Transfer(domestic)

        lt_header[ internal_name = 'issuingOrReceivingPlant' ]-mandatory = /vpcoe/cl_xls_handler=>sc_mandatory-mandatory.

    ENDCASE.

    et_header = lt_header.

  ENDMETHOD.


  METHOD get_material_document.
    TYPES: BEGIN OF ty_s_bkpf,
             belnr                    TYPE bkpf-belnr,
             accounting_document_type TYPE bkpf-blart,
           END OF ty_s_bkpf.

    DATA lo_badi TYPE REF TO /vpcoe/adjust_data_retrieval.

    DATA: lv_skip                   TYPE abap_bool,
          lt_r_session_ids          TYPE /vpcoe/cl_rdp_payload_handler=>gty_r_sesion_id,
          lt_r_matnr                TYPE RANGE OF matnr,
          lt_material_document      TYPE /vpcoe/cl_rdp_material_doc=>gty_t_material_data,
          lt_material_document_pack TYPE /vpcoe/cl_rdp_material_doc=>gty_t_material_data,
          lv_step_counter           TYPE int4,
          lv_total_lines            TYPE int4,
          lt_session_id             TYPE /vpcoe/cl_rdp_payload_handler=>gty_r_sesion_id,
          lt_bkpf                   TYPE SORTED TABLE OF ty_s_bkpf WITH NON-UNIQUE KEY belnr.

    CLEAR: ev_failed,
           et_material_document,
           et_supinvi,
           et_billdi.

    ev_no_data = abap_true.

    me->mo_log = io_log.
    me->mv_mode = iv_mode.

    DATA(lv_max_count) = /vpcoe/cl_rdp_helper=>get_max_lines_count( ).

    GET BADI lo_badi.
    CALL BADI lo_badi->skip_selection
      EXPORTING
        iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
        iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
        iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc
        iv_level    = /vpcoe/cl_rdp_helper=>sc_level-hdr
        is_sel_opt  = is_sel_opt
        iv_code     = iv_code
        iv_mode     = me->mv_mode
        io_log      = me->mo_log
      CHANGING
        cv_skip     = lv_skip.

    me->mv_session_id = io_cust->generate_session_id( ).
    INSERT VALUE #( sign = 'I' option = 'EQ' low = me->mv_session_id ) INTO TABLE lt_r_session_ids.

    DATA(lv_where_for_variant) = me->build_where_for_variant( is_sel_opt = is_sel_opt
                                                              iv_code    = iv_code ).

    ASSIGN ct_total_count[ sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-material_doc ] TO FIELD-SYMBOL(<ls_sum>).
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO  ct_total_count ASSIGNING <ls_sum>.
      <ls_sum>-sub_object = /vpcoe/cl_rdp_log=>sc_log_sub_obj-material_doc.
    ENDIF.

    IF iv_mode = /vpcoe/cl_rdp_helper=>sc_mode-document.
      lv_max_count = '2147483647'.
    ENDIF.

    MESSAGE i000(/vpcoe/common) WITH `Start of` iv_code `data selection` INTO me->mo_log->sv_msg_text.
    me->mo_log->add_msg_progress( EXPORTING iv_msg_text = me->mo_log->sv_msg_text ).

    IF lv_skip = abap_false.

      DATA(lv_db_connection) = /vpcoe/cl_common_helper=>get_db_connection(
                                                          iv_api_type = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
                                                          iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
                                                          iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc
                                                          iv_level    = /vpcoe/cl_rdp_helper=>sc_level-hdr
                                                          is_sel_opt  = is_sel_opt ).

*    Prepare filter by Product type and SalesOrg.
      IF is_sel_opt-mtart IS NOT INITIAL OR is_sel_opt-vkorg IS NOT INITIAL.
        SELECT DISTINCT mara~matnr AS low,
                        'I'        AS sign,
                        'EQ'       AS option
          FROM mara INNER JOIN mvke ON mara~matnr = mvke~matnr
            INTO CORRESPONDING FIELDS OF TABLE @lt_r_matnr
            WHERE mara~mtart IN @is_sel_opt-mtart
              AND mvke~vkorg IN @is_sel_opt-vkorg.
        IF sy-subrc <> 0.
          INSERT VALUE #( sign = 'E' option = 'CP' low = '*' ) INTO TABLE lt_r_matnr.
        ENDIF.
      ENDIF.

      SELECT COUNT( * )
         INTO @lv_total_lines
         CONNECTION (lv_db_connection)
          FROM mseg AS ms1
            LEFT JOIN t156  ON ms1~bwart = t156~bwart
            LEFT JOIN ekpo  ON ms1~ebeln = ekpo~ebeln AND ms1~ebelp = ekpo~ebelp
            LEFT JOIN ekko  ON ms1~ebeln = ekko~ebeln
               WHERE    ms1~mblnr      IN @is_sel_opt-matdocid
                    AND ms1~mjahr      IN @is_sel_opt-material_doc_year
                    AND ms1~werks      IN @me->mt_r_werks
                    AND ms1~budat_mkpf IN @is_sel_opt-posting_date
                    AND ms1~bwart      IN @is_sel_opt-movement_type
                    AND ms1~matnr      IN @is_sel_opt-matnr
                    AND (lv_where_for_variant).
      IF sy-subrc <> 0.
        CLEAR lv_total_lines.
      ENDIF.

      SELECT DISTINCT
            ms1~mblnr      AS id,
            ms1~zeile      AS item,
            ms1~line_id    AS line,
            ms1~mjahr      AS year,
            ms1~budat_mkpf AS posting_date,
            ms1~cpudt_mkpf AS creation_date,
            ms1~cputm_mkpf AS creation_time,
            ms1~werks      AS plant,
            ms1~lgort      AS lgort,"Stor.Loc
            ms1~umwrk      AS issuing_or_receiving_plant,
            ms1~matbf      AS stock_material,
            ms1~sobkz      AS inventory_stock_type,
            ms1~bukrs      AS company_code,
            ms1~meins      AS base_unit_of_measure,
            ms1~menge      AS quantity_in_base_unit,
            CASE WHEN ms1~shkzg = 'H' THEN - ms1~menge
                                      ELSE ms1~menge
                       END AS materialstockchangeqtybaseunit,
            ms1~erfme      AS entry_unit,
            ms1~erfmg      AS quantity_in_entry_unit,
            ms1~lfbnr      AS invtry_mgmt_reference_document,
            ms1~xblnr_mkpf AS reference_document,
            ms1~kdauf      AS sales_order,
            ms1~ebeln      AS purchase_order,
            ms1~aufnr      AS production_order,
            ms1~ebelp      AS purchase_order_item,
            ms1~vbeln_im   AS delivery,
            ms1~vbelp_im   AS delivery_document_item,
            ms1~bwart      AS goods_movement_type,
            ms1~matnr      AS product,
            ms1~ummat      AS issg_or_rcvg_product_id,
            ms1~charg      AS batch,
            ms1~hsdat      AS manufacture_date,
            ms1~umcha      AS issg_or_rcvg_batch_id,
            ms1~lifnr      AS supplier,
            ms1~kunnr      AS customer,
            ms1~wempf      AS goods_recipient_name,
            ms1~kzbew      AS reference_document_type,
            ms1~vgart_mkpf AS transaction_type,
            ms1~umlgo      AS storage_location,
            ms1~smbln      AS cancellation_id,
            ms1~smblp      AS mat_doc_item,
            ms1~shkzg      AS deb_cred_ind,
            ms1~sgtxt      AS text_id,
             CASE
              WHEN  ms1~shkzg = 'S' AND t156~xstbw = ' ' AND t156~kzdru <> ' '
                 OR ms1~shkzg = 'S' AND t156~xstbw = 'X' AND t156~kzdru = '1'
                 OR ms1~shkzg = 'H' AND t156~xstbw = 'X' AND t156~kzdru <> ' '
                 OR ms1~shkzg = 'H' AND t156~xstbw = ' ' AND t156~kzdru = '2' THEN 'GR'
              WHEN  ms1~shkzg = 'S' AND t156~xstbw = ' ' AND t156~kzdru = ' '
                 OR ms1~shkzg = 'S' AND t156~xstbw = 'X' AND t156~kzdru <> '1'
                 OR ms1~shkzg = 'H' AND t156~xstbw = 'X' AND t156~kzdru = ' '
                 OR ms1~shkzg = 'H' AND t156~xstbw = ' ' AND t156~kzdru <> '2' THEN 'GI'
                       END AS stock_change_category,
            CASE
              WHEN ms1~umwrk <> ' ' AND ms1~umwrk <> ms1~werks THEN 'T'
                                                               ELSE 'F'
                       END AS is_cross_plant_transfer,
            CASE WHEN t156~xstbw = 'X' THEN 'X'
                                       ELSE '-'
                       END AS is_reversal_movement_type,
            ekpo~ebelp     AS ebelp,
            ekpo~emlif     AS suppl_vendor,
            ekpo~adrnr     AS address,
            ekpo~adrn2     AS address2,
            ekpo~pstyp     AS category,
            ekpo~kunnr     AS customer_po,
            ekko~lifnr     AS vendor_acc_number,
            ekko~llief     AS goods_supplier,
            CASE
              WHEN ekpo~inco1 <> ' ' THEN ekpo~inco1 ELSE ekko~inco1 END AS incoterms,
            ekko~reswk AS supplying_or_issuing_plant,
            @iv_code   AS logistic_process
         INTO CORRESPONDING FIELDS OF TABLE @lt_material_document
          CONNECTION (lv_db_connection)
          PACKAGE SIZE @lv_max_count
          FROM mseg AS ms1
            LEFT JOIN t156  ON ms1~bwart = t156~bwart
            LEFT JOIN ekpo  ON ms1~ebeln = ekpo~ebeln AND ms1~ebelp = ekpo~ebelp
            LEFT JOIN ekko  ON ms1~ebeln = ekko~ebeln
               WHERE    ms1~mblnr      IN @is_sel_opt-matdocid
                    AND ms1~mjahr      IN @is_sel_opt-material_doc_year
                    AND ms1~werks      IN @me->mt_r_werks
                    AND ms1~budat_mkpf IN @is_sel_opt-posting_date
                    AND ms1~bwart      IN @is_sel_opt-movement_type
                    AND ms1~matnr      IN @is_sel_opt-matnr
                    AND (lv_where_for_variant).

        " Apply filtet by Product type and SalesOrg.
        IF is_sel_opt-mtart IS NOT INITIAL OR is_sel_opt-vkorg IS NOT INITIAL.
          DELETE lt_material_document WHERE product NOT IN lt_r_matnr.
          IF lt_material_document IS INITIAL.
            CONTINUE.
          ENDIF.
        ENDIF.

        " Get BKPF Data
        SELECT DISTINCT bkpf~belnr AS belnr
                        bkpf~blart AS accounting_document_type
          INTO CORRESPONDING FIELDS OF TABLE lt_bkpf
           FROM bkpf
             FOR ALL ENTRIES IN lt_material_document
              WHERE bkpf~belnr = lt_material_document-id.

        LOOP AT lt_material_document ASSIGNING FIELD-SYMBOL(<ls_mat_doc>).
          <ls_mat_doc>-accounting_document_type = VALUE #( lt_bkpf[ belnr = <ls_mat_doc>-id ]-accounting_document_type OPTIONAL ).
        ENDLOOP.

        ADD 1 TO lv_step_counter.

        DATA(lv_progress) = COND #( WHEN lv_max_count >= lv_total_lines
                                        THEN 100
                                        ELSE ( lv_max_count * lv_step_counter * 100 ) DIV lv_total_lines ).

        me->mo_log->add_msg_progress( EXPORTING iv_level    = CONV #( iv_code )
                                                iv_progress = lv_progress ).

        me->cancellation_check( CHANGING ct_material_document = lt_material_document ).

        me->adjust_header(
          EXPORTING
            is_sel_opt           = is_sel_opt
            iv_code              = iv_code
          CHANGING
            ct_material_document = lt_material_document ).

        IF lt_material_document IS NOT INITIAL.
          ev_no_data = abap_false.
        ENDIF.

        IF iv_mode <> /vpcoe/cl_rdp_helper=>sc_mode-send.
          et_material_document = CORRESPONDING #( BASE ( et_material_document ) lt_material_document ).
          IF lines( et_material_document ) >= /vpcoe/cl_rdp_helper=>gc_display_amount.
            RETURN.
          ENDIF.
        ELSE.

          IF iv_send_mat_doc = abap_true.
            LOOP AT lt_material_document ASSIGNING FIELD-SYMBOL(<ls_material_document>).
              INSERT <ls_material_document> INTO TABLE lt_material_document_pack.
              IF lines( lt_material_document_pack ) = me->mv_package_size.
                me->store_json(
                   EXPORTING
                     it_material_document = lt_material_document_pack
                     io_cust              = io_cust ).

                CLEAR lt_material_document_pack.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

        IF is_sel_opt-get_billdi = abap_true OR is_sel_opt-get_supinvi = abap_true.
          me->handle_billdi_supinvi( EXPORTING it_mat_doc = lt_material_document
                                               is_sel_opt = is_sel_opt
                                     RECEIVING rt_sesion_id = lt_session_id ).
        ENDIF.

      ENDSELECT.

      IF lt_material_document_pack IS NOT INITIAL.
        IF iv_send_mat_doc = abap_true.
          me->store_json(
             EXPORTING
               it_material_document = lt_material_document_pack
               io_cust              = io_cust ).
        ENDIF.
        CLEAR lt_material_document_pack.
      ENDIF.

    ELSE.

      CALL BADI lo_badi->adjust_data_retrieval
        EXPORTING
          iv_api_type   = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
          iv_srv_grp    = /vpcoe/cl_rdp_helper=>sc_grp_id-material_doc
          iv_srv_id     = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc
          iv_level      = /vpcoe/cl_rdp_helper=>sc_level-hdr
          is_sel_opt    = is_sel_opt
          iv_mode       = me->mv_mode
          io_log        = me->mo_log
          iv_session_id = me->mv_session_id
          iv_code       = iv_code
        CHANGING
          ct_data       = lt_material_document.

      IF iv_mode <> /vpcoe/cl_rdp_helper=>sc_mode-send.
        LOOP AT lt_material_document ASSIGNING <ls_mat_doc>.
          INSERT CORRESPONDING #( <ls_mat_doc> ) INTO TABLE et_material_document.
          IF lines( et_material_document ) > /vpcoe/cl_rdp_helper=>gc_display_amount.
            EXIT.
          ENDIF.
        ENDLOOP.
        RETURN.
      ENDIF.

      IF lt_material_document IS NOT INITIAL.

        IF is_sel_opt-get_billdi = abap_true OR is_sel_opt-get_supinvi = abap_true.
          me->handle_billdi_supinvi( EXPORTING it_mat_doc = lt_material_document
                                               is_sel_opt = is_sel_opt
                                     RECEIVING rt_sesion_id = lt_session_id ).
        ENDIF.

        IF iv_send_mat_doc = abap_true.
          LOOP AT lt_material_document INTO DATA(lgr_material_document) GROUP BY ( sy-tabix - 1 ) DIV me->mv_package_size + 1.
            CLEAR lt_material_document_pack.
            LOOP AT GROUP lgr_material_document ASSIGNING <ls_material_document>.
              INSERT <ls_material_document> INTO TABLE lt_material_document_pack.
            ENDLOOP.
            me->store_json(
               EXPORTING
                 it_material_document = lt_material_document_pack
                 io_cust              = io_cust ).

          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

    IF iv_send = abap_true AND iv_send_mat_doc = abap_true.
      NEW /vpcoe/cl_rdp_payload_handler( )->send_payload_multi(
        EXPORTING
          it_r_session_ids = lt_r_session_ids
          iv_api_type      = /vpcoe/cl_rdp_helper=>sc_api_type-rdp
          iv_srv_id        = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc
          io_log           = me->mo_log
          io_cust          = io_cust
        CHANGING
          cv_total         = <ls_sum>-total
          cv_total_failed  = <ls_sum>-total_failed ).
    ENDIF.

*    Send Billing Document Items
    IF is_sel_opt-get_billdi = abap_true AND ( iv_code = /vpcoe/cl_rdp_material_doc=>sc_variants-gr_st_imp OR iv_code = /vpcoe/cl_rdp_material_doc=>sc_variants-gi_st_exp ).
      MESSAGE i000(/vpcoe/common) WITH `Start to handle BillingDocumentItems` INTO me->mo_log->sv_msg_text.
      me->mo_log->add_msg_progress( EXPORTING iv_msg_text = me->mo_log->sv_msg_text ).

      NEW /vpcoe/cl_rdp_billdocit_data( iv_mode = iv_mode )->process_billing_doc_items(
        EXPORTING
          io_log          = io_log_billdoc
          iv_service_id   = /vpcoe/cl_rdp_helper=>sc_service_id-material_doc
          iv_code         = iv_code
          iv_send         = iv_send_bil_doc
          is_sel_opt      = VALUE #( bdocdate  = is_sel_opt-bdocdate
                                     bsddoccat = is_sel_opt-bsddoccat
                                     billdoc   = is_sel_opt-billdoc
                                     billdi_country = is_sel_opt-billdi_country
                                     billdoc_session_id = lt_session_id )
        IMPORTING
          et_billdi = et_billdi
        CHANGING
          ct_total_count_billdoc = ct_total_count_billdoc ).
    ENDIF.

*    Send Supplier Invoice Items
    IF is_sel_opt-get_supinvi = abap_true AND ( iv_code = /vpcoe/cl_rdp_material_doc=>sc_variants-gr_po OR iv_code = /vpcoe/cl_rdp_material_doc=>sc_variants-gr_po_imp ).
      MESSAGE i000(/vpcoe/common) WITH `Start to handle SupplierInvoiceItems` INTO me->mo_log->sv_msg_text.
      me->mo_log->add_msg_progress( EXPORTING iv_msg_text = me->mo_log->sv_msg_text ).

*      DATA(go_supp_inv_data) = /vpcoe/cl_rdp_api_instance=>get_instance( )->get_supplier_inv_handler( iv_mode = iv_mode
*                                                                                                      io_rdp_helper = NEW /vpcoe/cl_rdp_helper( iv_api_type = me->mv_api_type
*                                                                                                      iv_srv_grp  = /vpcoe/cl_rdp_helper=>sc_grp_id-supinvi
*                                                                                                      iv_srv_id   = /vpcoe/cl_rdp_helper=>sc_service_id-supinvi ) ).
      NEW /vpcoe/cl_rdp_supinvi_data( iv_mode )->process_supplier_inv_items(
       EXPORTING
         iv_code = iv_code
         io_log = io_log_supp_inv
         iv_send    = iv_send_sup_inv
         is_sel_opt = VALUE #( supplier_invoice      = is_sel_opt-supplier_invoice
                               supplier_invoice_item = is_sel_opt-supplier_invoice_item
                               fiscal_year           = is_sel_opt-fiscal_year
                               supinvi_country       = is_sel_opt-supinvi_country
                               comp_code             = is_sel_opt-comp_code
                               plant                 = is_sel_opt-plant
                               supinvi_session_id    = lt_session_id )
       IMPORTING
         et_supinvi = et_supinvi
       CHANGING
         ct_total_count_suppinv = ct_total_count_suppinv ).
*      NEW /vpcoe/cl_rdp_supinvi_data( iv_mode )->process_supplier_inv_items(
*        EXPORTING
*          iv_code = iv_code
*          io_log = io_log_supp_inv
*          iv_send    = iv_send_sup_inv
*          is_sel_opt = VALUE #( supplier_invoice      = is_sel_opt-supplier_invoice
*                                supplier_invoice_item = is_sel_opt-supplier_invoice_item
*                                fiscal_year           = is_sel_opt-fiscal_year
*                                supinvi_country       = is_sel_opt-supinvi_country
*                                comp_code             = is_sel_opt-comp_code
*                                plant                 = is_sel_opt-plant )
*        IMPORTING
*          et_supinvi = et_supinvi
*        CHANGING
*          ct_total_count_suppinv = ct_total_count_suppinv ).
    ENDIF.

  ENDMETHOD.


  METHOD handle_billdi_supinvi.

    DATA: lt_material_document TYPE SORTED TABLE OF /vpcoe/cl_rdp_material_doc=>gty_s_material_data WITH NON-UNIQUE KEY primary_key COMPONENTS ship_from_country,
          lt_billdi            TYPE SORTED TABLE OF /vpcoe/billdi WITH NON-UNIQUE KEY primary_key COMPONENTS session_id billdi,
          lv_session_id        TYPE raw16.

    lt_material_document = it_mat_doc.

    LOOP AT lt_material_document ASSIGNING FIELD-SYMBOL(<ls_mat_doc>).
      CLEAR lv_session_id.
      lv_session_id = /vpcoe/cl_common_helper=>generate_session_id( ).
      INSERT VALUE #( session_id             = lv_session_id
                      mat_doc                = <ls_mat_doc>-id
                      added_on               = sy-datum
                      code                   = <ls_mat_doc>-logistic_process
                      purchase_order         = <ls_mat_doc>-purchase_order
                      delivery_document_item = <ls_mat_doc>-delivery_document_item
                      delivery_document      = <ls_mat_doc>-delivery
                      purchase_order_item    = <ls_mat_doc>-purchase_order_item
                      ship_to_country        = <ls_mat_doc>-ship_to_country
                      ship_from_country      = <ls_mat_doc>-ship_from_country
                      ) INTO TABLE lt_billdi.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = lv_session_id ) INTO TABLE rt_sesion_id.
    ENDLOOP.

    IF lt_billdi IS NOT INITIAL.
      MODIFY /vpcoe/billdi FROM TABLE lt_billdi.
      CALL FUNCTION 'DB_COMMIT'.
    ENDIF.

  ENDMETHOD.


  METHOD store_json.
    DATA: lt_material_doc_json TYPE /vpcoe/t_material_jsn,
          lt_mapping           TYPE /vpcoe/cl_rdp_helper=>gty_t_name_mappings.

    lt_material_doc_json = CORRESPONDING #( it_material_document ).

    lt_mapping = VALUE #( ( abap = `materialstockchangeqtybaseunit` json = `materialStockChangeQtyInBaseUnit` )
                          ( abap = `invtry_mgmt_reference_document` json = `inventoryManagementReferenceDocument` )
                          ( abap = `issg_or_rcvg_product_id`        json = `issuingOrReceivingProduct` )
                          ( abap = `issg_or_rcvg_batch_id`          json = `issuingOrReceivingBatch` ) ).

    es_json-elements = /vpcoe/cl_rdp_helper=>serialize_json(
                                EXPORTING is_data          = VALUE /vpcoe/str_material_json( source   = me->mv_source
                                                                                             elements = lt_material_doc_json )
                                          it_name_mappings = lt_mapping
                                          iv_pretty_name   = /vpcoe/cl_rdp_helper=>sc_pretty_mode-camel_case ).

    REPLACE ALL OCCURRENCES OF ':""' IN es_json-elements WITH ': null' ##no_text.
    REPLACE ALL OCCURRENCES OF ':"0000-00-00"' IN es_json-elements WITH ': null' ##no_text.
    REPLACE ALL OCCURRENCES OF '"isCrossPlantTransfer":"F"' IN es_json-elements WITH '"isCrossPlantTransfer": false' ##no_text.
    REPLACE ALL OCCURRENCES OF '"isCrossPlantTransfer":"T"' IN es_json-elements WITH '"isCrossPlantTransfer": true' ##no_text.

    es_json-count = lines( lt_material_doc_json ).

    me->mv_session_item = me->mv_session_item + 1.

    CALL FUNCTION '/VPCOE/STORE_JSON_BCKGRND'
      EXPORTING
        iv_api_type     = me->mv_api_type
        iv_srv_grp      = io_cust->get_srv_grp( )
        iv_srv_id       = io_cust->get_srv_id( )
        iv_session_id   = me->mv_session_id
        iv_session_item = me->mv_session_item
        is_json         = es_json.

  ENDMETHOD.
ENDCLASS.
