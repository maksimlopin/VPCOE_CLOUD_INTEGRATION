INTERFACE /vpcoe/if_pckf_entity_proc
  PUBLIC .


  CONSTANTS:
    BEGIN OF gc_customer_role,
      gc_end_consum TYPE string VALUE 'END_CONSUM',
      gc_horeca     TYPE string VALUE 'HORECA',
      gc_hospital   TYPE string VALUE 'HOSPITAL',
      gc_prof_custm TYPE string VALUE 'PROF_CUSTM',
      gc_retailer   TYPE string VALUE 'RETAILER',
      gc_no_role    TYPE string VALUE 'NO_ROLE',
    END OF gc_customer_role .
  CONSTANTS:
    BEGIN OF gc_param_roles,
      rfc_des       TYPE string VALUE 'RFC_DES',
      package_size  TYPE string VALUE 'PACKAGE_SIZE',
      test_mode     TYPE string VALUE 'TEST_MODE',
      customer_role TYPE string VALUE 'CUSTOMER_ROLE',
      cust_role_url TYPE string VALUE 'CUST_ROLE_URL',
    END OF gc_param_roles .
  CONSTANTS:
    BEGIN OF gc_param_exemption,
      rfc_des         TYPE string VALUE 'RFC_DES',
      package_size    TYPE string VALUE 'PACKAGE_SIZE',
      test_mode       TYPE string VALUE 'TEST_MODE',
      customer        TYPE string VALUE 'CUSTOMER',
      cust_exemp_url  TYPE string VALUE 'CUST_EXEMP_URL',
      report_category TYPE string VALUE 'REPORT_CATEGORY',
      load_id         TYPE string VALUE 'INITIAL_LOAD_ID',
    END OF gc_param_exemption.
  CONSTANTS:
    BEGIN OF gc_param_comp_code,
      rfc_des         TYPE string VALUE 'RFC_DES',
      package_size    TYPE string VALUE 'PACKAGE_SIZE',
      test_mode       TYPE string VALUE 'TEST_MODE',
      company_code    TYPE string VALUE 'COMPANY_CODE',
      comp_code_url   TYPE string VALUE 'COMP_CODE_URL',
      report_category TYPE string VALUE 'REPORT_CATEGORY',
    END OF gc_param_comp_code.
  CONSTANTS:
    BEGIN OF gc_upload_mode,
      gc_upload_mode_full  TYPE /vpcoe/upload_mode VALUE 'F',
      gc_upload_mode_delta TYPE /vpcoe/upload_mode VALUE 'D',
    END OF gc_upload_mode .
  CONSTANTS:
    BEGIN OF gc_parameters,
      rfc_des          TYPE string VALUE 'RFC_DES',
      package_size     TYPE string VALUE 'PACKAGE_SIZE',
      test_mode        TYPE string VALUE 'TEST_MODE',
      report_categ_id  TYPE string VALUE 'REPORT_CATEG_ID',
      report_config_id TYPE string VALUE 'REPORT_CONFIG_ID',
      country          TYPE string VALUE 'COUNTRY',
      region           TYPE string VALUE 'REGION',
      ship_parrole     TYPE string VALUE 'SHIP_PARROLE',
      context          TYPE string VALUE 'CONTEXT',
      proc_drctn       TYPE string VALUE 'BUS_PROC_DIR',
      next_link        TYPE string VALUE 'NEXT_LINK',
      delta_link       TYPE string VALUE 'DELTA_LINK',
      cust_depend      TYPE string VALUE 'CUST_DEPEND',
      load_id          TYPE string VALUE 'INITIAL_LOAD_ID',
    END OF gc_parameters .
  CONSTANTS:
    BEGIN OF gc_defaults,
      retrieve_package_size TYPE i VALUE 500,
      posting_package_size  TYPE i VALUE 100,
      failed_retention_days TYPE i VALUE 7,
    END OF gc_defaults .
  CONSTANTS gc_date_bot TYPE datum VALUE '00010101' ##NO_TEXT.

  "! When class is instantiated map the entity type and mode to the attributes.
  METHODS init_processor
    IMPORTING
      !iv_entity_type TYPE /vpcoe/pckf_entity
      !iv_upload_mode TYPE /vpcoe/upload_mode
      !is_parameters  TYPE any OPTIONAL .
  "! Get the current entity.
  METHODS get_entity
    RETURNING
      VALUE(rv_entity) TYPE /vpcoe/pckf_entity .
  "! Get the current retrieval mode.
  METHODS get_mode
    RETURNING
      VALUE(rv_mode) TYPE /vpcoe/upload_mode .
  METHODS prepare_retrieve
    RETURNING
      VALUE(rv_record_cnt) TYPE i .
  METHODS retrieve_package
    IMPORTING
      !iv_act_package       TYPE i OPTIONAL
      !iv_package_size      TYPE i OPTIONAL
    RETURNING
      VALUE(rt_entity_data) TYPE /vpcoe/t_pckf_entity_data .
  "! The transfer step is to post the entity data.
  METHODS transfer_package
    IMPORTING
      !it_entity_data     TYPE /vpcoe/t_pckf_entity_data
    RETURNING
      VALUE(rv_error_flg) TYPE abap_bool .
  "! Get the parameters derived in prepare process.
  METHODS get_parameters
    RETURNING
      VALUE(rv_result) TYPE REF TO data .
  "! Get the parameter value for the given parameter name
  METHODS get_parameter_value
    IMPORTING
      !iv_name  TYPE string
    EXPORTING
      !ev_value TYPE any .
  "! Set the parameter value for the given parameter name
  METHODS set_parameter_value
    IMPORTING
      !iv_name  TYPE string
      !iv_value TYPE any .
  "! This method is used to deserialize the selection screen parameters that is read from protocol table to create
  "! the input structure for further use.
  METHODS deserialize_selection_params
    IMPORTING
      !iv_json_str         TYPE string
    EXPORTING
      !es_selection_params TYPE any .
ENDINTERFACE.
