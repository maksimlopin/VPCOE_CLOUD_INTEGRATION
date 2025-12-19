class /VPCOE/CL_RDP_CLAS_DATA_CREATE definition
  public
  create public .

public section.

  types:
    BEGIN OF mty_s_classification,
        classtype            TYPE bapi_class_key-classtype,
        classnumber          TYPE bapi_class_key-classnum,
        classbasicdata       TYPE bapi1003_basic,
        description          TYPE STANDARD TABLE OF bapi1003_catch WITH NON-UNIQUE DEFAULT KEY,
        classcharacteristics TYPE STANDARD TABLE OF bapi1003_charact WITH NON-UNIQUE DEFAULT KEY,
      END OF mty_s_classification .
  types:
    mty_tty_classifications TYPE STANDARD TABLE OF mty_s_classification .
  types:
    BEGIN OF mty_s_characteristic,
        charactdetail       TYPE bapicharactdetail,
        changenumber        TYPE bapicharactkey-changenum,
        keydate             TYPE bapicharactkey-keydate,
        description         TYPE STANDARD TABLE OF bapicharactdescr WITH NON-UNIQUE DEFAULT KEY,
        num_values          TYPE STANDARD TABLE OF bapicharactvaluesnum WITH NON-UNIQUE DEFAULT KEY,
        char_values         TYPE STANDARD TABLE OF bapicharactvalueschar WITH NON-UNIQUE DEFAULT KEY,
        char_descr          TYPE STANDARD TABLE OF bapicharactvaluesdescr WITH NON-UNIQUE DEFAULT KEY,
        curr_values         TYPE STANDARD TABLE OF bapicharactvaluescurr WITH NON-UNIQUE DEFAULT KEY,
        charactreferences   TYPE bapicharactreferences,
        charactrestrictions TYPE bapicharactrestrictions,
      END OF mty_s_characteristic .
  types:
    mty_tty_characteristics TYPE STANDARD TABLE OF mty_s_characteristic .
  types:
    BEGIN OF mty_s_fail_log_clas,
        class_number TYPE bapi_class_key-classnum,
        reason       TYPE bapi_msg,
      END OF mty_s_fail_log_clas .
  types:
    BEGIN OF mty_s_log_clas,
             class_number TYPE bapi_class_key-classnum,
             msg          TYPE bapi_msg,
           END OF mty_s_log_clas .
  types:
    mty_tty_log_clas TYPE STANDARD TABLE OF mty_s_log_clas .
  types:
    BEGIN OF mty_s_log_char,
             char_name TYPE bapicharactdetail-charact_name,
             msg       TYPE bapi_msg,
           END OF mty_s_log_char .
  types:
    mty_tty_log_char TYPE STANDARD TABLE OF mty_s_log_char .
  types:
    mty_tty_clasnum TYPE STANDARD TABLE OF bapi_class_key-classnum .
  types:
    mty_tty_charnam TYPE STANDARD TABLE OF bapicharactdetail-charact_name .

  class-data GV_SCOPE_SPECDB type STRING value 'specdb' ##NO_TEXT.
  class-data GV_SCOPE_MATCLS type STRING value 'matcls' ##NO_TEXT.
  class-data GV_SCOPE_CUST type STRING value 'cust' ##NO_TEXT.
  constants GC_TCODE type SYST_TCODE value '/VPCOE/DATA_CREATOR' ##NO_TEXT.
  class-data GV_SCOPE_PLST type STRING value 'plst' ##NO_TEXT.
  class-data GV_SCOPE_MAIN_AND_SUBORDINATE type STRING value 'bom' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IV_CLASSI_PREFIX type CHAR4
      !IV_CHARAC_PREFIX type CHAR4
      !IV_SCOPE type STRING
      !IV_TEST_RUN type TESTRUN default ABAP_FALSE .
  methods CHECK_EXISTING_CLASSI_CHARACT .
  methods GET_CREATED_DATA
    exporting
      !ET_CREATED_CLAS type MTY_TTY_CLASNUM
      !ET_CREATED_CHAR type MTY_TTY_CHARNAM .
  methods CREATE_OR_UPDATE_CHAR_AND_CLAS .
  methods GET_SUCCESS_UPDATE_OR_CREATE
    exporting
      !ET_CREATED_CLAS type MTY_TTY_LOG_CLAS
      !ET_UPDATED_CLAS type MTY_TTY_LOG_CLAS
      !ET_CREATED_CHAR type MTY_TTY_LOG_CHAR
      !ET_UPDATED_CHAR type MTY_TTY_LOG_CHAR .
  methods GET_FAILS_ON_UPDATE_OR_CREATE
    exporting
      !ET_FAIL_CREATE_CLAS type MTY_TTY_LOG_CLAS
      !ET_FAIL_CREATE_CHAR type MTY_TTY_LOG_CHAR
      !ET_FAIL_UPDATE_CLAS type MTY_TTY_LOG_CLAS
      !ET_FAIL_UPDATE_CHAR type MTY_TTY_LOG_CHAR .
  PROTECTED SECTION.

private section.

  data MT_CLASSI_TO_CREATE type MTY_TTY_CLASSIFICATIONS .
  data MT_CHARAC_TO_CREATE type MTY_TTY_CHARACTERISTICS .
  data MT_CLASSI_TO_UPDATE type MTY_TTY_CLASSIFICATIONS .
  data MT_CHARAC_TO_UPDATE type MTY_TTY_CHARACTERISTICS .
  data MT_CREATED_CLAS type MTY_TTY_LOG_CLAS .
  data MT_CREATED_CHAR type MTY_TTY_LOG_CHAR .
  data MT_UPDATED_CLAS type MTY_TTY_LOG_CLAS .
  data MT_UPDATED_CHAR type MTY_TTY_LOG_CHAR .
  data MT_FAIL_CREATE_CLAS type MTY_TTY_LOG_CLAS .
  data MT_FAIL_CREATE_CHAR type MTY_TTY_LOG_CHAR .
  data MT_FAIL_UPDATE_CLAS type MTY_TTY_LOG_CLAS .
  data MT_FAIL_UPDATE_CHAR type MTY_TTY_LOG_CHAR .

  methods SETUP_CUST_EXT
    importing
      !IV_CHARAC_PREFIX type CHAR4
      !IV_CLASSI_PREFIX type CHAR4
      !IV_SCOPE type STRING .
  methods SETUP_ES_PLASTICTAX
    importing
      !IV_CHARAC_PREFIX type CHAR4
      !IV_CLASSI_PREFIX type CHAR4
      !IV_SCOPE type STRING .
  methods SETUP_PACKELEM_FRAC_DATASET
    importing
      !IV_CHARAC_PREFIX type CHAR4
      !IV_CLASSI_PREFIX type CHAR4
      !IV_SCOPE type STRING .
  methods SETUP_PACKELEM_ATTR_DATASET
    importing
      !IV_CHARAC_PREFIX type CHAR4
      !IV_CLASSI_PREFIX type CHAR4
      !IV_SCOPE type STRING .
  methods SETUP_DATASET
    importing
      !IV_CLASSI_PREFIX type CHAR4
      !IV_CHARAC_PREFIX type CHAR4
      !IV_SCOPE type STRING .
  methods CREATE_CLASSIFICATION_DATA .
  methods CREATE_CHARACTERISTIC_DATA .
  methods SETUP_PACKELEM_MAIN_AND_SUBORD
    importing
      !IV_CHARAC_PREFIX type CHAR4
      !IV_CLASSI_PREFIX type CHAR4
      !IV_SCOPE type STRING .
  methods UPDATE_CLASSIFICATION_DATA .
  methods UPDATE_CHARACTERISTIC_DATA .
  methods COMMIT_TRANSACTION .
  methods ROLLBACK_TRANSACTION .
  methods CLASSI_EXISTENCE_CHECK
    importing
      !IV_CLASSTYPE type BAPI_CLASS_KEY-CLASSTYPE
      !IV_CLASSNUM type BAPI_CLASS_KEY-CLASSNUM
    returning
      value(RV_EXIST) type ABAP_BOOL .
  methods CHARACT_EXISTENCE_CHECK
    importing
      !IV_CHARACNAME type BAPICHARACTKEY-CHARACTNAME
    returning
      value(RV_EXIST) type ABAP_BOOL .
ENDCLASS.



CLASS /VPCOE/CL_RDP_CLAS_DATA_CREATE IMPLEMENTATION.


  METHOD charact_existence_check.

    DATA lt_bapireturn TYPE TABLE OF bapiret2.

    CALL FUNCTION 'BAPI_CHARACT_EXISTENCECHECK'
      EXPORTING
        charactname = iv_characname
      TABLES
        return      = lt_bapireturn.

    READ TABLE lt_bapireturn INDEX 1 REFERENCE INTO DATA(lr_bapireturn).

    IF lr_bapireturn->type = 'S'.
      rv_exist = abap_true.
    ELSE.
      rv_exist = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD check_existing_classi_charact.

      LOOP AT mt_classi_to_create REFERENCE INTO DATA(lr_classi_param).

      IF classi_existence_check( iv_classnum  = lr_classi_param->classnumber
                                 iv_classtype = lr_classi_param->classtype ).
        APPEND lr_classi_param->* TO mt_classi_to_update.
        DELETE mt_classi_to_create.
      ENDIF.

    ENDLOOP.

    LOOP AT mt_charac_to_create REFERENCE INTO DATA(lr_charac_param).

      IF charact_existence_check( iv_characname = lr_charac_param->charactdetail-charact_name ).
        APPEND lr_charac_param->* TO mt_charac_to_update.
        DELETE mt_charac_to_create.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD classi_existence_check.

    DATA lt_bapireturn     TYPE TABLE OF bapiret2.

    CALL FUNCTION 'BAPI_CLASS_EXISTENCECHECK'
      EXPORTING
        classtype = iv_classtype
        classnum  = iv_classnum
      TABLES
        return    = lt_bapireturn.

    READ TABLE lt_bapireturn INDEX 1 REFERENCE INTO DATA(lr_bapireturn).

    IF lr_bapireturn->type = 'S'.
      rv_exist = abap_true.
    ELSE.
      rv_exist = abap_false.
    ENDIF.

  ENDMETHOD.


  method COMMIT_TRANSACTION.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING wait = 'X'.
  endmethod.


  METHOD constructor.
    "mv_test_run = iv_test_run.

    setup_dataset( iv_charac_prefix = iv_charac_prefix
                   iv_classi_prefix = iv_classi_prefix
                   iv_scope         = iv_scope ).

  ENDMETHOD.


  METHOD create_characteristic_data.

    DATA lt_bapireturn TYPE TABLE OF bapiret2.

    LOOP AT mt_charac_to_create REFERENCE INTO DATA(lr_char_to_create).
      CALL FUNCTION 'BAPI_CHARACT_CREATE'
        EXPORTING
          charactdetail      = lr_char_to_create->charactdetail
          "testrun            = mv_test_run
        TABLES
          charactdescr       = lr_char_to_create->description
          charactvaluesnum   = lr_char_to_create->num_values
          charactvalueschar  = lr_char_to_create->char_values
          charactvaluesdescr = lr_char_to_create->char_descr
          charactvaluescurr  = lr_char_to_create->curr_values
          return             = lt_bapireturn.

      DATA(lt_errors) = VALUE string_table( FOR <ls_bapireturn> IN lt_bapireturn WHERE ( type = 'E' ) ( CONV #( <ls_bapireturn>-message ) ) ).
      IF lt_errors IS INITIAL.
        DATa: lv_success TYPE stringval.
        LOOP AT lt_bapireturn INTO DATA(ls_bapireturn) WHERE type <> 'E'.
          CONCATENATE lv_success ls_bapireturn-message INTO lv_success SEPARATED BY ' / '.
        ENDLOOP.
        APPEND VALUE #( char_name = lr_char_to_create->charactdetail-charact_name
                        msg       = lv_success ) TO mt_created_char.
        commit_transaction( ).
      ELSE.
        CONCATENATE LINES OF lt_errors INTO DATA(lv_error) SEPARATED BY ' / '.
        CLEAR lt_errors.
        APPEND VALUE #( char_name = lr_char_to_create->charactdetail-charact_name
                        msg       = lv_error ) TO mt_fail_create_char.
        rollback_transaction( ).
      ENDIF.
      CLEAR lt_bapireturn.

    ENDLOOP.

  ENDMETHOD.


  METHOD create_classification_data.

    DATA lt_bapireturn TYPE TABLE OF bapiret2.

    LOOP AT mt_classi_to_create REFERENCE INTO DATA(lr_clas_to_create).

      CALL FUNCTION 'BAPI_CLASS_CREATE'
        EXPORTING
          classnumnew          = lr_clas_to_create->classnumber
          classtypenew         = lr_clas_to_create->classtype
          classbasicdata       = lr_clas_to_create->classbasicdata
          "testrun              = mv_test_run
        TABLES
          return               = lt_bapireturn
          classdescriptions    = lr_clas_to_create->description
          classcharacteristics = lr_clas_to_create->classcharacteristics.

    DATA(lt_errors) = VALUE string_table( FOR <ls_bapireturn> IN lt_bapireturn WHERE ( type = 'E' ) ( CONV #( <ls_bapireturn>-message ) ) ).
      IF lt_errors IS INITIAL.
        DATa: lv_success TYPE stringval.
        LOOP AT lt_bapireturn INTO DATA(ls_bapireturn) WHERE type <> 'E'.
          CONCATENATE lv_success ls_bapireturn-message INTO lv_success SEPARATED BY ' / '.
        ENDLOOP.
        APPEND VALUE #( class_number = lr_clas_to_create->classnumber
                        msg          = lv_success ) TO mt_created_clas.
        commit_transaction( ).
      ELSE.
        CONCATENATE LINES OF lt_errors INTO DATA(lv_error) SEPARATED BY ' / '.
        CLEAR lt_errors.
        APPEND VALUE #( class_number = lr_clas_to_create->classnumber
                        msg          = lv_error ) TO mt_fail_create_clas.
        rollback_transaction( ).
      ENDIF.
      CLEAR lt_bapireturn.

    ENDLOOP.

  ENDMETHOD.


  method CREATE_OR_UPDATE_CHAR_AND_CLAS.

    IF mt_charac_to_create IS NOT INITIAL.
      create_characteristic_data( ).
    ENDIF.

    IF mt_classi_to_create IS NOT INITIAL.
      create_classification_data( ).
    ENDIF.

    IF mt_classi_to_update IS NOT INITIAL.
        update_classification_data( ).
    ENDIF.

    IF mt_charac_to_update IS NOT INITIAL.
      update_characteristic_data( ).
    ENDIF.

  endmethod.


  METHOD get_created_data.

    et_created_char = mt_created_char.
    et_created_clas = mt_created_clas.

  ENDMETHOD.


  method GET_FAILS_ON_UPDATE_OR_CREATE.
    et_fail_create_char = mt_fail_create_char.
    et_fail_create_clas = mt_fail_create_clas.
    et_fail_update_clas = mt_fail_update_clas.
    et_fail_update_char = mt_fail_update_char.
  endmethod.


  method GET_SUCCESS_UPDATE_OR_CREATE.
    et_created_char = mt_created_char.
    et_created_clas = mt_created_clas.
    et_updated_clas = mt_updated_clas.
    et_updated_char = mt_updated_char.

  endmethod.


  method ROLLBACK_TRANSACTION.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  endmethod.


  METHOD SETUP_CUST_EXT.

    DATA lv_classtype TYPE bapi_class_key-classtype.
    DATA lt_pckattr_charac_params   TYPE mty_tty_characteristics.
    DATA lt_pckattr_classi_params TYPE mty_tty_classifications.
    DATA lt_class_characts_params TYPE STANDARD TABLE OF bapi1003_charact WITH NON-UNIQUE DEFAULT KEY.

    lt_pckattr_charac_params = VALUE #( ( charactdetail-charact_name = iv_charac_prefix && '_CUST_EXT_ROLE'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 10
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = 'END_CONSUM' )
                                                                                 ( value_char = 'HOSPITAL'   )
                                                                                 ( value_char = 'RETAILER'   )
                                                                                 ( value_char = 'PROF_CUSTM' )
                                                                                 ( value_char = 'HORECA'     ) )
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = 'END_CONSUM' description = 'End-consumer' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'HOSPITAL'   description = 'Hospital' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'RETAILER'   description = 'Retailer' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'PROF_CUSTM' description = 'Professional customer' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'HORECA'     description = 'Hotels, restaurants, and cater' ) )

                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Customer Role​' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_CUST_EXT_TAG'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 10
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = 'TAG1' )
                                                                                 ( value_char = 'TAG2' )
                                                                                 ( value_char = 'TAG3' ) )
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = 'TAG1' description = 'Tag 1 (customer-defined)' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'TAG2' description = 'Tag 2 (customer-defined)' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'TAG3' description = 'Tag 3 (customer-defined)' ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Customer Tag​' ) ) )

                                         ).
    APPEND LINES OF lt_pckattr_charac_params TO mt_charac_to_create.

    lv_classtype = '011'.

    LOOP AT lt_pckattr_charac_params REFERENCE INTO DATA(lr_charact_pckattr_params).

      APPEND VALUE #( name_char = lr_charact_pckattr_params->charactdetail-charact_name ) TO lt_class_characts_params.

    ENDLOOP.

    lt_pckattr_classi_params = VALUE #( ( classnumber    = iv_classi_prefix && '_RDP_CUST_EXT'
                                          classtype      = lv_classtype
                                          classbasicdata = VALUE #( status = '1' same_value_no = 'X' valid_from = '00010101' valid_to = '99991231' )
                                          description    = VALUE #( ( langu = 'E' langu_iso = 'EN' catchword = 'RDP specific Customer Attributes​' ) )
                                          classcharacteristics = lt_class_characts_params ) ).
    APPEND LINES OF lt_pckattr_classi_params TO mt_classi_to_create.

  ENDMETHOD.


  METHOD setup_dataset.

    IF iv_scope = gv_scope_main_and_subordinate
        OR iv_scope = gv_scope_specdb.
      setup_packelem_main_and_subord( iv_charac_prefix = iv_charac_prefix
                                      iv_classi_prefix = iv_classi_prefix
                                      iv_scope         = iv_scope ).
    ELSEIF iv_scope = gv_scope_matcls.
      setup_packelem_attr_dataset( iv_charac_prefix = iv_charac_prefix
                                   iv_classi_prefix = iv_classi_prefix
                                   iv_scope         = iv_scope ).
      setup_packelem_frac_dataset( iv_charac_prefix = iv_charac_prefix
                                   iv_classi_prefix = iv_classi_prefix
                                   iv_scope         = iv_scope ).
    ELSEIF iv_scope = gv_scope_plst.
      setup_es_plastictax( iv_charac_prefix = iv_charac_prefix
                           iv_classi_prefix = iv_classi_prefix
                           iv_scope         = iv_scope ).
    ELSEIF iv_scope = gv_scope_cust.
      setup_cust_ext( iv_charac_prefix = iv_charac_prefix
                      iv_classi_prefix = iv_classi_prefix
                      iv_scope         = iv_scope ).
    ENDIF.

  ENDMETHOD.


  METHOD setup_es_plastictax.

    DATA lv_classtype TYPE bapi_class_key-classtype.
    DATA lt_pckattr_charac_params   TYPE mty_tty_characteristics.
    DATA lt_pckattr_classi_params TYPE mty_tty_classifications.
    DATA lt_class_characts_params TYPE STANDARD TABLE OF bapi1003_charact WITH NON-UNIQUE DEFAULT KEY.

    lt_pckattr_charac_params = VALUE #( ( charactdetail-charact_name = iv_charac_prefix && '_ES_PLASTIC'
                                           charactdetail-data_type    = 'NUM'
                                           charactdetail-length       = 15
                                           charactdetail-decimals     = 6
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           num_values                 = VALUE #( ( value_from = 0 ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'ES: Weight of plastic' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_ES_NONRECYCLED_PLASTIC'
                                           charactdetail-data_type    = 'NUM'
                                           charactdetail-length       = 15
                                           charactdetail-decimals     = 6
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           num_values                 = VALUE #( ( value_from = 0 ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'ES: Weight of non-rec. plastic' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_ES_EXEMPTION'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 5
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'ES: Exemption article' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_ES_DECREFNO'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 30
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'ES: Declaration ref. number' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_ES_CERTREFNO'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 30
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'ES: Certificate ref. number' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_ES_PRODORDER_CHECK'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 1
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = 'Y' )
                                                                                 ( value_char = 'N' ) )
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = 'Y' description = 'Yes' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'N' description = 'No' ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'ES: Check Production Order' ) ) )

                                         ( charactdetail-charact_name = iv_charac_prefix && '_ES_PACKMAT_TAX'
                                           charactdetail-data_type    = 'NUM'
                                           charactdetail-length       = 15
                                           charactdetail-decimals     = 6
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           num_values                 = VALUE #( ( value_from = 0 ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'ES: Pl. Tax for PackMat in EUR' ) ) )
                                           ).
    APPEND LINES OF lt_pckattr_charac_params TO mt_charac_to_create.

    lv_classtype = '001'.

    LOOP AT lt_pckattr_charac_params REFERENCE INTO DATA(lr_charact_pckattr_params).

      APPEND VALUE #( name_char = lr_charact_pckattr_params->charactdetail-charact_name ) TO lt_class_characts_params.

    ENDLOOP.

    lt_pckattr_classi_params = VALUE #( ( classnumber    = iv_classi_prefix && '_ES_PLASTICTAX'
                                          classtype      = lv_classtype
                                          classbasicdata = VALUE #( status = '1' same_value_no = 'X' valid_from = '00010101' valid_to = '99991231' )
                                          description    = VALUE #( ( langu = 'E' langu_iso = 'EN' catchword = 'ES plastic tax data for invoice print' ) )
                                          classcharacteristics = lt_class_characts_params ) ).
    APPEND LINES OF lt_pckattr_classi_params TO mt_classi_to_create.

  ENDMETHOD.


  METHOD setup_packelem_attr_dataset.

    DATA lv_classtype TYPE bapi_class_key-classtype.
    DATA lt_pckattr_charac_params   TYPE mty_tty_characteristics.
    DATA lt_pckattr_classi_params TYPE mty_tty_classifications.
    DATA lt_class_characts_params TYPE STANDARD TABLE OF bapi1003_charact WITH NON-UNIQUE DEFAULT KEY.

    lt_pckattr_charac_params = VALUE #( ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_UOM'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 3
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-check_table  = 'T006'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Unit of Measure of PackElemnt' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_TYPE'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 10
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = 'APL'  )      "value_char_long = 'APL'
                                                                                 ( value_char = 'BAG'  )      "value_char_long = 'BAG'
                                                                                 ( value_char = 'BCK'  )      "value_char_long = 'BCK'
                                                                                 ( value_char = 'BIG'  )      "value_char_long = 'BIG'
                                                                                 ( value_char = 'BLS'  )      "value_char_long = 'BLS'
                                                                                 ( value_char = 'BND'  )      "value_char_long = 'BND'
                                                                                 ( value_char = 'BOX'  )      "value_char_long = 'BOX'
                                                                                 ( value_char = 'BRK'  )      "value_char_long = 'BRK'
                                                                                 ( value_char = 'BSK'  )      "value_char_long = 'BSK'
                                                                                 ( value_char = 'BTL'  )      "value_char_long = 'BTL'
                                                                                 ( value_char = 'CAN'  )      "value_char_long = 'CAN'
                                                                                 ( value_char = 'CAP'  )      "value_char_long = 'CAP'
                                                                                 ( value_char = 'CGE'  )      "value_char_long = 'CGE'
                                                                                 ( value_char = 'CNR'  )      "value_char_long = 'CNR'
                                                                                 ( value_char = 'CPS'  )      "value_char_long = 'CPS'
                                                                                 ( value_char = 'CRA'  )      "value_char_long = 'CRA'
                                                                                 ( value_char = 'CTG'  )      "value_char_long = 'CTG'
                                                                                 ( value_char = 'CUP'  )      "value_char_long = 'CUP'
                                                                                 ( value_char = 'CUS'  )      "value_char_long = 'CUS'
                                                                                 ( value_char = 'CUT'  )      "value_char_long = 'CUT'
                                                                                 ( value_char = 'DCO'  )      "value_char_long = 'DCO'
                                                                                 ( value_char = 'DCO'  )      "value_char_long = 'DCO'
                                                                                 ( value_char = 'DOS'  )      "value_char_long = 'DOS'
                                                                                 ( value_char = 'DSC'  )      "value_char_long = 'DSC'
                                                                                 ( value_char = 'DSP'  )      "value_char_long = 'DSP'
                                                                                 ( value_char = 'ENV'  )      "value_char_long = 'ENV'
                                                                                 ( value_char = 'FLA'  )      "value_char_long = 'FLA'
                                                                                 ( value_char = 'FOI'  )      "value_char_long = 'FOI'
                                                                                 ( value_char = 'GAC'  )      "value_char_long = 'GAC'
                                                                                 ( value_char = 'HDL'  )      "value_char_long = 'HDL'
                                                                                 ( value_char = 'HGR'  )      "value_char_long = 'HGR'
                                                                                 ( value_char = 'HOD'  )      "value_char_long = 'HOD'
                                                                                 ( value_char = 'IBC'  )      "value_char_long = 'IBC'
                                                                                 ( value_char = 'JAR'  )      "value_char_long = 'JAR'
                                                                                 ( value_char = 'LBL'  )      "value_char_long = 'LBL'
                                                                                 ( value_char = 'LCE'  )      "value_char_long = 'LCE'
                                                                                 ( value_char = 'NET'  )      "value_char_long = 'NET'
                                                                                 ( value_char = 'PFE'  )      "value_char_long = 'PFE'
                                                                                 ( value_char = 'PLT'  )      "value_char_long = 'PLT'
                                                                                 ( value_char = 'PRP'  )      "value_char_long = 'PRP'
                                                                                 ( value_char = 'RAW'  )      "value_char_long = 'RAW'
                                                                                 ( value_char = 'SBG'  )      "value_char_long = 'SBG'
                                                                                 ( value_char = 'SCH'  )      "value_char_long = 'SCH'
                                                                                 ( value_char = 'SEL'  )      "value_char_long = 'SEL'
                                                                                 ( value_char = 'SHT'  )      "value_char_long = 'SHT'
                                                                                 ( value_char = 'SLV'  )      "value_char_long = 'SLV'
                                                                                 ( value_char = 'SPL'  )      "value_char_long = 'SPL'
                                                                                 ( value_char = 'SPT'  )      "value_char_long = 'SPT'
                                                                                 ( value_char = 'STR'  )      "value_char_long = 'STR'
                                                                                 ( value_char = 'TAP'  )      "value_char_long = 'TAP'
                                                                                 ( value_char = 'TBE'  )      "value_char_long = 'TBE'
                                                                                 ( value_char = 'TBW'  )      "value_char_long = 'TBW'
                                                                                 ( value_char = 'TRY'  )      "value_char_long = 'TRY'
                                                                                 ( value_char = 'UNF'  )      "value_char_long = 'UNF'
                                                                                 ( value_char = 'WRP'  ) )    "value_char_long = 'WRP'
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = 'APL' description = 'Applicator, element that supports the use of the product' ) "value_char_long = 'APL'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'BAG' description = 'Bag, sack, pouch' )                                         "value_char_long = 'BAG'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'BCK' description = 'Bucket' )                                                   "value_char_long = 'BCK'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'BIG' description = 'Big bag' )                                                  "value_char_long = 'BIG'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'BLS' description = 'Blister' )                                                  "value_char_long = 'BLS'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'BND' description = 'Strap band' )                                               "value_char_long = 'BND'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'BOX' description = 'Box, case' )                                                "value_char_long = 'BOX'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'BRK' description = 'Brick or gable top container' )                             "value_char_long = 'BRK'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'BSK' description = 'Basket' )                                                   "value_char_long = 'BSK'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'BTL' description = 'Bottle' )                                                   "value_char_long = 'BTL'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'BTL' description = 'Can, drum, barrel' )                                        "value_char_long = 'BTL'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'CAP' description = 'Closure, cap, lid' )                                        "value_char_long = 'CAP'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'CGE' description = 'Cage, wire mesh cage, pallet cage' )                        "value_char_long = 'CGE'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'CNR' description = 'Generic container (unspecific)' )                           "value_char_long = 'CNR'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'CPS' description = 'Capsule' )                                                  "value_char_long = 'CPS'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'CRA' description = 'Crate' )                                                    "value_char_long = 'CRA'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'CTG' description = 'Cartridge' )                                                "value_char_long = 'CTG'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'CUP' description = 'Cup' )                                                      "value_char_long = 'CUP'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'CUS' description = 'Cushion, filler, stuffer' )                                 "value_char_long = 'CUS'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'CUT' description = 'Spoon, cutlery' )                                           "value_char_long = 'CUT'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'DCO' description = 'Decorative packaging element' )                             "value_char_long = 'DCO'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'DOS' description = 'Dosing aid' )                                               "value_char_long = 'DOS'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'DSC' description = 'Desiccant sachet' )                                         "value_char_long = 'DSC'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'DSP' description = 'Display stand' )                                            "value_char_long = 'DSP'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'ENV' description = 'Envelope' )                                                 "value_char_long = 'ENV'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'FLA' description = 'Flask, vial, ampule' )                                      "value_char_long = 'FLA'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'FOI' description = 'Foil, film, wrapper, tape' )                                "value_char_long = 'FOI'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'GAC' description = 'Gas or aerosol can' )                                       "value_char_long = 'GAC'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'HDL' description = 'Handle' )                                                   "value_char_long = 'HDL'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'HGR' description = 'Hanger' )                                                   "value_char_long = 'HGR'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'HOD' description = 'Hood' )                                                     "value_char_long = 'HOD'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'IBC' description = 'Intermediate bulk container' )                              "value_char_long = 'IBC'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'JAR' description = 'Jar, jug, tub' )                                            "value_char_long = 'JAR'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'LBL' description = 'Label' )                                                    "value_char_long = 'LBL'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'LCE' description = 'Lace, tie, ribbon' )                                        "value_char_long = 'LCE'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'NET' description = 'Netting, string bag' )                                      "value_char_long = 'NET'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'PFE' description = 'Protective and fastening element' )                         "value_char_long = 'PFE'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'PLT' description = 'Pallet' )                                                   "value_char_long = 'PLT'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'PRP' description = 'Printed paper (not packaging)' )                            "value_char_long = 'PRP'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'RAW' description = 'Raw material (for the production of packaging)' )           "value_char_long = 'RAW'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'SBG' description = 'Shopping bag, carry-out bag' )                              "value_char_long = 'SBG'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'SCH' description = 'Sachet' )                                                   "value_char_long = 'SCH'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'SEL' description = 'Seal, tamper-evident closure' )                             "value_char_long = 'SEL'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'SHT' description = 'Sheet, board' )                                             "value_char_long = 'SHT'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'SLV' description = 'Sleeve, collar' )                                           "value_char_long = 'SLV'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'SPL' description = 'Spool, reel, bobbin' )                                      "value_char_long = 'SPL'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'SPT' description = 'Spout' )                                                    "value_char_long = 'SPT'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'STR' description = 'Straw' )                                                    "value_char_long = 'STR'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'TAP' description = 'Adhesive tape' )                                            "value_char_long = 'TAP'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'TBE' description = 'Tube' )                                                     "value_char_long = 'TBE'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'TBW' description = 'Tableware' )                                                "value_char_long = 'TBW'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'TRY' description = 'Tray' )                                                     "value_char_long = 'TRY'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'UNF' description = 'Semifinished packaging' )                                   "value_char_long = 'UNF'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'WRP' description = 'Shrink wrap, stretch wrap' ) )                              "value_char_long = 'WRP'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Packaging Type' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_USAGE'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 10
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = ''  )    "value_char_long = ''
                                                                                 ( value_char = 'C'  )   "value_char_long = 'C'
                                                                                 ( value_char = 'H'  )   "value_char_long = 'H'
                                                                                 ( value_char = 'I'  )   "value_char_long = 'I'
                                                                                 ( value_char = 'T'  ) ) "value_char_long = 'T'
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = ''  description = 'Unknown' )      "value_char_long = ''
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'C'  description = 'Commercial' )  "value_char_long = 'C'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'H'  description = 'Household' )   "value_char_long = 'H'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'I'  description = 'Industrial' )  "value_char_long = 'I'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'T'  description = 'Transport' ) ) "value_char_long = 'T'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Usage' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_FLEXIBILTY'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 2
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = ''   )    "value_char_long = ''
                                                                                 ( value_char = 'FL'  )   "value_char_long = 'FL'
                                                                                 ( value_char = 'RI'  ) ) "value_char_long = 'RI'
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = ''   description = 'Unknown' )  "value_char_long = ''
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'FL' description = 'Flexible' ) "value_char_long = 'FL'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'RI' description = 'Rigid' ) )  "value_char_long = 'RI'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Flexibility' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_IS_REUSABLE'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 1
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = ''  )    "value_char_long = ''
                                                                                 ( value_char = 'Y'  )   "value_char_long = 'Y'
                                                                                 ( value_char = 'N'  ) ) "value_char_long = 'N'
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = ''  description = 'Unknown' ) "value_char_long = ''
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'Y'  description = 'True' )   "value_char_long = 'Y'
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'N'  description = 'False' ) )"value_char_long = 'N'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Is Reusable' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_REUSE_TIMES'
                                           charactdetail-data_type    = 'NUM'
                                           charactdetail-length       = 3
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           num_values                 = VALUE #( ( value_from = 0 ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Reuse Times' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_REUSE_LTIME'
                                           charactdetail-data_type    = 'NUM'
                                           charactdetail-length       = 4
                                           charactdetail-decimals     = 1
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Reuse Lifetime in Years' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_RECYCL_PERC'
                                           charactdetail-data_type    = 'NUM'
                                           charactdetail-length       = 3
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           charactdetail-unit_of_measurement = '%'
                                           charactdetail-unit_of_measurement_iso = 'P1'
                                           num_values                 = VALUE #( ( value_from = 0 value_to = 100 ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Recyclable in Percent' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_COMPOT_PERC'
                                           charactdetail-data_type    = 'NUM'
                                           charactdetail-length       = 3
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           charactdetail-unit_of_measurement = '%'
                                           charactdetail-unit_of_measurement_iso = 'P1'
                                           num_values                 = VALUE #( ( value_from = 0 value_to = 100 ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Compostable in Percent' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_IS_ASEPTIC'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 1
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = ''  )
                                                                                 ( value_char = 'Y'  )
                                                                                 ( value_char = 'N'  ) )
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = ''  description = 'Unknown' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'Y'  description = 'True' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'N'  description = 'False' ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Is Aseptic' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_IS_NOTEMPTY'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 1
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = '' )
                                                                                 ( value_char = 'Y' )
                                                                                 ( value_char = 'N' ) )
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = '' description = 'Unknown' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'Y' description = 'True' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'N' description = 'False' ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Is Not Empty' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_LENGTH'
                                           charactdetail-data_type    = 'NUM'
                                           charactdetail-length       = 15
                                           charactdetail-decimals     = 6
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-interval_allowed = 'X'
                                           charactdetail-additional_values = 'X'
                                           charactdetail-unformated   = 'X'
                                           charactdetail-unit_of_measurement = 'MM'
                                           charactdetail-unit_of_measurement_iso = 'MMT'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Length' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_WIDTH'
                                           charactdetail-data_type    = 'NUM'
                                           charactdetail-length       = 15
                                           charactdetail-decimals     = 6
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-interval_allowed = 'X'
                                           charactdetail-additional_values = 'X'
                                           charactdetail-unformated   = 'X'
                                           charactdetail-unit_of_measurement = 'MM'
                                           charactdetail-unit_of_measurement_iso = 'MMT'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Width' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_HEIGHT'
                                           charactdetail-data_type    = 'NUM'
                                           charactdetail-length       = 15
                                           charactdetail-decimals     = 6
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-interval_allowed = 'X'
                                           charactdetail-additional_values = 'X'
                                           charactdetail-unformated   = 'X'
                                           charactdetail-unit_of_measurement = 'MM'
                                           charactdetail-unit_of_measurement_iso = 'MMT'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Height' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_VOLUME'
                                           charactdetail-data_type    = 'NUM'
                                           charactdetail-length       = 15
                                           charactdetail-decimals     = 6
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           charactdetail-unit_of_measurement = 'CDM'
                                           charactdetail-unit_of_measurement_iso = 'DMQ'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Volume' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_IS_DEPOSIT'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 1
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = ''  )
                                                                                 ( value_char = 'Y'  )
                                                                                 ( value_char = 'N'  ) )
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = ''  description = 'Unknown' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'Y'  description = 'True' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'N'  description = 'False' ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Is Deposit' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_IS_SERVPACK'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 1
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = ''  )
                                                                                 ( value_char = 'Y'  )
                                                                                 ( value_char = 'N'  ) )
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = ''   description = 'Unknown' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'Y'  description = 'True' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'N'  description = 'False' ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Is Service Packaging' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_IS_OPT_DECT'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 1
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = ''   )
                                                                                 ( value_char = 'Y'  )
                                                                                 ( value_char = 'N'  ) )
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = ''  description = 'Unknown' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'Y'  description = 'True' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'N'  description = 'False' ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Is Optically Detectable' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_LAMINA_TYPE'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 10
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           char_values                = VALUE #( ( value_char = '' )
                                                                                 ( value_char = 'D' )
                                                                                 ( value_char = 'N' )
                                                                                 ( value_char = 'S' ) )
                                           char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = '' description = 'Unknown' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'D' description = 'Both sides' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'N' description = 'Not laminated' )
                                                                                 ( language_int = 'E' language_iso = 'EN' value_char = 'S' description = 'Single side' ) )
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Lamination Type' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_DECREFNO'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 30
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Recyc-Content Self-Declaration' ) ) )
                                         ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_ATTR_CERTR'
                                           charactdetail-data_type    = 'CHAR'
                                           charactdetail-length       = 30
                                           charactdetail-decimals     = 0
                                           charactdetail-status       = '1'
                                           charactdetail-value_assignment = 'S'
                                           charactdetail-additional_values = 'X'
                                           description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = 'Recyc-Content Certificate' ) ) ) ).
    APPEND LINES OF lt_pckattr_charac_params TO mt_charac_to_create.

    IF iv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_matcls.
      lv_classtype = '001'.
    ELSEIF iv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_specdb.
      lv_classtype = '100'.
    ENDIF.

    LOOP AT lt_pckattr_charac_params REFERENCE INTO DATA(lr_charact_pckattr_params).

      APPEND VALUE #( name_char = lr_charact_pckattr_params->charactdetail-charact_name ) TO lt_class_characts_params.

    ENDLOOP.

    lt_pckattr_classi_params = VALUE #( ( classnumber    = iv_classi_prefix && '_PACKELEM_ATTR'
                                          classtype      = lv_classtype
                                          classbasicdata = VALUE #( status = '1' same_value_no = 'X' valid_from = '00010101' valid_to = '99991231' )
                                          description    = VALUE #( ( langu = 'E' langu_iso = 'EN' catchword = 'Packaging Element Attributes' ) )
                                          classcharacteristics = lt_class_characts_params ) ).
    APPEND LINES OF lt_pckattr_classi_params TO mt_classi_to_create.

  ENDMETHOD.


  METHOD setup_packelem_frac_dataset.

    CONSTANTS:
      BEGIN OF lc_funcgroupname,
        funcgroupname_10  TYPE char255 VALUE 'Additives and Fillers',
        funcgroupname_20  TYPE char255 VALUE 'Barriers',
        funcgroupname_30  TYPE char255 VALUE 'Coatings',
      END OF lc_funcgroupname.

    DATA: lv_classtype             TYPE bapi_class_key-classtype,
          lv_fraction_create_count TYPE i,
          lv_fnction_create_count  TYPE i,
          lv_fraction_id           TYPE i,
          lv_function_id           TYPE i,
          lv_fraction_id_str       TYPE string,
          lv_function_id_str       TYPE string,
          lv_description_prefix    TYPE string,
          lv_description_prefix_func    TYPE string,
          lv_description_prefix_fract   TYPE string,
          lv_frac_clas_mid_str     TYPE string,
          lv_sufix_e               TYPE string,
          lv_sufix_c               TYPE string,
          lv_midstr_a              TYPE string.

    DATA lt_pckfrac_classi_params TYPE mty_tty_classifications.
    DATA lt_pckfrac_charac_params TYPE mty_tty_characteristics.
    DATA lt_pckfunc_charac_params TYPE mty_tty_characteristics.
    DATA lt_class_characts_params TYPE STANDARD TABLE OF bapi1003_charact WITH NON-UNIQUE DEFAULT KEY.

    IF iv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_matcls.
      lv_frac_clas_mid_str = '_PACKELE_FRAC'.
      lv_fraction_create_count = 4.
      lv_fnction_create_count = 2.
      lv_sufix_e = ''.
      lv_sufix_c = ''.
      lv_midstr_a = ''.
      lv_classtype = '001'.
    ELSEIF iv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_specdb.
      lv_frac_clas_mid_str = '_PACKELEM_FRAC'.
      lv_fraction_create_count = 1.
      lv_sufix_e = 'E'.
      lv_sufix_c = 'C'.
      lv_midstr_a = 'A'.
      lv_classtype = '100'.
    ENDIF.

    lv_fraction_id = 1.

    DO lv_fraction_create_count TIMES.

      IF iv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_matcls.
        lv_fraction_id_str = |{ lv_fraction_id }|.
        lv_description_prefix = |{ lv_fraction_id }: |.
      ELSEIF iv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_specdb.
        lv_fraction_id_str = ''.
        lv_description_prefix = ''.
      ENDIF.
      lt_pckfrac_charac_params = VALUE #( ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_CODE'
                                            charactdetail-data_type    = 'CHAR'
                                            charactdetail-length       = 10
                                            charactdetail-decimals     = 0
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'X'
                                            char_values                = VALUE #( ( value_char = '1110'  )      "value_char_long = '1110'
                                                                                  ( value_char = '1150'  )      "value_char_long = '1150'
                                                                                  ( value_char = '1210'  )      "value_char_long = '1210'
                                                                                  ( value_char = '1310'  )      "value_char_long = '1310'
                                                                                  ( value_char = '1320'  )      "value_char_long = '1320'
                                                                                  ( value_char = '1330'  )      "value_char_long = '1330'
                                                                                  ( value_char = '1340'  )      "value_char_long = '1340'
                                                                                  ( value_char = '1380'  )      "value_char_long = '1380'
                                                                                  ( value_char = '1520'  )      "value_char_long = '1520'
                                                                                  ( value_char = '1530'  )      "value_char_long = '1530'
                                                                                  ( value_char = '2010'  )      "value_char_long = '2010'
                                                                                  ( value_char = '2020'  )      "value_char_long = '2020'
                                                                                  ( value_char = '2030'  )      "value_char_long = '2030'
                                                                                  ( value_char = '2031'  )      "value_char_long = '2031'
                                                                                  ( value_char = '2090'  )      "value_char_long = '2090'
                                                                                  ( value_char = '3000'  )      "value_char_long = '3000'
                                                                                  ( value_char = '3100'  )      "value_char_long = '3100'
                                                                                  ( value_char = '4111'  )      "value_char_long = '4111'
                                                                                  ( value_char = '4112'  )      "value_char_long = '4112'
                                                                                  ( value_char = '4113'  )      "value_char_long = '4113'
                                                                                  ( value_char = '4114'  )      "value_char_long = '4114'
                                                                                  ( value_char = '4115'  )      "value_char_long = '4115'
                                                                                  ( value_char = '4121'  )      "value_char_long = '4121'
                                                                                  ( value_char = '4122'  )      "value_char_long = '4122'
                                                                                  ( value_char = '4130'  )      "value_char_long = '4130'
                                                                                  ( value_char = '4140'  )      "value_char_long = '4140'
                                                                                  ( value_char = '4150'  )      "value_char_long = '4150'
                                                                                  ( value_char = '4160'  )      "value_char_long = '4160'
                                                                                  ( value_char = '4170'  )      "value_char_long = '4170'
                                                                                  ( value_char = '4180'  )      "value_char_long = '4180'
                                                                                  ( value_char = '4191'  )      "value_char_long = '4191'
                                                                                  ( value_char = '4210'  )      "value_char_long = '4210'
                                                                                  ( value_char = '4211'  )      "value_char_long = '4211'
                                                                                  ( value_char = '4212'  )      "value_char_long = '4212'
                                                                                  ( value_char = '4213'  )      "value_char_long = '4213'
                                                                                  ( value_char = '4214'  )      "value_char_long = '4214'
                                                                                  ( value_char = '4215'  )      "value_char_long = '4215'
                                                                                  ( value_char = '4216'  )      "value_char_long = '4216'
                                                                                  ( value_char = '4220'  )      "value_char_long = '4220'
                                                                                  ( value_char = '4233'  )      "value_char_long = '4233'
                                                                                  ( value_char = '4251'  )      "value_char_long = '4251'
                                                                                  ( value_char = '4252'  )      "value_char_long = '4252'
                                                                                  ( value_char = '4310'  )      "value_char_long = '4310'
                                                                                  ( value_char = '4320'  )      "value_char_long = '4320'
                                                                                  ( value_char = '4330'  )      "value_char_long = '4330'
                                                                                  ( value_char = '4340'  )      "value_char_long = '4340'
                                                                                  ( value_char = '4410'  )      "value_char_long = '4410'
                                                                                  ( value_char = '4420'  )      "value_char_long = '4420'
                                                                                  ( value_char = '4430'  )      "value_char_long = '4430'
                                                                                  ( value_char = '4440'  )      "value_char_long = '4440'
                                                                                  ( value_char = '4450'  )      "value_char_long = '4450'
                                                                                  ( value_char = '4451'  )      "value_char_long = '4451'
                                                                                  ( value_char = '4452'  )      "value_char_long = '4452'
                                                                                  ( value_char = '4453'  )      "value_char_long = '4453'
                                                                                  ( value_char = '4460'  )      "value_char_long = '4460'
                                                                                  ( value_char = '4461'  )      "value_char_long = '4461'
                                                                                  ( value_char = '4470'  )      "value_char_long = '4470'
                                                                                  ( value_char = '4480'  )      "value_char_long = '4480'
                                                                                  ( value_char = '4510'  )      "value_char_long = '4510'
                                                                                  ( value_char = '4910'  )      "value_char_long = '4910'
                                                                                  ( value_char = '6000'  )      "value_char_long = '6000'
                                                                                  ( value_char = '7000'  )      "value_char_long = '7000'
                                                                                  ( value_char = '7002'  )      "value_char_long = '7002'
                                                                                  ( value_char = '7100'  )      "value_char_long = '7100'
                                                                                  ( value_char = '9010'  ) )    "value_char_long = '9010'
                                            char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = '1110' description = 'Wood' )                                                                 "value_char_long =

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '1150' description = 'Cork' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '1210' description = 'Plywood' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '1310' description = 'Paper' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '1320' description = 'Cardboard' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '1330' description = 'Corrugated cardboard' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '1340' description = 'Papier-mache' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '1380' description = 'Kraft paper' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '1520' description = 'Corn based carton and papier-mache' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '1530' description = 'Hemp based carton and papier-mache' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '2010' description = 'Steel' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '2020' description = 'Aluminum' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '2030' description = 'Tinplate' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '2031' description = 'ECCS (electrolytic chromium coated steel)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '2090' description = 'Non-magnetic steel' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '3000' description = 'Glass' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '3100' description = 'Glass other than soda-lime glass' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4111' description = 'EPS (Expanded polystyrene)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4112' description = 'XPS (Extruded polystyrene)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4113' description = 'HIPS (High impact polystyrene)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4114' description = 'OPS (Oriented polystyrene)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4115' description = 'Polystyrene, non-expanded' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4121' description = 'HDPE (High density polyethylene)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4122' description = 'LDPE (Low-density polyethylene)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4130' description = 'PET (Polyethylene terephthalate)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4140' description = 'PET(G) (Polyethylene terephthalate glycol)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4150' description = 'PA (Polyamide) (Nylon)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4160' description = 'Polycaprolactam (Nylon 6)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4170' description = 'PMMA (Polymethyl methacrylate) (Acrylic glass)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4180' description = 'PTFE (Polytetrafluoroethylene)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4191' description = 'PVC (Polyvinyl chloride)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4210' description = 'PVDC (Polyvinylidene chloride)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4211' description = 'PP (Polypropylene)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4212' description = 'PC (Polycarbonate)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4213' description = 'ABS (Acrylonitrile butadiene styrene)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4214' description = 'POM (Polyoxymethylene)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4215' description = 'PEEK (Polyether ether ketone)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4216' description = 'PPS (Polyphenylene sulfide)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4220' description = 'EVOH (Ethylene vinyl alcohol)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4233' description = 'EVA (also known as EVAC, ethylene-vinyl acetate copolymer)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4251' description = 'EAA (Ethylene acrylic acid)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4252' description = 'EMAA (Ethylene methacrylic acid)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4310' description = 'PU (Polyurethane)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4320' description = 'Epoxy' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4330' description = 'PF (Phenol formaldehyde)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4340' description = 'Urea and melamine formaldehyde' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4410' description = 'PBS (Polybutylene succinate)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4420' description = 'PLA (Polylactic acid)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4430' description = 'PVA (Polyvinyl alcohol)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4440' description = 'PAH (polyanhydride)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4450' description = 'PHA (Polyhydroxyalkanoates)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4451' description = 'PHB (Poly-3-hydroxybutyrate)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4452' description = 'PHH (Polyhydroxyhexanoate)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4453' description = 'PHV (Polyhydroxyvalerate)' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4460' description = 'Cellulose acetate' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4461' description = 'Cellulose regenerated' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4470' description = 'Cellulose diacetate' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4480' description = 'Nitrocellulose' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4510' description = 'Natural rubber' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '4910' description = 'Silicone' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '6000' description = 'Ceramics, stoneware' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '7000' description = 'Textiles: generic' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '7002' description = 'Textiles: cotton' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '7100' description = 'Textiles: fossil source' )

                                                                            ( language_int = 'E' language_iso = 'EN' value_char = '9010' description = 'Ink, lacquer, glue' ) )

                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Fraction Code| ) ) )
                                          ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_WEIGHT'
                                            charactdetail-data_type    = 'NUM'
                                            charactdetail-length       = 15
                                            charactdetail-decimals     = 6
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            charactdetail-unit_of_measurement = 'G'
                                            charactdetail-unit_of_measurement_iso = 'GRM'
                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Weight| ) ) )
                                          ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_TRANSPAREN' && lv_sufix_c
                                            charactdetail-data_type    = 'CHAR'
                                            charactdetail-length       = 10
                                            charactdetail-decimals     = 0
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            char_values                = VALUE #( ( value_char = ''   )
                                                                                  ( value_char = 'OP' )
                                                                                  ( value_char = 'TR' ) )
                                            char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = ''   description = 'Unknown' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'OP' description = 'Opaque' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'TR' description = 'Transparent' ) )
                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Transparency| ) ) )
                                          ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_COLOR'
                                            charactdetail-data_type    = 'CHAR'
                                            charactdetail-length       = 10
                                            charactdetail-decimals     = 0
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            char_values                = VALUE #( ( value_char = ''    )     "value_char_long = ''
                                                                                  ( value_char = 'BLK' )     "value_char_long = 'BLK'
                                                                                  ( value_char = 'BLU' )     "value_char_long = 'BLU'
                                                                                  ( value_char = 'BRO' )     "value_char_long = 'BRO'
                                                                                  ( value_char = 'CLR' )     "value_char_long = 'CLR'
                                                                                  ( value_char = 'CYA' )     "value_char_long = 'CYA'
                                                                                  ( value_char = 'GRE' )     "value_char_long = 'GRE'
                                                                                  ( value_char = 'GRY' )     "value_char_long = 'GRY'
                                                                                  ( value_char = 'LBL' )     "value_char_long = 'LBL'
                                                                                  ( value_char = 'ORA' )     "value_char_long = 'ORA'
                                                                                  ( value_char = 'RED' )     "value_char_long = 'RED'
                                                                                  ( value_char = 'WHI' )     "value_char_long = 'WHI'
                                                                                  ( value_char = 'YLW' ) )   "value_char_long = 'YLW'
                                            char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = ''     description = 'Unknown' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'BLK'  description = 'Black' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'BLU'  description = 'Blue' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'BRO'  description = 'Brown' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'CLR'  description = 'Clear' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'CYA'  description = 'Cyan' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'GRE'  description = 'Green' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'GRY'  description = 'Grey' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'LBL'  description = 'Light blue' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'ORA'  description = 'Orange' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'RED'  description = 'Red' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'WHI'  description = 'White' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'YLW'  description = 'Yellow' ) )
                                            description                 = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Color| ) ) )
                                          ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_CHEMRC_PER' && lv_sufix_c
                                            charactdetail-data_type    = 'NUM'
                                            charactdetail-length       = 3
                                            charactdetail-decimals     = 0
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            charactdetail-unit_of_measurement = '%'
                                            charactdetail-unit_of_measurement_iso = 'P1'
                                            num_values                 = VALUE #( ( value_from = 0 value_to = 100 ) )
                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Chemically recycled Content in Percent| ) ) )
                                          ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_CHEMRC_VAM' && lv_sufix_e
                                            charactdetail-data_type    = 'CHAR'
                                            charactdetail-length       = 20
                                            charactdetail-decimals     = 0
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            char_values                = VALUE #( ( value_char = 'MASS_BALANCED'  ) )
                                            char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = 'MASS_BALANCED'  description = 'Mass balanced' ) )
                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Value Method for chemically recycled content| ) ) )
                                          ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_MECHRC_PER' && lv_sufix_c
                                            charactdetail-data_type    = 'NUM'
                                            charactdetail-length       = 3
                                            charactdetail-decimals     = 0
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            charactdetail-unit_of_measurement = '%'
                                            charactdetail-unit_of_measurement_iso = 'P1'
                                            num_values                 = VALUE #( ( value_from = 0 value_to = 100 ) )
                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Mechanically recycled Content in Percent| ) ) )
                                          ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_MECHRC_VAM'  && lv_sufix_e
                                            charactdetail-data_type    = 'CHAR'
                                            charactdetail-length       = 20
                                            charactdetail-decimals     = 0
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            char_values                = VALUE #( ( value_char = 'MASS_BALANCED' ) )
                                            char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = 'MASS_BALANCED' description = 'Mass balanced' ) )
                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Value Method for mechanically recycled Content| ) ) )
                                          ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_RECYCO_PER' && lv_sufix_c
                                            charactdetail-data_type    = 'NUM'
                                            charactdetail-length       = 3
                                            charactdetail-decimals     = 0
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            charactdetail-unit_of_measurement = '%'
                                            charactdetail-unit_of_measurement_iso = 'P1'
                                            num_values                 = VALUE #( ( value_from = 0 value_to = 100 ) )
                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Other Recycled Content in Percent| ) ) )
                                          ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_RENEW' && lv_midstr_a &&'_PERC'
                                            charactdetail-data_type    = 'NUM'
                                            charactdetail-length       = 3
                                            charactdetail-decimals     = 0
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            charactdetail-unit_of_measurement = '%'
                                            charactdetail-unit_of_measurement_iso = 'P1'
                                            num_values                 = VALUE #( ( value_from = 0 value_to = 100 ) )
                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Renewable in Percent| ) ) )
                                          ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_THICKNESS'
                                            charactdetail-data_type    = 'NUM'
                                            charactdetail-length       = 15
                                            charactdetail-decimals     = 6
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            charactdetail-unit_of_measurement = 'MM'
                                            charactdetail-unit_of_measurement_iso = 'MMT'
                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Thickness| ) ) )
                                          ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_DENSITY'
                                            charactdetail-data_type    = 'NUM'
                                            charactdetail-length       = 15
                                            charactdetail-decimals     = 6
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            charactdetail-unit_of_measurement = 'RHO'
                                            charactdetail-unit_of_measurement_iso = '23'
                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Density| ) ) )
                                          ( charactdetail-charact_name = iv_charac_prefix && '_PACKELEM_FRAC' && lv_fraction_id_str && '_IS_REINFOR' && lv_sufix_c
                                            charactdetail-data_type    = 'CHAR'
                                            charactdetail-length       = 1
                                            charactdetail-decimals     = 0
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            char_values                = VALUE #( ( value_char = ''  )
                                                                                  ( value_char = 'Y'  )
                                                                                  ( value_char = 'N'  ) )
                                            char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = ''  description = 'Unknown' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'Y'  description = 'True' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'N'  description = 'False' ) )
                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Is Reinforced| ) ) )
                                          ( charactdetail-charact_name = |{ iv_charac_prefix }_PACKELEM_FRAC{ lv_fraction_id_str }_IS_EXP{ lv_midstr_a }NDED|
                                            charactdetail-data_type    = 'CHAR'
                                            charactdetail-length       = 1
                                            charactdetail-decimals     = 0
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = ''
                                            char_values                = VALUE #( ( value_char = '' )
                                                                                  ( value_char = 'Y' )
                                                                                  ( value_char = 'N' ) )
                                            char_descr                 = VALUE #( ( language_int = 'E' language_iso = 'EN' value_char = ''  description = 'Unknown' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'Y' description = 'True' )
                                                                                  ( language_int = 'E' language_iso = 'EN' value_char = 'N' description = 'False' ) )
                                            description                = VALUE #( ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix }Is Expanded| ) ) )
                                      ).
      APPEND LINES OF lt_pckfrac_charac_params TO mt_charac_to_create.
      CLEAR lt_class_characts_params.

      LOOP AT lt_pckfrac_charac_params REFERENCE INTO DATA(lr_charact_pckfrac_params).

        APPEND VALUE #( name_char = lr_charact_pckfrac_params->charactdetail-charact_name ) TO lt_class_characts_params.

      ENDLOOP.

      lv_function_id = 1.
      DO lv_fnction_create_count TIMES.

      IF iv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_matcls.
        lv_function_id_str = |{ lv_function_id }|.
        lv_description_prefix_fract = |{ lv_fraction_id }|.
        lv_description_prefix_func = |{ lv_function_id }|.
      ELSEIF iv_scope = /vpcoe/cl_rdp_clas_data_create=>gv_scope_specdb.
        lv_function_id_str = ''.
        lv_description_prefix = ''.
      ENDIF.

      lt_pckfunc_charac_params = VALUE #( ( charactdetail-charact_name = |{ iv_charac_prefix }_PACKELEM_FRAC{ lv_fraction_id_str }_FUNCTION{ lv_function_id_str }|
                                            charactdetail-data_type    = 'CHAR'
                                            charactdetail-length       = 9
                                            charactdetail-decimals     = 0
                                            charactdetail-status       = '1'
                                            charactdetail-value_assignment = 'S'
                                            charactdetail-additional_values = 'X'
                                            char_values  = VALUE #(
                                              ( value_char = '10.100' )
                                              ( value_char = '10.101' )
                                              ( value_char = '10.102' )
                                              ( value_char = '10.103' )
                                              ( value_char = '10.104' )
                                              ( value_char = '10.105' )
                                              ( value_char = '10.106' )
                                              ( value_char = '10.107' )
                                              ( value_char = '10.108' )
                                              ( value_char = '10.109' )
                                              ( value_char = '10.110' )
                                              ( value_char = '10.111' )
                                              ( value_char = '10.112' )
                                              ( value_char = '10.113' )

                                              ( value_char = '20.100' )
                                              ( value_char = '20.101' )
                                              ( value_char = '20.102' )
                                              ( value_char = '20.103' )
                                              ( value_char = '20.104' )
                                              ( value_char = '20.105' )
                                              ( value_char = '20.106' )
                                              ( value_char = '20.107' )

                                              ( value_char = '30.100' )
                                              ( value_char = '30.101' )
                                              ( value_char = '30.102' )
                                             )
                                            char_descr = VALUE #(
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.100' description = |{ lc_funcgroupname-funcgroupname_10 }:UV stabilizers| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.101' description = |{ lc_funcgroupname-funcgroupname_10 }:AA scavengers or blockers| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.102' description = |{ lc_funcgroupname-funcgroupname_10 }:Oxygen scavengers| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.103' description = |{ lc_funcgroupname-funcgroupname_10 }:Slip and anti-block agents| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.104' description = |{ lc_funcgroupname-funcgroupname_10 }:PAE| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.105' description = |{ lc_funcgroupname-funcgroupname_10 }:Other Additives and Fillers| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.106' description = |{ lc_funcgroupname-funcgroupname_10 }:Flame Retardant| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.107' description = |{ lc_funcgroupname-funcgroupname_10 }:Plasticiser| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.108' description = |{ lc_funcgroupname-funcgroupname_10 }:Lubricants| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.109' description = |{ lc_funcgroupname-funcgroupname_10 }:Peroxide| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.110' description = |{ lc_funcgroupname-funcgroupname_10 }:Nucleating Agents| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.111' description = |{ lc_funcgroupname-funcgroupname_10 }:Optical Brighteners| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.112' description = |{ lc_funcgroupname-funcgroupname_10 }:Mineral Fillers (Inorganic)| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '10.113' description = |{ lc_funcgroupname-funcgroupname_10 }:Organic Fillers| )

                                              ( language_int = 'E' language_iso = 'EN' value_char = '20.100' description = |{ lc_funcgroupname-funcgroupname_20 }:AlOx| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '20.101' description = |{ lc_funcgroupname-funcgroupname_20 }:SiOx| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '20.102' description = |{ lc_funcgroupname-funcgroupname_20 }:EVOH| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '20.103' description = |{ lc_funcgroupname-funcgroupname_20 }:PVOH| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '20.104' description = |{ lc_funcgroupname-funcgroupname_20 }:PVDC| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '20.105' description = |{ lc_funcgroupname-funcgroupname_20 }:Metallization| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '20.106' description = |{ lc_funcgroupname-funcgroupname_20 }:PA| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '20.107' description = |{ lc_funcgroupname-funcgroupname_20 }:Other Barriers| )

                                              ( language_int = 'E' language_iso = 'EN' value_char = '30.100' description = |{ lc_funcgroupname-funcgroupname_30 }:Carbon plasma coating| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '30.101' description = |{ lc_funcgroupname-funcgroupname_30 }:Wax coating| )
                                              ( language_int = 'E' language_iso = 'EN' value_char = '30.102' description = |{ lc_funcgroupname-funcgroupname_30 }:Other Coatings| )
                                             )

                                            description = VALUE #(
                                              ( language_int = 'E' language_iso = 'EN' description = |{ lv_description_prefix_fract }: Function Group: Function { lv_description_prefix_func }| ) ) )
                                      ).

      APPEND LINES OF lt_pckfunc_charac_params TO mt_charac_to_create.
      LOOP AT lt_pckfunc_charac_params REFERENCE INTO DATA(lr_charact_pckfunc_params).

        APPEND VALUE #( name_char = lr_charact_pckfunc_params->charactdetail-charact_name ) TO lt_class_characts_params.

      ENDLOOP.

      lv_function_id = lv_function_id + 1.

      ENDDO.

      lt_pckfrac_classi_params = VALUE #( ( classnumber       = iv_classi_prefix && lv_frac_clas_mid_str && lv_fraction_id_str
                                            classtype         = lv_classtype
                                            classbasicdata    = VALUE #( status = '1' same_value_no = 'X' valid_from = '00010101' valid_to = '99991231' )
                                            description       = VALUE #( ( langu = 'E' langu_iso = 'EN' catchword = 'Packaging Element Fraction ' && lv_fraction_id_str ) )
                                            classcharacteristics = lt_class_characts_params ) ).

      APPEND LINES OF lt_pckfrac_classi_params TO mt_classi_to_create.
      lv_fraction_id = lv_fraction_id + 1.
    ENDDO.

  ENDMETHOD.


  METHOD setup_packelem_main_and_subord.
    DATA lv_classtype             TYPE bapi_class_key-classtype.

    DATA lt_subcomponent_characteristcs TYPE mty_tty_characteristics.
    DATA lt_subcomponent_charists_names TYPE STANDARD TABLE OF bapi1003_charact WITH NON-UNIQUE DEFAULT KEY.

    DATA lt_component_characteristics TYPE mty_tty_characteristics.
    DATA lt_component_charists_names TYPE STANDARD TABLE OF bapi1003_charact WITH NON-UNIQUE DEFAULT KEY.

    DATA lt_classifications TYPE mty_tty_classifications.

    lt_component_characteristics = VALUE #(
        charactdetail-status            = '1'
        charactdetail-value_assignment  = 'S'
        charactdetail-additional_values = 'X'
        ( charactdetail-charact_name            = |{ iv_charac_prefix }_PACKCOMP_COVERAGE|
          charactdetail-data_type               = 'NUM'
          charactdetail-length                  = 5
          charactdetail-decimals                = 2
*          charactdetail-template                = '___,__'
*          charactdetail-template_long           = '___,__'
          charactdetail-unit_of_measurement     = '%'
          charactdetail-unit_of_measurement_iso = 'P1'
          num_values                            = VALUE #( ( value_from = 0 value_relation = '1' ) )
          description                           = VALUE #(
              ( language_int = 'E' language_iso = 'EN' description = 'Percentage of area covered' ) ) )
        ( charactdetail-charact_name            = |{ iv_charac_prefix }_PACKCOMP_USAGE|
          charactdetail-data_type               = 'CHAR'
          charactdetail-case_sensitiv           = ''
          charactdetail-length                  = 10
          charactdetail-decimals                = 0
          char_values                           = VALUE #( ( value_char = 'C' )
                                                           ( value_char = 'H' )
                                                           ( value_char = 'I' )
                                                           ( value_char = 'T' ) )
          char_descr                            = VALUE #(
              language_int = 'E'
              language_iso = 'EN'
              ( value_char = 'C' description = 'Commercial' )
              ( value_char = 'H' description = 'Household' )
              ( value_char = 'I' description = 'Industrial' )
              ( value_char = 'T' description = 'Transport' ) )
          description                           = VALUE #(
              ( language_int = 'E' language_iso = 'EN' description = 'Packaging elem. usage context' ) ) )
        ( charactdetail-charact_name            = |{ iv_charac_prefix }_PACKCOMP_LEVEL|
          charactdetail-data_type               = 'CHAR'
          charactdetail-case_sensitiv           = ''
          charactdetail-length                  = 10
          charactdetail-decimals                = 0
          char_values                           = VALUE #( ( value_char = '01' )
                                                           ( value_char = '10' )
                                                           ( value_char = '11' )
                                                           ( value_char = '20' )
                                                           ( value_char = '30' ) )
          char_descr                            = VALUE #(
              language_int = 'E'
              language_iso = 'EN'
              ( value_char = '01' description = 'Packaging as Product' )
              ( value_char = '10' description = 'Primary Packaging' )
              ( value_char = '11' description = 'Sub-primary Packaging' )
              ( value_char = '20' description = 'Secondary Packaging' )
              ( value_char = '30' description = 'Transport Packaging, aka. Tertiary Packaging' ) )
          description                           = VALUE #(
              ( language_int = 'E' language_iso = 'EN' description = 'The level of the packaging el.' ) ) ) ).


    LOOP AT lt_component_characteristics REFERENCE INTO DATA(lr_c).
      APPEND VALUE #( name_char = lr_c->charactdetail-charact_name ) TO lt_component_charists_names.
    ENDLOOP.
    APPEND LINES OF lt_component_characteristics TO mt_charac_to_create.

    IF iv_scope = gv_scope_specdb.
      lt_component_characteristics = VALUE #(
          charactdetail-status            = '1'
          charactdetail-value_assignment  = 'S'
          charactdetail-additional_values = 'X'
          ( charactdetail-charact_name  = |{ iv_charac_prefix }_PACKCOMP_ELEMENT|
            charactdetail-data_type     = 'CHAR'
            charactdetail-case_sensitiv = ''
            charactdetail-length        = 12
            charactdetail-decimals      = 0
            description                 = VALUE #(
                ( language_int = 'E' language_iso = 'EN' description = 'Referenced Packaging Element' ) ) ) ).
      APPEND VALUE #( name_char = |{ iv_charac_prefix }_PACKCOMP_ELEMENT| ) TO lt_component_charists_names.
      APPEND LINES OF lt_component_characteristics TO mt_charac_to_create.
    ENDIF.

    lt_subcomponent_characteristcs = VALUE #(
        charactdetail-status            = '1'
        charactdetail-value_assignment  = 'S'
        charactdetail-additional_values = ''
        ( charactdetail-charact_name  = |{ iv_charac_prefix }_PACKCOMP_SEPARABILITY|
          charactdetail-data_type     = 'CHAR'
          charactdetail-case_sensitiv = 'X'
          charactdetail-length        = 20
          charactdetail-decimals      = 0
          char_values                 = VALUE #(
              ( value_char = 'N-NOT'                   )
              ( value_char = 'M-CONSUMPTION'           )
              ( value_char = 'M-CONSUMER'              )
              ( value_char = 'D-WATER'                 )
              ( value_char = 'D-ALK'                   )
              ( value_char = 'T-TECH'                  ) )
          char_descr                  = VALUE #(
              language_int = 'E'
              language_iso = 'EN'
              ( value_char = 'N-NOT'                 description = 'Not separable' )
              ( value_char = 'M-CONSUMPTION'         description = 'By consumption' )
              ( value_char = 'M-CONSUMER'            description = 'By consumer - reassembly possible' )
              ( value_char = 'D-WATER'               description = 'Detachable in water' )
              ( value_char = 'D-ALK'                 description = 'Detachable in alkaline fluid' )
              ( value_char = 'T-TECH'                description = 'Technically separable' ) )
          description                 = VALUE #(
              ( language_int = 'E' language_iso = 'EN' description = 'Separability of subcomponent' ) ) ) ).

    APPEND VALUE #( name_char = |{ iv_charac_prefix }_PACKCOMP_USAGE| ) TO lt_subcomponent_charists_names.
    IF iv_scope = gv_scope_specdb.
        APPEND VALUE #( name_char = |{ iv_charac_prefix }_PACKCOMP_ELEMENT| ) TO lt_subcomponent_charists_names.
    ENDIF.

    LOOP AT lt_subcomponent_characteristcs REFERENCE INTO DATA(lr_sc).
      APPEND VALUE #( name_char = lr_sc->charactdetail-charact_name ) TO lt_subcomponent_charists_names.
    ENDLOOP.

    APPEND LINES OF lt_subcomponent_characteristcs TO mt_charac_to_create.

    IF iv_scope = gv_scope_specdb.
      lv_classtype = '100'.
    ELSEIF iv_scope = gv_scope_main_and_subordinate.
      lv_classtype = '023'.
    ENDIF.

    lt_classifications = VALUE #(
        ( classnumber          = |{ iv_classi_prefix }_PACKCOMP_ATTR|
          classtype            = lv_classtype
          classbasicdata       = VALUE #( status        = '1'
                                          same_value_no = 'X'
                                          valid_from    = '00010101'
                                          valid_to      = '99991231' )
          description          = VALUE #(
              ( langu = 'E' langu_iso = 'EN' catchword = 'Pckg. comp. main packaging element' ) )
          classcharacteristics = lt_component_charists_names )
        ( classnumber          = |{ iv_classi_prefix }_PACKSUBC_ATTR|
          classtype            = lv_classtype
          classbasicdata       = VALUE #( status        = '1'
                                          same_value_no = 'X'
                                          valid_from    = '00010101'
                                          valid_to      = '99991231' )
          description          = VALUE #(
              ( langu = 'E' langu_iso = 'EN' catchword = 'Pckg. comp. subord. packaging element' ) )
          classcharacteristics = lt_subcomponent_charists_names )
          ).
    APPEND LINES OF lt_classifications TO mt_classi_to_create.
  ENDMETHOD.


  METHOD update_characteristic_data.
    LOOP AT mt_charac_to_update REFERENCE INTO DATA(lr_char_to_update).

      DATA lt_charactdetailnew TYPE TABLE OF bapicharactdetail.
      CLEAR lt_charactdetailnew.
      APPEND lr_char_to_update->charactdetail TO lt_charactdetailnew.

      DATA lt_bapireturn TYPE TABLE OF bapiret2 WITH EMPTY KEY.
      CALL FUNCTION 'BAPI_CHARACT_CHANGE'
        EXPORTING charactname           = lr_char_to_update->charactdetail-charact_name
        TABLES    charactdetailnew      = lt_charactdetailnew
                  charactdescrnew       = lr_char_to_update->description
                  charactvaluesnumnew   = lr_char_to_update->num_values
                  charactvaluescharnew  = lr_char_to_update->char_values
                  charactvaluesdescrnew = lr_char_to_update->char_descr
                  charactvaluescurrnew  = lr_char_to_update->curr_values
                  return                = lt_bapireturn.

      DATA(lt_errors) = VALUE string_table( FOR <ls_bapireturn> IN lt_bapireturn WHERE ( type = 'E' ) ( CONV #( <ls_bapireturn>-message ) ) ).
      IF lt_errors IS INITIAL.
        DATA: lv_success TYPE stringval.
        LOOP AT lt_bapireturn INTO DATA(ls_bapireturn) WHERE type <> 'E'.
          CONCATENATE lv_success ls_bapireturn-message INTO lv_success SEPARATED BY ' / '.
        ENDLOOP.
        APPEND VALUE #( char_name = lr_char_to_update->charactdetail-charact_name
                        msg       = lv_success ) TO mt_updated_char.
        commit_transaction( ).
      ELSE.
        CONCATENATE LINES OF lt_errors INTO DATA(lv_error) SEPARATED BY ' / '.
        CLEAR lt_errors.
        APPEND VALUE #( char_name = lr_char_to_update->charactdetail-charact_name
                        msg       = lv_error ) TO mt_fail_update_char.
        rollback_transaction( ).
      ENDIF.
      CLEAR lt_bapireturn.
    ENDLOOP.
  ENDMETHOD.


  METHOD update_classification_data.
    LOOP AT mt_classi_to_update REFERENCE INTO DATA(lr_clas_to_update).
      DATA lt_bapireturn                 TYPE TABLE OF bapiret2 WITH EMPTY KEY.
      DATA previous_classbasicdata       TYPE bapi1003_basic.
      DATA previous_classdescriptions_r  TYPE TABLE OF bapi1003_catch_r.
      DATA previous_classdescriptions    TYPE TABLE OF bapi1003_catch.
      DATA previous_classcharacteristics TYPE TABLE OF bapi1003_charact_r.

      CALL FUNCTION 'BAPI_CLASS_GETDETAIL'
        EXPORTING
          classnum             = lr_clas_to_update->classnumber
          classtype            = lr_clas_to_update->classtype
        IMPORTING
          classbasicdata       = previous_classbasicdata
        TABLES
          classdescriptions    = previous_classdescriptions_r
          classcharacteristics = previous_classcharacteristics.

      " make sure we only add but do not delete any assignments of characteristics to this class
      LOOP AT previous_classcharacteristics REFERENCE INTO DATA(lr_previous_char).
        IF NOT line_exists( lr_clas_to_update->classcharacteristics[ name_char = lr_previous_char->name_char ] ).
          APPEND lr_previous_char->* TO lr_clas_to_update->classcharacteristics.
        ENDIF.
      ENDLOOP.

      MOVE-CORRESPONDING previous_classdescriptions_r TO previous_classdescriptions.

      CALL FUNCTION 'BAPI_CLASS_CHANGE'
        EXPORTING
          classnum                = lr_clas_to_update->classnumber
          classtype               = lr_clas_to_update->classtype
          classbasicdata          = previous_classbasicdata
          classbasicdatanew       = lr_clas_to_update->classbasicdata
        TABLES
          return                  = lt_bapireturn
          classdescriptions       = previous_classdescriptions
          classdescriptionsnew    = lr_clas_to_update->description
          classcharacteristics    = previous_classcharacteristics
          classcharacteristicsnew = lr_clas_to_update->classcharacteristics.

      DATA(lt_errors) = VALUE string_table( FOR <ls_bapireturn> IN lt_bapireturn WHERE ( type = 'E' ) ( CONV #( <ls_bapireturn>-message ) ) ).
      IF lt_errors IS INITIAL.
        DATA: lv_success TYPE stringval.
        LOOP AT lt_bapireturn INTO DATA(ls_bapireturn) WHERE type <> 'E'.
          CONCATENATE lv_success ls_bapireturn-message INTO lv_success SEPARATED BY ' / '.
        ENDLOOP.
        APPEND VALUE #( class_number = lr_clas_to_update->classnumber
                        msg          = lv_success ) TO mt_updated_clas.
        commit_transaction( ).
      ELSE.
        CONCATENATE LINES OF lt_errors INTO DATA(lv_error) SEPARATED BY ' / '.
        CLEAR lt_errors.
        APPEND VALUE #( class_number = lr_clas_to_update->classnumber
                        msg          = lv_error ) TO mt_fail_update_clas.
        rollback_transaction( ).
      ENDIF.
      CLEAR lt_bapireturn.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
