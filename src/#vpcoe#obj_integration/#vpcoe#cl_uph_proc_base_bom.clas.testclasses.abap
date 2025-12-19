*CLASS tcl_bom_expl_wrapper DEFINITION FOR TESTING.
*
*  PUBLIC SECTION.
*
*    INTERFACES: lif_ext_modules_wrapper.
*
*    METHODS:
*      constructor
*        IMPORTING
*          !iv_testcase TYPE i.
*
*  PRIVATE SECTION.
*
*    DATA:
*       mv_testcase TYPE i.
*
*ENDCLASS.
*
*CLASS tcl_bom_expl_wrapper IMPLEMENTATION.
*
*  METHOD lif_ext_modules_wrapper~get_material_detail.
*  ENDMETHOD.
*
*  METHOD lif_ext_modules_wrapper~call_bom_explosion.
*
*    CLEAR: es_exp_bom_hdr, et_exp_bom_items, et_exp_bom_nodes.
*
*    CASE mv_testcase.
*      WHEN 1.
*
*        es_exp_bom_hdr = VALUE #( matnr = 'RDP-BM-PC-ORDPACK' maktx = 'Packaging ordinary hazard' werks = '0001' prwrk = '0001' stlty = 'M'
*                          stlnr = '00001353' stlan = '1' stlal = '01' bmeng = '1.000' emmbm = '1.000' bmein = 'ST' emeng = '1.000'
*                          emgme = 'PC' datuv = '20220101' datub = '99991231' andat = '20220405' stlst = '01' ).
*
*        et_exp_bom_items = VALUE #(
*        ( stufe = '1' wegxx = '1' bmtyp = 'M' ttidx = '1' ojtxb = 'Packaging ordinary hazard' stlan = '1' stlal = '01' mnglg = '6.000'
*        mngko = '6.000' msign = '+' stlty = 'M' stlnr = '00001353' stlkn = '00000001' stpoz = '00000002' datuv = '20220101' andat =
*        '20220405' postp = 'L' posnr = '0010' meins = 'ST' menge = '6.000' datub = '99991231' index = '1' )
*        ( stufe = '1' wegxx = '2' bmtyp = 'M' ttidx = '1' ojtxb = 'Packaging ordinary hazard' stlan = '1' stlal = '01' mnglg = '1.000'
*        mngko = '1.000' msign = '+' stlty = 'M' stlnr = '00001353' stlkn = '00000002' stpoz = '00000004' datuv = '20220101' andat =
*        '20220405' postp = 'L' posnr = '0020' meins = 'ST' menge = '1.000' datub = '99991231' index = '2' )
*        ( stufe = '1' wegxx = '3' bmtyp = 'M' ttidx = '1' ojtxb = 'Packaging ordinary hazard' stlan = '1' stlal = '01' mnglg = '1.000'
*        mngko = '1.000' msign = '+' stlty = 'M' stlnr = '00001353' stlkn = '00000003' stpoz = '00000006' datuv = '20220101' andat =
*        '20220405' postp = 'L' posnr = '0030' meins = 'ST' menge = '1.000' datub = '99991231' index = '3' )
*        ( stufe = '1' wegxx = '4' bmtyp = 'M' ttidx = '1' ojtxb = 'Packaging ordinary hazard' stlan = '1' stlal = '01' mnglg = '1.000'
*        mngko = '1.000' msign = '+' stlty = 'M' stlnr = '00001353' stlkn = '00000004' stpoz = '00000008' datuv = '20220101' andat =
*        '20220405' postp = 'L' posnr = '0040' meins = 'ST' menge = '1.000' datub = '99991231' index = '4' )
*         ).
*
*        et_exp_bom_nodes = VALUE #(
*        ( matnr = 'RDP-BM-PC-ORDPACK' prwrk = '0001' index = '1' )
*         ).
*
*      WHEN 2.
*        es_exp_bom_hdr = VALUE #( matnr = 'RDP-BM-PC-ORDPACK' ).
*
*      WHEN OTHERS.
*    ENDCASE.
*
*  ENDMETHOD.
*
*  METHOD constructor.
*    mv_testcase = iv_testcase.
*  ENDMETHOD.
*
*ENDCLASS.
*
*CLASS tcl_surdp_uph_proc_base_bom_ip DEFINITION DEFERRED.
*CLASS /vpcoe/cl_uph_proc_base_bom DEFINITION LOCAL FRIENDS tcl_surdp_uph_proc_base_bom_ip.
*
*CLASS tcl_surdp_uph_proc_base_bom_ip DEFINITION FOR TESTING
*  INHERITING FROM /vpcoe/cl_uph_proc_base_bom.
*
*  PUBLIC SECTION.
*
*    METHODS:
*      constructor,
*
*      get_determined_records RETURNING VALUE(rt_keys) TYPE /vpcoe/surdps_uph_bom_key_t,
*
*      /vpcoe/if_uph_entity_bom_proc~map_bom_data REDEFINITION.
*
*ENDCLASS.
*
*CLASS tcl_surdp_uph_proc_base_bom_ip IMPLEMENTATION.
*
*  METHOD constructor.
*
*    "init processor without BOM explosion
*    super->constructor( ).
*
*    "set empty bom explosion wrapper
*    set_ext_modules_wrapper( NEW tcl_bom_expl_wrapper( 0 ) ).
*
*  ENDMETHOD.
*
*  METHOD get_determined_records.
*    rt_keys = mt_bom_keys.
*  ENDMETHOD.
*
*  METHOD /vpcoe/if_uph_entity_bom_proc~map_bom_data.
** Test mapping: For each BOM header/item create a simple composition/item
*
*    "call parent to ensure coverage
*    super->/vpcoe/if_uph_entity_bom_proc~map_bom_data( it_bom_data ).
*
*    LOOP AT it_bom_data INTO DATA(lr_bom_data).
*
*      DATA(lt_cmp_item_data) = VALUE surdpt_uph_entity_data( ).
*
*      LOOP AT lr_bom_data->get_items(  ) INTO DATA(lr_bom_item).
*
*        DATA(lo_pckg_comp_item) = NEW cl_surdp_uph_ent_pckg_cmp_item( is_data = VALUE #( packagingelementdisplayid = lr_bom_item->get_material(  ) ) ).
*        APPEND lo_pckg_comp_item TO lt_cmp_item_data.
*
*      ENDLOOP.
*
*      DATA(lo_pckg_comp) = NEW cl_surdp_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data = VALUE #( displayid = lr_bom_data->get_material( )  ) it_cmp_item_data = lt_cmp_item_data it_products = VALUE #( ) ).
*
*      APPEND lo_pckg_comp TO rt_entity_data.
*
*    ENDLOOP.
*
*  ENDMETHOD.
*
*ENDCLASS.
*
*CLASS tcl_surdp_uph_proc_base_bom DEFINITION FOR TESTING
*  DURATION SHORT RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    CLASS-DATA:
*      mo_osql_env TYPE REF TO if_osql_test_environment.
*
*    DATA:
*      f_cut             TYPE REF TO tcl_surdp_uph_proc_base_bom_ip.  "class under test
*
*    CLASS-METHODS:
*      class_setup,
*      class_teardown.
*
*    METHODS: setup.
*    METHODS: teardown.
*
*    METHODS: test_prepare_process_w_selpar FOR TESTING.
*    METHODS: test_prepare_process_delta FOR TESTING.
*    METHODS: test_process_package FOR TESTING.
*    METHODS: test_process_package_w_empty FOR TESTING.
*
*    METHODS: test_map_bom_data FOR TESTING.
*
*    METHODS: test_determine_bom_w_valchg FOR TESTING.
*    METHODS: test_determine_bom_w_alter FOR TESTING.
*    METHODS: test_determine_bom_w_status FOR TESTING.
*    METHODS: test_determine_bom_w_usage FOR TESTING.
*    METHODS: test_determine_bom_w_mattyp FOR TESTING.
*
*ENDCLASS.
*
*CLASS tcl_surdp_uph_proc_base_bom IMPLEMENTATION.
*
*
*  METHOD class_setup.
*
*    mo_osql_env = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'STKO' ) ( 'STPO' ) ( 'MAST' ) ( 'STZU' ) ( 'STAS' ) ( 'MARA' )
*                                                                                 ( 'SURDPD_UPH_PROT' )
*                                                                                ) ).
*
*  ENDMETHOD.
*
*  METHOD class_teardown.
*
*    mo_osql_env->destroy(  ).
*
*  ENDMETHOD.
*
*  METHOD setup.
*
*    th_surdp_uph_factory_injector=>inject_factory_double( io_double = NEW td_surdp_uph_factory(  ) ).
*
*    f_cut = NEW tcl_surdp_uph_proc_base_bom_ip( ).
*
*    mo_osql_env->clear_doubles(  ).
*
*    "Setup test data scenario
*    "MAST
*    DATA: lt_mast_data TYPE STANDARD TABLE OF mast.
*    lt_mast_data = VALUE #(
*        ( matnr ='RDP-BM-PUMP' werks ='0001' stlan ='1' stlnr ='00001355' stlal ='01' losvn ='1.000 ' losbs ='99.000 ' andat ='20220405' aedat ='20220405' cslty =''
*        material_bom_key = 'RDP-BM-PUMP                             0001101'  )
*        ( matnr ='RDP-BM-PUMP' werks ='0001' stlan ='1' stlnr ='00001355' stlal ='02' losvn ='100.000 ' losbs ='999.000 ' andat ='20220405' aedat ='20220405' cslty =''
*        material_bom_key = 'RDP-BM-PUMP                             0001102'  )
*
*        ( matnr ='RDP-BM-PC-ORDPACK' werks ='0001' stlan ='1' stlnr ='00001353' stlal ='01' losvn ='0.000 ' losbs ='0.000 ' cslty =''
*          material_bom_key ='RDP-BM-PC-ORDPACK                       0001101'  )
*    ).
*    mo_osql_env->get_double( 'mast' )->insert( cl_osql_test_data=>create( lt_mast_data ) ).
*
*    "MARA
*    DATA: lt_mara_data TYPE STANDARD TABLE OF mara.
*    lt_mara_data = VALUE #(
*        ( matnr ='RDP-BM-PUMP' mtart ='FERT' meins = 'PC')
*        ( matnr ='RDP-BM-PC-ORDPACK' mtart = 'VERP'  meins = 'PC' )
*        ( matnr ='RDP-BM-MOTOR-S' mtart = 'HALB' meins = 'PC'  )
*        ( matnr ='RDP-BM-BATPACK' mtart = 'HALB' meins = 'PC'  )
*        ( matnr ='RDP-BM-PC-CPLPACK' mtart = 'VERP' meins = 'PC'  )
*        ( matnr ='RDP-BM-PE-WDSCREW' mtart = 'VERP' meins = 'PC'  )
*        ( matnr ='RDP-BM-PE-LABEL' mtart = 'VERP' meins = 'PC'  )
*        ( matnr ='RDP-BM-PE-BAG' mtart = 'VERP' meins = 'PC'  )
*        ( matnr ='RDP-BM-PE-MOT-LINING' mtart = 'VERP' meins = 'PC'  )
*    ).
*    mo_osql_env->get_double( 'mara' )->insert( cl_osql_test_data=>create( lt_mara_data ) ).
*
*    "STKO
*    DATA: lt_stko_data TYPE STANDARD TABLE OF stko.
*    lt_stko_data = VALUE #(
*      ( stlty ='M' stlnr ='00001355' stlal ='01' stkoz ='00000001' datuv ='20220101' aennr ='' lkenz ='' loekz ='' vgkzl ='00000000' andat ='20220405' aedat ='00000000' bmein ='ST' bmeng ='1.000 ' cadkz ='' labor =''
*        ltxsp ='' stktx ='' stlst ='01' wrkan ='0001' dvdat ='00000000' valid_to ='00000000' ecn_to ='' bom_versn ='' versnst ='' versnlastind ='' lastchangedatetime ='0.0000000 '
*        bom_ain_ind ='' bom_prev_versn ='' dummy_stko_incl_eew_ps =''  )
*      ( stlty ='M' stlnr ='00001355' stlal ='02' stkoz ='00000010' datuv ='20220101' aennr ='' lkenz ='' loekz ='' vgkzl ='00000000' andat ='20220405' aedat ='00000000' bmein ='ST' bmeng ='1.000 ' cadkz ='' labor =''
*        ltxsp ='' stktx ='' stlst ='01' wrkan ='0001' dvdat ='00000000' valid_to ='00000000' ecn_to ='' bom_versn ='' versnst ='' versnlastind ='' lastchangedatetime ='0.0000000 '
*        bom_ain_ind ='' bom_prev_versn ='' dummy_stko_incl_eew_ps =''  )
*
*      ( stlty ='M' stlnr ='00001353' stlal ='01' stkoz ='00000001' datuv ='20220101' techv ='' aennr ='' lkenz ='' loekz ='' vgkzl ='00000000' andat ='20220405' aedat ='00000000'  bmein ='ST'
*        bmeng ='1.000 ' cadkz ='' labor ='' ltxsp ='' stktx ='' stlst ='01' wrkan ='0001' dvdat ='00000000' dvnam ='' aehlp ='00' alekz ='' valid_to ='00000000' ecn_to ='' bom_versn ='' versnst =''
*        versnlastind ='' lastchangedatetime ='0.0000000 '  )
*      ).
*    mo_osql_env->get_double( 'stko' )->insert( cl_osql_test_data=>create( lt_stko_data ) ).
*
*    "STPO
*    DATA: lt_stpo_data TYPE STANDARD TABLE OF stpo.
*
*    lt_stpo_data = VALUE #(
*      ( stlty ='M' stlnr ='00001355' stlkn ='00000001' stpoz ='00000002' datuv ='20220101' aennr ='' lkenz ='' vgknt ='00000000' vgpzl ='00000000' andat ='20220405' aedat ='00000000' idnrk ='RDP-BM-MOTOR-S' pswrk ='' postp ='L'
*      posnr ='0010' meins =
*        'ST' menge ='1.000 ' valid_to ='00000000'  )
*       ( stlty ='M' stlnr ='00001355' stlkn ='00000002' stpoz ='00000004' datuv ='20220101' aennr ='' lkenz ='' vgknt ='00000000' vgpzl ='00000000' andat ='20220405' aedat ='00000000' idnrk ='RDP-BM-BATPACK' pswrk ='' postp ='L'
*       posnr ='0020' meins =
*        'ST' menge ='1.000 ' valid_to ='00000000'  )
*       ( stlty ='M' stlnr ='00001355' stlkn ='00000003' stpoz ='00000006' datuv ='20220101' aennr ='' lkenz ='' vgknt ='00000000' vgpzl ='00000000' andat ='20220405' aedat ='00000000' idnrk ='RDP-BM-PC-ORDPACK' pswrk ='' postp ='L'
*       posnr ='0030' sortf =''
*         meins ='ST' menge ='1.000 ' valid_to ='00000000'  )
*       ( stlty ='M' stlnr ='00001355' stlkn ='00000004' stpoz ='00000008' datuv ='20220101' aennr ='' lkenz ='' vgknt ='00000000' vgpzl ='00000000' andat ='20220405' aedat ='00000000' idnrk ='' pswrk ='' postp ='D'
*       posnr ='0040' meins ='ST' menge =
*         '1.000 ' valid_to ='00000000'  )
*       ( stlty ='M' stlnr ='00001355' stlkn ='00000005' stpoz ='00000011' datuv ='20220101' aennr ='' lkenz ='' vgknt ='00000000' vgpzl ='00000000' andat ='20220405' aedat ='00000000' idnrk ='RDP-BM-MOTOR-S' pswrk ='' postp ='L'
*       posnr ='0010' meins =
*        'ST' menge ='1.000 ' valid_to ='00000000'  )
*       ( stlty ='M' stlnr ='00001355' stlkn ='00000006' stpoz ='00000013' datuv ='20220101' aennr ='' lkenz ='' vgknt ='00000000' vgpzl ='00000000' andat ='20220405' aedat ='00000000' idnrk ='RDP-BM-BATPACK' pswrk ='' postp ='L'
*       posnr ='0020' meins =
*        'ST' menge ='1.000 ' valid_to ='00000000'  )
*       ( stlty ='M' stlnr ='00001355' stlkn ='00000007' stpoz ='00000015' datuv ='20220101' aennr ='' lkenz ='' vgknt ='00000000' vgpzl ='00000000' andat ='20220405' aedat ='00000000' idnrk ='RDP-BM-PC-CPLPACK' pswrk ='' postp ='L'
*       posnr ='0030' sortf =''
*        meins ='ST' menge ='1.000 ' valid_to ='00000000'  )
*
*       ( stlty ='M' stlnr ='00001353' stlkn ='00000001' stpoz ='00000002' datuv ='20220101' techv ='' aennr ='' lkenz ='' vgknt ='00000000' vgpzl ='00000000' andat ='20220405'  aedat ='00000000'  idnrk ='RDP-BM-PE-WDSCREW' pswrk ='' postp ='L'
*        posnr ='0010' sortf ='' meins ='ST' menge ='6.000 ' fmeng =''  )
*       ( stlty ='M' stlnr ='00001353' stlkn ='00000002' stpoz ='00000004' datuv ='20220101' techv ='' aennr ='' lkenz ='' vgknt ='00000000' vgpzl ='00000000' andat ='20220405'  aedat ='00000000'  idnrk ='RDP-BM-PE-LABEL' pswrk ='' postp ='L'
*       posnr ='0020' sortf ='' meins ='ST' menge ='1.000 ' fmeng =''  )
*       ( stlty ='M' stlnr ='00001353' stlkn ='00000003' stpoz ='00000006' datuv ='20220101' techv ='' aennr ='' lkenz ='' vgknt ='00000000' vgpzl ='00000000' andat ='20220405'  aedat ='00000000'  idnrk ='RDP-BM-PE-BAG' pswrk ='' postp ='L'
*       posnr ='0030' sortf ='' meins ='ST' menge ='1.000 ' fmeng =''  )
*       ( stlty ='M' stlnr ='00001353' stlkn ='00000004' stpoz ='00000008' datuv ='20220101' techv ='' aennr ='' lkenz ='' vgknt ='00000000' vgpzl ='00000000' andat ='20220405'  aedat ='00000000'  idnrk ='RDP-BM-PE-MOT-LINING' pswrk ='' postp ='L'
*       posnr ='0040' sortf ='' meins ='ST' menge ='1.000 ' fmeng =''  )
*     ).
*    mo_osql_env->get_double( 'stpo' )->insert( cl_osql_test_data=>create( lt_stpo_data ) ).
*
*    "STZU
*    DATA: lt_stzu_data TYPE STANDARD TABLE OF stzu.
*
*    lt_stzu_data = VALUE #( ( stlty ='M' stlnr ='00001355' stlan ='1' exstl ='' altst ='X' varst ='' kbaus ='' ltxsp ='' stlbe ='' ztext ='' wrkan ='0001' hisdt ='X' hissr ='' histk ='' stuez ='00000016' maxkn ='00000007' kzpln ='' aenrl =''
*                                clsmx ='00000000' stldt ='20220405' stltm ='163757' maxkan ='000000' tstmp ='20220405143757 ' versnind =''  )
*
*                             ( stlty ='M' stlnr ='00001353' stlan ='1' exstl ='' altst ='' varst ='' kbaus ='' ltxsp ='' stlbe ='' ztext ='' wrkan ='0001' hisdt ='X' hissr ='' histk ='' stuez ='00000009' maxkn ='00000004' kzpln ='' aenrl =''
*                                clsmx ='00000000' stldt ='20220406' stltm ='093701' maxkan ='000000' tstmp ='20220406073701 ' versnind =''  )
*    ).
*    mo_osql_env->get_double( 'stzu' )->insert( cl_osql_test_data=>create( lt_stzu_data ) ).
*
*    "STAS
*    DATA: lt_stas_data TYPE STANDARD TABLE OF stas.
*
*    lt_stas_data =
*    VALUE #( ( stlty ='M' stlnr ='00001353' stlal ='01' stlkn ='00000001' stasz ='00000003' datuv ='20220101' techv ='' aennr ='' lkenz =''
*             andat ='20220405'  aedat ='00000000' aenam ='' dvdat ='00000000' dvnam ='' aehlp ='00' stvkn ='00000001' idpos ='' idvar ='' lpsrt ='0000' bom_versn =''  )
*             ( stlty ='M' stlnr ='00001353' stlal ='01' stlkn ='00000002' stasz ='00000005' datuv ='20220101' techv ='' aennr ='' lkenz =''
*             andat ='20220405'  aedat ='00000000' aenam ='' dvdat ='00000000' dvnam ='' aehlp ='00' stvkn ='00000002' idpos ='' idvar ='' lpsrt ='0000' bom_versn =''  )
*             ( stlty ='M' stlnr ='00001353' stlal ='01' stlkn ='00000003' stasz ='00000007' datuv ='20220101' techv ='' aennr ='' lkenz =''
*             andat ='20220405'  aedat ='00000000' aenam ='' dvdat ='00000000' dvnam ='' aehlp ='00' stvkn ='00000003' idpos ='' idvar ='' lpsrt ='0000' bom_versn =''  )
*             ( stlty ='M' stlnr ='00001353' stlal ='01' stlkn ='00000004' stasz ='00000009' datuv ='20220101' techv ='' aennr ='' lkenz =''
*             andat ='20220405'  aedat ='00000000' aenam ='' dvdat ='00000000' dvnam ='' aehlp ='00' stvkn ='00000004' idpos ='' idvar ='' lpsrt ='0000' bom_versn =''  )
*             ( stlty ='M' stlnr ='00001355' stlal ='01' stlkn ='00000001' stasz ='00000003' datuv ='20220101' techv ='' aennr ='' lkenz =''
*             andat ='20220405'  aedat ='00000000' aenam ='' dvdat ='00000000' dvnam ='' aehlp ='00' stvkn ='00000001' idpos ='' idvar ='' lpsrt ='0000' bom_versn =''  )
*             ( stlty ='M' stlnr ='00001355' stlal ='01' stlkn ='00000002' stasz ='00000005' datuv ='20220101' techv ='' aennr ='' lkenz =''
*             andat ='20220405'  aedat ='00000000' aenam ='' dvdat ='00000000' dvnam ='' aehlp ='00' stvkn ='00000002' idpos ='' idvar ='' lpsrt ='0000' bom_versn =''  )
*             ( stlty ='M' stlnr ='00001355' stlal ='01' stlkn ='00000003' stasz ='00000007' datuv ='20220101' techv ='' aennr ='' lkenz =''
*             andat ='20220405'  aedat ='00000000' aenam ='' dvdat ='00000000' dvnam ='' aehlp ='00' stvkn ='00000003' idpos ='' idvar ='' lpsrt ='0000' bom_versn =''  )
*             ( stlty ='M' stlnr ='00001355' stlal ='01' stlkn ='00000004' stasz ='00000009' datuv ='20220101' techv ='' aennr ='' lkenz =''
*             andat ='20220405'  aedat ='00000000' aenam ='' dvdat ='00000000' dvnam ='' aehlp ='00' stvkn ='00000004' idpos ='' idvar ='' lpsrt ='0000' bom_versn =''  )
*             ( stlty ='M' stlnr ='00001355' stlal ='02' stlkn ='00000005' stasz ='00000012' datuv ='20220101' techv ='' aennr ='' lkenz =''
*             andat ='20220405'  aedat ='00000000' aenam ='' dvdat ='00000000' dvnam ='' aehlp ='00' stvkn ='00000005' idpos ='' idvar ='' lpsrt ='0000' bom_versn =''  )
*             ( stlty ='M' stlnr ='00001355' stlal ='02' stlkn ='00000006' stasz ='00000014' datuv ='20220101' techv ='' aennr ='' lkenz =''
*             andat ='20220405'  aedat ='00000000' aenam ='' dvdat ='00000000' dvnam ='' aehlp ='00' stvkn ='00000006' idpos ='' idvar ='' lpsrt ='0000' bom_versn =''  )
*             ( stlty ='M' stlnr ='00001355' stlal ='02' stlkn ='00000007' stasz ='00000016' datuv ='20220101' techv ='' aennr ='' lkenz =''
*             andat ='20220405'  aedat ='00000000' aenam ='' dvdat ='00000000' dvnam ='' aehlp ='00' stvkn ='00000007' idpos ='' idvar ='' lpsrt ='0000' bom_versn =''  )
*     ).
*    mo_osql_env->get_double( 'stas' )->insert( cl_osql_test_data=>create( lt_stas_data ) ).
*
*  ENDMETHOD.
*
*  METHOD teardown.
*
*    mo_osql_env->clear_doubles(  ).
*
*  ENDMETHOD.
*
*
*  METHOD test_map_bom_data.
*
*    DATA(lt_entity_data) = f_cut->if_surdp_uph_entity_bom_proc~map_bom_data( VALUE #(  ) ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lines( lt_entity_data  )
*      exp   = 0
*    ).
*
*  ENDMETHOD.
*
*  METHOD test_prepare_process_w_selpar.
** Test the method prepare_process with selection parameters
*
*    "Given
*    DATA lt_bom_keys TYPE surdpt_uph_bom_key.
*    DATA lt_bom_hdr_data TYPE surdpt_uph_wrap_bom_hdr.
*    DATA ls_input_data TYPE surdps_pckg_bom_input.
*
*    ls_input_data-material = VALUE #(  ( sign = 'I' option = 'EQ' low = 'RDP-BM-PUMP' ) ).
*    ls_input_data-plant = VALUE #(  ( sign = 'I' option = 'EQ' low = '0001' ) ).
*    ls_input_data-valfromdate = '20220101'.
*    ls_input_data-max_exp_lvl = 10.
*
*    f_cut->if_surdp_uph_entity_proc~init_processor(
*             iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
*             iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*             is_parameters = ls_input_data ).
*
*    DATA(lv_record_cnt) = f_cut->if_surdp_uph_entity_proc~prepare_process(  ).
*    lt_bom_keys = f_cut->get_determined_records(  ).
*
*    "When
*    lt_bom_hdr_data = f_cut->if_surdp_uph_entity_bom_proc~retrieve_bom_data( it_bom_key = lt_bom_keys ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals(
*      act  = lv_record_cnt
*      exp  = 2
*    ).
*
*  ENDMETHOD.
*
*  METHOD test_prepare_process_delta.
** Test the delta processing
*
*    "Given
*    DATA ls_input_data TYPE surdps_pckg_bom_input.
*    ls_input_data-material = VALUE #(  ( sign = 'I' option = 'CP' low = 'RDP-BM-*' ) ).
*    ls_input_data-plant = VALUE #(  ( sign = 'I' option = 'EQ' low = '0001' ) ).
*    ls_input_data-valfromdate = '20220101'.
*    ls_input_data-max_exp_lvl = 10.
*
*    "init processor as initial load
*    f_cut->if_surdp_uph_entity_proc~init_processor(
*        iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
*        iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters = ls_input_data ).
*
*    "When / Then
*    DATA(lv_record_cnt) = f_cut->if_surdp_uph_entity_proc~prepare_process( ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act  = lv_record_cnt
*      exp  = 3
*    ).
*
*    DATA(lt_entity_data) = f_cut->if_surdp_uph_entity_proc~process_package( iv_act_package = 1 iv_package_size = 50 ).
*
*    " write protocol entry
*    DATA(ls_protocol) = VALUE surdpd_uph_prot( start_timestamp = sy-datum end_timestamp = sy-datum failed = abap_false
*                                                upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
*                                                upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full  ).
*
*    f_cut->if_surdp_uph_entity_proc~write_to_protocol(
*              is_protocol = ls_protocol is_selection_params = ls_input_data ).
*
*
*    "Change BOM data
*    "BOM with no 00001355 is changed now
*    DATA: lt_stzu_data TYPE STANDARD TABLE OF stzu.
*
*    lt_stzu_data = VALUE #( ( stlty ='M' stlnr ='00001355' stlan ='1' exstl ='' altst ='X' varst ='' kbaus ='' ltxsp ='' stlbe ='' ztext ='' wrkan ='0001' hisdt ='X' hissr ='' histk ='' stuez ='00000016' maxkn ='00000007' kzpln ='' aenrl =''
*                                clsmx ='00000000' stldt = sy-datum stltm = sy-uzeit maxkan ='000000' tstmp ='20220405143757 ' versnind =''  )
*    ).
*    mo_osql_env->get_double( 'stzu' )->clear( ).
*    mo_osql_env->get_double( 'stzu' )->insert( cl_osql_test_data=>create( lt_stzu_data ) ).
*
*
*    DATA: lt_prot_data TYPE STANDARD TABLE OF surdpd_uph_prot.
*
*    lt_prot_data = VALUE #( ( uuid = CONV #( '0894EF4586611EEC9EBA5FAEA97A8530' ) upload_entity = 'PC_BOM' upload_mode = 'F' start_timestamp = '20220120080000.0000000' end_timestamp = '20220120080100.0000000' failed = '' selection =
*                            '{"material":[{"sign":"I","option":"CP","low":"RDP-BM-*"}],"plant":[{"sign":"I","option":"EQ","low":"0001"}],"valfromdate":"2022-01-01","max_exp_lvl":"10"}' ) ).
*
*    mo_osql_env->get_double( 'SURDPD_UPH_PROT' )->insert( cl_osql_test_data=>create( lt_prot_data ) ).
*
*    "init processor as delta load
*    f_cut->if_surdp_uph_entity_proc~init_processor(
*        iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
*        iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_delta
*        is_parameters = ls_input_data ).
*
*    lv_record_cnt = f_cut->if_surdp_uph_entity_proc~prepare_process( ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act  = lv_record_cnt
*      exp  = 2
*      msg  = 'The expected BOM are not selected in delta mode'
*    ).
*
*  ENDMETHOD.
*
*  METHOD test_process_package.
** Test process package with BOM explosion data
*    "Given
*    DATA ls_input_data TYPE surdps_pckg_bom_input.
*    ls_input_data-material = VALUE #(  ( sign = 'I' option = 'EQ' low = 'RDP-BM-PC-ORDPACK' ) ).
*    ls_input_data-plant = VALUE #(  ( sign = 'I' option = 'EQ' low = '0001' ) ).
*    ls_input_data-valfromdate = '20220101'.
*    ls_input_data-max_exp_lvl = 10.
*
*    "init processor as initial load
*    f_cut->if_surdp_uph_entity_proc~init_processor(
*        iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
*        iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters = ls_input_data ).
*
*    "set bom explosion wrapper with result
*    f_cut->set_ext_modules_wrapper( NEW tcl_bom_expl_wrapper( 1 ) ).
*
*    "When
*    DATA(lt_entity_data) = f_cut->if_surdp_uph_entity_proc~process_package( iv_act_package = 1 iv_package_size = 50 ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals(
*      act   = lines( lt_entity_data )
*      exp   = 1
*    ).
*
*    DATA(lt_entity_items) = CAST cl_surdp_uph_ent_pckg_cmp_hdr( lt_entity_data[ 1 ] )->get_items(  ).
*    cl_abap_unit_assert=>assert_equals(
*      act   = lines( lt_entity_items )
*      exp   = 4
*    ).
*
*  ENDMETHOD.
*
*  METHOD test_process_package_w_empty.
** Test process package with empty BOM explosion
*
*    "Given
*    DATA ls_input_data TYPE surdps_pckg_bom_input.
*    ls_input_data-material = VALUE #(  ( sign = 'I' option = 'EQ' low = 'RDP-BM-PC-ORDPACK' ) ).
*    ls_input_data-plant = VALUE #(  ( sign = 'I' option = 'EQ' low = '0001' ) ).
*    ls_input_data-valfromdate = '20220101'.
*    ls_input_data-max_exp_lvl = 10.
*
*    "init processor as initial load
*    f_cut->if_surdp_uph_entity_proc~init_processor(
*        iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom
*        iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full
*        is_parameters = ls_input_data ).
*
*    "Variant 1:
*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    "set bom explosion wrapper with empty result
*    f_cut->set_ext_modules_wrapper( NEW tcl_bom_expl_wrapper( 0 ) ).
*
*    "When
*    DATA(lt_entity_data) = f_cut->if_surdp_uph_entity_proc~process_package( iv_act_package = 1 iv_package_size = 50 ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals(
*      act   = lines( lt_entity_data )
*      exp   = 0
*    ).
*
*
*    "Variant 2:
*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    "Given
*    "set bom explosion wrapper with one header result
*    f_cut->set_ext_modules_wrapper( NEW tcl_bom_expl_wrapper( 2 ) ).
*
*    "When
*    lt_entity_data = f_cut->if_surdp_uph_entity_proc~process_package( iv_act_package = 1 iv_package_size = 50 ).
*
*    "Then
*    cl_abap_unit_assert=>assert_equals(
*      act   = lines( lt_entity_data )
*      exp   = 1
*    ).
*
*  ENDMETHOD.
*
*  METHOD test_determine_bom_w_valchg.
** Tests validity and last change filter
*
*    DATA ls_parameters TYPE surdps_pckg_bom_input.
*
*    "Test key date:
*    "Out of range
*    ls_parameters-material = VALUE #(  ( sign = 'I' option = 'EQ' low = 'RDP-BM-PC-ORDPACK' ) ).
*    ls_parameters-plant = VALUE #(  ( sign = 'I' option = 'EQ' low = '0001' ) ).
*    ls_parameters-valfromdate = '20211231'.
*
*    f_cut->if_surdp_uph_entity_proc~init_processor( is_parameters = ls_parameters iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom ).
*    f_cut->if_surdp_uph_entity_proc~prepare_process(  ).
*    DATA(lt_keys) = f_cut->get_determined_records(  ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( lt_keys ) ).
*
*    "Test left overlap
*    ls_parameters-valfromdate = '20220101'.
*
*    f_cut->if_surdp_uph_entity_proc~init_processor( is_parameters = ls_parameters iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom ).
*    f_cut->if_surdp_uph_entity_proc~prepare_process(  ).
*    lt_keys = f_cut->get_determined_records(  ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_keys ) ).
*
*    READ TABLE lt_keys WITH KEY material = 'RDP-BM-PC-ORDPACK' TRANSPORTING NO FIELDS. "#EC PREF_LINE_EX
*    cl_abap_unit_assert=>assert_equals(  exp = 0 act = sy-subrc ).
*
*    "Test last changed
*    ls_parameters-material = VALUE #(  ( sign = 'I' option = 'CP' low = 'RDP-BM-*' ) ).
*    ls_parameters-plant = VALUE #(  ( sign = 'I' option = 'EQ' low = '0001' ) ).
*    ls_parameters-bom_chgdon = VALUE #( ( sign = 'I' option = 'EQ' low = '20220406' )  ).
*    ls_parameters-valfromdate = '20220101'.
*
*    f_cut->if_surdp_uph_entity_proc~init_processor( is_parameters = ls_parameters iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom ).
*    f_cut->if_surdp_uph_entity_proc~prepare_process(  ).
*    lt_keys = f_cut->get_determined_records(  ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_keys ) ).
*
*  ENDMETHOD.
*
*  METHOD test_determine_bom_w_alter.
** Tests bom alternative filter
*
*    DATA ls_parameters TYPE surdps_pckg_bom_input.
*
*    "Test BOM alternative
*    ls_parameters-material = VALUE #(  ( sign = 'I' option = 'EQ' low = 'RDP-BM-PUMP' ) ).
*    ls_parameters-plant = VALUE #(  ( sign = 'I' option = 'EQ' low = '0001' ) ).
*    ls_parameters-bom_alter = VALUE #( ( sign = 'I' option = 'EQ' low = '02' )  ).
*    ls_parameters-valfromdate = '20220101'.
*
*    f_cut->if_surdp_uph_entity_proc~init_processor( is_parameters = ls_parameters iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom ).
*    f_cut->if_surdp_uph_entity_proc~prepare_process(  ).
*    DATA(lt_keys) = f_cut->get_determined_records(  ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_keys ) ).
*
*  ENDMETHOD.
*
*  METHOD test_determine_bom_w_status.
** Test bom status filter
*
*    DATA ls_parameters TYPE surdps_pckg_bom_input.
*
*    "Test BOM status
*    ls_parameters-material = VALUE #(  ( sign = 'I' option = 'EQ' low = 'RDP-BM-PUMP' ) ).
*    ls_parameters-plant = VALUE #(  ( sign = 'I' option = 'EQ' low = '0001' ) ).
*    ls_parameters-bom_status = '01'.
*    ls_parameters-valfromdate = '20220101'.
*
*    f_cut->if_surdp_uph_entity_proc~init_processor( is_parameters = ls_parameters iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom ).
*    f_cut->if_surdp_uph_entity_proc~prepare_process(  ).
*    DATA(lt_keys) = f_cut->get_determined_records(  ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_keys ) ).
*
*  ENDMETHOD.
*
*  METHOD test_determine_bom_w_mattyp.
** Test material type filter
*
*    DATA ls_parameters TYPE surdps_pckg_bom_input.
*
*    "Test material type
*    ls_parameters-material = VALUE #(  ( sign = 'I' option = 'EQ' low = 'RDP-BM-PUMP' ) ).
*    ls_parameters-plant = VALUE #(  ( sign = 'I' option = 'EQ' low = '0001' ) ).
*    ls_parameters-mat_type = VALUE #(  ( sign = 'I' option = 'EQ' low = 'FERT' ) ).
*    ls_parameters-valfromdate = '20220101'.
*
*    f_cut->if_surdp_uph_entity_proc~init_processor( is_parameters = ls_parameters iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom ).
*    f_cut->if_surdp_uph_entity_proc~prepare_process(  ).
*    DATA(lt_keys) = f_cut->get_determined_records(  ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_keys ) ).
*
*  ENDMETHOD.
*
*  METHOD test_determine_bom_w_usage.
** Test bom usage filter
*
*    DATA ls_parameters TYPE surdps_pckg_bom_input.
*
*    "Test material type
*    ls_parameters-material = VALUE #(  ( sign = 'I' option = 'EQ' low = 'RDP-BM-PUMP' ) ).
*    ls_parameters-plant = VALUE #(  ( sign = 'I' option = 'EQ' low = '0001' ) ).
*    ls_parameters-bom_usage = VALUE #(  ( sign = 'I' option = 'EQ' low = '1' ) ).
*    ls_parameters-valfromdate = '20220101'.
*
*    f_cut->if_surdp_uph_entity_proc~init_processor( is_parameters = ls_parameters iv_upload_mode = if_surdp_uph_entity_proc=>gc_upload_mode-gc_upload_mode_full iv_upload_entity = if_surdp_uph_entity_proc=>gc_upload_entities-gc_ue_pc_bom ).
*    f_cut->if_surdp_uph_entity_proc~prepare_process(  ).
*    DATA(lt_keys) = f_cut->get_determined_records(  ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_keys ) ).
*
*  ENDMETHOD.
*
*ENDCLASS.
