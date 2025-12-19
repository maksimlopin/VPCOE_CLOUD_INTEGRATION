class /VPCOE/CL_ABAP_GZIP definition
  public
  final
  create public .

*"* public components of class /VPCOE/CL_ABAP_GZIP
*"* do not include other source files here!!!
public section.

  class-methods COMPRESS_BINARY_WITH_HEADER
    importing
      !RAW_IN type XSTRING
      !COMPRESS_LEVEL type I default 6
      !MTIME_IN type TIMESTAMP optional
      !FNAME_IN type STRING optional
      !FCOMMENT_IN type STRING optional
      !FEXTRA_IN type XSTRING optional
      !FHCRC type ABAP_BOOL default ABAP_FALSE
    exporting
      !GZIP_OUT type XSTRING
    raising
      CX_PARAMETER_INVALID_RANGE
      CX_SY_BUFFER_OVERFLOW
      CX_SY_COMPRESSION_ERROR .
  class-methods DECOMPRESS_BINARY_WITH_HEADER
    importing
      !GZIP_IN type XSTRING
    exporting
      !RAW_OUT type XSTRING
      !MTIME_OUT type TIMESTAMP
      !FNAME_OUT type STRING
      !FCOMMENT_OUT type STRING
      !FEXTRA_OUT type XSTRING
    raising
      CX_PARAMETER_INVALID
      CX_SY_BUFFER_OVERFLOW
      CX_SY_COMPRESSION_ERROR .
protected section.
*"* protected components of class /VPCOE/CL_ABAP_GZIP
*"* do not include other source files here!!!
private section.
*"* private components of class /VPCOE/CL_ABAP_GZIP
*"* do not include other source files here!!!

  constants:
    begin of co_head,
      id    type x length 2 value '1F8B',    " GZIP file format identification
      cm    type x          value '08',      " Compression method: 08 - DEFLATE
      os    type x          value 'FF',       " Operating system: FF - unknown
    end of co_head .
  constants:
    begin of co_flg,
      ftext    type x value '01',
      fhcrc    type x value '02',
      fextra   type x value '04',
      fname    type x value '08',
      fcomment type x value '10',
      reserved type x value 'E0',
    end of co_flg .
ENDCLASS.



CLASS /VPCOE/CL_ABAP_GZIP IMPLEMENTATION.


METHOD compress_binary_with_header.
  DATA:
    BEGIN OF l_head,
      flg      TYPE x,          " Flags
      mtime    TYPE x LENGTH 4, " Modification time in seconds since 1.1.1970
      xfl      TYPE x,          " Extra flags
      xlen     TYPE x LENGTH 2, " Opt. header: length of FEXTRA
      fhcrc    TYPE x LENGTH 2, " Opt. header: lower 2 bytes of header's CRC32
      fname    TYPE xstring,    " Opt. header: filename
      fcomment TYPE xstring, " Opt. header: comment entry
      conc     TYPE xstring,    " Concatenated header parts
    END OF l_head,
    BEGIN OF l_tail,
      crc32 TYPE x LENGTH 4, " Uncompressed data's CRC32
      size  TYPE x LENGTH 4, " Size of uncompressed data modulo 2^32
    END OF l_tail,
    l_string   TYPE string,
    l_previous TYPE REF TO cx_root.

  DEFINE macro_build_head. " needed twice, for "with FHCRC" and "without"
    concatenate
        co_head-id co_head-cm l_head-flg l_head-mtime l_head-xfl co_head-os
        l_head-conc
        into l_head-conc in byte mode.
  END-OF-DEFINITION.

  " FEXTRA's length is a 2-byte integer => check limit
  IF xstrlen( fextra_in ) > 65535.
    RAISE EXCEPTION TYPE cx_parameter_invalid_range
      EXPORTING
        parameter = 'FEXTRA_IN'
        value     = '>65535'.
  ENDIF.

  " FNAME and FCOMMENT are stored zero-terminated => must not contain \0; convert to ISO-8859-1
  l_head-fname = lcl_helper=>text_to_gzip( value = fname_in name = 'FNAME_IN' ).
  l_head-fcomment = lcl_helper=>text_to_gzip( value = fcomment_in name = 'FCOMMENT_IN' ).
  TRY.
      l_head-mtime = lcl_helper=>to_mtime( mtime_in ).
    CATCH cx_sy_conversion_overflow INTO l_previous.
      l_string = mtime_in.
      RAISE EXCEPTION TYPE cx_parameter_invalid_range
        EXPORTING
          parameter = 'MTIME_IN'
          value     = l_string
          previous  = l_previous.
  ENDTRY.

  " Set XFL
  IF compress_level = 1.
    l_head-xfl = '04'.
  ELSEIF compress_level = 9.
    l_head-xfl = '02'.
  ENDIF.

  " Compute FLG and optional header parts and assemble header
  IF fextra_in IS NOT INITIAL.
    l_head-flg = l_head-flg BIT-OR co_flg-fextra.
    l_head-xlen = xstrlen( fextra_in ).
    lcl_helper=>swap16( CHANGING f = l_head-xlen ).
    CONCATENATE l_head-xlen fextra_in INTO l_head-conc IN BYTE MODE.
  ENDIF.
  IF l_head-fname IS NOT INITIAL.
    l_head-flg = l_head-flg BIT-OR co_flg-fname.
    CONCATENATE l_head-conc l_head-fname lcl_helper=>co_null INTO l_head-conc IN BYTE MODE.
  ENDIF.
  IF l_head-fcomment IS NOT INITIAL.
    l_head-flg = l_head-flg BIT-OR co_flg-fcomment.
    CONCATENATE l_head-conc l_head-fcomment lcl_helper=>co_null INTO l_head-conc IN BYTE MODE.
  ENDIF.
  IF fhcrc = abap_true.  " lower 2 Bytes of header's checksum
    l_head-flg = l_head-flg BIT-OR co_flg-fhcrc.
    macro_build_head.
    l_head-fhcrc = cl_abap_zip=>crc32( l_head-conc ).
    lcl_helper=>swap16( CHANGING f = l_head-fhcrc ).
    CONCATENATE l_head-conc l_head-fhcrc INTO l_head-conc IN BYTE MODE.
  ELSE.
    macro_build_head.
  ENDIF.

  " Set tail: CRC32 and size
  l_tail-crc32 = cl_abap_zip=>crc32( raw_in ). " checksum of uncompressed input
  lcl_helper=>swap32( CHANGING f = l_tail-crc32 ).
  l_tail-size = xstrlen( raw_in ).   " length of uncompressed input modulo 2^32
  lcl_helper=>swap32( CHANGING f = l_tail-size ).

  " Compress input data
  cl_abap_gzip=>compress_binary(
      EXPORTING
        compress_level = compress_level
        raw_in         = raw_in
      IMPORTING
        gzip_out       = gzip_out ).

  " Add envelope
  CONCATENATE l_head-conc gzip_out l_tail-crc32 l_tail-size
      INTO gzip_out IN BYTE MODE.

ENDMETHOD.


METHOD decompress_binary_with_header.
  DATA:
    BEGIN OF l_mtime,
      date TYPE c LENGTH 8,
      time TYPE c LENGTH 6,
    END OF l_mtime,
    l_head_flg          TYPE x, " FLG byte
    l_head_fname_ofs    TYPE i, " start of FNAME in header
    l_head_fname_len    TYPE i, " length of FNAME (excluding terminating \0)
    l_head_fcomment_ofs TYPE i, " start of FCOMMENT in header
    l_head_fcomment_len TYPE i, " length of FCOMMENT (excluding terminating \0)
    l_head_fextra_ofs   TYPE i, " start of FEXTRA in header
    l_head_fextra_len   TYPE i, " length of FEXTRA
    l_xstr              TYPE xstring,
    l_x4                TYPE x LENGTH 4,
    l_ofs               TYPE i. " current offset


  " ID and CM OK?
  IF xstrlen( gzip_in ) < 10
      OR gzip_in(2) <> co_head-id
      OR gzip_in+2(1) <> co_head-cm.
    RAISE EXCEPTION TYPE cx_parameter_invalid.
  ENDIF.

  " Reserved bits = 0?
  l_head_flg = gzip_in+3(1).
  IF ( l_head_flg BIT-AND co_flg-reserved ) <> lcl_helper=>co_null.
    RAISE EXCEPTION TYPE cx_parameter_invalid.
  ENDIF.

  " Extract MTIME
  l_x4 = gzip_in+4(4).
  l_mtime = lcl_helper=>from_mtime( l_x4 ).

  " Determine header offsets and length
  l_ofs = 10.
  IF ( l_head_flg BIT-AND co_flg-fextra ) <> lcl_helper=>co_null.
    l_x4(2) = gzip_in+l_ofs(2).
    lcl_helper=>swap16( CHANGING f = l_x4 ).
    l_head_fextra_len = l_x4(2).
    l_head_fextra_ofs = l_ofs + 2.
    l_ofs = l_head_fextra_ofs + l_head_fextra_len.
  ENDIF.
  IF ( l_head_flg BIT-AND co_flg-fname ) <> lcl_helper=>co_null.
    l_head_fname_ofs = l_ofs.
    FIND FIRST OCCURRENCE OF lcl_helper=>co_null IN SECTION OFFSET l_ofs OF gzip_in IN BYTE MODE MATCH OFFSET l_ofs.
    IF sy-subrc <> 0.
      " Unterminated FNAME
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING parameter = 'GZIP_IN'.
    ENDIF.
    l_head_fname_len = l_ofs - l_head_fname_ofs.
    ADD 1 TO l_ofs.
  ENDIF.
  IF ( l_head_flg BIT-AND co_flg-fcomment ) <> lcl_helper=>co_null.
    l_head_fcomment_ofs = l_ofs.
    FIND FIRST OCCURRENCE OF lcl_helper=>co_null IN SECTION OFFSET l_ofs OF gzip_in IN BYTE MODE MATCH OFFSET l_ofs.
    IF sy-subrc <> 0.
      " Unterminated FCOMMENT
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING parameter = 'GZIP_IN'.
    ENDIF.
    l_head_fcomment_len = l_ofs - l_head_fcomment_ofs.
    ADD 1 TO l_ofs.
  ENDIF.
  IF ( l_head_flg BIT-AND co_flg-fhcrc ) <> lcl_helper=>co_null.
    l_xstr = gzip_in(l_ofs).
    l_x4 = cl_abap_zip=>crc32( l_xstr ).
    lcl_helper=>swap32( CHANGING f = l_x4 ).
    IF gzip_in+l_ofs(2) <> l_x4(2). " only least-significant 16 bits are relevant
      " Wrong FHCRC
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING parameter = 'GZIP_IN'.
    ENDIF.
    ADD 2 TO l_ofs.
  ENDIF.

  " Decode payload
  l_xstr = gzip_in+l_ofs.
  cl_abap_gzip=>decompress_binary( EXPORTING gzip_in = l_xstr IMPORTING raw_out = l_xstr ).

  " Check tail
  l_ofs = xstrlen( gzip_in ) - 4.
  l_x4 = gzip_in+l_ofs(4).
  lcl_helper=>swap32( CHANGING f = l_x4 ).
  IF l_x4 <> xstrlen( l_xstr ).
    " Wrong length of uncompressed data
    RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING parameter = 'GZIP_IN'.
  ENDIF.
  SUBTRACT 4 FROM l_ofs.
  l_x4 = gzip_in+l_ofs(4).
  lcl_helper=>swap32( CHANGING f = l_x4 ).
  IF l_x4 <> cl_abap_zip=>crc32( l_xstr ).
    " Wrong CRC32 of uncompressed data
    RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING parameter = 'GZIP_IN'.
  ENDIF.

  " All OK: assign results
  raw_out      = l_xstr.
  l_xstr = gzip_in+l_head_fname_ofs(l_head_fname_len).
  fname_out    = cl_abap_codepage=>convert_from( codepage = 'ISO-8859-1' source = l_xstr ).
  l_xstr = gzip_in+l_head_fcomment_ofs(l_head_fcomment_len).
  fcomment_out = cl_abap_codepage=>convert_from( codepage = 'ISO-8859-1' source = l_xstr ).
  fextra_out   = gzip_in+l_head_fextra_ofs(l_head_fextra_len).
  mtime_out    = l_mtime.
ENDMETHOD.
ENDCLASS.
