*"* local class implementation for public class
*"* use this source file for the implementation part of
*"* local helper classes

class lcl_helper definition.
  public section.
      types x4 type x length 4.
      constants co_null type x value '00'.
    class-methods:
      swap16 changing f type x, " reverse order of first 2 bytes of F
      swap32 changing f type x, " reverse order of first 4 bytes of F
      to_mtime importing t type timestamp returning value(mtime) type x4 raising cx_sy_conversion_overflow,
      from_mtime importing mtime type x4 returning value(t) type timestamp,
      text_to_gzip importing value type string name type string returning value(xstr) type xstring raising cx_parameter_invalid_range.
  private section.
    constants: co_1970 type timestamp value '19700101000000'.
endclass.

class lcl_helper implementation.

  method swap16.
    data b type x length 1.
    b = f+0(1). f+0(1) = f+1(1). f+1(1) = b.
  endmethod.

  method swap32.
    data b type x length 1.
    b = f+0(1). f+0(1) = f+3(1). f+3(1) = b.
    b = f+1(1). f+1(1) = f+2(1). f+2(1) = b.
  endmethod.

  method to_mtime.
    data:
      i8 type TZNTSTMPL, "750: int8
      st type string.
    if t is initial.
      clear mtime.
    else.
      i8 = cl_abap_tstmp=>subtract( tstmp1 = t tstmp2 = co_1970 ).
      " = 0 means "not set" and is reserved for initial input
      " values outside range of unsigned 32-bit integer lead to overflow
      if i8 <= 0 or i8 > 4294967295.
        st = t.
        raise exception type cx_sy_conversion_overflow exporting value = st.
      endif.
      mtime = i8.
      swap32( changing f = mtime ).
    endif.
  endmethod.

  method from_mtime.
    data:
        i8 type TZNTSTMPL, "750: int8
        x4 type x4.
    if mtime is initial.
      clear t.
    else.
      x4 = mtime.
      swap32( changing f = x4 ).
      i8 = x4.
      t = cl_abap_tstmp=>add( tstmp = co_1970 secs = i8 ).
    endif.
  endmethod.

  method text_to_gzip.
    data:
      l_previous type ref to cx_sy_conversion_codepage,
      l_value    type string.
    try.
        xstr = cl_abap_codepage=>convert_to( codepage = 'ISO-8859-1' source = value ).
      catch cx_sy_conversion_codepage into l_previous.
        l_value = l_previous->source_extract.
        raise exception type cx_parameter_invalid_range
          exporting
              parameter = name
              value     = l_value
              previous  = l_previous.
    endtry.
    find first occurrence of co_null in xstr in byte mode.
    if sy-subrc = 0.
      raise exception type cx_parameter_invalid_range
        exporting
          parameter = name
          value     = '00'.
    endif.
  endmethod.

endclass.
