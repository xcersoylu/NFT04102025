  METHOD calculate_tax.
    DATA lv_tax_ratio TYPE ynft_t_t012-tax_percentage.
    SELECT * FROM ynft_t_t012 INTO TABLE @DATA(lt_t012).
    lv_tax_ratio = VALUE #( lt_t012[ mwskz = is_line-taxcode ]-tax_percentage OPTIONAL ).
*    lv_tax_ratio = SWITCH #( is_line-taxcode WHEN 'V0' THEN 0
*                                             WHEN 'V1' THEN 1
*                                             WHEN 'V2' THEN 10
*                                             WHEN 'V3' THEN 20 ).
    CASE iv_calculation_type.
      WHEN mc_gross.
        rv_tax = is_line-value - ( is_line-value / ( 1 + ( lv_tax_ratio / 100 ) ) ).
      WHEN mc_net.
        rv_tax = is_line-value * lv_tax_ratio / 100.
    ENDCASE.
    mv_total_tax += rv_tax.
  ENDMETHOD.