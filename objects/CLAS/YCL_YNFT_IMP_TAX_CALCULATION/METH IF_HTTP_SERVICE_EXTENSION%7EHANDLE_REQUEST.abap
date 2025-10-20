  METHOD if_http_service_extension~handle_request.
    DATA lv_calculation_type TYPE ynft_e_gross_net.
    DATA lv_count TYPE i.
    DATA lv_tax_ratio TYPE p LENGTH 16 DECIMALS 9.
    DATA lv_tax_value TYPE ynft_e_bwert.
    DATA(lv_request_body) = request->get_text( ).
    DATA(lv_get_method) = request->get_method( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ms_request ).

    CASE ms_request-costsource.
      WHEN '1'. "Mal bedeli faturası
        lv_calculation_type = mc_net.
      WHEN '2'. "Dosya masrafları
        lv_calculation_type = mc_gross.
      WHEN '3'. "Çekme masrafları
        lv_calculation_type = mc_gross.
      WHEN '4'. "Diğer masraflar
        lv_calculation_type = mc_gross.
      WHEN '5'. "Kur farkı kaydı
        lv_calculation_type = mc_gross.
      WHEN '6'. "Peşinat
        lv_calculation_type = mc_net.
    ENDCASE.

    IF lv_calculation_type IS INITIAL.
      "hata ver
    ELSE.
      CLEAR mv_total_tax.
*      LOOP AT ms_request-taxlines INTO DATA(ls_tax_line).
*        APPEND VALUE #( value    = ls_tax_line-value
*                        currency = ls_tax_line-currency
*                        taxcode  = ls_tax_line-taxcode
*                        taxvalue = calculate_tax( is_line = ls_tax_line iv_calculation_type = lv_calculation_type ) ) TO ms_response-taxlines.
*      ENDLOOP.
*      IF sy-subrc = 0.
*        ms_response-totaltax = mv_total_tax.
*      ENDIF.
      DATA(lt_taxlines) = ms_request-taxlines.
      SORT lt_taxlines BY taxcode.
      DELETE ADJACENT DUPLICATES FROM lt_taxlines COMPARING taxcode.
      SELECT taxratio~taxcode , taxratio~conditionrateratio , taxratio~vatconditiontype AS conditiontype ,
             taxratio~accountkeyforglaccount AS taxitemclassification
          FROM @lt_taxlines AS taxlines INNER JOIN  i_taxcoderate AS taxratio ON taxratio~taxcode = taxlines~taxcode
                                                INNER JOIN i_companycode AS t001 ON t001~country = taxratio~country
          WHERE t001~companycode = @ms_request-companycode
            AND taxratio~cndnrecordvalidityenddate >= @sy-datum
            AND taxratio~cndnrecordvaliditystartdate <= @sy-datum
            INTO TABLE @DATA(lt_ratio).
      LOOP AT ms_request-taxlines INTO DATA(ls_tax_line).
        CLEAR lv_count. CLEAR lv_tax_ratio.
        LOOP AT lt_ratio INTO DATA(ls_ratio) WHERE taxcode = ls_tax_line-taxcode.
          lv_count += 1.
          lv_tax_ratio += ls_ratio-conditionrateratio.
        ENDLOOP.
        IF sy-subrc = 0.
          CASE lv_count.
            WHEN 1. "normal hesap.
              CASE lv_calculation_type.
                WHEN mc_gross.
                  lv_tax_value = ls_tax_line-value - ( ls_tax_line-value / ( 1 + ( ls_ratio-conditionrateratio / 100 ) ) ).
                WHEN mc_net.
                  lv_tax_value = ls_tax_line-value * ls_ratio-conditionrateratio / 100.
              ENDCASE.

              APPEND VALUE #( value    = ls_tax_line-value
                              currency = ls_tax_line-currency
                              taxcode  = ls_tax_line-taxcode
                              taxvalue = lv_tax_value
                              conditiontype = ls_ratio-conditiontype
                              taxitemclassification = ls_ratio-taxitemclassification ) TO ms_response-taxlines.
              mv_total_tax += lv_tax_value.
              CLEAR lv_tax_value.
            WHEN OTHERS. "tevkifatlı.
              CASE lv_calculation_type.
                WHEN mc_gross.
                  CLEAR lv_tax_value.
                  lv_tax_value = ls_tax_line-value - ( ls_tax_line-value / ( 1 + ( lv_tax_ratio / 100 ) ) ).
                  LOOP AT lt_ratio INTO ls_ratio WHERE taxcode = ls_tax_line-taxcode.
                    APPEND VALUE #( value    = ls_tax_line-value
                                    currency = ls_tax_line-currency
                                    taxcode  = ls_tax_line-taxcode
                                    taxvalue = lv_tax_value * ( ls_ratio-conditionrateratio / lv_tax_ratio )
                              conditiontype = ls_ratio-conditiontype
                              taxitemclassification = ls_ratio-taxitemclassification
                                    ) TO ms_response-taxlines.
                  ENDLOOP.
                  mv_total_tax += lv_tax_value.
                WHEN mc_net.
                  LOOP AT lt_ratio INTO ls_ratio WHERE taxcode = ls_tax_line-taxcode.
                    lv_tax_value = ls_ratio-conditionrateratio * ls_tax_line-value / 100.
                    APPEND VALUE #( value    = ls_tax_line-value
                                    currency = ls_tax_line-currency
                                    taxcode  = ls_tax_line-taxcode
                                    taxvalue = lv_tax_value
                              conditiontype = ls_ratio-conditiontype
                              taxitemclassification = ls_ratio-taxitemclassification
                                    ) TO ms_response-taxlines.
                    mv_total_tax += lv_tax_value.
                    CLEAR lv_tax_value.
                  ENDLOOP.
              ENDCASE.
          ENDCASE.
        ELSE.

        ENDIF.
      ENDLOOP.
    ENDIF.

    response->set_status('200').
    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).
  ENDMETHOD.