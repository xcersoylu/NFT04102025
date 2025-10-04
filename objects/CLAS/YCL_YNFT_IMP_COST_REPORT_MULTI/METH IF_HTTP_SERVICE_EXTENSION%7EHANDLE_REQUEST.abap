  METHOD if_http_service_extension~handle_request.
    DATA(lv_request_body) = request->get_text( ).
    DATA(lv_get_method) = request->get_method( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ms_request ).
    SELECT r005~companycode,
           r005~accountingdocument_inv,
           r005~fiscalyear_inv,
           si~postingdate
      FROM ynft_t_r005 AS r005 INNER JOIN i_supplierinvoiceapi01 AS si  ON si~supplierinvoice = r005~accountingdocument_inv
                                                                       AND si~fiscalyear         = r005~fiscalyear_inv
      WHERE r005~companycode = @ms_request-companycode
        AND r005~fiscalyear = @ms_request-fiscalyear
        AND r005~accountingdocument = @ms_request-accountingdocument
        AND r005~accountingdocumentitem = @ms_request-accountingdocumentitem
      INTO CORRESPONDING FIELDS OF TABLE @ms_response-clearbalancelines.
    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).
  ENDMETHOD.