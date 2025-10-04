  METHOD if_http_service_extension~handle_request.
    TYPES BEGIN OF ty_business_data.
    INCLUDE TYPE ycl_nft_supplier_invoice=>tys_parameters_1.
    TYPES END OF ty_business_data.
    DATA :lo_function_resource TYPE REF TO /iwbep/if_cp_resource_function,
          lo_function_request  TYPE REF TO /iwbep/if_cp_request_function,
          lo_function_response TYPE REF TO /iwbep/if_cp_response_function,
          ls_reversal_return   TYPE ycl_nft_supplier_invoice=>tys_cancel_invoice_export_para,
          ls_reversal          TYPE ty_business_data,
          lo_http_client       TYPE REF TO if_web_http_client,
          lo_client_proxy      TYPE REF TO /iwbep/if_cp_client_proxy,
          lv_postingdate       TYPE timestamp.

    DATA(lv_request_body) = request->get_text( ).
    DATA(lv_get_method) = request->get_method( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ms_request ).

    SELECT
      FROM ynft_t_parameter
      FIELDS *
      WHERE parametername = 'SUPPLIER_INVOICE'
      INTO TABLE @DATA(lt_parameters).
    TRY.
        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                                     comm_scenario  = VALUE #( lt_parameters[ parameterkey = 'COMM_SCENARIO' ]-value OPTIONAL )
                                                     comm_system_id = VALUE #( lt_parameters[ parameterkey = 'COMM_SYSTEM_ID' ]-value OPTIONAL )
                                                     service_id     = VALUE #( lt_parameters[ parameterkey = 'SERVICE_ID' ]-value OPTIONAL ) ).

        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
        lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy( EXPORTING is_proxy_model_key      = VALUE #( repository_id       = 'DEFAULT'
                                                                                                                            proxy_model_id      = 'YSCM_NFT_SUPPLIER_INVOICE'
                                                                                                                            proxy_model_version = '0001' )
                                                                                         io_http_client           = lo_http_client
                                                                                         iv_relative_service_root = '/sap/opu/odata/sap/API_SUPPLIERINVOICE_PROCESS_SRV' ).

        lo_function_resource = lo_client_proxy->create_resource_for_function( 'CANCEL' ).
        lv_postingdate = ms_request-posting_date && cl_abap_context_info=>get_system_time(  ).
        ls_reversal = VALUE #( supplier_invoice = ms_request-supplier_invoce
                               fiscal_year      = ms_request-fiscal_year
                               reversal_reason  = ms_request-reversal_reason
                               posting_date     = lv_postingdate ).

        lo_function_resource->set_parameter( ls_reversal ).
        lo_function_request = lo_function_resource->create_request( ).
        lo_function_response = lo_function_request->execute( /iwbep/if_cp_request_function=>gcs_http_method-post ).
        CHECK lo_function_response->has_business_data( ) = abap_true.
        lo_function_response->get_business_data( IMPORTING ea_response_data = ls_reversal_return ).

        IF ls_reversal_return-success = abap_true.
          ms_response-reverse_document = ls_reversal_return-reverse_document.

          "Başarılı ters kayıt alındığında R005 tablosundaki kaydın silinmesi gerekiyor.
          DELETE FROM ynft_t_r005 WHERE accountingdocument_inv = @ms_request-supplier_invoce
                                    AND fiscalyear_inv         = @ms_request-fiscal_year.
        ELSE.
        ENDIF.
      CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
        DATA(lv_error) = lx_gateway->if_message~get_longtext(  ).
        APPEND lv_error TO ms_response-messages.
      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        lv_error = lx_web_http_client_error->if_message~get_longtext(  ).
        APPEND lv_error TO ms_response-messages.
      CATCH cx_http_dest_provider_error INTO DATA(lx_provider_error).
        lv_error = lx_provider_error->if_message~get_longtext(  ).
        APPEND lv_error TO ms_response-messages.
    ENDTRY.

    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).
  ENDMETHOD.