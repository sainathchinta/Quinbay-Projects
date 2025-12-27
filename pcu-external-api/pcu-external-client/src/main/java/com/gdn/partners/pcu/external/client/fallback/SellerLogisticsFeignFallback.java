package com.gdn.partners.pcu.external.client.fallback;

import java.util.Collections;
import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.pcu.external.client.feign.SellerLogisticsFeign;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.seller.logistics.web.model.request.UploadExcelRequest;
import com.gdn.seller.logistics.web.model.response.DownloadSkuTemplateResponse;
import com.gdn.seller.logistics.web.model.response.GetSellerLogisticProductResponse;
import com.gdn.seller.logistics.web.model.response.UploadExcelSkuUpdateResponse;
import com.gdn.seller.logistics.web.model.response.UploadExcelSkuUpdateStatusResponse;

@Component
public class SellerLogisticsFeignFallback implements SellerLogisticsFeign {


  @Override
  public Response<List<GetSellerLogisticProductResponse>> getSellerLogisticProducts(String merchantCode,
      String merchantDeliveryType) {
    return new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(), null, Collections
        .singletonMap(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
            Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)), null);
  }

  @Override
  public Response<DownloadSkuTemplateResponse> getTemplateData(String merchantCode,
      String merchantDeliveryType) {
    return new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(), null,
        Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
            Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)),
        null);
  }

  @Override
  public Response<UploadExcelSkuUpdateResponse> excelUpload(UploadExcelRequest requestBody,
      String merchantCode, String merchantDeliveryType) {
    return new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(), null,
        Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
            Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)),
        null);
  }

  @Override
  public Response<UploadExcelSkuUpdateStatusResponse> excelUploadStatus(String merchantCode) {
    return new Response<>(HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.name(), null,
        Collections.singletonMap(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
            Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE)),
        null);
  }
}
