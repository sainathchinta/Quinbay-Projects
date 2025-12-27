package com.gdn.partners.pcu.external.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.seller.logistics.web.model.request.UploadExcelRequest;
import com.gdn.seller.logistics.web.model.response.DownloadSkuTemplateResponse;
import com.gdn.seller.logistics.web.model.response.GetSellerLogisticProductResponse;
import com.gdn.seller.logistics.web.model.response.UploadExcelSkuUpdateResponse;
import com.gdn.seller.logistics.web.model.response.UploadExcelSkuUpdateStatusResponse;

public class SellerLogisticsFeignFallbackTest {

  private static final String MERCHANT_CODE = "merchant-code";
  private static final String PICKUP = "pickup";
  private Map<String, List<String>> errorMap = new HashMap<>();
  private SellerLogisticsFeignFallback sellerLogisticsFeignFallback =
      new SellerLogisticsFeignFallback();

  @BeforeEach
  public void setUp() throws Exception {
    errorMap.put(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        Collections.singletonList(ErrorMessages.FALLBACK_ERR_MESSAGE));
  }

  @Test
  public void getSellerLogisticProducts() throws Exception {
    Response<List<GetSellerLogisticProductResponse>> response =
        sellerLogisticsFeignFallback.getSellerLogisticProducts(MERCHANT_CODE, PICKUP);
    assertEquals(HttpStatus.BAD_REQUEST.name(), response.getStatus());
    assertEquals(errorMap, response.getErrors());
  }

  @Test
  public void getTemplateData() throws Exception {
    Response<DownloadSkuTemplateResponse> response =
        sellerLogisticsFeignFallback.getTemplateData(MERCHANT_CODE, PICKUP);
    assertEquals(HttpStatus.BAD_REQUEST.name(), response.getStatus());
    assertEquals(errorMap, response.getErrors());
  }

  @Test
  public void excelUpload() throws Exception {
    Response<UploadExcelSkuUpdateResponse> response =
        sellerLogisticsFeignFallback.excelUpload(new UploadExcelRequest(), MERCHANT_CODE, PICKUP);
    assertEquals(HttpStatus.BAD_REQUEST.name(), response.getStatus());
    assertEquals(errorMap, response.getErrors());
  }

  @Test
  public void excelUploadStatus() throws Exception {
    Response<UploadExcelSkuUpdateStatusResponse> response =
        sellerLogisticsFeignFallback.excelUploadStatus(MERCHANT_CODE);
    assertEquals(HttpStatus.BAD_REQUEST.name(), response.getStatus());
    assertEquals(errorMap, response.getErrors());
  }

}
