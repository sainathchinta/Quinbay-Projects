package com.gdn.partners.pcu.external.client.fallback;

import java.util.Collections;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.model.request.SimpleListAssemblyDisassemblyRequest;
import com.gdn.partners.pcu.external.web.model.request.TransferRequest;
import com.gdn.partners.pcu.external.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.MasterWarehouseListWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBundleRecipeEditableResponse;
import com.gdn.partners.pcu.external.web.model.response.RequestFormResponse;

public class ProductAssemblyFeignFallbackTest {

  private static final String PAGE = "1";
  private static final String LIMIT = "500";
  private static final int PAGE_NUMBER = 0;
  private static final int PAGE_SIZE = 50;
  private static final String SORT_ORDER = "ASC";
  private static final String ITEM_SKU= "itemSku";
  private static final String REQUEST_FORM_NUMBER= "requestFormNumber";
  private static final String RETRY_TYPE = "RETRY";
  private static final String REQUEST_ID = "requestId";

  @InjectMocks
  private ProductAssemblyFeignFallback productAssemblyFeignFallback;

  private TransferRequest transferRequest;
  private SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    transferRequest = new TransferRequest();
    simpleListAssemblyDisassemblyRequest = new SimpleListAssemblyDisassemblyRequest();
  }

  @Test
  public void getWarehouseCodeAndFulfillmentCenterTest() {
    GdnRestListResponse<MasterWarehouseListWebResponse> response =
        productAssemblyFeignFallback.getWarehouseCodeAndFulfillmentCenter(PAGE, LIMIT);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getAssemblyDisAssemblyAndTransferRequestListingResponseTest() {
    GdnRestListResponse<RequestFormResponse> response =
        productAssemblyFeignFallback.getListingResponse(PAGE_NUMBER, PAGE_SIZE, null);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getBundleRecipeEditableInfoByItemSkusTest() {
    GdnRestListResponse<ProductBundleRecipeEditableResponse> response =
        productAssemblyFeignFallback.getBundleRecipeEditableInfoByItemCodes(Collections.singletonList(ITEM_SKU));
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void transferRequestTest() {
    GdnBaseRestResponse response =
        productAssemblyFeignFallback.transferRequest(transferRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void simpleListAssemblyDisassemblyRequestTest() {
    GdnBaseRestResponse response =
        productAssemblyFeignFallback.assemblyDisassemblyRequest(Constants.TRANSFER_REQUEST,
            simpleListAssemblyDisassemblyRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }


  @Test
  public void getHistoryTest(){
    GdnRestListResponse<HistoryWebResponse> response =
        productAssemblyFeignFallback.getHistory(PAGE_NUMBER, PAGE_SIZE, SORT_ORDER, REQUEST_FORM_NUMBER, "");
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void cancelOrRetryRequestFormTest() {
    GdnBaseRestResponse response =
        productAssemblyFeignFallback.cancelOrRetryRequestForm(REQUEST_FORM_NUMBER, RETRY_TYPE, "", REQUEST_ID);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }
}
