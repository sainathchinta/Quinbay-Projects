package com.gdn.partners.pcu.internal.client.fallback;



import java.util.ArrayList;
import java.util.Arrays;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.x.product.model.vo.ProductCenterSummaryRequest;
import com.gdn.x.product.model.vo.ProductCenterSummaryResponse;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;

public class XProductFeignFallbackTest {

  private static final String STORE_ID = "storeId";
  private static final String REQUEST_ID = "requestId";
  private static final String CATEGORY_CODE = "CAT-CODE";
  private static final String CATALOG_CODE = "12501";
  private static final String PRODUCT_SKU = "productSku";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 100;
  private static final String USERNAME = "username";
  private static final String CURATION_STATUS = "curationStatus";

  private XProductFeignFallback xProductFeignFallback = new XProductFeignFallback();

  @Test
  public void checkImageSizeByImageFilenameTest(){
    GdnRestListResponse<ProductCenterSummaryResponse> response =
        xProductFeignFallback.getProductCenterSummaryFilter(STORE_ID, REQUEST_ID, PAGE, SIZE, new ProductCenterSummaryRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void addProductSalesCatalogTest(){
    GdnBaseRestResponse response =
        xProductFeignFallback.addProductSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE, new SimpleListStringRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void deleteSalesCatalogTest(){
    GdnBaseRestResponse response =
        xProductFeignFallback.deleteSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE, new SimpleListStringRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void moveProductSalesCatalog() {
    GdnBaseRestResponse response = xProductFeignFallback
        .moveProductSalesCatalog(STORE_ID, REQUEST_ID, CATALOG_CODE, CATEGORY_CODE, CATEGORY_CODE,
            new SimpleListStringRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductCenterHistoryTEst() {
    GdnBaseRestResponse response = xProductFeignFallback
        .getProductCenterHistory(PRODUCT_SKU, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductAndItems() {
    GdnRestSingleResponse response = xProductFeignFallback.getProductAndItems(STORE_ID, REQUEST_ID, PRODUCT_SKU, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductDetailsForProductCenter() {
    GdnRestSingleResponse response = xProductFeignFallback.getProductDetailsForProductCenter(STORE_ID, REQUEST_ID, PRODUCT_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void reindexAndClearCacheByProductSkus() {
    GdnBaseRestResponse response =
        xProductFeignFallback.reindexByProductSkus(STORE_ID, REQUEST_ID, new SimpleListStringRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductDetailsByProductSkuTest() {
    GdnRestSingleResponse<ProductL3Response> response =
        xProductFeignFallback.getProductDetailsByProductSku(STORE_ID, REQUEST_ID, PRODUCT_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void takeDownOrReactivateProductTest() {
    GdnBaseRestResponse response = xProductFeignFallback.takeDownOrReactivateProduct(false, new ArrayList<>());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductDetailsByProductSkuListTest() {
    GdnBaseRestResponse response = xProductFeignFallback.getProductDetailsByProductSkuList(Arrays.asList());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getHalalProductHistoryTest() {
    GdnBaseRestResponse response =
        xProductFeignFallback.getHalalDashboardProducts(PAGE, SIZE, new HalalProductsFilterRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateHalalConfigOfProductTest() {
    GdnBaseRestResponse response =
        xProductFeignFallback.updateHalalConfigOfProduct(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, CURATION_STATUS);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductBasicDetailsTest() {
    GdnBaseRestResponse response = xProductFeignFallback.getProductBasicDetails(new SimpleListStringRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }
}
