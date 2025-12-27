package com.gdn.partners.pcu.internal.service.impl;

import java.util.Collections;

import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.partners.pcu.internal.client.feign.MasterCatalogFeign;
import com.gdn.partners.pcu.internal.client.feign.ProductAnalyticsFeign;
import com.gdn.partners.pcu.internal.client.feign.XBulkFeign;
import com.gdn.partners.pcu.internal.client.model.request.BigQueryFetchRequest;
import com.gdn.partners.pcu.internal.client.model.request.ProductAttributeExtractionsRequest;
import com.gdn.partners.pcu.internal.web.model.request.BigQueryFetchWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.DeleteProductWebRequest;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.productcategorybase.dto.request.SystemParameterRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.client.feign.XProductFeign;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovalEligibilityWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ItemViewConfigWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.PDTStateUpdateWebRequest;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class UtilityServiceImplTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_ID = "productId";
  private static final String OPERATION_TYPE = "operationType";
  private static final String STATE = "state";
  private static final String REVIEW_TYPE = "reviewType";
  private static final String ENTITY = "entity";
  private static final String PROCESS_NAME = "processName";
  private static final String SYSTEM_PARAM_VALUE = "100";
  private static final String SYSTEM_PARAMETER_VARIABLE = "fetchSize";

  @InjectMocks
  private UtilityServiceImpl utilityService;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private PDTFeign pdtFeign;

  @Mock
  private XProductFeign xProductFeign;

  @Mock
  private XBulkFeign xBulkFeign;

  @Mock
  private MasterCatalogFeign masterCatalogFeign;

  @Mock
  private ProductAnalyticsFeign productAnalyticsFeign;

  @BeforeEach
  public void setUp() throws Exception {
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pbpFeign);
    Mockito.verifyNoMoreInteractions(pcbFeign);
    Mockito.verifyNoMoreInteractions(pdtFeign);
    Mockito.verifyNoMoreInteractions(xBulkFeign);
    Mockito.verifyNoMoreInteractions(masterCatalogFeign);
  }

  @Test
  public void publishAddEditedEventToPDT() {
    Mockito.when(pbpFeign.publishEditedEvent(PRODUCT_CODE, REVIEW_TYPE)).thenReturn(new GdnBaseRestResponse(true));
    utilityService.publishAddEditedEventToPDT(PRODUCT_CODE, REVIEW_TYPE);
    Mockito.verify(pbpFeign).publishEditedEvent(PRODUCT_CODE, REVIEW_TYPE);
  }

  @Test
  public void publishAddRevisedEventToPDT() {
    Mockito.when(pbpFeign.publishRevisedEvent(PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(true));
    utilityService.publishAddRevisedEventToPDT(PRODUCT_CODE);
    Mockito.verify(pbpFeign).publishRevisedEvent(PRODUCT_CODE);
  }

  @Test
  public void retryProductImageResize() {
    Mockito.when(pbpFeign.retryResizeImage(PRODUCT_CODE, false)).thenReturn(new GdnBaseRestResponse(true));
    utilityService.retryProductImageResize(PRODUCT_CODE);
    Mockito.verify(pbpFeign).retryResizeImage(PRODUCT_CODE, false);
  }

  @Test
  public void retryEditedProductImageResize() {
    Mockito.when(pbpFeign.retryEditedResizeImage(PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(true));
    utilityService.retryEditedProductImageResize(PRODUCT_CODE);
    Mockito.verify(pbpFeign).retryEditedResizeImage(PRODUCT_CODE);
  }

  @Test
  public void updatePBPProductWorkflow() {
    Mockito.when(pbpFeign.updateProductWorkflow(PRODUCT_CODE, STATE)).thenReturn(new GdnBaseRestResponse(true));
    utilityService.updatePBPProductWorkflow(PRODUCT_CODE, STATE);
    Mockito.verify(pbpFeign).updateProductWorkflow(PRODUCT_CODE, STATE);
  }

  @Test
  public void updatePBPReviewPending() {
    Mockito.when(pbpFeign.updateReviewPending(PRODUCT_CODE, false)).thenReturn(new GdnBaseRestResponse(true));
    utilityService.updatePBPReviewPending(PRODUCT_CODE, false);
    Mockito.verify(pbpFeign).updateReviewPending(PRODUCT_CODE, false);
  }

  @Test
  public void updatePBPActivatedAndViewableTest() {
    Mockito.when(pbpFeign.updateActivatedAndViewable(PRODUCT_CODE, false, false))
        .thenReturn(new GdnBaseRestResponse(true));
    utilityService.updatePBPActivatedAndViewable(PRODUCT_CODE, false, false);
    Mockito.verify(pbpFeign).updateActivatedAndViewable(PRODUCT_CODE, false, false);
  }

  @Test
  public void checkProductAutoApprovalEligibility() {
    Mockito.when(pbpFeign.checkAutoApprovalEligibility(PRODUCT_CODE, new AutoApprovalTypeRequest()))
        .thenReturn(new GdnRestSingleResponse<>(new AutoApprovalTypeResponse(), PRODUCT_CODE));
    utilityService.checkProductAutoApprovalEligibility(PRODUCT_CODE, new AutoApprovalEligibilityWebRequest());
    Mockito.verify(pbpFeign).checkAutoApprovalEligibility(PRODUCT_CODE, new AutoApprovalTypeRequest());
  }

  @Test
  public void republishPCBProductPublishEvent() {
    Mockito.when(pcbFeign.republishProductFromPCB(Collections.singletonList(PRODUCT_CODE), OPERATION_TYPE))
        .thenReturn(new GdnBaseRestResponse(true));
    utilityService.republishPCBProductPublishEvent(PRODUCT_CODE, OPERATION_TYPE);
    Mockito.verify(pcbFeign).republishProductFromPCB(Collections.singletonList(PRODUCT_CODE), OPERATION_TYPE);
  }

  @Test
  public void clearPCBProductCache() {
    Mockito.when(pcbFeign.clearPCBCache(PRODUCT_ID, PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(true));
    utilityService.clearPCBProductCache(PRODUCT_CODE, PRODUCT_ID);
    Mockito.verify(pcbFeign).clearPCBCache(PRODUCT_ID, PRODUCT_CODE);
  }

  @Test
  public void updatePCBProductViewable() {
    Mockito.when(pcbFeign.updateViewable(PRODUCT_CODE, false)).thenReturn(new GdnBaseRestResponse(true));
    utilityService.updatePCBProductViewable(PRODUCT_CODE, false);
    Mockito.verify(pcbFeign).updateViewable(PRODUCT_CODE, false);
  }

  @Test
  public void updatePCBProductReviewPending() {
    Mockito.when(pcbFeign.updateReviewPending(PRODUCT_CODE, false)).thenReturn(new GdnBaseRestResponse(true));
    utilityService.updatePCBProductReviewPending(PRODUCT_CODE, false);
    Mockito.verify(pcbFeign).updateReviewPending(PRODUCT_CODE, false);
  }

  @Test
  public void updatePDTState() {
    Mockito.when(pdtFeign.updateProductStateInPDT(PRODUCT_CODE, new ProductRetryStatusUpdate()))
        .thenReturn(new GdnBaseRestResponse(true));
    utilityService.updatePDTState(PRODUCT_CODE, new PDTStateUpdateWebRequest());
    Mockito.verify(pdtFeign).updateProductStateInPDT(PRODUCT_CODE, new ProductRetryStatusUpdate());
  }

  @Test
  public void takeDownOrReactivateProduct() {
    Mockito.when(xProductFeign
        .takeDownOrReactivateProduct(false, Collections.singletonList(new ItemViewConfigAndItemSkuRequest())))
        .thenReturn(new GdnBaseRestResponse(true));
    utilityService
        .takeDownOrReactivateProduct(PRODUCT_CODE, false, Collections.singletonList(new ItemViewConfigWebRequest()));
    Mockito.verify(xProductFeign)
        .takeDownOrReactivateProduct(false, Collections.singletonList(new ItemViewConfigAndItemSkuRequest()));
  }

  @Test
  public void abortPendingBulkProcessById() {
    Mockito.when(xBulkFeign.abortPendingBulkProcess(PRODUCT_CODE))
      .thenReturn(new GdnBaseRestResponse(true));
    utilityService.abortPendingBulkProcessById(PRODUCT_CODE);
    Mockito.verify(xBulkFeign).abortPendingBulkProcess(PRODUCT_CODE);
  }

  @Test
  public void abortPendingDownloadsByEntityTest() {
    Mockito.when(xBulkFeign.clearInProgressDownloads(ENTITY,STATE))
      .thenReturn(new GdnBaseRestResponse(true));
    utilityService.abortPendingDownloadsByEntity(ENTITY,STATE);
    Mockito.verify(xBulkFeign).clearInProgressDownloads(ENTITY,STATE);
  }

  @Test
  public void deleteProductCollectionTest() {
    Mockito.when(pbpFeign.deleteProductCollection(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
      Mockito.any(DeleteProductRequest.class))).thenReturn(new GdnBaseRestResponse(true));
    DeleteProductWebRequest deleteProductWebRequest =
      DeleteProductWebRequest.builder().productCode(PRODUCT_CODE).notes(STATE).build();
    utilityService.deleteProductCollection(Constants.DEFAULT_USERNAME, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CHANNEL_ID, false,
      deleteProductWebRequest);
    Mockito.verify(pbpFeign)
      .deleteProductCollection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.any(DeleteProductRequest.class));
  }

  @Test
  public void reindexActiveProductByProductCodeTest() {
    Mockito.when(pbpFeign.reindexActiveProductByProductCode(PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(true));
    utilityService.reindexActiveProductByProductCode(PRODUCT_CODE);
    Mockito.verify(pbpFeign).reindexActiveProductByProductCode(PRODUCT_CODE);
  }

  @Test
  public void fetchDataFromBigQueryForMasterSkuReviewTest() {
    BigQueryFetchRequest bigQueryFetchRequest = new BigQueryFetchRequest();
    bigQueryFetchRequest.setAnchorDetailsDeltaFetch(true);
    BigQueryFetchWebRequest bigQueryFetchWebRequest = new BigQueryFetchWebRequest();
    bigQueryFetchWebRequest.setAnchorDetailsDeltaFetch(true);
    Mockito.when(masterCatalogFeign.fetchDataFromBigQuery(PROCESS_NAME, bigQueryFetchRequest)).thenReturn(new GdnBaseRestResponse(true));
    utilityService.fetchDataFromBigQueryForMasterSkuReview(PROCESS_NAME, bigQueryFetchWebRequest);
    Mockito.verify(masterCatalogFeign).fetchDataFromBigQuery(PROCESS_NAME, bigQueryFetchRequest);
  }

  @Test
  public void migrateProductAndL5DetailsByProductSkuTest() {
    GdnBaseRestResponse response =
      new GdnBaseRestResponse(null, null, true, Constants.DEFAULT_REQUEST_ID);
    ProductAndL5MigrationRequest productAndL5MigrationRequest =
      ProductAndL5MigrationRequest.builder().productSku(PRODUCT_ID)
        .productType(ProductType.BIG_PRODUCT).dimensionsMissing(true).build();
    Mockito.when(pbpFeign.migrateProductAndL5DetailsByProductSku(Constants.STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, productAndL5MigrationRequest))
      .thenReturn(response);
    utilityService.migrateProductAndL5DetailsByProductSku(Constants.STORE_ID,
      Constants.DEFAULT_REQUEST_ID,
       Constants.DEFAULT_CLIENT_ID,Constants.DEFAULT_CHANNEL_ID,
      productAndL5MigrationRequest, Constants.DEFAULT_USERNAME);
    Mockito.verify(pbpFeign)
      .migrateProductAndL5DetailsByProductSku(Constants.STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        productAndL5MigrationRequest);
  }

  @Test
  public void updateSystemParametersInPCBTest() {
    GdnBaseRestResponse response =
      new GdnBaseRestResponse(null, null, true, Constants.DEFAULT_REQUEST_ID);
    SystemParameterRequest systemParameterRequest = new SystemParameterRequest();
    systemParameterRequest.setValue(SYSTEM_PARAM_VALUE);
    systemParameterRequest.setVariable(SYSTEM_PARAMETER_VARIABLE);
    Mockito.when(pcbFeign.updateSystemParameter(Constants.STORE_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CHANNEL_ID, systemParameterRequest))
      .thenReturn(response);
    utilityService.updateSystemParameterInPCB(Constants.STORE_ID, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CHANNEL_ID, systemParameterRequest);
    Mockito.verify(pcbFeign).updateSystemParameter(Constants.STORE_ID, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CHANNEL_ID, systemParameterRequest);
  }

  @Test
  void generateProductScoreByProductSkuOrProductCode_shouldCallFeignAndValidateResponse() {
    // Arrange
    String storeId = "10001";
    String channelId = "ProductChannel";
    String clientId = "ProductClient";
    String requestId = "request-40de6d5c-6dd7-45cb-a141-e10570774b06";
    String username = "1";
    boolean updateCategory = false;
    String productSku = "1";
    String productCode = "1";
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);

    Mockito.when(
      xProductFeign.generateProductScoreByProductSkuOrProductCode(storeId, channelId, clientId,
        requestId, username, updateCategory, productSku, productCode)).thenReturn(response);

    // Act
    utilityService.generateProductScoreByProductSkuOrProductCode(storeId, channelId, clientId,
      requestId, username, updateCategory, productSku, productCode);

    // Assert
    Mockito.verify(xProductFeign)
      .generateProductScoreByProductSkuOrProductCode(storeId, channelId, clientId, requestId,
        username, updateCategory, productSku, productCode);
  }

  @Test
  void publishProductAttributeExtractionsTest() {
    String storeId = "10001";
    ProductAttributeExtractionsRequest productAttributeExtractionsRequest = new ProductAttributeExtractionsRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);

    Mockito.when(productAnalyticsFeign.publishEventsByProductSku(storeId, productAttributeExtractionsRequest))
      .thenReturn(response);

    utilityService.publishProductAttributeExtractions(storeId, productAttributeExtractionsRequest);

    Mockito.verify(productAnalyticsFeign)
        .publishEventsByProductSku(storeId, productAttributeExtractionsRequest);
  }
}