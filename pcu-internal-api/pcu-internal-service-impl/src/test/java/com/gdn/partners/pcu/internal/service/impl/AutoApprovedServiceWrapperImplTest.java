package com.gdn.partners.pcu.internal.service.impl;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedSellerResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.client.feign.ProductAnalyticsFeign;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedSelectedDownloadRequest;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedListWebResponse;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.event.model.BulkReviewUploadModel;
import com.gdn.partners.pcu.internal.service.impl.event.model.DeleteAutoApprovedProductsEventModel;
import com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.AutoApprovedProductsDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovedProductsDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.properties.KafkaTopicProperties;
import com.gdn.partners.pcu.internal.service.PBPOutboundService;
import com.gdn.partners.pcu.internal.service.ProductService;
import com.gdn.partners.pcu.internal.service.ProductServiceWrapper;
import com.gdn.partners.pcu.internal.service.impl.exception.ConstraintViolationException;
import com.gdn.partners.pcu.internal.web.model.enums.AutoApprovedActionsEnum;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovedProductsActionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductWebRequest;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;

import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockMultipartFile;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Collections;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AutoApprovedServiceWrapperImplTest {

  private static final String PRODUCT_CODE = "product-code";
  private static final String PRODUCT_NAME = "product-name";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String REASON = "reason";
  private static final String NOTES = "notes";
  private static final String CLIENT_ID = "clientId";
  private static final String KEYWORD = "keyword";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String ASSIGNEE = "assignee";
  private byte[] fileContent;
  private static final String PATH = "path";
  private static final String FILE = "file";
  private static final String TYPE = "type";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final String STORE_ID = "store-id";
  private static final String VENDOR_CODE = "vendorCode";


  @InjectMocks
  private AutoApprovedServiceWrapperImpl autoApprovedServiceWrapper;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private PBPOutboundService pbpOutboundService;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ProductService productService;

  @Mock
  private ProductAnalyticsFeign productAnalyticsFeign;

  private AutoApprovedProductsActionWebRequest autoApprovedProductsActionWebRequest;
  private ProductBasicResponse productBasicResponse;

  @Captor
  private ArgumentCaptor<AutoApprovedProductsDownloadRequest>
      autoApprovedProductsDownloadRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkReviewUploadModel> bulkReviewUploadModelArgumentCaptor;

  private AutoApprovedProductsDownloadWebRequest autoApprovedProductsDownloadWebRequest;

  @BeforeEach
  public void setup() {
    autoApprovedProductsActionWebRequest = new AutoApprovedProductsActionWebRequest();
    autoApprovedProductsDownloadWebRequest = new AutoApprovedProductsDownloadWebRequest();
    fileContent = new byte[] {-1, -40, -20, -10};

    productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU);
    productBasicResponse.setProductExists(true);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
    Mockito.verifyNoMoreInteractions(pbpOutboundService);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(productServiceWrapper);
    Mockito.verifyNoMoreInteractions(kafkaPublisher);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(productAnalyticsFeign);
    Mockito.verifyNoMoreInteractions(fileStorageService);
  }

  @Test
  void performActionOnAutoApprovedProductsSuspendTest() throws Exception {
    autoApprovedProductsActionWebRequest.setAction(AutoApprovedActionsEnum.SUSPEND.name());
    autoApprovedProductsActionWebRequest.setProductName(PRODUCT_NAME);
    autoApprovedProductsActionWebRequest.setReason(REASON);
    autoApprovedProductsActionWebRequest.setNotes(NOTES);
    Mockito.when(pbpOutboundService.getProductSkuByProductCode(PRODUCT_CODE)).thenReturn(PRODUCT_SKU);
    Mockito.when(kafkaTopicProperties.getDeleteAutoApprovedProductEventName()).thenReturn(NOTES);
    Mockito.when(kafkaTopicProperties.getInternalHistoryEventName()).thenReturn(NOTES);
    Mockito.when(productService.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    autoApprovedServiceWrapper.performActionOnAutoApprovedProducts(PRODUCT_CODE, autoApprovedProductsActionWebRequest);
    Mockito.verify(pbpOutboundService).getProductSkuByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).doSuspensionAction(Mockito.any());
    Mockito.verify(kafkaTopicProperties).getInternalHistoryEventName();
    Mockito.verify(kafkaTopicProperties).getDeleteAutoApprovedProductEventName();
    Mockito.verify(kafkaPublisher, Mockito.times(2)).send(Mockito.any(), Mockito.anyString(), Mockito.any());
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(productService).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU));
  }

  @Test
  void performActionOnAutoApprovedProductsSuspendTest2() throws Exception {
    autoApprovedProductsActionWebRequest.setAction(AutoApprovedActionsEnum.SUSPEND.name());
    autoApprovedProductsActionWebRequest.setProductName(PRODUCT_NAME);
    autoApprovedProductsActionWebRequest.setReason(REASON);
    autoApprovedProductsActionWebRequest.setNotes(NOTES);
    Mockito.when(pbpOutboundService.getProductSkuByProductCode(PRODUCT_CODE)).thenReturn(PRODUCT_SKU);
    Mockito.when(kafkaTopicProperties.getDeleteAutoApprovedProductEventName()).thenReturn(NOTES);
    Mockito.when(kafkaTopicProperties.getInternalHistoryEventName()).thenReturn(NOTES);
    productBasicResponse.setSuspended(true);
    productBasicResponse.setMarkForDelete(true);
    Mockito.when(productService.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    autoApprovedServiceWrapper.performActionOnAutoApprovedProducts(PRODUCT_CODE, autoApprovedProductsActionWebRequest);
    Mockito.verify(pbpOutboundService).getProductSkuByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).doSuspensionAction(Mockito.any());
    Mockito.verify(kafkaTopicProperties).getInternalHistoryEventName();
    Mockito.verify(kafkaTopicProperties).getDeleteAutoApprovedProductEventName();
    Mockito.verify(kafkaPublisher, Mockito.times(2)).send(Mockito.any(), Mockito.anyString(), Mockito.any());
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(productService).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU));
  }

  @Test
  void performActionOnAutoApprovedProductsNullResponseProductTest() throws Exception {
    autoApprovedProductsActionWebRequest.setAction(AutoApprovedActionsEnum.SUSPEND.name());
    autoApprovedProductsActionWebRequest.setProductName(PRODUCT_NAME);
    autoApprovedProductsActionWebRequest.setReason(REASON);
    autoApprovedProductsActionWebRequest.setNotes(NOTES);
    Mockito.when(pbpOutboundService.getProductSkuByProductCode(PRODUCT_CODE)).thenReturn(PRODUCT_SKU);
    Mockito.when(kafkaTopicProperties.getDeleteAutoApprovedProductEventName()).thenReturn(NOTES);
    Assertions.assertThrows(ConstraintViolationException.class,
        () -> autoApprovedServiceWrapper.performActionOnAutoApprovedProducts(PRODUCT_CODE,
            autoApprovedProductsActionWebRequest));
    Mockito.verify(pbpOutboundService).getProductSkuByProductCode(PRODUCT_CODE);
    Mockito.verify(kafkaTopicProperties).getDeleteAutoApprovedProductEventName();
    Mockito.verify(kafkaPublisher).send(Mockito.any(), Mockito.anyString(), Mockito.any());
    Mockito.verify(productService).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU));
  }

  @Test
  void performActionOnAutoApprovedProductsExistsFalseTest() throws Exception {
    autoApprovedProductsActionWebRequest.setAction(AutoApprovedActionsEnum.SUSPEND.name());
    autoApprovedProductsActionWebRequest.setProductName(PRODUCT_NAME);
    autoApprovedProductsActionWebRequest.setReason(REASON);
    autoApprovedProductsActionWebRequest.setNotes(NOTES);
    Mockito.when(pbpOutboundService.getProductSkuByProductCode(PRODUCT_CODE)).thenReturn(PRODUCT_SKU);
    Mockito.when(kafkaTopicProperties.getDeleteAutoApprovedProductEventName()).thenReturn(NOTES);
    productBasicResponse.setProductExists(false);
    Mockito.when(productService.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Assertions.assertThrows(ConstraintViolationException.class,
        () -> autoApprovedServiceWrapper.performActionOnAutoApprovedProducts(PRODUCT_CODE,
            autoApprovedProductsActionWebRequest));
    Mockito.verify(pbpOutboundService).getProductSkuByProductCode(PRODUCT_CODE);
    Mockito.verify(kafkaTopicProperties).getDeleteAutoApprovedProductEventName();
    Mockito.verify(kafkaPublisher).send(Mockito.any(), Mockito.anyString(), Mockito.any());
    Mockito.verify(productService).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU));
  }

  @Test
  void performActionOnAutoApprovedProductsExistsDeletedTest() throws Exception {
    autoApprovedProductsActionWebRequest.setAction(AutoApprovedActionsEnum.SUSPEND.name());
    autoApprovedProductsActionWebRequest.setProductName(PRODUCT_NAME);
    autoApprovedProductsActionWebRequest.setReason(REASON);
    autoApprovedProductsActionWebRequest.setNotes(NOTES);
    Mockito.when(pbpOutboundService.getProductSkuByProductCode(PRODUCT_CODE)).thenReturn(PRODUCT_SKU);
    Mockito.when(kafkaTopicProperties.getDeleteAutoApprovedProductEventName()).thenReturn(NOTES);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setMarkForDelete(true);
    productBasicResponse.setSuspended(false);
    Mockito.when(productService.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Assertions.assertThrows(ConstraintViolationException.class,
        () -> autoApprovedServiceWrapper.performActionOnAutoApprovedProducts(PRODUCT_CODE,
            autoApprovedProductsActionWebRequest));
    Mockito.verify(pbpOutboundService).getProductSkuByProductCode(PRODUCT_CODE);
    Mockito.verify(kafkaTopicProperties).getDeleteAutoApprovedProductEventName();
    Mockito.verify(kafkaPublisher).send(Mockito.any(), Mockito.anyString(), Mockito.any());
    Mockito.verify(productService).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU));
  }

  @Test
  void performActionOnAutoApprovedProductsSuspendProductSkuEmptyTest() throws Exception {
    autoApprovedProductsActionWebRequest.setAction(AutoApprovedActionsEnum.SUSPEND.name());
    autoApprovedProductsActionWebRequest.setProductName(PRODUCT_NAME);
    autoApprovedProductsActionWebRequest.setReason(REASON);
    autoApprovedProductsActionWebRequest.setNotes(NOTES);
    Mockito.when(pbpOutboundService.getProductSkuByProductCode(PRODUCT_CODE)).thenReturn(StringUtils.EMPTY);
    Mockito.when(kafkaTopicProperties.getDeleteAutoApprovedProductEventName()).thenReturn(NOTES);
    Assertions.assertThrows(ConstraintViolationException.class,
        () -> autoApprovedServiceWrapper.performActionOnAutoApprovedProducts(PRODUCT_CODE,
            autoApprovedProductsActionWebRequest));
    Mockito.verify(pbpOutboundService).getProductSkuByProductCode(PRODUCT_CODE);
    Mockito.verify(kafkaTopicProperties).getDeleteAutoApprovedProductEventName();
    Mockito.verify(kafkaPublisher).send(Mockito.any(), Mockito.anyString(), Mockito.any());
  }

  @Test
  void performActionOnAutoApprovedProductsSuspendReasonEmptyTest() throws Exception {
    autoApprovedProductsActionWebRequest.setAction(AutoApprovedActionsEnum.SUSPEND.name());
    autoApprovedProductsActionWebRequest.setProductName(PRODUCT_NAME);
    autoApprovedProductsActionWebRequest.setNotes(NOTES);
    Assertions.assertThrows(ConstraintViolationException.class,
        () -> autoApprovedServiceWrapper.performActionOnAutoApprovedProducts(PRODUCT_CODE,
            autoApprovedProductsActionWebRequest));
  }

  @Test
  void performActionOnAutoApprovedProductsSuspendNotesEmptyTest() throws Exception {
    autoApprovedProductsActionWebRequest.setAction(AutoApprovedActionsEnum.SUSPEND.name());
    autoApprovedProductsActionWebRequest.setProductName(PRODUCT_NAME);
    autoApprovedProductsActionWebRequest.setReason(NOTES);
    Assertions.assertThrows(ConstraintViolationException.class,
        () -> autoApprovedServiceWrapper.performActionOnAutoApprovedProducts(PRODUCT_CODE,
            autoApprovedProductsActionWebRequest));
  }


  @Test
  void performActionOnAutoApprovedProductsAcceptTest() throws Exception {
    autoApprovedProductsActionWebRequest.setAction(AutoApprovedActionsEnum.ACCEPT.name());
    autoApprovedProductsActionWebRequest.setContentUpdated(true);
    autoApprovedProductsActionWebRequest.setProductWebRequest(
        ProductWebRequest.builder().productCode(PRODUCT_CODE).build());
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(NOTES);
    Mockito.when(clientParameterHelper.getUserType()).thenReturn(NOTES);
    Mockito.when(kafkaTopicProperties.getDeleteAutoApprovedProductEventName()).thenReturn(NOTES);
    Mockito.when(kafkaTopicProperties.getInternalHistoryEventName()).thenReturn(NOTES);
    autoApprovedServiceWrapper.performActionOnAutoApprovedProducts(PRODUCT_CODE, autoApprovedProductsActionWebRequest);
    Mockito.verify(kafkaTopicProperties).getInternalHistoryEventName();
    Mockito.verify(kafkaTopicProperties).getDeleteAutoApprovedProductEventName();
    Mockito.verify(kafkaPublisher, Mockito.times(2)).send(Mockito.any(), Mockito.anyString(), Mockito.any());
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper, Mockito.times(2)).getStoreId();
    Mockito.verify(clientParameterHelper, Mockito.times(2)).getUsername();
    Mockito.verify(clientParameterHelper, Mockito.times(2)).getUserType();
    Mockito.verify(productServiceWrapper)
        .updateProduct(Mockito.anyString(), Mockito.anyString(), Mockito.any(), Mockito.anyBoolean(),
            Mockito.anyBoolean());
  }

  @Test
  void performActionOnAutoApprovedProductsAcceptWithoutEditTest() throws Exception {
    autoApprovedProductsActionWebRequest.setAction(AutoApprovedActionsEnum.ACCEPT.name());
    Mockito.when(kafkaTopicProperties.getDeleteAutoApprovedProductEventName()).thenReturn(NOTES);
    Mockito.when(kafkaTopicProperties.getInternalHistoryEventName()).thenReturn(NOTES);
    autoApprovedServiceWrapper.performActionOnAutoApprovedProducts(PRODUCT_CODE, autoApprovedProductsActionWebRequest);
    Mockito.verify(kafkaTopicProperties).getInternalHistoryEventName();
    Mockito.verify(kafkaTopicProperties).getDeleteAutoApprovedProductEventName();
    Mockito.verify(kafkaPublisher, Mockito.times(2)).send(Mockito.any(), Mockito.anyString(), Mockito.any());
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getUsername();
  }

  @Test
  void testFetchAutoApprovedProductDetail_NotMarkForDelete() {
    ProductDetailWebResponse mockResponse = new ProductDetailWebResponse();
    mockResponse.setMarkForDelete(false);
    Mockito.when(productService.getProductDetail(PRODUCT_CODE, false, CLIENT_ID, false)).thenReturn(mockResponse);
    AutoApprovedSelectedDownloadRequest request =
      AutoApprovedSelectedDownloadRequest.builder().productCodes(Collections.singletonList(PRODUCT_CODE)).build();
    AutoApprovedListWebResponse autoApprovedListWebResponse = new AutoApprovedListWebResponse();
    autoApprovedListWebResponse.setProductCode(PRODUCT_CODE);
    autoApprovedListWebResponse.setAssignedTo(ASSIGNEE);
    autoApprovedListWebResponse.setSourceEn("sourceEn");
    autoApprovedListWebResponse.setSourceId("sourceId");
    autoApprovedListWebResponse.setReason("reason");
    AutoApprovedSellerResponse sellerResponse =
        AutoApprovedSellerResponse.builder().official(true).build();
    autoApprovedListWebResponse.setSeller(sellerResponse);
    GdnRestListResponse<AutoApprovedListWebResponse> response =
      new GdnRestListResponse<>(Collections.singletonList(autoApprovedListWebResponse),
        new PageMetaData(10, 0, 10), Constants.REQUEST_ID);
    Mockito.when(productAnalyticsFeign.fetchAutoApprovedSelectedProductsList(request)).thenReturn(response);
    ProductDetailWebResponse result =
        autoApprovedServiceWrapper.fetchAutoApprovedProductDetail(PRODUCT_CODE, CLIENT_ID);
    Assertions.assertNotNull(result);
    Assertions.assertFalse(result.isMarkForDelete());
    Assertions.assertEquals("reason", result.getReason());
    Assertions.assertEquals("sourceEn", result.getSourceEn());
    Assertions.assertEquals("sourceId", result.getSourceId());
    Mockito.verify(productAnalyticsFeign).fetchAutoApprovedSelectedProductsList(request);
    Mockito.verify(productService).getProductDetail(PRODUCT_CODE, false, CLIENT_ID, false);
    Mockito.verifyNoInteractions(kafkaPublisher);
  }

  @Test
  void testFetchAutoApprovedProductDetail_EmptyProductAnalyticsResponse() {
    ProductDetailWebResponse mockResponse = new ProductDetailWebResponse();
    mockResponse.setMarkForDelete(false);
    mockResponse.setAssignedTo(StringUtils.EMPTY);
    Mockito.when(productService.getProductDetail(PRODUCT_CODE, false, CLIENT_ID, false))
        .thenReturn(mockResponse);
    AutoApprovedSelectedDownloadRequest request = AutoApprovedSelectedDownloadRequest.builder()
        .productCodes(Collections.singletonList(PRODUCT_CODE)).build();
    AutoApprovedListWebResponse autoApprovedListWebResponse = new AutoApprovedListWebResponse();
    autoApprovedListWebResponse.setProductCode(PRODUCT_CODE);
    autoApprovedListWebResponse.setAssignedTo(ASSIGNEE);
    GdnRestListResponse<AutoApprovedListWebResponse> response =
        new GdnRestListResponse<>(Collections.emptyList(), new PageMetaData(10, 0, 10),
            Constants.REQUEST_ID);
    Mockito.when(productAnalyticsFeign.fetchAutoApprovedSelectedProductsList(request))
        .thenReturn(response);
    ProductDetailWebResponse result =
        autoApprovedServiceWrapper.fetchAutoApprovedProductDetail(PRODUCT_CODE, CLIENT_ID);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(StringUtils.EMPTY, result.getAssignedTo());
    Assertions.assertFalse(result.isMarkForDelete());
    Mockito.verify(productAnalyticsFeign).fetchAutoApprovedSelectedProductsList(request);
    Mockito.verify(productService).getProductDetail(PRODUCT_CODE, false, CLIENT_ID, false);
    Mockito.verifyNoInteractions(kafkaPublisher);
  }

  @Test
  void testFetchAutoApprovedProductDetail_MarkForDelete() {
    AutoApprovedListWebResponse autoApprovedListWebResponse = new AutoApprovedListWebResponse();
    autoApprovedListWebResponse.setProductCode(PRODUCT_CODE);
    autoApprovedListWebResponse.setAssignedTo(ASSIGNEE);
    GdnRestListResponse<AutoApprovedListWebResponse> response =
      new GdnRestListResponse<>(Collections.singletonList(autoApprovedListWebResponse),
        new PageMetaData(10, 0, 10), Constants.REQUEST_ID);
    Mockito.when(productService.getProductDetail(PRODUCT_CODE, false, CLIENT_ID, false))
        .thenThrow(new ApplicationRuntimeException());
    Mockito.when(kafkaTopicProperties.getDeleteAutoApprovedProductEventName()).thenReturn(NOTES);
    ProductDetailWebResponse result =
        autoApprovedServiceWrapper.fetchAutoApprovedProductDetail(PRODUCT_CODE, CLIENT_ID);
    Assertions.assertNotNull(result);
    Assertions.assertTrue(result.isMarkForDelete());
    Mockito.verify(kafkaTopicProperties).getDeleteAutoApprovedProductEventName();
    Mockito.verify(productService).getProductDetail(PRODUCT_CODE, false, CLIENT_ID, false);
    Mockito.verify(kafkaPublisher).send(Mockito.eq(NOTES), Mockito.eq(PRODUCT_CODE),
        Mockito.any(DeleteAutoApprovedProductsEventModel.class));
  }

  @Test
  void downloadAllItemsForAutoApprovedProductsTest() {
    autoApprovedProductsDownloadWebRequest.setKeyword(KEYWORD);
    autoApprovedProductsDownloadWebRequest.setCategoryCode(CATEGORY_CODE);
    autoApprovedProductsDownloadWebRequest.setB2bActivated(true);
    autoApprovedProductsDownloadWebRequest.setB2cActivated(false);
    this.autoApprovedServiceWrapper.downloadItemsForAutoApprovedProducts(Constants.USER_NAME,
        autoApprovedProductsDownloadWebRequest);
    Mockito.verify(this.kafkaPublisher).send(Mockito.eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), eq(Constants.USER_NAME),
        autoApprovedProductsDownloadRequestArgumentCaptor.capture());
    AutoApprovedProductsDownloadRequest autoApprovedProductsDownloadRequest =
        autoApprovedProductsDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(autoApprovedProductsDownloadRequest);
    Assertions.assertEquals(KEYWORD, autoApprovedProductsDownloadRequest.getKeyword());
    Assertions.assertEquals(CATEGORY_CODE, autoApprovedProductsDownloadRequest.getCategoryCode());
    Assertions.assertEquals(BulkProcessEntity.AUTO_APPROVED_PRODUCTS_ALL_DOWNLOAD,
        autoApprovedProductsDownloadRequest.getBulkProcessEntity());
  }

  @Test
  void downloadAllItemsForAutoApprovedProductsB2bActivatedB2cActivatedFalseTest() {
    autoApprovedProductsDownloadWebRequest.setKeyword(KEYWORD);
    autoApprovedProductsDownloadWebRequest.setCategoryCode(CATEGORY_CODE);
    autoApprovedProductsDownloadWebRequest.setB2bActivated(false);
    autoApprovedProductsDownloadWebRequest.setB2cActivated(false);
    this.autoApprovedServiceWrapper.downloadItemsForAutoApprovedProducts(Constants.USER_NAME,
        autoApprovedProductsDownloadWebRequest);
    Mockito.verify(this.kafkaPublisher).send(Mockito.eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), eq(Constants.USER_NAME),
        autoApprovedProductsDownloadRequestArgumentCaptor.capture());
    AutoApprovedProductsDownloadRequest autoApprovedProductsDownloadRequest =
        autoApprovedProductsDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(autoApprovedProductsDownloadRequest);
    Assertions.assertEquals(KEYWORD, autoApprovedProductsDownloadRequest.getKeyword());
    Assertions.assertEquals(CATEGORY_CODE, autoApprovedProductsDownloadRequest.getCategoryCode());
    Assertions.assertEquals(BulkProcessEntity.AUTO_APPROVED_PRODUCTS_ALL_DOWNLOAD,
        autoApprovedProductsDownloadRequest.getBulkProcessEntity());
  }

  @Test
  void downloadAllItemsForAutoApprovedProductsB2bActivatedB2cActivatedTrueTest() {
    autoApprovedProductsDownloadWebRequest.setKeyword(KEYWORD);
    autoApprovedProductsDownloadWebRequest.setCategoryCode(CATEGORY_CODE);
    autoApprovedProductsDownloadWebRequest.setB2bActivated(true);
    autoApprovedProductsDownloadWebRequest.setB2cActivated(true);
    this.autoApprovedServiceWrapper.downloadItemsForAutoApprovedProducts(Constants.USER_NAME,
        autoApprovedProductsDownloadWebRequest);
    Mockito.verify(this.kafkaPublisher)
        .send(Mockito.eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), Mockito.eq(Constants.USER_NAME),
            autoApprovedProductsDownloadRequestArgumentCaptor.capture());
    AutoApprovedProductsDownloadRequest autoApprovedProductsDownloadRequest =
        autoApprovedProductsDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(autoApprovedProductsDownloadRequest);
    Assertions.assertEquals(KEYWORD, autoApprovedProductsDownloadRequest.getKeyword());
    Assertions.assertEquals(CATEGORY_CODE, autoApprovedProductsDownloadRequest.getCategoryCode());
    Assertions.assertEquals(BulkProcessEntity.AUTO_APPROVED_PRODUCTS_ALL_DOWNLOAD,
        autoApprovedProductsDownloadRequest.getBulkProcessEntity());
  }

  @Test
  void downloadSelectedItemsForAutoApprovedProductsTest() {
    autoApprovedProductsDownloadWebRequest.setProductCodeList(List.of(PRODUCT_CODE));
    autoApprovedProductsDownloadWebRequest.setB2bActivated(false);
    autoApprovedProductsDownloadWebRequest.setB2cActivated(true);
    this.autoApprovedServiceWrapper.downloadItemsForAutoApprovedProducts(Constants.USER_NAME,
        autoApprovedProductsDownloadWebRequest);
    Mockito.verify(this.kafkaPublisher)
        .send(Mockito.eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), Mockito.eq(Constants.USER_NAME),
            autoApprovedProductsDownloadRequestArgumentCaptor.capture());
    AutoApprovedProductsDownloadRequest autoApprovedProductsDownloadRequest =
        autoApprovedProductsDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(autoApprovedProductsDownloadRequest);
    Assertions.assertEquals(autoApprovedProductsDownloadWebRequest.getProductCodeList(),
        autoApprovedProductsDownloadRequest.getProductCodeList());
    Assertions.assertEquals(BulkProcessEntity.AUTO_APPROVED_PRODUCTS_SELECTED_DOWNLOAD,
        autoApprovedProductsDownloadRequest.getBulkProcessEntity());
  }

  @Test
  void uploadBulkAssignFileTest() throws Exception {
    mockFile();
    MockMultipartFile multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME,
        "application/vnd.ms-excel", FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(fileStorageService.uploadFilePath(any(), anyString(), anyString())).thenReturn(PATH);
    autoApprovedServiceWrapper.uploadBulkAssignFile(multipartFile, REQUEST_ID, STORE_ID, USERNAME,
        VENDOR_CODE);
    verify(fileStorageService).uploadFilePath(any(), anyString(), anyString());
    verify(kafkaPublisher).send(eq(DomainEventName.BULK_REVIEW_UPLOAD_EVENT), anyString(),
        bulkReviewUploadModelArgumentCaptor.capture());
    Assertions.assertNotNull(bulkReviewUploadModelArgumentCaptor.getValue());
    Assertions.assertEquals(BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name(),
        bulkReviewUploadModelArgumentCaptor.getValue().getBulkProcessType());
    Assertions.assertEquals(REQUEST_ID, bulkReviewUploadModelArgumentCaptor.getValue().getRequestId());
    Assertions.assertEquals(STORE_ID, bulkReviewUploadModelArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(USERNAME, bulkReviewUploadModelArgumentCaptor.getValue().getCreatedBy());
    Mockito.verify(fileStorageService).uploadFilePath(any(), anyString(), anyString());
  }

  private void mockFile() throws IOException {
    File file = new File("pathfile");
    FileUtils.writeByteArrayToFile(file, fileContent);
  }
}
