package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.request.MerchantConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;

public class BulkConfigurationServiceBeanTest {

  private static final String STORE_ID = "storeId";
  private static final String UPDATED_BY = "updatedBy";
  private static final String REQUEST_ID = "requestId";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String USERNAME = "username";
  private static final String MERCHANT_CODE_1 = "merchantCode1";
  private static final String MERCHANT_NAME_1 = "merchantName1";
  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String CATEGORY_NAME_1 = "categoryName1";
  private static final String SUCCESS = "success";
  private static final String REVIEW_CONFIG = "status";
  private static final String ACTION_TYPE_MERCHANT = "Merchant";
  private static final String ACTION_TYPE_CATEGORY = "Category";
  private static final String CODE = "code";
  private static final String NAME = "name";
  private static final String SEARCH_KEY = "searchKey";
  private static final String SORT_ORDER = "sortOrder";
  private static final String LANGUAGE = "eng";
  private static final String FILENAME = "file-name";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String CONFIG_TYPE = "configType";

  @Mock
  private PCBOutboundService pcbOutboundService;

  @InjectMocks
  private BulkConfigurationServiceBean bulkConfigurationServiceBean;

  private BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest;
  private MerchantConfigurationRequest merchantConfigurationRequest;
  private CategoryConfigurationRequest categoryConfigurationRequest;
  private BulkMerchantConfigUploadResponse bulkMerchantConfigUploadResponse;
  private MerchantConfigurationRequestList merchantConfigurationRequestList;
  private CategoryConfigurationRequestList categoryConfigurationRequestList;
  private BulkCategoryConfigUploadResponse bulkCategoryConfigUploadResponse;
  private AttributeCodesRequest attributeCodesRequest;
  private BulkConfigDataResponse bulkConfigDataResponse;
  private BulkDownloadRequest bulkDownloadRequest;
  private ConfigurationFilterRequest configurationFilterRequest;
  private CategoryConfigurationFilterResponse categoryConfigurationFilterResponse;
  private MerchantConfigurationFilterResponse merchantConfigurationFilterResponse;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);

    bulkConfigurationUpdateRequest = new BulkConfigurationUpdateRequest();
    bulkConfigurationUpdateRequest.setRequestId(REQUEST_ID);
    bulkConfigurationUpdateRequest.setStoreId(STORE_ID);
    bulkConfigurationUpdateRequest.setActionType(ACTION_TYPE_MERCHANT);
    bulkConfigurationUpdateRequest.setUpdatedBy(UPDATED_BY);

    merchantConfigurationRequest =
        MerchantConfigurationRequest.builder().businessPartnerCode(MERCHANT_CODE_1).businessPartnerName(MERCHANT_NAME_1)
            .reviewConfig(REVIEW_CONFIG).build();

    merchantConfigurationRequestList = MerchantConfigurationRequestList.builder()
        .merchantConfigurationRequestList(Arrays.asList(merchantConfigurationRequest)).build();

    bulkMerchantConfigUploadResponse = new BulkMerchantConfigUploadResponse();
    bulkMerchantConfigUploadResponse.setErrorMessage(SUCCESS);
    bulkMerchantConfigUploadResponse.setBusinessPartnerName(MERCHANT_NAME_1);
    bulkMerchantConfigUploadResponse.setBusinessPartnerCode(MERCHANT_CODE_1);

    categoryConfigurationRequest =
        CategoryConfigurationRequest.builder().categoryCode(CATEGORY_CODE_1).categoryName(CATEGORY_NAME_1)
            .reviewConfig(REVIEW_CONFIG).build();

    categoryConfigurationRequestList = CategoryConfigurationRequestList.builder()
        .categoryConfigurationRequestList(Arrays.asList(categoryConfigurationRequest)).build();

    bulkCategoryConfigUploadResponse =
        BulkCategoryConfigUploadResponse.builder().categoryCode(CATEGORY_CODE_1).categoryName(CATEGORY_NAME_1)
            .errorMessage(SUCCESS).build();

    configurationFilterRequest =
        ConfigurationFilterRequest.builder().categoryCode(CODE).sortOrder(SORT_ORDER).searchKey(SEARCH_KEY)
            .reviewConfig(REVIEW_CONFIG).build();

    bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setUsername(USERNAME);
    bulkDownloadRequest.setRequestId(REQUEST_ID);

    attributeCodesRequest = new AttributeCodesRequest();
    attributeCodesRequest.setAttributeCodes(Arrays.asList(CODE));

    bulkConfigDataResponse = BulkConfigDataResponse.builder().code(CODE).name(NAME).reviewConfig(REVIEW_CONFIG).build();

    categoryConfigurationFilterResponse =
        CategoryConfigurationFilterResponse.builder().categoryCode(CODE).categoryName(NAME).reviewConfig(REVIEW_CONFIG)
            .build();

    merchantConfigurationFilterResponse =
        MerchantConfigurationFilterResponse.builder().merchantName(CODE).merchantCode(NAME).reviewConfig(REVIEW_CONFIG)
            .build();

  }

  @Test
  public void bulkMerchantConfigUploadTest() throws Exception {
    Mockito.when(pcbOutboundService
        .bulkMerchantConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkConfigurationUpdateRequest.getRequestId(), bulkConfigurationUpdateRequest.getUpdatedBy(),
            merchantConfigurationRequestList)).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(bulkMerchantConfigUploadResponse), new PageMetaData(), REQUEST_ID));
    List<BulkMerchantConfigUploadResponse> response = bulkConfigurationServiceBean
        .bulkMerchantConfigUpload(Arrays.asList(merchantConfigurationRequest), bulkConfigurationUpdateRequest);
    Mockito.verify(pcbOutboundService)
        .bulkMerchantConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkConfigurationUpdateRequest.getRequestId(), bulkConfigurationUpdateRequest.getUpdatedBy(),
            merchantConfigurationRequestList);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(MERCHANT_CODE_1, response.get(0).getBusinessPartnerCode());
  }

  @Test
  public void bulkMerchantConfigUploadNullResponseTest() throws Exception {
    Mockito.when(pcbOutboundService
        .bulkMerchantConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkConfigurationUpdateRequest.getRequestId(), bulkConfigurationUpdateRequest.getUpdatedBy(),
            merchantConfigurationRequestList)).thenReturn(null);
    List<BulkMerchantConfigUploadResponse> response = null;
    Assertions.assertThrows(ApplicationException.class,
        () -> bulkConfigurationServiceBean.bulkMerchantConfigUpload(
            Arrays.asList(merchantConfigurationRequest), bulkConfigurationUpdateRequest));
    Mockito.verify(pcbOutboundService)
        .bulkMerchantConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID,
            Constant.CLIENT_ID, bulkConfigurationUpdateRequest.getRequestId(),
            bulkConfigurationUpdateRequest.getUpdatedBy(), merchantConfigurationRequestList);
  }

  @Test
  public void bulkMerchantConfigUploadExceptionTest() throws Exception {
    Mockito.when(pcbOutboundService
        .bulkMerchantConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkConfigurationUpdateRequest.getRequestId(), bulkConfigurationUpdateRequest.getUpdatedBy(),
            merchantConfigurationRequestList)).thenThrow(ApplicationRuntimeException.class);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkConfigurationServiceBean.bulkMerchantConfigUpload(
              Arrays.asList(merchantConfigurationRequest), bulkConfigurationUpdateRequest));
    } catch (ApplicationRuntimeException e) {} finally {
      Mockito.verify(pcbOutboundService)
          .bulkMerchantConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID,
              Constant.CLIENT_ID, bulkConfigurationUpdateRequest.getRequestId(),
              bulkConfigurationUpdateRequest.getUpdatedBy(), merchantConfigurationRequestList);
    }
  }

  @Test
  public void bulkCategoryConfigUploadTest() throws Exception {
    Mockito.when(pcbOutboundService
        .bulkCategoryConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkConfigurationUpdateRequest.getRequestId(), bulkConfigurationUpdateRequest.getUpdatedBy(),
            categoryConfigurationRequestList)).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(bulkCategoryConfigUploadResponse), new PageMetaData(), REQUEST_ID));
    List<BulkCategoryConfigUploadResponse> response;
    response = bulkConfigurationServiceBean
        .bulkCategoryConfigUpload(Arrays.asList(categoryConfigurationRequest), bulkConfigurationUpdateRequest);
    Mockito.verify(pcbOutboundService)
        .bulkCategoryConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkConfigurationUpdateRequest.getRequestId(), bulkConfigurationUpdateRequest.getUpdatedBy(),
            categoryConfigurationRequestList);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(CATEGORY_CODE_1, response.get(0).getCategoryCode());
  }

  @Test
  public void bulkCategoryConfigUploadExceptionTest() throws Exception {
    Mockito.when(pcbOutboundService
        .bulkCategoryConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkConfigurationUpdateRequest.getRequestId(), bulkConfigurationUpdateRequest.getUpdatedBy(),
            categoryConfigurationRequestList)).thenThrow(ApplicationRuntimeException.class);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkConfigurationServiceBean.bulkCategoryConfigUpload(
              Arrays.asList(categoryConfigurationRequest), bulkConfigurationUpdateRequest));
    } catch (ApplicationRuntimeException e) { } finally {
      Mockito.verify(pcbOutboundService)
          .bulkCategoryConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID,
              Constant.CLIENT_ID, bulkConfigurationUpdateRequest.getRequestId(),
              bulkConfigurationUpdateRequest.getUpdatedBy(), categoryConfigurationRequestList);
    }
  }

  @Test
  public void bulkCategoryConfigUploadNullResponseTest() throws Exception {
    Mockito.when(pcbOutboundService
        .bulkCategoryConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID, Constant.CLIENT_ID,
            bulkConfigurationUpdateRequest.getRequestId(), bulkConfigurationUpdateRequest.getUpdatedBy(),
            categoryConfigurationRequestList)).thenReturn(null);
    List<BulkCategoryConfigUploadResponse> response = null;
    Assertions.assertThrows(ApplicationException.class,
        () -> bulkConfigurationServiceBean
        .bulkCategoryConfigUpload(Arrays.asList(categoryConfigurationRequest), bulkConfigurationUpdateRequest));
    Mockito.verify(pcbOutboundService)
        .bulkCategoryConfigUpload(bulkConfigurationUpdateRequest.getStoreId(), Constant.CHANNEL_ID,
            Constant.CLIENT_ID, bulkConfigurationUpdateRequest.getRequestId(),
            bulkConfigurationUpdateRequest.getUpdatedBy(), categoryConfigurationRequestList);
  }

  @Test
  public void fetchConfigDetailsByCodesTest() throws Exception {
    Mockito.when(pcbOutboundService
        .fetchConfigDetailsByCodes(eq(STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(REQUEST_ID),
            eq(USERNAME), eq(CONFIG_TYPE), Mockito.any(AttributeCodesRequest.class)))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(bulkConfigDataResponse), new PageMetaData(), REQUEST_ID));
    List<BulkConfigDataResponse> response = bulkConfigurationServiceBean
        .fetchConfigDetailsByCodes(STORE_ID, CONFIG_TYPE, bulkDownloadRequest, Arrays.asList(CODE));
    Mockito.verify(pcbOutboundService)
        .fetchConfigDetailsByCodes(eq(STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(REQUEST_ID),
            eq(USERNAME), eq(CONFIG_TYPE), Mockito.any(AttributeCodesRequest.class));
    Assertions.assertNotNull(response);
  }

  @Test
  public void fetchConfigDetailsByCodesExceptionTest() throws Exception {
    Mockito.when(pcbOutboundService
        .fetchConfigDetailsByCodes(eq(STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(REQUEST_ID),
            eq(USERNAME), eq(CONFIG_TYPE), Mockito.any(AttributeCodesRequest.class))).thenReturn(null);
    List<BulkConfigDataResponse> response = null;
    Assertions.assertThrows(ApplicationException.class,
        () -> bulkConfigurationServiceBean
        .fetchConfigDetailsByCodes(STORE_ID, CONFIG_TYPE, bulkDownloadRequest, Arrays.asList(CODE)));
    Mockito.verify(pcbOutboundService)
        .fetchConfigDetailsByCodes(eq(STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(REQUEST_ID), eq(USERNAME), eq(CONFIG_TYPE),
            Mockito.any(AttributeCodesRequest.class));
  }

  @Test
  public void getCategoryConfigurationListTest() throws Exception {
    Mockito.when(pcbOutboundService
        .getCategoryConfigurationList(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
            configurationFilterRequest, PAGE, SIZE)).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(categoryConfigurationFilterResponse), new PageMetaData(), REQUEST_ID));
    GdnRestListResponse<CategoryConfigurationFilterResponse> response = bulkConfigurationServiceBean
        .getCategoryConfigurationList(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
            configurationFilterRequest, PAGE, SIZE);
    Mockito.verify(pcbOutboundService)
        .getCategoryConfigurationList(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
            configurationFilterRequest, PAGE, SIZE);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getCategoryConfigurationListExceptionTest() throws Exception {
    Mockito.when(pcbOutboundService
        .getCategoryConfigurationList(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
            configurationFilterRequest, PAGE, SIZE)).thenReturn(null);
    GdnRestListResponse<CategoryConfigurationFilterResponse> response = null;
    Assertions.assertThrows(ApplicationException.class,
        () -> bulkConfigurationServiceBean.getCategoryConfigurationList(STORE_ID,
            Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
            configurationFilterRequest, PAGE, SIZE));
    Mockito.verify(pcbOutboundService)
        .getCategoryConfigurationList(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
            configurationFilterRequest, PAGE, SIZE);
  }

  @Test
  public void getMerchantConfigurationListTest() throws Exception {
    Mockito.when(pcbOutboundService
        .getMerchantConfigurationList(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
            configurationFilterRequest, PAGE, SIZE)).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(merchantConfigurationFilterResponse), new PageMetaData(), REQUEST_ID));
    GdnRestListResponse<MerchantConfigurationFilterResponse> response = bulkConfigurationServiceBean
        .getMerchantConfigurationList(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
            configurationFilterRequest, PAGE, SIZE);
    Mockito.verify(pcbOutboundService)
        .getMerchantConfigurationList(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
            configurationFilterRequest, PAGE, SIZE);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getMerchantConfigurationListExceptionTest() throws Exception {
    Mockito.when(pcbOutboundService
        .getMerchantConfigurationList(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
            configurationFilterRequest, PAGE, SIZE)).thenReturn(null);
    GdnRestListResponse<MerchantConfigurationFilterResponse> response = null;
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> bulkConfigurationServiceBean.getMerchantConfigurationList(STORE_ID,
              Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
              configurationFilterRequest, PAGE, SIZE));
    } finally {
      Mockito.verify(pcbOutboundService)
          .getMerchantConfigurationList(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
              configurationFilterRequest, PAGE, SIZE);
      Assertions.assertNull(response);
    }
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(pcbOutboundService);
  }
}
