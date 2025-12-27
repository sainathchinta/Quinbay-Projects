package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.models.download.BrandAuthFilterRequest;
import com.gdn.mta.bulk.models.download.responsedata.ProductImageAndVideoResponse;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.BatchVatUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.request.MerchantConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BatchVatUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public class PCBOutboundServiceBeanTest {

  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String SUCCESS = "SUCCESS";
  private static final String MERCHANT_NAME = "merchantName";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String REVIEW_CONFIG = "status";
  private static final String CODE = "code";
  private static final String NAME = "name";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String CONFIG_TYPE = "configType";
  private static final String SEARCH_KEY = "searchKey";
  private static final String SORT_ORDER = "sortOrder";
  private static final String VALUE = "value";
  private static final String ATTRIBUTE_ID = "attributeId";
  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private static final String PRODUCT_CODE = "MTA-1234567";
  private static final String BRAND_NAME = "brand";
  private static final String BRAND_CODE = "brand-code";
  private static final String SELLER_CODE = "seller-code";
  private static final String CATEGORY_KEYWORDS_UPDATE_ERROR_MESSAGE = "Error while updating category keywords";
  private static final String ALL = "ALL";


  @Mock
  private PCBFeign pcbFeign;

  @InjectMocks
  private PCBOutboundServiceBean pcbOutboundService;

  private MerchantConfigurationRequest merchantConfigurationRequest;
  private MerchantConfigurationRequestList merchantConfigurationRequestList;
  private CategoryConfigurationRequest categoryConfigurationRequest;
  private CategoryConfigurationRequestList categoryConfigurationRequestList;
  private BulkMerchantConfigUploadResponse bulkMerchantConfigUploadResponse;
  private BulkCategoryConfigUploadResponse bulkCategoryConfigUploadResponse;
  private AttributeCodesRequest attributeCodesRequest;
  private BulkConfigDataResponse bulkConfigDataResponse;
  private ConfigurationFilterRequest configurationFilterRequest;
  private CategoryConfigurationFilterResponse categoryConfigurationFilterResponse;
  private MerchantConfigurationFilterResponse merchantConfigurationFilterResponse;
  private ConfigurationStatusResponse configurationStatusResponse;
  private ConfigurationStatusRequest configurationStatusRequest;
  private List<CategoryTreeResponse> categoryTreeResponses;
  private CategoryDetailAndShippingResponse categoryDetailAndShippingResponse;
  private GdnRestListResponse<PredefinedAllowedAttributeValueResponse>
      predefinedAllowedAttributeValueResponseGdnRestListResponse;

  @BeforeEach
  public void init() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(pcbOutboundService,  "catalogId", "categoryCode");
    ReflectionTestUtils.setField(pcbOutboundService,  "categoryHierarchyFetchBatchSize", 1);
    ReflectionTestUtils.setField(pcbOutboundService,  "maxProductSize", 1);

    merchantConfigurationRequest =
        MerchantConfigurationRequest.builder().businessPartnerCode(MERCHANT_CODE).businessPartnerName(MERCHANT_NAME)
            .reviewConfig(REVIEW_CONFIG).build();

    merchantConfigurationRequestList = MerchantConfigurationRequestList.builder()
        .merchantConfigurationRequestList(Arrays.asList(merchantConfigurationRequest)).build();

    categoryConfigurationRequest =
        CategoryConfigurationRequest.builder().categoryCode(CATEGORY_CODE).categoryName(CATEGORY_NAME)
            .reviewConfig(REVIEW_CONFIG).build();

    categoryConfigurationRequestList = CategoryConfigurationRequestList.builder()
        .categoryConfigurationRequestList(Arrays.asList(categoryConfigurationRequest)).build();

    bulkMerchantConfigUploadResponse =
        BulkMerchantConfigUploadResponse.builder().businessPartnerCode(MERCHANT_CODE).businessPartnerName(MERCHANT_NAME)
            .errorMessage(SUCCESS).build();

    bulkCategoryConfigUploadResponse =
        BulkCategoryConfigUploadResponse.builder().categoryName(CATEGORY_NAME).categoryCode(CATEGORY_CODE)
            .errorMessage(SUCCESS).build();

    bulkConfigDataResponse = BulkConfigDataResponse.builder().code(CODE).name(NAME).reviewConfig(REVIEW_CONFIG).build();

    attributeCodesRequest = new AttributeCodesRequest();
    attributeCodesRequest.setAttributeCodes(Arrays.asList(CODE));

    configurationFilterRequest =
        ConfigurationFilterRequest.builder().categoryCode(CODE).sortOrder(SORT_ORDER).searchKey(SEARCH_KEY)
            .reviewConfig(REVIEW_CONFIG).build();

    categoryConfigurationFilterResponse =
        CategoryConfigurationFilterResponse.builder().categoryCode(CODE).categoryName(NAME).reviewConfig(REVIEW_CONFIG)
            .build();

    merchantConfigurationFilterResponse =
        MerchantConfigurationFilterResponse.builder().merchantName(CODE).merchantCode(NAME).reviewConfig(REVIEW_CONFIG)
            .build();

    configurationStatusRequest =
        ConfigurationStatusRequest.builder().categoryCode(CATEGORY_CODE).businessPartnerCode(MERCHANT_CODE).build();

    configurationStatusResponse =
        ConfigurationStatusResponse.builder().reviewConfig(REVIEW_CONFIG).categoryCode(CATEGORY_CODE)
            .merchantCode(MERCHANT_CODE).build();

    categoryTreeResponses = new ArrayList<>();
    CategoryTreeResponse categoryTreeResponse = new CategoryTreeResponse();
    categoryTreeResponse.setCategoryCode(CATEGORY_CODE);
    categoryTreeResponses.add(categoryTreeResponse);

    categoryDetailAndShippingResponse = new CategoryDetailAndShippingResponse();
    categoryDetailAndShippingResponse.setCategoryCode(CATEGORY_CODE);

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
            new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponseGdnRestListResponse =
            new GdnRestListResponse<>(Arrays.asList(predefinedAllowedAttributeValueResponse), new PageMetaData(0, 10, 1),
                    REQUEST_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(pcbFeign);
  }

  @Test
  public void bulkMerchantConfigUploadTest() {
    Mockito.when(pcbFeign.bulkMerchantConfigUpload(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        merchantConfigurationRequestList)).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(bulkMerchantConfigUploadResponse), new PageMetaData(), REQUEST_ID));
    GdnRestListResponse<BulkMerchantConfigUploadResponse> response = pcbOutboundService
        .bulkMerchantConfigUpload(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            merchantConfigurationRequestList);
    Mockito.verify(pcbFeign).bulkMerchantConfigUpload(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        merchantConfigurationRequestList);
    Assertions.assertNotNull(response);
  }

  @Test
  public void bulkCategoryConfigUploadTest() {
    Mockito.when(pcbFeign.bulkCategoryConfigUpload(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        categoryConfigurationRequestList)).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(bulkCategoryConfigUploadResponse), new PageMetaData(), REQUEST_ID));
    GdnRestListResponse<BulkCategoryConfigUploadResponse> response = pcbOutboundService
        .bulkCategoryConfigUpload(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            categoryConfigurationRequestList);
    Mockito.verify(pcbFeign).bulkCategoryConfigUpload(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        categoryConfigurationRequestList);
    Assertions.assertNotNull(response);
  }

  @Test
  public void fetchConfigDetailsByCodesTest() {
    Mockito.when(pcbFeign.fetchConfigDetailsByCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, CONFIG_TYPE,
        attributeCodesRequest))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(bulkConfigDataResponse), new PageMetaData(), REQUEST_ID));
    GdnRestListResponse<BulkConfigDataResponse> response = pcbOutboundService
        .fetchConfigDetailsByCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, CONFIG_TYPE,
            attributeCodesRequest);
    Mockito.verify(pcbFeign)
        .fetchConfigDetailsByCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, CONFIG_TYPE,
            attributeCodesRequest);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getCategoryConfigurationListTest() {
    Mockito.when(pcbFeign
        .getCategoryConfigurationList(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
            configurationFilterRequest)).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(categoryConfigurationFilterResponse), new PageMetaData(), REQUEST_ID));
    GdnRestListResponse<CategoryConfigurationFilterResponse> response = pcbOutboundService
        .getCategoryConfigurationList(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, configurationFilterRequest,
            PAGE, SIZE);
    Mockito.verify(pcbFeign)
        .getCategoryConfigurationList(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
            configurationFilterRequest);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(CODE,response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(NAME,response.getContent().get(0).getCategoryName());
    Assertions.assertEquals(REVIEW_CONFIG,response.getContent().get(0).getReviewConfig());
  }

  @Test
  public void getMerchantConfigurationListTest() {
    Mockito.when(pcbFeign
        .getMerchantConfigurationList(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
            configurationFilterRequest)).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(merchantConfigurationFilterResponse), new PageMetaData(), REQUEST_ID));
    GdnRestListResponse<MerchantConfigurationFilterResponse> response = pcbOutboundService
        .getMerchantConfigurationList(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, configurationFilterRequest,
            PAGE, SIZE);
    Mockito.verify(pcbFeign)
        .getMerchantConfigurationList(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
            configurationFilterRequest);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getconfigurationstatusTest() {
    Mockito.when(pcbFeign.getconfigurationstatus(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        Arrays.asList(configurationStatusRequest))).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(configurationStatusResponse), new PageMetaData(), REQUEST_ID));
    GdnRestListResponse<ConfigurationStatusResponse> response = pcbOutboundService
        .getconfigurationstatus(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            Arrays.asList(configurationStatusRequest));
    Mockito.verify(pcbFeign).getconfigurationstatus(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        Arrays.asList(configurationStatusRequest));
    Assertions.assertNotNull(response);
  }

  @Test
  public void getBrandSuggestionsTest() throws Exception {
    Mockito.when(this.pcbFeign
        .getBrandSuggestions(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, VALUE, BUSINESS_PARTNER_CODE, true, true, 0, 0))
        .thenReturn(predefinedAllowedAttributeValueResponseGdnRestListResponse);
    setMdcParameters();
    List<PredefinedAllowedAttributeValueResponse> responses =
        this.pcbOutboundService.getBrandSuggestions(VALUE, BUSINESS_PARTNER_CODE, true, true);
    Mockito.verify(this.pcbFeign)
        .getBrandSuggestions(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, VALUE, BUSINESS_PARTNER_CODE, true, true, 0, 0);
    Assertions.assertNotNull(responses);
    Assertions.assertEquals(1, responses.size());
  }

  @Test
  public void getBrandSuggestionsExceptionTest() throws Exception {
    Mockito.when(this.pcbFeign
        .getBrandSuggestions(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, VALUE, BUSINESS_PARTNER_CODE, true, true, 0, 0))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, REQUEST_ID));
    setMdcParameters();
    List<PredefinedAllowedAttributeValueResponse> responses =
        this.pcbOutboundService.getBrandSuggestions(VALUE, BUSINESS_PARTNER_CODE, true, true);
    Mockito.verify(this.pcbFeign)
        .getBrandSuggestions(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, VALUE, BUSINESS_PARTNER_CODE, true, true, 0, 0);
    Assertions.assertNotNull(responses);
    Assertions.assertEquals(0, responses.size());
  }

  @Test
  public void getGenericTemplateCategoriesTest() {
    Mockito.when(pcbFeign
        .getGenericTemplateCategories(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, true, true))
        .thenReturn(new GdnRestListResponse<>(categoryTreeResponses, new PageMetaData(), REQUEST_ID));
    List<CategoryTreeResponse> response = pcbOutboundService.getGenericTemplateCategories(true,
      true);
    Mockito.verify(pcbFeign)
        .getGenericTemplateCategories(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, true, true);
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
  }

  @Test
  public void getCategoryDetailResponseTest() throws ApplicationException {
    setMdcParameters();
    Mockito.when(pcbFeign
        .getCategoryDetailByCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(new CategoryDetailResponse(), REQUEST_ID));
    CategoryDetailResponse response = pcbOutboundService.getCategoryDetailResponse(CATEGORY_CODE);
    Mockito.verify(pcbFeign)
        .getCategoryDetailByCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATEGORY_CODE);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getCategoryDetailResponseNullTest() {
    setMdcParameters();
    Mockito.when(pcbFeign
        .getCategoryDetailByCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATEGORY_CODE)).thenReturn(null);
    CategoryDetailResponse response = pcbOutboundService.getCategoryDetailResponse(CATEGORY_CODE);
    Mockito.verify(pcbFeign)
        .getCategoryDetailByCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATEGORY_CODE);
    Assertions.assertNull(response.getId());
  }

  @Test
  public void getBasicCategoryInfoAndCatalogInfoTest() throws ApplicationException {
    setMdcParameters();
    Mockito.when(pcbFeign
            .getBasicCategoryInfoAndCatalogInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
                Constant.USER_NAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(new CategoryResponse(), REQUEST_ID));
    CategoryResponse response = pcbOutboundService.getBasicCategoryInfoAndCatalogInfo(CATEGORY_CODE);
    Mockito.verify(pcbFeign)
        .getBasicCategoryInfoAndCatalogInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATEGORY_CODE);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getBasicCategoryInfoAndCatalogInfoFalseTest() throws ApplicationException {
    setMdcParameters();
    Mockito.when(pcbFeign
            .getBasicCategoryInfoAndCatalogInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
                Constant.USER_NAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null,null,false,new CategoryResponse(), REQUEST_ID));
    CategoryResponse response = pcbOutboundService.getBasicCategoryInfoAndCatalogInfo(CATEGORY_CODE);
    Mockito.verify(pcbFeign)
        .getBasicCategoryInfoAndCatalogInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATEGORY_CODE);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getBasicCategoryInfoAndCatalogInfoNullTest() throws ApplicationException {
    setMdcParameters();
    Mockito.when(pcbFeign
            .getBasicCategoryInfoAndCatalogInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
                Constant.USER_NAME, CATEGORY_CODE))
        .thenReturn(null);
    CategoryResponse response = pcbOutboundService.getBasicCategoryInfoAndCatalogInfo(CATEGORY_CODE);
    Mockito.verify(pcbFeign)
        .getBasicCategoryInfoAndCatalogInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATEGORY_CODE);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getAttributeDetailTest() {
    setMdcParameters();
    Mockito.when(pcbFeign
        .getAttributeDetail(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, ATTRIBUTE_ID, false))
        .thenReturn(new GdnRestSingleResponse<>(new AttributeResponse(), REQUEST_ID));
    AttributeResponse response = pcbOutboundService.getAttributeDetail(ATTRIBUTE_ID);
    Mockito.verify(pcbFeign)
        .getAttributeDetail(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, ATTRIBUTE_ID, false);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getAttributeDetailByIdTest() {
    setMdcParameters();
    Mockito.when(pcbFeign
            .getAttributeDetailById(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
                USERNAME, ATTRIBUTE_ID, VALUE))
        .thenReturn(new GdnRestSingleResponse<>(new AttributeResponse(), REQUEST_ID));
    AttributeResponse response = pcbOutboundService.getAttributeDetailById(STORE_ID,ATTRIBUTE_ID,VALUE);
    Mockito.verify(pcbFeign)
        .getAttributeDetailById(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
            USERNAME, ATTRIBUTE_ID, VALUE);
    Assertions.assertNotNull(response);
  }
  @Test
  public void getAttributeDetailNullTest() {
    setMdcParameters();
    Mockito.when(pcbFeign
        .getAttributeDetail(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, ATTRIBUTE_ID, false)).thenReturn(null);
    AttributeResponse response = pcbOutboundService.getAttributeDetail(ATTRIBUTE_ID);
    Mockito.verify(pcbFeign)
        .getAttributeDetail(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, ATTRIBUTE_ID, false);
    Assertions.assertNull(response.getId());
  }

  @Test
  public void getAttributeDetailByIdNullTest() {
    setMdcParameters();
    GdnRestSingleResponse response = new GdnRestSingleResponse<>(new AttributeResponse(), REQUEST_ID);
    response.setSuccess(false);

    Mockito.when(pcbFeign
            .getAttributeDetailById(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
                USERNAME, ATTRIBUTE_ID, VALUE))
        .thenReturn(response);

    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getAttributeDetailById(STORE_ID, ATTRIBUTE_ID, VALUE));
    }
    finally {
      Mockito.verify(pcbFeign)
          .getAttributeDetailById(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ATTRIBUTE_ID, VALUE);
    }
  }

  @Test
  public void getAttributeDetailByIdResponseNullTest() {
    setMdcParameters();
    Mockito.when(pcbFeign
            .getAttributeDetailById(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
                USERNAME, ATTRIBUTE_ID, VALUE))
        .thenReturn(null);

    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getAttributeDetailById(STORE_ID, ATTRIBUTE_ID, VALUE));
    }
    finally {
      Mockito.verify(pcbFeign)
          .getAttributeDetailById(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ATTRIBUTE_ID, VALUE);
    }
  }



  @Test
  public void getGenericTemplateCategoriesExceptionTest() {
    List<CategoryTreeResponse> response = null;
    try {
      setMdcParameters();
      Mockito.when(pcbFeign
          .getGenericTemplateCategories(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, true, true))
          .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(), REQUEST_ID));
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getGenericTemplateCategories(true, true));
    } catch (ApplicationRuntimeException e) {
      throw new ApplicationRuntimeException();
    } finally {
      Mockito.verify(pcbFeign)
          .getGenericTemplateCategories(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, true, true);
      Assertions.assertNull(null);
    }
  }

  @Test
  public void getCategoryInfoByCategoryIdTest() {
    setMdcParameters();
    Mockito.when(pcbFeign
        .getCategoryInfoByCategoryId(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailAndShippingResponse, REQUEST_ID));
    CategoryDetailAndShippingResponse response = pcbOutboundService.getCategoryInfoByCategoryId(CATEGORY_CODE);
    Mockito.verify(pcbFeign)
        .getCategoryInfoByCategoryId(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATEGORY_CODE);
    Assertions.assertEquals(CATEGORY_CODE, response.getCategoryCode());
  }

  @Test
  public void getCategoryInfoByCategoryIdExceptionTest() {
    setMdcParameters();
    Mockito.when(pcbFeign
        .getCategoryInfoByCategoryId(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATEGORY_CODE)).thenReturn(new GdnRestSingleResponse<>(null, REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getCategoryInfoByCategoryId(CATEGORY_CODE));
    } finally {
      {
        Mockito.verify(pcbFeign).getCategoryInfoByCategoryId(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, CATEGORY_CODE);
      }
    }
  }

  @Test
  public void getInReviewBrandsTest() {
    setMdcParameters();
    Mockito.when(pcbFeign.getInReviewBrands(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
        eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(0), eq(Integer.MAX_VALUE),
        any(BrandWipSummaryRequest.class)))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(), REQUEST_ID));
    List<BrandWipResponse> response = pcbOutboundService.getInReviewBrands(new BrandWipSummaryRequest());
    Mockito.verify(pcbFeign).getInReviewBrands(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
        eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(0), eq(Integer.MAX_VALUE),
        any(BrandWipSummaryRequest.class));
    Assertions.assertNotNull(response);
  }

  @Test
  public void getInReviewBrandsExceptionTest() {
    List<BrandWipResponse> response = null;
    try {
      setMdcParameters();
      Mockito.when(pcbFeign.getInReviewBrands(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
          eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(0), eq(Integer.MAX_VALUE),
          any(BrandWipSummaryRequest.class)))
          .thenReturn(new GdnRestListResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, REQUEST_ID));
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getInReviewBrands(new BrandWipSummaryRequest()));
    } catch (ApplicationRuntimeException e) {
      throw new ApplicationRuntimeException();
    } finally {
      Mockito.verify(pcbFeign).getInReviewBrands(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
          eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(0), eq(Integer.MAX_VALUE),
          any(BrandWipSummaryRequest.class));
      Assertions.assertNull(response);
    }
  }

  @Test
  public void getAttributeByNameStartingWithAndPageableTest() {
    setMdcParameters();
    Mockito.when(pcbFeign.getAttributeByNameStartingWithAndPageable(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(0), eq(Integer.MAX_VALUE),
        eq(Constant.ATTRIBUTE_NAME_BRAND)))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(), REQUEST_ID));
    List<AttributeResponse> response =
        pcbOutboundService.getAttributeByNameStartingWithAndPageable(Constant.ATTRIBUTE_NAME_BRAND);
    Mockito.verify(pcbFeign).getAttributeByNameStartingWithAndPageable(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(0), eq(Integer.MAX_VALUE),
        eq(Constant.ATTRIBUTE_NAME_BRAND));
    Assertions.assertNotNull(response);
  }

  @Test
  public void getAttributeByNameStartingWithAndPageableExceptionTest() {
    List<AttributeResponse> response = null;
    try {
      setMdcParameters();
      Mockito.when(pcbFeign.getAttributeByNameStartingWithAndPageable(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
          eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(0), eq(Integer.MAX_VALUE),
          eq(Constant.ATTRIBUTE_NAME_BRAND)))
          .thenReturn(new GdnRestListResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, REQUEST_ID));
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getAttributeByNameStartingWithAndPageable(
              Constant.ATTRIBUTE_NAME_BRAND));
    } catch (ApplicationRuntimeException e) {
      throw new ApplicationRuntimeException();
    } finally {
      Mockito.verify(pcbFeign).getAttributeByNameStartingWithAndPageable(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
          eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(0), eq(Integer.MAX_VALUE),
          eq(Constant.ATTRIBUTE_NAME_BRAND));
      Assertions.assertNull(response);
    }
  }

  @Test
  public void getCategoryTreeTest() throws Exception {
    CategoryTreeResponse categoryTreeResponse = new CategoryTreeResponse();
    GdnRestListResponse gdnRestListResponse =
        new GdnRestListResponse(Arrays.asList(categoryTreeResponse), new PageMetaData(0, 0, 0), REQUEST_ID);
    when(pcbFeign.getCategoryTree(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, StringUtils.EMPTY, Arrays.asList(CATEGORY_CODE))).thenReturn(gdnRestListResponse);
    CategoryTreeResponse response = pcbOutboundService.getCategoryTree(CATEGORY_CODE);
    verify(pcbFeign).getCategoryTree(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, StringUtils.EMPTY, Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void getCategoryTreeSuccessFalseTest() throws Exception {
    GdnRestListResponse gdnRestListResponse = new GdnRestListResponse();
    when(pcbFeign.getCategoryTree(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, StringUtils.EMPTY, Arrays.asList(CATEGORY_CODE))).thenReturn(gdnRestListResponse);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getCategoryTree(CATEGORY_CODE));
    } finally {
      verify(pcbFeign).getCategoryTree(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
          Constant.USER_NAME, StringUtils.EMPTY, Arrays.asList(CATEGORY_CODE));
    }
  }

  @Test
  public void getCategoryTreeNoContentTest() throws Exception {
    GdnRestListResponse gdnRestListResponse = new GdnRestListResponse();
    gdnRestListResponse.setSuccess(true);
    when(pcbFeign.getCategoryTree(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, StringUtils.EMPTY, Arrays.asList(CATEGORY_CODE))).thenReturn(gdnRestListResponse);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getCategoryTree(CATEGORY_CODE));
    } finally {
      verify(pcbFeign).getCategoryTree(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
          Constant.USER_NAME, StringUtils.EMPTY, Arrays.asList(CATEGORY_CODE));
    }
  }

  @Test
  public void batchVatFlagUpdateTest() throws Exception {
    GdnRestListResponse gdnRestListResponse = new GdnRestListResponse();
    gdnRestListResponse.setSuccess(true);
    when(pcbFeign
        .updateVatFlagBySkuCodes(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, new BatchVatUpdateRequest(BUSINESS_PARTNER_CODE, new HashMap<>())))
        .thenReturn(gdnRestListResponse);
    List<BatchVatUpdateResponse> responses =
        pcbOutboundService.batchVatFlagUpdate(Constant.USER_NAME, BUSINESS_PARTNER_CODE, new HashMap<>());
    verify(pcbFeign)
        .updateVatFlagBySkuCodes(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, new BatchVatUpdateRequest(BUSINESS_PARTNER_CODE, new HashMap<>()));
  }

  @Test
  public void batchVatFlagUpdateExceptionTest() throws Exception {
    GdnRestListResponse gdnRestListResponse = new GdnRestListResponse();
    gdnRestListResponse.setSuccess(false);
    when(pcbFeign.updateVatFlagBySkuCodes(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, new BatchVatUpdateRequest(BUSINESS_PARTNER_CODE, new HashMap<>()))).thenReturn(gdnRestListResponse);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.batchVatFlagUpdate(Constant.USER_NAME, BUSINESS_PARTNER_CODE,
              new HashMap<>()));
    } finally {
      verify(pcbFeign).updateVatFlagBySkuCodes(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
          Constant.USER_NAME, new BatchVatUpdateRequest(BUSINESS_PARTNER_CODE, new HashMap<>()));
    }
  }

  @Test
  public void getProductDetailByProductCodeTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(true);
    gdnRestSingleResponse.setValue(new ProductDetailResponse());
    Mockito.when(pcbFeign
        .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
            PRODUCT_CODE, false)).thenReturn(gdnRestSingleResponse);
    pcbOutboundService.getProductDetailByProductCode(PRODUCT_CODE, true, false);
    Mockito.verify(pcbFeign)
        .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
            PRODUCT_CODE, false);
  }

  @Test
  public void getProductDetailByProductCodeExceptionTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(pcbFeign
        .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
            PRODUCT_CODE, false)).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getProductDetailByProductCode(PRODUCT_CODE, true, false));
    } finally {
      Mockito.verify(pcbFeign)
          .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
              PRODUCT_CODE, false);
    }
  }

  @Test
  public void getProductDetailByProductCodeEmptyResponseExceptionTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(true);
    Mockito.when(pcbFeign
        .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
            PRODUCT_CODE, false)).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getProductDetailByProductCode(PRODUCT_CODE, true, false));
    } finally {
      Mockito.verify(pcbFeign)
          .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
              PRODUCT_CODE, false);
    }
  }

  @Test
  public void getProtectedBrandListTest() {
    ProtectedBrandResponse protectedBrandResponse = new ProtectedBrandResponse();
    protectedBrandResponse.setBrandName(BRAND_NAME);
    protectedBrandResponse.setBrandCode(BRAND_CODE);
    List<ProtectedBrandResponse> protectedBrandResponseList = new ArrayList<>();
    protectedBrandResponseList.add(protectedBrandResponse);
    GdnRestListResponse<ProtectedBrandResponse> response =
      new GdnRestListResponse<>(protectedBrandResponseList, null, REQUEST_ID);
    Mockito.when(pcbFeign.getProtectedBrandList(STORE_ID,
      MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER))).thenReturn(response);
    pcbOutboundService.getProtectedBrandList(STORE_ID);
    Mockito.verify(pcbFeign).getProtectedBrandList(STORE_ID,
      MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
  }

  @Test
  public void getProtectedBrandListExceptionTest() {
    setMdcParameters();
    GdnRestListResponse<ProtectedBrandResponse> response =
      new GdnRestListResponse<>(null, null,false,null, null,REQUEST_ID);
    Mockito.when(pcbFeign.getProtectedBrandList(STORE_ID,
      MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER))).thenReturn(response);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getProtectedBrandList(STORE_ID));
    }
    finally {
    Mockito.verify(pcbFeign).getProtectedBrandList(STORE_ID,
      MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
      MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
  }
  }

  @Test
  public void getBrandAuthorisationTest() {
    setMdcParameters();
    Mockito.when(pcbFeign
      .getBrandAuthorisation(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        BUSINESS_PARTNER_CODE, BRAND_CODE)).thenReturn(
      new GdnRestSingleResponse(null, null, true, new SimpleBooleanResponse(),
        Constants.DEFAULT_REQUEST_ID));
    pcbOutboundService
      .getBrandAuthorisation(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        BRAND_CODE, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbFeign)
      .getBrandAuthorisation(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        BUSINESS_PARTNER_CODE, BRAND_CODE);
  }

  @Test
  public void getBrandAuthorisationExceptionTest() {
    setMdcParameters();
    Mockito.when(pcbFeign
      .getBrandAuthorisation(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        BUSINESS_PARTNER_CODE, BRAND_CODE)).thenReturn(
      new GdnRestSingleResponse(null, null, false, null,
        Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService
        .getBrandAuthorisation(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          BRAND_CODE, BUSINESS_PARTNER_CODE));
    } finally {
      Mockito.verify(pcbFeign)
        .getBrandAuthorisation(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          BUSINESS_PARTNER_CODE, BRAND_CODE);
    }
  }

  private void setMdcParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        STORE_ID);
  }

  @Test
  public void deleteBrandAuthorisationTest() {
    setMdcParameters();
    Mockito.when(pcbFeign.deleteBrandAuthorisation(any(), any(), any(), any(), any(),
        eq(SELLER_CODE), eq(BRAND_CODE))).thenReturn(new GdnBaseRestResponse(null, null, true, null));
    pcbOutboundService.deleteBrandAuthorisation(STORE_ID, SELLER_CODE, BRAND_CODE);
    Mockito.verify(pcbFeign).deleteBrandAuthorisation(any(), any(), any(), any(), any(), eq(SELLER_CODE), eq(BRAND_CODE));
  }

  @Test
  public void deleteBrandAuthorisationSuccessFalseTest() {
    setMdcParameters();
    Mockito.when(pcbFeign.deleteBrandAuthorisation(any(), any(), any(), any(), any(),
        eq(SELLER_CODE), eq(BRAND_CODE))).thenReturn(new GdnBaseRestResponse(null, null, false, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.deleteBrandAuthorisation(STORE_ID, SELLER_CODE, BRAND_CODE));
    } finally {
      Mockito.verify(pcbFeign).deleteBrandAuthorisation(any(), any(), any(), any(), any(), eq(SELLER_CODE), eq(BRAND_CODE));
    }
  }

  @Test
  public void getAllChildCategoriesFromC1CategoryCodeTest(){
    setMdcParameters();
    CategoryCodeResponse categoryCodeResponse =
      new CategoryCodeResponse(Collections.singletonList(CATEGORY_CODE));
    GdnRestSingleResponse<CategoryCodeResponse> response = new GdnRestSingleResponse<>();
    response.setValue(categoryCodeResponse);
    response.setSuccess(true);
    Mockito.when(pcbFeign.getAllChildCategoriesFromC1CategoryCode(any(),any(),any(),any(),any(),
      anyBoolean(),any(CategoryCodeRequest.class))).thenReturn(response);
    List<String> categoryCode =
      pcbOutboundService.getAllChildCategoriesFromC1CategoryCode(REQUEST_ID,
        new CategoryCodeRequest(), true);
    verify(pcbFeign).getAllChildCategoriesFromC1CategoryCode(any(),any(),any(),any(),any(),
      anyBoolean(),any(CategoryCodeRequest.class));
    Assertions.assertFalse(Objects.isNull(categoryCode));
  }

  @Test
  public void getAllChildCategoriesFromC1CategoryCodExceptionTest(){
    setMdcParameters();
    CategoryCodeResponse categoryCodeResponse =
      new CategoryCodeResponse(Collections.singletonList(CATEGORY_CODE));
    GdnRestSingleResponse<CategoryCodeResponse> response = new GdnRestSingleResponse<>();
    response.setValue(null);
    response.setSuccess(false);
    Mockito.when(pcbFeign.getAllChildCategoriesFromC1CategoryCode(any(),any(),any(),any(),any(),
      anyBoolean(),any(CategoryCodeRequest.class))).thenReturn(response);
    List<String> categoryCode =
      pcbOutboundService.getAllChildCategoriesFromC1CategoryCode(REQUEST_ID,
        new CategoryCodeRequest(), true);
    verify(pcbFeign).getAllChildCategoriesFromC1CategoryCode(any(),any(),any(),any(),any(),
      anyBoolean(),any(CategoryCodeRequest.class));
    Assertions.assertEquals(categoryCode,Collections.emptyList());
  }

  @Test
  public void updateCategoriesWithRestrictedKeywordsTest(){
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList = new CategoryKeywordUpdateRequestList();
    Mockito.when(pcbFeign.updateCategoriesWithRestrictedKeywords(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
        CATEGORY_CODE, categoryKeywordUpdateRequestList)).thenReturn(response);

    String errorMessage = pcbOutboundService.updateCategoriesWithRestrictedKeywords(STORE_ID, CATEGORY_CODE, categoryKeywordUpdateRequestList);

    Mockito.verify(pcbFeign).updateCategoriesWithRestrictedKeywords(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
        CATEGORY_CODE, categoryKeywordUpdateRequestList);

    Assertions.assertTrue(StringUtils.isEmpty(errorMessage));
  }

  @Test
  public void updateCategoriesWithRestrictedKeywordsErrorTest(){
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(false);
    response.setErrorMessage(CATEGORY_KEYWORDS_UPDATE_ERROR_MESSAGE);
    CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList = new CategoryKeywordUpdateRequestList();
    Mockito.when(pcbFeign.updateCategoriesWithRestrictedKeywords(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
        CATEGORY_CODE, categoryKeywordUpdateRequestList)).thenReturn(response);

    String errorMessage = pcbOutboundService.updateCategoriesWithRestrictedKeywords(STORE_ID, CATEGORY_CODE, categoryKeywordUpdateRequestList);

    Mockito.verify(pcbFeign).updateCategoriesWithRestrictedKeywords(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
        CATEGORY_CODE, categoryKeywordUpdateRequestList);

    Assertions.assertEquals(CATEGORY_KEYWORDS_UPDATE_ERROR_MESSAGE, errorMessage);
  }

  @Test
  public void getChildFromParentByCatalogIdWithChildCountTest() {
    setMdcParameters();
    Mockito.when(
            pcbFeign.getChildFromParentByCatalogIdWithChildCount(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
                Constant.REQUEST_ID, USERNAME, 0, 1, CATEGORY_CODE, ALL, ALL))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(), REQUEST_ID));
    List<CategoryDTO> response =
        pcbOutboundService.getChildFromParentByCatalogIdWithChildCount(Constant.STORE_ID, 0, 1, ALL, ALL, USERNAME);
    Mockito.verify(pcbFeign)
        .getChildFromParentByCatalogIdWithChildCount(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, USERNAME, 0, 1, CATEGORY_CODE, ALL, ALL);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getChildFromParentByCatalogIdWithChildCountNullTest() {
    setMdcParameters();
    GdnRestListResponse response = new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(), REQUEST_ID);
    response.setSuccess(false);
    Mockito.when(
        pcbFeign.getChildFromParentByCatalogIdWithChildCount(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, USERNAME, 0, 1, CATEGORY_CODE, ALL, ALL)).thenReturn(response);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getChildFromParentByCatalogIdWithChildCount(Constant.STORE_ID, 0,
              1, ALL, ALL, USERNAME));
    } finally {
      Mockito.verify(pcbFeign)
          .getChildFromParentByCatalogIdWithChildCount(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, USERNAME, 0, 1, CATEGORY_CODE, ALL, ALL);
    }
  }

  @Test
  public void getChildFromParentByCatalogIdWithChildCount_NullTest() {
    setMdcParameters();
    Mockito.when(
        pcbFeign.getChildFromParentByCatalogIdWithChildCount(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, USERNAME, 0, 1, CATEGORY_CODE, ALL, ALL)).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getChildFromParentByCatalogIdWithChildCount(Constant.STORE_ID, 0,
              1, ALL, ALL, USERNAME));
    } finally {
      Mockito.verify(pcbFeign)
          .getChildFromParentByCatalogIdWithChildCount(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, USERNAME, 0, 1, CATEGORY_CODE, ALL, ALL);
    }
  }

  @Test
  public void validateDestinationCategoryTest(){
    GdnRestListResponse response = new GdnRestListResponse();
    response.setSuccess(true);
    CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList = new CategoryKeywordUpdateRequestList();
    Mockito.when(pcbFeign.validateCategoryForRestrictedKeywordCategoryChange(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
        new ArrayList<>())).thenReturn(response);

    pcbOutboundService.validateDestinationCategory(STORE_ID, new ArrayList<>());

    Mockito.verify(pcbFeign).validateCategoryForRestrictedKeywordCategoryChange(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, new ArrayList<>());
  }

  @Test
  public void validateDestinationCategoryErrorTest() {
    GdnRestListResponse response = new GdnRestListResponse();
    response.setSuccess(false);
    CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList = new CategoryKeywordUpdateRequestList();
    Mockito.when(
        pcbFeign.validateCategoryForRestrictedKeywordCategoryChange(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, new ArrayList<>())).thenReturn(response);

    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.validateDestinationCategory(STORE_ID, new ArrayList<>()));
    } finally {
      Mockito.verify(pcbFeign)
          .validateCategoryForRestrictedKeywordCategoryChange(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, new ArrayList<>());
    }
  }

  @Test
  public void createBulkBrandAuthorisationTest() {
    setMdcParameters();
    GdnRestSingleResponse<BrandAuthCreateResponse> response =
        new GdnRestSingleResponse(new String(), new String(), true, new BrandAuthCreateResponse(), new String());
    BrandAuthCreateRequest brandAuthCreateRequest = new BrandAuthCreateRequest();
    Mockito.when(pcbFeign.create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), any(BrandAuthCreateRequest.class))).thenReturn(response);
    pcbOutboundService.createBulkBrandAuthorisation(STORE_ID, brandAuthCreateRequest);
    Mockito.verify(pcbFeign)
        .create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            any(BrandAuthCreateRequest.class));
  }

  @Test
  public void createBulkBrandAuthorisationSuccessFalseTest() {
    setMdcParameters();
    GdnRestSingleResponse<BrandAuthCreateResponse> response =
        new GdnRestSingleResponse(new String(), new String(), false, new BrandAuthCreateResponse(), new String());
    BrandAuthCreateRequest brandAuthCreateRequest = new BrandAuthCreateRequest();
    Mockito.when(pcbFeign.create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), any(BrandAuthCreateRequest.class))).thenReturn(response);
    pcbOutboundService.createBulkBrandAuthorisation(STORE_ID, brandAuthCreateRequest);
    Mockito.verify(pcbFeign)
        .create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            any(BrandAuthCreateRequest.class));
  }

  @Test
  public void createBulkBrandAuthorisationExceptionTest() {
    setMdcParameters();
    try {
      BrandAuthCreateRequest brandAuthCreateRequest = new BrandAuthCreateRequest();
      Mockito.when(pcbFeign.create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), any(BrandAuthCreateRequest.class))).thenReturn(null);
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.createBulkBrandAuthorisation(STORE_ID, brandAuthCreateRequest));
    } finally {
      Mockito.verify(pcbFeign)
          .create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), any(BrandAuthCreateRequest.class));
    }
  }

  @Test
  public void deleteBulkBrandAuthorisationTest() {
    setMdcParameters();
    Mockito.when(pcbFeign.delete(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList())).thenReturn(new GdnBaseRestResponse(true));
    pcbOutboundService.deleteBulkBrandAuthorisation(STORE_ID, new ArrayList<>());
    Mockito.verify(pcbFeign)
        .delete(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyList());
  }

  @Test
  public void deleteBulkBrandAuthorisationFalseTest() {
    setMdcParameters();
    Mockito.when(pcbFeign.delete(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList())).thenReturn(new GdnBaseRestResponse(false));
    pcbOutboundService.deleteBulkBrandAuthorisation(STORE_ID, new ArrayList<>());
    Mockito.verify(pcbFeign)
        .delete(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyList());
  }

  @Test
  public void deleteBulkBrandAuthorisationExceptionTest() {
    setMdcParameters();
    try {
      Mockito.when(pcbFeign.delete(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), Mockito.anyList())).thenReturn(null);
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.deleteBulkBrandAuthorisation(STORE_ID, new ArrayList<>()));
    } finally {
      Mockito.verify(pcbFeign)
          .delete(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyList());
    }
  }

  @Test
  public void getAuthorisationsTest() {
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    Mockito.when(pcbFeign.getAuthorisations(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, 0, 1, brandAuthFilterRequest)).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 1, 1), REQUEST_ID));
    pcbOutboundService.getAuthorisations(STORE_ID, 0, 1, brandAuthFilterRequest);
    Mockito.verify(pcbFeign).getAuthorisations(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, 0, 1, brandAuthFilterRequest);

  }

  @Test
  public void getAuthorisationsErrorTest() {
    BrandAuthFilterRequest brandAuthFilterRequest = new BrandAuthFilterRequest();
    Mockito.when(pcbFeign.getAuthorisations(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, 0, 1, brandAuthFilterRequest)).thenReturn(
        new GdnRestListResponse<>(null, null, false, new ArrayList<>(), new PageMetaData(0, 1, 1), REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.getAuthorisations(STORE_ID, 0, 1, brandAuthFilterRequest));
    }
    finally {
      Mockito.verify(pcbFeign)
          .getAuthorisations(STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
              0, 1, brandAuthFilterRequest);
    }
  }


  @Test
  public void filterCategoryHierarchyByCategoryCodesTest() {
    setMdcParameters();
    List<String> categoryCodes = Arrays.asList(CATEGORY_CODE);
    List<CategoryHierarchyResponse> mockResponses =
        Arrays.asList(new CategoryHierarchyResponse(), new CategoryHierarchyResponse());
    GdnRestListResponse<CategoryHierarchyResponse> mockResponse =
        new GdnRestListResponse<>(null, null, true, mockResponses, new PageMetaData(), Constant.REQUEST_ID);
    Mockito.when(
        pcbFeign.filterCategoryHierarchyByCategoryCodes(Mockito.eq(Constant.STORE_ID), Mockito.eq(Constant.CHANNEL_ID),
            Mockito.eq(Constant.CLIENT_ID), Mockito.eq(Constant.REQUEST_ID), Mockito.eq(Constant.USER_NAME),
            Mockito.any(CategoryCodeRequest.class))).thenReturn(mockResponse);
    List<CategoryHierarchyResponse> result = pcbOutboundService.filterCategoryHierarchyByCategoryCodes(categoryCodes);
    Mockito.verify(pcbFeign)
        .filterCategoryHierarchyByCategoryCodes(Mockito.eq(Constant.STORE_ID), Mockito.eq(Constant.CHANNEL_ID),
            Mockito.eq(Constant.CLIENT_ID), Mockito.eq(Constant.REQUEST_ID), Mockito.eq(Constant.USER_NAME),
            Mockito.any(CategoryCodeRequest.class));
    Assertions.assertEquals(2, result.size());
  }

  @Test
  public void filterCategoryHierarchyByCategoryCodesCommunicationFailureTest() {
    setMdcParameters();
    List<String> categoryCodes = Arrays.asList("C1", "C2", "C3");
    GdnRestListResponse<CategoryHierarchyResponse> mockResponse =
        new GdnRestListResponse<>(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
            ErrorCategory.COMMUNICATION_FAILURE.getCode(), false, null, null, Constant.REQUEST_ID);
    Mockito.when(
        pcbFeign.filterCategoryHierarchyByCategoryCodes(Mockito.eq(Constant.STORE_ID), Mockito.eq(Constant.CHANNEL_ID),
            Mockito.eq(Constant.CLIENT_ID), Mockito.eq(Constant.REQUEST_ID), Mockito.eq(Constant.USER_NAME),
            Mockito.any(CategoryCodeRequest.class))).thenReturn(mockResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> pcbOutboundService.filterCategoryHierarchyByCategoryCodes(categoryCodes));
    Mockito.verify(pcbFeign)
        .filterCategoryHierarchyByCategoryCodes(Mockito.eq(Constant.STORE_ID), Mockito.eq(Constant.CHANNEL_ID),
            Mockito.eq(Constant.CLIENT_ID), Mockito.eq(Constant.REQUEST_ID), Mockito.eq(Constant.USER_NAME),
            Mockito.any(CategoryCodeRequest.class));
  }

  @Test
  public void filterCategoryHierarchyByCategoryCodesEmptyContentTest() {
    setMdcParameters();
    List<String> categoryCodes = Arrays.asList("C1", "C2", "C3");
    GdnRestListResponse<CategoryHierarchyResponse> mockResponse =
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(), Constant.REQUEST_ID);
    Mockito.when(
        pcbFeign.filterCategoryHierarchyByCategoryCodes(Mockito.eq(Constant.STORE_ID), Mockito.eq(Constant.CHANNEL_ID),
            Mockito.eq(Constant.CLIENT_ID), Mockito.eq(Constant.REQUEST_ID), Mockito.eq(Constant.USER_NAME),
            Mockito.any(CategoryCodeRequest.class))).thenReturn(mockResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> pcbOutboundService.filterCategoryHierarchyByCategoryCodes(categoryCodes));
    Mockito.verify(pcbFeign)
        .filterCategoryHierarchyByCategoryCodes(Mockito.eq(Constant.STORE_ID), Mockito.eq(Constant.CHANNEL_ID),
            Mockito.eq(Constant.CLIENT_ID), Mockito.eq(Constant.REQUEST_ID), Mockito.eq(Constant.USER_NAME),
            Mockito.any(CategoryCodeRequest.class));
  }

  @Test
  public void testGetBasicInfoFromPCB_Success() {
    List<ProductImageAndVideoResponse> content1 = Collections.singletonList(new ProductImageAndVideoResponse());
    List<ProductImageAndVideoResponse> content2 = Collections.singletonList(new ProductImageAndVideoResponse());
    GdnRestListResponse<ProductImageAndVideoResponse> resp1 = new GdnRestListResponse<>();
    resp1.setSuccess(true);
    resp1.setContent(content1);
    GdnRestListResponse<ProductImageAndVideoResponse> resp2 = new GdnRestListResponse<>();
    resp2.setSuccess(true);
    resp2.setContent(content2);
    Mockito.when(pcbFeign.getBasicInfoProductDetailsListByProductCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList())).thenReturn(resp1, resp2);
    List<ProductImageAndVideoResponse> result = pcbOutboundService.getBasicInfoFromPCB(Arrays.asList("SKU1", "SKU2"));
    Mockito.verify(pcbFeign, times(2))
        .getBasicInfoProductDetailsListByProductCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyList());
    Assertions.assertEquals(2, result.size());
  }

  @Test
  public void testGetBasicInfoFromPCB_ResponseNotSuccess() {
    GdnRestListResponse<ProductImageAndVideoResponse> resp = new GdnRestListResponse<>();
    resp.setSuccess(false);
    Mockito.when(pcbFeign.getBasicInfoProductDetailsListByProductCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList())).thenReturn(resp);
    ApplicationRuntimeException ex = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      pcbOutboundService.getBasicInfoFromPCB(Collections.singletonList("SKU1"));
    });
    Mockito.verify(pcbFeign).getBasicInfoProductDetailsListByProductCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void testGetBasicInfoFromPCB_ContentIsEmpty() {
    GdnRestListResponse<ProductImageAndVideoResponse> resp = new GdnRestListResponse<>();
    resp.setSuccess(true);
    resp.setContent(Collections.emptyList());
    Mockito.when(pcbFeign.getBasicInfoProductDetailsListByProductCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList())).thenReturn(resp);
    ApplicationRuntimeException ex = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      pcbOutboundService.getBasicInfoFromPCB(Collections.singletonList("SKU1"));
    });
    Mockito.verify(pcbFeign).getBasicInfoProductDetailsListByProductCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void testGetBasicInfoFromPCB_ExceptionThrown() {
    Mockito.when(pcbFeign.getBasicInfoProductDetailsListByProductCodes(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList()))
        .thenThrow(new RuntimeException("PCB error"));
    ApplicationRuntimeException ex = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      pcbOutboundService.getBasicInfoFromPCB(Collections.singletonList("SKU1"));
    });
    Mockito.verify(pcbFeign).getBasicInfoProductDetailsListByProductCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void getAttributeDetailByIdIgnoreCaseTest() {
    setMdcParameters();
    Mockito.when(
        pcbFeign.getAttributeDetailByIdIgnoreCase(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
          USERNAME, ATTRIBUTE_ID, VALUE, true))
      .thenReturn(new GdnRestSingleResponse<>(new AttributeResponse(), REQUEST_ID));
    AttributeResponse response =
      pcbOutboundService.getAttributeDetailByIdIgnoreCase(STORE_ID, ATTRIBUTE_ID, VALUE);
    Mockito.verify(pcbFeign)
      .getAttributeDetailByIdIgnoreCase(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        ATTRIBUTE_ID, VALUE, true);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getAttributeDetailIgnoreCaseByIdNullTest() {
    setMdcParameters();
    GdnRestSingleResponse<AttributeResponse> response =
      new GdnRestSingleResponse<>(new AttributeResponse(), REQUEST_ID);
    response.setSuccess(false);

    Mockito.when(
      pcbFeign.getAttributeDetailByIdIgnoreCase(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME, ATTRIBUTE_ID, VALUE, true)).thenReturn(response);

    try {
      Assertions.assertThrows(RuntimeException.class,
        () -> pcbOutboundService.getAttributeDetailByIdIgnoreCase(STORE_ID, ATTRIBUTE_ID, VALUE));
    } finally {
      Mockito.verify(pcbFeign)
        .getAttributeDetailByIdIgnoreCase(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
          ATTRIBUTE_ID, VALUE, true);
    }
  }

  @Test
  public void getAttributeDetailByIdResponseNullIgnoreCaseTest() {
    setMdcParameters();
    Mockito.when(
      pcbFeign.getAttributeDetailByIdIgnoreCase(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME, ATTRIBUTE_ID, VALUE, true)).thenReturn(null);

    try {
      Assertions.assertThrows(RuntimeException.class,
        () -> pcbOutboundService.getAttributeDetailByIdIgnoreCase(STORE_ID, ATTRIBUTE_ID, VALUE));
    } finally {
      Mockito.verify(pcbFeign)
        .getAttributeDetailByIdIgnoreCase(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
          ATTRIBUTE_ID, VALUE, true);
    }
  }

  @Test
  public void testEditItemUpcCode() {
    String productCode = "Product code";
    List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequests = new ArrayList<>();
    productItemUpcCodeUpdateRequests.add(
        ProductItemUpcCodeUpdateRequest.builder().upcCode("upc").skuCode("sku").build());
    Mockito.when(
        pcbFeign.editItemUpcCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyList())).thenReturn(new GdnBaseRestResponse(true));
    pcbOutboundService.editItemUpcCode(productCode, productItemUpcCodeUpdateRequests);
    Mockito.verify(pcbFeign)
        .editItemUpcCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void testEditItemUpcCodeErrorTest() {
    String productCode = "Product code";
    List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequests = new ArrayList<>();
    productItemUpcCodeUpdateRequests.add(
        ProductItemUpcCodeUpdateRequest.builder().upcCode("upc").skuCode("sku").build());
    Mockito.when(
        pcbFeign.editItemUpcCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyList())).thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pcbOutboundService.editItemUpcCode(productCode, productItemUpcCodeUpdateRequests));
    } finally {
      Mockito.verify(pcbFeign)
          .editItemUpcCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyList());
    }
  }

  @Test
  public void testGetProductItemBySkuCodes() {
    SkuCodesRequest request = new SkuCodesRequest();
    request.setSkuCodes(Collections.singletonList("Sku"));
    ProductItemResponse response = new ProductItemResponse();
    response.setSkuCode("sku");
    Mockito.when(pcbFeign.getProductItemBySkuCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), any()))
        .thenReturn(new GdnRestListResponse<>(Collections.singletonList(response), null, null));
    pcbOutboundService.getProductItemBySkuCodes(request);
    Mockito.verify(pcbFeign)
        .getProductItemBySkuCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), any());
  }

  @Test
  public void testGetProductItemBySkuCodesErrorTest() {
    SkuCodesRequest request = new SkuCodesRequest();
    request.setSkuCodes(Collections.singletonList("Sku"));
    Mockito.when(pcbFeign.getProductItemBySkuCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(new GdnRestListResponse<>(null, null, false, "123"));
    try {
      Assertions.assertThrows(RuntimeException.class, () -> pcbOutboundService.getProductItemBySkuCodes(request));
    } finally {
      Mockito.verify(pcbFeign)
          .getProductItemBySkuCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), any());
    }
  }
}
