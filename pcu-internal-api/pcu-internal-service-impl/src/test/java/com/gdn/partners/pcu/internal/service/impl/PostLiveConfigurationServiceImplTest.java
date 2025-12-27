package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
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
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.domain.Page;
import org.springframework.mock.web.MockMultipartFile;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.client.feign.XBPFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.internal.streaming.model.bulk.CategoryConfigurationDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.MerchantConfigurationDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ConfigurationsStatusWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantWebSearchResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantSearchResponse;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.ConfigurationCountResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationHistoryResponse;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class PostLiveConfigurationServiceImplTest {

  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BL-00001";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "BliBli";
  private static final int PAGE = 0;
  private static final int SIZE = 30;
  private static final int TOTAL_RECORD = 1;
  private static final String STATUS = "ACTIVE";
  private static final String KEYWORD = "Bli";
  private static final String FLAG = "MERCHANT";
  private static final String SORT_DIRECTION = "ASC";
  private static final String STORE_ID = "storeId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String NEUTRAL_STATUS = "NEUTRAL";
  private static final String SEARCH_KEYWORD = "searchKey";
  private static final String CATEGORY_CODE = "CAT-01";
  private static final String CATEGORY_NAME = "CAT-NAME";
  private static final String SORT_ORDER = "desc";
  private static final String CREATED_BY = "created-by";
  private static final String REVIEW_CONFIG_FLAG = "Pre-live";
  private static final String PATH = "path";
  private static final String FILE = "/filestore/originalFilename.xls";
  private static final Long MERCHANT_CONFIGURATION_COUNT = Long.valueOf(10);
  private static final Long CATEGORY_CONFIGURATION_COUNT = Long.valueOf(20);
  private static final String ACTIVITY = "Registered";
  private static final String UPDATED_BY = "updatedBy";
  private static final String TYPE = "Type";
  private static final String CATEGORY = "category";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";

  private BusinessPartnerFilterRequest businessPartnerFilterRequest = new BusinessPartnerFilterRequest();
  private ProfileResponse profileResponse = new ProfileResponse();
  private GdnRestListResponse<ProfileResponse> profileResponseGdnRestListResponse;
  private MerchantConfigurationRequest merchantConfigurationRequest;
  private MerchantSearchResponse merchantSearchResponse;
  private GdnRestListResponse<MerchantSearchResponse> merchantSearchResponseGdnRestListResponse;
  private CategoryConfigurationRequest categoryConfigurationRequest;
  private List<CategoryConfigurationRequest> categoryConfigurationRequestList;
  private List<MerchantConfigurationRequest> merchantConfigurationRequestList;
  private GdnBaseRestResponse gdnBaseRestResponse;
  private Map<String, String> mainCategory;
  private GdnRestSingleResponse<ProfileResponse> gdnRestSingleResponse;
  private MerchantConfigurationRequest merchantConfigurationRequest1;
  private ConfigurationStatusRequest configurationStatusRequest;
  private GdnRestListResponse<ConfigurationStatusResponse> configurationStatusResponseGdnRestListResponse;
  private ConfigurationStatusResponse configurationStatusResponse;
  private ConfigurationWebRequest configurationWebRequest;
  private ConfigurationCountResponse configurationCountResponse =
      new ConfigurationCountResponse(CATEGORY_CONFIGURATION_COUNT, MERCHANT_CONFIGURATION_COUNT);
  private GdnRestSingleResponse<ConfigurationCountResponse> countResponseGdnRestListResponse;
  private ConfigurationFilterRequest configurationFilterRequest = new ConfigurationFilterRequest();
  private ConfigurationFilterWebRequest configurationFilterWebRequest = new ConfigurationFilterWebRequest();
  private CategoryConfigurationFilterResponse categoryConfigurationFilterResponse = new CategoryConfigurationFilterResponse();
  private MerchantConfigurationFilterResponse merchantConfigurationFilterResponse =
      new MerchantConfigurationFilterResponse();
  private CategoryConfigurationHistoryResponse categoryConfigurationHistoryResponse =
      new CategoryConfigurationHistoryResponse();
  private GdnRestListResponse<CategoryConfigurationHistoryResponse>
      categoryConfigurationHistoryResponseGdnRestListResponse;
  private byte[] fileContent;
  private MockMultipartFile multipartFile;
  private MerchantConfigurationHistoryResponse merchantConfigurationHistoryResponse =
      new MerchantConfigurationHistoryResponse();
  private GdnRestListResponse<MerchantConfigurationHistoryResponse>
      merchantConfigurationHistoryResponseGdnRestListResponse;

  @Mock
  private XBPFeign xbpFeign;

  @Mock
  private PCBFeign pcbFeign;

  @InjectMocks
  private PostLiveConfigurationServiceImpl postLiveConfigurationServiceImpl;

  @Mock
  private SystemParameterProperties systemParameterProperties;

  @Mock
  private FileStorageService fileStorageService;

  @Captor
  private ArgumentCaptor<BulkConfigurationUpdateRequest> bulkConfigurationUpdateRequestArgumentCaptor;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Captor
  private ArgumentCaptor<BulkDownloadRequest> bulkDownloadRequestArgumentCaptor;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    businessPartnerFilterRequest = BusinessPartnerFilterRequest.builder().status(STATUS).category(null)
        .businessPartnerCodes(null).keywords(KEYWORD).flag(FLAG).merchantTypes(null).sortDirection(SORT_DIRECTION)
        .sortedBy(StringUtils.EMPTY).tags(null).type(StringUtils.EMPTY).build();
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    profileResponse.getCompany().setTypeOfBusiness(CATEGORY_CODE);
    profileResponseGdnRestListResponse =
        new GdnRestListResponse<>(null, null, true, Arrays.asList(profileResponse), new PageMetaData(PAGE, SIZE, TOTAL_RECORD),
            REQUEST_ID);
    merchantConfigurationRequest =
        new MerchantConfigurationRequest(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_NAME, null);
    merchantSearchResponse =
        new MerchantSearchResponse(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_NAME, NEUTRAL_STATUS);
    merchantSearchResponseGdnRestListResponse =
        new GdnRestListResponse<>(null, null, true, Arrays.asList(merchantSearchResponse), new PageMetaData(PAGE, SIZE, TOTAL_RECORD),
            REQUEST_ID);
    categoryConfigurationRequest =
        CategoryConfigurationRequest.builder().categoryCode(CATEGORY_CODE).reviewConfig(REVIEW_CONFIG_FLAG).build();
    gdnBaseRestResponse = new GdnBaseRestResponse(true);

    mainCategory = new HashMap<>();
    mainCategory.put(DEFAULT_BUSINESS_PARTNER_CODE, CATEGORY_CODE);

    gdnRestSingleResponse = new GdnRestSingleResponse<>(null, null, true, profileResponse, REQUEST_ID);
    merchantConfigurationRequest1 =
        new MerchantConfigurationRequest(DEFAULT_BUSINESS_PARTNER_CODE, null, REVIEW_CONFIG_FLAG);
    categoryConfigurationRequestList = new ArrayList<>();
    merchantConfigurationRequestList = new ArrayList<>();
    configurationStatusRequest = ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
        .categoryCode(CATEGORY_CODE).build();
    configurationStatusResponse =
        ConfigurationStatusResponse.builder().merchantCode(DEFAULT_BUSINESS_PARTNER_CODE).categoryCode(CATEGORY_CODE)
            .reviewConfig(NEUTRAL_STATUS).build();
    configurationStatusResponseGdnRestListResponse =
        new GdnRestListResponse<>(null, null, true, Arrays.asList(configurationStatusResponse),
            new PageMetaData(PAGE, SIZE, TOTAL_RECORD), REQUEST_ID);
    configurationWebRequest =
        ConfigurationWebRequest.builder().merchantCode(DEFAULT_BUSINESS_PARTNER_CODE).categoryCode(CATEGORY_CODE)
            .build();
    countResponseGdnRestListResponse =
        new GdnRestSingleResponse<>(null, null, true, configurationCountResponse, REQUEST_ID);
    configurationFilterRequest.setSearchKey(SEARCH_KEYWORD);
    configurationFilterRequest.setCategoryCode(CATEGORY_CODE);
    configurationFilterRequest.setCategoryCode(Constants.PRE_LIVE_STATUS);
    configurationFilterRequest.setSortOrder(SORT_ORDER);
    configurationFilterWebRequest.setSearchKey(SEARCH_KEYWORD);
    configurationFilterWebRequest.setCategoryCode(CATEGORY_CODE);
    configurationFilterWebRequest.setCategoryCode(Constants.PRE_LIVE_STATUS);
    configurationFilterWebRequest.setSortOrder(SORT_ORDER);
    categoryConfigurationFilterResponse.setCategoryCode(CATEGORY_CODE);
    categoryConfigurationFilterResponse.setCategoryName(CATEGORY_NAME);
    categoryConfigurationFilterResponse.setReviewConfig(Constants.PRE_LIVE_STATUS);
    categoryConfigurationFilterResponse.setCreatedBy(CREATED_BY);
    categoryConfigurationFilterResponse.setCreatedDate(new Date());

    merchantConfigurationFilterResponse.setCreatedDate(new Date());
    merchantConfigurationFilterResponse.setReviewConfig(Constants.PRE_LIVE_STATUS);
    merchantConfigurationFilterResponse.setMerchantName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfigurationFilterResponse.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationFilterResponse.setCategoryName(CATEGORY_NAME);
    merchantConfigurationFilterResponse.setCreatedBy(CREATED_BY);

    categoryConfigurationHistoryResponse.setUpdatedDate(new Date());
    categoryConfigurationHistoryResponse.setCreatedDate(new Date());
    categoryConfigurationHistoryResponse.setCreatedBy(CREATED_BY);
    categoryConfigurationHistoryResponse.setUpdatedBy(UPDATED_BY);
    categoryConfigurationHistoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryConfigurationHistoryResponse.setCategoryName(CATEGORY_NAME);
    categoryConfigurationHistoryResponse.setOldValue(Constants.PRE_LIVE_STATUS);
    categoryConfigurationHistoryResponse.setNewValue(Constants.POST_LIVE);
    categoryConfigurationHistoryResponse.setActivity(ACTIVITY);

    categoryConfigurationHistoryResponseGdnRestListResponse =
        new GdnRestListResponse<>(null, null, true, Arrays.asList(categoryConfigurationHistoryResponse),
            new PageMetaData(PAGE, SIZE, TOTAL_RECORD), REQUEST_ID);
    configurationStatusRequest = ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
        .categoryCode(CATEGORY_CODE).build();
    configurationStatusResponse =
        ConfigurationStatusResponse.builder().merchantCode(DEFAULT_BUSINESS_PARTNER_CODE).categoryCode(CATEGORY_CODE)
            .reviewConfig(NEUTRAL_STATUS).build();
    configurationStatusResponseGdnRestListResponse =
        new GdnRestListResponse<>(null, null, true, Arrays.asList(configurationStatusResponse),
            new PageMetaData(PAGE, SIZE, TOTAL_RECORD), REQUEST_ID);
    configurationWebRequest =
        ConfigurationWebRequest.builder().merchantCode(DEFAULT_BUSINESS_PARTNER_CODE).categoryCode(CATEGORY_CODE)
            .build();
    countResponseGdnRestListResponse =
        new GdnRestSingleResponse<>(null, null, true, configurationCountResponse, REQUEST_ID);
    configurationFilterRequest.setSearchKey(SEARCH_KEYWORD);
    configurationFilterRequest.setCategoryCode(CATEGORY_CODE);
    configurationFilterRequest.setReviewConfig(Constants.PRE_LIVE_STATUS);
    configurationFilterRequest.setSortOrder(SORT_ORDER);
    configurationFilterWebRequest.setSearchKey(SEARCH_KEYWORD);
    configurationFilterWebRequest.setCategoryCode(CATEGORY_CODE);
    configurationFilterWebRequest.setReviewConfig(Constants.PRE_LIVE_STATUS);
    configurationFilterWebRequest.setSortOrder(SORT_ORDER);
    categoryConfigurationFilterResponse.setCategoryCode(CATEGORY_CODE);
    categoryConfigurationFilterResponse.setCategoryName(CATEGORY_NAME);
    categoryConfigurationFilterResponse.setReviewConfig(Constants.PRE_LIVE_STATUS);
    categoryConfigurationFilterResponse.setCreatedBy(CREATED_BY);
    categoryConfigurationFilterResponse.setCreatedDate(new Date());

    merchantConfigurationFilterResponse.setCreatedDate(new Date());
    merchantConfigurationFilterResponse.setReviewConfig(Constants.PRE_LIVE_STATUS);
    merchantConfigurationFilterResponse.setMerchantName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfigurationFilterResponse.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationFilterResponse.setCategoryName(CATEGORY_NAME);
    merchantConfigurationFilterResponse.setCreatedBy(CREATED_BY);

    fileContent = new byte[] {-1, -40, -20, -10};
    merchantConfigurationHistoryResponse.setOldValue(NEUTRAL_STATUS);
    merchantConfigurationHistoryResponse.setNewValue(NEUTRAL_STATUS);
    merchantConfigurationHistoryResponse.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationHistoryResponse.setMerchantName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfigurationHistoryResponseGdnRestListResponse =
        new GdnRestListResponse<>(null, null, true, Arrays.asList(merchantConfigurationHistoryResponse),
            new PageMetaData(PAGE, SIZE, TOTAL_RECORD), REQUEST_ID);
  }

  @AfterEach
  public void teardown() throws IOException {
    Mockito.verifyNoMoreInteractions(this.pcbFeign);
    Mockito.verifyNoMoreInteractions(this.xbpFeign);
    verifyNoMoreInteractions(systemParameterProperties);
    verifyNoMoreInteractions(this.kafkaProducer);
    FileUtils.deleteDirectory(new File(PATH));
  }

  @Test
  public void getMerchantsBySearchKeywordTest() {
    Mockito.when(this.xbpFeign.getAllActiveMerchantList(businessPartnerFilterRequest, PAGE, SIZE))
        .thenReturn(profileResponseGdnRestListResponse);
    Mockito.when(this.pcbFeign.fetchMerchantSearchResult(Arrays.asList(merchantConfigurationRequest)))
        .thenReturn(merchantSearchResponseGdnRestListResponse);
    Page<MerchantWebSearchResponse> merchantWebSearchResponsePage =
        this.postLiveConfigurationServiceImpl.getMerchantsBySearchKeyword(KEYWORD, PAGE, SIZE);
    Mockito.verify(this.xbpFeign).getAllActiveMerchantList(businessPartnerFilterRequest, PAGE, SIZE);
    Mockito.verify(this.pcbFeign).fetchMerchantSearchResult(Arrays.asList(merchantConfigurationRequest));
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        merchantWebSearchResponsePage.getContent().get(0).getMerchantCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        merchantWebSearchResponsePage.getContent().get(0).getMerchantName());
    Assertions.assertEquals(NEUTRAL_STATUS,
        merchantWebSearchResponsePage.getContent().get(0).getReviewConfig());
  }

  @Test
  public void addCategoryConfigurationTest() {
    Mockito.when(this.pcbFeign.addCategoryConfigurationStatus(Arrays.asList(categoryConfigurationRequest)))
        .thenReturn(gdnBaseRestResponse);
    this.postLiveConfigurationServiceImpl.addCategoryConfiguration(Arrays.asList(categoryConfigurationRequest));
    Mockito.verify(this.pcbFeign).addCategoryConfigurationStatus(Arrays.asList(categoryConfigurationRequest));
  }

  @Test
  public void addCategoryConfigurationTest_Exception() {
    try {
      this.postLiveConfigurationServiceImpl.addCategoryConfiguration(
          categoryConfigurationRequestList);
    }
    catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void updateCategoryConfigurationTest() {
    Mockito.when(this.pcbFeign.updateCategoryConfigurationStatus(categoryConfigurationRequest))
        .thenReturn(gdnBaseRestResponse);
    this.postLiveConfigurationServiceImpl.updateCategoryConfiguration(CATEGORY_CODE, REVIEW_CONFIG_FLAG);
    Mockito.verify(this.pcbFeign).updateCategoryConfigurationStatus(categoryConfigurationRequest);
  }

  @Test
  public void updateCategoryConfigurationTest_Exception() {
    try {
      this.postLiveConfigurationServiceImpl.updateCategoryConfiguration(StringUtils.EMPTY,
          REVIEW_CONFIG_FLAG);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void updateCategoryConfigurationTestWithReviewFlagTest_Exception() {
    try {
      this.postLiveConfigurationServiceImpl.updateCategoryConfiguration(CATEGORY_CODE,
          StringUtils.EMPTY);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void deleteCategoryConfigurationTest() {
    Mockito.when(this.pcbFeign.deleteCategoryConfigurationStatus(CATEGORY_CODE)).thenReturn(gdnBaseRestResponse);
    this.postLiveConfigurationServiceImpl.deleteCategoryConfiguration(CATEGORY_CODE);
    Mockito.verify(this.pcbFeign).deleteCategoryConfigurationStatus(CATEGORY_CODE);
  }

  @Test
  public void deleteCategoryConfiguration_Exception() {
    try {
      this.postLiveConfigurationServiceImpl.deleteCategoryConfiguration(StringUtils.EMPTY);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void addMerchantConfigurationTest() {
    Mockito.when(this.pcbFeign
        .addMerchantConfigurationStatus(Collections.singletonList(merchantConfigurationRequest1)))
        .thenReturn(gdnBaseRestResponse);
    this.postLiveConfigurationServiceImpl
        .addMerchantConfiguration(Collections.singletonList(merchantConfigurationRequest1));
    Mockito.verify(this.pcbFeign)
        .addMerchantConfigurationStatus(Collections.singletonList(merchantConfigurationRequest1));
  }

  @Test
  public void addMerchantConfigurationTest_Exception() {
    try {
      this.postLiveConfigurationServiceImpl.addMerchantConfiguration(
          merchantConfigurationRequestList);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void updateMerchantConfigurationTest() {
    Mockito.when(this.pcbFeign.updateMerchantConfigurationStatus(merchantConfigurationRequest1))
        .thenReturn(gdnBaseRestResponse);
    this.postLiveConfigurationServiceImpl
        .updateMerchantConfiguration(DEFAULT_BUSINESS_PARTNER_CODE, REVIEW_CONFIG_FLAG);
    Mockito.verify(this.pcbFeign).updateMerchantConfigurationStatus(merchantConfigurationRequest1);
  }

  @Test
  public void updateMerchantConfigurationTest_Exception() {
    try {
      this.postLiveConfigurationServiceImpl.updateMerchantConfiguration(StringUtils.EMPTY,
          REVIEW_CONFIG_FLAG);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void updateMerchantConfigurationWithReviewFlagTest_Exception() {
    try {
      this.postLiveConfigurationServiceImpl.updateMerchantConfiguration(
          DEFAULT_BUSINESS_PARTNER_CODE, StringUtils.EMPTY);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void deleteMerchantConfigurationTest() {
    Mockito.when(this.pcbFeign.deleteMerchantConfigurationStatus(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(gdnBaseRestResponse);
    this.postLiveConfigurationServiceImpl.deleteMerchantConfiguration(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.pcbFeign).deleteMerchantConfigurationStatus(DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void deleteMerchantConfigurationTest_Exception() {
    try {
      this.postLiveConfigurationServiceImpl.deleteMerchantConfiguration(StringUtils.EMPTY);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void getConfigurationsStatusTest() throws Exception {
    Mockito
        .when(this.pcbFeign.getConfigurationsStatusByMerchantAndCategoryCode(Arrays.asList(configurationStatusRequest)))
        .thenReturn(configurationStatusResponseGdnRestListResponse);
    List<ConfigurationsStatusWebResponse> configurationsStatusWebResponseList =
        this.postLiveConfigurationServiceImpl.getConfigurationsStatus(Arrays.asList(configurationWebRequest));
    Mockito.verify(this.pcbFeign)
        .getConfigurationsStatusByMerchantAndCategoryCode(Arrays.asList(configurationStatusRequest));
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, configurationsStatusWebResponseList.get(0).getMerchantCode());
    Assertions.assertEquals(CATEGORY_CODE, configurationsStatusWebResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(NEUTRAL_STATUS, configurationsStatusWebResponseList.get(0).getReviewConfig());

  }

  @Test
  public void getConfigurationCountsTest() {
    Mockito.when(this.pcbFeign.fetchConfigurationCounts()).thenReturn(countResponseGdnRestListResponse);
    ConfigurationCountResponse configurationCountResponse = this.postLiveConfigurationServiceImpl.getConfigurationCounts();
    Mockito.verify(this.pcbFeign).fetchConfigurationCounts();
    Assertions.assertEquals(CATEGORY_CONFIGURATION_COUNT, configurationCountResponse.getCategoryConfigurationCount());
    Assertions.assertEquals(MERCHANT_CONFIGURATION_COUNT, configurationCountResponse.getMerchantConfigurationCount());
  }

  @Test
  public void getCategoryConfigurationListTest() {
    Mockito.when(this.pcbFeign.getCategoryConfigurationList(configurationFilterRequest, PAGE, SIZE)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(categoryConfigurationFilterResponse),
            new PageMetaData(PAGE, SIZE, TOTAL_RECORD), REQUEST_ID));
    Page<CategoryConfigurationFilterWebResponse> categoryConfigurationFilterResponsePage =
        this.postLiveConfigurationServiceImpl
            .filterCategoryConfiguration(configurationFilterWebRequest, PAGE, SIZE);
    Mockito.verify(this.pcbFeign).getCategoryConfigurationList(configurationFilterRequest, PAGE, SIZE);
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationFilterResponsePage.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, categoryConfigurationFilterResponsePage.getContent().get(0).getCategoryName());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS, categoryConfigurationFilterResponsePage.getContent().get(0).getReviewConfig());
    Assertions.assertEquals(CREATED_BY, categoryConfigurationFilterResponsePage.getContent().get(0).getCreatedBy());
    Assertions.assertNotNull(categoryConfigurationFilterResponsePage.getContent().get(0).getCreatedDate());
  }

  @Test
  public void getMerchantConfigurationListTest() {
    Mockito.when(this.pcbFeign.getMerchantConfigurationList(configurationFilterRequest, PAGE, SIZE)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(merchantConfigurationFilterResponse),
            new PageMetaData(PAGE, SIZE, TOTAL_RECORD), REQUEST_ID));
    Page<MerchantConfigurationFilterWebResponse> merchantConfigurationFilterWebResponsePage =
        this.postLiveConfigurationServiceImpl
            .filterMerchantConfiguration(configurationFilterWebRequest, PAGE, SIZE);
    Mockito.verify(this.pcbFeign).getMerchantConfigurationList(configurationFilterRequest, PAGE, SIZE);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        merchantConfigurationFilterWebResponsePage.getContent().get(0).getMerchantName());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, merchantConfigurationFilterWebResponsePage.getContent().get(0).getMerchantCode());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS, merchantConfigurationFilterWebResponsePage.getContent().get(0).getReviewConfig());
    Assertions.assertEquals(CREATED_BY, merchantConfigurationFilterWebResponsePage.getContent().get(0).getCreatedBy());
    Assertions.assertNotNull(merchantConfigurationFilterWebResponsePage.getContent().get(0).getCreatedDate());
    Assertions.assertEquals(CATEGORY_NAME, merchantConfigurationFilterWebResponsePage.getContent().get(0).getCategoryName());
  }

  @Test
  public void getCategoryConfigurationHistoryTest() {
    Mockito.when(this.pcbFeign.getCategoryConfigurationHistory(CATEGORY_CODE, PAGE, SIZE))
        .thenReturn(categoryConfigurationHistoryResponseGdnRestListResponse);
    Page<CategoryConfigurationHistoryWebResponse> categoryConfigurationHistoryWebResponsePage =
        this.postLiveConfigurationServiceImpl.getCategoryConfigurationHistory(CATEGORY_CODE, PAGE, SIZE);
    Mockito.verify(this.pcbFeign).getCategoryConfigurationHistory(CATEGORY_CODE, PAGE, SIZE);
    Assertions.assertNotNull(categoryConfigurationHistoryWebResponsePage.getContent().get(0).getCreatedDate());
    Assertions.assertNotNull(categoryConfigurationHistoryWebResponsePage.getContent().get(0).getUpdatedDate());
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationHistoryWebResponsePage.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, categoryConfigurationHistoryWebResponsePage.getContent().get(0).getCategoryName());
    Assertions.assertEquals(UPDATED_BY, categoryConfigurationHistoryWebResponsePage.getContent().get(0).getUpdatedBy());
    Assertions.assertEquals(CREATED_BY, categoryConfigurationHistoryWebResponsePage.getContent().get(0).getCreatedBy());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS, categoryConfigurationHistoryWebResponsePage.getContent().get(0).getOldValue());
    Assertions.assertEquals(Constants.POST_LIVE, categoryConfigurationHistoryWebResponsePage.getContent().get(0).getNewValue());
    Assertions.assertEquals(ACTIVITY, categoryConfigurationHistoryWebResponsePage.getContent().get(0).getActivity());
  }

  @Test
  public void uploadBulkConfigurationTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.when(fileStorageService.uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(PATH);
    postLiveConfigurationServiceImpl.uploadBulkConfiguration(multipartFile, TYPE, REQUEST_ID, STORE_ID, USERNAME);
    Mockito.verify(fileStorageService).uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(kafkaProducer).send(eq(DomainEventName.BULK_CONFIGURATION_UPDATE), eq(USERNAME),
        bulkConfigurationUpdateRequestArgumentCaptor.capture());
    Assertions.assertNotNull(new File(PATH + REQUEST_ID + ORIGINAL_FILENAME));
    Assertions.assertNotNull(bulkConfigurationUpdateRequestArgumentCaptor.getValue());
    Assertions.assertEquals(TYPE, bulkConfigurationUpdateRequestArgumentCaptor.getValue().getActionType());
    Assertions.assertEquals(REQUEST_ID, bulkConfigurationUpdateRequestArgumentCaptor.getValue().getRequestId());
    Assertions.assertEquals(STORE_ID, bulkConfigurationUpdateRequestArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(USERNAME, bulkConfigurationUpdateRequestArgumentCaptor.getValue().getUpdatedBy());
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void getMerchantConfigurationHistoryTest() {
    Mockito.when(this.pcbFeign.getMerchantConfigurationHistory(DEFAULT_BUSINESS_PARTNER_CODE, PAGE, SIZE))
        .thenReturn(merchantConfigurationHistoryResponseGdnRestListResponse);
    Page<MerchantConfigurationHistoryWebResponse> merchantConfigurationHistoryWebResponsePage =
        this.postLiveConfigurationServiceImpl
            .getMerchantConfigurationHistory(DEFAULT_BUSINESS_PARTNER_CODE, PAGE, SIZE);
    Mockito.verify(this.pcbFeign).getMerchantConfigurationHistory(DEFAULT_BUSINESS_PARTNER_CODE, PAGE, SIZE);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        merchantConfigurationHistoryWebResponsePage.getContent().get(0).getMerchantCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        merchantConfigurationHistoryWebResponsePage.getContent().get(0).getMerchantName());
    Assertions.assertEquals(NEUTRAL_STATUS, merchantConfigurationHistoryWebResponsePage.getContent().get(0).getOldValue());
    Assertions.assertEquals(NEUTRAL_STATUS, merchantConfigurationHistoryWebResponsePage.getContent().get(0).getNewValue());
  }

  @Test
  public void downloadBulkConfigurationMerchantTest() {
    this.postLiveConfigurationServiceImpl
        .downloadBulkConfiguration(configurationFilterWebRequest, FLAG, STORE_ID, USERNAME);
    Mockito.verify(this.kafkaProducer)
        .send(eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), eq(USERNAME),
            bulkDownloadRequestArgumentCaptor.capture());
    MerchantConfigurationDownloadRequest merchantConfigurationDownloadRequest =
        (MerchantConfigurationDownloadRequest) bulkDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(merchantConfigurationDownloadRequest);
    Assertions.assertEquals(CATEGORY_CODE, merchantConfigurationDownloadRequest.getCategoryCode());
    Assertions.assertEquals(REVIEW_CONFIG_FLAG, merchantConfigurationDownloadRequest.getReviewConfig());
    Assertions.assertEquals(BulkProcessEntity.CONFIGURATION_MERCHANT_SUMMARY,
        merchantConfigurationDownloadRequest.getBulkProcessEntity());

  }

  @Test
  public void downloadBulkConfigurationCategoryTest() {
    this.postLiveConfigurationServiceImpl
        .downloadBulkConfiguration(configurationFilterWebRequest, CATEGORY, STORE_ID, USERNAME);
    Mockito.verify(this.kafkaProducer)
        .send(eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), eq(USERNAME),bulkDownloadRequestArgumentCaptor.capture());
    CategoryConfigurationDownloadRequest categoryConfigurationDownloadRequest =
        (CategoryConfigurationDownloadRequest) bulkDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(categoryConfigurationDownloadRequest);
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationDownloadRequest.getCategoryCode());
    Assertions.assertEquals(REVIEW_CONFIG_FLAG, categoryConfigurationDownloadRequest.getReviewConfig());
    Assertions.assertEquals(BulkProcessEntity.CONFIGURATION_CATEGORY_SUMMARY,
        categoryConfigurationDownloadRequest.getBulkProcessEntity());
  }
}
