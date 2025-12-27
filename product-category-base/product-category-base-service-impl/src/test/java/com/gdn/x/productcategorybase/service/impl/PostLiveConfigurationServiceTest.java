package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantSearchResponse;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.ConfigurationCountResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryConfiguration;
import com.gdn.x.productcategorybase.entity.CategoryConfigurationHistory;
import com.gdn.x.productcategorybase.entity.MerchantConfiguration;
import com.gdn.x.productcategorybase.entity.MerchantConfigurationHistory;
import com.gdn.x.productcategorybase.repository.MerchantConfigurationRepository;
import com.gdn.x.productcategorybase.service.BusinessPartner.BusinessPartnerService;
import com.gdn.x.productcategorybase.service.CategoryConfigurationHistoryService;
import com.gdn.x.productcategorybase.service.CategoryConfigurationService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.MerchantConfigurationHistoryService;
import com.gdn.x.productcategorybase.service.MerchantConfigurationService;

public class PostLiveConfigurationServiceTest {

  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "BliBli";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE_1 = "businessPartnerCode1";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME_1 = "BliBli1";
  private static final String POST_LIVE_STATUS = "Post-live";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private Long TOTAL_RECORDS = Long.valueOf(1);
  private static final String STORE_ID = "10001";
  private static final String CATEGORY_CODE = "CAT-01";
  private static final String CATEGORY_NAME = "CAT-NAME";
  private static final String SEARCH_KEY = "CAT";
  private static final String SORT_ORDER = "desc";
  private static final String CREATED_BY = "created_by";
  private static final String UPDATED_BY = "updated_by";
  private static final Long MERCHANT_CONFIGURATION_COUNT = Long.valueOf(10);
  private static final Long CATEGORY_CONFIGURATION_COUNT = Long.valueOf(20);
  private static final String PRE_LIVE_STATUS = "Pre-live";
  private static final String DEFAULT_REVIEW_CONFIG_FLAG = "Pre-live";
  private static final String DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT = "Neutral";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String CATEGORY_ID = "categoryId";
  private static final String ACTIVITY = "Registered";
  private static final String UPDATE_ACTIVITY = "Update";
  private static final String TYPE_OF_BUSINESS = "typeOfBusiness";
  private static final Date DATE = new Date();
  private static String TYPE = "Merchant";
  private static final String CODE = "code";
  private static final String NAME = "name";
  private static final String INACTIVE = "INACTIVE";
  private static final String ACTIVE = "ACTIVE";

  private MerchantConfigurationRequest merchantConfigurationRequest = new MerchantConfigurationRequest();
  private List<MerchantConfigurationRequest> merchantConfigurationRequestList = new ArrayList<>();
  private MerchantConfiguration merchantConfiguration = new MerchantConfiguration();
  private Pageable pageable = PageRequest.of(PAGE, SIZE);
  private CategoryConfiguration categoryConfiguration = new CategoryConfiguration();
  private Category category = new Category();
  private ConfigurationFilterRequest configurationFilterRequest;

  private List<CategoryConfigurationRequest> categoryConfigurationRequestList;
  private CategoryConfigurationRequest categoryConfigurationRequest;
  private List<CategoryConfiguration> categoryConfigurationList;
  private List<Category> categoryList;
  private CategoryConfigurationHistory categoryConfigurationHistory;
  private List<MerchantConfigurationRequest> merchantConfigurationRequestList1;
  private MerchantConfigurationRequest merchantConfigurationRequest1;
  private List<MerchantConfiguration> merchantConfigurationList;
  private MerchantConfiguration merchantConfiguration1;
  private MerchantConfigurationHistory merchantConfigurationHistory;
  private ConfigurationStatusRequest configurationStatusRequest;
  private ProfileResponse profileResponse;
  private CompanyDTO companyDTO;

  @Mock
  private MerchantConfigurationRepository merchantConfigurationRepository;

  @InjectMocks
  private PostLiveConfigurationServiceImpl postLiveConfigurationServiceImpl;

  @Mock
  private MerchantConfigurationService merchantConfigurationService;

  @Mock
  private CategoryConfigurationService categoryConfigurationService;

  @Mock
  private MerchantConfigurationHistoryService merchantConfigurationHistoryService;

  @Mock
  private CategoryConfigurationHistoryService categoryConfigurationHistoryService;

  @Mock
  private CategoryService categoryService;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    merchantConfigurationRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationRequest.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfigurationRequestList.add(merchantConfigurationRequest);
    merchantConfiguration.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfiguration.setMerchantName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfiguration.setMarkForDelete(false);
    merchantConfiguration.setReviewConfig(POST_LIVE_STATUS);
    merchantConfiguration.setCreatedDate(new Date());
    merchantConfiguration.setCreatedBy(CREATED_BY);
    categoryConfiguration.setReviewConfig(Constants.PRE_LIVE_STATUS);
    category.setName(CATEGORY_NAME);
    category.setCategoryCode(CATEGORY_CODE);
    category.setCreatedBy(CREATED_BY);
    category.setCreatedDate(new Date());
    category.setActivated(true);
    categoryConfiguration.setCategory(category);
    configurationFilterRequest =
        new ConfigurationFilterRequest(Constants.PRE_LIVE_STATUS, CATEGORY_CODE, SEARCH_KEY, SORT_ORDER);


    categoryConfigurationRequest = new CategoryConfigurationRequest();
    categoryConfigurationRequest.setCategoryCode(CATEGORY_CODE);
    categoryConfigurationRequest.setCategoryName(CATEGORY_NAME);
    categoryConfigurationRequest.setReviewConfig(POST_LIVE_STATUS);
    categoryConfigurationRequestList = new ArrayList<>();
    categoryConfigurationRequestList.add(categoryConfigurationRequest);

    categoryList = new ArrayList<>();
    categoryList.add(category);
    categoryConfigurationList = new ArrayList<>();
    categoryConfigurationList.add(categoryConfiguration);

    categoryConfigurationHistory = new CategoryConfigurationHistory();
    categoryConfigurationHistory.setCategoryCode(CATEGORY_CODE);
    categoryConfigurationHistory.setCategoryName(CATEGORY_NAME);
    categoryConfigurationHistory.setOldValue(PRE_LIVE_STATUS);
    categoryConfigurationHistory.setNewValue(POST_LIVE_STATUS);
    categoryConfigurationHistory.setActivity(ACTIVITY);
    categoryConfigurationHistory.setCreatedBy(CREATED_BY);
    categoryConfigurationHistory.setCreatedDate(new Date());
    categoryConfigurationHistory.setUpdatedBy(UPDATED_BY);
    categoryConfigurationHistory.setUpdatedDate(new Date());

    merchantConfigurationRequest1 = new MerchantConfigurationRequest();
    merchantConfigurationRequest1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationRequest1.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfigurationRequest1.setReviewConfig(DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT);
    merchantConfigurationRequestList1 = new ArrayList<>();
    merchantConfigurationRequestList1.add(merchantConfigurationRequest1);

    merchantConfiguration1 = new MerchantConfiguration();
    merchantConfiguration1.setCategoryName(CATEGORY_NAME);
    merchantConfiguration1.setMerchantName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfiguration1.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfiguration1.setReviewConfig(DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT);
    merchantConfigurationList = new ArrayList<>();
    merchantConfigurationList.add(merchantConfiguration1);

    merchantConfigurationHistory = new MerchantConfigurationHistory();
    merchantConfigurationHistory.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationHistory.setMerchantName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfigurationHistory.setOldValue(DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT);
    merchantConfigurationHistory.setNewValue(DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT);
    merchantConfigurationHistory.setActivity(ACTIVITY);
    merchantConfigurationHistory.setOldValue(Constants.NEUTRAL_STATUS);
    merchantConfigurationHistory.setMerchantName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfigurationHistory.setCreatedBy(CREATED_BY);
    merchantConfigurationHistory.setCreatedDate(new Date());
    merchantConfigurationHistory.setUpdatedBy(UPDATED_BY);
    merchantConfigurationHistory.setUpdatedDate(new Date());

    configurationStatusRequest = ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
        .categoryCode(CATEGORY_CODE).build();
    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    profileResponse.setMerchantStatus(ACTIVE);
    companyDTO = new CompanyDTO();
    companyDTO.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    companyDTO.setTypeOfBusiness(TYPE_OF_BUSINESS);
    profileResponse.setCompany(companyDTO);
    ReflectionTestUtils.setField(postLiveConfigurationServiceImpl, "categoryDefault", "Pre-live");
    ReflectionTestUtils.setField(postLiveConfigurationServiceImpl, "merchantDefault", "Neutral");

    pageable = PageRequest.of(PAGE, SIZE);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.merchantConfigurationRepository);
    Mockito.verifyNoMoreInteractions(this.categoryService);
    Mockito.verifyNoMoreInteractions(this.merchantConfigurationService);
    Mockito.verifyNoMoreInteractions(this.categoryConfigurationService);
    Mockito.verifyNoMoreInteractions(this.merchantConfigurationHistoryService);
    Mockito.verifyNoMoreInteractions(this.categoryConfigurationHistoryService);
    Mockito.verifyNoMoreInteractions(this.businessPartnerService);
  }

  @Test
  public void fetchMerchantConfigurationTest() {
    Mockito.when(this.merchantConfigurationRepository
        .findByMerchantCodeInAndMarkForDeleteFalse(Arrays.asList(DEFAULT_BUSINESS_PARTNER_CODE)))
        .thenReturn(Collections.singletonList(merchantConfiguration));
    List<MerchantSearchResponse> merchantSearchResponseList =
        postLiveConfigurationServiceImpl.fetchMerchantConfiguration(merchantConfigurationRequestList);
    Mockito.verify(this.merchantConfigurationRepository)
        .findByMerchantCodeInAndMarkForDeleteFalse(Arrays.asList(DEFAULT_BUSINESS_PARTNER_CODE));
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, merchantSearchResponseList.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, merchantSearchResponseList.get(0).getBusinessPartnerName());
    Assertions.assertEquals(POST_LIVE_STATUS, merchantSearchResponseList.get(0).getReviewConfig());
  }

  @Test
  public void fetchMerchantConfigurationTest_unConfiguredMerchant() {
    List<String> merchantCodesList = new ArrayList<>();
    merchantCodesList.add(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantCodesList.add(DEFAULT_BUSINESS_PARTNER_CODE_1);
    merchantConfigurationRequestList
        .add(new MerchantConfigurationRequest(DEFAULT_BUSINESS_PARTNER_CODE_1, DEFAULT_BUSINESS_PARTNER_NAME_1, null));
    Mockito.when(this.merchantConfigurationRepository.findByMerchantCodeInAndMarkForDeleteFalse(merchantCodesList))
        .thenReturn(Collections.singletonList(merchantConfiguration));
    List<MerchantSearchResponse> merchantSearchResponseList =
        postLiveConfigurationServiceImpl.fetchMerchantConfiguration(merchantConfigurationRequestList);
    Mockito.verify(this.merchantConfigurationRepository)
        .findByMerchantCodeInAndMarkForDeleteFalse(merchantCodesList);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, merchantSearchResponseList.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, merchantSearchResponseList.get(0).getBusinessPartnerName());
    Assertions.assertEquals(POST_LIVE_STATUS, merchantSearchResponseList.get(0).getReviewConfig());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE_1, merchantSearchResponseList.get(1).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME_1, merchantSearchResponseList.get(1).getBusinessPartnerName());
    Assertions.assertEquals(Constants.NEUTRAL_STATUS, merchantSearchResponseList.get(1).getReviewConfig());
  }

  @Test
  public void fetchConfigurationCountsTest() {
    Mockito.when(this.merchantConfigurationService.getMerchantConfigurationCount(STORE_ID))
        .thenReturn(MERCHANT_CONFIGURATION_COUNT);
    Mockito.when(this.categoryConfigurationService.getCategoryConfigurationCount(STORE_ID))
        .thenReturn(CATEGORY_CONFIGURATION_COUNT);
    ConfigurationCountResponse configurationCountResponse =
        this.postLiveConfigurationServiceImpl.fetchConfigurationCounts(STORE_ID);
    Mockito.verify(this.categoryConfigurationService).getCategoryConfigurationCount(STORE_ID);
    Mockito.verify(this.merchantConfigurationService).getMerchantConfigurationCount(STORE_ID);
    Assertions.assertEquals(MERCHANT_CONFIGURATION_COUNT, configurationCountResponse.getMerchantConfigurationCount());
    Assertions.assertEquals(CATEGORY_CONFIGURATION_COUNT, configurationCountResponse.getCategoryConfigurationCount());
  }

  @Test
  public void getCategoryConfigurationListTest() {
    Mockito.when(
        this.categoryConfigurationService.getCategoryConfigurationList(STORE_ID, configurationFilterRequest, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(categoryConfiguration), pageable, TOTAL_RECORDS));
    Page<CategoryConfigurationFilterResponse> categoryConfigurationFilterResponsePage =
        this.postLiveConfigurationServiceImpl
            .getCategoryConfigurationList(STORE_ID, configurationFilterRequest, PAGE, SIZE);
    Mockito.verify(this.categoryConfigurationService)
        .getCategoryConfigurationList(STORE_ID, configurationFilterRequest, pageable);
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationFilterResponsePage.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS,
        categoryConfigurationFilterResponsePage.getContent().get(0).getReviewConfig());
    Assertions.assertEquals(CATEGORY_NAME, categoryConfigurationFilterResponsePage.getContent().get(0).getCategoryName());
  }

  @Test
  public void getCategoryConfigurationListTest_emptySearchKey() {
    configurationFilterRequest.setSearchKey(null);
    Mockito.when(
        this.categoryConfigurationService.getCategoryConfigurationList(STORE_ID, configurationFilterRequest, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(categoryConfiguration), pageable, TOTAL_RECORDS));
    Page<CategoryConfigurationFilterResponse> categoryConfigurationFilterResponsePage =
        this.postLiveConfigurationServiceImpl
            .getCategoryConfigurationList(STORE_ID, configurationFilterRequest, PAGE, SIZE);
    Mockito.verify(this.categoryConfigurationService)
        .getCategoryConfigurationList(STORE_ID, configurationFilterRequest, pageable);
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationFilterResponsePage.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(Constants.PRE_LIVE_STATUS, categoryConfigurationFilterResponsePage.getContent().get(0).getReviewConfig());
    Assertions.assertEquals(CATEGORY_NAME, categoryConfigurationFilterResponsePage.getContent().get(0).getCategoryName());
  }

  @Test
  public void addCategoryConfigurationTest() throws Exception {
    categoryConfiguration.setReviewConfig(Constants.POST_LIVE_STATUS);
    Mockito.when(this.categoryService
        .findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE)))
        .thenReturn(categoryList);
    Mockito.when(this.categoryConfigurationService
        .getCategoryConfigurationsByCategoryList(DEFAULT_STORE_ID, Collections.singletonList(category))).thenReturn(new ArrayList<>());
    this.postLiveConfigurationServiceImpl.addCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequestList);
    Mockito.verify(this.categoryService)
        .findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE));
    Mockito.verify(this.categoryConfigurationService)
        .getCategoryConfigurationsByCategoryList(DEFAULT_STORE_ID, Collections.singletonList(category));
    Mockito.verify(this.categoryConfigurationService).saveCategoryConfigurations(Mockito.anyList());
    Mockito.verify(this.categoryConfigurationHistoryService)
        .saveCategoryHistoryConfigurations(Mockito.anyList());
  }

  @Test
  public void addCategoryConfigurationWithExistingRecordTest() throws Exception {
    categoryConfigurationHistory.setActivity(UPDATE_ACTIVITY);
    categoryConfigurationHistory.setOldValue(PRE_LIVE_STATUS);
    categoryConfigurationHistory.setNewValue(POST_LIVE_STATUS);
    Mockito.when(this.categoryService
        .findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE)))
        .thenReturn(categoryList);
    Mockito.when(this.categoryConfigurationService
        .getCategoryConfigurationsByCategoryList(DEFAULT_STORE_ID, Collections.singletonList(category)))
        .thenReturn(categoryConfigurationList);
    this.postLiveConfigurationServiceImpl.addCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequestList);
    Mockito.verify(this.categoryService)
        .findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE));
    Mockito.verify(this.categoryConfigurationService)
        .getCategoryConfigurationsByCategoryList(DEFAULT_STORE_ID, Collections.singletonList(category));
    Mockito.verify(this.categoryConfigurationService)
        .saveCategoryConfigurations(Collections.singletonList(categoryConfiguration));
    Mockito.verify(this.categoryConfigurationHistoryService)
        .saveCategoryHistoryConfigurations(Mockito.anyList());
  }

  @Test
  public void addCategoryConfigurationWithInActiveCategoryTest() throws Exception {
    category.setActivated(false);
    Mockito.when(
        this.categoryService.findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(category));
    Mockito.when(this.categoryConfigurationService
        .getCategoryConfigurationsByCategoryList(DEFAULT_STORE_ID, Collections.singletonList(category)))
        .thenReturn(categoryConfigurationList);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.postLiveConfigurationServiceImpl
          .addCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequestList));
    } finally {
      Mockito.verify(this.categoryService)
          .findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE));
      Mockito.verify(this.categoryConfigurationService)
          .getCategoryConfigurationsByCategoryList(DEFAULT_STORE_ID, Collections.singletonList(category));
    }
  }

  @Test
  public void updateCategoryConfigurationTest() throws Exception {
    categoryConfigurationHistory.setActivity(UPDATE_ACTIVITY);
    Mockito.when(
        this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, CATEGORY_CODE))
        .thenReturn(category);
    Mockito.when(
        this.categoryConfigurationService.getCategoryConfigurationByCategoryAndMarkForDeleteFalse(DEFAULT_STORE_ID, category))
        .thenReturn(categoryConfiguration);
    this.postLiveConfigurationServiceImpl.updateCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequest);
    Mockito.verify(this.categoryService)
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, CATEGORY_CODE);
    Mockito.verify(this.categoryConfigurationService)
        .getCategoryConfigurationByCategoryAndMarkForDeleteFalse(DEFAULT_STORE_ID, category);
    Mockito.verify(this.categoryConfigurationService).saveCategoryConfiguration(categoryConfiguration);
    Mockito.verify(this.categoryConfigurationHistoryService)
        .saveCategoryHistoryConfiguration(Mockito.any(CategoryConfigurationHistory.class));
  }

  @Test
  public void updateCategoryConfigurationWithInActiveCategoryTest() throws Exception {
    category.setActivated(false);
    Mockito.when(this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, CATEGORY_CODE)).thenReturn(category);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.postLiveConfigurationServiceImpl.updateCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequest));
    } finally {
      Mockito.verify(this.categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
          DEFAULT_STORE_ID, CATEGORY_CODE);
    }
  }

  @Test
  public void deleteCategoryConfigurationTest() throws Exception {
    categoryConfigurationHistory.setActivity(UPDATE_ACTIVITY);
    categoryConfigurationHistory.setNewValue(PRE_LIVE_STATUS);
    Mockito.when(
        this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, CATEGORY_CODE))
        .thenReturn(category);
    Mockito.when(this.categoryConfigurationService.getCategoryConfigurationByCategory(DEFAULT_STORE_ID, category))
        .thenReturn(categoryConfiguration);
    this.postLiveConfigurationServiceImpl.deleteCategoryConfiguration(DEFAULT_STORE_ID, CATEGORY_CODE);
    Mockito.verify(this.categoryService)
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, CATEGORY_CODE);
    Mockito.verify(this.categoryConfigurationService)
        .getCategoryConfigurationByCategory(DEFAULT_STORE_ID, category);
    Mockito.verify(this.categoryConfigurationService).saveCategoryConfiguration(categoryConfiguration);
    Mockito.verify(this.categoryConfigurationHistoryService)
        .saveCategoryHistoryConfiguration(Mockito.any(CategoryConfigurationHistory.class));
  }

  @Test
  public void addMerchantConfigurationTest() throws Exception {
    merchantConfiguration1.setReviewConfig(DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT);
    profileResponse.setMerchantStatus(ACTIVE);
    Mockito.when(this.businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(this.merchantConfigurationService.getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE))).thenReturn(new ArrayList<>());
    this.postLiveConfigurationServiceImpl.addMerchantConfiguration(DEFAULT_STORE_ID, merchantConfigurationRequestList1);
    Mockito.verify(this.businessPartnerService).getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationService).getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.verify(this.merchantConfigurationService).saveMerchantConfigurations(Mockito.anyList());
    Mockito.verify(this.merchantConfigurationHistoryService)
        .saveMerchantHistoryConfigurations(Mockito.anyList());
  }

  @Test
  public void addMerchantConfigurationWithExistingRecordTest() throws Exception {
    merchantConfigurationRequest1.setReviewConfig(PRE_LIVE_STATUS);
    merchantConfigurationHistory.setNewValue(PRE_LIVE_STATUS);
    merchantConfigurationHistory.setActivity(UPDATE_ACTIVITY);
    profileResponse.setMerchantStatus(ACTIVE);
    Mockito.when(this.merchantConfigurationService.getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE))).thenReturn(merchantConfigurationList);
    Mockito.when(this.businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    this.postLiveConfigurationServiceImpl
        .addMerchantConfiguration(DEFAULT_STORE_ID, Arrays.asList(merchantConfigurationRequest1));
    Mockito.verify(this.merchantConfigurationService).getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.verify(this.businessPartnerService).getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationService)
        .saveMerchantConfigurations(Collections.singletonList(merchantConfiguration1));
    Mockito.verify(this.merchantConfigurationHistoryService)
        .saveMerchantHistoryConfigurations(Mockito.anyList());
  }

  @Test
  public void addMerchantConfigurationWithInActiveMerchantTest() throws Exception {
    profileResponse.setMerchantStatus(INACTIVE);
    Mockito.when(this.merchantConfigurationService.getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE))).thenReturn(merchantConfigurationList);
    Mockito.when(this.businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.postLiveConfigurationServiceImpl
          .addMerchantConfiguration(DEFAULT_STORE_ID, Arrays.asList(merchantConfigurationRequest1)));
    } finally {
      Mockito.verify(this.merchantConfigurationService).getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
          Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE));
      Mockito.verify(this.businessPartnerService).getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void updateMerchantConfigurationTest() throws Exception {
    merchantConfigurationHistory.setActivity(UPDATE_ACTIVITY);
    merchantConfigurationRequest1.setReviewConfig(POST_LIVE_STATUS);
    merchantConfigurationHistory.setNewValue(POST_LIVE_STATUS);
    Mockito.when(this.businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(this.merchantConfigurationService
        .getMerchantConfigurationByMerchantCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(merchantConfiguration1);
    this.postLiveConfigurationServiceImpl.updateMerchantConfiguration(DEFAULT_STORE_ID, merchantConfigurationRequest1);
    Mockito.verify(this.businessPartnerService).getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationService)
        .getMerchantConfigurationByMerchantCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationService).saveMerchantConfiguration(merchantConfiguration1);
    Mockito.verify(this.merchantConfigurationHistoryService)
        .saveMerchantHistoryConfiguration(Mockito.any(MerchantConfigurationHistory.class));
  }

  @Test
  public void updateMerchantConfigurationWithInActiveMerchantTest() throws Exception {
    Mockito.when(this.businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.postLiveConfigurationServiceImpl
          .updateMerchantConfiguration(DEFAULT_STORE_ID, merchantConfigurationRequest1));
    } finally {
      Mockito.verify(this.businessPartnerService).getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void updateMerchantConfigurationPrivateMethodExceptionTest() throws Exception {
    merchantConfigurationHistory.setActivity(UPDATE_ACTIVITY);
    merchantConfigurationRequest1.setReviewConfig("Neutral_Postlive");
    merchantConfigurationHistory.setNewValue(POST_LIVE_STATUS);
    Mockito.when(this.businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(this.merchantConfigurationService
        .getMerchantConfigurationByMerchantCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(merchantConfiguration1);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.postLiveConfigurationServiceImpl.updateMerchantConfiguration(DEFAULT_STORE_ID, merchantConfigurationRequest1));
    }
    finally {
      Mockito.verify(this.businessPartnerService).getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
      Mockito.verify(this.merchantConfigurationService)
          .getMerchantConfigurationByMerchantCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void deleteMerchantConfigurationTest() throws Exception {
    merchantConfigurationHistory.setActivity(UPDATE_ACTIVITY);
    Mockito.when(this.merchantConfigurationService
        .getMerchantConfigurationByMerchantCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(merchantConfiguration1);
    this.postLiveConfigurationServiceImpl.deleteMerchantConfiguration(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationService)
        .getMerchantConfigurationByMerchantCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationService).saveMerchantConfiguration(merchantConfiguration1);
    Mockito.verify(this.merchantConfigurationHistoryService)
        .saveMerchantHistoryConfiguration(Mockito.any(MerchantConfigurationHistory.class));
  }

  @Test
  public void getConfigurationsTest() throws Exception {
    Mockito.when(this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, CATEGORY_CODE)).thenReturn(category);
    Mockito.when(this.categoryConfigurationService
        .getCategoryConfigurationByCategoryAndMarkForDeleteFalse(DEFAULT_STORE_ID, category))
        .thenReturn(categoryConfiguration);
    Mockito.when(this.businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(this.merchantConfigurationService
        .getMerchantConfigurationByMerchantCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(merchantConfiguration);
    List<ConfigurationStatusResponse> configurationStatusResponseList = postLiveConfigurationServiceImpl
        .getConfigurations(DEFAULT_STORE_ID, Collections.singletonList(configurationStatusRequest));
    Mockito.verify(this.categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, CATEGORY_CODE);
    Mockito.verify(this.categoryConfigurationService)
        .getCategoryConfigurationByCategoryAndMarkForDeleteFalse(DEFAULT_STORE_ID, category);
    Mockito.verify(this.businessPartnerService).getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationService)
        .getMerchantConfigurationByMerchantCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, configurationStatusResponseList.get(0).getMerchantCode());
    Assertions.assertEquals(CATEGORY_CODE, configurationStatusResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(POST_LIVE_STATUS, configurationStatusResponseList.get(0).getReviewConfig());
  }

  @Test
  public void getConfigurationsTestWithPostLiveFlag() throws Exception {
    merchantConfiguration.setReviewConfig(POST_LIVE_STATUS);
    categoryConfiguration.setReviewConfig(POST_LIVE_STATUS);
    category.setCategoryCode(CATEGORY_CODE);
    Mockito.when(this.categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, CATEGORY_CODE))
        .thenReturn(category);
    Mockito.when(this.categoryConfigurationService
        .getCategoryConfigurationByCategoryAndMarkForDeleteFalse(DEFAULT_STORE_ID, category))
        .thenReturn(categoryConfiguration);
    Mockito.when(this.businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(this.merchantConfigurationService
        .getMerchantConfigurationByMerchantCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(merchantConfiguration);
    List<ConfigurationStatusResponse> configurationStatusResponseList = postLiveConfigurationServiceImpl
        .getConfigurations(DEFAULT_STORE_ID, Collections.singletonList(configurationStatusRequest));
    Mockito.verify(this.categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, CATEGORY_CODE);
    Mockito.verify(this.categoryConfigurationService)
        .getCategoryConfigurationByCategoryAndMarkForDeleteFalse(DEFAULT_STORE_ID, category);
    Mockito.verify(this.businessPartnerService).getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationService)
        .getMerchantConfigurationByMerchantCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, configurationStatusResponseList.get(0).getMerchantCode());
    Assertions.assertEquals(CATEGORY_CODE, configurationStatusResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(POST_LIVE_STATUS, configurationStatusResponseList.get(0).getReviewConfig());
  }

  @Test
  public void bulkMerchantConfigUploadTest() throws Exception {
    merchantConfigurationHistory.setActivity(UPDATE_ACTIVITY);
    merchantConfiguration1.setReviewConfig(Constants.PRE_LIVE_STATUS);
    merchantConfigurationHistory.setOldValue(Constants.PRE_LIVE_FLAG);
    profileResponse.setMerchantStatus(ACTIVE);
    Mockito.when(this.businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(this.merchantConfigurationService.getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE))).thenReturn(merchantConfigurationList);
    List<BulkMerchantConfigUploadResponse> response = this.postLiveConfigurationServiceImpl
        .bulkMerchantConfigUpload(DEFAULT_STORE_ID, merchantConfigurationRequestList1);
    Mockito.verify(this.businessPartnerService, Mockito.times(2))
        .getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationService).getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.verify(this.merchantConfigurationService)
        .saveMerchantConfigurations(Collections.singletonList(merchantConfiguration1));
    Mockito.verify(this.merchantConfigurationHistoryService)
        .saveMerchantHistoryConfigurations(Mockito.anyList());
    Assertions.assertEquals(Constants.SUCCESS, response.get(0).getErrorMessage());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, response.get(0).getBusinessPartnerCode());
  }

  @Test
  public void bulkMerchantConfigUploadProfileResponseExceptionTest() throws Exception {
    merchantConfigurationRequestList1.get(0).setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME_1);
    Mockito.when(this.businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenThrow(ApplicationRuntimeException.class);
    List<BulkMerchantConfigUploadResponse> response = this.postLiveConfigurationServiceImpl
        .bulkMerchantConfigUpload(DEFAULT_STORE_ID, merchantConfigurationRequestList1);
    Mockito.verify(this.businessPartnerService).getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(ErrorMessage.NOT_VALID_MERCHANT.getMessage(), response.get(0).getErrorMessage());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME_1, response.get(0).getBusinessPartnerName());
  }

  @Test
  public void bulkMerchantConfigUploadAddMerchantConfigExceptionTest() throws Exception {
    Mockito.when(this.businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(this.merchantConfigurationService.getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE))).thenThrow(RuntimeException.class);
    List<BulkMerchantConfigUploadResponse> response = this.postLiveConfigurationServiceImpl
        .bulkMerchantConfigUpload(DEFAULT_STORE_ID, merchantConfigurationRequestList1);
    Mockito.verify(this.businessPartnerService).getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationService).getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE));
    Assertions.assertEquals(ErrorMessage.ERROR_UPDATING_MERCHANT_CONFIG.getMessage(), response.get(0).getErrorMessage());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, response.get(0).getBusinessPartnerName());
  }

  @Test
  public void bulkMerchantConfigUploadAddMerchantConfigApplicationRuntimeExceptionTest() throws Exception {
    Mockito.when(this.businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(this.merchantConfigurationService.getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE)))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND));
    List<BulkMerchantConfigUploadResponse> response = this.postLiveConfigurationServiceImpl
        .bulkMerchantConfigUpload(DEFAULT_STORE_ID, merchantConfigurationRequestList1);
    Mockito.verify(this.businessPartnerService).getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationService).getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE));
    Assertions.assertEquals(ErrorCategory.DATA_NOT_FOUND.getMessage(), response.get(0).getErrorMessage());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, response.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, response.get(0).getBusinessPartnerName());
  }

  @Test
  public void bulkCategoryConfigUploadTest() throws Exception {
    categoryConfiguration.setReviewConfig(Constants.POST_LIVE_STATUS);
    Mockito.when(
        this.categoryService.findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE)))
        .thenReturn(categoryList);
    Mockito.when(this.categoryConfigurationService
        .getCategoryConfigurationsByCategoryList(DEFAULT_STORE_ID, Collections.singletonList(category)))
        .thenReturn(new ArrayList<>());
    List<BulkCategoryConfigUploadResponse> response = this.postLiveConfigurationServiceImpl
        .bulkCategoryConfigUpload(DEFAULT_STORE_ID, categoryConfigurationRequestList);
    Mockito.verify(this.categoryService, Mockito.times(2))
        .findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE));
    Mockito.verify(this.categoryConfigurationService)
        .getCategoryConfigurationsByCategoryList(DEFAULT_STORE_ID, Collections.singletonList(category));
    Mockito.verify(this.categoryConfigurationService).saveCategoryConfigurations(Mockito.anyList());
    Mockito.verify(this.categoryConfigurationHistoryService)
        .saveCategoryHistoryConfigurations(Mockito.anyList());
    Assertions.assertEquals(Constants.SUCCESS, response.get(0).getErrorMessage());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
  }

  @Test
  public void bulkCategoryConfigUploadCategoryConfigTest() throws Exception {
    Mockito.when(
        this.categoryService.findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE)))
        .thenThrow(ApplicationRuntimeException.class);
    List<BulkCategoryConfigUploadResponse> response = null;
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.postLiveConfigurationServiceImpl
          .bulkCategoryConfigUpload(DEFAULT_STORE_ID, categoryConfigurationRequestList));
    } catch (ApplicationRuntimeException e) {
      throw e;
    } finally {
      Mockito.verify(this.categoryService)
          .findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE));
      assertNull(response);
    }
  }

  @Test
  public void bulkCategoryConfigUploadCategoryConfigExceptionTest() throws Exception {
    Mockito.when(
        this.categoryService.findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE)))
        .thenThrow(ApplicationRuntimeException.class);
    List<BulkCategoryConfigUploadResponse> response = null;
    try {
      response = this.postLiveConfigurationServiceImpl
          .bulkCategoryConfigUpload(DEFAULT_STORE_ID, categoryConfigurationRequestList);
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(this.categoryService)
          .findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE));
      assertNull(response);
    }
  }

  @Test
  public void getConfigurationChangesByDate() throws Exception {
    Page<CategoryConfiguration> categoryConfigurationPage = new PageImpl<>(Collections.singletonList(categoryConfiguration));
    Page<MerchantConfiguration> merchantConfigurationPage = new PageImpl<>(Collections.singletonList(merchantConfiguration));
    Mockito.when(categoryConfigurationService.getCategoryConfigurationByUpdatedDateGreaterThan(
        Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DATE), Mockito.any(Pageable.class))).thenReturn(categoryConfigurationPage);
    Mockito.when(merchantConfigurationService.getMerchantConfigurationByUpdatedDateGreaterThan(
        Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DATE), Mockito.any(Pageable.class))).thenReturn(merchantConfigurationPage);

    List<ConfigurationStatusResponse> configurationStatusResponseList = postLiveConfigurationServiceImpl
        .getConfigurationChangesByDate(DEFAULT_STORE_ID, DATE);

    Mockito.verify(categoryConfigurationService).getCategoryConfigurationByUpdatedDateGreaterThan(
        Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DATE), Mockito.any(Pageable.class));
    Mockito.verify(merchantConfigurationService).getMerchantConfigurationByUpdatedDateGreaterThan(
        Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DATE), Mockito.any(Pageable.class));

    Assertions.assertEquals(CATEGORY_CODE, configurationStatusResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(DEFAULT_REVIEW_CONFIG_FLAG, configurationStatusResponseList.get(0).getReviewConfig());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, configurationStatusResponseList.get(1).getMerchantCode());
    Assertions.assertEquals(POST_LIVE_STATUS, configurationStatusResponseList.get(1).getReviewConfig());
  }

  @Test
  public void getMerchantConfigurationListTest() throws Exception {
    Mockito.when(
        this.merchantConfigurationService.getMerchantConfigurationPage(STORE_ID, configurationFilterRequest, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(merchantConfiguration), pageable, TOTAL_RECORDS));
    Page<MerchantConfigurationFilterResponse> merchantConfigurationFilterResponsePage =
        this.postLiveConfigurationServiceImpl
            .getMerchantConfigurationList(STORE_ID, configurationFilterRequest, PAGE, SIZE);
    Mockito.verify(this.merchantConfigurationService)
        .getMerchantConfigurationPage(STORE_ID, configurationFilterRequest, pageable);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        merchantConfigurationFilterResponsePage.getContent().get(0).getMerchantCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, merchantConfigurationFilterResponsePage.getContent().get(0).getMerchantName());
    Assertions.assertEquals(POST_LIVE_STATUS, merchantConfigurationFilterResponsePage.getContent().get(0).getReviewConfig());
    Assertions.assertEquals(CREATED_BY, merchantConfigurationFilterResponsePage.getContent().get(0).getCreatedBy());
    Assertions.assertNotNull(merchantConfigurationFilterResponsePage.getContent().get(0).getCreatedDate());
  }

  @Test
  public void getCategoryConfigurationTest() {
    categoryConfigurationHistory.setCategoryName(CATEGORY_NAME);
    Mockito.when(this.categoryConfigurationHistoryService
        .getCategoryConfigurationHistory(DEFAULT_STORE_ID, CATEGORY_CODE, PAGE, SIZE))
        .thenReturn(new PageImpl<>(Arrays.asList(categoryConfigurationHistory), pageable, TOTAL_RECORDS));
    Page<CategoryConfigurationHistoryResponse> categoryConfigurationHistoryResponsePage =
        this.postLiveConfigurationServiceImpl
            .getCategoryConfigurationHistory(DEFAULT_STORE_ID, CATEGORY_CODE, PAGE, SIZE);
    Mockito.verify(this.categoryConfigurationHistoryService)
        .getCategoryConfigurationHistory(DEFAULT_STORE_ID, CATEGORY_CODE, PAGE, SIZE);
    Assertions.assertEquals(CATEGORY_NAME, categoryConfigurationHistoryResponsePage.getContent().get(0).getCategoryName());
    Assertions.assertEquals(CATEGORY_CODE, categoryConfigurationHistoryResponsePage.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(ACTIVITY, categoryConfigurationHistoryResponsePage.getContent().get(0).getActivity());
    Assertions.assertEquals(CREATED_BY, categoryConfigurationHistoryResponsePage.getContent().get(0).getCreatedBy());
    Assertions.assertEquals(UPDATED_BY, categoryConfigurationHistoryResponsePage.getContent().get(0).getUpdatedBy());
    Assertions.assertNotNull(categoryConfigurationHistoryResponsePage.getContent().get(0).getCreatedDate());
    Assertions.assertNotNull(categoryConfigurationHistoryResponsePage.getContent().get(0).getUpdatedDate());
  }

  @Test
  public void getMerchantConfigurationHistoryTest() {
    Mockito.when(this.merchantConfigurationHistoryService
        .getMerchantConfigurationHistoryPage(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(merchantConfigurationHistory), pageable, TOTAL_RECORDS));
    Page<MerchantConfigurationHistoryResponse> merchantConfigurationHistoryResponseList =
        this.postLiveConfigurationServiceImpl
            .getMerchantConfigurationHistory(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, PAGE, SIZE);
    Mockito.verify(this.merchantConfigurationHistoryService)
        .getMerchantConfigurationHistoryPage(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, merchantConfigurationHistoryResponseList.getContent().get(0).getMerchantName());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, merchantConfigurationHistoryResponseList.getContent().get(0).getMerchantCode());
    Assertions.assertEquals(Constants.NEUTRAL_STATUS, merchantConfigurationHistoryResponseList.getContent().get(0).getOldValue());
    Assertions.assertEquals(Constants.NEUTRAL_STATUS, merchantConfigurationHistoryResponseList.getContent().get(0).getNewValue());
    Assertions.assertEquals(ACTIVITY, merchantConfigurationHistoryResponseList.getContent().get(0).getActivity());
    Assertions.assertEquals(CREATED_BY, merchantConfigurationHistoryResponseList.getContent().get(0).getCreatedBy());
    Assertions.assertEquals(UPDATED_BY, merchantConfigurationHistoryResponseList.getContent().get(0).getUpdatedBy());
    Assertions.assertNotNull(merchantConfigurationHistoryResponseList.getContent().get(0).getCreatedDate());
    Assertions.assertNotNull(merchantConfigurationHistoryResponseList.getContent().get(0).getUpdatedDate());
  }

  @Test
  public void fetchConfigDetailsByConfigTypeForMerchantCodesTest() throws Exception {
    Mockito.when(this.merchantConfigurationService.getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE))).thenReturn(merchantConfigurationList);
    List<BulkConfigDataResponse> result = postLiveConfigurationServiceImpl
        .fetchConfigDetailsByConfigTypeForCodes(STORE_ID, TYPE, Arrays.asList(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.verify(this.merchantConfigurationService).getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE));
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, result.get(0).getCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, result.get(0).getName());
  }

  @Test
  public void fetchConfigDetailsByConfigTypeForCodesCategoryTest() throws Exception {
    TYPE = "Category";
    Mockito.when(
        this.categoryService.findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE)))
        .thenReturn(categoryList);
    Mockito.when(this.categoryConfigurationService
        .getCategoryConfigurationsByCategoryList(DEFAULT_STORE_ID, Collections.singletonList(category)))
        .thenReturn(categoryConfigurationList);
    List<BulkConfigDataResponse> result = postLiveConfigurationServiceImpl
        .fetchConfigDetailsByConfigTypeForCodes(STORE_ID, TYPE, Arrays.asList(CATEGORY_CODE));
    Mockito.verify(this.categoryService)
        .findByStoreIdAndCategoryCodes(DEFAULT_STORE_ID, Collections.singletonList(CATEGORY_CODE));
    Mockito.verify(this.categoryConfigurationService)
        .getCategoryConfigurationsByCategoryList(DEFAULT_STORE_ID, Collections.singletonList(category));
    Assertions.assertEquals(CATEGORY_CODE, result.get(0).getCode());
  }

  @Test
  public void updateCategoryConfigurationExceptionTest() {
    categoryConfigurationRequest.setReviewConfig("Neutral");
    try {
      categoryConfigurationHistory.setActivity(UPDATE_ACTIVITY);
      Mockito.when(this.categoryService
          .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, CATEGORY_CODE)).thenReturn(category);
      Mockito.when(this.categoryConfigurationService
          .getCategoryConfigurationByCategoryAndMarkForDeleteFalse(DEFAULT_STORE_ID, category)).thenReturn(categoryConfiguration);
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.postLiveConfigurationServiceImpl.updateCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequest));
    }
    finally {
      Mockito.verify(this.categoryService)
          .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, CATEGORY_CODE);
      Mockito.verify(this.categoryConfigurationService)
          .getCategoryConfigurationByCategoryAndMarkForDeleteFalse(DEFAULT_STORE_ID, category);
    }
  }

}
