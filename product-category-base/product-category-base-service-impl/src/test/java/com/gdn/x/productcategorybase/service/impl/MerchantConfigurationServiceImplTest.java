package com.gdn.x.productcategorybase.service.impl;

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

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.MerchantConfiguration;
import com.gdn.x.productcategorybase.repository.MerchantConfigurationRepository;
import com.gdn.x.productcategorybase.service.CategoryService;

public class MerchantConfigurationServiceImplTest {

  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "BliBli";
  private static final String DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT = "Neutral";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String CATEGORY_NAME = "category";
  private static final String CATEGORY_CODE = "CAT-01";
  private static final Long MERCHANT_CONFIGURATION_COUNT = Long.valueOf(10);
  private static final Date DATE = new Date();
  private static final Pageable PAGEABLE = PageRequest.of(0, 10);
  private static final String CREATED_BY = "CREATED_BY";
  private static final String KEYWORD = "keyword";
  private static final String SORT_ORDER = "desc";
  private static final int SIZE = 25;
  private static final int PAGE = 0;
  private static final Long TOTAL_RECORDS = Long.valueOf(1);

  private MerchantConfiguration merchantConfiguration;
  private Category category = new Category();
  private Pageable pageable;
  private ConfigurationFilterRequest configurationFilterRequest;

  @Mock
  private MerchantConfigurationRepository merchantConfigurationRepository;

  @Mock
  private CategoryService categoryService;

  @InjectMocks
  private MerchantConfigurationServiceImpl merchantConfigurationService;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);

    merchantConfiguration = new MerchantConfiguration();
    merchantConfiguration.setCategoryName(CATEGORY_NAME);
    merchantConfiguration.setMerchantName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfiguration.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfiguration.setReviewConfig(DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT);
    merchantConfiguration.setCreatedBy(CREATED_BY);
    merchantConfiguration.setCreatedDate(new Date());

    category.setCategoryCode(CATEGORY_CODE);
    category.setName(CATEGORY_NAME);

    pageable = PageRequest.of(PAGE, SIZE);
    configurationFilterRequest =
        new ConfigurationFilterRequest(Constants.NEUTRAL_STATUS, CATEGORY_CODE, KEYWORD, SORT_ORDER);
  }

  @Test
  public void getMerchantConfigurationsByMerchantCodeListTest() throws Exception {
    Mockito.when(this.merchantConfigurationRepository
        .findByStoreIdAndMerchantCodeIn(DEFAULT_STORE_ID, Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE)))
        .thenReturn(new ArrayList<>());
    this.merchantConfigurationService.getMerchantConfigurationsByMerchantCodeList(DEFAULT_STORE_ID,
        Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.verify(this.merchantConfigurationRepository)
        .findByStoreIdAndMerchantCodeIn(DEFAULT_STORE_ID, Collections.singletonList(DEFAULT_BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getMerchantConfigurationByMerchantCodeTest() throws Exception {
    Mockito.when(this.merchantConfigurationRepository
        .findByStoreIdAndMerchantCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(merchantConfiguration);
    this.merchantConfigurationService
        .getMerchantConfigurationByMerchantCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationRepository)
        .findByStoreIdAndMerchantCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void getMerchantConfigurationByMerchantCodeByMarkForDeleteFalseTest() throws Exception {
    Mockito.when(this.merchantConfigurationRepository
        .findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(merchantConfiguration);
    this.merchantConfigurationService
        .getMerchantConfigurationByMerchantCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.merchantConfigurationRepository)
        .findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void saveMerchantEntitiesTest() throws Exception {
    this.merchantConfigurationService.saveMerchantConfigurations(Collections.singletonList(merchantConfiguration));
    Mockito.verify(this.merchantConfigurationRepository).saveAll(Collections.singletonList(merchantConfiguration));
  }

  @Test
  public void saveMerchantEntityTest() throws Exception {
    this.merchantConfigurationService.saveMerchantConfiguration(merchantConfiguration);
    Mockito.verify(this.merchantConfigurationRepository).save(merchantConfiguration);
  }

  @Test
  public void getMerchantConfigurationCountTest() throws Exception {
    Mockito.when(this.merchantConfigurationRepository.countByStoreIdAndMarkForDeleteFalse(DEFAULT_STORE_ID))
        .thenReturn(MERCHANT_CONFIGURATION_COUNT);
    Long result = this.merchantConfigurationService.getMerchantConfigurationCount(DEFAULT_STORE_ID);
    Mockito.verify(this.merchantConfigurationRepository).countByStoreIdAndMarkForDeleteFalse(DEFAULT_STORE_ID);
    Assertions.assertEquals(MERCHANT_CONFIGURATION_COUNT, result);
  }

  @Test
  public void getMerchantConfigurationByUpdatedDateGreaterThanTest() {
    List<MerchantConfiguration> merchantConfigurationList = Collections.singletonList(merchantConfiguration);
    Mockito.when(this.merchantConfigurationRepository.findByStoreIdAndUpdatedDateGreaterThan(
        DEFAULT_STORE_ID, DATE, PAGEABLE))
        .thenReturn(new PageImpl<>(merchantConfigurationList));
    Page<MerchantConfiguration> merchantConfigurations =
        merchantConfigurationService.getMerchantConfigurationByUpdatedDateGreaterThan(DEFAULT_STORE_ID, DATE, PAGEABLE);
    Mockito.verify(merchantConfigurationRepository).findByStoreIdAndUpdatedDateGreaterThan(DEFAULT_STORE_ID, DATE, PAGEABLE);
    Assertions.assertEquals(merchantConfigurationList, merchantConfigurations.getContent());
  }

  @Test
  public void getMerchantConfigurationByUpdatedDateGreaterThanEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> merchantConfigurationService.getMerchantConfigurationByUpdatedDateGreaterThan(null, DATE, PAGEABLE));
  }

  @Test
  public void getMerchantConfigurationByUpdatedDateGreaterThanEmptyDateTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> merchantConfigurationService.getMerchantConfigurationByUpdatedDateGreaterThan(DEFAULT_STORE_ID, null, PAGEABLE));
  }

  @Test
  public void getMerchantConfigurationsPageTest() throws Exception {
    Mockito.when(this.categoryService.findByStoreIdAndCategoryCode(DEFAULT_STORE_ID, CATEGORY_CODE))
        .thenReturn(category);
    Mockito.when(this.merchantConfigurationRepository
        .findByReviewConfigAndCategoryNameAndKeywordMarkForDeleteFalseOrderByCreatedDate(DEFAULT_STORE_ID,
            Constants.NEUTRAL_STATUS, CATEGORY_NAME, KEYWORD, SORT_ORDER, pageable)).thenReturn(new PageImpl<>(
        Arrays.asList(merchantConfiguration), pageable, TOTAL_RECORDS));
    Page<MerchantConfiguration> merchantConfigurationPage = this.merchantConfigurationService
        .getMerchantConfigurationPage(DEFAULT_STORE_ID, configurationFilterRequest, pageable);
    Mockito.verify(this.merchantConfigurationRepository)
        .findByReviewConfigAndCategoryNameAndKeywordMarkForDeleteFalseOrderByCreatedDate(DEFAULT_STORE_ID,
            Constants.NEUTRAL_STATUS, CATEGORY_NAME, KEYWORD, SORT_ORDER, pageable);
    Mockito.verify(this.categoryService).findByStoreIdAndCategoryCode(DEFAULT_STORE_ID, CATEGORY_CODE);
    Assertions.assertEquals(CATEGORY_NAME, merchantConfigurationPage.getContent().get(0).getCategoryName());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, merchantConfigurationPage.getContent().get(0).getMerchantCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, merchantConfigurationPage.getContent().get(0).getMerchantName());
    Assertions.assertEquals(Constants.NEUTRAL_STATUS, merchantConfigurationPage.getContent().get(0).getReviewConfig());
    Assertions.assertEquals(CREATED_BY, merchantConfigurationPage.getContent().get(0).getCreatedBy());
    Assertions.assertNotNull(merchantConfigurationPage.getContent().get(0).getCreatedDate());
  }

  @Test
  public void getMerchantConfigurationsPageTest_expectException() throws Exception {
    Mockito.when(this.categoryService.findByStoreIdAndCategoryCode(DEFAULT_STORE_ID, CATEGORY_CODE)).thenReturn(null);
    try {
      Page<MerchantConfiguration> merchantConfigurationPage = this.merchantConfigurationService
          .getMerchantConfigurationPage(DEFAULT_STORE_ID, configurationFilterRequest, pageable);
    } catch (Exception e) {
      Mockito.verify(this.categoryService).findByStoreIdAndCategoryCode(DEFAULT_STORE_ID, CATEGORY_CODE);
    }
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.merchantConfigurationRepository);
    Mockito.verifyNoMoreInteractions(this.categoryService);
  }
}
