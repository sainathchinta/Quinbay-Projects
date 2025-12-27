package com.gdn.x.productcategorybase.service.impl;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;

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

import com.gdn.x.productcategorybase.entity.MerchantConfigurationHistory;
import com.gdn.x.productcategorybase.repository.MerchantConfigurationHistoryRepository;

public class MerchantConfigurationHistoryServiceImplTest {

  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT = "Neutral";
  private static final String ACTIVITY = "Registered";
  private static final String DEFAULT_STORE_ID = "10001";
  private Pageable pageable = PageRequest.of(1, 50);
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final Long TOTAL_RECORDS = Long.valueOf(1);

  private MerchantConfigurationHistory merchantConfigurationHistory;
  private Page<MerchantConfigurationHistory> merchantConfigurationHistoryPage;

  @InjectMocks
  private MerchantConfigurationHistoryServiceImpl merchantConfigurationHistoryService;

  @Mock
  private MerchantConfigurationHistoryRepository merchantConfigurationHistoryRepository;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);

    merchantConfigurationHistory = new MerchantConfigurationHistory();
    merchantConfigurationHistory.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationHistory.setNewValue(DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT);
    merchantConfigurationHistory.setActivity(ACTIVITY);

    pageable = PageRequest.of(PAGE, SIZE);

    merchantConfigurationHistoryPage = new PageImpl<>(Arrays.asList(merchantConfigurationHistory), pageable,
        Arrays.asList(merchantConfigurationHistory).size());
  }

  @Test
  public void saveMerchantHistoryEntitiesTest() throws Exception {
    this.merchantConfigurationHistoryService.saveMerchantHistoryConfigurations(Arrays.asList(merchantConfigurationHistory));
    Mockito.verify(this.merchantConfigurationHistoryRepository)
        .saveAll(Collections.singletonList(merchantConfigurationHistory));
  }

  @Test
  public void saveMerchantHistoryEntityTest() throws Exception {
    this.merchantConfigurationHistoryService.saveMerchantHistoryConfiguration(merchantConfigurationHistory);
    Mockito.verify(this.merchantConfigurationHistoryRepository).save(merchantConfigurationHistory);
  }

  @Test
  public void getMerchantConfigurationByCreatedDateTest() {
    Date date = new Date();
    Mockito.when(this.merchantConfigurationHistoryRepository
        .findByStoreIdAndCreatedDateGreaterThanOrderByCreatedDateDesc(DEFAULT_STORE_ID, date, pageable))
        .thenReturn(merchantConfigurationHistoryPage);
    this.merchantConfigurationHistoryService.getMerchantConfigurationByCreatedDate(DEFAULT_STORE_ID, date, pageable);
    Mockito.verify(this.merchantConfigurationHistoryRepository)
        .findByStoreIdAndCreatedDateGreaterThanOrderByCreatedDateDesc(DEFAULT_STORE_ID, date, pageable);
  }

  @Test
  public void getMerchantConfigurationHistoryTest() {
    Mockito.when(this.merchantConfigurationHistoryRepository
        .findByStoreIdAndMerchantCodeAndMarkForDeleteFalseOrderByCreatedDateDesc(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(merchantConfigurationHistory), pageable, TOTAL_RECORDS));
    Page<MerchantConfigurationHistory> merchantConfigurationHistoryPage = this.merchantConfigurationHistoryService
        .getMerchantConfigurationHistoryPage(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable);
    Mockito.verify(this.merchantConfigurationHistoryRepository)
        .findByStoreIdAndMerchantCodeAndMarkForDeleteFalseOrderByCreatedDateDesc(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, pageable);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        merchantConfigurationHistoryPage.getContent().get(0).getMerchantCode());
    Assertions.assertEquals(DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT,
        merchantConfigurationHistoryPage.getContent().get(0).getNewValue());
    Assertions.assertEquals(ACTIVITY, merchantConfigurationHistoryPage.getContent().get(0).getActivity());
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.merchantConfigurationHistoryRepository);
  }
}
