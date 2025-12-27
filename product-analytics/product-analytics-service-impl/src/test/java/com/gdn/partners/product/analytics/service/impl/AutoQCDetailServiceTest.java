package com.gdn.partners.product.analytics.service.impl;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.partners.product.analytics.entity.AutoQCDetail;
import com.gdn.partners.product.analytics.properties.ApplicationProperties;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.AutoQCRepository;
import com.gdn.partners.product.analytics.service.KafkaProducerService;
import com.gdn.partners.product.analytics.service.cache.AutoQCDetailCacheableService;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import com.mongodb.bulk.BulkWriteResult;


public class AutoQCDetailServiceTest {

  @InjectMocks
  AutoQCDetailServiceImpl autoQCDetailService;

  @Mock
  AutoQCDetailCacheableService autoQCDetailCacheableService;

  @Mock
  private AutoQCRepository autoQCRepository;

  @Mock
  private ApplicationProperties applicationProperties;

  @Mock
  private GCPProperties gcpProperties;

  @Mock
  private KafkaProducerService kafkaProducerService;

  @Captor
  private ArgumentCaptor<List<AutoQCDetail>> listArgumentCaptor;

  public static final String MERCHANT_CODE = "merchant_code";
  public static final String CATEGORY_CODE = "category_code";
  private AutoQCDetail autoQCDetail;
  private Date ACTIVATION_DATE = new Date();
  private SellerFieldsChangeResponse sellerFieldsChangeResponse;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    autoQCDetail = AutoQCDetail.builder().businessPartnerCode(MERCHANT_CODE).activationDate(ACTIVATION_DATE)
        .c1RejectionPercent((float) 12).c1Code(CATEGORY_CODE).cncActivated(true).c1ReviewApprovedCount365(12).build();
    sellerFieldsChangeResponse = new SellerFieldsChangeResponse();
    sellerFieldsChangeResponse.setSellerCode(MERCHANT_CODE);
    sellerFieldsChangeResponse.setCategoryCode(CATEGORY_CODE);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.autoQCDetailCacheableService);
    verifyNoMoreInteractions(applicationProperties);
    verifyNoMoreInteractions(gcpProperties);
    verifyNoMoreInteractions(autoQCRepository);
    verifyNoMoreInteractions(kafkaProducerService);
  }

  @Test
  void findByMerchantCodeAndCategoryCodeTest() throws Exception {
    when(autoQCDetailCacheableService.findCacheablesByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE))
        .thenReturn(autoQCDetail);
    autoQCDetailService.findByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE);
    verify(autoQCDetailCacheableService).findCacheablesByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE);
  }

  @Test
  void findByMerchantCodeAndCategoryCode_NullTest() throws Exception {
    when(autoQCDetailCacheableService.findCacheablesByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE))
        .thenReturn(null);
    autoQCDetailService.findByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE);
    verify(autoQCDetailCacheableService).findCacheablesByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE);
  }

  @Test
  void updateOfficialStoreFlagBySellerCodeTest() {
    autoQCDetail.setIsOfficialStore(true);
    when(autoQCRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(Collections.singletonList(autoQCDetail));
    when(gcpProperties.getBulkUpdateBatchSize()).thenReturn(5);
    autoQCDetailService.updateOfficialStoreFlagBySellerCode(MERCHANT_CODE, true);
    Mockito.verify(autoQCRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(gcpProperties).getBulkUpdateBatchSize();
  }

  @Test
  void updateOfficialStoreFlagBySellerCodeUpdatedTest() {
    autoQCDetail.setIsOfficialStore(false);
    when(autoQCRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(Collections.singletonList(autoQCDetail));
    when(gcpProperties.getBulkUpdateBatchSize()).thenReturn(5);
    when(applicationProperties.getChangeFieldList()).thenReturn(StringUtils.EMPTY);
    when(autoQCRepository.bulkWriteAutoQcDetail(Collections.singletonList(autoQCDetail), new ArrayList<>(),
        Collections.singletonList(StringUtils.EMPTY))).thenReturn(mock(BulkWriteResult.class));
    autoQCDetailService.updateOfficialStoreFlagBySellerCode(MERCHANT_CODE, true);
    Mockito.verify(autoQCRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(gcpProperties).getBulkUpdateBatchSize();
    Mockito.verify(applicationProperties).getChangeFieldList();
    Mockito.verify(autoQCRepository).bulkWriteAutoQcDetail(listArgumentCaptor.capture(), Mockito.eq(new ArrayList<>()),
        Mockito.eq(Collections.singletonList(StringUtils.EMPTY)));
    Assertions.assertTrue(listArgumentCaptor.getValue().get(0).getIsOfficialStore());
  }

  @Test
  public void clearCacheAndPublishEventTest() {
    autoQCDetailService.clearCacheAndPublishEvent(Collections.singletonList(sellerFieldsChangeResponse));
    Mockito.verify(autoQCDetailCacheableService).evictCacheByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE);
    Mockito.verify(kafkaProducerService).publishMessage(sellerFieldsChangeResponse);
  }
}
