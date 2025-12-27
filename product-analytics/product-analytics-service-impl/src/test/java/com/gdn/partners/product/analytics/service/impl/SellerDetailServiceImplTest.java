package com.gdn.partners.product.analytics.service.impl;

import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.product.analytics.entity.SellerQCDetail;
import com.gdn.partners.product.analytics.repository.SellerQCDetailRepository;
import com.gdn.partners.product.analytics.service.cache.SellerDetailCacheableService;

public class SellerDetailServiceImplTest {

  private static final String MERCHANT_CODE = "merchant_code";
  private SellerQCDetail sellerQCDetail;

  @InjectMocks
  private SellerDetailServiceImpl sellerDetailService;

  @Mock
  private SellerDetailCacheableService sellerDetailCacheableService;

  @Mock
  private SellerQCDetailRepository sellerQCDetailRepository;

  @Captor
  private ArgumentCaptor<List<SellerQCDetail>> sellerQCDetailArgumentCaptor;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    sellerQCDetail = new SellerQCDetail();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(sellerDetailCacheableService);
    Mockito.verifyNoMoreInteractions(sellerQCDetailRepository);
  }

  @Test
  public void findSellerDetailByMerchantCodeTest() throws Exception {
    sellerDetailService.findSellerDetailByMerchantCode(MERCHANT_CODE);
    Mockito.verify(sellerDetailCacheableService).findCacheablesByMerchantCode(MERCHANT_CODE);
  }

  @Test
  public void updateOfficialStoreFlagBySellerCodeNullTest() {
    sellerDetailService.updateOfficialStoreFlagBySellerCode(MERCHANT_CODE, false);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void updateOfficialStoreFlagBySellerCodeFlagNullTest() {
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    sellerDetailService.updateOfficialStoreFlagBySellerCode(MERCHANT_CODE, false);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void updateOfficialStoreFlagBySellerCodeNoChangeTest() {
    sellerQCDetail.setIsOfficialStore(false);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    sellerDetailService.updateOfficialStoreFlagBySellerCode(MERCHANT_CODE, false);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void updateOfficialStoreFlagBySellerCodeTest() {
    sellerQCDetail.setIsOfficialStore(true);
    Mockito.when(sellerQCDetailRepository.findByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(sellerQCDetail);
    sellerDetailService.updateOfficialStoreFlagBySellerCode(MERCHANT_CODE, false);
    Mockito.verify(sellerQCDetailRepository).findByBusinessPartnerCode(MERCHANT_CODE);
    Mockito.verify(sellerQCDetailRepository).bulkWriteSellerQcDetail(sellerQCDetailArgumentCaptor.capture());
    Mockito.verify(sellerDetailCacheableService).evictCacheByMerchantCode(MERCHANT_CODE);
    Assertions.assertFalse(sellerQCDetailArgumentCaptor.getValue().get(0).getIsOfficialStore());
  }
}