package com.gdn.partners.product.analytics.service.impl.cache;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.mockito.MockitoAnnotations.openMocks;

import com.gdn.common.exception.ApplicationRuntimeException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.partners.product.analytics.entity.AutoQCDetail;
import com.gdn.partners.product.analytics.repository.AutoQCRepository;

public class AutoQCDetailCacheableServiceTest {

  @InjectMocks
  AutoQCDetailCacheableServiceImpl autoQCDetailCacheableService;

  @Mock
  AutoQCRepository autoQCRepository;

  public static final String MERCHANT_CODE = "merchant_code";
  public static final String CATEGORY_CODE = "category_code";
  private AutoQCDetail autoQCDetail = new AutoQCDetail();

  @BeforeEach
  public void setUp() {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.autoQCRepository);
  }

  @Test
  public void findCacheablesByMerchantCodeAndCategoryCode() throws Exception {
    when(autoQCRepository.findByBusinessPartnerCodeAndC1Code(MERCHANT_CODE, CATEGORY_CODE)).thenReturn(autoQCDetail);
    autoQCDetailCacheableService.findCacheablesByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE);
    verify(autoQCRepository).findByBusinessPartnerCodeAndC1Code(MERCHANT_CODE, CATEGORY_CODE);
  }

  @Test
  public void findCacheablesByMerchantCodeAndCategoryCode_Null() throws Exception {
    try {
      autoQCDetailCacheableService.findCacheablesByMerchantCodeAndCategoryCode(null, CATEGORY_CODE);
    }
    catch (ApplicationRuntimeException e){
      Assertions.assertNotNull(e.getErrorMessage(),"merchant code must not be blank");
    }
  }

  @Test
  public void findCacheablesByMerchantCodeAndCategoryCode_Null2() throws Exception {
    try {
      autoQCDetailCacheableService.findCacheablesByMerchantCodeAndCategoryCode(null, CATEGORY_CODE);
    }
    catch (ApplicationRuntimeException e){
      Assertions.assertNotNull(e.getErrorMessage(),"category code must not be blank");
    }
  }

  @Test
  public void evictCacheByMerchantCodeAndCategoryCodeTest() {
    autoQCDetailCacheableService.evictCacheByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE);
  }
}
