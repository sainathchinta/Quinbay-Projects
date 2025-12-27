package com.gdn.partners.product.analytics.service.impl;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class SellerDataWrapperServiceImplTest {

  private static final String SELLER_CODE = "sellerCode";

  @InjectMocks
  private SellerDataWrapperServiceImpl sellerDataWrapperService;

  @Mock
  private AutoQCDetailServiceImpl autoQCDetailService;

  @Mock
  private SellerDetailServiceImpl sellerDetailService;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(autoQCDetailService);
    Mockito.verifyNoMoreInteractions(sellerDetailService);
  }

  @Test
  public void updateOfficialStoreFlagForASeller() {
    sellerDataWrapperService.updateOfficialStoreFlagForASeller(SELLER_CODE, true);
    Mockito.verify(autoQCDetailService).updateOfficialStoreFlagBySellerCode(SELLER_CODE, true);
    Mockito.verify(sellerDetailService).updateOfficialStoreFlagBySellerCode(SELLER_CODE, true);
  }
}