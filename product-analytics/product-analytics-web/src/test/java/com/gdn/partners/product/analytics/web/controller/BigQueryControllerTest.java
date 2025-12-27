package com.gdn.partners.product.analytics.web.controller;

import static org.mockito.MockitoAnnotations.initMocks;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.partners.product.analytics.model.enums.JobProcessTypes;
import com.gdn.partners.product.analytics.service.bigQuery.DownloadSellerInfoBigQueryService;

public class BigQueryControllerTest {

  @InjectMocks
  private BigQueryController bigQueryController;

  @Mock
  private DownloadSellerInfoBigQueryService downloadSellerInfoBigQueryService;

  @BeforeEach
  public void setUp() {
    initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(downloadSellerInfoBigQueryService);
  }

  @Test
  void updateProductImageFeedback() {
    Mockito.doNothing().when(downloadSellerInfoBigQueryService)
      .execute(Mockito.anyString(), Mockito.eq(JobProcessTypes.SELLER_INFO_BQ_JOB.name()),
        Mockito.eq(24), Mockito.eq(24), Mockito.eq(StringUtils.EMPTY));
    bigQueryController.updateSellerQCData("10000", JobProcessTypes.SELLER_INFO_BQ_JOB.name(), 24,
      24, StringUtils.EMPTY);
    Mockito.verify(downloadSellerInfoBigQueryService)
      .execute(Mockito.anyString(), Mockito.eq(JobProcessTypes.SELLER_INFO_BQ_JOB.name()),
        Mockito.eq(24),  Mockito.eq(24), Mockito.eq(StringUtils.EMPTY));
  }
}