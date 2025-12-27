package com.gdn.partners.product.analytics.service.impl.BigQuery;


import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.product.analytics.entity.ProductAnalyticsProcess;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.repository.ProductAnalyticsProcessLockRepository;

import static org.mockito.ArgumentMatchers.eq;

public class ProductAnalyticsProcessLockServiceImplTest {

  @InjectMocks
  private ProductAnalyticsProcessLockServiceImpl productAnalyticsProcessLockService;

  @Mock
  private ProductAnalyticsProcessLockRepository productAnalyticsProcessLockRepository;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productAnalyticsProcessLockRepository);
  }

  @Test
  public void insertLock() {
    productAnalyticsProcessLockService.insertLock(Constants.SELLER_INFO_BQ_JOB, 10, new ProductAnalyticsProcess());
    Mockito.verify(productAnalyticsProcessLockRepository)
        .insertLock(eq(Constants.SELLER_INFO_BQ_JOB), Mockito.anyLong(), Mockito.any(ProductAnalyticsProcess.class));
  }

  @Test
  public void acquireLockIfReleased() {
    productAnalyticsProcessLockService
        .acquireLockIfReleased(Constants.SELLER_INFO_BQ_JOB, 10, new ProductAnalyticsProcess());
    Mockito.verify(productAnalyticsProcessLockRepository)
        .acquireLockIfReleased(eq(Constants.SELLER_INFO_BQ_JOB), Mockito.anyLong(),
            Mockito.any(ProductAnalyticsProcess.class));
  }

  @Test
  public void releaseLock() {
    productAnalyticsProcessLockService.releaseLock(Constants.SELLER_INFO_BQ_JOB, new ProductAnalyticsProcess());
    Mockito.verify(productAnalyticsProcessLockRepository)
        .releaseLock(eq(Constants.SELLER_INFO_BQ_JOB), Mockito.any(ProductAnalyticsProcess.class));
  }
}