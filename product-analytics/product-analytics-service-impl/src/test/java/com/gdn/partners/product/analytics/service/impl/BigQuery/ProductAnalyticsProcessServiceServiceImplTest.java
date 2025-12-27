package com.gdn.partners.product.analytics.service.impl.BigQuery;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.partners.product.analytics.entity.ProductAnalyticsProcess;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.enums.ProductAnalyticsProcessStatus;
import com.gdn.partners.product.analytics.properties.SubmitBigQueryProcessLockProperties;
import com.gdn.partners.product.analytics.repository.ProductAnalyticsProcessRepository;
import com.gdn.partners.product.analytics.service.bigQuery.ProductAnalyticsProcessLockService;
import org.mockito.MockitoAnnotations;

public class ProductAnalyticsProcessServiceServiceImplTest {

  @Mock
  private SubmitBigQueryProcessLockProperties submitBigQueryProcessLockProperties;

  @Mock
  private ProductAnalyticsProcessLockService productAnalyticsProcessLockService;

  @Mock
  private ProductAnalyticsProcessRepository productAnalyticsProcessRepository;

  @InjectMocks
  private ProductAnalyticsProcessServiceServiceImpl submitBigQueryProcessHelper;

  @Captor
  private ArgumentCaptor<ProductAnalyticsProcess> productAnalyticsProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<Long> lockTimeArgumentCaptor;

  @Mock
  private ProductAnalyticsProcess productAnalyticsProcess;

  private static final String PROCESS_ID = "processId";
  private static final long LOCK_TIME = 1;
  private static final TimeUnit LOCK_TIME_UNIT = TimeUnit.HOURS;
  private static final long LOCK_TIME_IN_MILLIS = LOCK_TIME_UNIT.toMillis(LOCK_TIME);
  private static final String UPDATED_STATUS = "status1";
  private static final String UPDATED_STATE = "state1";

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    productAnalyticsProcess = ProductAnalyticsProcess.builder().id(PROCESS_ID).build();
  }

  @Test
  public void createProcessTest() {
    when(productAnalyticsProcessRepository.save(any(ProductAnalyticsProcess.class))).thenReturn(productAnalyticsProcess);
    submitBigQueryProcessHelper.createProcess();
    verify(productAnalyticsProcessRepository).save(productAnalyticsProcessArgumentCaptor.capture());
    assertEquals(productAnalyticsProcessArgumentCaptor.getValue().getStatus(), ProductAnalyticsProcessStatus.CREATED.getStatus());
  }

  @Test
  public void acquireLock_Insert_Test() {
    mockSubmitBigQueryProcessLockProperties();
    when(productAnalyticsProcessLockService.insertLock(anyString(), anyLong(), any(ProductAnalyticsProcess.class)))
        .thenReturn(true);
    submitBigQueryProcessHelper.acquireLock(productAnalyticsProcess, Constants.SELLER_INFO_BQ_JOB);
    verify(submitBigQueryProcessLockProperties).getLockTime();
    verify(submitBigQueryProcessLockProperties).getLockTimeUnit();
    verify(productAnalyticsProcessLockService).insertLock(eq(Constants.SELLER_INFO_BQ_JOB),
        lockTimeArgumentCaptor.capture(), eq(productAnalyticsProcess));
    assertEquals(LOCK_TIME_IN_MILLIS, lockTimeArgumentCaptor.getValue().longValue());
  }

  @Test
  public void acquireLock_Update_Test() {
    mockSubmitBigQueryProcessLockProperties();
    when(productAnalyticsProcessLockService.insertLock(anyString(), anyLong(), any(ProductAnalyticsProcess.class)))
        .thenReturn(false);
    when(productAnalyticsProcessLockService.acquireLockIfReleased(anyString(), anyLong(), any(ProductAnalyticsProcess.class)))
        .thenReturn(true);
    submitBigQueryProcessHelper.acquireLock(productAnalyticsProcess, Constants.SELLER_INFO_BQ_JOB);
    verify(submitBigQueryProcessLockProperties).getLockTime();
    verify(submitBigQueryProcessLockProperties).getLockTimeUnit();
    verify(productAnalyticsProcessLockService).insertLock(eq(Constants.SELLER_INFO_BQ_JOB),
        lockTimeArgumentCaptor.capture(), eq(productAnalyticsProcess));
    verify(productAnalyticsProcessLockService).acquireLockIfReleased(eq(Constants.SELLER_INFO_BQ_JOB),
        lockTimeArgumentCaptor.capture(), eq(productAnalyticsProcess));
    assertEquals(LOCK_TIME_IN_MILLIS, lockTimeArgumentCaptor.getValue().longValue());
  }

  @Test
  public void acquireLock_FailedTest() {
    mockSubmitBigQueryProcessLockProperties();
    when(productAnalyticsProcessLockService.insertLock(anyString(), anyLong(), any(ProductAnalyticsProcess.class)))
        .thenReturn(false);
    when(productAnalyticsProcessLockService.acquireLockIfReleased(anyString(), anyLong(), any(ProductAnalyticsProcess.class)))
        .thenReturn(false);
    boolean acquireLock = submitBigQueryProcessHelper.acquireLock(productAnalyticsProcess, Constants.SELLER_INFO_BQ_JOB);
    verify(submitBigQueryProcessLockProperties).getLockTime();
    verify(submitBigQueryProcessLockProperties).getLockTimeUnit();
    verify(productAnalyticsProcessLockService).insertLock(eq(Constants.SELLER_INFO_BQ_JOB),
        lockTimeArgumentCaptor.capture(), eq(productAnalyticsProcess));
    verify(productAnalyticsProcessLockService).acquireLockIfReleased(eq(Constants.SELLER_INFO_BQ_JOB),
        lockTimeArgumentCaptor.capture(), eq(productAnalyticsProcess));
    assertFalse(acquireLock);
  }

  @Test
  public void releaseLockTest() {
    submitBigQueryProcessHelper.releaseLock(productAnalyticsProcess, Constants.SELLER_INFO_BQ_JOB);
    verify(productAnalyticsProcessLockService).releaseLock(eq(Constants.SELLER_INFO_BQ_JOB),
        productAnalyticsProcessArgumentCaptor.capture());
    assertEquals(productAnalyticsProcess, productAnalyticsProcessArgumentCaptor.getValue());
  }

  @Test
  public void updateProcessStatusTest() {
    submitBigQueryProcessHelper.updateProcessStatus(productAnalyticsProcess, UPDATED_STATUS);
    verify(productAnalyticsProcessRepository).save(productAnalyticsProcessArgumentCaptor.capture());
    assertEquals(UPDATED_STATUS, productAnalyticsProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void updateCurrentStateTest() {
    submitBigQueryProcessHelper.updateCurrentState(productAnalyticsProcess, UPDATED_STATE);
    verify(productAnalyticsProcessRepository).save(productAnalyticsProcessArgumentCaptor.capture());
    assertEquals(UPDATED_STATE, productAnalyticsProcessArgumentCaptor.getValue().getCurrentState());
  }

  private void mockSubmitBigQueryProcessLockProperties() {
    when(submitBigQueryProcessLockProperties.getLockTime()).thenReturn(LOCK_TIME);
    when(submitBigQueryProcessLockProperties.getLockTimeUnit()).thenReturn(LOCK_TIME_UNIT);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(submitBigQueryProcessLockProperties);
    verifyNoMoreInteractions(productAnalyticsProcessRepository);
    verifyNoMoreInteractions(productAnalyticsProcessLockService);
  }
}