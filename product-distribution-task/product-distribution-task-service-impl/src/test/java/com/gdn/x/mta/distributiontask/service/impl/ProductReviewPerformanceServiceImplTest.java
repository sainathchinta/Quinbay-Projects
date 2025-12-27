package com.gdn.x.mta.distributiontask.service.impl;

import java.util.Calendar;
import java.util.Date;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.mta.distributiontask.dao.api.ProductReviewPerformanceRepository;
import com.gdn.x.mta.distributiontask.model.ProductReviewPerformance;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

public class ProductReviewPerformanceServiceImplTest {

  private static final String DEFAULT_PRODUCT_CODE_1 = "MTA-0000001";
  private static final String DEFAULT_PRODUCT_CODE_2 = "MTA-0000002";

  @Mock
  private ProductReviewPerformanceRepository productReviewPerformanceRepository;

  @InjectMocks
  private ProductReviewPerformanceServiceImpl productReviewPerformanceServiceImpl;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.openMocks(this);
    ProductReviewPerformance productReviewPerformance = new ProductReviewPerformance();
    Mockito.when(
        this.productReviewPerformanceRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_1))).thenReturn(
        productReviewPerformance);
    Mockito.when(
        this.productReviewPerformanceRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.eq(ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_2))).thenReturn(null);
    Mockito.when(this.productReviewPerformanceRepository.saveAndFlush((ProductReviewPerformance) Mockito.any()))
        .thenReturn(null);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productReviewPerformanceRepository);
  }

  @Test
   void calculatePerformanceTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    this.productReviewPerformanceServiceImpl
        .calculatePerformance(ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_1, null, null,
            WorkflowState.IN_REVIEW, date, date);
    Mockito.verify(this.productReviewPerformanceRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(this.productReviewPerformanceRepository)
        .saveAndFlush((ProductReviewPerformance) Mockito.any());
  }

  @Test
   void calculatePerformanceWithImageApprovedWorkflowStateTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    this.productReviewPerformanceServiceImpl.calculatePerformance(
        ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_1, null, null, WorkflowState.PASSED,
        date, date);
    Mockito.verify(this.productReviewPerformanceRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(this.productReviewPerformanceRepository)
        .saveAndFlush((ProductReviewPerformance) Mockito.any());
  }

  @Test
   void calculatePerformanceWithContentApprovedWorkflowStateTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    this.productReviewPerformanceServiceImpl.calculatePerformance(
        ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_1, null, null, WorkflowState.PASSED,
        date, date);
    Mockito.verify(this.productReviewPerformanceRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(this.productReviewPerformanceRepository)
        .saveAndFlush((ProductReviewPerformance) Mockito.any());
  }

  @Test
   void calculatePerformanceWithPassedWorkflowStateTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    this.productReviewPerformanceServiceImpl.calculatePerformance(
        ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_1, null, null, WorkflowState.PASSED, date, date);
    Mockito.verify(this.productReviewPerformanceRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(this.productReviewPerformanceRepository)
        .saveAndFlush((ProductReviewPerformance) Mockito.any());
  }

  @Test
   void calculatePerformanceWithRejectedWorkflowStateTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    this.productReviewPerformanceServiceImpl.calculatePerformance(
        ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_1, null, null, WorkflowState.REJECTED, date, date);
    Mockito.verify(this.productReviewPerformanceRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_1));
    Mockito.verify(this.productReviewPerformanceRepository)
        .saveAndFlush((ProductReviewPerformance) Mockito.any());
  }

  @Test
   void calculatePerformance_2_Test() throws Exception {
    Date date = Calendar.getInstance().getTime();
    this.productReviewPerformanceServiceImpl
        .calculatePerformance(ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_2, null, null,
            WorkflowState.IN_REVIEW, date, date);
    Mockito.verify(this.productReviewPerformanceRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.eq(ProductReviewPerformanceServiceImplTest.DEFAULT_PRODUCT_CODE_2));
    Mockito.verify(this.productReviewPerformanceRepository)
        .saveAndFlush((ProductReviewPerformance) Mockito.any());
  }

}
