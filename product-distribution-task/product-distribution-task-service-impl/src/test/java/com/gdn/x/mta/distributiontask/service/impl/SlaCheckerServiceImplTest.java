package com.gdn.x.mta.distributiontask.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ProductPublisherService;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class SlaCheckerServiceImplTest {

  private static final String PRODUCT_ID = "prd_id";
  private static final String PRODUCT_CODE = "prd_code";
  private static final String TASK_ID = "task_id";
  private static final String TASK_CODE = "task_code";
  private static final String VENDOR_ID = "vendor_id";
  private static final String VENDOR_CODE = "vendor_code";
  private static final String USERNAME = "username";
  private static final String SYSTEM_USERNAME = "System";

  @InjectMocks
  private SlaCheckerServiceImpl instance;

  @Mock
  private ProductDistributionTaskService productDistributionTaskService;

  @Mock
  private ProductPublisherService productPublisherService;

  @Mock
  private ProductService productService;

  @Mock
  private TaskHistoryService taskHistoryService;

  private ProductDistributionTask productDistributionTask;

  private List<ProductDistributionTask> productDistributionTaskList;

  @BeforeEach
  public void setUp() throws Exception {
    Vendor vendor = new Vendor.Builder().id(VENDOR_ID).vendorCode(VENDOR_CODE).build();
    Product product = new Product.Builder().id(PRODUCT_ID).productCode(PRODUCT_CODE).build();
    this.productDistributionTask = new ProductDistributionTask(TASK_CODE, vendor, product,
        WorkflowState.IN_REVIEW, new Date());
    this.productDistributionTask.setProductId(product.getId());
    this.productDistributionTaskList =
        new ArrayList<ProductDistributionTask>(
            Collections.singletonList(this.productDistributionTask));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productPublisherService);
    Mockito.verifyNoMoreInteractions(this.productDistributionTaskService);
  }

  @Test
   void testExecute() throws Exception {
    Mockito
        .when(this.productDistributionTaskService
            .getListOfProductDistributionTaskSLA((Date) Mockito.any(), Mockito.anyBoolean()))
        .thenReturn(this.productDistributionTaskList);
    this.instance.execute();
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1))
        .getListOfProductDistributionTaskSLA((Date) Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.productPublisherService, Mockito.times(1))
        .publishEvent(this.productDistributionTask);
  }

  @Test
   void testExecuteNull() throws Exception {
    Mockito
        .when(this.productDistributionTaskService
            .getListOfProductDistributionTaskSLA((Date) Mockito.any(), Mockito.anyBoolean()))
        .thenReturn(null);
    this.instance.execute();
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1))
        .getListOfProductDistributionTaskSLA((Date) Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.productPublisherService, Mockito.times(0))
        .publishEvent(this.productDistributionTask);
  }

  @Test
   void testTurnTaskToDistributionList() throws Exception {
    Mockito
        .when(
            this.productDistributionTaskService.findByIdMarkForDeleteFalseAndStillInReview(TASK_ID))
        .thenReturn(this.productDistributionTask);
    this.instance.turnTaskToDistributionList(TASK_ID);
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1))
        .findByIdMarkForDeleteFalseAndStillInReview(TASK_ID);
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1))
        .removeProductDistributionTask(PRODUCT_ID, WorkflowState.EXCEEDED_SLA.name());
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1))
        .slaExceedUpdate(this.productDistributionTask);
    Mockito.verify(this.taskHistoryService, Mockito.times(1)).createTaskHistory(
        this.productDistributionTask.getStoreId(), SYSTEM_USERNAME,
        this.productDistributionTask.getProduct(),
        this.productDistributionTask.getVendor(), SlaCheckerServiceImpl.REJECTED_MESSAGE,
        WorkflowState.EXCEEDED_SLA, TASK_CODE);
  }

  @Test
   void testTurnTaskToDistributionListNotInReview() throws Exception {
    this.productDistributionTask.setState(WorkflowState.PASSED);
    Mockito
        .when(
            this.productDistributionTaskService.findByIdMarkForDeleteFalseAndStillInReview(TASK_ID))
        .thenReturn(this.productDistributionTask);
    this.instance.turnTaskToDistributionList(TASK_ID);
    Mockito.verify(this.productDistributionTaskService, Mockito.times(1))
        .findByIdMarkForDeleteFalseAndStillInReview(TASK_ID);
    Mockito.verify(this.productDistributionTaskService, Mockito.times(0))
        .removeProductDistributionTask(PRODUCT_ID, WorkflowState.EXCEEDED_SLA.name());
    Mockito.verify(this.productDistributionTaskService, Mockito.times(0))
        .slaExceedUpdate(this.productDistributionTask);
    Mockito.verify(this.taskHistoryService, Mockito.times(0)).createTaskHistory(
        this.productDistributionTask.getStoreId(), USERNAME, this.productDistributionTask.getProduct(),
        this.productDistributionTask.getVendor(), SlaCheckerServiceImpl.REJECTED_MESSAGE,
        WorkflowState.EXCEEDED_SLA, TASK_CODE);
  }

}
