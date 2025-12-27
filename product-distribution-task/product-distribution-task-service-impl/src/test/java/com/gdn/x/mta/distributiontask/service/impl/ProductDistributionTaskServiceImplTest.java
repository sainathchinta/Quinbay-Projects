package com.gdn.x.mta.distributiontask.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.MDC;

import com.gdn.partners.pdt.service.distribution.DistributionTaskService;
import com.gdn.x.mta.distributiontask.dao.api.ProductDistributionTaskRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;

/**
 * Created by Alok on 9/23/16.
 */

@ExtendWith(MockitoExtension.class)
public class ProductDistributionTaskServiceImplTest {

  private static final String TASK_ID = "ID1";
  private static final String PRODUCT_ID = "PRODUCTID";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String VENDOR_ID = "VENDORID";
  private static final String STORE_ID = "storeId";

  private final ProductDistributionTask productDistributionTask = new ProductDistributionTask();

  @InjectMocks
  private ProductDistributionTaskServiceImpl productDistributionTaskService;

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private TaskHistoryService taskHistoryService;

  @Mock
  private DistributionTaskService distributionTaskService;

  private List<ProductDistributionTask> createProductDistributionTaskList() {
    Date slaDate = new Date();
    ProductDistributionTask productDistributionTask =  new ProductDistributionTask("task_code", new Vendor(), new Product(),
        WorkflowState.IN_REVIEW, slaDate);
    productDistributionTask.setId(TASK_ID);
    List<ProductDistributionTask> productDistributionTaskList = new ArrayList<>();
    productDistributionTaskList.add(productDistributionTask);
    productDistributionTask =  new ProductDistributionTask("task_code", new Vendor(), new Product(),
        WorkflowState.IN_REVIEW, slaDate);
    productDistributionTask.setId(TASK_ID+2);
    productDistributionTaskList.add(productDistributionTask);
    return productDistributionTaskList;
  }

  @BeforeEach
  public void setUp() {
    MDC.put(STORE_ID, STORE_ID);
    productDistributionTask.setProductId(PRODUCT_ID);
  }

  @Test
   void testFindById() {
    productDistributionTaskService.findByIdMarkForDeleteFalseAndStillInReview(TASK_ID);
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
        .getProductDistributionTaskByIdAndStillInReview(TASK_ID);
  }

  @Test
   void testGetListOfProductDistributionTaskSLA() throws Exception {
    productDistributionTaskService.getListOfProductDistributionTaskSLA(new Date(), true);
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
        .getListOfProductDistributionTaskSLA((Date)any(),anyBoolean());
  }

  @Test
   void testSlaExceedUpdate() throws Exception {
    Mockito.when(this.productDistributionTaskRepository.findById(Mockito.anyString())).thenReturn(Optional.of(createProductDistributionTaskList().get(0)));
    productDistributionTaskService.slaExceedUpdate(createProductDistributionTaskList().get(0));
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
    .findById(Mockito.anyString());
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
        .saveAndFlush((ProductDistributionTask)any());
  }

  @Test
  void testSlaExceedUpdateException() throws Exception {
    ProductDistributionTask productDistributionTask = createProductDistributionTaskList().get(0);
    productDistributionTask.setId(null);
    Assertions.assertThrows(Exception.class,
        () -> productDistributionTaskService.slaExceedUpdate(productDistributionTask));
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(0))
        .findById(Mockito.anyString());
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(0))
        .saveAndFlush((ProductDistributionTask) any());
  }

  @Test
   void testSlaExceedUpdateException2() throws Exception {
    ProductDistributionTask productDistributionTask = createProductDistributionTaskList().get(0);
    Mockito.when(this.productDistributionTaskRepository.findById(Mockito.anyString())).thenReturn(null);
    Assertions.assertThrows(Exception.class,
      () -> productDistributionTaskService.slaExceedUpdate(productDistributionTask));
      Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
      .findById(Mockito.anyString());
      Mockito.verify(this.productDistributionTaskRepository, Mockito.times(0))
          .saveAndFlush((ProductDistributionTask)any());
  }

  @Test
   void testFindByProductId() throws Exception {
    productDistributionTaskService.findByProductId(PRODUCT_ID);
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
        .getProductDistributionTaskByProduct(PRODUCT_ID);
  }

  @Test
   void testGetWorkflowStatePostApproval() throws Exception {
    ProductDistributionTask productDistributionTask = createProductDistributionTaskList().get(0);
    productDistributionTask.getVendor().setId(VENDOR_ID);
    productDistributionTask.setVendorId(productDistributionTask.getVendor().getId());
    Mockito.when(productDistributionTaskRepository.getProductDistributionTaskByProduct(PRODUCT_ID))
        .thenReturn(productDistributionTask);
    productDistributionTaskService.getWorkflowStatePostApproval(PRODUCT_ID, VENDOR_ID);
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
        .save((ProductDistributionTask) any());
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
    .getProductDistributionTaskByProduct(PRODUCT_ID);
  }

  @Test
   void testGetWorkflowStatePostApprovalException() throws Exception {
    ProductDistributionTask productDistributionTask = createProductDistributionTaskList().get(0);
    productDistributionTask.getVendor().setId(VENDOR_ID);
    productDistributionTask.setVendorId(productDistributionTask.getVendor().getId());
    Mockito.when(productDistributionTaskRepository.getProductDistributionTaskByProduct(PRODUCT_ID))
      .thenThrow(RuntimeException.class);
    Assertions.assertThrows(RuntimeException.class,
      () -> productDistributionTaskService.getWorkflowStatePostApproval(PRODUCT_ID, VENDOR_ID));
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(0))
      .save((ProductDistributionTask) any());
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
      .getProductDistributionTaskByProduct(PRODUCT_ID);
  }

  @Test
   void findTopByProductProductCodeOrderByUpdatedDateDescTest() {
    productDistributionTaskService.findTopByProductProductCodeOrderByUpdatedDateDesc(PRODUCT_CODE);
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
        .findTopByProductProductCodeOrderByUpdatedDateDesc(PRODUCT_CODE);
  }


  @Test public void movingProductsbackToProductDistributionTestOk() throws Exception {
    Vendor vendor = new Vendor.Builder().vendorCode("vendorCode").id("ID").name("name").build();
    productDistributionTaskService.movingProductsbackToProductDistribution(vendor);
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
        .clearProductsInProductDistributionTask(Mockito.anyString());
    Mockito.verify(this.productRepository, Mockito.times(1))
        .clearVendorForProduct(Mockito.anyString());
  }

  @Test
   void movingProductsbackToProductDistributionTestNotOk() {
    Vendor vendor = new Vendor.Builder().vendorCode("").id("ID").name("name").build();
    Mockito.doThrow(RuntimeException.class).when(this.productDistributionTaskRepository)
        .clearProductsInProductDistributionTask(Mockito.anyString());
    Assertions.assertThrows(RuntimeException.class,
        () -> productDistributionTaskService.movingProductsbackToProductDistribution(vendor));
    Mockito.verify(productDistributionTaskRepository).clearProductsInProductDistributionTask(Mockito.anyString());
  }

  @Test
   void updateStateTest() {
    Mockito.when(productDistributionTaskRepository.findById(createProductDistributionTaskList().get(0).getId())).thenReturn(Optional.of(createProductDistributionTaskList().get(0)));
    productDistributionTaskService.updateState(createProductDistributionTaskList().get(0), WorkflowState.PASSED);
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
        .saveAndFlush(createProductDistributionTaskList().get(0));
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
    .findById(createProductDistributionTaskList().get(0).getId());
  }

  @Test
  public void updateState_nullTest() {
    productDistributionTaskService.updateState(null, WorkflowState.PASSED);
  }


  @Test
   void removeProductDistributionTaskTest() throws Exception {
    Mockito.when(
        productDistributionTaskRepository.getProductDistributionTaskByProduct(createProductDistributionTaskList().get(0)
            .getId())).thenReturn(createProductDistributionTaskList().get(0));
    productDistributionTaskService.removeProductDistributionTask(
        createProductDistributionTaskList().get(0).getId(), "PASSED");
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1)).save(
        any(ProductDistributionTask.class));
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
        .getProductDistributionTaskByProduct(createProductDistributionTaskList().get(0).getId());
  }

  @Test
   void removeProductDistributionTask_exception_Test() throws Exception {
    Mockito.when(productDistributionTaskRepository.getProductDistributionTaskByProduct(
      createProductDistributionTaskList().get(0).getId())).thenReturn(null);
    Assertions.assertThrows(Exception.class,
      () -> productDistributionTaskService.removeProductDistributionTask(
        createProductDistributionTaskList().get(0).getId(), "PASSED"));
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(0))
      .save(createProductDistributionTaskList().get(0));
    Mockito.verify(this.productDistributionTaskRepository, Mockito.times(1))
      .getProductDistributionTaskByProduct(createProductDistributionTaskList().get(0).getId());
  }

  @Test
   void clearPresentDistributionTaskAndCreateNewTaskTest() throws Exception {
    productDistributionTaskService.clearPresentDistributionTaskAndCreateNewTask(new Product());
    Mockito.verify(this.distributionTaskService).clearPresentDistributionTaskAndCreateNewTask(STORE_ID, new Product());
  }

  @Test
   void findByProductIdInAllProductsTest() {
    Mockito.when(this.productDistributionTaskRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_ID)).thenReturn(Collections.singletonList(productDistributionTask));
    List<ProductDistributionTask> productDistributionTask =
      this.productDistributionTaskService.findStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID);
    Mockito.verify(this.productDistributionTaskRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_ID);
    Assertions.assertEquals(PRODUCT_ID, productDistributionTask.get(0).getProductId());
  }

  @Test
   void findByProductIdInAllProducts_emptyStoreIdTest() {
    Assertions.assertThrows(Exception.class,
      () ->  this.productDistributionTaskService.findStoreIdAndProductIdAndMarkForDeleteFalse(StringUtils.EMPTY, PRODUCT_ID));
  }

  @Test
   void findByProductIdInAllProducts_emptyProductIdTest() {
    Assertions.assertThrows(Exception.class,
      () -> this.productDistributionTaskService.findStoreIdAndProductIdAndMarkForDeleteFalse(STORE_ID, StringUtils.EMPTY));
  }

  @Test
   void saveProductDistributionTaskListTest() {
    productDistributionTaskService.saveProductDistributionTaskList(
      Collections.singletonList(productDistributionTask));
    Mockito.verify(this.productDistributionTaskRepository)
      .saveAll(Collections.singletonList(productDistributionTask));
  }

  @Test
   void saveProductDistributionTaskList_emptyInputTest() {
    Assertions.assertThrows(Exception.class,
      () -> productDistributionTaskService.saveProductDistributionTaskList(Collections.emptyList()));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.productDistributionTaskRepository);
    Mockito.verifyNoMoreInteractions(this.productRepository);
    Mockito.verifyNoMoreInteractions(this.taskHistoryService);
    Mockito.verifyNoMoreInteractions(this.distributionTaskService);
  }
}
