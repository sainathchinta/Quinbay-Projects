package com.gdn.x.mta.distributiontask.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

import com.gdn.x.mta.distributiontask.domain.event.InternalHistoryEventModel;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.TaskHistoryRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTHistoryEventModel;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

@ExtendWith(MockitoExtension.class)
public class TaskHistoryServiceImplTest {

  private static final String STORE_ID = "STORE_ID";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final String CATEGORY_CODE = "CATEGORY_CODES";
  private static final String USERNAME = "USERNAME";
  private static final String CATEGORY_NAME = "CATEGORY_NAME";
  private static final String REASON = "REASON";
  private static final String TASK_CODE = "TASK_CODE";

  @InjectMocks
  private TaskHistoryServiceImpl instance;

  @Mock
  private TaskHistoryRepository taskHistoryRepository;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private KafkaPublisher kafkaProducer;

  private Product product;

  private Vendor vendor;

  @BeforeEach
  public void setUp() throws Exception {
    this.product = new Product();
    this.product.setProductCode(PRODUCT_CODE);
    this.product.setProductName(PRODUCT_NAME);
    this.product.setCategoryCode(CATEGORY_CODE);
    this.product.setCategoryName(CATEGORY_NAME);

    this.vendor = new Vendor();

  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.taskHistoryRepository);
    Mockito.verifyNoMoreInteractions(this.kafkaProducer);
    Mockito.verifyNoMoreInteractions(this.kafkaTopicProperties);
  }

  @Test
   void testCreateTaskHistoryStringStringProductVendorStringWorkflowState() throws Exception {
    Mockito.when(kafkaTopicProperties.getInternalHistoryEventName()).thenReturn("EVENT");
    this.instance.createTaskHistory(STORE_ID, USERNAME, this.product, this.vendor, REASON,
        WorkflowState.IN_REVIEW, TASK_CODE);
    Mockito.verify(this.taskHistoryRepository, Mockito.times(1))
        .save((TaskHistory) Mockito.any());
    Mockito.verify(kafkaTopicProperties).getInternalHistoryEventName();
    Mockito.verify(kafkaProducer).send(anyString(), anyString(), any(InternalHistoryEventModel.class));
  }

  @Test
   void testGetHistoryFromProductCode() throws Exception {
    this.instance.getHistoryFromProductCode(STORE_ID, PRODUCT_CODE,PageRequest.of(0, 10));
    Mockito.verify(this.taskHistoryRepository, Mockito.times(1))
        .findAllByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), (Pageable) Mockito.any());
  }

  @Test
   void testGetHistoryFromTaskCode() throws Exception {
    this.instance.getHistoryFromTaskCode(STORE_ID, TASK_CODE, PageRequest.of(0, 10));
    Mockito.verify(this.taskHistoryRepository, Mockito.times(1))
        .findAllByStoreIdAndTaskCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), (Pageable) Mockito.any());
  }

  @Test
   void generatePDTHistoryEventModelTest() {
    PDTHistoryEventModel pdtHistoryEventModel = this.instance
        .generatePDTHistoryEventModel(STORE_ID, USERNAME, product, vendor, REASON, WorkflowState.IN_REVIEW, TASK_CODE);
    Assertions.assertNotNull(pdtHistoryEventModel);
    Assertions.assertEquals(USERNAME, pdtHistoryEventModel.getExecutor());
    Assertions.assertEquals(REASON, pdtHistoryEventModel.getReason());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, pdtHistoryEventModel.getState());
    Assertions.assertEquals(TASK_CODE, pdtHistoryEventModel.getTaskCode());
  }

  @Test
   void testCreateTaskHistoryWithEventSwitch() throws Exception {
    ReflectionTestUtils.setField(instance, "pdtHistoryUpdateThroughEvent", true);
    Mockito.when(kafkaTopicProperties.getInternalHistoryEventName()).thenReturn("EVENT");
    this.instance
        .createTaskHistory(STORE_ID, USERNAME, this.product, this.vendor, REASON, WorkflowState.IN_REVIEW, TASK_CODE);
    Mockito.verify(kafkaTopicProperties).getInternalHistoryEventName();
    Mockito.verify(kafkaProducer).send(anyString(), anyString(), any(InternalHistoryEventModel.class));
  }

}
