package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.ItemService;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.service.partners.pbp.distributiontask.ProductDistributionTaskQCService;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductVendorApprovedEventModel;

public class ProductDistributionTaskVendorApprovalKafkaConsumerBeanTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String USERNAME = "username";
  private ObjectMapper mapper;

  @InjectMocks
  private ProductDistributionTaskVendorApprovalKafkaConsumerBean productDistributionTaskVendorApprovalKafkaConsumerBean;

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private ItemService itemService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductDistributionTaskQCService productDistributionTaskQCService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private PDTProductDomainEventModel pdtProductDomainEventModel;
  private PDTProductVendorApprovedEventModel pdtProductVendorApprovedEventModel;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    pdtProductDomainEventModel = new PDTProductDomainEventModel();
    pdtProductDomainEventModel.setProductCode(PRODUCT_CODE);
    pdtProductVendorApprovedEventModel = new PDTProductVendorApprovedEventModel();
    pdtProductVendorApprovedEventModel.setProductCode(PRODUCT_CODE);
    mapper = new ObjectMapper();
  }

  @Test
  public void listenWithNullObject() throws Exception {
    String message = mapper.writeValueAsString(null);
    Mockito.when(objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class))
        .thenReturn(null);
    this.productDistributionTaskVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTProductVendorApprovedEventModel.class);
  }

  @Test
  public void listen() throws Exception {
    pdtProductVendorApprovedEventModel.setPostLive(true);
    pdtProductVendorApprovedEventModel.setReviewPending(true);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    this.productDistributionTaskVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processProductDistributionTaskQCKafkaConsumer(pdtProductDomainEventModel, 0);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(itemService).publishItemStatusEvent(PRODUCT_CODE, ProductStatus.ACTIVE);
  }

  @Test
  public void onDomainEventConsumedPriority1Test() throws Exception {
    pdtProductVendorApprovedEventModel.setPostLive(true);
    pdtProductVendorApprovedEventModel.setReviewPending(true);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    this.productDistributionTaskVendorApprovalKafkaConsumerBean.onDomainEventConsumedPriority1(message);
    verify(objectMapper).readValue(message, PDTProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processProductDistributionTaskQCKafkaConsumer(pdtProductDomainEventModel, 1);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(itemService).publishItemStatusEvent(PRODUCT_CODE, ProductStatus.ACTIVE);
  }

  @Test
  public void onDomainEventConsumedPriority2Test() throws Exception {
    pdtProductVendorApprovedEventModel.setPostLive(true);
    pdtProductVendorApprovedEventModel.setReviewPending(true);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    this.productDistributionTaskVendorApprovalKafkaConsumerBean.onDomainEventConsumedPriority2(message);
    verify(objectMapper).readValue(message, PDTProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processProductDistributionTaskQCKafkaConsumer(pdtProductDomainEventModel,2);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(itemService).publishItemStatusEvent(PRODUCT_CODE, ProductStatus.ACTIVE);
  }

  @Test
  public void listen_2() throws Exception {
    pdtProductVendorApprovedEventModel.setPostLive(false);
    pdtProductVendorApprovedEventModel.setReviewPending(false);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    pdtProductVendorApprovedEventModel.setUpdatedBy(USERNAME);
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    this.productDistributionTaskVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processProductDistributionTaskQCKafkaConsumer(pdtProductDomainEventModel, 0);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(itemService).publishItemStatusEvent(PRODUCT_CODE, ProductStatus.ACTIVE);
  }

  @Test
  public void listen_3() throws Exception {
    pdtProductVendorApprovedEventModel.setPostLive(true);
    pdtProductVendorApprovedEventModel.setReviewPending(false);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    this.productDistributionTaskVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processProductDistributionTaskQCKafkaConsumer(pdtProductDomainEventModel, 0);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
  }

  @Test
  public void listen_4() throws Exception {
    pdtProductVendorApprovedEventModel.setPostLive(false);
    pdtProductVendorApprovedEventModel.setReviewPending(true);
    Mockito.when(productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(pdtProductDomainEventModel);
    pdtProductVendorApprovedEventModel.setUpdatedBy(USERNAME);
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    this.productDistributionTaskVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskQCService)
        .processProductDistributionTaskQCKafkaConsumer(pdtProductDomainEventModel, 0);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
  }

  @Test
  public void listenExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    this.productDistributionTaskVendorApprovalKafkaConsumerBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, PDTProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(kafkaTopicProperties).getVendorApprovalEventNoPriority();
  }

  @Test
  public void listenExceptionPriority1Test() throws Exception {
    Mockito.doThrow(Exception.class).when(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    this.productDistributionTaskVendorApprovalKafkaConsumerBean.onDomainEventConsumedPriority1(message);
    verify(objectMapper).readValue(message, PDTProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(kafkaTopicProperties).getVendorApprovalEventPriority1();
  }

  @Test
  public void listenExceptionPriority2Test() throws Exception {
    Mockito.doThrow(Exception.class).when(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    String message = mapper.writeValueAsString(pdtProductVendorApprovedEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductVendorApprovedEventModel.class))
        .thenReturn(pdtProductVendorApprovedEventModel);
    this.productDistributionTaskVendorApprovalKafkaConsumerBean.onDomainEventConsumedPriority2(message);
    verify(objectMapper).readValue(message, PDTProductVendorApprovedEventModel.class);
    Mockito.verify(productDistributionTaskRepository)
        .getPDTDomainModelResponseByCode(Mockito.anyString(), Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(kafkaTopicProperties).getVendorApprovalEventPriority2();
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(productDistributionTaskQCService);
    Mockito.verifyNoMoreInteractions(productDistributionTaskRepository, objectMapper, itemService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

}