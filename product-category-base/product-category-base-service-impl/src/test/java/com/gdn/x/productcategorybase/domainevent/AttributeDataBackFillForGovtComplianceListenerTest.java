package com.gdn.x.productcategorybase.domainevent;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.SchedulerService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class AttributeDataBackFillForGovtComplianceListenerTest {

  private static final String MESSAGE = "{\\\"productCode\\\":\\\"MTA-51336606\\\"}";
  private static final String EVENT = "Event";
  private static final String PRODUCT_CODE = "productCode";

  @InjectMocks
  private AttributeDataBackFillForGovtComplianceListener listener;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private SchedulerService schedulerService;


  @BeforeEach
  void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties, objectMapper, productServiceWrapper,
        schedulerService);
  }

  @Test
  public void onDomainEventConsumedTest() throws JsonProcessingException {
    CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel =
        new CommonImageBackfillingEventModel();
    productAttributeDataBackFillingEventModel.setProductCode(PRODUCT_CODE);
    Mockito.when(kafkaTopicProperties.getProductAttributeMigrationTopic()).thenReturn(EVENT);
    Mockito.when(objectMapper.readValue(MESSAGE, CommonImageBackfillingEventModel.class))
        .thenReturn(productAttributeDataBackFillingEventModel);
    Mockito.doNothing().when(productServiceWrapper)
        .processProductAttributeDataBackFilling(productAttributeDataBackFillingEventModel);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(kafkaTopicProperties).getProductAttributeMigrationTopic();
    Mockito.verify(objectMapper)
        .readValue(MESSAGE, CommonImageBackfillingEventModel.class);
    Mockito.verify(productServiceWrapper)
        .processProductAttributeDataBackFilling(productAttributeDataBackFillingEventModel);
  }


  @Test
  public void onDomainEventConsumed_Exception_Test() throws JsonProcessingException {
    CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel =
        new CommonImageBackfillingEventModel();
    productAttributeDataBackFillingEventModel.setProductCode(PRODUCT_CODE);
    Mockito.when(kafkaTopicProperties.getProductAttributeMigrationTopic()).thenReturn(EVENT);
    Mockito.when(objectMapper.readValue(MESSAGE, CommonImageBackfillingEventModel.class))
        .thenReturn(productAttributeDataBackFillingEventModel);
    Mockito.doThrow(new ApplicationRuntimeException()).when(productServiceWrapper)
        .processProductAttributeDataBackFilling(productAttributeDataBackFillingEventModel);
    Mockito.doNothing().when(schedulerService)
        .updateProductMigrationStatus(Mockito.anyString(), Mockito.any(), Mockito.any());
    try {
      listener.onDomainEventConsumed(MESSAGE);
    } finally {
      Mockito.verify(kafkaTopicProperties).getProductAttributeMigrationTopic();
      Mockito.verify(objectMapper).readValue(MESSAGE, CommonImageBackfillingEventModel.class);
      Mockito.verify(schedulerService)
          .updateProductMigrationStatus(Mockito.anyString(), Mockito.any(), Mockito.any());
      Mockito.verify(productServiceWrapper)
          .processProductAttributeDataBackFilling(productAttributeDataBackFillingEventModel);
    }
  }
}