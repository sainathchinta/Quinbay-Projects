package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.ImageDeleteService;
import model.ImageDeleteEventModel;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class PermanentImageDeleteEventListenerTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String SELLER_CODE = "sellerCode";
  private static final String EVENT = "event";
  private static final String MESSAGE = "MESSAGE";

  @InjectMocks
  private PermanentImageDeleteEventListener permanentImageDeleteEventListener;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ImageDeleteService imageDeleteService;

  private ImageDeleteEventModel eventModel;

  @BeforeEach
  public void setup() {
    eventModel = new ImageDeleteEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setSellerCode(SELLER_CODE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(imageDeleteService);
  }

  @Test
  void onDomainEventConsumedTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getProductImageDeleteEventName()).thenReturn(EVENT);
    Mockito.when(objectMapper.readValue(MESSAGE, ImageDeleteEventModel.class))
      .thenReturn(eventModel);
    permanentImageDeleteEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ImageDeleteEventModel.class);
    Mockito.verify(imageDeleteService)
      .updateImageCollectionForProductDelete(PRODUCT_CODE);
    Mockito.verify(kafkaTopicProperties).getProductImageDeleteEventName();
  }

  @Test
  void onDomainEventConsumedEmptyProductCodeTest() throws Exception {
    eventModel.setProductCode(StringUtils.EMPTY);
    Mockito.when(kafkaTopicProperties.getProductImageDeleteEventName()).thenReturn(EVENT);
    Mockito.when(objectMapper.readValue(MESSAGE, ImageDeleteEventModel.class))
      .thenReturn(eventModel);
    permanentImageDeleteEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ImageDeleteEventModel.class);
    Mockito.verify(kafkaTopicProperties).getProductImageDeleteEventName();
  }

  @Test
  void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getProductImageDeleteEventName()).thenReturn(EVENT);
    Mockito.when(objectMapper.readValue(MESSAGE, ImageDeleteEventModel.class))
      .thenThrow(new ApplicationRuntimeException());
    permanentImageDeleteEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ImageDeleteEventModel.class);
    Mockito.verify(kafkaTopicProperties).getProductImageDeleteEventName();
  }
}
