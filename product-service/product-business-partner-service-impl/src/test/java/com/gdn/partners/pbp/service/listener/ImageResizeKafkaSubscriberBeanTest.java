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
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.domain.event.modal.ImageResizeEvent;
import com.gdn.mta.product.service.ImageProcessorService;
import com.gdn.mta.product.service.config.KafkaTopicProperties;

public class ImageResizeKafkaSubscriberBeanTest {

  public static final String STORE_ID = "storeId";
  public static final String PRODUCT_CODE = "productCode";
  private ObjectMapper mapper;

  @InjectMocks
  private ImageResizeKafkaSubscriberBean imageResizeKafkaSubscriberBean;

  @Mock
  private ImageProcessorService imageProcessorService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private ImageResizeEvent imageResizeEvent;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    imageResizeEvent = new ImageResizeEvent();
    imageResizeEvent.setProductCode(PRODUCT_CODE);
    imageResizeEvent.setStoreId(STORE_ID);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(imageProcessorService, objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    String message = mapper.writeValueAsString(imageResizeEvent);
    Mockito.when(objectMapper.readValue(message, ImageResizeEvent.class)).thenReturn(imageResizeEvent);
    this.imageResizeKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 0);
    Mockito.verify(kafkaTopicProperties).getImageResizeEventNoPriority();
    verify(objectMapper).readValue(message, ImageResizeEvent.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 0);
    String message = mapper.writeValueAsString(imageResizeEvent);
    Mockito.when(objectMapper.readValue(message, ImageResizeEvent.class)).thenReturn(imageResizeEvent);
    this.imageResizeKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 0);
    Mockito.verify(kafkaTopicProperties).getImageResizeEventNoPriority();
    verify(objectMapper).readValue(message, ImageResizeEvent.class);
  }

  @Test
  public void onDomainEventConsumedForPrioritySeller1Test() throws Exception {
    String message = mapper.writeValueAsString(imageResizeEvent);
    Mockito.when(objectMapper.readValue(message, ImageResizeEvent.class)).thenReturn(imageResizeEvent);
    this.imageResizeKafkaSubscriberBean.onDomainEventConsumedForPrioritySeller1(message);
    Mockito.verify(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE,  1);
    Mockito.verify(kafkaTopicProperties).getImageResizeEventPriority1();
    verify(objectMapper).readValue(message, ImageResizeEvent.class);
  }

  @Test
  public void onDomainEventConsumedForPrioritySeller1ExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 0);
    String message = mapper.writeValueAsString(imageResizeEvent);
    Mockito.when(objectMapper.readValue(message, ImageResizeEvent.class)).thenReturn(imageResizeEvent);
    this.imageResizeKafkaSubscriberBean.onDomainEventConsumedForPrioritySeller1(message);
    Mockito.verify(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 1);
    Mockito.verify(kafkaTopicProperties).getImageResizeEventPriority1();
    verify(objectMapper).readValue(message, ImageResizeEvent.class);
  }

  @Test
  public void onDomainEventConsumedForPrioritySeller2Test() throws Exception {
    String message = mapper.writeValueAsString(imageResizeEvent);
    Mockito.when(objectMapper.readValue(message, ImageResizeEvent.class)).thenReturn(imageResizeEvent);
    this.imageResizeKafkaSubscriberBean.onDomainEventConsumedForPrioritySeller2(message);
    Mockito.verify(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 2);
    Mockito.verify(kafkaTopicProperties).getImageResizeEventPriority2();
    verify(objectMapper).readValue(message, ImageResizeEvent.class);
  }

  @Test
  public void onDomainEventConsumedForPrioritySeller2ExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 0);
    String message = mapper.writeValueAsString(imageResizeEvent);
    Mockito.when(objectMapper.readValue(message, ImageResizeEvent.class)).thenReturn(imageResizeEvent);
    this.imageResizeKafkaSubscriberBean.onDomainEventConsumedForPrioritySeller2(message);
    Mockito.verify(imageProcessorService).resizeImage(STORE_ID, PRODUCT_CODE, 2);
    Mockito.verify(kafkaTopicProperties).getImageResizeEventPriority2();
    verify(objectMapper).readValue(message, ImageResizeEvent.class);
  }

}