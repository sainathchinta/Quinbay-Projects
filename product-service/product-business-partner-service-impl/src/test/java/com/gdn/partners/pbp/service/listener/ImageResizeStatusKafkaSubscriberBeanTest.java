package com.gdn.partners.pbp.service.listener;


import static org.mockito.Mockito.verify;

import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.service.config.KafkaTopicProperties;

public class ImageResizeStatusKafkaSubscriberBeanTest {

  public static final String STORE_ID = "storeId";
  public static final String PRODUCT_CODE = "PRODUCT_CODE";
  public static final String USER_NAME = "USER_NAME";

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private ImageResizeStatusKafkaSubscriberBean imageResizeStatusKafkaSubscriberBean;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private BulkImageProcessResponse bulkImageProcessResponse;
  private BulkImageProcessResponse bulkImageProcessResponseFailue;
  private ObjectMapper mapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkImageProcessResponse = new BulkImageProcessResponse();
    bulkImageProcessResponse.setUsername(USER_NAME);
    bulkImageProcessResponse.setStoreId(STORE_ID);
    bulkImageProcessResponse.setGroupCode(PRODUCT_CODE);
    ImageResponse imageResponse = new ImageResponse();
    imageResponse.setSuccess(true);
    ImageResponse imageResponse1 = new ImageResponse();
    imageResponse1.setSuccess(true);
    ImageResponse imageResponse2 = new ImageResponse();
    imageResponse2.setSuccess(true);
    ImageResponse imageResponse3 = new ImageResponse();
    imageResponse3.setSuccess(false);
    bulkImageProcessResponse.setImageResponses(Arrays.asList(imageResponse, imageResponse1, imageResponse2));
    bulkImageProcessResponseFailue = new BulkImageProcessResponse();
    bulkImageProcessResponseFailue.setStoreId(STORE_ID);
    bulkImageProcessResponseFailue.setGroupCode(PRODUCT_CODE);
    bulkImageProcessResponseFailue
        .setImageResponses(Arrays.asList(imageResponse, imageResponse1, imageResponse2, imageResponse3));
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productServiceWrapper, objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
        .thenReturn(bulkImageProcessResponse);
    this.imageResizeStatusKafkaSubscriberBean.
            onDomainEventConsumed(message);
    Mockito.verify(productServiceWrapper).updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    verify(kafkaTopicProperties).getImageResizeStatusEventNoPriority();
    Assertions.assertEquals(USER_NAME, GdnMandatoryRequestParameterUtil.getUsername());
  }

  @Test
  public void onDomainEventConsumedFailureTest() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponseFailue);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
        .thenReturn(bulkImageProcessResponseFailue);
    this.imageResizeStatusKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(productServiceWrapper).updateImagePathsAndFlagOnResizingImageFailure(bulkImageProcessResponseFailue);
    verify(kafkaTopicProperties).getImageResizeStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedSuccessFalseTest() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
        .thenReturn(bulkImageProcessResponse);
    this.imageResizeStatusKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(productServiceWrapper).updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(kafkaTopicProperties).getImageResizeStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedResizeFailureSuccessFalseTest() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponseFailue);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
        .thenReturn(bulkImageProcessResponseFailue);
    this.imageResizeStatusKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(productServiceWrapper).updateImagePathsAndFlagOnResizingImageFailure(bulkImageProcessResponseFailue);
    verify(kafkaTopicProperties).getImageResizeStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedPriority1Test() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
            .thenReturn(bulkImageProcessResponse);
    this.imageResizeStatusKafkaSubscriberBean.
            onDomainEventConsumedStatusEventForPriority1(message);
    Mockito.verify(productServiceWrapper).updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Assertions.assertEquals(USER_NAME, GdnMandatoryRequestParameterUtil.getUsername());
    verify(kafkaTopicProperties).getImageResizeStatusEventPriority1();
  }

  @Test
  public void onDomainEventConsumedPriority1FailureTest() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponseFailue);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
            .thenReturn(bulkImageProcessResponseFailue);
    this.imageResizeStatusKafkaSubscriberBean.onDomainEventConsumedStatusEventForPriority1(message);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(productServiceWrapper).updateImagePathsAndFlagOnResizingImageFailure(bulkImageProcessResponseFailue);
    verify(kafkaTopicProperties).getImageResizeStatusEventPriority1();
  }

  @Test
  public void onDomainEventConsumedPriority1SuccessFalseTest() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
            .thenReturn(bulkImageProcessResponse);
    this.imageResizeStatusKafkaSubscriberBean.onDomainEventConsumedStatusEventForPriority1(message);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(productServiceWrapper).updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(kafkaTopicProperties).getImageResizeStatusEventPriority1();
  }

  @Test
  public void onDomainEventConsumedPriority1ResizeFailureSuccessFalseTest() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponseFailue);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
            .thenReturn(bulkImageProcessResponseFailue);
    this.imageResizeStatusKafkaSubscriberBean.onDomainEventConsumedStatusEventForPriority1(message);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(productServiceWrapper).updateImagePathsAndFlagOnResizingImageFailure(bulkImageProcessResponseFailue);
    verify(kafkaTopicProperties).getImageResizeStatusEventPriority1();
  }

  @Test
  public void onDomainEventConsumedPriority2Test() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
            .thenReturn(bulkImageProcessResponse);
    this.imageResizeStatusKafkaSubscriberBean.
            onDomainEventConsumedStatusEventForPriority2(message);
    Mockito.verify(productServiceWrapper).updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Assertions.assertEquals(USER_NAME, GdnMandatoryRequestParameterUtil.getUsername());
    verify(kafkaTopicProperties).getImageResizeStatusEventPriority2();
  }

  @Test
  public void onDomainEventConsumedPriority2FailureTest() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponseFailue);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
            .thenReturn(bulkImageProcessResponseFailue);
    this.imageResizeStatusKafkaSubscriberBean.onDomainEventConsumedStatusEventForPriority2(message);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(productServiceWrapper).updateImagePathsAndFlagOnResizingImageFailure(bulkImageProcessResponseFailue);
    verify(kafkaTopicProperties).getImageResizeStatusEventPriority2();
  }

  @Test
  public void onDomainEventConsumedPriority2SuccessFalseTest() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
            .thenReturn(bulkImageProcessResponse);
    this.imageResizeStatusKafkaSubscriberBean.onDomainEventConsumedStatusEventForPriority2(message);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(productServiceWrapper).updateImagePathsAndSkipScreeningForPostLiveProducts(bulkImageProcessResponse);
    verify(kafkaTopicProperties).getImageResizeStatusEventPriority2();
  }

  @Test
  public void onDomainEventConsumedPriority2ResizeFailureSuccessFalseTest() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponseFailue);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
            .thenReturn(bulkImageProcessResponseFailue);
    this.imageResizeStatusKafkaSubscriberBean.onDomainEventConsumedStatusEventForPriority2(message);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(productServiceWrapper).updateImagePathsAndFlagOnResizingImageFailure(bulkImageProcessResponseFailue);
    verify(kafkaTopicProperties).getImageResizeStatusEventPriority2();
  }

  @Test
  public void onDomainEventConsumedPriority1ExceptionTest() throws Exception {
    BulkImageProcessResponse response = new BulkImageProcessResponse();
    response.setGroupCode(PRODUCT_CODE);
    response.setStoreId(STORE_ID);
    response.setUsername(USER_NAME);
    String message = mapper.writeValueAsString(response);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
            .thenReturn(new BulkImageProcessResponse());
    Mockito.doThrow(new Exception()).when(productServiceWrapper)
            .updateImagePathsAndFlagOnResizingImageFailure(Mockito.any(BulkImageProcessResponse.class));
    imageResizeStatusKafkaSubscriberBean.onDomainEventConsumedStatusEventForPriority1(message);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    verify(kafkaTopicProperties).getImageResizeStatusEventPriority1();
  }

  @Test
  public void onDomainEventConsumedPriority2ExceptionTest() throws Exception {
    BulkImageProcessResponse response = new BulkImageProcessResponse();
    response.setGroupCode(PRODUCT_CODE);
    response.setStoreId(STORE_ID);
    response.setUsername(USER_NAME);
    String message = mapper.writeValueAsString(response);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class))
            .thenReturn(new BulkImageProcessResponse());
    Mockito.doThrow(new Exception()).when(productServiceWrapper)
            .updateImagePathsAndFlagOnResizingImageFailure(Mockito.any(BulkImageProcessResponse.class));
    imageResizeStatusKafkaSubscriberBean.onDomainEventConsumedStatusEventForPriority2(message);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    verify(kafkaTopicProperties).getImageResizeStatusEventPriority2();
  }
}