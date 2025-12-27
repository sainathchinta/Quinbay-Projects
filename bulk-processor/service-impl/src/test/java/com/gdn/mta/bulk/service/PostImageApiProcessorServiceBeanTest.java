package com.gdn.mta.bulk.service;

import static org.mockito.Mockito.times;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkImageQueue;
import com.gdn.x.mta.rest.web.request.BulkProcessPostImageRequest;
import com.gdn.x.mta.rest.web.request.BulkProcessProductImageRequest;
import com.gdn.x.mta.rest.web.request.QueueHistoryResult;

public class PostImageApiProcessorServiceBeanTest {

  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLT-23808";
  private static final String DEFAULT_REQUEST_ID = "100000";
  private static final String DEFAULT_BRAND = "brand";
  private static final String DEFAULT_PRODUCT_NAME = "p-name";
  private static final String DEFAULT_IMAGE_NAME = "image-name.png";
  private static final String DEFAULT_IMAGE_ENCODED = "asdqwezxc";
  private static final String POST_IMAGE_ACTION = "postImage";
  
  @InjectMocks
  private PostImageApiProcessorServiceBean postImageApiProcessorServiceBean;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  
  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(kafkaProducer, objectMapper, kafkaTopicProperties);
  }
  
  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
  }
  
  @Test
  public void preProcessTest(){
    // Do Nothing
  }
  
  @Test
  public void processTest() throws Exception{
    BulkProcessPostImageRequest bulkProcessPostImageRequest = BulkProcessPostImageRequestBuilder();
    postImageApiProcessorServiceBean.process(bulkProcessPostImageRequest);
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageApiToMtaEvent()), Mockito.any(BulkImageQueue.class));
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(QueueHistoryResult.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProcessImageApiToMtaEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processNoImageMapTest() throws Exception{
    BulkProcessPostImageRequest bulkProcessPostImageRequest = BulkProcessPostImageRequestBuilder();
    Map<String, String> images = new HashMap<String, String>();
    bulkProcessPostImageRequest.getProductImages().get(0).setImages(images);
    postImageApiProcessorServiceBean.process(bulkProcessPostImageRequest);
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageApiToMtaEvent()), Mockito.any(BulkImageQueue.class));
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(QueueHistoryResult.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProcessImageApiToMtaEvent();
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkProcessImageResponseEvent();
  }
  
  @Test
  public void processNoProductImageRequestTest() throws Exception{
    BulkProcessPostImageRequest bulkProcessPostImageRequest = new BulkProcessPostImageRequest();
    List<BulkProcessProductImageRequest> bulkProcessProductImageRequestList = new ArrayList<BulkProcessProductImageRequest>();
    bulkProcessPostImageRequest.setProductImages(bulkProcessProductImageRequestList);
    postImageApiProcessorServiceBean.process(bulkProcessPostImageRequest);
  }
  
  private BulkProcessPostImageRequest BulkProcessPostImageRequestBuilder(){
    BulkProcessPostImageRequest bulkProcessPostImageRequest = new BulkProcessPostImageRequest();
    bulkProcessPostImageRequest.setRequestId(DEFAULT_REQUEST_ID);
    bulkProcessPostImageRequest.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcessPostImageRequest.setAction(POST_IMAGE_ACTION);
    BulkProcessProductImageRequest bulkProcessProductImageRequest = new BulkProcessProductImageRequest();
    List<BulkProcessProductImageRequest> bulkProcessProductImageRequestList = new ArrayList<BulkProcessProductImageRequest>();
    bulkProcessProductImageRequest.setBrand(DEFAULT_BRAND);
    bulkProcessProductImageRequest.setProductName(DEFAULT_PRODUCT_NAME);
    Map<String, String> images = new HashMap<String, String>();
    images.put(DEFAULT_IMAGE_NAME, DEFAULT_IMAGE_ENCODED);
    bulkProcessProductImageRequest.setImages(images);
    bulkProcessProductImageRequestList.add(bulkProcessProductImageRequest);
    bulkProcessPostImageRequest.setProductImages(bulkProcessProductImageRequestList);
    return bulkProcessPostImageRequest;
  }

  @Test
  public void processMoreImagesTest() throws Exception{
    Map<String, String> images =
        IntStream.range(0, 15).mapToObj(i -> String.valueOf(i)).collect(Collectors.toMap(i -> i, i -> i));
    BulkProcessPostImageRequest bulkProcessPostImageRequest = BulkProcessPostImageRequestBuilder();
    bulkProcessPostImageRequest.getProductImages().get(0).setImages(images);
    postImageApiProcessorServiceBean.process(bulkProcessPostImageRequest);
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageApiToMtaEvent()), Mockito.any(BulkImageQueue.class));
    Mockito.verify(kafkaProducer).send(Mockito.eq
        (kafkaTopicProperties.getBulkProcessImageResponseEvent()), Mockito.any(QueueHistoryResult.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProcessImageApiToMtaEvent();
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkProcessImageResponseEvent();
  }
  
}
