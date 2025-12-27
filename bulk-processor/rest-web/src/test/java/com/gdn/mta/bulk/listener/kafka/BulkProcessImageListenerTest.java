package com.gdn.mta.bulk.listener.kafka;

import java.util.ArrayList;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.service.GeneralProcessorService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.mta.rest.web.request.BulkProcessPostImageRequest;
import com.gdn.x.mta.rest.web.request.BulkProcessProductImageRequest;

/**
 * Created by hardikbohra on 21/06/18.
 */
public class BulkProcessImageListenerTest {

  private static final String POST_IMAGE_ACTION = "postImage";

  @InjectMocks
  private BulkProcessImageListener listener;

  @Mock
  private GeneralProcessorService<BulkProcessPostImageRequest, Void, Void> postImageApiProcessorService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(postImageApiProcessorService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void postImageV2Test() throws Exception {
    BulkProcessPostImageRequest bulkProcessPostImageRequest = new BulkProcessPostImageRequest();
    bulkProcessPostImageRequest.setAction(POST_IMAGE_ACTION);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessPostImageRequest.class))
        .thenReturn(bulkProcessPostImageRequest);
    Mockito.doReturn(null).when(postImageApiProcessorService).process(Mockito.eq(bulkProcessPostImageRequest));
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(postImageApiProcessorService).process(Mockito.eq(bulkProcessPostImageRequest));
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessPostImageRequest.class);
    Mockito.verify(kafkaTopicProperties).getBulkApiProcessImageV1Event();
  }

  @Test
  public void listenWhenError() throws Exception {
    BulkProcessPostImageRequest bulkProcessPostImageRequest = new BulkProcessPostImageRequest();
    bulkProcessPostImageRequest.setProductImages(new ArrayList<>());
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessPostImageRequest.class))
        .thenReturn(bulkProcessPostImageRequest);
    Mockito.doThrow(new Exception()).when(postImageApiProcessorService).process(Mockito.eq(bulkProcessPostImageRequest));
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(postImageApiProcessorService).process(Mockito.eq(bulkProcessPostImageRequest));
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessPostImageRequest.class);
    Mockito.verify(kafkaTopicProperties).getBulkApiProcessImageV1Event();
  }
}
