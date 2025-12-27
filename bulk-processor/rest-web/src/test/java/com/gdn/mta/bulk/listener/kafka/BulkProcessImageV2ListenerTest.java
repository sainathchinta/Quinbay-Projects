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
import com.gdn.mta.bulk.models.BulkProcessPostImageV2Request;
import com.gdn.mta.bulk.models.BulkProcessProductImageV2Request;
import com.gdn.mta.bulk.service.GeneralProcessorService;
import com.gdn.partners.bulk.util.Constant;

/**
 * Created by hardikbohra on 29/05/18.
 */
public class BulkProcessImageV2ListenerTest {

  public static final String POST_IMAGE_ACTION_V2 = "postImageV2";

  @InjectMocks
  private BulkProcessImageV2Listener listener;

  @Mock
  private GeneralProcessorService<BulkProcessPostImageV2Request, Void, Void> postImageApiProcessorV2Service;

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
    Mockito.verifyNoMoreInteractions(postImageApiProcessorV2Service);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void postImageV2Test() throws Exception {
    BulkProcessPostImageV2Request bulkProcessPostImageRequest = new BulkProcessPostImageV2Request();
    bulkProcessPostImageRequest.setAction(POST_IMAGE_ACTION_V2);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessPostImageV2Request.class))
        .thenReturn(bulkProcessPostImageRequest);
    Mockito.doReturn(null).when(postImageApiProcessorV2Service).process(Mockito.eq(bulkProcessPostImageRequest));
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(postImageApiProcessorV2Service).process(Mockito.eq(bulkProcessPostImageRequest));
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessPostImageV2Request.class);
    Mockito.verify(kafkaTopicProperties).getBulkApiProcessImageV2Event();
  }

  @Test
  public void listenWhenError() throws Exception {
    BulkProcessPostImageV2Request bulkProcessPostImageRequest = new BulkProcessPostImageV2Request();
    bulkProcessPostImageRequest.setImages(new ArrayList<BulkProcessProductImageV2Request>());
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessPostImageV2Request.class))
        .thenReturn(bulkProcessPostImageRequest);
    Mockito.doThrow(new Exception()).when(postImageApiProcessorV2Service)
        .process(Mockito.eq(bulkProcessPostImageRequest));
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(postImageApiProcessorV2Service).process(Mockito.eq(bulkProcessPostImageRequest));
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessPostImageV2Request.class);
    Mockito.verify(kafkaTopicProperties).getBulkApiProcessImageV2Event();
  }
}
