package com.gdn.mta.product.service.domainevent;

import java.util.ArrayList;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.response.UpdatedProductHistoryRequest;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.mta.product.service.config.KafkaTopicProperties;

public class ProductSkuSolrUpdateHistorySubscriberTest {

  @InjectMocks
  ProductSkuSolrUpdateHistorySubscriber productSkuSolrUpdateHistorySubscriber;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private UpdatedProductHistoryService updatedProductHistoryService;

  private ObjectMapper mapper = new ObjectMapper();
  private UpdatedProductHistoryRequest updatedProductHistoryRequest = new UpdatedProductHistoryRequest();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.
        readValue(mapper.writeValueAsString
            (updatedProductHistoryRequest),
        UpdatedProductHistoryRequest.class)).thenReturn(updatedProductHistoryRequest);
    productSkuSolrUpdateHistorySubscriber.onDomainEventConsumed(
        mapper.writeValueAsString(updatedProductHistoryRequest));
    Mockito.verify(updatedProductHistoryService).saveUpdatedProductHistoryToSolr(new ArrayList<>());
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(updatedProductHistoryRequest), UpdatedProductHistoryRequest.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(updatedProductHistoryRequest),
        UpdatedProductHistoryRequest.class)).thenReturn(updatedProductHistoryRequest);
    Mockito.doThrow(NullPointerException.class).when(updatedProductHistoryService).saveUpdatedProductHistoryToSolr(new ArrayList<>());
    productSkuSolrUpdateHistorySubscriber.onDomainEventConsumed(
        mapper.writeValueAsString(updatedProductHistoryRequest));
    Mockito.verify(updatedProductHistoryService).saveUpdatedProductHistoryToSolr(new ArrayList<>());
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(updatedProductHistoryRequest), UpdatedProductHistoryRequest.class);
    Mockito.verify(kafkaTopicProperties).getProductSkuSolrUpdateEvent();
  }

}