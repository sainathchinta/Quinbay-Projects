package com.gdn.x.product.service.event.listener;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.DeleteTerminatedSellerProductEventModel;
import com.gdn.x.product.service.api.DeleteTerminatedSellerProductService;
import com.gdn.x.product.service.properties.KafkaTopicProperties;

public class DeleteTerminatedSellerProductEventListenerTest {
  private static final String PRODUCT_CODE = "productCode";
  private static final String SELLER_CODE = "sellerCode";
  private static final String EVENT_NAME = "com.gdn.product.analytics.permanent.delete.product.xproduct";

  @InjectMocks
  private DeleteTerminatedSellerProductEventListener deleteTerminatedSellerProductEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private DeleteTerminatedSellerProductService deleteTerminatedSellerProductService;

  private ObjectMapper mapper;
  private DeleteTerminatedSellerProductEventModel deleteTerminatedSellerProductEventModel;
  private String json;

  @BeforeEach
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.openMocks(this);

    mapper = new ObjectMapper();
    deleteTerminatedSellerProductEventModel =
        DeleteTerminatedSellerProductEventModel.builder().productCode(PRODUCT_CODE).sellerCode(SELLER_CODE).build();
    json = mapper.writeValueAsString(deleteTerminatedSellerProductEventListener);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, kafkaTopicProperties, deleteTerminatedSellerProductService);
  }

  @Test
  public void onDomainEventConsumed() throws JsonProcessingException {
    Mockito.when(objectMapper.readValue(json, DeleteTerminatedSellerProductEventModel.class))
        .thenReturn(deleteTerminatedSellerProductEventModel);
    Mockito.doNothing().when(deleteTerminatedSellerProductService)
        .deleteTerminatedSellerProductData(deleteTerminatedSellerProductEventModel);
    Mockito.when(kafkaTopicProperties.getDeleteTerminatedSellerProductEvent()).thenReturn(EVENT_NAME);

    deleteTerminatedSellerProductEventListener.onDomainEventConsumed(json);

    Mockito.verify(objectMapper).readValue(json, DeleteTerminatedSellerProductEventModel.class);
    Mockito.verify(deleteTerminatedSellerProductService)
        .deleteTerminatedSellerProductData(deleteTerminatedSellerProductEventModel);
    Mockito.verify(kafkaTopicProperties).getDeleteTerminatedSellerProductEvent();
  }

  @Test
  public void onDomainEventConsumed_Error() throws JsonProcessingException {
    Mockito.when(objectMapper.readValue(json, DeleteTerminatedSellerProductEventModel.class))
        .thenThrow(JsonProcessingException.class);
    Mockito.when(kafkaTopicProperties.getDeleteTerminatedSellerProductEvent()).thenReturn(EVENT_NAME);

    deleteTerminatedSellerProductEventListener.onDomainEventConsumed(json);

    Mockito.verify(objectMapper).readValue(json, DeleteTerminatedSellerProductEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getDeleteTerminatedSellerProductEvent();
  }

}