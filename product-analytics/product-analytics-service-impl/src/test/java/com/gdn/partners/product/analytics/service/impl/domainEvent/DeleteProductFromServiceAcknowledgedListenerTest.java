package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.TerminatedSellerDeletionService;
import model.DeleteProductResultEventModel;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;

import static org.mockito.ArgumentMatchers.anyString;

@ExtendWith(MockitoExtension.class)
class DeleteProductFromServiceAcknowledgedListenerTest {

  @InjectMocks
  private DeleteProductFromServiceAcknowledgedListener deleteProductListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private TerminatedSellerDeletionService terminatedSellerDeletionService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  public static final String message =
      "{\"productCode\": \"productCode\", \"sellerCode\": \"sellerCode\", \"service\": "
          + "\"service\", "
          + "\"result\": \"result\"}";
  public static final String PRODUCT_CODE = "productCode";
  public static final String SELLER_CODE = "sellerCode";
  public static final String SERVICE = "service";
  public static final String RESULT = "result";
  public static final String DELETE_EVENT = "delete.event";


  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(terminatedSellerDeletionService);
  }

  @Test
  void testOnDomainEventConsumed() throws IOException {
    DeleteProductResultEventModel model = new DeleteProductResultEventModel();
    model.setProductCode(PRODUCT_CODE);
    model.setSellerCode(SELLER_CODE);
    model.setService(SERVICE);
    model.setResult(RESULT);

    Mockito.when(kafkaTopicProperties.getPermanentDeleteProductEventName())
        .thenReturn(DELETE_EVENT);

    Mockito.when(objectMapper.readValue(anyString(), (Class<Object>) Mockito.any()))
        .thenReturn(model);

    deleteProductListener.onDomainEventConsumed(message);

    Mockito.verify(objectMapper).readValue(message, DeleteProductResultEventModel.class);
    Mockito.verify(terminatedSellerDeletionService)
        .updateStatusForParticularService(PRODUCT_CODE, SELLER_CODE, SERVICE, RESULT);
  }

  @Test
  void testOnDomainEventConsumedError() throws IOException {
    DeleteProductResultEventModel model = new DeleteProductResultEventModel();
    model.setProductCode(PRODUCT_CODE);
    model.setSellerCode(SELLER_CODE);
    model.setService(SERVICE);
    model.setResult(RESULT);

    Mockito.when(kafkaTopicProperties.getPermanentDeleteProductEventName())
        .thenReturn(DELETE_EVENT);

    Mockito.when(objectMapper.readValue(anyString(), (Class<Object>) Mockito.any()))
        .thenThrow(new ApplicationRuntimeException());

    deleteProductListener.onDomainEventConsumed(message);

    Mockito.verify(objectMapper).readValue(message, DeleteProductResultEventModel.class);
  }
}