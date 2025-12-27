package com.gdn.partners.pbp.service.listener;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.ImageQcResponseDomainEvent;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.partners.pbp.commons.constants.Constants;

public class ImageQcBacklogKafkaSubscriberBeanTest {

  private static final String PRODUCT_CODE = "productCode";
  private ObjectMapper mapper = new ObjectMapper();
  private ImageQcResponseDomainEvent imageQcResponseDomainEvent;

  @InjectMocks
  private ImageQcBacklogKafkaSubscriberBean imageQcBacklogKafkaSubscriberBean;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    imageQcResponseDomainEvent = new ImageQcResponseDomainEvent();
    imageQcResponseDomainEvent.setProductCode(PRODUCT_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productServiceWrapper);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(
        objectMapper.readValue(mapper.writeValueAsString(imageQcResponseDomainEvent), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    imageQcBacklogKafkaSubscriberBean.onDomainEventConsumed(mapper.writeValueAsString(imageQcResponseDomainEvent));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(imageQcResponseDomainEvent), ImageQcResponseDomainEvent.class);
    Mockito.verify(productServiceWrapper)
        .processImageQcForBacklogProducts(Constants.DEFAULT_STORE_ID, imageQcResponseDomainEvent);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(productServiceWrapper)
        .processImageQcForBacklogProducts(Constants.DEFAULT_STORE_ID, imageQcResponseDomainEvent);
    Mockito.when(
        objectMapper.readValue(mapper.writeValueAsString(imageQcResponseDomainEvent), ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    imageQcBacklogKafkaSubscriberBean.onDomainEventConsumed(mapper.writeValueAsString(imageQcResponseDomainEvent));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(imageQcResponseDomainEvent), ImageQcResponseDomainEvent.class);
    Mockito.verify(productServiceWrapper)
        .processImageQcForBacklogProducts(Constants.DEFAULT_STORE_ID, imageQcResponseDomainEvent);
  }
}