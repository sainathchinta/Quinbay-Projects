package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.repository.AutoApprovedRepository;
import com.gdn.partners.product.analytics.service.AutoApprovedService;
import model.DeleteAutoApprovedProductsEventModel;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import static org.mockito.MockitoAnnotations.initMocks;


public class AutoApprovedProductListenerTest {

  private static final String MESSAGE = "message";

  private static final String PRODUCT_CODE = "product-code";

  private static final String ACTION = "action";

  private static final String DELETE_EVENT = "delete-event";

  @InjectMocks
  private AutoApprovedProductListener productListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private AutoApprovedRepository autoApprovedRepository;

  @Mock
  private AutoApprovedService autoApprovedService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private DeleteAutoApprovedProductsEventModel eventModel;

  @BeforeEach
  public void setUp() {
    initMocks(this);
    eventModel = new DeleteAutoApprovedProductsEventModel();
    eventModel.setProductCode(PRODUCT_CODE);
    eventModel.setAction(ACTION);
  }


  @AfterEach
  public void afterTest() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(autoApprovedRepository);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(autoApprovedService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getDeleteAutoApprovedProductEventName())
      .thenReturn(DELETE_EVENT);
    Mockito.when(objectMapper.readValue(MESSAGE, DeleteAutoApprovedProductsEventModel.class))
      .thenReturn(eventModel);
    productListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, DeleteAutoApprovedProductsEventModel.class);
    Mockito.verify(autoApprovedService).deleteAutoApprovedProduct(PRODUCT_CODE, ACTION);
    Mockito.verify(kafkaTopicProperties).getDeleteAutoApprovedProductEventName();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getDeleteAutoApprovedProductEventName())
      .thenReturn(DELETE_EVENT);
    Mockito.when(objectMapper.readValue(MESSAGE, DeleteAutoApprovedProductsEventModel.class))
      .thenThrow(new ApplicationRuntimeException());
    productListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, DeleteAutoApprovedProductsEventModel.class);
    Mockito.verify(kafkaTopicProperties).getDeleteAutoApprovedProductEventName();
  }

  @Test
  public void onDomainEventConsumedEmptyProductCodeTest() throws Exception {
    eventModel.setProductCode(StringUtils.EMPTY);
    Mockito.when(kafkaTopicProperties.getDeleteAutoApprovedProductEventName())
      .thenReturn(DELETE_EVENT);
    Mockito.when(objectMapper.readValue(MESSAGE, DeleteAutoApprovedProductsEventModel.class))
      .thenReturn(eventModel);
    productListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, DeleteAutoApprovedProductsEventModel.class);
    Mockito.verify(kafkaTopicProperties).getDeleteAutoApprovedProductEventName();
  }
}
