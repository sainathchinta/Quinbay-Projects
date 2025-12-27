package com.gdn.x.product.service.event.listener;

import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.service.api.AllowedAttributeValuesService;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;

public class AttributeChangeListenerTest {

  private AttributeDomainEventModel attributeDomainEventModel;

  @InjectMocks
  private AttributeChangeListener attributeChangeListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private AllowedAttributeValuesService allowedAttributeValuesService;

  private final String MESSAGE = "message";


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    attributeDomainEventModel = new AttributeDomainEventModel();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.objectMapper);
    verifyNoMoreInteractions(this.allowedAttributeValuesService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    when(this.objectMapper.readValue(MESSAGE, AttributeDomainEventModel.class)).thenReturn(
      attributeDomainEventModel);
    attributeChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper)
        .readValue(MESSAGE, AttributeDomainEventModel.class);
    Mockito.verify(this.allowedAttributeValuesService)
        .updateAttributeValueSequenceByAttributeCode(attributeDomainEventModel);
  }


  @Test
  public void onDomainEventConsumed_exceptionTest() throws Exception {
    when(this.objectMapper.readValue(MESSAGE, AttributeDomainEventModel.class)).thenReturn(
      attributeDomainEventModel);
    Mockito.doThrow(Exception.class).when(this.allowedAttributeValuesService)
        .updateAttributeValueSequenceByAttributeCode(attributeDomainEventModel);
    attributeChangeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper)
      .readValue(MESSAGE, AttributeDomainEventModel.class);
    Mockito.verify(this.allowedAttributeValuesService)
        .updateAttributeValueSequenceByAttributeCode(attributeDomainEventModel);
  }


}
