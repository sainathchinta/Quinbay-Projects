package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeExtractionModel;
import com.gdn.x.productcategorybase.domainevent.ProductAttributeExtractionBackfillingListener;
import com.gdn.x.productcategorybase.service.ProductAttributeExtractionService;

public class ProductAttributeExtractionBackfillingListenerTest {

  private static final String MESSAGE = "message";

  @InjectMocks
  private ProductAttributeExtractionBackfillingListener productAttributeExtractionBackfillingListener;

  @Mock
  private ProductAttributeExtractionService productAttributeExtractionService;

  @Mock
  private ObjectMapper objectMapper;

  private ProductAttributeExtractionModel productAttributeExtractionModel;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    productAttributeExtractionModel = new ProductAttributeExtractionModel();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productAttributeExtractionService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    when(objectMapper.readValue(MESSAGE, ProductAttributeExtractionModel.class))
        .thenReturn(productAttributeExtractionModel);
    Mockito.doNothing().when(productAttributeExtractionService)
        .updateExtractedAttributesForBackfilling(productAttributeExtractionModel);
    productAttributeExtractionBackfillingListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(productAttributeExtractionService)
        .updateExtractedAttributesForBackfilling(productAttributeExtractionModel);
    verify(objectMapper).readValue(MESSAGE, ProductAttributeExtractionModel.class);
  }

  @Test
  public void onDomainEventConsumedTest_Exception() throws Exception {
    doThrow(RuntimeException.class).when(objectMapper).readValue(MESSAGE, ProductAttributeExtractionModel.class);
    productAttributeExtractionBackfillingListener.onDomainEventConsumed(MESSAGE);
    verify(objectMapper).readValue(MESSAGE, ProductAttributeExtractionModel.class);
  }
}
