package com.gdn.x.productcategorybase.domainevent;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.ProductSuitabilityEventModel;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@ExtendWith(MockitoExtension.class)
public class ProductDsAttributeExtractionListenerTest {

  private static final Logger log = LoggerFactory.getLogger(ProductDsAttributeExtractionListenerTest.class);
  private static final String TEST_TOPIC = "test-topic";
  private static final String TEST_MESSAGE = "{\"productCode\":\"PROD001\",\"attributes\":[]}";
  private static final String TEST_PRODUCT_CODE = "PROD001";

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @InjectMocks
  private ProductDsAttributeExtractionListener productDsAttributeExtractionListener;

  private ProductSuitabilityEventModel productSuitabilityEventModel;

  @BeforeEach
  void setUp() throws JsonProcessingException {
    productSuitabilityEventModel = new ProductSuitabilityEventModel();
    productSuitabilityEventModel.setProductId(TEST_PRODUCT_CODE);

    when(kafkaTopicProperties.getProductDsAttributeExtractionEvent()).thenReturn(TEST_TOPIC);
    when(objectMapper.readValue(TEST_MESSAGE, ProductSuitabilityEventModel.class)).thenReturn(
        productSuitabilityEventModel);
  }

  @Test
  void testOnDomainEventConsumed_Success() throws Exception {
    productDsAttributeExtractionListener.onDomainEventConsumed(TEST_MESSAGE);
    verify(objectMapper).readValue(TEST_MESSAGE, ProductSuitabilityEventModel.class);
    verify(productServiceWrapper).validateAndProcessProductDsAttributeMapping(productSuitabilityEventModel);
  }

  @Test
  void testOnDomainEventConsumed_Exception() throws Exception {
    doThrow(new RuntimeException()).when(productServiceWrapper)
        .validateAndProcessProductDsAttributeMapping(any(ProductSuitabilityEventModel.class));
    productDsAttributeExtractionListener.onDomainEventConsumed(TEST_MESSAGE);
    verify(objectMapper).readValue(TEST_MESSAGE, ProductSuitabilityEventModel.class);
    verify(productServiceWrapper).validateAndProcessProductDsAttributeMapping(productSuitabilityEventModel);
  }
}
