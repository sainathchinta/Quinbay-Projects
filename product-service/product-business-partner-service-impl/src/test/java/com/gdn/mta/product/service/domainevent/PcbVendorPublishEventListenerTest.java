package com.gdn.mta.product.service.domainevent;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.VendorPublishEventModel;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.service.config.KafkaTopicProperties;

public class PcbVendorPublishEventListenerTest {

  @InjectMocks
  private PcbVendorPublishEventListener pcbVendorPublishEventListener;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  private VendorPublishEventModel vendorPublishEventModel;

  private static final String PRODUCT_CODE = "productCode";
  private static final String REVIEW_TYPE = "reviewType";

  @BeforeEach
  public void init() throws Exception {
    MockitoAnnotations.initMocks(this);
    vendorPublishEventModel = new VendorPublishEventModel();
    vendorPublishEventModel.setProductCode(PRODUCT_CODE);
    vendorPublishEventModel.setReviewType(REVIEW_TYPE);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productServiceWrapper);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = objectMapper.writeValueAsString(vendorPublishEventModel);
    Mockito.when(objectMapper.readValue(message, VendorPublishEventModel.class)).thenReturn(vendorPublishEventModel);
    pcbVendorPublishEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, VendorPublishEventModel.class);
    Mockito.verify(objectMapper).writeValueAsString(vendorPublishEventModel);
    Mockito.verify(productServiceWrapper).processPcbVendorPublishEvent(vendorPublishEventModel);
    Mockito.verify(kafkaTopicProperties).getPcbVendorPublishEvent();
  }

  @Test
  public void onDomainEventConsumedWithBlankProductCodeTest() throws Exception {
    vendorPublishEventModel.setProductCode("");
    String message = objectMapper.writeValueAsString(vendorPublishEventModel);
    Mockito.when(objectMapper.readValue(message, VendorPublishEventModel.class)).thenReturn(vendorPublishEventModel);
    pcbVendorPublishEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, VendorPublishEventModel.class);
    Mockito.verify(objectMapper).writeValueAsString(vendorPublishEventModel);
    Mockito.verify(productServiceWrapper, times(0)).processPcbVendorPublishEvent(vendorPublishEventModel);
    Mockito.verify(kafkaTopicProperties).getPcbVendorPublishEvent();
  }

  @Test
  public void onDomainEventConsumedWithBlankReviewTypeTest() throws Exception {
    vendorPublishEventModel.setReviewType("");
    String message = objectMapper.writeValueAsString(vendorPublishEventModel);
    Mockito.when(objectMapper.readValue(message, VendorPublishEventModel.class)).thenReturn(vendorPublishEventModel);
    pcbVendorPublishEventListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, VendorPublishEventModel.class);
    Mockito.verify(objectMapper).writeValueAsString(vendorPublishEventModel);
    Mockito.verify(productServiceWrapper, times(0)).processPcbVendorPublishEvent(vendorPublishEventModel);
    Mockito.verify(kafkaTopicProperties).getPcbVendorPublishEvent();
  }
}