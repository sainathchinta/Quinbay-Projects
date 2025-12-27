package com.gdn.partners.pbp.service.listener;

import java.util.Collections;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.AutoNeedRevisionDomainEvent;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductServiceWrapper;

public class ProductAutoNeedRevisionSubscriberBeanTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String NOTES = "notes";
  private ObjectMapper mapper;
  private AutoNeedRevisionDomainEvent autoNeedRevisionDomainEvent;

  @InjectMocks
  private ProductAutoNeedRevisionSubscriberBean autoNeedRevisionSubscriberBean;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductService productService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    mapper = new ObjectMapper();
    autoNeedRevisionDomainEvent = new AutoNeedRevisionDomainEvent();
    autoNeedRevisionDomainEvent.setProductCode(PRODUCT_CODE);
    autoNeedRevisionDomainEvent.setPredictionTypeList(Collections.singletonList(NOTES));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productServiceWrapper);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(productService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper
        .readValue(mapper.writeValueAsString(autoNeedRevisionDomainEvent), AutoNeedRevisionDomainEvent.class))
        .thenReturn(mapper
            .readValue(mapper.writeValueAsString(autoNeedRevisionDomainEvent), AutoNeedRevisionDomainEvent.class));
    autoNeedRevisionSubscriberBean.onDomainEventConsumed(mapper.writeValueAsString(autoNeedRevisionDomainEvent));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(autoNeedRevisionDomainEvent), AutoNeedRevisionDomainEvent.class);
    Mockito.verify(productServiceWrapper).autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, false, true, true, false);
    Mockito.verify(productService).publishProductStatusEventByProductCode(PRODUCT_CODE,
        ProductStatus.NEED_CORRECTION, StringUtils.EMPTY);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper
        .readValue(mapper.writeValueAsString(autoNeedRevisionDomainEvent), AutoNeedRevisionDomainEvent.class))
        .thenReturn(mapper
            .readValue(mapper.writeValueAsString(autoNeedRevisionDomainEvent), AutoNeedRevisionDomainEvent.class));
    Mockito.doThrow(Exception.class).when(productServiceWrapper).autoNeedRevisionProduct(autoNeedRevisionDomainEvent,
        false, false, true, true, false);
    autoNeedRevisionSubscriberBean.onDomainEventConsumed(mapper.writeValueAsString(autoNeedRevisionDomainEvent));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(autoNeedRevisionDomainEvent), AutoNeedRevisionDomainEvent.class);
    Mockito.verify(productServiceWrapper).autoNeedRevisionProduct(autoNeedRevisionDomainEvent, false, false, true, true, false);
  }
}