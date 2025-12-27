package com.gdn.x.mta.distributiontask.inbound.impl;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.domain.event.model.VendorSearchAutoHealEventModel;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;

public class VendorProductSearchAutoHealListenerTest {
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "MTA-0000001";

  @InjectMocks
  private VendorProductSearchAutoHealListener vendorProductSearchAutoHealListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductWrapperService productWrapperService;

  private ObjectMapper mapper = new ObjectMapper();
  private VendorSearchAutoHealEventModel vendorSearchAutoHealEventModel;
  private String message;

  @BeforeEach
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.openMocks(this);
    vendorSearchAutoHealEventModel = new VendorSearchAutoHealEventModel(STORE_ID, PRODUCT_CODE);
    message = mapper.writeValueAsString(vendorSearchAutoHealEventModel);
  }

  @Test
   void onDomainEventConsumedTest() throws JsonProcessingException {
    Mockito.when(objectMapper.readValue(message, VendorSearchAutoHealEventModel.class))
        .thenReturn(vendorSearchAutoHealEventModel);
    Mockito.doNothing().when(productWrapperService).processVendorSearchAutoHealProduct(STORE_ID, PRODUCT_CODE);
    vendorProductSearchAutoHealListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, VendorSearchAutoHealEventModel.class);
    Mockito.verify(productWrapperService).processVendorSearchAutoHealProduct(STORE_ID, PRODUCT_CODE);
  }

  @Test
   void onDomainEventConsumedErrorTest() throws JsonProcessingException {
    Mockito.when(objectMapper.readValue(message, VendorSearchAutoHealEventModel.class))
        .thenReturn(vendorSearchAutoHealEventModel);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productWrapperService).processVendorSearchAutoHealProduct(STORE_ID, PRODUCT_CODE);
    vendorProductSearchAutoHealListener.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, VendorSearchAutoHealEventModel.class);
    Mockito.verify(productWrapperService).processVendorSearchAutoHealProduct(STORE_ID, PRODUCT_CODE);
  }

}
