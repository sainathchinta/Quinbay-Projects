package com.gdn.x.product.service.event.listener;

import static org.mockito.MockitoAnnotations.openMocks;

import java.io.IOException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.PickupPointVOEventModel;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.ObjectConverterService;

public class PickupPointUpdateAllEventListenerTest {

  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String CODE = "code";
  private PickupPointVOEventModel pickupPointChange;
  private ObjectMapper mapper = new ObjectMapper();
  private BusinessPartnerPickupPoint businessPartnerPickupPoint;

  @InjectMocks
  private PickupPointUpdateAllEventListener pickupPointUpdateAllEventListener;

  @Mock
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ObjectConverterService objectConverterService;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    pickupPointChange = new PickupPointVOEventModel();
    pickupPointChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointChange.setCode(CODE);

    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(businessPartnerPickupPointService);
    Mockito.verifyNoMoreInteractions(objectConverterService);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws IOException {
    Mockito.doThrow(RuntimeException.class).when(objectMapper)
        .readValue(mapper.writeValueAsString(pickupPointChange), PickupPointVOEventModel.class);
    pickupPointUpdateAllEventListener.onDomainEventConsumed(mapper.writeValueAsString(pickupPointChange));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(pickupPointChange), PickupPointVOEventModel.class);
  }

  @Test
  public void onDomainEventConsumedTest() throws IOException {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(pickupPointChange), PickupPointVOEventModel.class))
        .thenReturn(mapper.readValue(mapper.writeValueAsString(pickupPointChange), PickupPointVOEventModel.class));
    pickupPointUpdateAllEventListener.onDomainEventConsumed(mapper.writeValueAsString(pickupPointChange));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(pickupPointChange), PickupPointVOEventModel.class);
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPoint(pickupPointChange.getStoreId(), pickupPointChange.getBusinessPartnerCode(),
            CODE);
    Mockito.verify(businessPartnerPickupPointService).saveBusinessPartnerPickupPoint(Mockito.any());
    Mockito.verify(objectConverterService)
        .convertToBusinessPartnerPickupPointFromPickupPointChange(Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void onDomainEventConsumedNotNullTest() throws IOException {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(pickupPointChange), PickupPointVOEventModel.class))
        .thenReturn(mapper.readValue(mapper.writeValueAsString(pickupPointChange), PickupPointVOEventModel.class));
    Mockito.when(businessPartnerPickupPointService
        .getBusinessPartnerPickupPoint(pickupPointChange.getStoreId(), pickupPointChange.getBusinessPartnerCode(),
            pickupPointChange.getCode())).thenReturn(businessPartnerPickupPoint);
    pickupPointUpdateAllEventListener.onDomainEventConsumed(mapper.writeValueAsString(pickupPointChange));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(pickupPointChange), PickupPointVOEventModel.class);
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPoint(pickupPointChange.getStoreId(), pickupPointChange.getBusinessPartnerCode(),
            CODE);
    Mockito.verify(objectConverterService)
        .convertToBusinessPartnerPickupPointFromPickupPointChange(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(businessPartnerPickupPointService).saveBusinessPartnerPickupPoint(Mockito.any());
  }
}