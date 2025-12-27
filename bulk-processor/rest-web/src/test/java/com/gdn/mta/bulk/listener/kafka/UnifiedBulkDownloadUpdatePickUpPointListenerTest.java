package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.UnifiedBulkDownloadService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.domain.event.model.PickupPointChange;

public class UnifiedBulkDownloadUpdatePickUpPointListenerTest {

  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";

  private PickupPointChange pickupPointChange;

  @InjectMocks
  private UnifiedBulkDownloadUpdatePickUpPointListener listener;

  @Mock
  private UnifiedBulkDownloadService unifiedBulkDownloadService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);

    pickupPointChange = new PickupPointChange();
    pickupPointChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  public void listenMessage() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, PickupPointChange.class)).thenReturn(pickupPointChange);
    Mockito.doNothing().when(this.unifiedBulkDownloadService).updatePickUpPointFlag(BUSINESS_PARTNER_CODE);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(this.unifiedBulkDownloadService).updatePickUpPointFlag(BUSINESS_PARTNER_CODE);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, PickupPointChange.class);
  }

  @Test
  public void listenMessageWhenError() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, PickupPointChange.class)).thenReturn(pickupPointChange);
    Mockito.doThrow(RuntimeException.class).when(unifiedBulkDownloadService)
        .updatePickUpPointFlag(BUSINESS_PARTNER_CODE);
    try {
      listener.onDomainEventConsumed(Constant.CLIENT_ID);
    } finally {
      Mockito.verify(this.unifiedBulkDownloadService).updatePickUpPointFlag(BUSINESS_PARTNER_CODE);
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, PickupPointChange.class);
    }
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(unifiedBulkDownloadService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }
}
