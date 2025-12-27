package com.gdn.x.product.service.event.listener;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.service.api.BusinessPartnerService;

public class BusinessPartnerUpdateEventListenerTest {

  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String MESSAGE = "message";

  private BusinessPartnerChange businessPartnerChange;

  @InjectMocks
  private BusinessPartnerUpdateEventListener businessPartnerUpdateEventListener;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private ObjectMapper objectMapper;

  @Captor
  private ArgumentCaptor<BusinessPartnerChange> businessPartnerChangeArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    businessPartnerChange = new BusinessPartnerChange();
    businessPartnerChange.setStoreId(Constants.STORE_ID);
    businessPartnerChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(businessPartnerService);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, BusinessPartnerChange.class))
      .thenReturn(businessPartnerChange);
    businessPartnerUpdateEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BusinessPartnerChange.class);
    Mockito.verify(businessPartnerService).upsertBusinessPartner(businessPartnerChangeArgumentCaptor.capture());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, businessPartnerChangeArgumentCaptor.getValue().getBusinessPartnerCode());
    Assertions.assertEquals(Constants.STORE_ID, businessPartnerChangeArgumentCaptor.getValue().getStoreId());
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(businessPartnerService).upsertBusinessPartner(businessPartnerChange);
    Mockito.when(this.objectMapper.readValue(MESSAGE, BusinessPartnerChange.class))
      .thenReturn(businessPartnerChange);
    businessPartnerUpdateEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, BusinessPartnerChange.class);
    Mockito.verify(businessPartnerService).upsertBusinessPartner(businessPartnerChangeArgumentCaptor.capture());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, businessPartnerChangeArgumentCaptor.getValue().getBusinessPartnerCode());
    Assertions.assertEquals(Constants.STORE_ID, businessPartnerChangeArgumentCaptor.getValue().getStoreId());
  }


}