package com.gdn.partners.product.analytics.service.impl.domainEvent;

import static org.mockito.MockitoAnnotations.initMocks;

import java.io.IOException;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.service.impl.SellerDataWrapperServiceImpl;
import model.OxfordMerchantEvent;

public class OxfordMerchantEventListenerTest {

  private ObjectMapper mapper;

  @InjectMocks
  private OxfordMerchantEventListener oxfordMerchantEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private SellerDataWrapperServiceImpl sellerDataWrapperService;

  @BeforeEach
  public void setUp() {
    initMocks(this);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(sellerDataWrapperService);
  }

  @Test
  public void onDomainEventConsumedEmptyTest() throws IOException {
    oxfordMerchantEventListener.onDomainEventConsumed(StringUtils.SPACE);
    Mockito.verify(objectMapper).readValue(StringUtils.SPACE, OxfordMerchantEvent.class);
  }

  @Test
  public void onDomainEventConsumedTest() throws IOException {
    Mockito
        .when(objectMapper.readValue(mapper.writeValueAsString(new OxfordMerchantEvent()), OxfordMerchantEvent.class))
        .thenReturn(new OxfordMerchantEvent());
    oxfordMerchantEventListener.onDomainEventConsumed(mapper.writeValueAsString(new OxfordMerchantEvent()));
    Mockito.verify(objectMapper)
        .readValue(mapper.writeValueAsString(new OxfordMerchantEvent()), OxfordMerchantEvent.class);
    Mockito.verify(sellerDataWrapperService).updateOfficialStoreFlagForASeller(null, false);
  }
}