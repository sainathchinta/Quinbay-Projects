package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.MockitoAnnotations.initMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.service.CacheEvictService;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;

class BusinessPartnerChangeListenerTest {

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private CacheEvictService cacheEvictService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BusinessPartnerChangeListener businessPartnerChangeListener;

  private BusinessPartnerChange businessPartnerChange;
  private String message;

  private static final String BUSINESS_PARTNER_CODE = "bpCode";

  @BeforeEach
  public void init() throws JsonProcessingException {
    initMocks(this);
    businessPartnerChange = new BusinessPartnerChange();
    businessPartnerChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    message = new ObjectMapper().writeValueAsString(businessPartnerChange);
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(cacheEvictService);
  }

  @Test
  void onDomainEventConsumedException() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class)
        .when(objectMapper).readValue(BUSINESS_PARTNER_CODE, BusinessPartnerChange.class);
    businessPartnerChangeListener.onDomainEventConsumed(BUSINESS_PARTNER_CODE);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBusinessPartnerChangeEvent();
    Mockito.verify(objectMapper).readValue(BUSINESS_PARTNER_CODE, BusinessPartnerChange.class);
  }

  @Test
  void onDomainEventConsumed() throws Exception {
    BusinessPartnerChange businessPartnerChange = new BusinessPartnerChange();
    businessPartnerChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(objectMapper.readValue(BUSINESS_PARTNER_CODE, BusinessPartnerChange.class))
        .thenReturn(businessPartnerChange);
    businessPartnerChangeListener.onDomainEventConsumed(BUSINESS_PARTNER_CODE);
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerChangeEvent();
    Mockito.verify(objectMapper).readValue(BUSINESS_PARTNER_CODE, BusinessPartnerChange.class);
    Mockito.verify(cacheEvictService).evictBusinessPartnerCache(BUSINESS_PARTNER_CODE);
  }
}