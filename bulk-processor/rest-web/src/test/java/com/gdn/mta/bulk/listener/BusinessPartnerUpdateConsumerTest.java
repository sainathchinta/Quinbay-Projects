package com.gdn.mta.bulk.listener;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.SellerProcessType;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.DormantSellerEvent;
import com.gdn.mta.bulk.service.DormantSellerService;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.businesspartner.domain.event.model.CompanyVO;

import org.checkerframework.checker.units.qual.C;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

public class BusinessPartnerUpdateConsumerTest {

  private static final String BUSINESS_PARTNER_CODE = "BPX-10000";
  private static final String STORE_ID = "10001";
  private final String MERCHANT_STATUS = "MERCHANT_STATUS";
  private static final String EVENT = "com.gdn.x.businesspartner.profile.update.fields";

  @InjectMocks
  private BusinessPartnerUpdateConsumer consumer;

  @Mock
  private DormantSellerService dormantSellerService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private ObjectMapper mapper = new ObjectMapper();
  private BusinessPartnerChange businessPartnerChange;
  private String payload;

  @BeforeEach
  public void initialize() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);
    businessPartnerChange = new BusinessPartnerChange();
    businessPartnerChange.setMerchantStatus("INACTIVE");
    businessPartnerChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    payload = mapper.writeValueAsString(businessPartnerChange);
    ReflectionTestUtils.setField(consumer, "inactiveMerchantStatusList", "INACTIVE");
    ReflectionTestUtils.setField(consumer, "terminatedMerchantStatusList", "TERMINATED");
    ReflectionTestUtils.setField(consumer, "dormantSellerStatuses", "PARTIAL_COMPLETED,FAILED,COMPLETED");

    when(kafkaTopicProperties.getBusinessPartnerUpdateEvent()).thenReturn(EVENT);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(dormantSellerService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedSuspendedTest() throws Exception {
    DormantSellerEvent dormantSellerEvent = new DormantSellerEvent();
    dormantSellerEvent.setStatus("IN_PROGRESS");
    dormantSellerEvent.setUpdatedDate(new Date(2023, 2, 2));
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class)).thenReturn(businessPartnerChange);
    Mockito.when(dormantSellerService.findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(STORE_ID,BUSINESS_PARTNER_CODE,
        SellerProcessType.SUSPEND.name())).thenReturn(Arrays.asList(dormantSellerEvent));
    Mockito.doNothing().when(dormantSellerService).processSellerDeactivate(businessPartnerChange.getBusinessPartnerCode(),
        SellerProcessType.SUSPEND.name());

    consumer.onDomainEventConsumed(payload);

    Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
    Mockito.verify(dormantSellerService, times(2))
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerUpdateEvent();
  }

  @Test
  public void onDomainEventConsumedSkipSuspensionTest() throws Exception {
    DormantSellerEvent dormantSellerEvent = new DormantSellerEvent();
    dormantSellerEvent.setStatus("IN_PROGRESS");
    dormantSellerEvent.setUpdatedDate(new Date(2023, 2, 2));
    ReflectionTestUtils.setField(consumer, "sellerPenaltyEnabledPhase2", true, boolean.class);
    businessPartnerChange.setSuspensionFlag(true);
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class))
        .thenReturn(businessPartnerChange);
    consumer.onDomainEventConsumed(payload);

    Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
    Mockito.verify(dormantSellerService, times(0))
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerUpdateEvent();
  }

  @Test
  public void onDomainEventConsumedSuspensionFlagFalseTest() throws Exception {
    DormantSellerEvent dormantSellerEvent = new DormantSellerEvent();
    dormantSellerEvent.setStatus("IN_PROGRESS");
    dormantSellerEvent.setUpdatedDate(new Date(2023, 2, 2));
    ReflectionTestUtils.setField(consumer, "sellerPenaltyEnabledPhase2", true, boolean.class);
    businessPartnerChange.setSuspensionFlag(false);
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class))
        .thenReturn(businessPartnerChange);
    Mockito.when(
            dormantSellerService.findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(
                STORE_ID, BUSINESS_PARTNER_CODE, SellerProcessType.SUSPEND.name()))
        .thenReturn(Arrays.asList(dormantSellerEvent));
    Mockito.doNothing().when(dormantSellerService)
        .processSellerDeactivate(businessPartnerChange.getBusinessPartnerCode(),
            SellerProcessType.SUSPEND.name());

    consumer.onDomainEventConsumed(payload);

    Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
    Mockito.verify(dormantSellerService, times(2))
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerUpdateEvent();
  }
  @Test
  public void onDomainEventConsumedTerminatedTest() throws Exception {
    DormantSellerEvent dormantSellerEvent = new DormantSellerEvent();
    dormantSellerEvent.setStatus("IN_PROGRESS");
    dormantSellerEvent.setUpdatedDate(new Date(2023, 2, 2));
    businessPartnerChange.setMerchantStatus("TERMINATED");
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class)).thenReturn(businessPartnerChange);
    Mockito.when(dormantSellerService
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(STORE_ID, BUSINESS_PARTNER_CODE,
            SellerProcessType.TERMINATED.name())).thenReturn(Arrays.asList(dormantSellerEvent));
    Mockito.doNothing().when(dormantSellerService).processSellerDeactivate(businessPartnerChange.getBusinessPartnerCode(),
        SellerProcessType.TERMINATED.name());

    consumer.onDomainEventConsumed(payload);

    Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
    Mockito.verify(dormantSellerService, times(2))
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerUpdateEvent();
  }

  @Test
  public void onDomainEventConsumedTerminatedTest4() throws Exception {
    businessPartnerChange.setMerchantStatus("TERMINATED");
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class)).thenReturn(businessPartnerChange);
    Mockito.when(dormantSellerService
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(STORE_ID, BUSINESS_PARTNER_CODE,
            SellerProcessType.TERMINATED.name())).thenReturn(null);
    Mockito.when(dormantSellerService
        .findByBusinessPartnerCodeAndProcessType(BUSINESS_PARTNER_CODE, SellerProcessType.TERMINATED.name()))
        .thenReturn(new DormantSellerEvent());
    Mockito.doNothing().when(dormantSellerService).processSellerDeactivate(businessPartnerChange.getBusinessPartnerCode(),
        SellerProcessType.TERMINATED.name());
    businessPartnerChange = new BusinessPartnerChange();
    businessPartnerChange.setMerchantStatus("INACTIVE");
    businessPartnerChange.setActivated(false);
    businessPartnerChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    payload = mapper.writeValueAsString(businessPartnerChange);
    consumer.onDomainEventConsumed(payload);

    Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
    Mockito.verify(dormantSellerService, times(2))
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
        Mockito.verify(dormantSellerService).processSellerDeactivate(businessPartnerChange.getBusinessPartnerCode(),
            SellerProcessType.TERMINATED.name());
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerUpdateEvent();

  }

  @Test
  public void onDomainEventConsumedCheckMerchantStatusTest() throws Exception {
    ReflectionTestUtils.setField(consumer, "checkMerchantStatusChanged", true);
    businessPartnerChange.setMerchantStatus("TERMINATED");
    businessPartnerChange.setActivated(false);
    businessPartnerChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    businessPartnerChange.setCompany(new CompanyVO());
    businessPartnerChange.getCompany().setChangedFields(Collections.singleton(MERCHANT_STATUS));
    payload = mapper.writeValueAsString(businessPartnerChange);
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class)).thenReturn(businessPartnerChange);
    Mockito.when(dormantSellerService
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(STORE_ID, BUSINESS_PARTNER_CODE,
            SellerProcessType.TERMINATED.name())).thenReturn(null);
    Mockito.when(dormantSellerService
            .findByBusinessPartnerCodeAndProcessType(BUSINESS_PARTNER_CODE, SellerProcessType.TERMINATED.name()))
        .thenReturn(new DormantSellerEvent());
    Mockito.doNothing().when(dormantSellerService).processSellerDeactivate(businessPartnerChange.getBusinessPartnerCode(),
        SellerProcessType.TERMINATED.name());
    consumer.onDomainEventConsumed(payload);
    Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
    Mockito.verify(dormantSellerService, times(2))
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    Mockito.verify(dormantSellerService).processSellerDeactivate(businessPartnerChange.getBusinessPartnerCode(),
        SellerProcessType.TERMINATED.name());
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerUpdateEvent();
  }

  @Test
  public void onDomainEventConsumedCheckMerchantStatusCompanyNullTest() throws Exception {
    ReflectionTestUtils.setField(consumer, "checkMerchantStatusChanged", true);
    businessPartnerChange.setMerchantStatus("TERMINATED");
    businessPartnerChange.setActivated(false);
    businessPartnerChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    payload = mapper.writeValueAsString(businessPartnerChange);
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class)).thenReturn(businessPartnerChange);
    Mockito.when(dormantSellerService
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(STORE_ID, BUSINESS_PARTNER_CODE,
            SellerProcessType.TERMINATED.name())).thenReturn(null);
    Mockito.when(dormantSellerService
            .findByBusinessPartnerCodeAndProcessType(BUSINESS_PARTNER_CODE, SellerProcessType.TERMINATED.name()))
        .thenReturn(new DormantSellerEvent());
    Mockito.doNothing().when(dormantSellerService).processSellerDeactivate(businessPartnerChange.getBusinessPartnerCode(),
        SellerProcessType.TERMINATED.name());
    consumer.onDomainEventConsumed(payload);
    Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
    Mockito.verify(dormantSellerService, times(2))
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerUpdateEvent();
  }

  @Test
  public void onDomainEventConsumedCheckMerchantStatusChangeEmptyTest() throws Exception {
    ReflectionTestUtils.setField(consumer, "checkMerchantStatusChanged", true);
    businessPartnerChange.setMerchantStatus("TERMINATED");
    businessPartnerChange.setActivated(false);
    businessPartnerChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    businessPartnerChange.setCompany(new CompanyVO());
    payload = mapper.writeValueAsString(businessPartnerChange);
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class)).thenReturn(businessPartnerChange);
    Mockito.when(dormantSellerService
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(STORE_ID, BUSINESS_PARTNER_CODE,
            SellerProcessType.TERMINATED.name())).thenReturn(null);
    Mockito.when(dormantSellerService
            .findByBusinessPartnerCodeAndProcessType(BUSINESS_PARTNER_CODE, SellerProcessType.TERMINATED.name()))
        .thenReturn(new DormantSellerEvent());
    Mockito.doNothing().when(dormantSellerService).processSellerDeactivate(businessPartnerChange.getBusinessPartnerCode(),
        SellerProcessType.TERMINATED.name());
    consumer.onDomainEventConsumed(payload);
    Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
    Mockito.verify(dormantSellerService, times(2))
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerUpdateEvent();
  }

  @Test
  public void onDomainEventConsumedCheckMerchantStatusChangeTypeTest() throws Exception {
    ReflectionTestUtils.setField(consumer, "checkMerchantStatusChanged", true);
    businessPartnerChange.setMerchantStatus("TERMINATED");
    businessPartnerChange.setActivated(false);
    businessPartnerChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    businessPartnerChange.setCompany(new CompanyVO());
    businessPartnerChange.getCompany().setChangedFields(Collections.singleton(BUSINESS_PARTNER_CODE));
    payload = mapper.writeValueAsString(businessPartnerChange);
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class)).thenReturn(businessPartnerChange);
    Mockito.when(dormantSellerService
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(STORE_ID, BUSINESS_PARTNER_CODE,
            SellerProcessType.TERMINATED.name())).thenReturn(null);
    Mockito.when(dormantSellerService
            .findByBusinessPartnerCodeAndProcessType(BUSINESS_PARTNER_CODE, SellerProcessType.TERMINATED.name()))
        .thenReturn(new DormantSellerEvent());
    Mockito.doNothing().when(dormantSellerService).processSellerDeactivate(businessPartnerChange.getBusinessPartnerCode(),
        SellerProcessType.TERMINATED.name());
    consumer.onDomainEventConsumed(payload);
    Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
    Mockito.verify(dormantSellerService, times(2))
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerUpdateEvent();
  }

  @Test
  public void onDomainEventConsumedTerminatedNoTypeTest() throws Exception {
    businessPartnerChange.setMerchantStatus("RESIGNED");
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class)).thenReturn(businessPartnerChange);

    consumer.onDomainEventConsumed(payload);

    Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerUpdateEvent();
  }

  @Test
  public void onDomainEventConsumedEntryAlreadyExistsTest() throws Exception {
    DormantSellerEvent dormantSellerEvent = new DormantSellerEvent();
    dormantSellerEvent.setStatus("FETCHED");
    dormantSellerEvent.setCreatedDate(new Date(2023,2,1));
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class)).thenReturn(businessPartnerChange);
    Mockito.when(dormantSellerService
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(dormantSellerEvent));

    consumer.onDomainEventConsumed(payload);

    Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
    Mockito.verify(dormantSellerService, times(2))
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(STORE_ID, BUSINESS_PARTNER_CODE,
            SellerProcessType.SUSPEND.name());
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerUpdateEvent();
  }

  @Test
  public void onDomainEventConsumedEntryBusinessPartnerActivatedTest() throws Exception {
    DormantSellerEvent dormantSellerEvent = new DormantSellerEvent();
    DormantSellerEvent dormantSellerEvent2 = new DormantSellerEvent();
    DormantSellerEvent dormantSellerEvent3 = new DormantSellerEvent();
    dormantSellerEvent3.setStatus("COMPLETED");
    dormantSellerEvent.setStatus("IN_PROGRESS");
    dormantSellerEvent2.setStatus("IN_PROGRESS");
    dormantSellerEvent.setUpdatedDate(new Date(2023));
    dormantSellerEvent2.setUpdatedDate(new Date());
    dormantSellerEvent3.setUpdatedDate(new Date());
    Mockito.when(dormantSellerService
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Arrays.asList(dormantSellerEvent, dormantSellerEvent2, dormantSellerEvent3));
    businessPartnerChange.setActivated(true);
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class)).thenReturn(businessPartnerChange);

    consumer.onDomainEventConsumed(payload);

    Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
    Mockito.verify(dormantSellerService)
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    Mockito.verify(dormantSellerService).saveDormantSellerEvents(Mockito.anyList());
    Mockito.verify(kafkaTopicProperties).getBusinessPartnerUpdateEvent();
  }

  @Test
  public void onDomainEventConsumedErrorTest() throws Exception {
    Mockito.when(objectMapper.readValue(payload, BusinessPartnerChange.class)).thenThrow(JsonProcessingException.class);
    try {
      consumer.onDomainEventConsumed(payload);
    } finally {
      Mockito.verify(objectMapper).readValue(payload, BusinessPartnerChange.class);
      Mockito.verify(kafkaTopicProperties, times(2)).getBusinessPartnerUpdateEvent();
    }
  }
}
