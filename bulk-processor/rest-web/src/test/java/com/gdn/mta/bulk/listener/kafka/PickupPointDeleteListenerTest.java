package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.PickupPointDeleteProcessDTO;
import com.gdn.mta.bulk.service.PickupPointDeleteService;
import com.gdn.x.businesspartner.domain.event.enums.PickupPointChangeFields;
import com.gdn.x.businesspartner.domain.event.model.PickupPointChange;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class PickupPointDeleteListenerTest {

  @InjectMocks
  private PickupPointDeleteListener pickupPointDeleteListener;

  @Mock
  private PickupPointDeleteService pickupPointDeleteService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PICKUP_POINT = "pickup-point";
  private static final String STORE_ID = "store-id";
  private static final String MESSAGE = "MESSAGE";
  private static Set<String> changedFields = new HashSet<>();
  private static String WAITING_DELETION = "WAITING_DELETION";

  private PickupPointChange pickupPointChange;
  private PickupPointDeleteProcessDTO pickupPointDeleteProcessDTO;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    changedFields.add(WAITING_DELETION);
    pickupPointDeleteProcessDTO =
      PickupPointDeleteProcessDTO.builder().pickupPointCode(PICKUP_POINT).storeId(STORE_ID)
        .businessPartnerCode(BUSINESS_PARTNER_CODE).build();
    pickupPointChange = new PickupPointChange();
    pickupPointChange.setArchived(true);
    pickupPointChange.setWaitingDeletion(true);
    pickupPointChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointChange.setCode(PICKUP_POINT);
    pickupPointChange.setChangedFields(changedFields);
    pickupPointChange.setStoreId(STORE_ID);
    ReflectionTestUtils.setField(pickupPointDeleteListener, "sellerActionTypesToSkipPPDeletion",
      "");
    ReflectionTestUtils.setField(pickupPointDeleteListener, "changeFieldsForPPDeletion",
      WAITING_DELETION);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(pickupPointDeleteService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }


  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    Mockito.doNothing().when(pickupPointDeleteService)
      .processDeletePickupPointEvent(pickupPointDeleteProcessDTO);
    this.pickupPointDeleteListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(pickupPointDeleteService)
      .processDeletePickupPointEvent(pickupPointDeleteProcessDTO);
    Mockito.verify(objectMapper).readValue(MESSAGE, PickupPointChange.class);
    Mockito.verify(kafkaTopicProperties).getUpdateFields();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    Mockito.doThrow(ApplicationRuntimeException.class).when(pickupPointDeleteService)
      .processDeletePickupPointEvent(pickupPointDeleteProcessDTO);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.pickupPointDeleteListener.onDomainEventConsumed(MESSAGE));
    } finally {
      Mockito.verify(pickupPointDeleteService)
        .processDeletePickupPointEvent(pickupPointDeleteProcessDTO);
      Mockito.verify(objectMapper).readValue(MESSAGE, PickupPointChange.class);
      Mockito.verify(kafkaTopicProperties).getUpdateFields();
    }
  }

  @Test
  public void onDomainEventConsumedWithActionTypesTest() throws Exception {
    ReflectionTestUtils.setField(pickupPointDeleteListener, "sellerActionTypesToSkipPPDeletion",
      "SELLER_TERMINATION,SELLER_RESIGN");
    pickupPointChange.setActionTypes(Collections.singleton("SELLER_RESIGN"));
    Mockito.when(objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    Mockito.doNothing().when(pickupPointDeleteService)
      .processDeletePickupPointEvent(pickupPointDeleteProcessDTO);
    this.pickupPointDeleteListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(kafkaTopicProperties).getUpdateFields();
    Mockito.verify(objectMapper).readValue(MESSAGE, PickupPointChange.class);
  }

  @Test
  public void onDomainEventConsumedWithActionTypesNotEligibleTest() throws Exception {
    ReflectionTestUtils.setField(pickupPointDeleteListener, "sellerActionTypesToSkipPPDeletion",
      "SELLER_TERMINATION,SELLER_RESIGN");
    pickupPointChange.setActionTypes(Collections.singleton("RANDOM"));
    Mockito.when(objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    Mockito.doNothing().when(pickupPointDeleteService)
      .processDeletePickupPointEvent(pickupPointDeleteProcessDTO);
    this.pickupPointDeleteListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(kafkaTopicProperties).getUpdateFields();
    Mockito.verify(pickupPointDeleteService)
      .processDeletePickupPointEvent(pickupPointDeleteProcessDTO);
    Mockito.verify(objectMapper).readValue(MESSAGE, PickupPointChange.class);
  }

  @Test
  public void onDomainEventConsumedWithActionWithNoTypesInResponseTest() throws Exception {
    ReflectionTestUtils.setField(pickupPointDeleteListener, "sellerActionTypesToSkipPPDeletion",
      "SELLER_TERMINATION,SELLER_RESIGN");
    pickupPointChange.setActionTypes(Collections.emptySet());
    Mockito.when(objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    Mockito.doNothing().when(pickupPointDeleteService)
      .processDeletePickupPointEvent(pickupPointDeleteProcessDTO);
    this.pickupPointDeleteListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(pickupPointDeleteService)
      .processDeletePickupPointEvent(pickupPointDeleteProcessDTO);
    Mockito.verify(kafkaTopicProperties).getUpdateFields();
    Mockito.verify(objectMapper).readValue(MESSAGE, PickupPointChange.class);
  }

  @Test
  public void onDomainEventConsumedEmptyStoreIdExceptionTest() throws Exception {
    pickupPointChange.setStoreId(null);
    Mockito.when(objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.pickupPointDeleteListener.onDomainEventConsumed(MESSAGE));
    } finally {
      Mockito.verify(objectMapper).readValue(MESSAGE, PickupPointChange.class);
      Mockito.verify(kafkaTopicProperties).getUpdateFields();
    }
  }

  @Test
  public void onDomainEventConsumedEmptyBpCodeExceptionTest() throws Exception {
    pickupPointChange.setBusinessPartnerCode(null);
    Mockito.when(objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.pickupPointDeleteListener.onDomainEventConsumed(MESSAGE));
    } finally {
      Mockito.verify(objectMapper).readValue(MESSAGE, PickupPointChange.class);
      Mockito.verify(kafkaTopicProperties).getUpdateFields();
    }
  }

  @Test
  public void onDomainEventConsumedEmptyPpCodeExceptionTest() throws Exception {
    pickupPointChange.setCode(null);
    Mockito.when(objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.pickupPointDeleteListener.onDomainEventConsumed(MESSAGE));
    } finally {
      Mockito.verify(objectMapper).readValue(MESSAGE, PickupPointChange.class);
      Mockito.verify(kafkaTopicProperties).getUpdateFields();
    }
  }

  @Test
  public void onDomainEventConsumedNotArchivalTest() throws Exception {
    pickupPointChange.setArchived(false);
    pickupPointChange.setWaitingDeletion(false);
    Mockito.when(objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    this.pickupPointDeleteListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, PickupPointChange.class);
    Mockito.verify(kafkaTopicProperties).getUpdateFields();
  }

  @Test
  public void onDomainEventConsumedNoChangedFieldsTest() throws Exception {
    pickupPointChange.setChangedFields(new HashSet<>());
    Mockito.when(objectMapper.readValue(MESSAGE, PickupPointChange.class))
      .thenReturn(pickupPointChange);
    this.pickupPointDeleteListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, PickupPointChange.class);
    Mockito.verify(kafkaTopicProperties).getUpdateFields();
  }

}
