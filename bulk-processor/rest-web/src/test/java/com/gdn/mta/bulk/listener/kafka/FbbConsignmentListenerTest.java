package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.ArgumentMatchers.eq;

import java.util.ArrayList;
import java.util.Arrays;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.FbbItem;
import com.gdn.mta.bulk.entity.FbbConsignmentEventModel;
import com.gdn.mta.bulk.service.FbbConsignmentService;

public class FbbConsignmentListenerTest {

  @InjectMocks
  private FbbConsignmentListener fbbConsignmentListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private FbbConsignmentService fbbConsignmentService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private static final String MESSAGE = "MESSAGE";
  private static final String CONSIGNMENT_ID_1 = "consignment-id-1";
  private static final String ITEM_SKU_1 = "item-sku-1";
  private static final String ITEM_SKU_2 = "item-sku-2";
  private static final String BUSINESS_PARTNER_CODE = "business-partner-code";

  private FbbItem fbbItem1 = new FbbItem();
  private FbbItem fbbItem2 = new FbbItem();
  private FbbConsignmentEventModel fbbConsignmentEventModel = new FbbConsignmentEventModel();

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    fbbItem1.setItemSku(ITEM_SKU_1);
    fbbItem2.setItemSku(ITEM_SKU_2);
    fbbItem1.setBuyable(true);
    fbbItem1.setDiscoverable(true);
    fbbItem2.setBuyable(false);
    fbbItem2.setDiscoverable(false);
    fbbConsignmentEventModel.setConsignmentId(CONSIGNMENT_ID_1);
    fbbConsignmentEventModel.setFbbItems(Arrays.asList(fbbItem1, fbbItem2));
    fbbConsignmentEventModel.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(fbbConsignmentService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper
      .readValue(eq(MESSAGE), Mockito.any(TypeReference.class))).thenReturn(fbbConsignmentEventModel);
    fbbConsignmentListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(fbbConsignmentService)
      .preProcessFbbConsignmentCreation(fbbConsignmentEventModel);
    Mockito.verify(objectMapper).readValue(Mockito.eq(MESSAGE), Mockito.any(TypeReference.class));
    Mockito.verify(kafkaTopicProperties).getFbbCreateConsignment();
  }

  @Test
  public void onDomainEventConsumedEmptyBusinessPartnerTest() throws Exception {
    fbbConsignmentEventModel.setBusinessPartnerCode(StringUtils.EMPTY);
    Mockito.when(objectMapper
      .readValue(eq(MESSAGE), Mockito.any(TypeReference.class))).thenReturn(fbbConsignmentEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> fbbConsignmentListener.onDomainEventConsumed(MESSAGE));
    }
    finally {
      Mockito.verify(objectMapper).readValue(Mockito.eq(MESSAGE), Mockito.any(TypeReference.class));
      Mockito.verify(kafkaTopicProperties).getFbbCreateConsignment();
    }
  }

  @Test
  public void onDomainEventConsumedEmptyConsignmentTest() throws Exception {
    fbbConsignmentEventModel.setConsignmentId(StringUtils.EMPTY);
    Mockito.when(objectMapper
      .readValue(eq(MESSAGE), Mockito.any(TypeReference.class))).thenReturn(fbbConsignmentEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> fbbConsignmentListener.onDomainEventConsumed(MESSAGE));
    }
    finally {
      Mockito.verify(objectMapper).readValue(Mockito.eq(MESSAGE), Mockito.any(TypeReference.class));
      Mockito.verify(kafkaTopicProperties).getFbbCreateConsignment();
    }
  }

  @Test
  public void onDomainEventConsumedEmptyItemListTest() throws Exception {
    fbbConsignmentEventModel.setFbbItems(new ArrayList<>());
    Mockito.when(objectMapper
      .readValue(eq(MESSAGE), Mockito.any(TypeReference.class))).thenReturn(fbbConsignmentEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> fbbConsignmentListener.onDomainEventConsumed(MESSAGE));
    }
    finally {
      Mockito.verify(objectMapper).readValue(Mockito.eq(MESSAGE), Mockito.any(TypeReference.class));
      Mockito.verify(kafkaTopicProperties).getFbbCreateConsignment();
    }
  }
}
