package com.gdn.mta.bulk.listener.kafka;

import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.BulkUpdateService;

public class BulkUploadUpdateItemListenerTest {

  private static final String STORE_ID = "10001";
  private static final String PARENT_PRODUCT = "parentProduct";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private ObjectMapper mapper = new ObjectMapper();

  @InjectMocks
  private BulkUploadUpdateItemListener listener;

  @Mock
  private BulkUpdateService bulkUpdateService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private BulkUpdateEventModel bulkUpdateEventModel;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    bulkUpdateEventModel =
        BulkUpdateEventModel.builder().rowNumbers(Arrays.asList(1, 2)).bulkProcessCode(BULK_PROCESS_CODE)
            .businessPartnerCode(BUSINESS_PARTNER_CODE).storeId(STORE_ID).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkUpdateService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.doNothing().when(bulkUpdateService).processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    this.listener.onDomainEventConsumed(mapper.writeValueAsString(bulkUpdateEventModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
    Mockito.verify(bulkUpdateService).processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkUploadUpdateItem();
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    Mockito.doThrow(Exception.class).when(bulkUpdateService).processBulkUpdateItem(bulkUpdateEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.listener.onDomainEventConsumed(
          mapper.writeValueAsString(bulkUpdateEventModel)));
    } finally {
      Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
      Mockito.verify(bulkUpdateService).processBulkUpdateItem(bulkUpdateEventModel);
      Mockito.verify(kafkaTopicProperties, Mockito.times(4)).getBulkUploadUpdateItem();
    }
  }

  @Test
  public void onDomainEventConsumedForPriority1Seller() throws Exception {
    Mockito.doNothing().when(bulkUpdateService).processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    this.listener.onDomainEventConsumedForPriority1Seller(mapper.writeValueAsString(bulkUpdateEventModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
    Mockito.verify(bulkUpdateService).processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkUploadUpdateItemPriority1();
  }

  @Test
  public void onDomainEventConsumedForPriority1SellerException() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    Mockito.doThrow(Exception.class).when(bulkUpdateService).processBulkUpdateItem(bulkUpdateEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.listener.onDomainEventConsumedForPriority1Seller(
              mapper.writeValueAsString(bulkUpdateEventModel)));
    } finally {
      Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
      Mockito.verify(bulkUpdateService).processBulkUpdateItem(bulkUpdateEventModel);
      Mockito.verify(kafkaTopicProperties, Mockito.times(4)).getBulkUploadUpdateItemPriority1();
    }
  }

  @Test
  public void onDomainEventConsumedForPriority2Seller() throws Exception {
    Mockito.doNothing().when(bulkUpdateService).processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    this.listener.onDomainEventConsumedForPriority2Seller(mapper.writeValueAsString(bulkUpdateEventModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
    Mockito.verify(bulkUpdateService).processBulkUpdateItem(bulkUpdateEventModel);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkUploadUpdateItemPriority2();
  }

  @Test
  public void onDomainEventConsumedForPriority2SellerException() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class))
        .thenReturn(bulkUpdateEventModel);
    Mockito.doThrow(Exception.class).when(bulkUpdateService).processBulkUpdateItem(bulkUpdateEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.listener.onDomainEventConsumedForPriority2Seller(
              mapper.writeValueAsString(bulkUpdateEventModel)));
    } finally {
      Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(bulkUpdateEventModel), BulkUpdateEventModel.class);
      Mockito.verify(bulkUpdateService).processBulkUpdateItem(bulkUpdateEventModel);
      Mockito.verify(kafkaTopicProperties, Mockito.times(4)).getBulkUploadUpdateItemPriority2();
    }
  }
}
