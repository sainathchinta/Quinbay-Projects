package com.gdn.x.product.service.impl;

import com.gdn.x.product.dao.api.KafkaEventLoggerRepository;
import com.gdn.x.product.model.entity.KafkaEventLogger;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.verifyNoMoreInteractions;

public class KafkaEventLoggerServiceImplTest {

  private static final String STORE_ID = "storeId";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final long TIMESTAMP = 1L;
  private static final String TOPIC = "topic";
  private static final String ID = "id";

  private KafkaEventLogger kafkaEventLogger = KafkaEventLogger.builder().build();

  @Mock
  private KafkaEventLoggerRepository kafkaEventLoggerRepository;

  @InjectMocks
  private KafkaEventLoggerServiceImpl kafkaEventLoggerService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(kafkaEventLoggerRepository);
  }

  @Test
  public void findEventByTimestampAndIdentifiersAndMarkForDeleteFalse() {
    Mockito.when(
      this.kafkaEventLoggerRepository.findByStoreIdAndTimestampAndPrimaryIdentifierAndSecondaryIdentifierAndMarkForDeleteFalse(
        STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, MERCHANT_CODE)).thenReturn(
      KafkaEventLogger.builder().primaryIdentifier(PICKUP_POINT_CODE)
        .secondaryIdentifier(MERCHANT_CODE).build());
    KafkaEventLogger kafkaEventLogger =
      this.kafkaEventLoggerService.findEventByTimestampAndIdentifiersAndMarkForDeleteFalse(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, MERCHANT_CODE);
    Mockito.verify(
      this.kafkaEventLoggerRepository).findByStoreIdAndTimestampAndPrimaryIdentifierAndSecondaryIdentifierAndMarkForDeleteFalse(
        STORE_ID, TIMESTAMP, PICKUP_POINT_CODE, MERCHANT_CODE);
    Assertions.assertEquals(PICKUP_POINT_CODE, kafkaEventLogger.getPrimaryIdentifier());
    Assertions.assertEquals(MERCHANT_CODE, kafkaEventLogger.getSecondaryIdentifier());
  }

  @Test
  public void insertKafkaEventLoggerTest() {
    this.kafkaEventLoggerService.insertKafkaEventLogger(STORE_ID, TIMESTAMP, PICKUP_POINT_CODE,
      MERCHANT_CODE, TOPIC, StringUtils.EMPTY);
    Mockito.verify(this.kafkaEventLoggerRepository).save(Mockito.any(KafkaEventLogger.class));
  }

  @Test
  public void updateKafkaEventToFinishedTest() {
    Mockito.when(this.kafkaEventLoggerRepository.findOne(ID)).thenReturn(kafkaEventLogger);
    this.kafkaEventLoggerService.updateKafkaEventToFinished(ID);
    Mockito.verify(this.kafkaEventLoggerRepository).findOne(ID);
    Mockito.verify(this.kafkaEventLoggerRepository).save(Mockito.any(KafkaEventLogger.class));
  }

  @Test
  public void updateKafkaEventToFinished_nullObjectTest() {
    Mockito.when(this.kafkaEventLoggerRepository.findOne(ID)).thenReturn(null);
    this.kafkaEventLoggerService.updateKafkaEventToFinished(ID);
    Mockito.verify(this.kafkaEventLoggerRepository).findOne(ID);
  }

  @Test
  public void updateKafkaEventToFinished_emptyIdTest() {
    this.kafkaEventLoggerService.updateKafkaEventToFinished(StringUtils.EMPTY);
  }
}