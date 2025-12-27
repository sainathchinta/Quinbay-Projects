package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletion;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletionData;
import com.gdn.mta.bulk.models.NeedRevisionDeletionEventModel;
import com.gdn.mta.bulk.service.NeedRevisionDeletionService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class BulkNeedRevisionDeletionListenerTest {

  @InjectMocks
  private BulkNeedRevisionDeletionListener bulkNeedRevisionDeletionListener;

  @Mock
  private NeedRevisionDeletionService needRevisionDeletionService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  public static final String MESSAGE = "{\"deletionProcessCode\":\"deletionProcessCode\"}";
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String DELETION_PROCESS_CODE = "deletionProcessCode";
  private static final String NR_DELETE_EVENT = "com.gdn.x.bulk.need.revision.deletion";
  private static final List<String> LIST_OF_IDS = List.of("Id", "Id2", "Id3");
  private NeedRevisionDeletionEventModel needRevisionDeletionEventModel =
      new NeedRevisionDeletionEventModel();
  private BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();

  @BeforeEach
  public void setup() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);
    needRevisionDeletionEventModel.setStoreId(STORE_ID);
    needRevisionDeletionEventModel.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    needRevisionDeletionEventModel.setDeletionProcessCode(DELETION_PROCESS_CODE);
    needRevisionDeletionEventModel.setNeedRevisionDeletionDataIds(List.of("Id", "Id2", "Id3"));

    bulkNeedRevisionDeletion.setStoreId(STORE_ID);
    bulkNeedRevisionDeletion.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkNeedRevisionDeletion.setDeletionProcessCode(DELETION_PROCESS_CODE);

    Mockito.when(objectMapper.readValue(MESSAGE, NeedRevisionDeletionEventModel.class))
        .thenReturn(needRevisionDeletionEventModel);
    Mockito.when(kafkaTopicProperties.getNeedRevisionDeletionEvent()).thenReturn(NR_DELETE_EVENT);
    ReflectionTestUtils.setField(bulkNeedRevisionDeletionListener, "batchSizeToPublishDataEntries",
        5);
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData = new BulkNeedRevisionDeletionData();
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData2 = new BulkNeedRevisionDeletionData();
    BulkNeedRevisionDeletionData bulkNeedRevisionDeletionData3 = new BulkNeedRevisionDeletionData();
    bulkNeedRevisionDeletionData.setProductCode(PRODUCT_CODE);
    bulkNeedRevisionDeletionData.setId("Id");
    bulkNeedRevisionDeletionData2.setId("Id2");
    bulkNeedRevisionDeletionData3.setId("Id3");
    Mockito.when(
        needRevisionDeletionService.fetchNeedRevisionDeletionDataByDeletionProcessCodeAndIds(
            STORE_ID, DELETION_PROCESS_CODE, LIST_OF_IDS)).thenReturn(
        List.of(bulkNeedRevisionDeletionData, bulkNeedRevisionDeletionData2,
            bulkNeedRevisionDeletionData3));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(needRevisionDeletionService, kafkaTopicProperties,
        objectMapper);
  }

  @Test
  void processBulkNeedRevisionDeletion_SuccessTest() throws Exception {
    Mockito.when(
        needRevisionDeletionService.fetchNeedRevisionDeletionByDeletionProcessCode(STORE_ID,
            DELETION_PROCESS_CODE)).thenReturn(bulkNeedRevisionDeletion);
    Mockito.doNothing().when(needRevisionDeletionService)
        .saveBulkNeedRevisionDeletion(bulkNeedRevisionDeletion);
    bulkNeedRevisionDeletionListener.processBulkNeedRevisionDeletion(MESSAGE);
    Mockito.verify(kafkaTopicProperties).getNeedRevisionDeletionEvent();
    Mockito.verify(objectMapper).readValue(MESSAGE, NeedRevisionDeletionEventModel.class);
    Mockito.verify(needRevisionDeletionService)
        .fetchNeedRevisionDeletionDataByDeletionProcessCodeAndIds(STORE_ID, DELETION_PROCESS_CODE,
            LIST_OF_IDS);
    Mockito.verify(needRevisionDeletionService).saveBulkNeedRevisionDeletionData(Mockito.any());
    Mockito.verify(needRevisionDeletionService)
        .performEligibilityCheckAndProcessDataDeletion(Mockito.anyString(), Mockito.anyList(),
            Mockito.anyString());
  }

  @Test
  void processBulkNeedRevisionDeletion_FailureTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(objectMapper)
        .readValue(MESSAGE, NeedRevisionDeletionEventModel.class);
    try {
      bulkNeedRevisionDeletionListener.processBulkNeedRevisionDeletion(MESSAGE);
    } finally {
      Mockito.verify(objectMapper).readValue(MESSAGE, NeedRevisionDeletionEventModel.class);
    }
  }
}