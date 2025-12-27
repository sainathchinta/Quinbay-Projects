package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.header.internals.RecordHeaders;
import org.apache.kafka.common.record.TimestampType;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

class SivaItemCollectionBatchDeduplicationListenerTest {

    // Constants for test data
    private static final String TEST_TOPIC = Topics.NEW_SIVA_ITEM_COLLECTION_UPDATE_EVENT;
    private static final String TEST_GROUP_ID = Topics.GROUP_ID_SIVA_BOTH_BY_ITEM;
    private static final int TEST_PARTITION = 0;
    private static final long TEST_OFFSET_1 = 0L;
    private static final long TEST_OFFSET_2 = 1L;
    private static final long TEST_OFFSET_3 = 2L;
    private static final long TEST_TIMESTAMP = System.currentTimeMillis();
    private static final TimestampType TEST_TIMESTAMP_TYPE = TimestampType.CREATE_TIME;
    private static final long TEST_CHECKSUM = ConsumerRecord.NULL_CHECKSUM;
    private static final int TEST_SERIALIZED_KEY_SIZE = 4;
    private static final int TEST_SERIALIZED_VALUE_SIZE = 10;
    private static final String TEST_KEY_1 = "key1";
    private static final String TEST_KEY_2 = "key2";
    private static final String TEST_KEY_3 = "key3";
    private static final String TEST_VALUE_1 = "test-value-1";
    private static final String TEST_VALUE_2 = "test-value-2";
    private static final String TEST_VALUE_3 = "test-value-3";

    @Mock
    private ListenerService listenerService;

    @InjectMocks
    private SivaItemCollectionBatchDeduplicationListener batchDeduplicationListener;

    private AutoCloseable closeable;

    @BeforeEach
    void setUp() {
        closeable = MockitoAnnotations.openMocks(this);
    }

    @AfterEach
    void tearDown() throws Exception {
        closeable.close();
    }

    private ConsumerRecord<String, String> createTestConsumerRecord(String key, String value, long offset) {
        return new ConsumerRecord<>(
            TEST_TOPIC, TEST_PARTITION, offset, TEST_TIMESTAMP,
            TEST_TIMESTAMP_TYPE, TEST_CHECKSUM, TEST_SERIALIZED_KEY_SIZE, TEST_SERIALIZED_VALUE_SIZE,
            key, value, new RecordHeaders(), null
        );
    }


    @Test
    void testProcessNewItemDataUpdateEventBatch_MultipleRecords_Success() throws JsonProcessingException {
        ConsumerRecord<String, String> record1 = createTestConsumerRecord(TEST_KEY_1, TEST_VALUE_1, TEST_OFFSET_1);
        ConsumerRecord<String, String> record2 = createTestConsumerRecord(TEST_KEY_2, TEST_VALUE_2, TEST_OFFSET_2);
        ConsumerRecord<String, String> record3 = createTestConsumerRecord(TEST_KEY_3, TEST_VALUE_3, TEST_OFFSET_3);
        List<ConsumerRecord<String, String>> records = Arrays.asList(record1, record2, record3);
        doNothing().when(listenerService).processSivaItemUpdateQueueEventBatch(records);
        batchDeduplicationListener.processNewItemDataUpdateEventBatch(records);
        verify(listenerService, times(1)).processSivaItemUpdateQueueEventBatch(eq(records));
    }


    @Test
    void testProcessNewItemDataUpdateEventBatch_NullRecordsList() throws JsonProcessingException {
        List<ConsumerRecord<String, String>> nullRecords = null;

        doNothing().when(listenerService).processSivaItemUpdateQueueEventBatch(nullRecords);
        batchDeduplicationListener.processNewItemDataUpdateEventBatch(nullRecords);

        verify(listenerService, times(1)).processSivaItemUpdateQueueEventBatch(eq(nullRecords));
    }

    @Test
    void testProcessNewItemDataUpdateEventBatch_RecordsWithSameKeys_Success() throws JsonProcessingException {
        ConsumerRecord<String, String> record1 = createTestConsumerRecord(TEST_KEY_1, TEST_VALUE_1, TEST_OFFSET_1);
        ConsumerRecord<String, String> record2 = createTestConsumerRecord(TEST_KEY_1, TEST_VALUE_2, TEST_OFFSET_2);
        ConsumerRecord<String, String> record3 = createTestConsumerRecord(TEST_KEY_1, TEST_VALUE_3, TEST_OFFSET_3);
        List<ConsumerRecord<String, String>> duplicateKeyRecords = Arrays.asList(record1, record2, record3);
        doNothing().when(listenerService).processSivaItemUpdateQueueEventBatch(duplicateKeyRecords);
        batchDeduplicationListener.processNewItemDataUpdateEventBatch(duplicateKeyRecords);
        verify(listenerService, times(1)).processSivaItemUpdateQueueEventBatch(eq(duplicateKeyRecords));
    }

    @Test
    void testProcessNewItemDataUpdateEventBatch_VerifyNoInteractionOnException() throws JsonProcessingException {
        ConsumerRecord<String, String> record = createTestConsumerRecord(TEST_KEY_1, TEST_VALUE_1, TEST_OFFSET_1);
        List<ConsumerRecord<String, String>> records = Collections.singletonList(record);
        doThrow(new RuntimeException("Exception")).when(listenerService)
            .processSivaItemUpdateQueueEventBatch(records);
        assertThrows(RuntimeException.class, () -> {
            batchDeduplicationListener.processNewItemDataUpdateEventBatch(records);
        });
        verify(listenerService, times(1)).processSivaItemUpdateQueueEventBatch(eq(records));
    }

    @Test
    void testProcessNewItemDataUpdateEventBatch_VerifyBatchProcessingBehavior() throws JsonProcessingException {
        ConsumerRecord<String, String> record1 = createTestConsumerRecord(TEST_KEY_1, TEST_VALUE_1, TEST_OFFSET_1);
        ConsumerRecord<String, String> record2 = createTestConsumerRecord(TEST_KEY_2, TEST_VALUE_2, TEST_OFFSET_2);
        List<ConsumerRecord<String, String>> batchRecords = Arrays.asList(record1, record2);
        doNothing().when(listenerService).processSivaItemUpdateQueueEventBatch(batchRecords);

        batchDeduplicationListener.processNewItemDataUpdateEventBatch(batchRecords);
        verify(listenerService, times(1)).processSivaItemUpdateQueueEventBatch(eq(batchRecords));
        verify(listenerService, never()).processSivaItemUpdateQueueEvent(any(ConsumerRecord.class));
    }
} 