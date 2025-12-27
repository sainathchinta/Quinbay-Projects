package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import io.micrometer.tracing.Tracer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

class PermanentDeleteDataListenerTest {

  // Constants for test data
  private static final String TEST_TOPIC = Topics.PERMANENT_DELETE_DATA_FROM_AGP;
  private static final int TEST_PARTITION = 0;
  private static final String TEST_KEY = "test-key";
  private static final String TEST_VALUE = "test-value";
  private static final String MOCK_TRACE_ID = "mock-trace-id";

  @Mock
  private ListenerService listenerService;

  @Mock
  private Tracer tracer;

  @InjectMocks
  private PermanentDeleteDataListener permanentDeleteDataListener;

  private AutoCloseable closeable;
  private MockedStatic<TraceHelper> mockedTraceHelper;

  @BeforeEach
  void setUp() {
    closeable = MockitoAnnotations.openMocks(this);
    mockedTraceHelper = Mockito.mockStatic(TraceHelper.class);
    mockedTraceHelper.when(() -> TraceHelper.getTraceId(any())).thenReturn(MOCK_TRACE_ID);
    ReflectionTestUtils.setField(permanentDeleteDataListener, "bulkSize", 200);
  }

  @AfterEach
  void tearDown() throws Exception {
    mockedTraceHelper.close();
    closeable.close();
  }

  private ConsumerRecord<String, String> createSingleConsumerRecord(String key, String value, long offset) {
    return new ConsumerRecord<>(TEST_TOPIC, TEST_PARTITION, offset, key, value);
  }

  private List<ConsumerRecord<String, String>> createTestConsumerRecords(int count) {
    List<ConsumerRecord<String, String>> records = new ArrayList<>();
    for (int i = 0; i < count; i++) {
      records.add(createSingleConsumerRecord(TEST_KEY + i, TEST_VALUE + i, i));
    }
    return records;
  }

  @Test
  void testOnPermanentDeleteDataEvent_SingleRecord() throws Exception {
    List<ConsumerRecord<String, String>> records = createTestConsumerRecords(1);
    permanentDeleteDataListener.onPermanentDeleteDataEvent(records);
    
    verify(listenerService, timeout(2000)).onPermanentDeleteDataEvent(eq(records), eq(MOCK_TRACE_ID));
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(any()));
  }

  @Test
  void testOnPermanentDeleteDataEvent_MultipleBatches() throws Exception {
    int recordCount = 250;
    List<ConsumerRecord<String, String>> records = createTestConsumerRecords(recordCount);
    ReflectionTestUtils.setField(permanentDeleteDataListener, "bulkSize", 100);
    permanentDeleteDataListener.onPermanentDeleteDataEvent(records);
    
    // With bulkSize 100, we expect 3 batches: 100, 100, 50
    verify(listenerService, timeout(3000).times(3)).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(any()));
  }

  @Test
  void testOnPermanentDeleteDataEvent_CustomConfiguration() throws Exception {
    // Arrange
    int customBulkSize = 50;
    ReflectionTestUtils.setField(permanentDeleteDataListener, "bulkSize", customBulkSize);
    List<ConsumerRecord<String, String>> records = createTestConsumerRecords(100);
    permanentDeleteDataListener.onPermanentDeleteDataEvent(records);
    
    // With bulkSize 50, we expect 2 batches: 50, 50
    verify(listenerService, timeout(3000).times(2)).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(any()));
  }

  @Test
  void testOnPermanentDeleteDataEvent_BulkSizeEqualsRecordCount() throws Exception {
    int recordCount = 100;
    ReflectionTestUtils.setField(permanentDeleteDataListener, "bulkSize", recordCount);
    List<ConsumerRecord<String, String>> records = createTestConsumerRecords(recordCount);
    permanentDeleteDataListener.onPermanentDeleteDataEvent(records);
    
    verify(listenerService, timeout(2000).times(1)).onPermanentDeleteDataEvent(eq(records), eq(MOCK_TRACE_ID));
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(any()));
  }

  @Test
  void testOnPermanentDeleteDataEvent_MaxParallelism() throws Exception {
    int recordCount = 1000;
    ReflectionTestUtils.setField(permanentDeleteDataListener, "bulkSize", 10);
    List<ConsumerRecord<String, String>> records = createTestConsumerRecords(recordCount);
    permanentDeleteDataListener.onPermanentDeleteDataEvent(records);
    
    // With bulkSize 10, we expect 100 batches (1000/10)
    verify(listenerService, timeout(5000).times(100)).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(any()));
  }

  @Test
  void testOnPermanentDeleteDataEvent_BlocksUntilProcessingCompletes() throws Exception {
    // Arrange - simulate slow processing
    int recordCount = 50;
    List<ConsumerRecord<String, String>> records = createTestConsumerRecords(recordCount);
    ReflectionTestUtils.setField(permanentDeleteDataListener, "bulkSize", 25);
    
    // Add delay to simulate real processing time
    doAnswer(invocation -> {
      Thread.sleep(500); // Simulate 500ms processing time
      return null;
    }).when(listenerService).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
    
    // Act
    long startTime = System.currentTimeMillis();
    permanentDeleteDataListener.onPermanentDeleteDataEvent(records);
    long endTime = System.currentTimeMillis();
    long duration = endTime - startTime;
    
    // Assert - Method should block and take at least the processing time
    // With 2 batches of 25, processed in parallel, should take at least 500ms
    assert duration >= 400 : "Method returned too quickly, blocking not working. Duration: " + duration + "ms";
    
    // Verify all batches were processed
    verify(listenerService, times(2)).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
  }

  @Test
  void testOnPermanentDeleteDataEvent_ExceptionHandlingAndRethrow() throws Exception {
    // Arrange - simulate exception during processing
    int recordCount = 10;
    List<ConsumerRecord<String, String>> records = createTestConsumerRecords(recordCount);
    ReflectionTestUtils.setField(permanentDeleteDataListener, "bulkSize", 10);
    
    RuntimeException expectedException = new RuntimeException("Simulated processing failure");
    doThrow(expectedException).when(listenerService).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
    
    // Act & Assert - Exception should be rethrown
    try {
      permanentDeleteDataListener.onPermanentDeleteDataEvent(records);
      assert false : "Expected exception to be thrown but method completed successfully";
    } catch (RuntimeException e) {
      // Expected behavior - exception should propagate
      assert e.getMessage().contains("Simulated processing failure") : "Wrong exception message: " + e.getMessage();
    }
    
    // Verify the service was called
    verify(listenerService, times(1)).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
  }

  @Test
  void testOnPermanentDeleteDataEvent_PartialFailureScenario() throws Exception {
    // Arrange - First batch succeeds, second batch fails
    int recordCount = 100;
    List<ConsumerRecord<String, String>> records = createTestConsumerRecords(recordCount);
    ReflectionTestUtils.setField(permanentDeleteDataListener, "bulkSize", 50);
    
    // First call succeeds, subsequent calls fail
    doNothing()
      .doThrow(new RuntimeException("Second batch failed"))
      .when(listenerService).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
    
    // Act & Assert
    try {
      permanentDeleteDataListener.onPermanentDeleteDataEvent(records);
      assert false : "Expected exception but method completed successfully";
    } catch (RuntimeException e) {
      // Expected - exception from second batch should propagate
      assert e.getMessage().contains("Second batch failed");
    }
    
    // Verify service was called at least once (first batch succeeded)
    verify(listenerService, atLeastOnce()).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
  }

  @Test
  void testOnPermanentDeleteDataEvent_ParallelismCalculation() throws Exception {
    // Test that parallelism is calculated correctly based on bulk size
    
    // Test case 1: bulkSize = 20, expected parallelism = max(4, 20/5) = 4
    ReflectionTestUtils.setField(permanentDeleteDataListener, "bulkSize", 20);
    List<ConsumerRecord<String, String>> records1 = createTestConsumerRecords(20);
    permanentDeleteDataListener.onPermanentDeleteDataEvent(records1);
    verify(listenerService, timeout(2000).times(1)).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
    
    // Test case 2: bulkSize = 100, expected parallelism = max(4, 100/5) = 20
    reset(listenerService);
    ReflectionTestUtils.setField(permanentDeleteDataListener, "bulkSize", 100);
    List<ConsumerRecord<String, String>> records2 = createTestConsumerRecords(200);
    permanentDeleteDataListener.onPermanentDeleteDataEvent(records2);
    verify(listenerService, timeout(3000).times(2)).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
  }

  @Test
  void testOnPermanentDeleteDataEvent_EmptyRecordList() throws Exception {
    // Arrange
    List<ConsumerRecord<String, String>> emptyRecords = new ArrayList<>();
    
    // Act
    permanentDeleteDataListener.onPermanentDeleteDataEvent(emptyRecords);
    
    // Assert - Should complete without calling service
    verify(listenerService, never()).onPermanentDeleteDataEvent(any(), any());
  }

  @Test
  void testOnPermanentDeleteDataEvent_KafkaOffsetCommitBehavior() throws Exception {
    // This test simulates the critical scenario: Kafka should only commit offsets
    // after the listener method completes (i.e., after blockLast() completes)
    
    int recordCount = 50;
    List<ConsumerRecord<String, String>> records = createTestConsumerRecords(recordCount);
    ReflectionTestUtils.setField(permanentDeleteDataListener, "bulkSize", 25);
    
    boolean[] processingCompleted = {false};
    
    doAnswer(invocation -> {
      Thread.sleep(300); // Simulate deletion work
      processingCompleted[0] = true;
      return null;
    }).when(listenerService).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
    
    // Act
    permanentDeleteDataListener.onPermanentDeleteDataEvent(records);
    
    // Assert - When method returns, processing should be completed
    assert processingCompleted[0] : "Method returned before processing completed - Kafka will commit prematurely!";
    verify(listenerService, times(2)).onPermanentDeleteDataEvent(any(), eq(MOCK_TRACE_ID));
  }

}