package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import io.micrometer.tracing.Tracer;
import org.apache.kafka.clients.consumer.ConsumerRecord;


import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.verify;

class DirectUpdateSivaBothByInventoryStockUpdateTest {

  // Constants for test data
  private static final String TEST_TOPIC = Topics.INVENTORY_STOCK_UPDATE_EVENT;

  private static final int TEST_PARTITION = 0;
  private static final long TEST_OFFSET = 0L;


  private static final String TEST_KEY = "stock-key";
  private static final String TEST_VALUE = "stock-value";
  private static final String MOCK_TRACE_ID = "mock-trace-id-stock-update";

  @Mock
  private ListenerService listenerService;

  @Mock
  private Tracer tracer;

  @InjectMocks
  private DirectUpdateSivaBothByInventoryStockUpdate stockUpdateListener;

  private AutoCloseable closeable;
  private MockedStatic<TraceHelper> mockedTraceHelper;

  @BeforeEach
  void setUp() {
    closeable = MockitoAnnotations.openMocks(this);
    mockedTraceHelper = Mockito.mockStatic(TraceHelper.class);
  }

  @AfterEach
  void tearDown() throws Exception {
    mockedTraceHelper.close();
    closeable.close();
  }

  private ConsumerRecord<String, String> createTestConsumerRecord() {
    return new ConsumerRecord<>(
      TEST_TOPIC,
      TEST_PARTITION,
      TEST_OFFSET,
      TEST_KEY,
      TEST_VALUE
    );
  }

  @Test
  void testOnInventoryStockUpdateEventSivaBoth() throws Exception {
    // Arrange
    ConsumerRecord<String, String> record = createTestConsumerRecord();
    mockedTraceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(MOCK_TRACE_ID);

    // Act
    stockUpdateListener.onInventoryInfoChangeEventSivaBoth(record);

    // Assert
    verify(listenerService).publishPickupPointUpsertEventOnInventoryStockUpdate(record, MOCK_TRACE_ID);
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(tracer));
  }

  @Test
  void testOnInventoryStockUpdateEventSivaBoth_WhenExceptionThrown() throws Exception {
    // Arrange
    ConsumerRecord<String, String> record = createTestConsumerRecord();
    mockedTraceHelper.when(() -> TraceHelper.getTraceId(tracer)).thenReturn(MOCK_TRACE_ID);
    Mockito.doThrow(new RuntimeException("Test exception"))
        .when(listenerService)
        .publishPickupPointUpsertEventOnInventoryStockUpdate(record, MOCK_TRACE_ID);

    // Act & Assert
    org.junit.jupiter.api.Assertions.assertThrows(
        Exception.class,
        () -> stockUpdateListener.onInventoryInfoChangeEventSivaBoth(record)
    );

    verify(listenerService).publishPickupPointUpsertEventOnInventoryStockUpdate(record, MOCK_TRACE_ID);
    mockedTraceHelper.verify(() -> TraceHelper.getTraceId(tracer));
  }
}
