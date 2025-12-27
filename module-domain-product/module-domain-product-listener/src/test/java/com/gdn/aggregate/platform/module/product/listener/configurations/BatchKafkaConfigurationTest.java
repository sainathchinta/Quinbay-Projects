package com.gdn.aggregate.platform.module.product.listener.configurations;

import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import com.gdn.aggregate.platform.module.product.listener.properties.BatchKafkaProperties;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.core.DefaultKafkaConsumerFactory;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class BatchKafkaConfigurationTest {

    @Mock
    private KafkaProperties kafkaProperties;

    @Mock
    private BatchKafkaProperties batchKafkaProperties;

    private BatchKafkaConfiguration batchKafkaConfiguration;
    private AutoCloseable closeable;

    private final ApplicationContextRunner contextRunner = new ApplicationContextRunner();

    @BeforeEach
    void setUp() {
        closeable = MockitoAnnotations.openMocks(this);
        batchKafkaConfiguration = new BatchKafkaConfiguration();
        ReflectionTestUtils.setField(batchKafkaConfiguration, "kafkaProperties", kafkaProperties);
        ReflectionTestUtils.setField(batchKafkaConfiguration, "batchKafkaProperties", batchKafkaProperties);
    }

    @AfterEach
    void tearDown() throws Exception {
        if (closeable != null) {
            closeable.close();
        }
    }

    @TestConfiguration
    static class TestConfig {
        @Bean
        public KafkaProperties kafkaProperties() {
            return new KafkaProperties();
        }

        @Bean
        public BatchKafkaProperties batchKafkaProperties() {
            BatchKafkaProperties properties = new BatchKafkaProperties();
            properties.setFetchMaxWaitMs(500);
            properties.setFetchMinBytes(1024);
            properties.setMaxPollRecords(100);
            return properties;
        }
    }

    @Test
    void testConditionalProperty_WhenDisabled_ShouldNotLoadConfiguration() {
        contextRunner
            .withPropertyValues(Enabler.FLOW_SIVA_ITEM_BATCH_DEDUPLICATION_ENABLED + "=false")
            .withUserConfiguration(BatchKafkaConfiguration.class, TestConfig.class)
            .run(context -> {
                assertFalse(context.containsBean("batchDeduplicationConsumerFactory"));
                assertFalse(context.containsBean("batchDeduplicationContainerFactory"));
            });
    }

    @Test
    void testConditionalProperty_WhenNotSet_ShouldNotLoadConfiguration() {
        contextRunner
            .withUserConfiguration(BatchKafkaConfiguration.class, TestConfig.class)
            .run(context -> {
                assertFalse(context.containsBean("batchDeduplicationConsumerFactory"));
                assertFalse(context.containsBean("batchDeduplicationContainerFactory"));
            });
    }

    @Test
    void testBatchDeduplicationConsumerFactory_ShouldReturnConfiguredFactory() {
        // Arrange
        Map<String, Object> baseProperties = new HashMap<>();
        baseProperties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092");
        baseProperties.put(ConsumerConfig.GROUP_ID_CONFIG, "test-group");
        
        when(kafkaProperties.buildConsumerProperties()).thenReturn(baseProperties);
        when(batchKafkaProperties.getFetchMaxWaitMs()).thenReturn(500);
        when(batchKafkaProperties.getFetchMinBytes()).thenReturn(1024);
        when(batchKafkaProperties.getMaxPollRecords()).thenReturn(100);

        // Act
        ConsumerFactory<Object, Object> factory = batchKafkaConfiguration.batchDeduplicationConsumerFactory();

        // Assert
        assertNotNull(factory);
        assertInstanceOf(DefaultKafkaConsumerFactory.class, factory);
        
        // Verify that all batch properties are properly configured
        verify(kafkaProperties).buildConsumerProperties();
        verify(batchKafkaProperties).getFetchMaxWaitMs();
        verify(batchKafkaProperties).getFetchMinBytes();
        verify(batchKafkaProperties).getMaxPollRecords();
    }

    @Test
    void testBatchDeduplicationConsumerFactory_ShouldConfigureCorrectProperties() {
        // Arrange
        Map<String, Object> baseProperties = new HashMap<>();
        baseProperties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092");
        
        when(kafkaProperties.buildConsumerProperties()).thenReturn(baseProperties);
        when(batchKafkaProperties.getFetchMaxWaitMs()).thenReturn(1000);
        when(batchKafkaProperties.getFetchMinBytes()).thenReturn(2048);
        when(batchKafkaProperties.getMaxPollRecords()).thenReturn(200);

        // Act
        ConsumerFactory<Object, Object> factory = batchKafkaConfiguration.batchDeduplicationConsumerFactory();
        DefaultKafkaConsumerFactory<Object, Object> defaultFactory = (DefaultKafkaConsumerFactory<Object, Object>) factory;
        Map<String, Object> configuredProperties = defaultFactory.getConfigurationProperties();

        // Assert
        assertEquals(1000, configuredProperties.get(ConsumerConfig.FETCH_MAX_WAIT_MS_CONFIG));
        assertEquals(2048, configuredProperties.get(ConsumerConfig.FETCH_MIN_BYTES_CONFIG));
        assertEquals(200, configuredProperties.get(ConsumerConfig.MAX_POLL_RECORDS_CONFIG));
        assertEquals("localhost:9092", configuredProperties.get(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG));
    }

    @Test
    void testBatchDeduplicationContainerFactory_WithVirtualThreadsEnabled_ShouldConfigureCorrectly() {
        // Arrange
        ReflectionTestUtils.setField(batchKafkaConfiguration, "virtualThreadsEnabled", true);
        
        Map<String, Object> baseProperties = new HashMap<>();
        when(kafkaProperties.buildConsumerProperties()).thenReturn(baseProperties);
        when(batchKafkaProperties.getFetchMaxWaitMs()).thenReturn(500);
        when(batchKafkaProperties.getFetchMinBytes()).thenReturn(1024);
        when(batchKafkaProperties.getMaxPollRecords()).thenReturn(100);

        // Act
        ConcurrentKafkaListenerContainerFactory<Object, Object> containerFactory = 
            batchKafkaConfiguration.batchDeduplicationContainerFactory();

        // Assert
        assertNotNull(containerFactory);
        assertTrue(containerFactory.isBatchListener());
        assertNull(containerFactory.getContainerProperties().getListenerTaskExecutor());
        assertEquals(batchKafkaConfiguration.batchDeduplicationConsumerFactory().getClass(), 
                     containerFactory.getConsumerFactory().getClass());
    }

    @Test
    void testBatchDeduplicationContainerFactory_WithVirtualThreadsDisabled_ShouldConfigureCorrectly() {
        // Arrange
        ReflectionTestUtils.setField(batchKafkaConfiguration, "virtualThreadsEnabled", false);
        
        Map<String, Object> baseProperties = new HashMap<>();
        when(kafkaProperties.buildConsumerProperties()).thenReturn(baseProperties);
        when(batchKafkaProperties.getFetchMaxWaitMs()).thenReturn(500);
        when(batchKafkaProperties.getFetchMinBytes()).thenReturn(1024);
        when(batchKafkaProperties.getMaxPollRecords()).thenReturn(100);

        // Act
        ConcurrentKafkaListenerContainerFactory<Object, Object> containerFactory = 
            batchKafkaConfiguration.batchDeduplicationContainerFactory();

        // Assert
        assertNotNull(containerFactory);
        assertTrue(containerFactory.isBatchListener());
        // When virtual threads are disabled, the task executor should remain as default (not null)
        // We can't easily assert on the exact task executor here as it's set by Spring framework
    }

    @Test
    void testBatchDeduplicationContainerFactory_ShouldAlwaysHaveBatchListenerEnabled() {
        // Arrange
        ReflectionTestUtils.setField(batchKafkaConfiguration, "virtualThreadsEnabled", false);
        
        Map<String, Object> baseProperties = new HashMap<>();
        when(kafkaProperties.buildConsumerProperties()).thenReturn(baseProperties);
        when(batchKafkaProperties.getFetchMaxWaitMs()).thenReturn(500);
        when(batchKafkaProperties.getFetchMinBytes()).thenReturn(1024);
        when(batchKafkaProperties.getMaxPollRecords()).thenReturn(100);

        // Act
        ConcurrentKafkaListenerContainerFactory<Object, Object> containerFactory = 
            batchKafkaConfiguration.batchDeduplicationContainerFactory();

        // Assert
        assertTrue(containerFactory.isBatchListener(), "Batch listener should always be enabled");
    }

    @Test
    void testBatchDeduplicationContainerFactory_ShouldConfigureCorrectly() {
        // Arrange
        ReflectionTestUtils.setField(batchKafkaConfiguration, "virtualThreadsEnabled", true);
        
        Map<String, Object> baseProperties = new HashMap<>();
        when(kafkaProperties.buildConsumerProperties()).thenReturn(baseProperties);
        when(batchKafkaProperties.getFetchMaxWaitMs()).thenReturn(500);
        when(batchKafkaProperties.getFetchMinBytes()).thenReturn(1024);
        when(batchKafkaProperties.getMaxPollRecords()).thenReturn(100);

        // Act
        ConcurrentKafkaListenerContainerFactory<Object, Object> containerFactory = 
            batchKafkaConfiguration.batchDeduplicationContainerFactory();

        // Assert
        assertNotNull(containerFactory);
        assertTrue(containerFactory.isBatchListener());
    }

    @Test
    void testVirtualThreadsFieldInjection() {
        // Test that the @Value annotation field can be set and retrieved
        assertDoesNotThrow(() -> {
            ReflectionTestUtils.setField(batchKafkaConfiguration, "virtualThreadsEnabled", true);
            Boolean virtualThreadsEnabled = (Boolean) ReflectionTestUtils.getField(batchKafkaConfiguration, "virtualThreadsEnabled");
            assertTrue(virtualThreadsEnabled);

            ReflectionTestUtils.setField(batchKafkaConfiguration, "virtualThreadsEnabled", false);
            virtualThreadsEnabled = (Boolean) ReflectionTestUtils.getField(batchKafkaConfiguration, "virtualThreadsEnabled");
            assertFalse(virtualThreadsEnabled);
        });
    }

    @Test
    void testKafkaPropertiesAutowiring() {
        // Test that kafkaProperties field can be autowired
        assertDoesNotThrow(() -> {
            KafkaProperties newProperties = mock(KafkaProperties.class);
            ReflectionTestUtils.setField(batchKafkaConfiguration, "kafkaProperties", newProperties);
            KafkaProperties injectedProperties = (KafkaProperties) ReflectionTestUtils.getField(batchKafkaConfiguration, "kafkaProperties");
            assertEquals(newProperties, injectedProperties);
        });
    }

    @Test
    void testBatchKafkaPropertiesAutowiring() {
        // Test that batchKafkaProperties field can be autowired
        assertDoesNotThrow(() -> {
            BatchKafkaProperties newProperties = mock(BatchKafkaProperties.class);
            ReflectionTestUtils.setField(batchKafkaConfiguration, "batchKafkaProperties", newProperties);
            BatchKafkaProperties injectedProperties = (BatchKafkaProperties) ReflectionTestUtils.getField(batchKafkaConfiguration, "batchKafkaProperties");
            assertEquals(newProperties, injectedProperties);
        });
    }

    @Test
    void testConsumerFactoryWithEmptyBaseProperties() {
        // Arrange
        Map<String, Object> emptyProperties = new HashMap<>();
        when(kafkaProperties.buildConsumerProperties()).thenReturn(emptyProperties);
        when(batchKafkaProperties.getFetchMaxWaitMs()).thenReturn(100);
        when(batchKafkaProperties.getFetchMinBytes()).thenReturn(512);
        when(batchKafkaProperties.getMaxPollRecords()).thenReturn(50);

        // Act
        ConsumerFactory<Object, Object> factory = batchKafkaConfiguration.batchDeduplicationConsumerFactory();
        DefaultKafkaConsumerFactory<Object, Object> defaultFactory = (DefaultKafkaConsumerFactory<Object, Object>) factory;
        Map<String, Object> configuredProperties = defaultFactory.getConfigurationProperties();

        // Assert
        assertNotNull(factory);
        assertEquals(100, configuredProperties.get(ConsumerConfig.FETCH_MAX_WAIT_MS_CONFIG));
        assertEquals(512, configuredProperties.get(ConsumerConfig.FETCH_MIN_BYTES_CONFIG));
        assertEquals(50, configuredProperties.get(ConsumerConfig.MAX_POLL_RECORDS_CONFIG));
    }

    @Test
    void testConsumerFactoryWithNullBatchProperties() {
        // Arrange
        Map<String, Object> baseProperties = new HashMap<>();
        baseProperties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092");
        
        when(kafkaProperties.buildConsumerProperties()).thenReturn(baseProperties);
        when(batchKafkaProperties.getFetchMaxWaitMs()).thenReturn(0);
        when(batchKafkaProperties.getFetchMinBytes()).thenReturn(0);
        when(batchKafkaProperties.getMaxPollRecords()).thenReturn(0);

        // Act
        ConsumerFactory<Object, Object> factory = batchKafkaConfiguration.batchDeduplicationConsumerFactory();
        DefaultKafkaConsumerFactory<Object, Object> defaultFactory = (DefaultKafkaConsumerFactory<Object, Object>) factory;
        Map<String, Object> configuredProperties = defaultFactory.getConfigurationProperties();

        // Assert
        assertNotNull(factory);
        assertEquals(0, configuredProperties.get(ConsumerConfig.FETCH_MAX_WAIT_MS_CONFIG));
        assertEquals(0, configuredProperties.get(ConsumerConfig.FETCH_MIN_BYTES_CONFIG));
        assertEquals(0, configuredProperties.get(ConsumerConfig.MAX_POLL_RECORDS_CONFIG));
    }

    @Test
    void testPermanentDeletionContainerFactory_ShouldConfigureCorrectProperties() {
        // Arrange
        Map<String, Object> baseProperties = new HashMap<>();
        baseProperties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092");
        
        when(kafkaProperties.buildConsumerProperties()).thenReturn(baseProperties);
        when(batchKafkaProperties.getPermanentDeleteMaxPollRecords()).thenReturn(1000);
        when(batchKafkaProperties.getPermanentDeleteFetchMaxWaitMs()).thenReturn(2000);
        when(batchKafkaProperties.getPermanentDeleteFetchMinBytes()).thenReturn(4096);
        when(batchKafkaProperties.getPermanentDeleteMaxPollIntervalMs()).thenReturn(7200000);
        when(batchKafkaProperties.getPermanentDeleteFetchMaxBytes()).thenReturn(52428800);
        ConcurrentKafkaListenerContainerFactory<Object, Object> factory =
            batchKafkaConfiguration.permanentDeletionContainerFactory();
        DefaultKafkaConsumerFactory<Object, Object> consumerFactory = 
            (DefaultKafkaConsumerFactory<Object, Object>) factory.getConsumerFactory();
        Map<String, Object> props = consumerFactory.getConfigurationProperties();
        assertNotNull(factory);
        assertEquals(1000, props.get(ConsumerConfig.MAX_POLL_RECORDS_CONFIG));
        assertEquals(2000, props.get(ConsumerConfig.FETCH_MAX_WAIT_MS_CONFIG));
        assertEquals(4096, props.get(ConsumerConfig.FETCH_MIN_BYTES_CONFIG));
        assertEquals(7200000, props.get(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG));
        assertEquals(300000, props.get(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG));
        assertEquals(52428800, props.get(ConsumerConfig.FETCH_MAX_BYTES_CONFIG));
        assertEquals(10485760, props.get(ConsumerConfig.MAX_PARTITION_FETCH_BYTES_CONFIG));
    }

    @Test
    void testPermanentDeletionContainerFactory_WithVirtualThreadsEnabled_ShouldConfigureCorrectly() {
        ReflectionTestUtils.setField(batchKafkaConfiguration, "virtualThreadsEnabled", true);
        Map<String, Object> baseProperties = new HashMap<>();
        when(kafkaProperties.buildConsumerProperties()).thenReturn(baseProperties);
        ConcurrentKafkaListenerContainerFactory<Object, Object> factory =
            batchKafkaConfiguration.permanentDeletionContainerFactory();
        assertNotNull(factory);
        assertTrue(factory.isBatchListener());
        assertNull(factory.getContainerProperties().getListenerTaskExecutor());
    }


    @Test
    void testPermanentDeletionContainerFactory_WithEmptyBaseProperties() {
        Map<String, Object> emptyProperties = new HashMap<>();
        when(kafkaProperties.buildConsumerProperties()).thenReturn(emptyProperties);
        when(batchKafkaProperties.getPermanentDeleteMaxPollRecords()).thenReturn(500);
        when(batchKafkaProperties.getPermanentDeleteFetchMaxWaitMs()).thenReturn(1000);
        when(batchKafkaProperties.getPermanentDeleteFetchMinBytes()).thenReturn(2048);
        when(batchKafkaProperties.getPermanentDeleteMaxPollIntervalMs()).thenReturn(3600000);
        when(batchKafkaProperties.getPermanentDeleteFetchMaxBytes()).thenReturn(26214400);
        ConcurrentKafkaListenerContainerFactory<Object, Object> factory =
            batchKafkaConfiguration.permanentDeletionContainerFactory();
        DefaultKafkaConsumerFactory<Object, Object> consumerFactory = 
            (DefaultKafkaConsumerFactory<Object, Object>) factory.getConsumerFactory();
        Map<String, Object> props = consumerFactory.getConfigurationProperties();
        assertNotNull(factory);
        assertEquals(500, props.get(ConsumerConfig.MAX_POLL_RECORDS_CONFIG));
        assertEquals(1000, props.get(ConsumerConfig.FETCH_MAX_WAIT_MS_CONFIG));
        assertEquals(2048, props.get(ConsumerConfig.FETCH_MIN_BYTES_CONFIG));
        assertEquals(3600000, props.get(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG));
        assertEquals(26214400, props.get(ConsumerConfig.FETCH_MAX_BYTES_CONFIG));
    }
}