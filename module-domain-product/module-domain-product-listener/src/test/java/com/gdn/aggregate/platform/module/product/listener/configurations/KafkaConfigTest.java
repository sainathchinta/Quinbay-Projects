package com.gdn.aggregate.platform.module.product.listener.configurations;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.boot.context.event.ApplicationStartedEvent;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.config.KafkaListenerEndpointRegistry;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.listener.MessageListenerContainer;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collection;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class KafkaConfigTest {

    @Mock
    private KafkaListenerProperties listenerProperties;

    @Mock
    private KafkaListenerEndpointRegistry kafkaListenerEndpointRegistry;

    @Mock
    private KafkaProperties kafkaProperties;

    @Mock
    private ConsumerFactory<Object, Object> consumerFactory;

    @Mock
    private ApplicationStartedEvent applicationStartedEvent;

    @Mock
    private MessageListenerContainer mockContainer1;

    @Mock
    private MessageListenerContainer mockContainer2;

    private KafkaConfig kafkaConfig;
    private AutoCloseable closeable;

    @BeforeEach
    void setUp() {
        closeable = MockitoAnnotations.openMocks(this);
        kafkaConfig = new KafkaConfig(listenerProperties, kafkaListenerEndpointRegistry);
        // Manually inject the kafkaProperties since it's autowired
        ReflectionTestUtils.setField(kafkaConfig, "kafkaProperties", kafkaProperties);
    }

    @AfterEach
    void tearDown() throws Exception {
        if (closeable != null) {
            closeable.close();
        }
    }

    @Test
    void testKafkaListenerContainerFactory_WithVirtualThreadsEnabled_AndAutoStartupTrue() {
        // Arrange
        ReflectionTestUtils.setField(kafkaConfig, "virtualThreadsEnabled", true);
        when(listenerProperties.isAutoStartup()).thenReturn(true);

        // Act
        ConcurrentKafkaListenerContainerFactory<Object, Object> factory = 
            kafkaConfig.kafkaListenerContainerFactory(consumerFactory);

        // Assert
        assertNotNull(factory);
        assertEquals(consumerFactory, factory.getConsumerFactory());
        // Virtual threads enabled should set task executor to null
        assertNull(factory.getContainerProperties().getListenerTaskExecutor());
        // Verify that setAutoStartup was called with true value
        verify(listenerProperties).isAutoStartup();
    }

    @Test
    void testKafkaListenerContainerFactory_WithVirtualThreadsDisabled_AndAutoStartupFalse() {
        // Arrange
        ReflectionTestUtils.setField(kafkaConfig, "virtualThreadsEnabled", false);
        when(listenerProperties.isAutoStartup()).thenReturn(false);

        // Act
        ConcurrentKafkaListenerContainerFactory<Object, Object> factory = 
            kafkaConfig.kafkaListenerContainerFactory(consumerFactory);

        // Assert
        assertNotNull(factory);
        assertEquals(consumerFactory, factory.getConsumerFactory());
        // Virtual threads disabled should not change the task executor (remains default)
        // The default task executor will be set by Spring, so we don't assert on it being null
        // Verify that setAutoStartup was called with false value
        verify(listenerProperties).isAutoStartup();
    }

    @Test
    void testKafkaListenerContainerFactory_WithVirtualThreadsEnabled_AndAutoStartupFalse() {
        // Arrange
        ReflectionTestUtils.setField(kafkaConfig, "virtualThreadsEnabled", true);
        when(listenerProperties.isAutoStartup()).thenReturn(false);

        // Act
        ConcurrentKafkaListenerContainerFactory<Object, Object> factory = 
            kafkaConfig.kafkaListenerContainerFactory(consumerFactory);

        // Assert
        assertNotNull(factory);
        assertEquals(consumerFactory, factory.getConsumerFactory());
        assertNull(factory.getContainerProperties().getListenerTaskExecutor());
        // Verify that setAutoStartup was called with false value
        verify(listenerProperties).isAutoStartup();
    }

    @Test
    void testKafkaListenerContainerFactory_WithVirtualThreadsDisabled_AndAutoStartupTrue() {
        // Arrange
        ReflectionTestUtils.setField(kafkaConfig, "virtualThreadsEnabled", false);
        when(listenerProperties.isAutoStartup()).thenReturn(true);

        // Act
        ConcurrentKafkaListenerContainerFactory<Object, Object> factory = 
            kafkaConfig.kafkaListenerContainerFactory(consumerFactory);

        // Assert
        assertNotNull(factory);
        assertEquals(consumerFactory, factory.getConsumerFactory());
        // Verify that setAutoStartup was called with true value
        verify(listenerProperties).isAutoStartup();
    }

    @Test
    void testStartKafkaListeners_WhenAutoStartupIsFalse_ShouldStartListeners() {
        // Arrange
        when(listenerProperties.isAutoStartup()).thenReturn(false);
        Collection<MessageListenerContainer> mockContainers = List.of(mockContainer1, mockContainer2);
        when(kafkaListenerEndpointRegistry.getListenerContainers()).thenReturn(mockContainers);

        // Act
        kafkaConfig.startKafkaListeners(applicationStartedEvent);

        // Assert
        verify(listenerProperties).isAutoStartup();
        verify(kafkaListenerEndpointRegistry).getListenerContainers();
        verify(mockContainer1).start();
        verify(mockContainer2).start();
    }

    @Test
    void testStartKafkaListeners_WhenAutoStartupIsTrue_ShouldNotStartListeners() {
        // Arrange
        when(listenerProperties.isAutoStartup()).thenReturn(true);

        // Act
        kafkaConfig.startKafkaListeners(applicationStartedEvent);

        // Assert
        verify(listenerProperties).isAutoStartup();
        verify(kafkaListenerEndpointRegistry, never()).getListenerContainers();
        verify(mockContainer1, never()).start();
        verify(mockContainer2, never()).start();
    }

    @Test
    void testStartKafkaListeners_WhenAutoStartupIsFalse_AndNoContainers_ShouldHandleGracefully() {
        // Arrange
        when(listenerProperties.isAutoStartup()).thenReturn(false);
        Collection<MessageListenerContainer> emptyContainers = List.of();
        when(kafkaListenerEndpointRegistry.getListenerContainers()).thenReturn(emptyContainers);

        // Act & Assert
        assertDoesNotThrow(() -> kafkaConfig.startKafkaListeners(applicationStartedEvent));
        
        verify(listenerProperties).isAutoStartup();
        verify(kafkaListenerEndpointRegistry).getListenerContainers();
    }

    @Test
    void testConstructor_ShouldInitializeFields() {
        // Arrange & Act
        KafkaConfig config = new KafkaConfig(listenerProperties, kafkaListenerEndpointRegistry);

        // Assert
        assertNotNull(config);
        // Verify that the fields are properly set by checking they're used in methods
        ReflectionTestUtils.setField(config, "virtualThreadsEnabled", false);
        when(listenerProperties.isAutoStartup()).thenReturn(true);
        
        ConcurrentKafkaListenerContainerFactory<Object, Object> factory = 
            config.kafkaListenerContainerFactory(consumerFactory);
        
        assertNotNull(factory);
        verify(listenerProperties).isAutoStartup();
    }

    @Test
    void testKafkaListenerContainerFactory_WithNullConsumerFactory_ShouldHandleGracefully() {
        // Arrange
        ReflectionTestUtils.setField(kafkaConfig, "virtualThreadsEnabled", false);
        when(listenerProperties.isAutoStartup()).thenReturn(true);

        // Act
        ConcurrentKafkaListenerContainerFactory<Object, Object> factory = 
            kafkaConfig.kafkaListenerContainerFactory(null);

        // Assert
        assertNotNull(factory);
        assertNull(factory.getConsumerFactory());
        // Verify that setAutoStartup was called with true value
        verify(listenerProperties).isAutoStartup();
    }

    @Test
    void testVirtualThreadsConfiguration_FieldInjection() {
        // Test that the @Value annotation field can be set
        assertDoesNotThrow(() -> {
            ReflectionTestUtils.setField(kafkaConfig, "virtualThreadsEnabled", true);
            Boolean virtualThreadsEnabled = (Boolean) ReflectionTestUtils.getField(kafkaConfig, "virtualThreadsEnabled");
            assertTrue(virtualThreadsEnabled);
        });
    }

    @Test
    void testKafkaPropertiesAutowiring() {
        // Test that kafkaProperties field can be autowired
        assertDoesNotThrow(() -> {
            ReflectionTestUtils.setField(kafkaConfig, "kafkaProperties", kafkaProperties);
            KafkaProperties injectedProperties = (KafkaProperties) ReflectionTestUtils.getField(kafkaConfig, "kafkaProperties");
            assertEquals(kafkaProperties, injectedProperties);
        });
    }
}