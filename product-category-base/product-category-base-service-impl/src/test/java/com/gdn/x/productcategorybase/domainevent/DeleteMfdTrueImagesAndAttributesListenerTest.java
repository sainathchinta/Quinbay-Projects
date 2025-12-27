package com.gdn.x.productcategorybase.domainevent;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.ProductMigrationStatus;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.SchedulerService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.util.CommonUtil;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

class DeleteMfdTrueImagesAndAttributesListenerTest {

    private static final String EXPECTED_TOPIC = "delete-mfd-true-images-and-attributes-topic";

    @Mock
    private ProductServiceWrapper productServiceWrapper;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private KafkaTopicProperties kafkaTopicProperties;

    @Mock
    private SchedulerService schedulerService;

    @InjectMocks
    private DeleteMfdTrueImagesAndAttributesListener listener;

    private CommonImageBackfillingEventModel validEventModel;
    private String validJsonMessage;

    @BeforeEach
    void setUp() throws JsonProcessingException {
        MockitoAnnotations.openMocks(this);
        validEventModel = new CommonImageBackfillingEventModel();
        validEventModel.setStoreId("store123");
        validEventModel.setMigrationType("DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES");
        validEventModel.setProductCode("product123");

        validJsonMessage = "{\"storeId\":\"store123\",\"migrationType\":\"DELETE_MFD_TRUE_IMAGES_AND_ATTRIBUTES\",\"productCode\":\"product123\"}";
        when(objectMapper.readValue(validJsonMessage, CommonImageBackfillingEventModel.class))
            .thenReturn(validEventModel);

        when(kafkaTopicProperties.getDeleteMfdTrueImageAndAttributeEvent())
            .thenReturn(EXPECTED_TOPIC);
        when(kafkaTopicProperties.isAutoStartup()).thenReturn(true);
    }

    @AfterEach
    void tearDown() throws Exception {
        verifyNoMoreInteractions(productServiceWrapper, objectMapper, kafkaTopicProperties, schedulerService);
    }

    @Test
    void onDomainEventConsumed_Success() throws JsonProcessingException {
        doNothing().when(productServiceWrapper).deleteMfdTrueImagesAndAttributes(validEventModel);
        listener.onDomainEventConsumed(validJsonMessage);
        verify(kafkaTopicProperties, times(1)).getDeleteMfdTrueImageAndAttributeEvent();
        verify(objectMapper, times(1)).readValue(eq(validJsonMessage), eq(CommonImageBackfillingEventModel.class));
        verify(productServiceWrapper).deleteMfdTrueImagesAndAttributes(validEventModel);
    }

    @Test
    void onDomainEventConsumed_ProcessingError() throws JsonProcessingException {
        String errorMessage = "Processing failed";
        doThrow(new RuntimeException(errorMessage))
            .when(productServiceWrapper).deleteMfdTrueImagesAndAttributes(validEventModel);
        listener.onDomainEventConsumed(validJsonMessage);
        verify(kafkaTopicProperties, times(1)).getDeleteMfdTrueImageAndAttributeEvent();
        verify(objectMapper, times(1)).readValue(eq(validJsonMessage), eq(CommonImageBackfillingEventModel.class));
        verify(productServiceWrapper).deleteMfdTrueImagesAndAttributes(validEventModel);
        verify(schedulerService).updateProductMigrationStatus(
            eq(validEventModel.getStoreId()),
            eq(null),
            eq(CommonUtil.getProductMigrationRequest(validEventModel, errorMessage, ProductMigrationStatus.FAILED))
        );
    }

    @Test
    void onDomainEventConsumed_JsonParsingError() throws JsonProcessingException {
        String invalidJson = "invalid json";
        when(objectMapper.readValue(invalidJson, CommonImageBackfillingEventModel.class))
            .thenThrow(new JsonProcessingException("Invalid JSON") {});
        assertThrows(JsonProcessingException.class, () -> 
            listener.onDomainEventConsumed(invalidJson)
        );
        verify(kafkaTopicProperties, times(1)).getDeleteMfdTrueImageAndAttributeEvent();
        verify(objectMapper, times(1)).readValue(eq(invalidJson), eq(CommonImageBackfillingEventModel.class));
        verifyNoInteractions(productServiceWrapper);
        verifyNoInteractions(schedulerService);
    }

    @Test
    void verifyKafkaTopicPropertiesConfiguration() {
        String topic = kafkaTopicProperties.getDeleteMfdTrueImageAndAttributeEvent();
        boolean autoStartup = kafkaTopicProperties.isAutoStartup();
        verify(kafkaTopicProperties, times(1)).getDeleteMfdTrueImageAndAttributeEvent();
        verify(kafkaTopicProperties, times(1)).isAutoStartup();
        assert topic.equals(EXPECTED_TOPIC);
        assert autoStartup;
    }
} 