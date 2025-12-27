package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkReviewUploadModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.partners.bulk.util.Constant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

public class BulkReviewUploadListenerTest {
    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private InternalProcessServiceWrapper internalProcessServiceWrapper;

    @Mock
    private KafkaTopicProperties kafkaTopicProperties;

    @InjectMocks
    private BulkReviewUploadListener bulkReviewUploadListener;
    private BulkReviewUploadModel bulkReviewUploadModel;
    private String message;

    @BeforeEach
    public void init() throws JsonProcessingException {
        initMocks(this);
        bulkReviewUploadModel = bulkReviewUploadModel.builder().bulkProcessType(Constant.INSTORE_BULK_PROCESS_TYPE)
                .bulkProcessCode(Constant.CATEGORY_CODE).filePath(Constant.DEFAULT_FILE_NAME).storeId(Constant.STORE_ID)
                .build();
        message = new ObjectMapper().writeValueAsString(bulkReviewUploadModel);
    }

    @AfterEach
    public void tearDown() {
        verifyNoMoreInteractions(objectMapper);
        verifyNoMoreInteractions(internalProcessServiceWrapper);
        verifyNoMoreInteractions(kafkaTopicProperties);
    }

    @Test
    public void onDomainEventConsumedException_Test() throws Exception {
        Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkReviewUploadModel.class))
                .thenThrow(JsonProcessingException.class);
        bulkReviewUploadListener.onDomainEventConsumed(Constant.CLIENT_ID);
        verify(objectMapper).readValue(Constant.CLIENT_ID, BulkReviewUploadModel.class);
        Mockito.verify(kafkaTopicProperties).getBulkReviewUploadEvent();
    }

    @Test
    public void onDomainEventConsumedNewFlowTest() throws Exception {
        Mockito.when(objectMapper.readValue(message, BulkReviewUploadModel.class)).thenReturn(bulkReviewUploadModel);
        bulkReviewUploadListener.onDomainEventConsumed(message);
        verify(internalProcessServiceWrapper).uploadBulkReviewProcess(bulkReviewUploadModel.getStoreId(),
                bulkReviewUploadModel);
        verify(objectMapper).readValue(message, BulkReviewUploadModel.class);
        Mockito.verify(kafkaTopicProperties).getBulkReviewUploadEvent();
    }
}