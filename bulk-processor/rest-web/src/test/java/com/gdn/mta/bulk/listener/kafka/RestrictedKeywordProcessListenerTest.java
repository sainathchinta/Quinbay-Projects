package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.RestrictedKeywordProcessModel;
import com.gdn.mta.bulk.service.RestrictedKeywordService;

public class RestrictedKeywordProcessListenerTest {
  private static final String STORE_ID = "10001";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String INTERNAL_PROCESS_REQUEST_ID = "id";

  @InjectMocks
  private RestrictedKeywordProcessListener restrictedKeywordProcessListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private RestrictedKeywordService restrictedKeywordService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private ObjectMapper mapper = new ObjectMapper();
  private RestrictedKeywordProcessModel restrictedKeywordProcessModel;
  private String json;

  @BeforeEach
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);
    restrictedKeywordProcessModel =
        RestrictedKeywordProcessModel.builder().storeId(STORE_ID).internalBulkRequestId(INTERNAL_PROCESS_REQUEST_ID)
            .internalBulkRequestCode(INTERNAL_PROCESS_REQUEST_ID).categoryCode(CATEGORY_CODE)
            .type(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()).build();
    json = mapper.writeValueAsString(restrictedKeywordProcessModel);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, restrictedKeywordService, kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(json, RestrictedKeywordProcessModel.class))
        .thenReturn(restrictedKeywordProcessModel);
    Mockito.doNothing().when(restrictedKeywordService)
        .processRestrictedKeywordBulkOperation(STORE_ID, restrictedKeywordProcessModel);
    restrictedKeywordProcessListener.onDomainEventConsumed(json);
    Mockito.verify(objectMapper).readValue(json, RestrictedKeywordProcessModel.class);
    Mockito.verify(restrictedKeywordService)
        .processRestrictedKeywordBulkOperation(STORE_ID, restrictedKeywordProcessModel);
    Mockito.verify(kafkaTopicProperties).getRestrictedKeywordBulkProcess();
  }

  @Test
  public void onDomainEventConsumedErrorTest() throws Exception {
    Mockito.when(objectMapper.readValue(json, RestrictedKeywordProcessModel.class))
        .thenThrow(JsonProcessingException.class);
    restrictedKeywordProcessListener.onDomainEventConsumed(json);
    Mockito.verify(objectMapper).readValue(json, RestrictedKeywordProcessModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getRestrictedKeywordBulkProcess();
  }

}
