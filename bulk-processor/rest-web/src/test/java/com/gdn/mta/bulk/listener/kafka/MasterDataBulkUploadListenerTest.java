package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.mta.bulk.service.MasterDataBulkUpdateService;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.partners.bulk.util.Constant;

import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.FAILED;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.MASTER_PRODUCT_BULK_UPDATE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_EVENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

public class MasterDataBulkUploadListenerTest {

  @Mock
  private MasterDataBulkUpdateService masterDataBulkUpdateService;

  @Mock
  private TrackerService trackerService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private MasterDataBulkUploadListener listener;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private static final String TRUE_STRING = "true";

  private MasterDataBulkUpdateRequest masterDataBulkUpdateRequest;
  private SystemParameterConfig systemParameterConfig;

  @BeforeEach
  public void init(){
    initMocks(this);
    masterDataBulkUpdateRequest = new MasterDataBulkUpdateRequest();
    systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(TRUE_STRING);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(masterDataBulkUpdateService);
    verifyNoMoreInteractions(trackerService);
    verifyNoMoreInteractions(systemParameterConfigService);
    verifyNoMoreInteractions(internalProcessServiceWrapper);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, MasterDataBulkUpdateRequest.class))
        .thenReturn(masterDataBulkUpdateRequest);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(masterDataBulkUpdateService).processBulkUpdate(masterDataBulkUpdateRequest);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.INTERNAL_UPLOAD_SWITCH);
    verify(objectMapper).readValue(Constant.CLIENT_ID, MasterDataBulkUpdateRequest.class);
    Mockito.verify(kafkaTopicProperties).getInternalUserBulkUploadEvent();
  }

  @Test
  public void onDomainEventConsumedFlagOnTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, MasterDataBulkUpdateRequest.class))
        .thenReturn(masterDataBulkUpdateRequest);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
        Constant.INTERNAL_UPLOAD_SWITCH)).thenReturn(systemParameterConfig);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.INTERNAL_UPLOAD_SWITCH);
    verify(internalProcessServiceWrapper).uploadInternalBulkUploadToBulkInternalProcess(
        masterDataBulkUpdateRequest.getStoreId(), masterDataBulkUpdateRequest);
    verify(objectMapper).readValue(Constant.CLIENT_ID, MasterDataBulkUpdateRequest.class);
    Mockito.verify(kafkaTopicProperties).getInternalUserBulkUploadEvent();
  }

  @Test
  public void onDomainEventConsumed_whenException_Test() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, MasterDataBulkUpdateRequest.class))
        .thenReturn(masterDataBulkUpdateRequest);
    doThrow(ApplicationRuntimeException.class).when(masterDataBulkUpdateService)
        .processBulkUpdate(any((MasterDataBulkUpdateRequest.class)));
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(masterDataBulkUpdateService).processBulkUpdate(masterDataBulkUpdateRequest);
    verify(systemParameterConfigService).findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.INTERNAL_UPLOAD_SWITCH);
    verify(trackerService).sendTracker(PRODUCT_UPDATE_EVENT, MASTER_PRODUCT_BULK_UPDATE,
        HYPHEN, FAILED, masterDataBulkUpdateRequest.getUpdatedBy());
    verify(objectMapper).readValue(Constant.CLIENT_ID, MasterDataBulkUpdateRequest.class);
    Mockito.verify(kafkaTopicProperties).getInternalUserBulkUploadEvent();
  }

}