package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.FbbItem;
import com.gdn.mta.bulk.dto.product.FbbL5CreateDTO;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.FbbConsignmentEventModel;
import com.gdn.mta.bulk.entity.FbbL5ItemEventModel;
import com.gdn.mta.bulk.entity.FbbStatusEventModel;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.InternalProcessDataDomainEventModel;
import com.gdn.partners.bulk.util.Constant;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Arrays;
import java.util.Collections;

public class FbbConsignmentServiceTest {

  private FbbConsignmentEventModel eventModel;
  private BulkInternalProcess bulkInternalProcess;
  private FbbL5CreateDTO fbbL5CreateDTO;
  private BulkInternalProcessData bulkInternalProcessData;
  private BulkInternalProcessData bulkInternalProcessData1;
  private FbbCreatePickupPointResponse fbbCreatePickupPointResponse;
  private FbbCreatePickupPointRequest pickupPointRequest;
  private ObjectMapper mapper = new ObjectMapper();
  private Pageable pageable = PageRequest.of(PAGE, BATCH_SIZE);

  private static final String ITEM_SKU = "item-sku";
  private static final String BUSINESS_PARTNER_CODE = "business-partner-code";
  private static final String STORE_ID = "10001";
  private static final String CONSIGNMENT_ID = "consignment-id";
  private static final String INTERNAL_PROCESS_CODE = "internal-code";
  private static final String REQUEST_ID = "request-id";
  private static final String ERROR_CODE = "ERR-1001";
  private static final String ERROR_MESSAGE = "Already Existing";
  private static final int PAGE = 0;
  private static final int BATCH_SIZE = 10;
  public static final int TOTAL_COUNT = 1;

  @InjectMocks
  private FbbConsignmentServiceBean fbbConsignmentService;

  @Mock
  private InternalProcessService internalProcessService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private PBPOutboundService pbpOutboundService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    FbbItem fbbItem = FbbItem.builder().buyable(true).discoverable(true).itemSku(ITEM_SKU).build();
    eventModel = new FbbConsignmentEventModel();
    eventModel.setConsignmentId(CONSIGNMENT_ID);
    eventModel.setFbbItems(Collections.singletonList(fbbItem));
    eventModel.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    bulkInternalProcess = new BulkInternalProcess();
    fbbL5CreateDTO = new FbbL5CreateDTO();
    BulkInternalProcess internalProcesses = new BulkInternalProcess();
    bulkInternalProcessData = new BulkInternalProcessData();
    bulkInternalProcessData1 = new BulkInternalProcessData();

    internalProcesses.setInternalProcessRequestCode(INTERNAL_PROCESS_CODE);

    fbbCreatePickupPointResponse = new FbbCreatePickupPointResponse();
    pickupPointRequest = new FbbCreatePickupPointRequest();
    pickupPointRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointRequest.setBuyable(true);
    pickupPointRequest.setBuyable(true);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(internalProcessService);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(pbpOutboundService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void preProcessFbbConsignmentCreationTest() {
    Mockito.when(internalProcessService.checkPendingFbbL5Process(STORE_ID, BUSINESS_PARTNER_CODE, CONSIGNMENT_ID))
        .thenReturn(null);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any(BulkInternalProcess.class)))
        .thenReturn(bulkInternalProcess);
    fbbConsignmentService.preProcessFbbConsignmentCreation(eventModel);
    Mockito.verify(internalProcessService).checkPendingFbbL5Process(STORE_ID, BUSINESS_PARTNER_CODE, CONSIGNMENT_ID);
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any(BulkInternalProcess.class));
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getFbbCreateL5()), Mockito.any(FbbL5ItemEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getFbbCreateL5();
  }

  @Test
  public void preProcessFbbConsignmentCreationInProgressTest() {
    Mockito.when(internalProcessService.checkPendingFbbL5Process(STORE_ID, BUSINESS_PARTNER_CODE, CONSIGNMENT_ID))
        .thenReturn(bulkInternalProcess);
    try {
      fbbConsignmentService.preProcessFbbConsignmentCreation(eventModel);
    } catch (Exception ex) {
      Mockito.verify(internalProcessService).checkPendingFbbL5Process(STORE_ID, BUSINESS_PARTNER_CODE, CONSIGNMENT_ID);
    }
  }

  @Test
  public void processL5CreationEventTest() throws JsonProcessingException {
    Mockito.when(internalProcessService.findByInternalProcessRequestCode(Mockito.anyString(), (Mockito.anyString())))
        .thenReturn(bulkInternalProcess);
    fbbConsignmentService.processL5CreationEvent(INTERNAL_PROCESS_CODE, fbbL5CreateDTO);
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(Mockito.any(BulkInternalProcessData.class));
    Mockito.verify(internalProcessService).findByInternalProcessRequestCode(Mockito.anyString(), (Mockito.anyString()));
  }

  @Test
  public void publishL5CreateRowsTest() throws JsonProcessingException {
    bulkInternalProcess.setId(INTERNAL_PROCESS_CODE);
    pickupPointRequest.setItemSku("TEST-600021-1234-0001");
    bulkInternalProcessData.setData(mapper.writeValueAsString(pickupPointRequest));
    Mockito.when(
            internalProcessService.getBulkInternalProcessDataByStoreIdAndInternalProcessRequestIdAndStatus(STORE_ID,
                INTERNAL_PROCESS_CODE, BulkProcessData.STATUS_PENDING))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    fbbConsignmentService.publishL5CreateRows(STORE_ID, REQUEST_ID,
        new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE,
            BulkProcessData.STATUS_PENDING);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getFbbL5UpdateRows()),
          Mockito.eq("TEST-600021-1234"),
          Mockito.any(InternalProcessDataDomainEventModel.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(Mockito.any(BulkInternalProcessData.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getFbbL5UpdateRows();
  }

  @Test
  public void publishL5CreateRowsExceptionTest() throws JsonProcessingException {
    bulkInternalProcess.setId(INTERNAL_PROCESS_CODE);
    pickupPointRequest.setItemSku("INVALID-1000-123");
    bulkInternalProcessData.setData(mapper.writeValueAsString(pickupPointRequest));
    Mockito.when(
        internalProcessService.getBulkInternalProcessDataByStoreIdAndInternalProcessRequestIdAndStatus(
          STORE_ID, INTERNAL_PROCESS_CODE, BulkProcessData.STATUS_PENDING))
      .thenReturn(Collections.singletonList(bulkInternalProcessData));
    try {
      fbbConsignmentService.publishL5CreateRows(STORE_ID, REQUEST_ID,
        new PageImpl<>(Collections.singletonList(bulkInternalProcess), pageable, TOTAL_COUNT));
    } finally {
      Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestIdAndStatus(STORE_ID,
          INTERNAL_PROCESS_CODE, BulkProcessData.STATUS_PENDING);
      Mockito.verify(internalProcessService)
        .saveBulkInternalProcessData(Mockito.any(BulkInternalProcessData.class));
      Mockito.verify(kafkaTopicProperties).getFbbL5UpdateRows();
    }
  }

  @Test
  public void processFbbL4RowEventNoRowInProgressTest() {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE,
        ProcessStatus.IN_PROGRESS.name())).thenReturn(null);
    fbbConsignmentService.processFbbL4RowEvent(STORE_ID, INTERNAL_PROCESS_CODE);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE, ProcessStatus.IN_PROGRESS.name());
  }

  @Test
  public void processFbbL4RowEventExceptionTest() {
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE,
        ProcessStatus.IN_PROGRESS.name())).thenReturn(bulkInternalProcessData);
    Mockito.doThrow(ApplicationRuntimeException.class).when(internalProcessService).saveInternalProcessData(Mockito.anyList());
    try {
      fbbConsignmentService.processFbbL4RowEvent(STORE_ID, INTERNAL_PROCESS_CODE);
    } finally {
      Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
      Mockito.verify(internalProcessService)
          .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE, ProcessStatus.IN_PROGRESS.name());
      Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData);
    }
  }

  @Test
  public void processFbbL4RowTest() throws Exception {
    fbbCreatePickupPointResponse.setErrorCode(null);
    bulkInternalProcessData.setData(mapper.writeValueAsString(pickupPointRequest));
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE,
        ProcessStatus.IN_PROGRESS.name())).thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(pbpOutboundService.createDefaultL5Fbb(Mockito.any(FbbCreatePickupPointRequest.class)))
        .thenReturn(fbbCreatePickupPointResponse);
    fbbConsignmentService.processFbbL4RowEvent(STORE_ID, INTERNAL_PROCESS_CODE);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE, ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData);
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(pbpOutboundService).createDefaultL5Fbb(Mockito.any(FbbCreatePickupPointRequest.class));
  }

  @Test
  public void processFbbL4RowWithErrorTest() throws Exception {
    fbbCreatePickupPointResponse.setErrorCode(ERROR_CODE);
    fbbCreatePickupPointResponse.setReason(ERROR_MESSAGE);
    bulkInternalProcessData.setData(mapper.writeValueAsString(pickupPointRequest));
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE,
        ProcessStatus.IN_PROGRESS.name())).thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(pbpOutboundService.createDefaultL5Fbb(Mockito.any(FbbCreatePickupPointRequest.class)))
        .thenReturn(fbbCreatePickupPointResponse);
    fbbConsignmentService.processFbbL4RowEvent(STORE_ID, INTERNAL_PROCESS_CODE);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE, ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData);
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(pbpOutboundService).createDefaultL5Fbb(Mockito.any(FbbCreatePickupPointRequest.class));
  }

  @Test
  public void setFinalStatusForDefaultFbbL5CreationPartiallyCompletedTest() throws Exception {
    bulkInternalProcess.setInternalProcessRequestCode(REQUEST_ID);
    bulkInternalProcess.setNotes(CONSIGNMENT_ID);
    fbbL5CreateDTO.setItemSku(ITEM_SKU);
    bulkInternalProcessData1.setData(mapper.writeValueAsString(fbbL5CreateDTO));
    bulkInternalProcessData1.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessData1.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData1.setNotes(ERROR_CODE);
    bulkInternalProcess.setId(REQUEST_ID);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, REQUEST_ID))
        .thenReturn(Arrays.asList(bulkInternalProcessData, bulkInternalProcessData1));
    fbbConsignmentService.setFinalStatusForDefaultFbbL5Creation(bulkInternalProcess, STORE_ID);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, REQUEST_ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcess);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getFbbCreateConsignmentResult()), Mockito.any(FbbStatusEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getFbbCreateConsignmentResult();
  }

  @Test
  public void setFinalStatusForDefaultFbbL5CreationPendingTest() throws Exception {
    bulkInternalProcess.setInternalProcessRequestCode(REQUEST_ID);
    bulkInternalProcess.setNotes(CONSIGNMENT_ID);
    fbbL5CreateDTO.setItemSku(ITEM_SKU);
    bulkInternalProcessData1.setData(mapper.writeValueAsString(fbbL5CreateDTO));
    bulkInternalProcessData1.setStatus(ProcessStatus.PENDING.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessData1.setErrorMessage(null);
    bulkInternalProcessData1.setNotes(null);
    bulkInternalProcess.setId(REQUEST_ID);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, REQUEST_ID))
      .thenReturn(Arrays.asList(bulkInternalProcessData, bulkInternalProcessData1));
    fbbConsignmentService.setFinalStatusForDefaultFbbL5Creation(bulkInternalProcess, STORE_ID);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, REQUEST_ID);
  }

  @Test
  public void setFinalStatusForDefaultFbbL5CreationInProgressTest() throws Exception {
    bulkInternalProcess.setInternalProcessRequestCode(REQUEST_ID);
    bulkInternalProcess.setNotes(CONSIGNMENT_ID);
    fbbL5CreateDTO.setItemSku(ITEM_SKU);
    bulkInternalProcessData1.setData(mapper.writeValueAsString(fbbL5CreateDTO));
    bulkInternalProcessData1.setStatus(ProcessStatus.IN_PROGRESS.name());
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    bulkInternalProcessData1.setErrorMessage(null);
    bulkInternalProcessData1.setNotes(null);
    bulkInternalProcess.setId(REQUEST_ID);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, REQUEST_ID))
      .thenReturn(Arrays.asList(bulkInternalProcessData, bulkInternalProcessData1));
    fbbConsignmentService.setFinalStatusForDefaultFbbL5Creation(bulkInternalProcess, STORE_ID);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, REQUEST_ID);
  }

  @Test
  public void setFinalStatusForDefaultFbbL5CreationEmptyTest() throws Exception {
    bulkInternalProcess.setInternalProcessRequestCode(REQUEST_ID);
    bulkInternalProcess.setNotes(CONSIGNMENT_ID);
    fbbL5CreateDTO.setItemSku(ITEM_SKU);
    bulkInternalProcessData1.setData(mapper.writeValueAsString(fbbL5CreateDTO));
    bulkInternalProcessData1.setStatus(ProcessStatus.IN_PROGRESS.name());
    bulkInternalProcessData.setStatus(ProcessStatus.IN_PROGRESS.name());
    bulkInternalProcessData1.setErrorMessage(null);
    bulkInternalProcessData1.setNotes(null);
    bulkInternalProcess.setId(REQUEST_ID);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, REQUEST_ID))
      .thenReturn(Collections.emptyList());
    fbbConsignmentService.setFinalStatusForDefaultFbbL5Creation(bulkInternalProcess, STORE_ID);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, REQUEST_ID);
  }


  @Test
  public void setFinalStatusForDefaultFbbL5CreationAllSuccessTest() throws Exception {
    bulkInternalProcess.setInternalProcessRequestCode(REQUEST_ID);
    bulkInternalProcess.setNotes(CONSIGNMENT_ID);
    fbbL5CreateDTO.setItemSku(ITEM_SKU);
    bulkInternalProcessData1.setData(mapper.writeValueAsString(fbbL5CreateDTO));
    bulkInternalProcessData1.setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessData.setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessData1.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData1.setNotes(ERROR_CODE);
    bulkInternalProcess.setId(REQUEST_ID);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, REQUEST_ID))
        .thenReturn(Arrays.asList(bulkInternalProcessData, bulkInternalProcessData1));
    fbbConsignmentService.setFinalStatusForDefaultFbbL5Creation(bulkInternalProcess, STORE_ID);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, REQUEST_ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcess);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getFbbCreateConsignmentResult()), Mockito.any(FbbStatusEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getFbbCreateConsignmentResult();
  }

  @Test
  public void setFinalStatusForDefaultFbbL5CreationNoSuccessTest() throws Exception {
    bulkInternalProcess.setInternalProcessRequestCode(REQUEST_ID);
    bulkInternalProcess.setNotes(CONSIGNMENT_ID);
    fbbL5CreateDTO.setItemSku(ITEM_SKU);
    bulkInternalProcessData1.setData(mapper.writeValueAsString(fbbL5CreateDTO));
    bulkInternalProcessData.setData(mapper.writeValueAsString(fbbL5CreateDTO));
    bulkInternalProcessData1.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData1.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData1.setNotes(ERROR_CODE);
    bulkInternalProcess.setId(REQUEST_ID);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, REQUEST_ID))
        .thenReturn(Arrays.asList(bulkInternalProcessData, bulkInternalProcessData1));
    fbbConsignmentService.setFinalStatusForDefaultFbbL5Creation(bulkInternalProcess, STORE_ID);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, REQUEST_ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcess);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getFbbCreateConsignmentResult()), Mockito.any(FbbStatusEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getFbbCreateConsignmentResult();
  }

  @Test
  public void processFbbL4RowEventMinimumPriceValidationSuccessTest() throws Exception {
    fbbCreatePickupPointResponse.setErrorCode(null);
    bulkInternalProcessData.setData(mapper.writeValueAsString(pickupPointRequest));
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE,
        ProcessStatus.IN_PROGRESS.name())).thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(pbpOutboundService.createDefaultL5Fbb(Mockito.any(FbbCreatePickupPointRequest.class)))
        .thenReturn(fbbCreatePickupPointResponse);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, Constant.MINIMUM_PRICE))
        .thenReturn(new SystemParameterConfig(Constant.MINIMUM_PRICE, "1", Constant.MINIMUM_PRICE));
    fbbConsignmentService.processFbbL4RowEvent(STORE_ID, INTERNAL_PROCESS_CODE);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE, ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData);
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(pbpOutboundService).createDefaultL5Fbb(Mockito.any(FbbCreatePickupPointRequest.class));
  }

  @Test
  public void processFbbL4RowEventMinimumPriceValidationFailedTest() throws Exception {
    fbbCreatePickupPointResponse.setErrorCode(null);
    bulkInternalProcessData.setData(mapper.writeValueAsString(pickupPointRequest));
    Mockito.when(internalProcessService.bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE,
        ProcessStatus.IN_PROGRESS.name())).thenReturn(bulkInternalProcessData);
    Mockito.when(internalProcessService.saveInternalProcessData(Mockito.anyList()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData));
    Mockito.when(pbpOutboundService.createDefaultL5Fbb(Mockito.any(FbbCreatePickupPointRequest.class)))
        .thenReturn(fbbCreatePickupPointResponse);
    fbbConsignmentService.processFbbL4RowEvent(STORE_ID, INTERNAL_PROCESS_CODE);
    Mockito.verify(internalProcessService)
        .bulkInternalProcessDataByIdAndStatus(STORE_ID, INTERNAL_PROCESS_CODE, ProcessStatus.IN_PROGRESS.name());
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessData);
    Mockito.verify(internalProcessService).saveInternalProcessData(Mockito.anyList());
    Mockito.verify(pbpOutboundService).createDefaultL5Fbb(Mockito.any(FbbCreatePickupPointRequest.class));
  }
}

