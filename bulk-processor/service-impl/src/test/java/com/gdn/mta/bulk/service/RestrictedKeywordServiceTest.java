package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.RestrictedKeywordProcessModel;
import com.gdn.mta.bulk.models.RestrictedKeywordRequestData;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.CategoryErrorResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.google.common.collect.ImmutableSet;

public class RestrictedKeywordServiceTest {

  private static final String STORE_ID = "10001";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String KEYWORD = "keyword";
  private static final String KEYWORD_TYPE = "keywordType";
  private static final String KEYWORD_ACTION = "No action";
  private static final String MESSAGE = "message";
  private static final String DESTINATION_CATEGORY = "AK-10001";
  private static final String EXCLUDE_CATEGORY = "AK-10002";
  private static final int EXCEL_ROW_NUMBER = 1;
  private static final String INTERNAL_PROCESS_REQUEST_ID = "id";
  private static final String CATEGORY_KEYWORDS_UPDATE_ERROR_MESSAGE = "Error while updating category keywords";
  private static final String FILE_PATH = "filepath";

  @InjectMocks
  private RestrictedKeywordServiceImpl restrictedKeywordService;

  @Mock
  private InternalProcessService internalProcessService;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @Mock
  private ObjectMapper mapper;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private MailDeliveryService mailDeliveryService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Captor
  private ArgumentCaptor<BulkInternalProcess> bulkInternalProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<Map<String, Object>> mapArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkDownloadRequest> bulkDownloadRequestArgumentCaptor;

  private ObjectMapper objectMapper = new ObjectMapper();
  private BulkInternalProcessData addRestrictedKeyword;
  private BulkInternalProcessData deleteRestrictedKeyword;
  RestrictedKeywordRequestData restrictedKeywordRequestData;

  @BeforeEach
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);
    restrictedKeywordRequestData =
        RestrictedKeywordRequestData.builder().keyword(KEYWORD).keywordType(KEYWORD_TYPE).keywordAction(KEYWORD_ACTION)
            .categoryCode(CATEGORY_CODE).destinationCategory(DESTINATION_CATEGORY).message(MESSAGE)
            .exclusionList(ImmutableSet.of(EXCLUDE_CATEGORY)).excelRowNumber(EXCEL_ROW_NUMBER).build();
    String json = objectMapper.writeValueAsString(restrictedKeywordRequestData);

    addRestrictedKeyword = new BulkInternalProcessData();
    addRestrictedKeyword.setStatus(ProcessStatus.PICKED.name());
    addRestrictedKeyword.setData(json);
    addRestrictedKeyword.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);

    deleteRestrictedKeyword = new BulkInternalProcessData();
    deleteRestrictedKeyword.setStatus(ProcessStatus.PICKED.name());
    deleteRestrictedKeyword.setData(json);
    deleteRestrictedKeyword.setInternalProcessRequestId(INTERNAL_PROCESS_REQUEST_ID);

    ReflectionTestUtils.setField(restrictedKeywordService, "gcsRestrictedKeywordErrorPath", "errorPath");
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(internalProcessService, pcbOutboundService, kafkaTopicProperties);
  }

  @Test
  public void processRestrictedKeywordBulkOperationAdditionTest() throws JsonProcessingException {
    RestrictedKeywordProcessModel restrictedKeywordProcessModel =
        RestrictedKeywordProcessModel.builder().storeId(STORE_ID).internalBulkRequestId(INTERNAL_PROCESS_REQUEST_ID)
            .internalBulkRequestCode(INTERNAL_PROCESS_REQUEST_ID).categoryCode(CATEGORY_CODE)
            .type(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()).build();

    Mockito.when(
        internalProcessService.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
            CATEGORY_CODE, BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name())).thenReturn(Arrays.asList(addRestrictedKeyword));
    Mockito.when(internalProcessService.saveInternalProcessData(Arrays.asList(addRestrictedKeyword)))
        .thenReturn(Arrays.asList(addRestrictedKeyword));
    Mockito.when(
        pcbOutboundService.updateCategoriesWithRestrictedKeywords(Mockito.eq(STORE_ID), Mockito.eq(CATEGORY_CODE),
            Mockito.any(CategoryKeywordUpdateRequestList.class))).thenReturn(StringUtils.EMPTY);
    Mockito.when(mapper.readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class)))
        .thenReturn(new RestrictedKeywordRequestData());

    restrictedKeywordService.processRestrictedKeywordBulkOperation(STORE_ID, restrictedKeywordProcessModel);

    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(CATEGORY_CODE,
            BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name());
    Mockito.verify(internalProcessService, Mockito.times(2)).saveInternalProcessData(Arrays.asList(addRestrictedKeyword));
    Mockito.verify(pcbOutboundService)
        .updateCategoriesWithRestrictedKeywords(Mockito.eq(STORE_ID), Mockito.eq(CATEGORY_CODE),
            Mockito.any(CategoryKeywordUpdateRequestList.class));
    Mockito.verify(mapper).readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class));
  }

  @Test
  public void processRestrictedKeywordBulkOperationAdditionErrorTest() throws JsonProcessingException {
    RestrictedKeywordProcessModel restrictedKeywordProcessModel =
        RestrictedKeywordProcessModel.builder().storeId(STORE_ID).internalBulkRequestId(INTERNAL_PROCESS_REQUEST_ID)
            .internalBulkRequestCode(INTERNAL_PROCESS_REQUEST_ID).categoryCode(CATEGORY_CODE)
            .type(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()).build();

    Mockito.when(mapper.readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class)))
        .thenReturn(new RestrictedKeywordRequestData());
    Mockito.when(
        internalProcessService.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
            CATEGORY_CODE, BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name())).thenReturn(Arrays.asList(addRestrictedKeyword));
    Mockito.when(internalProcessService.saveInternalProcessData(Arrays.asList(addRestrictedKeyword)))
        .thenReturn(Arrays.asList(addRestrictedKeyword));
    Mockito.when(
        pcbOutboundService.updateCategoriesWithRestrictedKeywords(Mockito.eq(STORE_ID), Mockito.eq(CATEGORY_CODE),
            Mockito.any(CategoryKeywordUpdateRequestList.class))).thenReturn(CATEGORY_KEYWORDS_UPDATE_ERROR_MESSAGE);

    restrictedKeywordService.processRestrictedKeywordBulkOperation(STORE_ID, restrictedKeywordProcessModel);

    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(CATEGORY_CODE,
            BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name());
    Mockito.verify(internalProcessService, Mockito.times(2)).saveInternalProcessData(Arrays.asList(addRestrictedKeyword));
    Mockito.verify(pcbOutboundService)
        .updateCategoriesWithRestrictedKeywords(Mockito.eq(STORE_ID), Mockito.eq(CATEGORY_CODE),
            Mockito.any(CategoryKeywordUpdateRequestList.class));
    Mockito.verify(mapper).readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class));
  }

  @Test
  public void processRestrictedKeywordBulkOperationAdditionNoDataTest() throws JsonProcessingException {
    RestrictedKeywordProcessModel restrictedKeywordProcessModel =
        RestrictedKeywordProcessModel.builder().storeId(STORE_ID).internalBulkRequestId(INTERNAL_PROCESS_REQUEST_ID)
            .internalBulkRequestCode(INTERNAL_PROCESS_REQUEST_ID).categoryCode(CATEGORY_CODE)
            .type(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()).build();

    Mockito.when(
        internalProcessService.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
            CATEGORY_CODE, BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name())).thenReturn(new ArrayList<>());

    restrictedKeywordService.processRestrictedKeywordBulkOperation(STORE_ID, restrictedKeywordProcessModel);

    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(CATEGORY_CODE,
            BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name());
  }

  @Test
  public void processRestrictedKeywordBulkOperationAdditionDeletionTest() throws JsonProcessingException {
    RestrictedKeywordProcessModel restrictedKeywordProcessModel =
        RestrictedKeywordProcessModel.builder().storeId(STORE_ID).internalBulkRequestId(INTERNAL_PROCESS_REQUEST_ID)
            .internalBulkRequestCode(INTERNAL_PROCESS_REQUEST_ID).categoryCode(CATEGORY_CODE)
            .type(BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name()).build();

    Mockito.when(
        internalProcessService.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
            CATEGORY_CODE, BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name())).thenReturn(Arrays.asList(addRestrictedKeyword));
    Mockito.when(internalProcessService.saveInternalProcessData(Arrays.asList(addRestrictedKeyword)))
        .thenReturn(Arrays.asList(addRestrictedKeyword));
    Mockito.when(
        pcbOutboundService.updateCategoriesWithRestrictedKeywords(Mockito.eq(STORE_ID), Mockito.eq(CATEGORY_CODE),
            Mockito.any(CategoryKeywordUpdateRequestList.class))).thenReturn(StringUtils.EMPTY);
    Mockito.when(mapper.readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class)))
        .thenReturn(new RestrictedKeywordRequestData());

    restrictedKeywordService.processRestrictedKeywordBulkOperation(STORE_ID, restrictedKeywordProcessModel);

    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(CATEGORY_CODE,
            BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name());
    Mockito.verify(internalProcessService, Mockito.times(2)).saveInternalProcessData(Arrays.asList(addRestrictedKeyword));
    Mockito.verify(pcbOutboundService)
        .updateCategoriesWithRestrictedKeywords(Mockito.eq(STORE_ID), Mockito.eq(CATEGORY_CODE),
            Mockito.any(CategoryKeywordUpdateRequestList.class));
    Mockito.verify(mapper).readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class));
  }

  @Test
  public void processRestrictedKeywordBulkOperationAdditionSystemErrorTest() throws JsonProcessingException {
    RestrictedKeywordProcessModel restrictedKeywordProcessModel =
        RestrictedKeywordProcessModel.builder().storeId(STORE_ID).internalBulkRequestId(INTERNAL_PROCESS_REQUEST_ID)
            .internalBulkRequestCode(INTERNAL_PROCESS_REQUEST_ID).categoryCode(CATEGORY_CODE)
            .type(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()).build();

    Mockito.when(
        internalProcessService.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
            CATEGORY_CODE, BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name())).thenReturn(Arrays.asList(addRestrictedKeyword));
    Mockito.when(internalProcessService.saveInternalProcessData(Arrays.asList(addRestrictedKeyword)))
        .thenThrow(ApplicationRuntimeException.class);

    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> restrictedKeywordService.processRestrictedKeywordBulkOperation(STORE_ID, restrictedKeywordProcessModel));
    } finally {
      Mockito.verify(internalProcessService)
          .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(CATEGORY_CODE,
              BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), INTERNAL_PROCESS_REQUEST_ID,
              ProcessStatus.PICKED.name());
      Mockito.verify(internalProcessService, Mockito.times(2))
          .saveInternalProcessData(Arrays.asList(addRestrictedKeyword));
    }
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelCompletedTest() throws Exception {
    String id = UUID.randomUUID().toString();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setId(id);
    RestrictedKeywordRequestData restrictedKeywordRequestData1 = new RestrictedKeywordRequestData();
    restrictedKeywordRequestData1.setExcelRowNumber(1);
    String data1 = objectMapper.writeValueAsString(restrictedKeywordRequestData1);
    RestrictedKeywordRequestData restrictedKeywordRequestData2 = new RestrictedKeywordRequestData();
    restrictedKeywordRequestData2.setExcelRowNumber(2);
    String data2 = objectMapper.writeValueAsString(restrictedKeywordRequestData1);
    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessData1.setData(data1);
    BulkInternalProcessData bulkInternalProcessData2 = new BulkInternalProcessData();
    bulkInternalProcessData2.setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessData2.setData(data2);

    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, id))
        .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    Mockito.when(mapper.readValue(data1, RestrictedKeywordRequestData.class)).thenReturn(restrictedKeywordRequestData1);
    Mockito.when(mapper.readValue(data2, RestrictedKeywordRequestData.class)).thenReturn(restrictedKeywordRequestData2);
    Mockito.when(internalProcessService.saveInternalProcess(bulkInternalProcessArgumentCaptor.capture()))
        .thenReturn(bulkInternalProcess);
    Mockito.doNothing().when(mailDeliveryService)
        .sendEmail(bulkDownloadRequestArgumentCaptor.capture(), mapArgumentCaptor.capture());

    restrictedKeywordService.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);

    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, id);
    Mockito.verify(mapper, Mockito.times(2)).readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class));
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.getValue());
    Mockito.verify(mailDeliveryService)
        .sendEmail(bulkDownloadRequestArgumentCaptor.getValue(), mapArgumentCaptor.getValue());

  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelFailedTest() throws Exception {
    String id = UUID.randomUUID().toString();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setId(id);
    bulkInternalProcess.setInternalProcessRequestCode(id);
    bulkInternalProcess.setFileName(FILE_PATH);
    RestrictedKeywordRequestData restrictedKeywordRequestData1 = new RestrictedKeywordRequestData();
    restrictedKeywordRequestData1.setExcelRowNumber(1);
    String data1 = objectMapper.writeValueAsString(restrictedKeywordRequestData1);
    RestrictedKeywordRequestData restrictedKeywordRequestData2 = new RestrictedKeywordRequestData();
    restrictedKeywordRequestData2.setExcelRowNumber(2);
    String data2 = objectMapper.writeValueAsString(restrictedKeywordRequestData2);
    RestrictedKeywordRequestData restrictedKeywordRequestData3 = new RestrictedKeywordRequestData();
    restrictedKeywordRequestData3.setExcelRowNumber(3);
    String data3 = objectMapper.writeValueAsString(restrictedKeywordRequestData3);
    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData1.setData(data1);
    bulkInternalProcessData1.setErrorMessage("System error");
    BulkInternalProcessData bulkInternalProcessData2 = new BulkInternalProcessData();
    bulkInternalProcessData2.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData2.setData(data2);
    bulkInternalProcessData2.setParentCode(CATEGORY_CODE);
    bulkInternalProcessData2.setErrorMessage("not found category with codes");
    BulkInternalProcessData bulkInternalProcessData3 = new BulkInternalProcessData();
    bulkInternalProcessData3.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData3.setData(data3);
    bulkInternalProcessData3.setParentCode(CATEGORY_CODE);
    bulkInternalProcessData3.setErrorMessage("system error");

    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, id))
        .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2, bulkInternalProcessData3));
    Mockito.when(mapper.readValue(data1, RestrictedKeywordRequestData.class)).thenReturn(restrictedKeywordRequestData1);
    Mockito.when(mapper.readValue(data2, RestrictedKeywordRequestData.class)).thenReturn(restrictedKeywordRequestData2);
    Mockito.when(mapper.readValue(data3, RestrictedKeywordRequestData.class)).thenReturn(restrictedKeywordRequestData3);
    Mockito.when(internalProcessService.saveInternalProcess(bulkInternalProcessArgumentCaptor.capture()))
        .thenReturn(bulkInternalProcess);
    Mockito.doNothing().when(mailDeliveryService)
        .sendEmail(bulkDownloadRequestArgumentCaptor.capture(), mapArgumentCaptor.capture());
    Mockito.doNothing().when(fileStorageService).downloadFileAndGenerateErrorFile(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList(), Mockito.any());

    restrictedKeywordService.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);

    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, id);
    Mockito.verify(mapper, Mockito.times(3)).readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class));
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.getValue());
    Mockito.verify(mailDeliveryService)
        .sendEmail(bulkDownloadRequestArgumentCaptor.getValue(), mapArgumentCaptor.getValue());
    Mockito.verify(fileStorageService).downloadFileAndGenerateErrorFile(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList(), Mockito.any());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelPartiallyCompletedTest() throws Exception {
    String id = UUID.randomUUID().toString();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setId(id);
    bulkInternalProcess.setInternalProcessRequestCode(id);
    bulkInternalProcess.setFileName(FILE_PATH);
    RestrictedKeywordRequestData restrictedKeywordRequestData1 = new RestrictedKeywordRequestData();
    restrictedKeywordRequestData1.setExcelRowNumber(1);
    String data1 = objectMapper.writeValueAsString(restrictedKeywordRequestData1);
    RestrictedKeywordRequestData restrictedKeywordRequestData2 = new RestrictedKeywordRequestData();
    restrictedKeywordRequestData2.setExcelRowNumber(2);
    String data2 = objectMapper.writeValueAsString(restrictedKeywordRequestData2);
    RestrictedKeywordRequestData restrictedKeywordRequestData3 = new RestrictedKeywordRequestData();
    restrictedKeywordRequestData3.setExcelRowNumber(3);
    String data3 = objectMapper.writeValueAsString(restrictedKeywordRequestData3);
    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData1.setData(data1);
    bulkInternalProcessData1.setErrorMessage("System error");
    BulkInternalProcessData bulkInternalProcessData2 = new BulkInternalProcessData();
    bulkInternalProcessData2.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData2.setData(data2);
    bulkInternalProcessData2.setParentCode(CATEGORY_CODE);
    bulkInternalProcessData2.setErrorMessage("not found category with codes");
    BulkInternalProcessData bulkInternalProcessData3 = new BulkInternalProcessData();
    bulkInternalProcessData3.setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessData3.setData(data3);
    bulkInternalProcessData3.setParentCode(CATEGORY_CODE);
    bulkInternalProcessData3.setErrorMessage("system error");

    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, id))
        .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2, bulkInternalProcessData3));
    Mockito.when(mapper.readValue(data1, RestrictedKeywordRequestData.class)).thenReturn(restrictedKeywordRequestData1);
    Mockito.when(mapper.readValue(data2, RestrictedKeywordRequestData.class)).thenReturn(restrictedKeywordRequestData2);
    Mockito.when(mapper.readValue(data3, RestrictedKeywordRequestData.class)).thenReturn(restrictedKeywordRequestData3);
    Mockito.when(internalProcessService.saveInternalProcess(bulkInternalProcessArgumentCaptor.capture()))
        .thenReturn(bulkInternalProcess);
    Mockito.doNothing().when(mailDeliveryService)
        .sendEmail(bulkDownloadRequestArgumentCaptor.capture(), mapArgumentCaptor.capture());
    Mockito.doNothing().when(fileStorageService).downloadFileAndGenerateErrorFile(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList(), Mockito.any());

    restrictedKeywordService.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);

    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, id);
    Mockito.verify(mapper, Mockito.times(3)).readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class));
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.getValue());
    Mockito.verify(mailDeliveryService)
        .sendEmail(bulkDownloadRequestArgumentCaptor.getValue(), mapArgumentCaptor.getValue());
    Mockito.verify(fileStorageService).downloadFileAndGenerateErrorFile(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList(), Mockito.any());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelProcessingTest() throws Exception {
    String id = UUID.randomUUID().toString();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setId(id);
    bulkInternalProcess.setInternalProcessRequestCode(id);
    bulkInternalProcess.setFileName(FILE_PATH);
    RestrictedKeywordRequestData restrictedKeywordRequestData1 = new RestrictedKeywordRequestData();
    restrictedKeywordRequestData1.setExcelRowNumber(1);
    String data1 = objectMapper.writeValueAsString(restrictedKeywordRequestData1);
    RestrictedKeywordRequestData restrictedKeywordRequestData2 = new RestrictedKeywordRequestData();
    restrictedKeywordRequestData2.setExcelRowNumber(2);
    String data2 = objectMapper.writeValueAsString(restrictedKeywordRequestData2);
    RestrictedKeywordRequestData restrictedKeywordRequestData3 = new RestrictedKeywordRequestData();
    restrictedKeywordRequestData3.setExcelRowNumber(3);
    String data3 = objectMapper.writeValueAsString(restrictedKeywordRequestData3);
    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setStatus(ProcessStatus.IN_PROGRESS.name());
    bulkInternalProcessData1.setData(data1);
    bulkInternalProcessData1.setErrorMessage("System error");
    BulkInternalProcessData bulkInternalProcessData2 = new BulkInternalProcessData();
    bulkInternalProcessData2.setStatus(ProcessStatus.PICKED.name());
    bulkInternalProcessData2.setData(data2);
    bulkInternalProcessData2.setParentCode(CATEGORY_CODE);
    bulkInternalProcessData2.setErrorMessage("not found category with codes");
    BulkInternalProcessData bulkInternalProcessData3 = new BulkInternalProcessData();
    bulkInternalProcessData3.setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessData3.setData(data3);
    bulkInternalProcessData3.setParentCode(CATEGORY_CODE);
    bulkInternalProcessData3.setErrorMessage("system error");

    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, id))
        .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2, bulkInternalProcessData3));

    restrictedKeywordService.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);

    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, id);
  }

  @Test
  public void processRestrictedKeywordBulkOperationAdditionDestinationCategoryTest() throws JsonProcessingException {
    RestrictedKeywordProcessModel restrictedKeywordProcessModel =
        RestrictedKeywordProcessModel.builder().storeId(STORE_ID).internalBulkRequestId(INTERNAL_PROCESS_REQUEST_ID)
            .internalBulkRequestCode(INTERNAL_PROCESS_REQUEST_ID).categoryCode(CATEGORY_CODE)
            .type(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()).build();
    restrictedKeywordRequestData.setDestinationCategory(CATEGORY_CODE);

    Mockito.when(
        internalProcessService.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
            CATEGORY_CODE, BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name())).thenReturn(Arrays.asList(addRestrictedKeyword));
    Mockito.when(internalProcessService.saveInternalProcessData(Arrays.asList(addRestrictedKeyword)))
        .thenReturn(Arrays.asList(addRestrictedKeyword));
    Mockito.when(mapper.readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class)))
        .thenReturn(restrictedKeywordRequestData);
    Mockito.when(pcbOutboundService.validateDestinationCategory(STORE_ID, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(new CategoryErrorResponse(CATEGORY_CODE, CATEGORY_KEYWORDS_UPDATE_ERROR_MESSAGE)));

    restrictedKeywordService.processRestrictedKeywordBulkOperation(STORE_ID, restrictedKeywordProcessModel);

    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(CATEGORY_CODE,
            BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name());
    Mockito.verify(internalProcessService, Mockito.times(2)).saveInternalProcessData(Arrays.asList(addRestrictedKeyword));
    Mockito.verify(mapper).readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class));
    Mockito.verify(pcbOutboundService).validateDestinationCategory(STORE_ID, Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void processRestrictedKeywordBulkOperationAdditionDestinationCategoryNoErrorTest() throws JsonProcessingException {
    RestrictedKeywordProcessModel restrictedKeywordProcessModel =
        RestrictedKeywordProcessModel.builder().storeId(STORE_ID).internalBulkRequestId(INTERNAL_PROCESS_REQUEST_ID)
            .internalBulkRequestCode(INTERNAL_PROCESS_REQUEST_ID).categoryCode(CATEGORY_CODE)
            .type(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()).build();
    restrictedKeywordRequestData.setDestinationCategory(CATEGORY_CODE);

    Mockito.when(
        internalProcessService.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
            CATEGORY_CODE, BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name())).thenReturn(Arrays.asList(addRestrictedKeyword));
    Mockito.when(internalProcessService.saveInternalProcessData(Arrays.asList(addRestrictedKeyword)))
        .thenReturn(Arrays.asList(addRestrictedKeyword));
    Mockito.when(
        pcbOutboundService.updateCategoriesWithRestrictedKeywords(Mockito.eq(STORE_ID), Mockito.eq(CATEGORY_CODE),
            Mockito.any(CategoryKeywordUpdateRequestList.class))).thenReturn(StringUtils.EMPTY);
    Mockito.when(mapper.readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class)))
        .thenReturn(restrictedKeywordRequestData);
    Mockito.when(pcbOutboundService.validateDestinationCategory(STORE_ID, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(new ArrayList<>());

    restrictedKeywordService.processRestrictedKeywordBulkOperation(STORE_ID, restrictedKeywordProcessModel);

    Mockito.verify(internalProcessService)
        .findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(CATEGORY_CODE,
            BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), INTERNAL_PROCESS_REQUEST_ID,
            ProcessStatus.PICKED.name());
    Mockito.verify(internalProcessService, Mockito.times(2)).saveInternalProcessData(Arrays.asList(addRestrictedKeyword));
    Mockito.verify(pcbOutboundService)
        .updateCategoriesWithRestrictedKeywords(Mockito.eq(STORE_ID), Mockito.eq(CATEGORY_CODE), Mockito.any(CategoryKeywordUpdateRequestList.class));
    Mockito.verify(mapper).readValue(Mockito.anyString(), Mockito.eq(RestrictedKeywordRequestData.class));
    Mockito.verify(pcbOutboundService).validateDestinationCategory(STORE_ID, Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void publishRestrictedKeywordBulkUploadTest() {
    String id = UUID.randomUUID().toString();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setId(id);

    Mockito.when(internalProcessService.getDistinctCategoryCodeForRestrictedKeywordBulkUpdate(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(ImmutableSet.of(CATEGORY_CODE));
    Mockito.doNothing().when(internalProcessService)
        .bulkInterUpdatedStatusForRestrictedKeywordBulkUpdate(ProcessStatus.PICKED.name(), Constant.SYSTEM, STORE_ID,
            id, ImmutableSet.of(CATEGORY_CODE));

    restrictedKeywordService.publishRestrictedKeywordBulkUpload(STORE_ID, Arrays.asList(bulkInternalProcess), 1);

    Mockito.verify(internalProcessService).getDistinctCategoryCodeForRestrictedKeywordBulkUpdate(STORE_ID,
        bulkInternalProcess.getId());
    Mockito.verify(internalProcessService)
        .bulkInterUpdatedStatusForRestrictedKeywordBulkUpdate(ProcessStatus.PICKED.name(), Constant.SYSTEM, STORE_ID,
            id, ImmutableSet.of(CATEGORY_CODE));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getRestrictedKeywordBulkProcess();
  }

  @Test
  public void publishRestrictedKeywordBulkUploadEmptyTest() {
    String id = UUID.randomUUID().toString();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setId(id);

    Mockito.when(internalProcessService.getDistinctCategoryCodeForRestrictedKeywordBulkUpdate(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(new HashSet<>());

    restrictedKeywordService.publishRestrictedKeywordBulkUpload(STORE_ID, Arrays.asList(bulkInternalProcess), 1);

    Mockito.verify(internalProcessService).getDistinctCategoryCodeForRestrictedKeywordBulkUpdate(STORE_ID,
        bulkInternalProcess.getId());
  }
}
