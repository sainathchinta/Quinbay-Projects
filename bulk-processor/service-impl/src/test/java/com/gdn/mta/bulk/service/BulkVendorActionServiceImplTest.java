package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.BrandAuthorizationEmailTemplate;
import com.gdn.mta.bulk.models.download.responsedata.BulkApprovalRejectionRequestData;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static com.gdn.mta.bulk.models.EmailConstants.*;

public class BulkVendorActionServiceImplTest {

    @InjectMocks
    private BulkVendorActionServiceImpl bulkVendorActionService;

    @Mock
    private FileStorageService fileStorageService;
    @Mock
    private MailDeliveryService mailDeliveryService;
    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private InternalProcessService internalProcessService;
    @Captor
    private ArgumentCaptor<BulkInternalProcess> bulkInternalProcessArgumentCaptor;
    private static final String STORE_ID =  "STORE_ID";
    private static final String ID =  "ID";
    private static final String DELETE_BRAND_AUTHORISATION =  "DELETE_BRAND_AUTHORISATION";
    private static final String ERROR_MESSAGE = "ERROR_MESSAGE";
    private static final String COMPLETED = "COMPLETED";
    private static final String PARTIAL_COMPLETED = "PARTIAL_COMPLETED";
    private static final String FAILED = "FAILED";
    private static final String USERNAME = "USERNAME";

  private BulkInternalProcess bulkInternalProcess;
  private BulkInternalProcessData bulkInternalProcessData1;
  private BulkInternalProcessData bulkInternalProcessData2;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
    bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(DELETE_BRAND_AUTHORISATION);
    bulkInternalProcess.setId(ID);
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setInternalProcessRequestCode(ID);
    bulkInternalProcess.setCreatedBy(USERNAME);
    bulkInternalProcess.setFileName(USERNAME);
    bulkInternalProcess.setErrorFilePath(USERNAME);
    bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData2 = new BulkInternalProcessData();
    ReflectionTestUtils.setField(bulkVendorActionService, "vendorActionErrorFilePath", ID);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkBrandAuthAddTest() throws Exception {
    BulkApprovalRejectionRequestData bulkVendorActionsModel =
            BulkApprovalRejectionRequestData.builder().excelRowNumber(1).build();
    String message = new ObjectMapper().writeValueAsString(bulkVendorActionsModel);
    bulkInternalProcessData1.setProcessType(BulkInternalProcessType.BULK_APPROVAL.name());
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_APPROVAL.name());
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcessData1.setData(message);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
          STORE_ID, bulkInternalProcess.getId()))
      .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    Mockito.when(objectMapper.readValue(message, BulkApprovalRejectionRequestData.class))
      .thenReturn(bulkVendorActionsModel);
    bulkVendorActionService.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.anyMap());
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkVendorApprovalTest() throws Exception {
    bulkInternalProcessData1.setStatus(PARTIAL_COMPLETED);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
          STORE_ID, bulkInternalProcess.getId()))
      .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    bulkVendorActionService.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkBulkActionsNullTest() throws Exception {
    Mockito.when(
      internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, bulkInternalProcess.getId())).thenReturn(null);
    bulkVendorActionService.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkBrandAuthDeleteTest() throws Exception {
    BulkApprovalRejectionRequestData bulkVendorActionsModel =
            BulkApprovalRejectionRequestData.builder().excelRowNumber(1).build();
    String message = new ObjectMapper().writeValueAsString(bulkVendorActionsModel);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    bulkInternalProcessData1.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcessData1.setData(message);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
          STORE_ID, bulkInternalProcess.getId()))
      .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    Mockito.when(objectMapper.readValue(message, BulkApprovalRejectionRequestData.class))
      .thenReturn(bulkVendorActionsModel);
    bulkVendorActionService.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.anyMap());
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId());
  }

  @Test
  public void processFinalStatusForBulkVendorApprovalTest() throws Exception {
    BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate =
      bulkVendorActionService.processFinalStatusForBulkVendorApproval(bulkInternalProcess,
        new HashMap<>(), 0, new String(), new BrandAuthorizationEmailTemplate());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
    Assertions.assertEquals(VENDOR_APPROVAL_TEMPLATE_ID,
      brandAuthorizationEmailTemplate.getEmailTemplateId());
    Assertions.assertEquals(VENDOR_APPROVAL_COMPLETED_TEMPLATE,
      brandAuthorizationEmailTemplate.getEmailTemplate());
    Assertions.assertEquals(0, brandAuthorizationEmailTemplate.getSuccessCount().intValue());
  }

  @Test
  public void processFinalStatusForBulkVendorApprovalFailedTest() throws Exception {
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcess.setFileName(STORE_ID);
    bulkInternalProcess.setErrorFilePath(STORE_ID);
    Map<Integer, BulkInternalProcessData> map = new HashMap<>();
    map.put(1, bulkInternalProcessData1);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any()))
      .thenReturn(bulkInternalProcess);
    bulkVendorActionService.processFinalStatusForBulkVendorApproval(bulkInternalProcess, map, 1,
      new String(), new BrandAuthorizationEmailTemplate());
    Mockito.verify(fileStorageService)
      .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList(), Mockito.any());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
  }

  @Test
  public void processFinalStatusForBulkApprovalPartialTest() throws Exception {
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setFileName(STORE_ID);
    bulkInternalProcess.setErrorFilePath(STORE_ID);
    Map<Integer, BulkInternalProcessData> map = new HashMap<>();
    map.put(1, bulkInternalProcessData1);
    map.put(2, bulkInternalProcessData2);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any()))
      .thenReturn(bulkInternalProcess);
    BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate =
      bulkVendorActionService.processFinalStatusForBulkVendorApproval(bulkInternalProcess, map, 1,
        new String(), new BrandAuthorizationEmailTemplate());
    Mockito.verify(fileStorageService)
      .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList(), Mockito.any());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
  }

  @Test
  public void processFinalStatusForRejectionSuccessTest() throws Exception {
    BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate =
      bulkVendorActionService.processFinalStatusForBulkVendorRejection(bulkInternalProcess,
        new HashMap<>(), 0, new String(), new BrandAuthorizationEmailTemplate());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
    Assertions.assertEquals(VENDOR_REJECTION_COMPLETED_TEMPLATE_ID,
      brandAuthorizationEmailTemplate.getEmailTemplateId());
    Assertions.assertEquals(VENDOR_REJECTION_COMPLETED_TEMPLATE,
      brandAuthorizationEmailTemplate.getEmailTemplate());
    Assertions.assertEquals(0, brandAuthorizationEmailTemplate.getSuccessCount().intValue());
  }

  @Test
  public void processFinalStatusForRejectionFailedTest() throws Exception {
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcess.setFileName(STORE_ID);
    bulkInternalProcess.setErrorFilePath(STORE_ID);
    Map<Integer, BulkInternalProcessData> map = new HashMap<>();
    map.put(1, bulkInternalProcessData1);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any()))
      .thenReturn(bulkInternalProcess);
    BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate =
      bulkVendorActionService.processFinalStatusForBulkVendorRejection(bulkInternalProcess, map, 1,
        new String(), new BrandAuthorizationEmailTemplate());
    Mockito.verify(fileStorageService)
      .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList(), Mockito.any());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
    Assertions.assertEquals(VENDOR_REJECTION_FAILED_TEMPLATE_ID,
      brandAuthorizationEmailTemplate.getEmailTemplateId());
    Assertions.assertEquals(VENDOR_REJECTION_FAILED_TEMPLATE,
      brandAuthorizationEmailTemplate.getEmailTemplate());
    Assertions.assertEquals(0, brandAuthorizationEmailTemplate.getSuccessCount().intValue());
  }

  @Test
  public void processFinalStatusForBulkRejectionPartialTest() throws Exception {
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setFileName(STORE_ID);
    bulkInternalProcess.setErrorFilePath(STORE_ID);
    Map<Integer, BulkInternalProcessData> map = new HashMap<>();
    map.put(1, bulkInternalProcessData1);
    map.put(2, bulkInternalProcessData2);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any()))
      .thenReturn(bulkInternalProcess);
    BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate =
      bulkVendorActionService.processFinalStatusForBulkVendorRejection(bulkInternalProcess, map, 1,
        new String(), new BrandAuthorizationEmailTemplate());
    Mockito.verify(fileStorageService)
      .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList(), Mockito.any());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
    Assertions.assertEquals(VENDOR_REJECTION_PARTIALLY_COMPLETED_TEMPLATE_ID,
      brandAuthorizationEmailTemplate.getEmailTemplateId());
    Assertions.assertEquals(VENDOR_REJECTION_PARTIALLY_COMPLETED_TEMPLATE,
      brandAuthorizationEmailTemplate.getEmailTemplate());
    Assertions.assertEquals(1, brandAuthorizationEmailTemplate.getSuccessCount().intValue());
  }


}
