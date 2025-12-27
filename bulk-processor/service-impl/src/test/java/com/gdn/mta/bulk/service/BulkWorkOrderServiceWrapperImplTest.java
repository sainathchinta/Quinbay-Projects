package com.gdn.mta.bulk.service;

import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.WorkOrderEventModel;
import com.gdn.mta.bulk.models.MasterWarehouseResponse;
import com.gdn.mta.bulk.models.WorkOrderDataModel;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.partners.bulk.util.BulkWorkOrderConstants;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;

public class BulkWorkOrderServiceWrapperImplTest {

  private static final String BULK_PROCESS_CODE = "BULK_PROCESS_CODE";
  private static final String ITEM_SKU = "PRODUCT-SKU-00001-00001";
  private static final String PRODUCT_SKU = "PRODUCT-SKU-00001";
  private static final String WAREHOUSE_CODE = "WAREHOUSE_CODE";
  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private static final String PAGE = "1";
  private static final String LIMIT = "500";

  @InjectMocks
  private BulkWorkOrderServiceWrapperImpl bulkWorkOrderServiceWrapper;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private ProductAssemblyOutboundService productAssemblyOutboundService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private BulkWorkOrderService bulkWorkOrderService;

  private WorkOrderDataModel workOrderDataModel;
  private WorkOrderEventModel workOrderEventModel;
  private String message;
  private BulkProcessData bulkProcessData;
  private BulkProcess bulkProcess;
  private BasicProductResponse basicProductResponse;

  @BeforeEach()
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);
    workOrderDataModel = new WorkOrderDataModel();
    workOrderEventModel = new WorkOrderEventModel();
    bulkProcessData = new BulkProcessData();
    bulkProcess = new BulkProcess();
    basicProductResponse = new BasicProductResponse();
    bulkProcess.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkProcess.setInternationalMerchant(true);
    bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
    workOrderEventModel.setId(Constant.ID);
    workOrderEventModel.setStoreId(Constant.STORE_ID);
    workOrderEventModel.setBulkProcessId(Constant.ID);
    workOrderEventModel.setRowNumber(1);
    workOrderEventModel.setBulkProcessCode(BULK_PROCESS_CODE);
    workOrderDataModel.setSourceItemSku(ITEM_SKU);
    workOrderDataModel.setWarehouseCode(WAREHOUSE_CODE);
    message = new ObjectMapper().writeValueAsString(workOrderDataModel);
    bulkProcessData.setBulkRequestData(message);
    ReflectionTestUtils.setField(bulkWorkOrderServiceWrapper, "productBundlingEligibleMerchantTypes", "TD");
    ReflectionTestUtils.setField(bulkWorkOrderServiceWrapper, "page", "1");
    ReflectionTestUtils.setField(bulkWorkOrderServiceWrapper, "limit", "500");
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkProcessDataService, bulkProcessService, objectMapper, xProductOutboundService,
        productAssemblyOutboundService, businessPartnerRepository, bulkWorkOrderService);
  }

  @Test
  public void processBulkWorkOrderUploadEmptyDataRowsTest() {
    Mockito.when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService.findBulkProcessDataById(Constant.ID)).thenReturn(Optional.empty());
    bulkWorkOrderServiceWrapper.processBulkWorkOrderUpload(workOrderEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService).findBulkProcessDataById(Constant.ID);
  }

  @Test
  public void processBulkWorkOrderUploadNotPendingTest() {
    bulkProcessData.setStatus(null);
    Mockito.when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService.findBulkProcessDataById(Constant.ID)).thenReturn(Optional.of(bulkProcessData));
    bulkWorkOrderServiceWrapper.processBulkWorkOrderUpload(workOrderEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService).findBulkProcessDataById(Constant.ID);
  }

  @Test
  public void processBulkWorkOrderUploadCCMerchantTest() throws Exception {
    Mockito.when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService.findBulkProcessDataById(Constant.ID)).thenReturn(Optional.of(bulkProcessData));
    Mockito.when(objectMapper.readValue(message, WorkOrderDataModel.class)).thenReturn(workOrderDataModel);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(
            ProfileResponse.builder().company(CompanyDTO.builder().merchantType(Constant.CC_MERCHANT).build()).build());
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    bulkWorkOrderServiceWrapper.processBulkWorkOrderUpload(workOrderEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService).findBulkProcessDataById(Constant.ID);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveOperation(bulkProcessData);
    Mockito.verify(objectMapper).readValue(message, WorkOrderDataModel.class);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.NOT_ELIGIBLE_FOR_WORK_ORDER, bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void processBulkWorkOrderUploadWarehouseCodeInvalidTest() throws Exception {
    Mockito.when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService.findBulkProcessDataById(Constant.ID)).thenReturn(Optional.of(bulkProcessData));
    Mockito.when(objectMapper.readValue(message, WorkOrderDataModel.class)).thenReturn(workOrderDataModel);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(
            ProfileResponse.builder().company(CompanyDTO.builder().merchantType(Constant.TD_MERCHANT).build()).build());
    Mockito.when(xProductOutboundService.getBasicProductInfo(Constant.STORE_ID, PRODUCT_SKU))
        .thenReturn(basicProductResponse);
    Mockito.when(productAssemblyOutboundService.getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, PAGE, LIMIT))
        .thenReturn(List.of(new MasterWarehouseResponse(ITEM_SKU, PRODUCT_SKU)));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    bulkWorkOrderServiceWrapper.processBulkWorkOrderUpload(workOrderEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService).findBulkProcessDataById(Constant.ID);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveOperation(bulkProcessData);
    Mockito.verify(objectMapper).readValue(message, WorkOrderDataModel.class);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(Constant.STORE_ID, PRODUCT_SKU);
    Mockito.verify(productAssemblyOutboundService).getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, PAGE, LIMIT);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.WAREHOUSE_CODE_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void processBulkWorkOrderUploadAssemblyRequestTest() throws Exception {
    bulkProcess.setBulkProcessType(BulkWorkOrderConstants.ASSEMBLY_BULK_PROCESS_TYPE);
    Mockito.when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService.findBulkProcessDataById(Constant.ID)).thenReturn(Optional.of(bulkProcessData));
    Mockito.when(objectMapper.readValue(message, WorkOrderDataModel.class)).thenReturn(workOrderDataModel);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(
            ProfileResponse.builder().company(CompanyDTO.builder().merchantType(Constant.TD_MERCHANT).build()).build());
    Mockito.when(xProductOutboundService.getBasicProductInfo(Constant.STORE_ID, PRODUCT_SKU))
        .thenReturn(basicProductResponse);
    Mockito.when(productAssemblyOutboundService.getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, PAGE, LIMIT))
        .thenReturn(List.of(new MasterWarehouseResponse(WAREHOUSE_CODE, PRODUCT_SKU)));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    bulkWorkOrderServiceWrapper.processBulkWorkOrderUpload(workOrderEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService).findBulkProcessDataById(Constant.ID);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveOperation(bulkProcessData);
    Mockito.verify(objectMapper).readValue(message, WorkOrderDataModel.class);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(Constant.STORE_ID, PRODUCT_SKU);
    Mockito.verify(productAssemblyOutboundService).getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, PAGE, LIMIT);
    Mockito.verify(bulkWorkOrderService)
        .validateAndProcessAssemblyDisassemblyRequest(Constant.STORE_ID, Constant.TD_MERCHANT, ITEM_SKU, PRODUCT_SKU,
            WAREHOUSE_CODE, true, BulkWorkOrderConstants.ASSEMBLY_BULK_PROCESS_TYPE, bulkProcess, bulkProcessData,
            workOrderDataModel, basicProductResponse);
    Assertions.assertNotEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertNotEquals(BulkWorkOrderConstants.WAREHOUSE_CODE_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertNull(bulkProcessData.getInputErrorCount());
  }

  @Test
  public void processBulkWorkOrderUploadDisassemblyRequestTest() throws Exception {
    bulkProcess.setBulkProcessType(BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE);
    Mockito.when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService.findBulkProcessDataById(Constant.ID)).thenReturn(Optional.of(bulkProcessData));
    Mockito.when(objectMapper.readValue(message, WorkOrderDataModel.class)).thenReturn(workOrderDataModel);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(
            ProfileResponse.builder().company(CompanyDTO.builder().merchantType(Constant.TD_MERCHANT).build()).build());
    Mockito.when(xProductOutboundService.getBasicProductInfo(Constant.STORE_ID, PRODUCT_SKU))
        .thenReturn(basicProductResponse);
    Mockito.when(productAssemblyOutboundService.getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, PAGE, LIMIT))
        .thenReturn(List.of(new MasterWarehouseResponse(WAREHOUSE_CODE, PRODUCT_SKU)));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    bulkWorkOrderServiceWrapper.processBulkWorkOrderUpload(workOrderEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService).findBulkProcessDataById(Constant.ID);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveOperation(bulkProcessData);
    Mockito.verify(objectMapper).readValue(message, WorkOrderDataModel.class);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(Constant.STORE_ID, PRODUCT_SKU);
    Mockito.verify(productAssemblyOutboundService).getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, PAGE, LIMIT);
    Mockito.verify(bulkWorkOrderService)
        .validateAndProcessAssemblyDisassemblyRequest(Constant.STORE_ID, Constant.TD_MERCHANT, ITEM_SKU, PRODUCT_SKU,
            WAREHOUSE_CODE, true, BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE, bulkProcess, bulkProcessData,
            workOrderDataModel, basicProductResponse);
    Assertions.assertNotEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertNotEquals(BulkWorkOrderConstants.WAREHOUSE_CODE_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertNull(bulkProcessData.getInputErrorCount());
  }

  @Test
  public void processBulkWorkOrderUploadTransferRequestTest() throws Exception {
    bulkProcess.setBulkProcessType(BulkWorkOrderConstants.TRANSFER_BULK_PROCESS_TYPE);
    Mockito.when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService.findBulkProcessDataById(Constant.ID)).thenReturn(Optional.of(bulkProcessData));
    Mockito.when(objectMapper.readValue(message, WorkOrderDataModel.class)).thenReturn(workOrderDataModel);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(
            ProfileResponse.builder().company(CompanyDTO.builder().merchantType(Constant.TD_MERCHANT).build()).build());
    Mockito.when(xProductOutboundService.getBasicProductInfo(Constant.STORE_ID, PRODUCT_SKU))
        .thenReturn(basicProductResponse);
    Mockito.when(productAssemblyOutboundService.getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, PAGE, LIMIT))
        .thenReturn(List.of(new MasterWarehouseResponse(WAREHOUSE_CODE, PRODUCT_SKU)));
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    bulkWorkOrderServiceWrapper.processBulkWorkOrderUpload(workOrderEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService).findBulkProcessDataById(Constant.ID);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveOperation(bulkProcessData);
    Mockito.verify(objectMapper).readValue(message, WorkOrderDataModel.class);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(Constant.STORE_ID, PRODUCT_SKU);
    Mockito.verify(productAssemblyOutboundService).getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, PAGE, LIMIT);
    Mockito.verify(bulkWorkOrderService)
        .validateAndProcessTransferRequest(Constant.STORE_ID, Constant.TD_MERCHANT, ITEM_SKU, PRODUCT_SKU,
            WAREHOUSE_CODE, true, bulkProcess, bulkProcessData, workOrderDataModel, basicProductResponse);
    Assertions.assertNotEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertNotEquals(BulkWorkOrderConstants.WAREHOUSE_CODE_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertNull(bulkProcessData.getInputErrorCount());
  }

  @Test
  public void processBulkWorkOrderUploadExceptionTest() throws Exception {
    bulkProcess.setBulkProcessType(BulkWorkOrderConstants.TRANSFER_BULK_PROCESS_TYPE);
    Mockito.when(bulkProcessService.findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkProcess);
    Mockito.when(bulkProcessDataService.findBulkProcessDataById(Constant.ID)).thenReturn(Optional.of(bulkProcessData));
    Mockito.when(objectMapper.readValue(message, WorkOrderDataModel.class)).thenReturn(workOrderDataModel);
    Mockito.when(businessPartnerRepository.filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(
            ProfileResponse.builder().company(CompanyDTO.builder().merchantType(Constant.TD_MERCHANT).build()).build());
    Mockito.when(xProductOutboundService.getBasicProductInfo(Constant.STORE_ID, PRODUCT_SKU))
        .thenReturn(basicProductResponse);
    Mockito.when(productAssemblyOutboundService.getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, PAGE, LIMIT))
        .thenReturn(List.of(new MasterWarehouseResponse(WAREHOUSE_CODE, PRODUCT_SKU)));
    Mockito.doThrow(ApplicationRuntimeException.class).when(bulkWorkOrderService)
        .validateAndProcessTransferRequest(Constant.STORE_ID, Constant.TD_MERCHANT, ITEM_SKU, PRODUCT_SKU,
            WAREHOUSE_CODE, true, bulkProcess, bulkProcessData, workOrderDataModel, basicProductResponse);
    Mockito.when(bulkProcessDataService.saveOperation(bulkProcessData)).thenReturn(bulkProcessData);
    bulkWorkOrderServiceWrapper.processBulkWorkOrderUpload(workOrderEventModel);
    Mockito.verify(bulkProcessService).findByBulkProcessCode(Constant.STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessDataService).findBulkProcessDataById(Constant.ID);
    Mockito.verify(bulkProcessDataService, Mockito.times(2)).saveOperation(bulkProcessData);
    Mockito.verify(objectMapper).readValue(message, WorkOrderDataModel.class);
    Mockito.verify(businessPartnerRepository).filterByBusinessPartnerCodeV2(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(Constant.STORE_ID, PRODUCT_SKU);
    Mockito.verify(productAssemblyOutboundService).getWarehouseCodeAndFulfillmentCenter(Constant.STORE_ID, PAGE, LIMIT);
    Mockito.verify(bulkWorkOrderService)
        .validateAndProcessTransferRequest(Constant.STORE_ID, Constant.TD_MERCHANT, ITEM_SKU, PRODUCT_SKU,
            WAREHOUSE_CODE, true, bulkProcess, bulkProcessData, workOrderDataModel, basicProductResponse);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(1, bulkProcessData.getSystemErrorCount().intValue());
    Assertions.assertEquals(Constant.SYSTEM_ERROR, bulkProcessData.getErrorMessage());
  }
}
