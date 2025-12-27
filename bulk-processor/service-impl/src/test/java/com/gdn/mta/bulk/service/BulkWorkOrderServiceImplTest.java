package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

import java.util.Arrays;
import java.util.Collections;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.models.ChildSkuAndCogsMapping;
import com.gdn.mta.bulk.models.SimpleListAssemblyDisassemblyRequest;
import com.gdn.mta.bulk.models.TransferRequest;
import com.gdn.mta.bulk.models.WorkOrderDataModel;
import com.gdn.partners.bulk.util.BulkWorkOrderConstants;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.BundleRecipeV2Response;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;

public class BulkWorkOrderServiceImplTest {

  private static final String STORE_ID = "STORE_ID";
  private static final String MERCHANT_TYPE = "MERCHANT_TYPE";
  private static final String ITEM_SKU = "PRODUCT-SKU-00001-00001";
  private static final String PRODUCT_SKU = "PRODUCT-SKU-00001";
  private static final String WAREHOUSE_CODE = "WAREHOUSE_CODE";
  private static final String WORK_ORDER_TYPE = "WORK_ORDER_TYPE";
  private static final String REQUEST_ID = "request_id";
  private static final String CREATED_BY = "username";
  private static final String DESTINATION_ITEM_SKU = "PRODUCT-SKU-00001-00002";

  @InjectMocks
  private BulkWorkOrderServiceImpl bulkWorkOrderService;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private ProductAssemblyOutboundService productAssemblyOutboundService;


  private BulkProcess bulkProcess;
  private BulkProcessData bulkProcessData;
  private WorkOrderDataModel workOrderDataModel;
  private BasicProductResponse productResponse;
  private BasicProductResponse destinationProductResponse;
  private SimpleListStringRequest simpleListStringRequest;
  private ItemBasicDetailV2Response itemBasicDetailV2Response;
  private ItemBasicDetailV2Response destinationItemBasicDetailV2Response;

  @BeforeEach()
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);
    bulkProcess = new BulkProcess();
    bulkProcessData = new BulkProcessData();
    workOrderDataModel = new WorkOrderDataModel();
    productResponse = new BasicProductResponse();
    destinationProductResponse = new BasicProductResponse();
    simpleListStringRequest = new SimpleListStringRequest();
    itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setItemSku(ITEM_SKU);
    destinationItemBasicDetailV2Response = new ItemBasicDetailV2Response();
    workOrderDataModel.setStock("1");
    destinationItemBasicDetailV2Response.setItemSku(DESTINATION_ITEM_SKU);
    bulkProcess.setRequestId(REQUEST_ID);
    bulkProcess.setCreatedBy(CREATED_BY);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(xProductOutboundService, productAssemblyOutboundService);
  }

  @Test
  public void validateAndProcessAssemblyDisassemblyRequestDeletedProductTest() {
    productResponse.setMarkForDelete(true);
    bulkWorkOrderService.validateAndProcessAssemblyDisassemblyRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, WORK_ORDER_TYPE, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.ITEM_SKU_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessAssemblyDisassemblyRequestArchivedProductTest() {
    productResponse.setArchived(true);
    bulkWorkOrderService.validateAndProcessAssemblyDisassemblyRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, WORK_ORDER_TYPE, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.ITEM_SKU_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessAssemblyDisassemblyRequestNotBundleTest() {
    productResponse.setBundleProduct(false);
    bulkWorkOrderService.validateAndProcessAssemblyDisassemblyRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, WORK_ORDER_TYPE, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.NOT_A_BUNDLE_PRODUCT.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessAssemblyDisassemblyRequestItemNotFoundTest() {
    productResponse.setBundleProduct(true);
    simpleListStringRequest.setValue(Collections.singletonList(ITEM_SKU));
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true, simpleListStringRequest))
        .thenThrow(ApplicationRuntimeException.class);
    bulkWorkOrderService.validateAndProcessAssemblyDisassemblyRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, WORK_ORDER_TYPE, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true, simpleListStringRequest);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.ITEM_SKU_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessAssemblyDisassemblyRequestItemDeletedTest() {
    productResponse.setBundleProduct(true);
    itemBasicDetailV2Response.setMarkForDelete(true);
    simpleListStringRequest.setValue(Collections.singletonList(ITEM_SKU));
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true, simpleListStringRequest))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response));
    bulkWorkOrderService.validateAndProcessAssemblyDisassemblyRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, BulkWorkOrderConstants.ASSEMBLY_BULK_PROCESS_TYPE, bulkProcess, bulkProcessData,
        workOrderDataModel, productResponse);
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true, simpleListStringRequest);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.ITEM_SKU_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessAssemblyDisassemblyRequestItemCogsMapEmptyTest() {
    productResponse.setBundleProduct(true);
    itemBasicDetailV2Response.setMarkForDelete(false);
    simpleListStringRequest.setValue(Collections.singletonList(ITEM_SKU));
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true, simpleListStringRequest))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response));
    bulkWorkOrderService.validateAndProcessAssemblyDisassemblyRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, BulkWorkOrderConstants.ASSEMBLY_BULK_PROCESS_TYPE, bulkProcess, bulkProcessData,
        workOrderDataModel, productResponse);
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true, simpleListStringRequest);
    Mockito.verify(productAssemblyOutboundService)
        .assemblyDisassemblyRequest(eq(STORE_ID), eq(BulkWorkOrderConstants.ASSEMBLY_BULK_PROCESS_TYPE), eq(REQUEST_ID),
            eq(CREATED_BY), any(SimpleListAssemblyDisassemblyRequest.class));
    Assertions.assertEquals(BulkProcessData.STATUS_SUCCESS, bulkProcessData.getStatus());
    Assertions.assertNull(bulkProcessData.getInputErrorCount());
  }

  @Test
  public void validateAndProcessAssemblyDisassemblyRequestDisassemblyEmptyItemCogsMapTest() {
    productResponse.setBundleProduct(true);
    itemBasicDetailV2Response.setMarkForDelete(false);
    workOrderDataModel.setChildSkuAndCogsMapping(Arrays.asList(new ChildSkuAndCogsMapping()));
    simpleListStringRequest.setValue(Collections.singletonList(ITEM_SKU));
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true, simpleListStringRequest))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response));
    bulkWorkOrderService.validateAndProcessAssemblyDisassemblyRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE, bulkProcess, bulkProcessData,
        workOrderDataModel, productResponse);
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true, simpleListStringRequest);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.CHILD_SKU_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessAssemblyDisassemblyRequestDisassemblyExceptionTest() {
    productResponse.setBundleProduct(true);
    itemBasicDetailV2Response.setMarkForDelete(false);
    BundleRecipeV2Response bundleRecipeV2Response = new BundleRecipeV2Response();
    bundleRecipeV2Response.setItemSku(ITEM_SKU);
    workOrderDataModel.setChildSkuAndCogsMapping(Arrays.asList(new ChildSkuAndCogsMapping(ITEM_SKU, "1")));
    itemBasicDetailV2Response.setBundleRecipeList(Arrays.asList(bundleRecipeV2Response));
    simpleListStringRequest.setValue(Collections.singletonList(ITEM_SKU));
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true, simpleListStringRequest))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response));
    Mockito.when(productAssemblyOutboundService.assemblyDisassemblyRequest(eq(STORE_ID),
        eq(BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE), eq(REQUEST_ID), eq(CREATED_BY),
        any(SimpleListAssemblyDisassemblyRequest.class))).thenThrow(ApplicationRuntimeException.class);
    bulkWorkOrderService.validateAndProcessAssemblyDisassemblyRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE, bulkProcess, bulkProcessData,
        workOrderDataModel, productResponse);
    Mockito.verify(productAssemblyOutboundService)
        .assemblyDisassemblyRequest(eq(STORE_ID), eq(BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE),
            eq(REQUEST_ID), eq(CREATED_BY), any(SimpleListAssemblyDisassemblyRequest.class));
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true, simpleListStringRequest);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessAssemblyDisassemblyRequestDisassemblyDifferentBundleRecipeTest() {
    productResponse.setBundleProduct(true);
    itemBasicDetailV2Response.setMarkForDelete(false);
    BundleRecipeV2Response bundleRecipeV2Response = new BundleRecipeV2Response();
    bundleRecipeV2Response.setItemSku(DESTINATION_ITEM_SKU);
    workOrderDataModel.setChildSkuAndCogsMapping(Arrays.asList(new ChildSkuAndCogsMapping(ITEM_SKU, "1")));
    itemBasicDetailV2Response.setBundleRecipeList(Arrays.asList(bundleRecipeV2Response));
    simpleListStringRequest.setValue(Collections.singletonList(ITEM_SKU));
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true, simpleListStringRequest))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response));
    bulkWorkOrderService.validateAndProcessAssemblyDisassemblyRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, BulkWorkOrderConstants.DISASSEMBLY_BULK_PROCESS_TYPE, bulkProcess, bulkProcessData,
        workOrderDataModel, productResponse);
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true, simpleListStringRequest);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.CHILD_SKU_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessTransferRequestSourceItemInvalidTest() {
    productResponse.setArchived(true);
    workOrderDataModel.setDestinationItemSku(ITEM_SKU);
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU)).thenReturn(productResponse);
    bulkWorkOrderService.validateAndProcessTransferRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.SOURCE_ITEM_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessTransferRequestDestinationItemInvalidTest() {
    workOrderDataModel.setDestinationItemSku(ITEM_SKU);
    destinationProductResponse.setArchived(true);
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(destinationProductResponse);
    bulkWorkOrderService.validateAndProcessTransferRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU);
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.DESTINATION_ITEM_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessTransferRequestSourceItemNotFoundTest() {
    workOrderDataModel.setDestinationItemSku(DESTINATION_ITEM_SKU);
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true,
            new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU))))
        .thenReturn(Collections.singletonList(destinationItemBasicDetailV2Response));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(destinationProductResponse);
    bulkWorkOrderService.validateAndProcessTransferRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU);
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true,
        new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU)));
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.SOURCE_ITEM_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessTransferRequestDestinationItemNotFoundTest() {
    workOrderDataModel.setDestinationItemSku(DESTINATION_ITEM_SKU);
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true,
            new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU))))
        .thenReturn(Collections.singletonList(itemBasicDetailV2Response));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(destinationProductResponse);
    bulkWorkOrderService.validateAndProcessTransferRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU);
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true,
        new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU)));
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.DESTINATION_ITEM_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessTransferRequestSourceItemDeletedTest() {
    workOrderDataModel.setDestinationItemSku(DESTINATION_ITEM_SKU);
    itemBasicDetailV2Response.setMarkForDelete(true);
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true,
            new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU))))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response, destinationItemBasicDetailV2Response));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(destinationProductResponse);
    bulkWorkOrderService.validateAndProcessTransferRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU);
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true,
        new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU)));
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.SOURCE_ITEM_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessTransferRequestDestinationItemDeletedTest() {
    workOrderDataModel.setDestinationItemSku(DESTINATION_ITEM_SKU);
    destinationItemBasicDetailV2Response.setMarkForDelete(true);
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true,
            new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU))))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response, destinationItemBasicDetailV2Response));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(destinationProductResponse);
    bulkWorkOrderService.validateAndProcessTransferRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU);
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true,
        new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU)));
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(BulkWorkOrderConstants.DESTINATION_ITEM_INVALID.get(true), bulkProcessData.getErrorMessage());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessTransferRequestApplicationRuntimeExceptionTest() {
    workOrderDataModel.setDestinationItemSku(DESTINATION_ITEM_SKU);
    destinationItemBasicDetailV2Response.setMarkForDelete(true);
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true,
            new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU))))
        .thenThrow(ApplicationRuntimeException.class);
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(destinationProductResponse);
    bulkWorkOrderService.validateAndProcessTransferRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU);
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true,
        new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU)));
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }

  @Test
  public void validateAndProcessTransferRequestSuccessTest() {
    workOrderDataModel.setDestinationItemSku(DESTINATION_ITEM_SKU);
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true,
            new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU))))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response, destinationItemBasicDetailV2Response));
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(destinationProductResponse);
    bulkWorkOrderService.validateAndProcessTransferRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Mockito.verify(productAssemblyOutboundService)
        .transferRequest(eq(STORE_ID), eq(bulkProcess.getRequestId()), eq(bulkProcess.getCreatedBy()),
            Mockito.any(TransferRequest.class));
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU);
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true,
        new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU)));
    Assertions.assertEquals(BulkProcessData.STATUS_SUCCESS, bulkProcessData.getStatus());
  }

  @Test
  public void validateAndProcessTransferRequestExceptionTest() {
    workOrderDataModel.setDestinationItemSku(DESTINATION_ITEM_SKU);
    Mockito.when(xProductOutboundService.getItemBasicDetailByItemSku(STORE_ID, true,
            new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU))))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response, destinationItemBasicDetailV2Response));
    Mockito.when(productAssemblyOutboundService.transferRequest(eq(STORE_ID), eq(bulkProcess.getRequestId()),
        eq(bulkProcess.getCreatedBy()), Mockito.any(TransferRequest.class))).thenThrow(RuntimeException.class);
    Mockito.when(xProductOutboundService.getBasicProductInfo(STORE_ID, PRODUCT_SKU))
        .thenReturn(destinationProductResponse);
    bulkWorkOrderService.validateAndProcessTransferRequest(STORE_ID, MERCHANT_TYPE, ITEM_SKU, PRODUCT_SKU,
        WAREHOUSE_CODE, true, bulkProcess, bulkProcessData, workOrderDataModel, productResponse);
    Mockito.verify(productAssemblyOutboundService)
        .transferRequest(eq(STORE_ID), eq(bulkProcess.getRequestId()), eq(bulkProcess.getCreatedBy()),
            Mockito.any(TransferRequest.class));
    Mockito.verify(xProductOutboundService).getBasicProductInfo(STORE_ID, PRODUCT_SKU);
    Mockito.verify(xProductOutboundService).getItemBasicDetailByItemSku(STORE_ID, true,
        new SimpleListStringRequest(Arrays.asList(ITEM_SKU, DESTINATION_ITEM_SKU)));
    Assertions.assertEquals(BulkProcessData.STATUS_FAIL, bulkProcessData.getStatus());
    Assertions.assertEquals(1, bulkProcessData.getInputErrorCount().intValue());
  }
}
