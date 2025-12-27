package com.gdn.partners.pbp.outbound.inventory;

import java.util.ArrayList;
import java.util.List;

import com.gda.mta.product.dto.response.L2StockAvailabilityDTO;
import com.gdn.mta.product.entity.L2StockDetailResponse;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.product.valueobject.InventoryStockInfoDTO;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.outbound.inventory.feign.InventoryFeign;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailStockInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryBulkUpdatePickupPointCodeRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointRequestDTO;
import com.gdn.partners.pbp.dao.UpdatePoDateByL3RequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryUpdatePickupPointResponseDTO;

public class InventoryOutboundBeanTest {

  @InjectMocks
  private InventoryOutboundBean inventoryOutboundBean;

  @Mock
  private InventoryFeign inventoryFeign;

  private static final String STORE_ID = "store-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String CLIENT_ID = "client-id";
  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final String WEB_MERCHANT_CODE = "webMerchantCode";
  private static final String WEB_ITEM_SKU = "webItemSku";
  private static final String WAREHOUSE_ITEM_SKU = "warehouseItemSku";
  private static final int STOCK = 10;
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String OFFLINE_INVENTORY_ID = "offlineInventoryId";

  private static final String ERROR_MSG = "error_msg";
  private static final String ERROR_CODE = "error_code";

  private MandatoryRequestParam mandatoryRequestParam;
  private InventoryDetailStockInfoRequestDTO inventoryDetailStockInfoRequestDTO;
  private List<WebInventoryBulkUpdatePickupPointCodeRequest> updateRequestList;
  private ListRequestDTO<InventoryDetailInfoRequestDTO> listRequestDTO;
  private ListRequestDTO<WebInventoryUpdatePickupPointRequestDTO> webInventoryUpdatePickupPointRequestDTOListRequestDTO;
  private L2StockDetailResponse l2StockDetailResponse;
  private UpdatePoDateByL3RequestDTO updatePoDateByL3RequestDTO;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID);
    mandatoryRequestParam.setUsername(USERNAME);

    updateRequestList = new ArrayList<>();
    inventoryDetailStockInfoRequestDTO = new InventoryDetailStockInfoRequestDTO();
    webInventoryUpdatePickupPointRequestDTOListRequestDTO = new ListRequestDTO<>();
    l2StockDetailResponse = new L2StockDetailResponse();
    updatePoDateByL3RequestDTO = new UpdatePoDateByL3RequestDTO();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(inventoryFeign);
  }


  @Test
  public void findDetailByWebProductSkus_valid_success() throws Exception {
    GdnRestListResponse<InventoryStockInfoDTO> response = new GdnRestListResponse<>();
    response.setSuccess(true);
    Mockito.when(inventoryFeign.findDetailByWebProductSkus(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), inventoryDetailStockInfoRequestDTO)).thenReturn(response);
    inventoryOutboundBean.findDetailByWebProductSkus(mandatoryRequestParam, inventoryDetailStockInfoRequestDTO);
    Mockito.verify(inventoryFeign).findDetailByWebProductSkus(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), inventoryDetailStockInfoRequestDTO);
  }

  @Test
  public void findDetailByWebProductSkus_successFalse_error() throws Exception {
    GdnRestListResponse<InventoryStockInfoDTO> response = new GdnRestListResponse<>();
    response.setSuccess(false);
    Mockito.when(inventoryFeign.findDetailByWebProductSkus(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), inventoryDetailStockInfoRequestDTO)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        inventoryOutboundBean.findDetailByWebProductSkus(mandatoryRequestParam, inventoryDetailStockInfoRequestDTO);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(inventoryFeign).findDetailByWebProductSkus(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), inventoryDetailStockInfoRequestDTO);
  }

  @Test
  public void findDetailByWebMerchantCodeAndWebItemSku_valid_success() throws Exception {
    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> response = new GdnRestListResponse<>();
    response.setSuccess(true);
    Mockito.when(inventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), listRequestDTO)).thenReturn(response);
    inventoryOutboundBean.findDetailByWebMerchantCodeAndWebItemSku(mandatoryRequestParam, listRequestDTO);
    Mockito.verify(inventoryFeign).findDetailByWebMerchantCodeAndWebItemSku(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), listRequestDTO);
  }

  @Test
  public void findDetailByWebMerchantCodeAndWebItemSku_successFalse_error() throws Exception {
    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> response = new GdnRestListResponse<>();
    response.setSuccess(false);
    Mockito.when(inventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), listRequestDTO)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        inventoryOutboundBean.findDetailByWebMerchantCodeAndWebItemSku(mandatoryRequestParam, listRequestDTO);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(inventoryFeign).findDetailByWebMerchantCodeAndWebItemSku(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), listRequestDTO);
  }

  @Test
  public void updatePickupPoint_valid_success() throws Exception {
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(inventoryFeign.updatePickupPoint(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), webInventoryUpdatePickupPointRequestDTOListRequestDTO)).thenReturn(response);
    inventoryOutboundBean.updatePickupPoint(mandatoryRequestParam, webInventoryUpdatePickupPointRequestDTOListRequestDTO);
    Mockito.verify(inventoryFeign).updatePickupPoint(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), webInventoryUpdatePickupPointRequestDTOListRequestDTO);
  }

  @Test
  public void updatePickupPoint_successFalse_error() throws Exception {
    GdnRestSingleResponse<WebInventoryUpdatePickupPointResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    Mockito.when(inventoryFeign.updatePickupPoint(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), webInventoryUpdatePickupPointRequestDTOListRequestDTO)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        inventoryOutboundBean.updatePickupPoint(mandatoryRequestParam, webInventoryUpdatePickupPointRequestDTOListRequestDTO);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(inventoryFeign).updatePickupPoint(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), webInventoryUpdatePickupPointRequestDTOListRequestDTO);
  }

  @Test
  public void updatePoDateByL3_valid_success() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    Mockito.when(inventoryFeign.updatePoDateByL3(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(),
        updatePoDateByL3RequestDTO)).thenReturn(response);
    inventoryOutboundBean.updatePoDateByL3(mandatoryRequestParam, updatePoDateByL3RequestDTO);
    Mockito.verify(inventoryFeign).updatePoDateByL3(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(),
        updatePoDateByL3RequestDTO);
  }

  @Test
  public void updatePoDateByL3_successFalse_error() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(false);
    response.setErrorCode("ERR001");
    response.setErrorMessage("Update failed");
    Mockito.when(inventoryFeign.updatePoDateByL3(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(),
        updatePoDateByL3RequestDTO)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        inventoryOutboundBean.updatePoDateByL3(mandatoryRequestParam, updatePoDateByL3RequestDTO);
      });
    } finally {
      Mockito.verify(inventoryFeign).updatePoDateByL3(mandatoryRequestParam.getStoreId(),
          mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
          mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(),
          updatePoDateByL3RequestDTO);
    }
  }

  @Test
  public void testFindStockAvailabilityByWarehouseItemSku_Success() {
    L2StockAvailabilityDTO expectedStockAvailability = new L2StockAvailabilityDTO();
    expectedStockAvailability.setWarehouseItemSku(WEB_ITEM_SKU);
    expectedStockAvailability.setDistributionWarehouseAvailable(true);
    GdnRestSingleResponse<L2StockAvailabilityDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(expectedStockAvailability);
    Mockito.when(inventoryFeign.findStockAvailabilityByWarehouseItemSku(
            mandatoryRequestParam.getStoreId(), mandatoryRequestParam.getChannelId(),
            mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
            mandatoryRequestParam.getUsername(), WEB_MERCHANT_CODE, WEB_ITEM_SKU))
        .thenReturn(response);
    L2StockAvailabilityDTO actualStockAvailability = inventoryOutboundBean.findStockAvailabilityByWarehouseItemSku(
        mandatoryRequestParam, WEB_MERCHANT_CODE, WEB_ITEM_SKU);
    Assertions.assertNotNull(actualStockAvailability);
    Assertions.assertEquals(expectedStockAvailability, actualStockAvailability);
    Mockito.verify(inventoryFeign).findStockAvailabilityByWarehouseItemSku(
        mandatoryRequestParam.getStoreId(), mandatoryRequestParam.getChannelId(),
        mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), WEB_MERCHANT_CODE, WEB_ITEM_SKU);
  }
  @Test
  public void testFindStockAvailabilityByWarehouseItemSku_Failure() {
    GdnRestSingleResponse<L2StockAvailabilityDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    response.setErrorCode("ERR001");
    response.setErrorMessage("Stock not found");
    Mockito.when(inventoryFeign.findStockAvailabilityByWarehouseItemSku(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), WEB_MERCHANT_CODE, WEB_ITEM_SKU)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        inventoryOutboundBean.findStockAvailabilityByWarehouseItemSku(mandatoryRequestParam, WEB_MERCHANT_CODE,
            WEB_ITEM_SKU);
      });
    } finally {
      Mockito.verify(inventoryFeign).findStockAvailabilityByWarehouseItemSku(mandatoryRequestParam.getStoreId(),
          mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
          mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), WEB_MERCHANT_CODE, WEB_ITEM_SKU);
    }
  }

  @Test
  public void isWarehouseStockAvailabilityTest_distributionWarehouseTrue() throws Exception {
    l2StockDetailResponse.setDistributionWarehouseAvailable(true);
    GdnRestSingleResponse<L2StockDetailResponse> response = new GdnRestSingleResponse<>();
    response.setValue(l2StockDetailResponse);
    response.setSuccess(true);
    Mockito.when(
        inventoryFeign.getStockDetailsByWarehouseItemSku(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.eq(WAREHOUSE_ITEM_SKU))).thenReturn(response);
    inventoryOutboundBean.isWarehouseStockPresent(mandatoryRequestParam, WAREHOUSE_ITEM_SKU);
    Mockito.verify(inventoryFeign).getStockDetailsByWarehouseItemSku(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.eq(WAREHOUSE_ITEM_SKU));
  }

  @Test
  public void isWarehouseStockAvailabilityTest_nonDistributionWarehouseTrue() throws Exception {
    l2StockDetailResponse.setNonDistributionWarehouseAvailable(true);
    GdnRestSingleResponse<L2StockDetailResponse> response = new GdnRestSingleResponse<>();
    response.setValue(l2StockDetailResponse);
    response.setSuccess(true);
    Mockito.when(
        inventoryFeign.getStockDetailsByWarehouseItemSku(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.eq(WAREHOUSE_ITEM_SKU))).thenReturn(response);
    inventoryOutboundBean.isWarehouseStockPresent(mandatoryRequestParam, WAREHOUSE_ITEM_SKU);
    Mockito.verify(inventoryFeign).getStockDetailsByWarehouseItemSku(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.eq(WAREHOUSE_ITEM_SKU));
  }

  @Test
  public void isWarehouseStockAvailabilityTest_nonDistributionWarehouseFalse() throws Exception {
    l2StockDetailResponse.setNonDistributionWarehouseAvailable(false);
    GdnRestSingleResponse<L2StockDetailResponse> response = new GdnRestSingleResponse<>();
    response.setValue(l2StockDetailResponse);
    response.setSuccess(true);
    Mockito.when(
        inventoryFeign.getStockDetailsByWarehouseItemSku(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.eq(WAREHOUSE_ITEM_SKU))).thenReturn(response);
    boolean warehouseStockPresent =
        inventoryOutboundBean.isWarehouseStockPresent(mandatoryRequestParam, WAREHOUSE_ITEM_SKU);
    Mockito.verify(inventoryFeign).getStockDetailsByWarehouseItemSku(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.eq(WAREHOUSE_ITEM_SKU));
    Assertions.assertFalse(warehouseStockPresent);
  }

  @Test
  public void isWarehouseStockPresentTest_successFalse() throws Exception {
    l2StockDetailResponse.setNonDistributionWarehouseAvailable(true);
    GdnRestSingleResponse<L2StockDetailResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    Mockito.when(
        inventoryFeign.getStockDetailsByWarehouseItemSku(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.eq(WAREHOUSE_ITEM_SKU))).thenReturn(response);
    try {
      inventoryOutboundBean.isWarehouseStockPresent(mandatoryRequestParam,
          WAREHOUSE_ITEM_SKU);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertNotNull(e);
    } finally {
      Mockito.verify(inventoryFeign)
          .getStockDetailsByWarehouseItemSku(Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.eq(WAREHOUSE_ITEM_SKU));
    }
  }

  @Test
  public void isWarehouseStockPresentTest_nullResponse() throws Exception {
    l2StockDetailResponse.setNonDistributionWarehouseAvailable(true);
    GdnRestSingleResponse<L2StockDetailResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(null);
    Mockito.when(
        inventoryFeign.getStockDetailsByWarehouseItemSku(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.eq(WAREHOUSE_ITEM_SKU))).thenReturn(response);
    try {
      inventoryOutboundBean.isWarehouseStockPresent(mandatoryRequestParam,
          WAREHOUSE_ITEM_SKU);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertNotNull(e);
    } finally {
      Mockito.verify(inventoryFeign)
          .getStockDetailsByWarehouseItemSku(Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.eq(WAREHOUSE_ITEM_SKU));
    }
  }
}