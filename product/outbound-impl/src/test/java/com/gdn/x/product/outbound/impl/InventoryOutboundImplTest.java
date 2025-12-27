package com.gdn.x.product.outbound.impl;

import static org.mockito.ArgumentMatchers.eq;

import java.util.Arrays;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailStockInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.InventoryStockAvailableResponseDTO;
import com.gdn.x.product.outbound.api.feign.InventoryFeign;

public class InventoryOutboundImplTest {

  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "system";
  private static final String PRODUCT_SKU = "productSku";

  private InventoryDetailStockInfoRequestDTO inventoryDetailStockInfoRequestDTO;

  @Mock
  private InventoryFeign inventoryFeign;

  @InjectMocks
  private InventoryOutboundImpl inventoryOutbound;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    inventoryDetailStockInfoRequestDTO =
        new InventoryDetailStockInfoRequestDTO(StringUtils.EMPTY, Arrays.asList(PRODUCT_SKU));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(inventoryFeign);
  }

  @Test
  public void isProductInStock_getStockInfoByProductSkuListTest() {
    InventoryStockInfoDTO inventoryStockInfoDTO = new InventoryStockInfoDTO();
    inventoryStockInfoDTO.setTotalStock(1);
    Mockito.when(
            this.inventoryFeign.findDetailByWebProductSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
                eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME),
                eq(inventoryDetailStockInfoRequestDTO)))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(inventoryStockInfoDTO), null, REQUEST_ID));
    Assertions.assertTrue(inventoryOutbound.isProductInStock(PRODUCT_SKU, false));
    Mockito.verify(this.inventoryFeign)
        .findDetailByWebProductSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME),
            eq(inventoryDetailStockInfoRequestDTO));
  }

  @Test
  public void isProductInStock_getStockInfoByProductSkuListOOsTest() {
    InventoryStockInfoDTO inventoryStockInfoDTO = new InventoryStockInfoDTO();
    inventoryStockInfoDTO.setTotalStock(0);
    Mockito.when(
            this.inventoryFeign.findDetailByWebProductSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
                eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME),
                eq(inventoryDetailStockInfoRequestDTO)))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(inventoryStockInfoDTO), null, REQUEST_ID));
    Assertions.assertFalse(inventoryOutbound.isProductInStock(PRODUCT_SKU, false));
    Mockito.verify(this.inventoryFeign)
        .findDetailByWebProductSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME),
            eq(inventoryDetailStockInfoRequestDTO));
  }

  @Test
  public void isProductInStock_getStockInfoByProductSkuList_successFalseTest() {
    Mockito.when(
            this.inventoryFeign.findDetailByWebProductSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
                eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME),
                eq(inventoryDetailStockInfoRequestDTO)))
        .thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> inventoryOutbound.isProductInStock(PRODUCT_SKU, false));
    } finally {
      Mockito.verify(this.inventoryFeign)
          .findDetailByWebProductSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
              eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME),
              eq(inventoryDetailStockInfoRequestDTO));
    }
  }

  @Test
  public void isProductInStock_getStockInfoByProductSkuList_responseNullTest() {
    Mockito.when(
            this.inventoryFeign.findDetailByWebProductSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
                eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME),
                eq(inventoryDetailStockInfoRequestDTO)))
        .thenReturn(new GdnRestListResponse<>(null, null, true, null, null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> inventoryOutbound.isProductInStock(PRODUCT_SKU, false));
    } finally {
      Mockito.verify(this.inventoryFeign)
          .findDetailByWebProductSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
              eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME),
              eq(inventoryDetailStockInfoRequestDTO));
    }
  }

  @Test
  public void deleteByItemSkuAndPickupPointCode() {
    Mockito.when(this.inventoryFeign.deleteByItemSkuAndPickupPointCode(eq(Constants.DEFAULT_STORE_ID),
        eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME),
        Mockito.any())).thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    inventoryOutbound.deleteByItemSkuAndPickupPointCode(REQUEST_ID, USERNAME,
        Arrays.asList(new WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO()));
    Mockito.verify(this.inventoryFeign).deleteByItemSkuAndPickupPointCode(eq(Constants.DEFAULT_STORE_ID),
        eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID),
        eq(USERNAME), Mockito.any());
  }

  @Test
  public void deleteByItemSkuAndPickupPointCode_successFalseTest() {
    Mockito.when(this.inventoryFeign.deleteByItemSkuAndPickupPointCode(eq(Constants.DEFAULT_STORE_ID),
        eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME),
        Mockito.any())).thenReturn(new GdnBaseRestResponse(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  inventoryOutbound.deleteByItemSkuAndPickupPointCode(REQUEST_ID, USERNAME,
          Arrays.asList(new WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO())));
    } finally {
      Mockito.verify(this.inventoryFeign).deleteByItemSkuAndPickupPointCode(eq(Constants.DEFAULT_STORE_ID),
          eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID),
          eq(USERNAME), Mockito.any());
    }
  }

  @Test
  public void isProductInStock_isProductOosOrNotSuccessTest() {
    ReflectionTestUtils.setField(inventoryOutbound, "callNewInventoryApiForStockAvailability", true);
    GdnRestSingleResponse<InventoryStockAvailableResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(new InventoryStockAvailableResponseDTO(true));
    Mockito.when(this.inventoryFeign.checkStockAvailable(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME), eq(PRODUCT_SKU), eq(false))).thenReturn(response);
    inventoryOutbound.isProductInStock(PRODUCT_SKU, false);
    Mockito.verify(this.inventoryFeign).checkStockAvailable(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME), eq(PRODUCT_SKU), eq(false));
  }

  @Test
  public void isProductInStock_isProductOosOrNotSuccessTest2() {
    ReflectionTestUtils.setField(inventoryOutbound, "callNewInventoryApiForStockAvailability", true);
    GdnRestSingleResponse<InventoryStockAvailableResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(null);
    Mockito.when(this.inventoryFeign.checkStockAvailable(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME), eq(PRODUCT_SKU), eq(false))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  inventoryOutbound.isProductInStock(PRODUCT_SKU, false));
    } finally {
      Mockito.verify(this.inventoryFeign)
          .checkStockAvailable(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
              eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME), eq(PRODUCT_SKU), eq(false));
    }
  }

  @Test
  public void isProductInStock_isProductOosOrNotExceptionTest() {
    ReflectionTestUtils.setField(inventoryOutbound, "callNewInventoryApiForStockAvailability", true);
    GdnRestSingleResponse<InventoryStockAvailableResponseDTO> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    response.setValue(null);
    Mockito.when(this.inventoryFeign.checkStockAvailable(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME), eq(PRODUCT_SKU), eq(false))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> inventoryOutbound.isProductInStock(PRODUCT_SKU, false));
    } finally {
      Mockito.verify(this.inventoryFeign)
          .checkStockAvailable(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
              eq(Constants.DEFAULT_CLIENT_ID_X_PRODUCT), eq(REQUEST_ID), eq(USERNAME), eq(PRODUCT_SKU), eq(false));
    }
  }

}