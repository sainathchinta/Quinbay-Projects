package com.gdn.partners.pbp.repository.productlevel3;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.slf4j.MDC;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.dao.WebInventoryUpdateStockRequestV2DTO;
import com.gdn.partners.pbp.outbound.inventory.feign.InventoryFeign;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryBaseRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebItemSkuRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebMerchantCodeRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryInsertRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdateMinimumStockAlertRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointCodeRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdateStockRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryCountWebItemSkuConditionResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryCountWebItemSkuOOSResponseDTO;

public class ProductLevel3InventoryRepositoryBeanTest {

  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String AUTHENTICATOR = "authenticator";

  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CONDITION_TYPE = "conditionType";
  private static final int STOCK = 1;

  @InjectMocks
  private ProductLevel3InventoryRepositoryBean productLevel3InventoryRepositoryBean;

  @Mock
  private InventoryFeign inventoryFeign;

  private MandatoryRequestParam mandatoryRequestParam;
  private GdnBaseRestResponse gdnBaseRestResponse;
  private InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO;
  private List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOs;
  private List<InventoryDetailInfoResponseDTO> inventoryDetailInfoResponseDTOs;
  private GdnRestListResponse<InventoryDetailInfoResponseDTO> gdnRestListInventoryDetailInfoResponseDTO;
  private WebInventoryUpdateMinimumStockAlertRequestDTO webInventoryUpdateMinimumStockAlertRequestDTO;
  private WebInventoryCountWebItemSkuOOSResponseDTO webInventoryCountWebItemSkuOOSResponseDTO;
  private GdnRestSingleResponse<WebInventoryCountWebItemSkuOOSResponseDTO>
      gdnRestSingleWebInventoryCountWebItemSkuOOSResponseDTO;
  private UpdateSyncStockByWebMerchantCodeRequestDTO updateSyncStockByWebMerchantCodeRequestDTO;
  private UpdateSyncStockByWebItemSkuRequestDTO updateSyncStockByWebItemSkuRequestDTO;
  private WebInventoryCountWebItemSkuConditionResponseDTO webInventoryCountWebItemSkuConditionResponseDTO;
  private GdnRestSingleResponse<WebInventoryCountWebItemSkuConditionResponseDTO>
      gdnRestSingleWebInventoryCountWebItemSkuConditionResponseDTO;
  private WebInventoryUpdatePickupPointCodeRequestDTO webInventoryUpdatePickupPointCodeRequestDTO;
  private WebInventoryUpdateStockRequestV2DTO webInventoryUpdateStockRequestDTO;
  private WebInventoryInsertRequestDTO webInventoryInsertRequestDTO;
  private List<WebInventoryInsertRequestDTO> webInventoryInsertRequestDTOs;

  @BeforeEach
  public void __initialize() throws Exception {
    initMocks(this);

    this.mandatoryRequestParam = MandatoryRequestParam.generateMandatoryRequestParam(ProductLevel3InventoryRepositoryBeanTest.STORE_ID,
        ProductLevel3InventoryRepositoryBeanTest.CHANNEL_ID, ProductLevel3InventoryRepositoryBeanTest.CLIENT_ID,
        ProductLevel3InventoryRepositoryBeanTest.REQUEST_ID, ProductLevel3InventoryRepositoryBeanTest.USERNAME,
        ProductLevel3InventoryRepositoryBeanTest.AUTHENTICATOR);

    this.gdnBaseRestResponse = new GdnBaseRestResponse(true);
    this.inventoryDetailInfoRequestDTOs = new ArrayList<>();
    this.inventoryDetailInfoRequestDTOs.add(this.inventoryDetailInfoRequestDTO);
    this.inventoryDetailInfoRequestDTO = new InventoryDetailInfoRequestDTO();
    this.inventoryDetailInfoResponseDTOs = new ArrayList<>();
    this.gdnRestListInventoryDetailInfoResponseDTO =
        new GdnRestListResponse<>(this.inventoryDetailInfoResponseDTOs, new PageMetaData(0, 0, 0),
            ProductLevel3InventoryRepositoryBeanTest.REQUEST_ID);
    this.webInventoryUpdateMinimumStockAlertRequestDTO = new WebInventoryUpdateMinimumStockAlertRequestDTO();
    this.webInventoryCountWebItemSkuOOSResponseDTO = new WebInventoryCountWebItemSkuOOSResponseDTO(1L);
    this.gdnRestSingleWebInventoryCountWebItemSkuOOSResponseDTO =
        new GdnRestSingleResponse<WebInventoryCountWebItemSkuOOSResponseDTO>(
            this.webInventoryCountWebItemSkuOOSResponseDTO, ProductLevel3InventoryRepositoryBeanTest.REQUEST_ID);
    this.updateSyncStockByWebMerchantCodeRequestDTO = new UpdateSyncStockByWebMerchantCodeRequestDTO();
    this.webInventoryCountWebItemSkuConditionResponseDTO = new WebInventoryCountWebItemSkuConditionResponseDTO();
    this.gdnRestSingleWebInventoryCountWebItemSkuConditionResponseDTO =
        new GdnRestSingleResponse<WebInventoryCountWebItemSkuConditionResponseDTO>(
            this.webInventoryCountWebItemSkuConditionResponseDTO, ProductLevel3InventoryRepositoryBeanTest.REQUEST_ID);
    this.updateSyncStockByWebItemSkuRequestDTO = new UpdateSyncStockByWebItemSkuRequestDTO();
    this.webInventoryUpdatePickupPointCodeRequestDTO = new WebInventoryUpdatePickupPointCodeRequestDTO();
    this.webInventoryUpdateStockRequestDTO = new WebInventoryUpdateStockRequestV2DTO();
    this.webInventoryInsertRequestDTO = new WebInventoryInsertRequestDTO();
    this.webInventoryInsertRequestDTOs = new ArrayList<>();
    this.webInventoryInsertRequestDTOs.add(this.webInventoryInsertRequestDTO);


    when(this.inventoryFeign.filterWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), ProductLevel3InventoryRepositoryBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryRepositoryBeanTest.STOCK, ProductLevel3InventoryRepositoryBeanTest.STOCK)).thenReturn(
        this.gdnRestListInventoryDetailInfoResponseDTO);
    when(this.inventoryFeign.updateMinimumStockAlertOfWebInventory(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), this.webInventoryUpdateMinimumStockAlertRequestDTO)).thenReturn(
        this.gdnBaseRestResponse);
    when(this.inventoryFeign.updateSyncStockOfWebInventoryByWebMerchantCode(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), this.updateSyncStockByWebMerchantCodeRequestDTO)).thenReturn(
        this.gdnBaseRestResponse);
    when(this.inventoryFeign.updateSyncStockOfWebInventoryByWebItemSku(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), this.updateSyncStockByWebItemSkuRequestDTO)).thenReturn(
        this.gdnBaseRestResponse);
    when(this.inventoryFeign.updatePickupPointCodeOfWebInventory(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), this.webInventoryUpdatePickupPointCodeRequestDTO)).thenReturn(
        this.gdnBaseRestResponse);
    when(this.inventoryFeign.decreaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), false, this.webInventoryUpdateStockRequestDTO)).thenReturn(
        this.gdnBaseRestResponse);
    when(this.inventoryFeign.increaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), false, this.webInventoryUpdateStockRequestDTO)).thenReturn(
        this.gdnBaseRestResponse);
    when(this.inventoryFeign.insertWebInventoryByList(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), new ListRequestDTO<>(webInventoryInsertRequestDTOs))).thenReturn(
        this.gdnBaseRestResponse);
  }

  @AfterEach
  public void _finalize() {
    verifyNoMoreInteractions(this.inventoryFeign);
  }

  @Test
  public void testFindDetailByBusinessPartnerCodeAndGdnSku_Exception() throws Exception {
    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> response = new GdnRestListResponse<>();
    response.setSuccess(false);
    Mockito.when(inventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(mandatoryRequestParam.getStoreId(),
            mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
            mandatoryRequestParam.getUsername(), new ListRequestDTO<>(inventoryDetailInfoRequestDTOs)))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryRepositoryBean.findDetailByBusinessPartnerCodeAndGdnSku(this.mandatoryRequestParam,
            this.inventoryDetailInfoRequestDTOs);
      });
    } catch (ApplicationRuntimeException e) {
      throw e;
    }
    verify(this.inventoryFeign).findDetailByWebMerchantCodeAndWebItemSku(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(),
        new ListRequestDTO<>(inventoryDetailInfoRequestDTOs));
  }

  @Test
  public void testFindDetailByBusinessPartnerCodeAndGdnSku_Success() throws Exception {
    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> response = new GdnRestListResponse<>();
    response.setSuccess(true);
    Mockito.when(inventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(mandatoryRequestParam.getStoreId(),
            mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
            mandatoryRequestParam.getUsername(), new ListRequestDTO<>(inventoryDetailInfoRequestDTOs)))
        .thenReturn(response);
    this.productLevel3InventoryRepositoryBean.findDetailByBusinessPartnerCodeAndGdnSku(this.mandatoryRequestParam,
        this.inventoryDetailInfoRequestDTOs);
    verify(this.inventoryFeign).findDetailByWebMerchantCodeAndWebItemSku(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), new ListRequestDTO<>(inventoryDetailInfoRequestDTOs));
  }

  @Test
  public void testFindDetailByBusinessPartnerCodeAndStock_Exception() throws Exception {
    this.gdnRestListInventoryDetailInfoResponseDTO.setSuccess(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryRepositoryBean.findDetailByBusinessPartnerCodeAndStock(this.mandatoryRequestParam,
            ProductLevel3InventoryRepositoryBeanTest.BUSINESS_PARTNER_CODE,
            ProductLevel3InventoryRepositoryBeanTest.STOCK);
      });
    } catch (ApplicationRuntimeException e) {
      throw e;
    } verify(this.inventoryFeign).filterWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(),
        ProductLevel3InventoryRepositoryBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryRepositoryBeanTest.STOCK, ProductLevel3InventoryRepositoryBeanTest.STOCK);
  }

  @Test
  public void testFindDetailByBusinessPartnerCodeAndStock_Success() throws Exception {
    this.productLevel3InventoryRepositoryBean.findDetailByBusinessPartnerCodeAndStock(this.mandatoryRequestParam,
        ProductLevel3InventoryRepositoryBeanTest.BUSINESS_PARTNER_CODE, ProductLevel3InventoryRepositoryBeanTest.STOCK);
    verify(this.inventoryFeign).filterWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), ProductLevel3InventoryRepositoryBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryRepositoryBeanTest.STOCK, ProductLevel3InventoryRepositoryBeanTest.STOCK);
  }

  @Test
  public void testInsertInventory_Exception() throws Exception {
    this.gdnBaseRestResponse.setSuccess(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryRepositoryBean.insertInventory(this.mandatoryRequestParam,
            this.webInventoryInsertRequestDTOs);
      });
    } catch (ApplicationRuntimeException e) {
      throw e;
    }
    verify(this.inventoryFeign).insertWebInventoryByList(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(),
        new ListRequestDTO<>(webInventoryInsertRequestDTOs));
  }

  @Test
  public void testInsertInventory_Success() throws Exception {
    this.productLevel3InventoryRepositoryBean.insertInventory(this.mandatoryRequestParam,
        this.webInventoryInsertRequestDTOs);
    verify(this.inventoryFeign).insertWebInventoryByList(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), new ListRequestDTO<>(webInventoryInsertRequestDTOs));
  }

  @Test
  public void testInsertInventorySettingTrackingTest() throws Exception {
    List<WebInventoryInsertRequestDTO> request = new ArrayList<>();
    webInventoryInsertRequestDTO.setInventoryBaseRequest(new InventoryBaseRequest());
    request.add(webInventoryInsertRequestDTO);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    this.productLevel3InventoryRepositoryBean.insertInventory(this.mandatoryRequestParam,
        this.webInventoryInsertRequestDTOs);
    verify(this.inventoryFeign).insertWebInventoryByList(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), new ListRequestDTO<>(webInventoryInsertRequestDTOs));
  }

  @Test
  public void testInsertInventorySettingTrackingChannelIdNull() throws Exception {
    List<WebInventoryInsertRequestDTO> request = new ArrayList<>();
    webInventoryInsertRequestDTO.setInventoryBaseRequest(null);
    request.add(webInventoryInsertRequestDTO);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, null);
    this.productLevel3InventoryRepositoryBean.insertInventory(this.mandatoryRequestParam,
        this.webInventoryInsertRequestDTOs);
    verify(this.inventoryFeign).insertWebInventoryByList(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), new ListRequestDTO<>(webInventoryInsertRequestDTOs));
  }

  @Test
  public void testUpdatePickupPoint_Exception() throws Exception {
    this.gdnBaseRestResponse.setSuccess(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryRepositoryBean.updatePickupPoint(this.mandatoryRequestParam,
            this.webInventoryUpdatePickupPointCodeRequestDTO);
      });
    } catch (ApplicationRuntimeException e) {
      throw e;
    }
    verify(this.inventoryFeign).updatePickupPointCodeOfWebInventory(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(),
        this.webInventoryUpdatePickupPointCodeRequestDTO);
  }

  @Test
  public void testUpdatePickupPoint_Success() throws Exception {
    this.productLevel3InventoryRepositoryBean.updatePickupPoint(this.mandatoryRequestParam,
        this.webInventoryUpdatePickupPointCodeRequestDTO);
    verify(this.inventoryFeign).updatePickupPointCodeOfWebInventory(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), this.webInventoryUpdatePickupPointCodeRequestDTO);
  }

  @Test
  public void testUpdateStockDecrease_Exception() throws Exception {
    this.gdnBaseRestResponse.setSuccess(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryRepositoryBean.updateStockDecrease(this.mandatoryRequestParam,
            this.webInventoryUpdateStockRequestDTO);
      });
    } catch (ApplicationRuntimeException e) {
      throw e;
    }
    verify(this.inventoryFeign).decreaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), false,
        this.webInventoryUpdateStockRequestDTO);
  }

  @Test
  public void testUpdateStockDecrease_Success() throws Exception {
    this.productLevel3InventoryRepositoryBean.updateStockDecrease(this.mandatoryRequestParam,
        this.webInventoryUpdateStockRequestDTO);
    verify(this.inventoryFeign).decreaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), false, this.webInventoryUpdateStockRequestDTO);
  }

  @Test
  public void testUpdateStockDecreaseInventoryBaseRequestTest() throws Exception {
    webInventoryUpdateStockRequestDTO.setInventoryBaseRequest(new InventoryBaseRequest());
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    this.productLevel3InventoryRepositoryBean.updateStockDecrease(this.mandatoryRequestParam,
        this.webInventoryUpdateStockRequestDTO);
    verify(this.inventoryFeign).decreaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), false, this.webInventoryUpdateStockRequestDTO);
  }

  @Test
  public void testUpdateStockDecreaseInventoryBaseRequestChannelIdNullTest() throws Exception {
    webInventoryUpdateStockRequestDTO.setInventoryBaseRequest(new InventoryBaseRequest());
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, null);
    this.productLevel3InventoryRepositoryBean.updateStockDecrease(this.mandatoryRequestParam,
        this.webInventoryUpdateStockRequestDTO);
    verify(this.inventoryFeign).decreaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), false, this.webInventoryUpdateStockRequestDTO);
  }

  @Test
  public void testUpdateStockIncrease_Exception() throws Exception {
    this.gdnBaseRestResponse.setSuccess(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryRepositoryBean.updateStockIncrease(this.mandatoryRequestParam,
            this.webInventoryUpdateStockRequestDTO);
      });
    } catch (ApplicationRuntimeException e) {
      throw e;
    }
    verify(this.inventoryFeign).increaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), false,
        this.webInventoryUpdateStockRequestDTO);
  }

  @Test
  public void testUpdateStockIncrease_Success() throws Exception {
    this.productLevel3InventoryRepositoryBean.updateStockIncrease(this.mandatoryRequestParam,
        this.webInventoryUpdateStockRequestDTO);
    verify(this.inventoryFeign).increaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), false, this.webInventoryUpdateStockRequestDTO);
  }

  @Test
  public void testUpdateStockIncreaseInventoryBaseRequestTest() throws Exception {
    webInventoryUpdateStockRequestDTO.setInventoryBaseRequest(new InventoryBaseRequest());
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    this.productLevel3InventoryRepositoryBean.updateStockIncrease(this.mandatoryRequestParam,
        this.webInventoryUpdateStockRequestDTO);
    verify(this.inventoryFeign).increaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), false, this.webInventoryUpdateStockRequestDTO);
  }

  @Test
  public void testUpdateStockIncreaseV2_Exception() throws Exception {
    this.gdnBaseRestResponse.setSuccess(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryRepositoryBean.updateStockIncrease(this.mandatoryRequestParam,
            this.webInventoryUpdateStockRequestDTO);
      });
    } catch (ApplicationRuntimeException e) {
      throw e;
    }
    verify(this.inventoryFeign).increaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), false,
        this.webInventoryUpdateStockRequestDTO);
  }

  @Test
  public void testUpdateStockIncreaseV2_Success() throws Exception {
    this.productLevel3InventoryRepositoryBean.updateStockIncreaseV2(this.mandatoryRequestParam,
        this.webInventoryUpdateStockRequestDTO);
    verify(this.inventoryFeign).increaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), false, this.webInventoryUpdateStockRequestDTO);
  }

  @Test
  public void testUpdateStockIncreaseV2_ValidationError() throws Exception {
    gdnBaseRestResponse.setErrorCode(ApiErrorCode.MAXIMUM_STOCK_LIMIT_EXCEEDED.name());
    this.productLevel3InventoryRepositoryBean.updateStockIncreaseV2(this.mandatoryRequestParam,
        this.webInventoryUpdateStockRequestDTO);
    verify(this.inventoryFeign).increaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), false, this.webInventoryUpdateStockRequestDTO);
  }

  @Test
  public void testUpdateStockIncreaseV2_SuccessFalseError() throws Exception {
    gdnBaseRestResponse.setSuccess(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryRepositoryBean.updateStockIncreaseV2(this.mandatoryRequestParam,
            this.webInventoryUpdateStockRequestDTO);
      });
    } finally {
      verify(this.inventoryFeign).increaseWebInventoryStock(mandatoryRequestParam.getStoreId(),
          mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
          mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(), false,
          this.webInventoryUpdateStockRequestDTO);
    }
  }


  @Test
  public void testUpdateStockMinimumInventory_Exception() throws Exception {
    this.gdnBaseRestResponse.setSuccess(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryRepositoryBean.updateMinimumStockAlert(this.mandatoryRequestParam,
            this.webInventoryUpdateMinimumStockAlertRequestDTO);
      });
    } catch (ApplicationRuntimeException e) {
      throw e;
    }
    verify(this.inventoryFeign).updateMinimumStockAlertOfWebInventory(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(),
        this.webInventoryUpdateMinimumStockAlertRequestDTO);
  }

  @Test
  public void testUpdateStockMinimumInventory_Success() throws Exception {
    this.productLevel3InventoryRepositoryBean.updateMinimumStockAlert(this.mandatoryRequestParam,
        this.webInventoryUpdateMinimumStockAlertRequestDTO);
    verify(this.inventoryFeign).updateMinimumStockAlertOfWebInventory(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), this.webInventoryUpdateMinimumStockAlertRequestDTO);
  }

  @Test
  public void testUpdateSyncStockByBusinessPartnerCode_Exception() throws Exception {
    this.gdnBaseRestResponse.setSuccess(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryRepositoryBean.updateSyncStockByBusinessPartnerCode(this.mandatoryRequestParam,
            this.updateSyncStockByWebMerchantCodeRequestDTO);
      });
    } catch (ApplicationRuntimeException e) {
      throw e;
    }
    verify(this.inventoryFeign).updateSyncStockOfWebInventoryByWebMerchantCode(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(),
        this.updateSyncStockByWebMerchantCodeRequestDTO);
  }

  @Test
  public void testUpdateSyncStockByBusinessPartnerCode_Success() throws Exception {
    this.productLevel3InventoryRepositoryBean.updateSyncStockByBusinessPartnerCode(this.mandatoryRequestParam,
        this.updateSyncStockByWebMerchantCodeRequestDTO);
    verify(this.inventoryFeign).updateSyncStockOfWebInventoryByWebMerchantCode(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), this.updateSyncStockByWebMerchantCodeRequestDTO);
  }

  @Test
  public void testUpdateSyncStockByBusinessPartnerCodeAndGdnSku_Exception() throws Exception {
    this.gdnBaseRestResponse.setSuccess(false);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryRepositoryBean.updateSyncStockByBusinessPartnerCodeAndGdnSku(
            this.mandatoryRequestParam, this.updateSyncStockByWebItemSkuRequestDTO);
      });
    } catch (ApplicationRuntimeException e) {
      throw e;
    }
    verify(this.inventoryFeign).updateSyncStockOfWebInventoryByWebItemSku(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(),
        mandatoryRequestParam.getRequestId(), mandatoryRequestParam.getUsername(),
        this.updateSyncStockByWebItemSkuRequestDTO);
  }

  @Test
  public void testUpdateSyncStockByBusinessPartnerCodeAndGdnSku_Success() throws Exception {
    this.productLevel3InventoryRepositoryBean.updateSyncStockByBusinessPartnerCodeAndGdnSku(this.mandatoryRequestParam,
        this.updateSyncStockByWebItemSkuRequestDTO);
    verify(this.inventoryFeign).updateSyncStockOfWebInventoryByWebItemSku(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), this.updateSyncStockByWebItemSkuRequestDTO);
  }

  @Test
  public void findByBusinessPartnerCodeAndItemSkuAndPickupPointCodeTest() {
    Mockito.when(
            this.inventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
                USERNAME, new ListRequestDTO<>(Collections.singletonList(inventoryDetailInfoRequestDTO))))
        .thenReturn(new GdnRestListResponse<>(null, null, true, new ArrayList<>(), null, REQUEST_ID));
    productLevel3InventoryRepositoryBean.findByBusinessPartnerCodeAndItemSkuAndPickupPointCode(STORE_ID, CHANNEL_ID,
        CLIENT_ID, REQUEST_ID, USERNAME, Collections.singletonList(inventoryDetailInfoRequestDTO));
    Mockito.verify(this.inventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, new ListRequestDTO<>(Collections.singletonList(inventoryDetailInfoRequestDTO)));
  }

  @Test
  public void updateSyncStockAtL5ExceptionTest() {
    Mockito.when(
        this.inventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
            USERNAME, new ListRequestDTO<>(Collections.singletonList(inventoryDetailInfoRequestDTO)))).thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productLevel3InventoryRepositoryBean.findByBusinessPartnerCodeAndItemSkuAndPickupPointCode(STORE_ID, CHANNEL_ID,
            CLIENT_ID, REQUEST_ID, USERNAME, Collections.singletonList(inventoryDetailInfoRequestDTO));
      });
    } finally {
      Mockito.verify(this.inventoryFeign)
          .findDetailByWebMerchantCodeAndWebItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, new ListRequestDTO<>(Collections.singletonList(inventoryDetailInfoRequestDTO)));
    }
  }

  @Test
  public void updateSyncStockAtL5Test() {
    Mockito.when(this.inventoryFeign.updateSyncStockAtL5(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, new ListRequestDTO<>(new ArrayList<>())))
        .thenReturn(new GdnRestListResponse<>(null, null, true, new ArrayList<>(), null, REQUEST_ID));
    productLevel3InventoryRepositoryBean.updateSyncStockAtL5(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, new ArrayList<>());
    Mockito.verify(this.inventoryFeign).updateSyncStockAtL5(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        new ListRequestDTO<>(new ArrayList<>()));
  }

  @Test
  public void findByBusinessPartnerCodeAndItemSkuAndPickupPointCode_successFalseTest() {
    Mockito.when(this.inventoryFeign.updateSyncStockAtL5(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, new ListRequestDTO<>(new ArrayList<>()))).thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productLevel3InventoryRepositoryBean.updateSyncStockAtL5(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            new ArrayList<>());
      });
    } finally {
      Mockito.verify(this.inventoryFeign).updateSyncStockAtL5(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, new ListRequestDTO<>(new ArrayList<>()));
    }
  }

  @Test
  public void deleteByItemSkuAndPickupPointCodeTest() throws Exception {
    this.gdnBaseRestResponse.setSuccess(false);
    Mockito.when(inventoryFeign.deleteByItemSkuAndPickupPointCode(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        new ListRequestDTO<>(new ArrayList<>()))).thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));

    productLevel3InventoryRepositoryBean.deleteByItemSkuAndPickupPointCode(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME, new ArrayList<>());

    Mockito.verify(inventoryFeign)
        .deleteByItemSkuAndPickupPointCode(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            new ListRequestDTO<>(new ArrayList<>()));
  }

  @Test
  public void deleteByItemSkuAndPickupPointCodeErrorTest() throws Exception {
    this.gdnBaseRestResponse.setSuccess(false);
    Mockito.when(inventoryFeign.deleteByItemSkuAndPickupPointCode(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        new ListRequestDTO<>(new ArrayList<>()))).thenReturn(new GdnBaseRestResponse(null, null, false, REQUEST_ID));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productLevel3InventoryRepositoryBean.deleteByItemSkuAndPickupPointCode(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, new ArrayList<>());
      });
    } finally {
      Mockito.verify(inventoryFeign)
          .deleteByItemSkuAndPickupPointCode(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
              new ListRequestDTO<>(new ArrayList<>()));
    }
  }
}
