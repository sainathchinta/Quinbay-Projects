package com.gdn.partners.pbp.service.productlevel3;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.dao.UpdateSyncStockByWebItemPickupPointRequest;
import org.apache.log4j.MDC;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.InventoryUpsertModel;
import com.gda.mta.product.dto.ItemSkuPickupPointSyncStockDto;
import com.gda.mta.product.dto.SyncStockUpdateOrInsertVo;
import com.gda.mta.product.dto.UpdateOrInsertStockVo;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.dao.WebInventoryInsertRequestV2DTO;
import com.gdn.partners.pbp.dao.WebInventoryUpdateStockRequestV2DTO;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.pickuppoint.PickupPointOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.repository.productlevel3.ProductLevel3InventoryRepository;
import com.gdn.partners.pbp.util.ProductLevel3InventoryUtil;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebItemSkuRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebMerchantCodeRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryInsertRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdateMinimumStockAlertRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointCodeRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdateStockRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryCountWebItemSkuConditionResponseDTO;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;

public class ProductLevel3InventoryServiceBeanTest {

  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String AUTHENTICATOR = "authenticator";

  private static final String BUSINESS_PARTNER_CODE = "BusinessPartnerCode";
  private static final Integer STOCK = 1;
  private static final Integer MIN_STOCK = 1;
  private static final Integer DELTA_STOCK = -1;
  private static final Integer DELTA_STOCK_ZERO = 0;
  private static final ProductLevel3InventoryCriteria INVENTORY_CRITERIA =
      ProductLevel3InventoryCriteria.AVAILABLE;
  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_CODE = "itemCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PICKUP_POINT_CODE_2 = "pickupPointCode2";
  private static final boolean SYNC_STOCK = true;
  private static final Long AVAILABLE = 1L;
  private static final Long OOS = 1L;
  private static final Long STOCK_ALERT = 1L;

  @InjectMocks
  private ProductLevel3InventoryServiceBean productLevel3InventoryServiceBean;

  @Mock
  private ProductLevel3InventoryRepository productLevel3InventoryRepository;

  @Mock
  private ProductLevel3Converter productLevel3Converter;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private PickupPointOutbound pickupPointOutbound;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Captor
  private ArgumentCaptor<List<WebInventoryInsertRequestDTO>>
      argumentCaptorWebInventoryInsertRequestDTOList;

  @Captor
  private ArgumentCaptor<List<UpdateSyncStockByWebItemPickupPointRequest>> updateSyncStockByWebItemPickupPointRequestDTOCaptor;

  @Captor
  private ArgumentCaptor<WebInventoryUpdateStockRequestV2DTO> updateStockRequestCaptor;

  private MandatoryRequestParam mandatoryRequestParam;
  private InventoryDetailInfoResponseDTO inventoryDetailInfoResponseDTO;
  private InventoryDetailInfoResponseV2DTO inventoryDetailInfoResponseV2DTO;
  private List<InventoryDetailInfoResponseDTO> inventoryDetailInfoResponseDTOList;
  private List<InventoryDetailInfoResponseV2DTO> inventoryDetailInfoResponseV2DTOList;
  private ProductLevel3Inventory productLevel3Inventory;
  private List<ProductLevel3Inventory> productLevel3InventoryList;
  private List<String> gdnSkus;
  private Map<String, String> gdnSkusMap;
  private WebInventoryCountWebItemSkuConditionResponseDTO webInventoryCountWebItemSkuConditionResponseDTO;
  private WebInventoryInsertRequestDTO webInventoryInsertRequestDTO;
  private List<WebInventoryInsertRequestDTO> webInventoryInsertRequestDTOList;
  private InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO =
    new InventoryDetailInfoRequestDTO();
  private GdnRestListResponse<ItemSkuPickupPointCodeResponse> pickupPointCodeByItemSkus;

  @BeforeEach
  public void __initialize() throws Exception {
    initMocks(this);

    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        ProductLevel3InventoryServiceBeanTest.STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        ProductLevel3InventoryServiceBeanTest.CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        ProductLevel3InventoryServiceBeanTest.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        ProductLevel3InventoryServiceBeanTest.REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        ProductLevel3InventoryServiceBeanTest.USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY,
        ProductLevel3InventoryServiceBeanTest.AUTHENTICATOR);

    this.mandatoryRequestParam = MandatoryRequestParam.generateMandatoryRequestParam(
        ProductLevel3InventoryServiceBeanTest.STORE_ID,
        ProductLevel3InventoryServiceBeanTest.CHANNEL_ID,
        ProductLevel3InventoryServiceBeanTest.CLIENT_ID,
        ProductLevel3InventoryServiceBeanTest.REQUEST_ID,
        ProductLevel3InventoryServiceBeanTest.USERNAME,
        ProductLevel3InventoryServiceBeanTest.AUTHENTICATOR);

    this.inventoryDetailInfoResponseDTO = new InventoryDetailInfoResponseDTO();
    this.inventoryDetailInfoResponseV2DTO = new InventoryDetailInfoResponseV2DTO();
    this.inventoryDetailInfoResponseDTOList = new ArrayList<>();
    this.inventoryDetailInfoResponseV2DTOList = new ArrayList<>();
    this.inventoryDetailInfoResponseDTOList.add(this.inventoryDetailInfoResponseDTO);
    this.inventoryDetailInfoResponseV2DTOList.add(this.inventoryDetailInfoResponseV2DTO);
    this.productLevel3Inventory = new ProductLevel3Inventory();
    this.productLevel3InventoryList = new ArrayList<>();
    this.productLevel3Inventory.setWebItemSku(ITEM_SKU);
    this.productLevel3Inventory.setWebMerchantCode(BUSINESS_PARTNER_CODE);
    this.productLevel3Inventory.setWarehouseItemSku(ITEM_SKU);
    this.productLevel3Inventory.setWarehouseMerchantCode(BUSINESS_PARTNER_CODE);
    this.productLevel3Inventory.setProductSku(PRODUCT_SKU);
    this.productLevel3InventoryList.add(this.productLevel3Inventory);
    this.gdnSkus = new ArrayList<>();
    this.gdnSkusMap = new HashMap<>();
    this.gdnSkusMap.put(ProductLevel3InventoryServiceBeanTest.ITEM_SKU, ProductLevel3InventoryServiceBeanTest.ITEM_SKU);
    this.gdnSkus.add(ProductLevel3InventoryServiceBeanTest.ITEM_SKU);
    this.webInventoryCountWebItemSkuConditionResponseDTO =
        new WebInventoryCountWebItemSkuConditionResponseDTO();
    this.webInventoryCountWebItemSkuConditionResponseDTO
        .setAvailable(ProductLevel3InventoryServiceBeanTest.AVAILABLE);
    this.webInventoryCountWebItemSkuConditionResponseDTO
        .setOos(ProductLevel3InventoryServiceBeanTest.OOS);
    this.webInventoryCountWebItemSkuConditionResponseDTO
        .setStockAlert(ProductLevel3InventoryServiceBeanTest.STOCK_ALERT);
    this.webInventoryInsertRequestDTO = new WebInventoryInsertRequestDTO();
    this.webInventoryInsertRequestDTOList = new ArrayList<>();
    this.webInventoryInsertRequestDTOList.add(this.webInventoryInsertRequestDTO);

    when(this.productLevel3InventoryRepository.findDetailByBusinessPartnerCodeAndGdnSku(
        eq(this.mandatoryRequestParam), anyList()))
            .thenReturn(this.inventoryDetailInfoResponseV2DTOList);
    when(this.productLevel3Converter.convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
        this.inventoryDetailInfoResponseV2DTO)).thenReturn(this.productLevel3Inventory);
    when(this.productLevel3Converter.convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
        this.inventoryDetailInfoResponseDTO)).thenReturn(this.productLevel3Inventory);
    when(xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any())).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(new ItemSkuPickupPointCodeResponse(ITEM_SKU, PICKUP_POINT_CODE)),
            new PageMetaData(), REQUEST_ID));
  }

  @AfterEach
  public void _finalize() {
    verifyNoMoreInteractions(this.productLevel3InventoryRepository);
    verifyNoMoreInteractions(this.productLevel3Converter);
    verifyNoMoreInteractions(this.xProductOutbound);
    verifyNoMoreInteractions(pickupPointOutbound);
  }

  @Test
  public void testFindInventoryByBusinessPartnerCodeAndGdnSku() throws Exception {
    ProductLevel3Inventory result =
        this.productLevel3InventoryServiceBean.findInventoryByBusinessPartnerCodeAndGdnSku(
            ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
            ProductLevel3InventoryServiceBeanTest.ITEM_SKU);
    assertNotNull(result);
    assertThat(result, equalTo(this.productLevel3Inventory));
    verify(this.productLevel3InventoryRepository).findDetailByBusinessPartnerCodeAndGdnSku(
        eq(this.mandatoryRequestParam), anyList());
    verify(this.productLevel3Converter)
        .convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
            this.inventoryDetailInfoResponseV2DTO);
    verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void testFindInventoryByBusinessPartnerCodeAndGdnSku_moreThanOne() throws Exception {
    this.inventoryDetailInfoResponseV2DTOList.add(this.inventoryDetailInfoResponseV2DTO);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryServiceBean.findInventoryByBusinessPartnerCodeAndGdnSku(
            ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
            ProductLevel3InventoryServiceBeanTest.ITEM_SKU);
      });
    } catch (ApplicationRuntimeException e) {
      assertThat(e.getErrorCodes(), equalTo(ErrorCategory.INVALID_STATE));
      assertTrue(e.getErrorMessage().contains("Found inventory more than one record"));
      throw e;
    }
    verify(this.productLevel3InventoryRepository).findDetailByBusinessPartnerCodeAndGdnSku(
        eq(this.mandatoryRequestParam), anyList());
    verify(this.productLevel3Converter, times(2))
        .convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
            this.inventoryDetailInfoResponseV2DTO);
    verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void testFindInventoryByBusinessPartnerCodeAndGdnSku_notFound() throws Exception {
    this.inventoryDetailInfoResponseV2DTOList.clear();
    ProductLevel3Inventory result =
        this.productLevel3InventoryServiceBean.findInventoryByBusinessPartnerCodeAndGdnSku(
            ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
            ProductLevel3InventoryServiceBeanTest.ITEM_SKU);
    assertNull(result);
    verify(this.productLevel3InventoryRepository).findDetailByBusinessPartnerCodeAndGdnSku(
        eq(this.mandatoryRequestParam), anyList());
    verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void testFindInventoryByBusinessPartnerCodeAndListOfGdnSku() throws Exception {
    List<ProductLevel3Inventory> result =
        this.productLevel3InventoryServiceBean.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE, this.gdnSkus);
    assertNotNull(result);
    assertThat(result, equalTo(this.productLevel3InventoryList));
    verify(this.productLevel3InventoryRepository).findDetailByBusinessPartnerCodeAndGdnSku(
        eq(this.mandatoryRequestParam), anyList());
    verify(this.productLevel3Converter)
        .convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
            this.inventoryDetailInfoResponseV2DTO);
    verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void testFindInventoryByBusinessPartnerCodeAndListOfGdnSkuXproductResponseFalse() throws Exception {
    when(xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any())).thenReturn(
        new GdnRestListResponse<>(null, null , false, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryServiceBean.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE, this.gdnSkus);
      });
    } finally {
      verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    }
  }

  @Test
  public void testFindInventoryByBusinessPartnerCodeAndListOfGdnSkuXproductEmptyContent() throws Exception {
    when(xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any())).thenReturn(
        new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData() , REQUEST_ID));
    List<ProductLevel3Inventory> result = null;
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryServiceBean.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE, this.gdnSkus);
      });
    } finally {
      verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    }
  }

  @Test
  public void testFindInventoryByBusinessPartnerCodeAndListOfGdnSkuXproductTwoRecordsContent() throws Exception {
    when(xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any())).thenReturn(new GdnRestListResponse<>(
        Arrays.asList(new ItemSkuPickupPointCodeResponse(ITEM_SKU, PICKUP_POINT_CODE),
            new ItemSkuPickupPointCodeResponse(ITEM_SKU, PICKUP_POINT_CODE)), new PageMetaData(), REQUEST_ID));
    List<ProductLevel3Inventory> result = null;
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryServiceBean.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE, this.gdnSkus);
      });
    } finally {
      verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    }
  }

  @Test
  public void testFindInventoryByBusinessPartnerCodeAndListOfGdnSkuXproductZeroRecordsContent() throws Exception {
    when(xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any())).thenReturn(new GdnRestListResponse<>(
        Arrays.asList(new ItemSkuPickupPointCodeResponse(PRODUCT_SKU, PICKUP_POINT_CODE)), new PageMetaData(), REQUEST_ID));
    List<ProductLevel3Inventory> result = null;
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryServiceBean.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE, this.gdnSkus);
      });
    } finally {
      verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    }
  }

  @Test
  public void testFindInventoryByBusinessPartnerCodeAndListOfGdnSku_gdnSkusNull() throws Exception {
    this.productLevel3InventoryList.clear();
    List<ProductLevel3Inventory> result =
        this.productLevel3InventoryServiceBean.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE, null);
    assertNotNull(result);
    assertThat(result, equalTo(this.productLevel3InventoryList));
  }

  @Test
  public void testFindInventoryByGdnSkuMap() throws Exception {
    List<ProductLevel3Inventory> result = this.productLevel3InventoryServiceBean.findInventoryByGdnSkuMap(this.gdnSkusMap);
    assertNotNull(result);
    assertThat(result, equalTo(this.productLevel3InventoryList));
    verify(this.productLevel3InventoryRepository).findDetailByBusinessPartnerCodeAndGdnSku(
        eq(this.mandatoryRequestParam), anyList());
    verify(this.productLevel3Converter)
        .convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
            this.inventoryDetailInfoResponseV2DTO);
    verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void testFindInventoryByGdnSkuMapXproductResponseFalse() throws Exception {
    when(xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any())).thenReturn(
        new GdnRestListResponse<>(null, null , false, REQUEST_ID));
    List<ProductLevel3Inventory> result = null;
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryServiceBean.findInventoryByGdnSkuMap(this.gdnSkusMap);
      });
    } finally {
      verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    }
  }

  @Test
  public void testFindInventoryByGdnSkuMapXproductEmptyContent() throws Exception {
    when(xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any())).thenReturn(
        new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(), REQUEST_ID));
    List<ProductLevel3Inventory> result = null;
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryServiceBean.findInventoryByGdnSkuMap(this.gdnSkusMap);
      });
    } finally {
      verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    }
  }

  @Test
  public void testfindInventoryByGdnSkuMapXproductTwoRecordsContent() throws Exception {
    when(xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any())).thenReturn(new GdnRestListResponse<>(
        Arrays.asList(new ItemSkuPickupPointCodeResponse(ITEM_SKU, PICKUP_POINT_CODE),
            new ItemSkuPickupPointCodeResponse(ITEM_SKU, PICKUP_POINT_CODE)), new PageMetaData(), REQUEST_ID));
    List<ProductLevel3Inventory> result = null;
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      this.productLevel3InventoryServiceBean.findInventoryByGdnSkuMap(this.gdnSkusMap);
      }); 
    } finally {
      verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    }
  }

  @Test
  public void testfindInventoryByGdnSkuMapXproductZeroRecordsContent() throws Exception {
    when(xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any())).thenReturn(new GdnRestListResponse<>(
        Arrays.asList(new ItemSkuPickupPointCodeResponse(PRODUCT_SKU, PICKUP_POINT_CODE)), new PageMetaData(), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      this.productLevel3InventoryServiceBean.findInventoryByGdnSkuMap(this.gdnSkusMap);
      }); } finally {
      verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    }
  }

  @Test
  public void testFindInventoryByGdnSkuMap_gdnSkusNull() throws Exception {
    this.productLevel3InventoryList.clear();
    List<ProductLevel3Inventory> result =
        this.productLevel3InventoryServiceBean.findInventoryByGdnSkuMap(null);
    assertNotNull(result);
    assertThat(result, equalTo(this.productLevel3InventoryList));
  }

  @Test
  public void testFindInventoryByInventoryFilter_stockNull_inventoryCriteriaNull()
      throws Exception {
    this.productLevel3InventoryList.clear();
    List<ProductLevel3Inventory> result =
        this.productLevel3InventoryServiceBean.findInventoryByInventoryFilter(
            ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE, null, null);
    assertNotNull(result);
    assertThat(result, equalTo(this.productLevel3InventoryList));
  }

  @Test
  public void testFindInventoryByInventoryFilter_withInventoryCriteria() throws Exception {
    List<ProductLevel3Inventory> result = this.productLevel3InventoryServiceBean
        .findInventoryByInventoryFilter(ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
            null, ProductLevel3InventoryServiceBeanTest.INVENTORY_CRITERIA);
    assertNotNull(result);
  }

  @Test
  public void testFindInventoryByInventoryFilter_withStock() throws Exception {
    when(this.productLevel3InventoryRepository.findDetailByBusinessPartnerCodeAndStock(
        this.mandatoryRequestParam, ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.STOCK))
        .thenReturn(this.inventoryDetailInfoResponseDTOList);
    List<ProductLevel3Inventory> result = this.productLevel3InventoryServiceBean
        .findInventoryByInventoryFilter(ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
            ProductLevel3InventoryServiceBeanTest.STOCK, null);
    assertNotNull(result);
    assertThat(result, equalTo(this.productLevel3InventoryList));
    verify(this.productLevel3InventoryRepository).findDetailByBusinessPartnerCodeAndStock(
        this.mandatoryRequestParam, ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.STOCK);
    verify(this.productLevel3Converter)
        .convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
            this.inventoryDetailInfoResponseDTO);
  }

  @Test
  public void testInsertInventory() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "mppForWhEnabled", false);
    productLevel3InventoryList.get(0).setFbbPP(false);
    productLevel3InventoryList.getFirst().setInitialPreOrderQuota(10);
    this.productLevel3InventoryServiceBean.insertInventory(this.productLevel3InventoryList);
    verify(this.productLevel3InventoryRepository).insertInventory(eq(this.mandatoryRequestParam),
        argumentCaptorWebInventoryInsertRequestDTOList.capture());
    Assertions.assertNotNull(argumentCaptorWebInventoryInsertRequestDTOList.getValue());
    Assertions.assertEquals(ITEM_SKU,
        argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWebItemSku());
    Assertions.assertEquals(ITEM_SKU,
        argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWarehouseItemSku());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0)
            .getWarehouseMerchantCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWebMerchantCode());
    Assertions.assertEquals(PRODUCT_SKU,
        argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWebProductSku());
    Assertions.assertFalse(argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).isFbbPP());

  }

  @Test
  public void testInsertInventoryWithFbbPPFlagFalse() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "mppForWhEnabled", false);
    productLevel3InventoryList.get(0).setFbbPP(true);
    this.productLevel3InventoryServiceBean.insertInventory(this.productLevel3InventoryList);
    verify(this.productLevel3InventoryRepository).insertInventory(eq(this.mandatoryRequestParam),
      argumentCaptorWebInventoryInsertRequestDTOList.capture());
    Assertions.assertNotNull(argumentCaptorWebInventoryInsertRequestDTOList.getValue());
    Assertions.assertEquals(ITEM_SKU,
      argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWebItemSku());
    Assertions.assertEquals(ITEM_SKU,
      argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWarehouseItemSku());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
      argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0)
        .getWarehouseMerchantCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
      argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWebMerchantCode());
    Assertions.assertEquals(PRODUCT_SKU,
      argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWebProductSku());
    Assertions.assertFalse(argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).isFbbPP());

  }
  @Test
  public void testInsertInventoryWithMppForWHEnabled() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "mppForWhEnabled", true);
    productLevel3InventoryList.get(0).setFbbPP(true);
    this.productLevel3InventoryServiceBean.insertInventory(this.productLevel3InventoryList);
    verify(this.productLevel3InventoryRepository).insertInventory(eq(this.mandatoryRequestParam),
      argumentCaptorWebInventoryInsertRequestDTOList.capture());
    Assertions.assertNotNull(argumentCaptorWebInventoryInsertRequestDTOList.getValue());
    Assertions.assertEquals(ITEM_SKU,
      argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWebItemSku());
    Assertions.assertEquals(ITEM_SKU,
      argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWarehouseItemSku());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
      argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0)
        .getWarehouseMerchantCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
      argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWebMerchantCode());
    Assertions.assertEquals(PRODUCT_SKU,
      argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWebProductSku());
    Assertions.assertTrue(argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).isFbbPP());
  }

  @Test
  public void testInsertInventory_paramEmpty() throws Exception {
    this.productLevel3InventoryList.clear();
    this.productLevel3InventoryServiceBean.insertInventory(this.productLevel3InventoryList);
  }

  @Test
  public void testUpdateMinimumStockAlert() throws Exception {
    this.productLevel3InventoryServiceBean.updateMinimumStockAlert(
        ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.ITEM_SKU,
        ProductLevel3InventoryServiceBeanTest.STOCK);
    verify(this.productLevel3InventoryRepository).updateMinimumStockAlert(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateMinimumStockAlertRequestDTO.class));
  }

  @Test
  public void testUpdatePickupPoint() throws Exception {
    this.productLevel3InventoryServiceBean.updatePickupPoint(
        ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.ITEM_SKU,
        ProductLevel3InventoryServiceBeanTest.PICKUP_POINT_CODE);
    verify(this.productLevel3InventoryRepository).updatePickupPoint(eq(this.mandatoryRequestParam),
        any(WebInventoryUpdatePickupPointCodeRequestDTO.class));
  }

  @Test
  public void testUpdateStock_decrease() throws Exception {
    this.productLevel3InventoryServiceBean.updateStock(
        ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.ITEM_SKU, -1);
    verify(this.productLevel3InventoryRepository, times(1)).updateStockDecrease(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
    verify(this.productLevel3InventoryRepository, times(0)).updateStockIncrease(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
    verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void testUpdateStockKeyNotPresent_decrease() throws Exception {
    when(xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any())).thenReturn(new GdnRestListResponse<>(
        Arrays.asList(new ItemSkuPickupPointCodeResponse(PRODUCT_SKU, PICKUP_POINT_CODE)), new PageMetaData(), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryServiceBean.updateStock(ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
            ProductLevel3InventoryServiceBeanTest.ITEM_SKU, -1);
      });
    } finally {
      verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    }
  }

  @Test
  public void testUpdateStockKeyPresentMoreThanOnce_decrease() throws Exception {
    when(xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any())).thenReturn(new GdnRestListResponse<>(
        Arrays.asList(new ItemSkuPickupPointCodeResponse(ProductLevel3InventoryServiceBeanTest.ITEM_SKU, PICKUP_POINT_CODE),
            new ItemSkuPickupPointCodeResponse(ProductLevel3InventoryServiceBeanTest.ITEM_SKU, PICKUP_POINT_CODE)), new PageMetaData(), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryServiceBean.updateStock(ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
            ProductLevel3InventoryServiceBeanTest.ITEM_SKU, -1);
      });
    } finally {
      verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    }
  }

  @Test
  public void testUpdateStock_deltaStockNull() throws Exception {
    this.productLevel3InventoryServiceBean.updateStock(
        ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.ITEM_SKU, null);
    verify(this.productLevel3InventoryRepository, times(0)).updateStockDecrease(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
    verify(this.productLevel3InventoryRepository, times(0)).updateStockIncrease(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
  }

  @Test
  public void testUpdateStock_deltaStockZero() throws Exception {
    this.productLevel3InventoryServiceBean.updateStock(
        ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.ITEM_SKU, 0);
    verify(this.productLevel3InventoryRepository, times(0)).updateStockDecrease(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
    verify(this.productLevel3InventoryRepository, times(0)).updateStockIncrease(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
  }

  @Test
  public void testUpdateStock_increase() throws Exception {
    this.productLevel3InventoryServiceBean.updateStock(
        ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.ITEM_SKU, 1);
    verify(this.productLevel3InventoryRepository, times(0)).updateStockDecrease(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
    verify(this.productLevel3InventoryRepository, times(1)).updateStockIncrease(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
    verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void testUpdateSyncStockByBusinessPartnerCode() throws Exception {
    this.productLevel3InventoryServiceBean.updateSyncStockByBusinessPartnerCode(
        ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.SYNC_STOCK);
    verify(this.productLevel3InventoryRepository).updateSyncStockByBusinessPartnerCode(
        eq(this.mandatoryRequestParam), any(UpdateSyncStockByWebMerchantCodeRequestDTO.class));
  }

  @Test
  public void testUpdateSyncStockByBusinessPartnerCodeAndGdnSku() throws Exception {
    this.productLevel3InventoryServiceBean.updateSyncStockByBusinessPartnerCodeAndGdnSku(
        ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.ITEM_SKU,
        ProductLevel3InventoryServiceBeanTest.SYNC_STOCK, new ArrayList<>());
    verify(this.productLevel3InventoryRepository).updateSyncStockByBusinessPartnerCodeAndGdnSku(
        eq(this.mandatoryRequestParam), any(UpdateSyncStockByWebItemSkuRequestDTO.class));
  }

  @Test
  public void testUpdateSyncStockByBusinessPartnerCodeAndGdnSkuAndPickupPointCode() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "inventoryL5SyncStockEnabled", true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setFbbActivated(true);
    ItemSkuPickupPointSyncStockDto itemSkuPickupPointSyncStockDto = new ItemSkuPickupPointSyncStockDto();
    itemSkuPickupPointSyncStockDto.setItemSku(ITEM_SKU);
    itemSkuPickupPointSyncStockDto.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    this.productLevel3InventoryServiceBean.updateSyncStockByBusinessPartnerCodeAndGdnSku(
        ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE, ProductLevel3InventoryServiceBeanTest.ITEM_SKU,
        ProductLevel3InventoryServiceBeanTest.SYNC_STOCK, Collections.singletonList(itemSkuPickupPointSyncStockDto));
    verify(pickupPointOutbound).getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID,
        Collections.singletonList(PICKUP_POINT_CODE));
    verify(this.productLevel3InventoryRepository).updateSyncStockAtL5(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList());
  }

  @Test
  public void testUpdateSyncStockByBusinessPartnerCodeAndGdnSkuAndPickupPointCodeTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "inventoryL5SyncStockEnabled", true);
    ItemSkuPickupPointSyncStockDto itemSkuPickupPointSyncStockDto = new ItemSkuPickupPointSyncStockDto();
    itemSkuPickupPointSyncStockDto.setItemSku(ITEM_SKU);
    itemSkuPickupPointSyncStockDto.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.EMPTY_LIST);
    this.productLevel3InventoryServiceBean.updateSyncStockByBusinessPartnerCodeAndGdnSku(
        ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE, ProductLevel3InventoryServiceBeanTest.ITEM_SKU,
        ProductLevel3InventoryServiceBeanTest.SYNC_STOCK, Collections.singletonList(itemSkuPickupPointSyncStockDto));
    verify(pickupPointOutbound).getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID,
        Collections.singletonList(PICKUP_POINT_CODE));
  }

  @Test
  public void testUpdateStockV2_decrease() throws Exception {
    this.productLevel3InventoryServiceBean.updateStockV2(ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.ITEM_SKU, -1);
    verify(this.productLevel3InventoryRepository, times(1))
        .updateStockDecrease(eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
    verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void testUpdateStockV2_deltaStockNull() throws Exception {
    this.productLevel3InventoryServiceBean.updateStockV2(
        ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.ITEM_SKU, null);
    verify(this.productLevel3InventoryRepository, times(0)).updateStockDecrease(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
    verify(this.productLevel3InventoryRepository, times(0)).updateStockIncrease(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
  }

  @Test
  public void testUpdateStockV2_deltaStockZero() throws Exception {
    this.productLevel3InventoryServiceBean.updateStockV2(
        ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.ITEM_SKU, 0);
    verify(this.productLevel3InventoryRepository, times(0)).updateStockDecrease(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
    verify(this.productLevel3InventoryRepository, times(0)).updateStockIncrease(
        eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
  }

  @Test
  public void testUpdateStockV2_increase() throws Exception {
    when(productLevel3InventoryRepository
        .updateStockIncreaseV2(eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class)))
        .thenReturn(null);
    this.productLevel3InventoryServiceBean.updateStockV2(ProductLevel3InventoryServiceBeanTest.BUSINESS_PARTNER_CODE,
        ProductLevel3InventoryServiceBeanTest.ITEM_SKU, 1);
    verify(this.productLevel3InventoryRepository, times(1))
        .updateStockIncreaseV2(eq(this.mandatoryRequestParam), any(WebInventoryUpdateStockRequestV2DTO.class));
    verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void updateStockForItemPickupPointWithErrorCodeTest() throws Exception {
    ApiErrorCode response = this.productLevel3InventoryServiceBean.updateStockForItemPickupPointWithErrorCode(BUSINESS_PARTNER_CODE, ITEM_SKU,
        PICKUP_POINT_CODE, DELTA_STOCK);
    verify(this.productLevel3InventoryRepository)
        .updateStockDecrease(Mockito.any(), Mockito.any());
    assertNull(response);
  }

  @Test
  public void updateStockForItemPickupPointWithErrorCodeErrorTest() throws Exception {
    Mockito.when(this.productLevel3InventoryRepository
        .updateStockIncreaseV2(Mockito.any(), Mockito.any())).thenReturn(ApiErrorCode.IMAGE_NOT_PRESENT);
    ApiErrorCode response = this.productLevel3InventoryServiceBean.updateStockForItemPickupPointWithErrorCode(BUSINESS_PARTNER_CODE, ITEM_SKU,
        PICKUP_POINT_CODE, STOCK);
    verify(this.productLevel3InventoryRepository)
        .updateStockIncreaseV2(Mockito.any(), Mockito.any());
    Assertions.assertEquals(ApiErrorCode.IMAGE_NOT_PRESENT.getDesc(), response.getDesc());
  }

  @Test
  public void updateStockForItemPickupPointWithErrorCodeDeltaStockNullTest() throws Exception {
    ApiErrorCode response = this.productLevel3InventoryServiceBean.updateStockForItemPickupPointWithErrorCode(BUSINESS_PARTNER_CODE, ITEM_SKU,
        PICKUP_POINT_CODE, null);
    assertNull(response);
  }

  @Test
  public void updateStockForItemPickupPointWithErrorCodeDeltaStockZeroTest() throws Exception {
    ApiErrorCode response = this.productLevel3InventoryServiceBean.updateStockForItemPickupPointWithErrorCode(BUSINESS_PARTNER_CODE, ITEM_SKU,
        PICKUP_POINT_CODE, DELTA_STOCK_ZERO);
    assertNull(response);
  }

  @Test
  public void findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCodeTest()
    throws Exception {
    Mockito.when(
        this.productLevel3InventoryRepository.findByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
          STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
          Collections.singletonList(inventoryDetailInfoRequestDTO)))
      .thenReturn(Collections.singletonList(inventoryDetailInfoResponseV2DTO));
    this.productLevel3InventoryServiceBean.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
      Collections.singletonList(inventoryDetailInfoRequestDTO));
    Mockito.verify(this.productLevel3InventoryRepository)
      .findByBusinessPartnerCodeAndItemSkuAndPickupPointCode(STORE_ID, CHANNEL_ID, CLIENT_ID,
        REQUEST_ID, USERNAME, Collections.singletonList(inventoryDetailInfoRequestDTO));
    verify(
      this.productLevel3Converter).convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
      this.inventoryDetailInfoResponseV2DTO);
  }

  @Test
  public void updateStockForItemPickupPoint_zeroDeltaStockTest() throws Exception {
    this.productLevel3InventoryServiceBean.updateStockForItemPickupPoint(BUSINESS_PARTNER_CODE,
      ITEM_SKU, PICKUP_POINT_CODE, 0, 0, false);
  }

  @Test
  public void updateStockForItemPickupPoint_nullDeltaStockTest() throws Exception {
    this.productLevel3InventoryServiceBean.updateStockForItemPickupPoint(BUSINESS_PARTNER_CODE,
      ITEM_SKU, PICKUP_POINT_CODE, null, 0, false);
  }

  @Test
  public void deleteByItemSkuAndPickupPointCodeTest() throws Exception {
    this.productLevel3InventoryServiceBean.deleteByItemSkuAndPickupPointCode(new ArrayList<>());
    Mockito.verify(productLevel3InventoryRepository).deleteByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
        mandatoryRequestParam.getChannelId(), mandatoryRequestParam.getClientId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getRequestId(), new ArrayList<>());
  }

  @Test
  public void updateStockForItemPickupPoint() throws Exception {
    Mockito.doNothing().when(this.productLevel3InventoryRepository)
      .updateStockDecrease(eq(mandatoryRequestParam),
        Mockito.any(WebInventoryUpdateStockRequestV2DTO.class));
    this.productLevel3InventoryServiceBean.updateStockForItemPickupPoint(BUSINESS_PARTNER_CODE,
      ITEM_SKU, PICKUP_POINT_CODE, -1, 0, false);
    Mockito.verify(this.productLevel3InventoryRepository)
      .updateStockDecrease(eq(mandatoryRequestParam),
        Mockito.any(WebInventoryUpdateStockRequestV2DTO.class));
  }

  @Test
  public void updateStockForItemPickupPoint_increaseTest() throws Exception {
    Mockito.when(
      this.productLevel3InventoryRepository.updateStockIncreaseV2(eq(mandatoryRequestParam),
        Mockito.any(WebInventoryUpdateStockRequestV2DTO.class))).thenReturn(null);
    this.productLevel3InventoryServiceBean.updateStockForItemPickupPoint(BUSINESS_PARTNER_CODE,
      ITEM_SKU, PICKUP_POINT_CODE, 1, 0, false);
    Mockito.verify(this.productLevel3InventoryRepository)
      .updateStockIncreaseV2(eq(mandatoryRequestParam),
        Mockito.any(WebInventoryUpdateStockRequestV2DTO.class));
  }


  @Test
  public void updateOrInsertStockTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "insertInventoryEnabledOnFailedUpdate", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ApiErrorCode apiErrorCode = this.productLevel3InventoryServiceBean.updateOrInsertStock(
        UpdateOrInsertStockVo.builder().profileResponse(profileResponse).businessPartnerCode(BUSINESS_PARTNER_CODE)
            .itemCode(ITEM_CODE).productSku(PRODUCT_SKU).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .stock(DELTA_STOCK).minimumStock(MIN_STOCK).syncStock(true).fbbActive(true).mppForWhEnabled(true).build());
    Mockito.verify(this.productLevel3InventoryRepository)
        .updateStockDecrease(eq(mandatoryRequestParam), Mockito.any(WebInventoryUpdateStockRequestV2DTO.class));
    assertNull(apiErrorCode);
  }

  @Test
  public void updateOrInsertStock_InsertInventoryEnabledOnFailedUpdateSwitchOff() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "insertInventoryEnabledOnFailedUpdate", false);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.doThrow(new Exception("Can not process invalid input data :webInventory must not be null"))
        .when(this.productLevel3InventoryRepository)
        .updateStockDecrease(eq(mandatoryRequestParam), Mockito.any(WebInventoryUpdateStockRequestV2DTO.class));
    try {
      Assertions.assertThrows(Exception.class, () -> {
        ApiErrorCode apiErrorCode =
            this.productLevel3InventoryServiceBean.updateOrInsertStock(UpdateOrInsertStockVo.builder().profileResponse(profileResponse).businessPartnerCode(BUSINESS_PARTNER_CODE)
                .itemCode(ITEM_CODE).productSku(PRODUCT_SKU).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
                .stock(DELTA_STOCK).minimumStock(MIN_STOCK).syncStock(true).fbbActive(true).mppForWhEnabled(true).build());
      });
    } finally {
      Mockito.verify(this.productLevel3InventoryRepository)

          .updateStockDecrease(eq(mandatoryRequestParam), Mockito.any(WebInventoryUpdateStockRequestV2DTO.class));
    }
  }

  @Test
  public void updateOrInsertStock_InsertInventoryEnabledOnFailedUpdateTrue() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "insertInventoryEnabledOnFailedUpdate", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.doThrow(new Exception("Can not process invalid input data :webInventory must not be null"))
        .when(this.productLevel3InventoryRepository)
        .updateStockDecrease(eq(mandatoryRequestParam), Mockito.any(WebInventoryUpdateStockRequestV2DTO.class));
    ApiErrorCode apiErrorCode =
        this.productLevel3InventoryServiceBean.updateOrInsertStock(UpdateOrInsertStockVo.builder().profileResponse(profileResponse).businessPartnerCode(BUSINESS_PARTNER_CODE)
            .itemCode(ITEM_CODE).productSku(PRODUCT_SKU).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .stock(DELTA_STOCK).minimumStock(MIN_STOCK).syncStock(true).fbbActive(true).mppForWhEnabled(true).build());
    Mockito.verify(this.productLevel3InventoryRepository)
        .updateStockDecrease(eq(mandatoryRequestParam), Mockito.any(WebInventoryUpdateStockRequestV2DTO.class));
    Mockito.verify(productLevel3InventoryRepository).insertInventory(eq(mandatoryRequestParam), Mockito.anyList());
    assertNull(apiErrorCode);
  }

  @Test
  public void updateOrInsertStock_InsertInventoryEnabledOnFailedUpdateFalse() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "insertInventoryEnabledOnFailedUpdate", true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.doThrow(new Exception("ERROR")).when(this.productLevel3InventoryRepository)
        .updateStockDecrease(eq(mandatoryRequestParam), Mockito.any(WebInventoryUpdateStockRequestV2DTO.class));
    try {
      Assertions.assertThrows(Exception.class, () -> {
        this.productLevel3InventoryServiceBean.updateOrInsertStock(UpdateOrInsertStockVo.builder().profileResponse(profileResponse).businessPartnerCode(BUSINESS_PARTNER_CODE)
            .itemCode(ITEM_CODE).productSku(PRODUCT_SKU).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .stock(DELTA_STOCK).minimumStock(MIN_STOCK).syncStock(true).fbbActive(true).mppForWhEnabled(true).build());
      });
    } finally {
      Mockito.verify(this.productLevel3InventoryRepository)
          .updateStockDecrease(eq(mandatoryRequestParam), Mockito.any(WebInventoryUpdateStockRequestV2DTO.class));
    }
  }

  @Test
  public void updateSyncStockOrInsertStockTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "inventoryL5SyncStockEnabled", true);
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "insertInventoryEnabledOnFailedSyncStockUpdate",
        true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    UpdateOrInsertStockVo updateOrInsertStockVo =
        UpdateOrInsertStockVo.builder().profileResponse(profileResponse).businessPartnerCode(BUSINESS_PARTNER_CODE)
            .itemCode(ITEM_CODE).productSku(PRODUCT_SKU).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .stock(DELTA_STOCK).minimumStock(MIN_STOCK).syncStock(true).fbbActive(true).mppForWhEnabled(true).build();
    SyncStockUpdateOrInsertVo syncStockUpdateOrInsertVo =
        SyncStockUpdateOrInsertVo.builder().updateOrInsertStockVo(updateOrInsertStockVo).pickupPointSyncStockDtoList(
            Collections.singletonList(new ItemSkuPickupPointSyncStockDto(ITEM_SKU, PICKUP_POINT_CODE, true))).build();
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.doNothing().when(this.productLevel3InventoryRepository)
        .updateSyncStockAtL5(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
            updateSyncStockByWebItemPickupPointRequestDTOCaptor.capture());
    this.productLevel3InventoryServiceBean.updateSyncStockOrInsertStock(syncStockUpdateOrInsertVo);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(this.productLevel3InventoryRepository)
        .updateSyncStockAtL5(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
            updateSyncStockByWebItemPickupPointRequestDTOCaptor.capture());
    Assertions.assertEquals(ITEM_SKU,
        updateSyncStockByWebItemPickupPointRequestDTOCaptor.getValue().get(0).getWebItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        updateSyncStockByWebItemPickupPointRequestDTOCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertTrue(updateSyncStockByWebItemPickupPointRequestDTOCaptor.getValue().get(0).isSyncStock());
  }

  @Test
  public void updateSyncStockOrInsertStockTest_InsertOnError() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "inventoryL5SyncStockEnabled", true);
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "insertInventoryEnabledOnFailedSyncStockUpdate",
        true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    UpdateOrInsertStockVo updateOrInsertStockVo =
        UpdateOrInsertStockVo.builder().profileResponse(profileResponse).businessPartnerCode(BUSINESS_PARTNER_CODE)
            .itemCode(ITEM_CODE).productSku(PRODUCT_SKU).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .stock(0).minimumStock(0).syncStock(true).fbbActive(true).mppForWhEnabled(true).build();
    SyncStockUpdateOrInsertVo syncStockUpdateOrInsertVo =
        SyncStockUpdateOrInsertVo.builder().updateOrInsertStockVo(updateOrInsertStockVo).pickupPointSyncStockDtoList(
            Collections.singletonList(new ItemSkuPickupPointSyncStockDto(ITEM_SKU, PICKUP_POINT_CODE, true))).build();
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(REQUEST_ID, Arrays.asList(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.doThrow(
            new ApplicationRuntimeException(ErrorCategory.VALIDATION, Constants.INVENTORY_NOT_FOUND_ERROR_MESSAGE))
        .when(this.productLevel3InventoryRepository)
        .updateSyncStockAtL5(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
            updateSyncStockByWebItemPickupPointRequestDTOCaptor.capture());
    Mockito.doNothing().when(productLevel3InventoryRepository)
        .insertInventory(Mockito.eq(mandatoryRequestParam), argumentCaptorWebInventoryInsertRequestDTOList.capture());
    this.productLevel3InventoryServiceBean.updateSyncStockOrInsertStock(syncStockUpdateOrInsertVo);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(REQUEST_ID, Arrays.asList(PICKUP_POINT_CODE));
    Mockito.verify(this.productLevel3InventoryRepository)
        .updateSyncStockAtL5(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
            updateSyncStockByWebItemPickupPointRequestDTOCaptor.capture());
    Mockito.verify(productLevel3InventoryRepository)
        .insertInventory(Mockito.eq(mandatoryRequestParam), argumentCaptorWebInventoryInsertRequestDTOList.capture());
    Assertions.assertEquals(ITEM_SKU,
        updateSyncStockByWebItemPickupPointRequestDTOCaptor.getValue().get(0).getWebItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        updateSyncStockByWebItemPickupPointRequestDTOCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertTrue(updateSyncStockByWebItemPickupPointRequestDTOCaptor.getValue().get(0).isSyncStock());
    Assertions.assertEquals(ITEM_SKU, argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWebItemSku());
    Assertions.assertEquals(ITEM_CODE,
        argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getWarehouseItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getPickupPointCode());
    Assertions.assertEquals(0,
        argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getAvailableStock().intValue());
    Assertions.assertEquals(0,
        argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).getMinimumStockAlert().intValue());
    Assertions.assertTrue(argumentCaptorWebInventoryInsertRequestDTOList.getValue().get(0).isSyncStock());
  }

  @Test
  public void updateSyncStockOrInsertStockTest_Error() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "inventoryL5SyncStockEnabled", true);
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "insertInventoryEnabledOnFailedSyncStockUpdate",
        true);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    UpdateOrInsertStockVo updateOrInsertStockVo =
        UpdateOrInsertStockVo.builder().profileResponse(profileResponse).businessPartnerCode(BUSINESS_PARTNER_CODE)
            .itemCode(ITEM_CODE).productSku(PRODUCT_SKU).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .stock(DELTA_STOCK).minimumStock(MIN_STOCK).syncStock(true).fbbActive(true).mppForWhEnabled(true).build();
    SyncStockUpdateOrInsertVo syncStockUpdateOrInsertVo =
        SyncStockUpdateOrInsertVo.builder().updateOrInsertStockVo(updateOrInsertStockVo).pickupPointSyncStockDtoList(
            Collections.singletonList(new ItemSkuPickupPointSyncStockDto(ITEM_SKU, PICKUP_POINT_CODE, true))).build();
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.doThrow(
            new ApplicationRuntimeException(ErrorCategory.VALIDATION, Constants.WAREHOUSE_SELLER_INVENTORY_FULFILLMENT))
        .when(this.productLevel3InventoryRepository)
        .updateSyncStockAtL5(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
            updateSyncStockByWebItemPickupPointRequestDTOCaptor.capture());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryServiceBean.updateSyncStockOrInsertStock(syncStockUpdateOrInsertVo);
      });
    } finally {
      Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Mockito.anyString(), Mockito.anyList());
      Mockito.verify(this.productLevel3InventoryRepository)
          .updateSyncStockAtL5(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
              updateSyncStockByWebItemPickupPointRequestDTOCaptor.capture());
      Assertions.assertEquals(ITEM_SKU,
          updateSyncStockByWebItemPickupPointRequestDTOCaptor.getValue().get(0).getWebItemSku());
      Assertions.assertEquals(PICKUP_POINT_CODE,
          updateSyncStockByWebItemPickupPointRequestDTOCaptor.getValue().get(0).getPickupPointCode());
      Assertions.assertTrue(updateSyncStockByWebItemPickupPointRequestDTOCaptor.getValue().get(0).isSyncStock());
    }
  }

  @Test
  public void updateSyncStockOrInsertStockTest_InsertOnErrorSwitchOff() throws Exception {
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "inventoryL5SyncStockEnabled", true);
    ReflectionTestUtils.setField(productLevel3InventoryServiceBean, "insertInventoryEnabledOnFailedSyncStockUpdate",
        false);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    UpdateOrInsertStockVo updateOrInsertStockVo =
        UpdateOrInsertStockVo.builder().profileResponse(profileResponse).businessPartnerCode(BUSINESS_PARTNER_CODE)
            .itemCode(ITEM_CODE).productSku(PRODUCT_SKU).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .stock(DELTA_STOCK).minimumStock(MIN_STOCK).syncStock(true).fbbActive(true).mppForWhEnabled(true).build();
    SyncStockUpdateOrInsertVo syncStockUpdateOrInsertVo =
        SyncStockUpdateOrInsertVo.builder().updateOrInsertStockVo(updateOrInsertStockVo).pickupPointSyncStockDtoList(
            Collections.singletonList(new ItemSkuPickupPointSyncStockDto(ITEM_SKU, PICKUP_POINT_CODE, true))).build();
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setFbbActivated(true);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    Mockito.doThrow(
            new ApplicationRuntimeException(ErrorCategory.VALIDATION, Constants.WAREHOUSE_SELLER_INVENTORY_FULFILLMENT))
        .when(this.productLevel3InventoryRepository)
        .updateSyncStockAtL5(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
            updateSyncStockByWebItemPickupPointRequestDTOCaptor.capture());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel3InventoryServiceBean.updateSyncStockOrInsertStock(syncStockUpdateOrInsertVo);
      });
    } finally {
      Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Mockito.anyString(), Mockito.anyList());
      Mockito.verify(this.productLevel3InventoryRepository)
          .updateSyncStockAtL5(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
              updateSyncStockByWebItemPickupPointRequestDTOCaptor.capture());
      Assertions.assertEquals(ITEM_SKU,
          updateSyncStockByWebItemPickupPointRequestDTOCaptor.getValue().get(0).getWebItemSku());
      Assertions.assertEquals(PICKUP_POINT_CODE,
          updateSyncStockByWebItemPickupPointRequestDTOCaptor.getValue().get(0).getPickupPointCode());
      Assertions.assertTrue(updateSyncStockByWebItemPickupPointRequestDTOCaptor.getValue().get(0).isSyncStock());
    }
  }


  @Test
  public void updatePickupPointInInventoryTest() throws Exception {
    InventoryUpsertModel inventoryUpsertModel =
        InventoryUpsertModel.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).itemCode(ITEM_CODE)
            .productSku(PRODUCT_SKU).itemSku(ITEM_SKU).oldPickupPointCode(PICKUP_POINT_CODE)
            .newPickupPointCode(PICKUP_POINT_CODE_2).stock(0).minimumStock(0).syncStock(true).fbbActive(true).build();

    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(ProfileResponse.builder().company(CompanyDTO.builder().build()).build());
    Mockito.doNothing().when(productLevel3InventoryRepository).insertInventory(
        Mockito.eq(ProductLevel3InventoryUtil.generateMandatoryRequestParam()), Mockito.anyList());

    productLevel3InventoryServiceBean.updatePickupPointInInventory(BUSINESS_PARTNER_CODE, Arrays.asList(inventoryUpsertModel));

    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3InventoryRepository).insertInventory(
        Mockito.eq(ProductLevel3InventoryUtil.generateMandatoryRequestParam()), Mockito.anyList());
  }

  @Test
  public void updateStockForItemPickupPoint_quotaOnlyUpdateTest() throws Exception {
    Mockito.when(this.productLevel3InventoryRepository
        .updateStockIncreaseV2(eq(mandatoryRequestParam), Mockito.any(WebInventoryUpdateStockRequestV2DTO.class)))
        .thenReturn(null);

    this.productLevel3InventoryServiceBean.updateStockForItemPickupPoint(
        BUSINESS_PARTNER_CODE, ITEM_SKU, PICKUP_POINT_CODE, null, 5, true);

    Mockito.verify(this.productLevel3InventoryRepository)
        .updateStockIncreaseV2(eq(mandatoryRequestParam), updateStockRequestCaptor.capture());

    WebInventoryUpdateStockRequestV2DTO capturedRequest = updateStockRequestCaptor.getValue();
    assertNotNull(capturedRequest);
    assertThat(capturedRequest.getPreOrderQuota(), equalTo(5));
    assertThat(capturedRequest.getStock(), equalTo(0));
  }

  @Test
  public void updateStockForItemPickupPoint_quotaZeroShouldSkipTest() throws Exception {
    this.productLevel3InventoryServiceBean.updateStockForItemPickupPoint(BUSINESS_PARTNER_CODE,
        ITEM_SKU, PICKUP_POINT_CODE, null, 0, true);
  
    verifyNoMoreInteractions(this.productLevel3InventoryRepository);
  }

  @Test
  public void updateStockForItemPickupPoint_quotaFlagFalseNoQuotaSetTest() throws Exception {
    Mockito.when(this.productLevel3InventoryRepository
        .updateStockIncreaseV2(eq(mandatoryRequestParam), Mockito.any(WebInventoryUpdateStockRequestV2DTO.class)))
        .thenReturn(null);

    this.productLevel3InventoryServiceBean.updateStockForItemPickupPoint(
        BUSINESS_PARTNER_CODE, ITEM_SKU, PICKUP_POINT_CODE, 2, 7, false);

    Mockito.verify(this.productLevel3InventoryRepository)
        .updateStockIncreaseV2(eq(mandatoryRequestParam), updateStockRequestCaptor.capture());

    WebInventoryUpdateStockRequestV2DTO capturedRequest = updateStockRequestCaptor.getValue();
    assertNotNull(capturedRequest);
    assertThat(capturedRequest.getPreOrderQuota(), equalTo(0));
    assertThat(capturedRequest.getStock(), equalTo(2));
  }

  @Test
  public void updateStockForItemPickupPoint_quotaFlagTrueButZeroQuotaTest() throws Exception {
    Mockito.when(this.productLevel3InventoryRepository
        .updateStockIncreaseV2(eq(mandatoryRequestParam), Mockito.any(WebInventoryUpdateStockRequestV2DTO.class)))
        .thenReturn(null);

    // deltaStock provided to ensure update proceeds, deltaQuota is zero, updatePreOrderQuota is true
    this.productLevel3InventoryServiceBean.updateStockForItemPickupPoint(
        BUSINESS_PARTNER_CODE, ITEM_SKU, PICKUP_POINT_CODE, 2, 0, true);

    Mockito.verify(this.productLevel3InventoryRepository)
        .updateStockIncreaseV2(eq(mandatoryRequestParam), updateStockRequestCaptor.capture());

    WebInventoryUpdateStockRequestV2DTO capturedRequest = updateStockRequestCaptor.getValue();
    assertNotNull(capturedRequest);
    // PreOrderQuota should not be set since deltaQuota is zero (invalid)
    assertThat(capturedRequest.getPreOrderQuota(), equalTo(0));
    assertThat(capturedRequest.getStock(), equalTo(2));
  }

  @Test
  public void updateStockForItemPickupPoint_decreaseQuota() throws Exception {
    this.productLevel3InventoryServiceBean.updateStockForItemPickupPoint(
        BUSINESS_PARTNER_CODE, ITEM_SKU, PICKUP_POINT_CODE, 0, -2, true);
    Mockito.verify(this.productLevel3InventoryRepository)
        .updateStockDecrease(eq(mandatoryRequestParam), updateStockRequestCaptor.capture());
    WebInventoryUpdateStockRequestV2DTO capturedRequest = updateStockRequestCaptor.getValue();
    assertNotNull(capturedRequest);
    assertThat(capturedRequest.getPreOrderQuota(), equalTo(2));
    assertThat(capturedRequest.getStock(), equalTo(0));
  }
}
