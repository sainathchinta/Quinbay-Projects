package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;

import java.util.Date;
import java.util.List;
import java.util.UUID;

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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.OfflineInventoryStockUpdatedEvent;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.service.ProductItemBusinessPartnerService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;

public class OfflineInventoryStockChangeSubscriberBeanTest {

  private static final String REQUEST_ID = UUID.randomUUID().toString();
  private static final String CLIENT_ID = "DEV";
  private static final String USERNAME = "DEVELOPER";
  private static final String STORE_ID = "storeId";
  private static final String ITEM_SKU = "itemSku";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ITEM_NAME = "itemName";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String X_PRODUCT_INTEGRATOR_CLIENT_ID = "x-product-integrator";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String PROHIBITED_MERCHANT_CODE =
      "prohibitedMerchantCode1,prohibitedMerchantCode2";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String OFFLINE_INVENTORY_ID = ITEM_SKU.concat("-").concat(PICKUP_POINT_CODE);
  private static final Integer OLD_ORIGINAL_STOCK = 10;
  private static final Integer NEW_ORIGINAL_STOCK = 15;

  @InjectMocks
  private OfflineInventoryStockChangeSubscriberBean offlineInventoryStockChangeSubscriberBean;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @Mock
  private UpdatedProductHistoryService logAuditTrailUpdatedOfflineProductService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Captor
  private ArgumentCaptor<List<UpdatedProductHistory>> logAuditTrailUpdatedOfflineProductCaptor;

  private OfflineInventoryStockUpdatedEvent offlineInventoryStockUpdatedEvent;
  private ProductSystemParameter prohibitedMerchantCodes;
  private ProductSystemParameter xProductIntegratorClientId;
  private ObjectMapper mapper;
  private ProductItemBusinessPartner productItemBusinessPartner;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    offlineInventoryStockUpdatedEvent = OfflineInventoryStockUpdatedEvent.builder()
        .clientId(CLIENT_ID)
        .itemSku(ITEM_SKU)
        .merchantCode(MERCHANT_CODE)
        .offlineInventoryId(OFFLINE_INVENTORY_ID)
        .oldOriginalStock(OLD_ORIGINAL_STOCK)
        .originalStock(NEW_ORIGINAL_STOCK)
        .pickupPointCode(PICKUP_POINT_CODE)
        .requestId(REQUEST_ID)
        .storeId(STORE_ID)
        .updatedDate(new Date())
        .username(USERNAME)
        .build();

    prohibitedMerchantCodes = new ProductSystemParameter();
    prohibitedMerchantCodes.setValue(PROHIBITED_MERCHANT_CODE);
    prohibitedMerchantCodes.setVariable(
        SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);

    xProductIntegratorClientId = new ProductSystemParameter();
    xProductIntegratorClientId.setValue(X_PRODUCT_INTEGRATOR_CLIENT_ID);
    xProductIntegratorClientId.setVariable(
        SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);

    productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setProductItemId(PRODUCT_ITEM_ID);
    productItemBusinessPartner.setProductBusinessPartner(new ProductBusinessPartner());
    productItemBusinessPartner.getProductBusinessPartner().setGdnProductSku(PRODUCT_SKU);

    Mockito.when(this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
            SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE))
        .thenReturn(prohibitedMerchantCodes);
    Mockito.when(this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID))
        .thenReturn(xProductIntegratorClientId);
    Mockito.when(this.productItemBusinessPartnerService.findProductItemByItemSku(STORE_ID,
        ITEM_SKU)).thenReturn(productItemBusinessPartner);
    Mockito.when(this.productOutbound.findProductNameByProductItemId(PRODUCT_ITEM_ID)).thenReturn(ITEM_NAME);
    mapper = new ObjectMapper();
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = mapper.writeValueAsString(offlineInventoryStockUpdatedEvent);
    Mockito.when(objectMapper.readValue(message, OfflineInventoryStockUpdatedEvent.class))
        .thenReturn(offlineInventoryStockUpdatedEvent);
    this.offlineInventoryStockChangeSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productSystemParameterService).findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
    Mockito.verify(this.productSystemParameterService).findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);
    Mockito.verify(this.logAuditTrailUpdatedOfflineProductService)
        .createAudit(logAuditTrailUpdatedOfflineProductCaptor.capture(), Mockito.eq(false));
    verify(objectMapper).readValue(message, OfflineInventoryStockUpdatedEvent.class);
    verify(productItemBusinessPartnerService).findProductItemByItemSku(STORE_ID, ITEM_SKU);
    verify(productOutbound).findProductNameByProductItemId(PRODUCT_ITEM_ID);
    UpdatedProductHistory result = logAuditTrailUpdatedOfflineProductCaptor.getValue().get(0);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(offlineInventoryStockUpdatedEvent.getPickupPointCode(), result.getPickupPointCode());
    Assertions.assertEquals(offlineInventoryStockUpdatedEvent.getItemSku(), result.getGdnSku());
    Assertions.assertEquals(ITEM_NAME, result.getGdnName());
    Assertions.assertEquals(PRODUCT_SKU, result.getProductSku());
  }

  @Test
  public void onDomainEventConsumedProhibitedMerchantTest() throws Exception {
    offlineInventoryStockUpdatedEvent.setMerchantCode("prohibitedMerchantCode1");
    String message = mapper.writeValueAsString(offlineInventoryStockUpdatedEvent);
    Mockito.when(objectMapper.readValue(message, OfflineInventoryStockUpdatedEvent.class))
        .thenReturn(offlineInventoryStockUpdatedEvent);
    this.offlineInventoryStockChangeSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, OfflineInventoryStockUpdatedEvent.class);
    Mockito.verify(this.productSystemParameterService).findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
  }

  @Test
  public void onDomainEventConsumedFromXProductIntegratorTest() throws Exception {
    offlineInventoryStockUpdatedEvent.setUsername(X_PRODUCT_INTEGRATOR_CLIENT_ID);
    String message = mapper.writeValueAsString(offlineInventoryStockUpdatedEvent);
    Mockito.when(objectMapper.readValue(message, OfflineInventoryStockUpdatedEvent.class))
        .thenReturn(offlineInventoryStockUpdatedEvent);
    this.offlineInventoryStockChangeSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productSystemParameterService).findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
    Mockito.verify(this.productSystemParameterService).findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);
    Mockito.verify(this.logAuditTrailUpdatedOfflineProductService)
        .createAudit(logAuditTrailUpdatedOfflineProductCaptor.capture(), Mockito.eq(false));
    verify(objectMapper).readValue(message, OfflineInventoryStockUpdatedEvent.class);
    verify(productItemBusinessPartnerService).findProductItemByItemSku(STORE_ID, ITEM_SKU);
    verify(productOutbound).findProductNameByProductItemId(PRODUCT_ITEM_ID);
    UpdatedProductHistory result = logAuditTrailUpdatedOfflineProductCaptor.getValue().get(0);
    Assertions.assertNotNull(result);
    Assertions.assertEquals("System", result.getChangedBy());
    Assertions.assertEquals(ITEM_NAME, result.getGdnName());
    Assertions.assertEquals(PRODUCT_SKU, result.getProductSku());
  }

  @Test
  public void onDomainEventConsumedThrowExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class)
        .when(this.logAuditTrailUpdatedOfflineProductService)
        .createAudit(Mockito.anyList(), Mockito.eq(false));
    String message = mapper.writeValueAsString(offlineInventoryStockUpdatedEvent);
    Mockito.when(objectMapper.readValue(message, OfflineInventoryStockUpdatedEvent.class))
        .thenReturn(offlineInventoryStockUpdatedEvent);
    this.offlineInventoryStockChangeSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productSystemParameterService).findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
    Mockito.verify(this.productSystemParameterService).findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);
    Mockito.verify(this.logAuditTrailUpdatedOfflineProductService)
        .createAudit(logAuditTrailUpdatedOfflineProductCaptor.capture(), Mockito.eq(false));
    verify(objectMapper).readValue(message, OfflineInventoryStockUpdatedEvent.class);
    verify(productItemBusinessPartnerService).findProductItemByItemSku(STORE_ID, ITEM_SKU);
    verify(productOutbound).findProductNameByProductItemId(PRODUCT_ITEM_ID);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(
        this.productSystemParameterService,
        this.logAuditTrailUpdatedOfflineProductService,
        this.objectMapper,
        this.productOutbound,
        this.productItemBusinessPartnerService);
  }
}
