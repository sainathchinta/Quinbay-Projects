package com.gdn.partners.pbp.service.listener;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.OfflineItemChangeEvent;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.service.ProductItemBusinessPartnerService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;

public class OfflineItemChangeSubscriberBeanTest {

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
  private static final String PROHIBITED_MERCHANT_CODE = "ALFAMART,ALFAMIDI";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String OFFLINE_ITEM_ID = ITEM_SKU.concat("-").concat(PICKUP_POINT_CODE);
  private static final Double LIST_PRICE = 40000.0;
  private static final Double OFFER_PRICE = 32000.0;
  private static final Double OLD_LIST_PRICE = 20000.0;
  private static final Double OLD_OFFER_PRICE = 16000.0;

  @InjectMocks
  private OfflineItemChangeSubscriberBean offlineItemChangeSubscriberBean;

  @Mock
  private UpdatedProductHistoryService logAuditTrailUpdatedOfflineProductService;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Captor
  private ArgumentCaptor<List<UpdatedProductHistory>> logAuditTrailUpdatedOfflineProductsCaptor;

  private OfflineItemChangeEvent offlineItemChangeEvent;
  private ProductSystemParameter prohibitedMerchantCodes;
  private ProductSystemParameter xProductIntegratorClientId;
  private ObjectMapper mapper;
  private ProductItemBusinessPartner productItemBusinessPartner;

  @BeforeEach
  public void setUp() {
    initMocks(this);

    offlineItemChangeEvent = OfflineItemChangeEvent.builder()
        .storeId(STORE_ID)
        .clientId(CLIENT_ID)
        .updatedDate(new Date())
        .username(USERNAME)
        .requestId(REQUEST_ID)
        .uniqueId(OFFLINE_ITEM_ID)
        .merchantCode(MERCHANT_CODE)
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .listPrice(LIST_PRICE)
        .offerPrice(OFFER_PRICE)
        .newData(Boolean.FALSE)
        .oldListPrice(OLD_LIST_PRICE)
        .oldOfferPrice(OLD_OFFER_PRICE)
        .syncPriceAction(Boolean.FALSE)
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

    when(productSystemParameterService.findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE))
        .thenReturn(prohibitedMerchantCodes);
    when(productSystemParameterService.findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID))
        .thenReturn(xProductIntegratorClientId);
    Mockito.when(this.productItemBusinessPartnerService.findProductItemByItemSku(STORE_ID,
        ITEM_SKU)).thenReturn(productItemBusinessPartner);
    Mockito.when(this.productOutbound.findProductNameByProductItemId(PRODUCT_ITEM_ID)).thenReturn(ITEM_NAME);

    mapper = new ObjectMapper();
  }

  private void onDomainEventConsumedTest_doubleFieldAndVerifyActivities(
      OfflineItemChangeEvent event, List<String> activities) throws Exception {

    String message = mapper.writeValueAsString(event);
    Mockito.when(objectMapper.readValue(message, OfflineItemChangeEvent.class))
        .thenReturn(event);
    this.offlineItemChangeSubscriberBean.onDomainEventConsumed(message);

    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);
    verify(logAuditTrailUpdatedOfflineProductService)
        .createAudit(logAuditTrailUpdatedOfflineProductsCaptor.capture(), Mockito.eq(false));
    verify(objectMapper).readValue(message, OfflineItemChangeEvent.class);
    verify(productItemBusinessPartnerService, times(2)).findProductItemByItemSku(STORE_ID, ITEM_SKU);
    verify(productOutbound, times(2)).findProductNameByProductItemId(PRODUCT_ITEM_ID);
    List<UpdatedProductHistory> result = logAuditTrailUpdatedOfflineProductsCaptor.getValue();
    assertNotNull(result);
    assertEquals(2, result.size());
    assertEquals(activities,
        result.stream().map(UpdatedProductHistory::getActivity).collect(Collectors.toList()));
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    onDomainEventConsumedTest_doubleFieldAndVerifyActivities(offlineItemChangeEvent,
        Arrays.asList(UpdateProductActivity.NORMAL_PRICE.getDesc(), UpdateProductActivity.SELLING_PRICE.getDesc()));
  }

  @Test
  public void onDomainEventConsumedTest_syncPrice() throws Exception {
    offlineItemChangeEvent.setSyncPriceAction(Boolean.TRUE);
    onDomainEventConsumedTest_doubleFieldAndVerifyActivities(offlineItemChangeEvent,
        Arrays.asList(UpdateProductActivity.SYNC_NORMAL_PRICE.getDesc(), UpdateProductActivity.SYNC_SELLING_PRICE.getDesc()));
  }

  @Test
  public void onDomainEventConsumedTest_newData() throws Exception {
    offlineItemChangeEvent.setNewData(Boolean.TRUE);
    onDomainEventConsumedTest_doubleFieldAndVerifyActivities(offlineItemChangeEvent,
        Arrays.asList(UpdateProductActivity.INITIAL_NORMAL_PRICE.getDesc(), UpdateProductActivity.INITIAL_SELLING_PRICE.getDesc()));
  }

  private void onDomainEventConsumedTest_singleFieldAndVerifyActivity(
      OfflineItemChangeEvent event, String activity) throws Exception {

    String message = mapper.writeValueAsString(event);
    Mockito.when(objectMapper.readValue(message, OfflineItemChangeEvent.class))
        .thenReturn(event);
    this.offlineItemChangeSubscriberBean.onDomainEventConsumed(message);

    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);
    verify(logAuditTrailUpdatedOfflineProductService)
        .createAudit(logAuditTrailUpdatedOfflineProductsCaptor.capture(), Mockito.eq(false));
    verify(objectMapper).readValue(message, OfflineItemChangeEvent.class);

    List<UpdatedProductHistory> result =
        logAuditTrailUpdatedOfflineProductsCaptor.getValue();
    assertNotNull(result);
    assertEquals(1, result.size());
    UpdatedProductHistory logAudit = result.get(0);
    assertEquals(activity, logAudit.getActivity());
  }

  @Test
  public void onDomainEventConsumedTest_onlyUpdateListPrice() throws Exception {
    offlineItemChangeEvent.setOldOfferPrice(OFFER_PRICE);
    onDomainEventConsumedTest_singleFieldAndVerifyActivity(
        offlineItemChangeEvent, UpdateProductActivity.NORMAL_PRICE.getDesc());
    verify(productItemBusinessPartnerService).findProductItemByItemSku(STORE_ID, ITEM_SKU);
    verify(productOutbound).findProductNameByProductItemId(PRODUCT_ITEM_ID);
  }

  @Test
  public void onDomainEventConsumedTest_onlyUpdateOfferPrice() throws Exception {
    offlineItemChangeEvent.setOldListPrice(LIST_PRICE);
    onDomainEventConsumedTest_singleFieldAndVerifyActivity(
        offlineItemChangeEvent, UpdateProductActivity.SELLING_PRICE.getDesc());
    verify(productItemBusinessPartnerService).findProductItemByItemSku(STORE_ID, ITEM_SKU);
    verify(productOutbound).findProductNameByProductItemId(PRODUCT_ITEM_ID);
  }

  @Test
  public void onDomainEventConsumedTest_onlySyncListPrice() throws Exception {
    offlineItemChangeEvent.setOldOfferPrice(OFFER_PRICE);
    offlineItemChangeEvent.setSyncPriceAction(Boolean.TRUE);
    onDomainEventConsumedTest_singleFieldAndVerifyActivity(
        offlineItemChangeEvent, UpdateProductActivity.SYNC_NORMAL_PRICE.getDesc());
    verify(productItemBusinessPartnerService).findProductItemByItemSku(STORE_ID, ITEM_SKU);
    verify(productOutbound).findProductNameByProductItemId(PRODUCT_ITEM_ID);
  }

  @Test
  public void onDomainEventConsumedTest_onlySyncOfferPrice() throws Exception {
    offlineItemChangeEvent.setOldListPrice(LIST_PRICE);
    offlineItemChangeEvent.setSyncPriceAction(Boolean.TRUE);
    onDomainEventConsumedTest_singleFieldAndVerifyActivity(
        offlineItemChangeEvent, UpdateProductActivity.SYNC_SELLING_PRICE.getDesc());
    verify(productItemBusinessPartnerService).findProductItemByItemSku(STORE_ID, ITEM_SKU);
    verify(productOutbound).findProductNameByProductItemId(PRODUCT_ITEM_ID);
  }

  @Test
  public void onDomainEventConsumedTest_nothingUpdated() throws Exception {
    offlineItemChangeEvent.setOldListPrice(LIST_PRICE);
    offlineItemChangeEvent.setOldOfferPrice(OFFER_PRICE);

    String message = mapper.writeValueAsString(offlineItemChangeEvent);
    Mockito.when(objectMapper.readValue(message, OfflineItemChangeEvent.class))
        .thenReturn(offlineItemChangeEvent);
    this.offlineItemChangeSubscriberBean.onDomainEventConsumed(message);

    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);
    verify(objectMapper).readValue(message, OfflineItemChangeEvent.class);
    verify(logAuditTrailUpdatedOfflineProductService, times(0))
        .createAudit(logAuditTrailUpdatedOfflineProductsCaptor.capture(), Mockito.eq(false));
  }

  @Test
  public void onDomainEventConsumedTest_nothingSynced() throws Exception {
    offlineItemChangeEvent.setOldListPrice(LIST_PRICE);
    offlineItemChangeEvent.setOldOfferPrice(OFFER_PRICE);

    String message = mapper.writeValueAsString(offlineItemChangeEvent);
    Mockito.when(objectMapper.readValue(message, OfflineItemChangeEvent.class))
        .thenReturn(offlineItemChangeEvent);
    this.offlineItemChangeSubscriberBean.onDomainEventConsumed(message);
    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);
    verify(objectMapper).readValue(message, OfflineItemChangeEvent.class);
    verify(logAuditTrailUpdatedOfflineProductService, times(0))
        .createAudit(logAuditTrailUpdatedOfflineProductsCaptor.capture(), Mockito.eq(false));
  }

  @Test
  public void onDomainEventConsumedTest_excludedMerchant() throws Exception {
    offlineItemChangeEvent.setMerchantCode("ALFAMART");
    String message = mapper.writeValueAsString(offlineItemChangeEvent);
    Mockito.when(objectMapper.readValue(message, OfflineItemChangeEvent.class))
        .thenReturn(offlineItemChangeEvent);
    this.offlineItemChangeSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, OfflineItemChangeEvent.class);
    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
  }

  @Test
  public void onDomainEventConsumedTest_fromXProductIntegrator() throws Exception {
    offlineItemChangeEvent.setUsername(X_PRODUCT_INTEGRATOR_CLIENT_ID);
    String message = mapper.writeValueAsString(offlineItemChangeEvent);
    Mockito.when(objectMapper.readValue(message, OfflineItemChangeEvent.class))
        .thenReturn(offlineItemChangeEvent);
    this.offlineItemChangeSubscriberBean.onDomainEventConsumed(message);
    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);
    verify(logAuditTrailUpdatedOfflineProductService)
        .createAudit(logAuditTrailUpdatedOfflineProductsCaptor.capture(), Mockito.eq(false));
    verify(objectMapper).readValue(message, OfflineItemChangeEvent.class);
    List<UpdatedProductHistory> result =
        logAuditTrailUpdatedOfflineProductsCaptor.getValue();

    assertNotNull(result);
    assertTrue(result.stream().allMatch(e -> "System".equals(e.getChangedBy())));
    verify(productItemBusinessPartnerService, times(2)).findProductItemByItemSku(STORE_ID, ITEM_SKU);
    verify(productOutbound, times(2)).findProductNameByProductItemId(PRODUCT_ITEM_ID);
  }

  @Test
  public void onDomainEventConsumedTest_withPriceNull() throws Exception {
    offlineItemChangeEvent.setUsername(X_PRODUCT_INTEGRATOR_CLIENT_ID);
    offlineItemChangeEvent.setOldListPrice(null);
    offlineItemChangeEvent.setOldOfferPrice(null);
    String message = mapper.writeValueAsString(offlineItemChangeEvent);
    Mockito.when(objectMapper.readValue(message, OfflineItemChangeEvent.class))
        .thenReturn(offlineItemChangeEvent);
    this.offlineItemChangeSubscriberBean.onDomainEventConsumed(message);
    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
    verify(objectMapper).readValue(message, OfflineItemChangeEvent.class);
    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);
  }

  @Test
  public void onDomainEventConsumedTest_exception() throws Exception {
    doThrow(RuntimeException.class).when(logAuditTrailUpdatedOfflineProductService)
        .createAudit(anyList(), Mockito.eq(false));
    String message = mapper.writeValueAsString(offlineItemChangeEvent);
    Mockito.when(objectMapper.readValue(message, OfflineItemChangeEvent.class))
        .thenReturn(offlineItemChangeEvent);
    this.offlineItemChangeSubscriberBean.onDomainEventConsumed(message);
    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE);
    verify(productSystemParameterService).findByStoreIdAndVariable(
        STORE_ID, SystemParameterConstants.OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID);
    verify(objectMapper).readValue(message, OfflineItemChangeEvent.class);
    verify(logAuditTrailUpdatedOfflineProductService)
        .createAudit(logAuditTrailUpdatedOfflineProductsCaptor.capture(), Mockito.eq(false));
    verify(productItemBusinessPartnerService, times(2)).findProductItemByItemSku(STORE_ID, ITEM_SKU);
    verify(productOutbound, times(2)).findProductNameByProductItemId(PRODUCT_ITEM_ID);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productSystemParameterService,
        logAuditTrailUpdatedOfflineProductService,
        objectMapper,
        productOutbound,
        productItemBusinessPartnerService);
  }
}
