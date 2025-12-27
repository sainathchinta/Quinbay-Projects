package com.gdn.partners.pbp.rest.converter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import com.gdn.partners.pbp.dto.promo.response.PromoMerchantCountResponse;
import com.gdn.partners.pbp.dto.promo.response.PromoMerchantDetailResponse;
import com.gdn.partners.pbp.dto.promo.response.PromoMerchantHistoryResponse;
import com.gdn.partners.pbp.dto.promo.response.PromoMerchantRuleComboResponse;
import com.gdn.partners.pbp.dto.promo.response.PromoMerchantRuleWholesaleResponse;
import com.gdn.partners.pbp.dto.promo.response.PromoMerchantSummaryResponse;
import com.gdn.partners.pbp.model.promo.PromoMerchant;
import com.gdn.partners.pbp.model.promo.PromoMerchantCount;
import com.gdn.partners.pbp.model.promo.PromoMerchantHistory;
import com.gdn.partners.pbp.model.promo.PromoMerchantInfoInventory;
import com.gdn.partners.pbp.model.promo.PromoMerchantInfoItem;
import com.gdn.partners.pbp.model.promo.PromoMerchantRuleCombo;
import com.gdn.partners.pbp.model.promo.PromoMerchantRuleWholesale;

public class PromoMerchantResponseConverterBeanTest {

  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String ITEM_SKU = "itemSku";
  private static final String PROMO_ID = "promoId";
  private static final String PROMO_TYPE = "promoType";
  private static final String PROMO_NAME = "promoName";
  private static final String PROMO_STATUS = "promoStatus";
  private static final Date PROMO_START = new Date();
  private static final Date PROMO_END = new Date();
  private static final int PROMO_QUOTA = 5;
  private static final int PROMO_SOLD = 6;
  private static final Date CREATED_DATE = new Date();
  private static final boolean COMBO_MAIN_SKU = true;
  private static final int COMBO_QTY = 9;
  private static final double COMBO_DISC_PERCENTAGE = 10;
  private static final int WHOLESALE_MIN_QTY = 11;
  private static final int WHOLESALE_MAX_QTY = 12;
  private static final double WHOLESALE_DISC_PERCENTAGE = 13;
  private static final String ITEM_CODE = "itemCode";
  private static final String ITEM_NAME = "itemName";
  private static final String ITEM_IMAGE_LOCATION_PATH = "itemImageLocationPath";
  private static final double ITEM_PRICE = 14;
  private static final double ITEM_SALE_PRICE = 15;
  private static final int INVENTORY_STOCK = 7;
  private static final int INVENTORY_STOCK_RESERVED = 8;
  private static final boolean INVENTORY_SYNC_STOCK = false;
  private static final int COUNT_ALL = 4;
  private static final int COUNT_NOT_YEY_ACTIVE = 1;
  private static final int COUNT_ACTIVE = 2;
  private static final int COUNT_EXPIRED = 3;
  private static final String HISTORY_ACTIVITY = "activity";
  private static final String HISTORY_NEW_VALUE = "newValue";
  private static final String HISTORY_OLD_VALUE = "oldValue";
  private static final Date HISTORY_UPDATED_DATE = new Date();
  private static final String HISTORY_UPDATED_BY = "updatedBy";

  @InjectMocks
  private PromoMerchantResponseConverterBean promoMerchantResponseConverterBean;

  private PromoMerchantCount promoMerchantCount;
  private PromoMerchantHistory promoMerchantHistory;
  private PromoMerchant promoMerchant;
  private PromoMerchantRuleCombo promoMerchantRuleCombo;
  private List<PromoMerchantRuleCombo> promoMerchantRuleComboList;
  private PromoMerchantRuleWholesale promoMerchantRuleWholesale;
  private List<PromoMerchantRuleWholesale> promoMerchantRuleWholesaleList;
  private PromoMerchantInfoItem promoMerchantInfoItem;
  private PromoMerchantInfoInventory promoMerchantInfoInventory;

  @BeforeEach
  public void __initialize() {
    initMocks(this);

    this.promoMerchantCount = new PromoMerchantCount();
    this.promoMerchantCount.setAll(PromoMerchantResponseConverterBeanTest.COUNT_ALL);
    this.promoMerchantCount
        .setNotYetActive(PromoMerchantResponseConverterBeanTest.COUNT_NOT_YEY_ACTIVE);
    this.promoMerchantCount.setActive(PromoMerchantResponseConverterBeanTest.COUNT_ACTIVE);
    this.promoMerchantCount.setExpired(PromoMerchantResponseConverterBeanTest.COUNT_EXPIRED);
    this.promoMerchantHistory = new PromoMerchantHistory();
    this.promoMerchantHistory.setPromoId(PromoMerchantResponseConverterBeanTest.PROMO_ID);
    this.promoMerchantHistory.setActivity(PromoMerchantResponseConverterBeanTest.HISTORY_ACTIVITY);
    this.promoMerchantHistory.setNewValue(PromoMerchantResponseConverterBeanTest.HISTORY_NEW_VALUE);
    this.promoMerchantHistory.setOldValue(PromoMerchantResponseConverterBeanTest.HISTORY_OLD_VALUE);
    this.promoMerchantHistory
        .setUpdatedDate(PromoMerchantResponseConverterBeanTest.HISTORY_UPDATED_DATE);
    this.promoMerchantHistory
        .setUpdatedBy(PromoMerchantResponseConverterBeanTest.HISTORY_UPDATED_BY);
    this.promoMerchantInfoItem = new PromoMerchantInfoItem();
    this.promoMerchantInfoItem.setItemSku(PromoMerchantResponseConverterBeanTest.ITEM_SKU);
    this.promoMerchantInfoItem.setItemCode(PromoMerchantResponseConverterBeanTest.ITEM_CODE);
    this.promoMerchantInfoItem.setItemName(PromoMerchantResponseConverterBeanTest.ITEM_NAME);
    this.promoMerchantInfoItem
        .setItemImageLocationPath(PromoMerchantResponseConverterBeanTest.ITEM_IMAGE_LOCATION_PATH);
    this.promoMerchantInfoItem.setItemPrice(PromoMerchantResponseConverterBeanTest.ITEM_PRICE);
    this.promoMerchantInfoItem
        .setItemSalePrice(PromoMerchantResponseConverterBeanTest.ITEM_SALE_PRICE);
    this.promoMerchantInfoInventory = new PromoMerchantInfoInventory();
    this.promoMerchantInfoInventory
        .setStock(PromoMerchantResponseConverterBeanTest.INVENTORY_STOCK);
    this.promoMerchantInfoInventory
        .setStockReserved(PromoMerchantResponseConverterBeanTest.INVENTORY_STOCK_RESERVED);
    this.promoMerchantInfoInventory
        .setSyncStock(PromoMerchantResponseConverterBeanTest.INVENTORY_SYNC_STOCK);
    this.promoMerchantRuleCombo = new PromoMerchantRuleCombo();
    this.promoMerchantRuleCombo.setMainSku(PromoMerchantResponseConverterBeanTest.COMBO_MAIN_SKU);
    this.promoMerchantRuleCombo.setItemSku(PromoMerchantResponseConverterBeanTest.ITEM_SKU);
    this.promoMerchantRuleCombo.setQty(PromoMerchantResponseConverterBeanTest.COMBO_QTY);
    this.promoMerchantRuleCombo
        .setDiscPercentage(PromoMerchantResponseConverterBeanTest.COMBO_DISC_PERCENTAGE);
    this.promoMerchantRuleCombo.setInfoItem(this.promoMerchantInfoItem);
    this.promoMerchantRuleCombo.setInfoInventory(this.promoMerchantInfoInventory);
    this.promoMerchantRuleComboList = new ArrayList<>();
    this.promoMerchantRuleComboList.add(this.promoMerchantRuleCombo);
    this.promoMerchantRuleWholesale = new PromoMerchantRuleWholesale();
    this.promoMerchantRuleWholesale
        .setMinQty(PromoMerchantResponseConverterBeanTest.WHOLESALE_MIN_QTY);
    this.promoMerchantRuleWholesale
        .setMaxQty(PromoMerchantResponseConverterBeanTest.WHOLESALE_MAX_QTY);
    this.promoMerchantRuleWholesale
        .setDiscPercentage(PromoMerchantResponseConverterBeanTest.WHOLESALE_DISC_PERCENTAGE);
    this.promoMerchantRuleWholesaleList = new ArrayList<>();
    this.promoMerchantRuleWholesaleList.add(this.promoMerchantRuleWholesale);
    this.promoMerchant = new PromoMerchant();
    this.promoMerchant
        .setBusinessPartnerCode(PromoMerchantResponseConverterBeanTest.BUSINESS_PARTNER_CODE);
    this.promoMerchant.setItemSku(PromoMerchantResponseConverterBeanTest.ITEM_SKU);
    this.promoMerchant.setPromoId(PromoMerchantResponseConverterBeanTest.PROMO_ID);
    this.promoMerchant.setPromoType(PromoMerchantResponseConverterBeanTest.PROMO_TYPE);
    this.promoMerchant.setPromoName(PromoMerchantResponseConverterBeanTest.PROMO_NAME);
    this.promoMerchant.setPromoStatus(PromoMerchantResponseConverterBeanTest.PROMO_STATUS);
    this.promoMerchant.setPromoStart(PromoMerchantResponseConverterBeanTest.PROMO_START);
    this.promoMerchant.setPromoEnd(PromoMerchantResponseConverterBeanTest.PROMO_END);
    this.promoMerchant.setPromoQuota(PromoMerchantResponseConverterBeanTest.PROMO_QUOTA);
    this.promoMerchant.setPromoSold(PromoMerchantResponseConverterBeanTest.PROMO_SOLD);
    this.promoMerchant.setCreatedDate(PromoMerchantResponseConverterBeanTest.CREATED_DATE);
    this.promoMerchant.setPromoRuleComboSet(this.promoMerchantRuleComboList);
    this.promoMerchant.setPromoRuleWholesaleSet(this.promoMerchantRuleWholesaleList);
    this.promoMerchant.setInfoItem(this.promoMerchantInfoItem);
    this.promoMerchant.setInfoInventory(this.promoMerchantInfoInventory);
  }

  @AfterEach
  public void _finalize() {}

  @Test
  public void testConvertToPromoMerchantCountResponse_promoMerchantCount() {
    PromoMerchantCountResponse result = this.promoMerchantResponseConverterBean
        .convertToPromoMerchantCountResponse(this.promoMerchantCount);
    assertNotNull(result);
    assertEquals(PromoMerchantResponseConverterBeanTest.COUNT_ALL, result.getAll());
    assertEquals(PromoMerchantResponseConverterBeanTest.COUNT_NOT_YEY_ACTIVE,
        result.getNotYetActive());
    assertEquals(PromoMerchantResponseConverterBeanTest.COUNT_ACTIVE, result.getActive());
    assertEquals(PromoMerchantResponseConverterBeanTest.COUNT_EXPIRED, result.getExpired());
  }

  @Test
  public void testConvertToPromoMerchantDetailResponse_promoMerchant() {
    PromoMerchantDetailResponse result = this.promoMerchantResponseConverterBean
        .convertToPromoMerchantDetailResponse(this.promoMerchant);
    assertNotNull(result);
    assertEquals(PromoMerchantResponseConverterBeanTest.BUSINESS_PARTNER_CODE,
        result.getBusinessPartnerCode());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_SKU, result.getItemSku());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_ID, result.getPromoId());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_TYPE, result.getPromoType());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_NAME, result.getPromoName());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_STATUS, result.getPromoStatus());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_START, result.getPromoStart());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_END, result.getPromoEnd());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_QUOTA, result.getPromoQuota());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_SOLD, result.getPromoSold());
    assertEquals(PromoMerchantResponseConverterBeanTest.CREATED_DATE, result.getCreatedDate());
    assertNotNull(result.getInfoItem());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_SKU,
        result.getInfoItem().getItemSku());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_CODE,
        result.getInfoItem().getItemCode());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_NAME,
        result.getInfoItem().getItemName());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_IMAGE_LOCATION_PATH,
        result.getInfoItem().getItemImageLocationPath());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_PRICE,
        result.getInfoItem().getItemPrice(), 0);
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_SALE_PRICE,
        result.getInfoItem().getItemSalePrice(), 0);
    assertNotNull(result.getInfoInventory());
    assertEquals(PromoMerchantResponseConverterBeanTest.INVENTORY_STOCK,
        result.getInfoInventory().getStock());
    assertEquals(PromoMerchantResponseConverterBeanTest.INVENTORY_STOCK_RESERVED,
        result.getInfoInventory().getStockReserved());
    assertEquals(PromoMerchantResponseConverterBeanTest.INVENTORY_SYNC_STOCK,
        result.getInfoInventory().isSyncStock());
    assertNotNull(result.getPromoRuleComboSet());
    PromoMerchantRuleComboResponse comboResult = result.getPromoRuleComboSet().iterator().next();
    assertEquals(PromoMerchantResponseConverterBeanTest.COMBO_MAIN_SKU, comboResult.isMainSku());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_SKU, comboResult.getItemSku());
    assertEquals(PromoMerchantResponseConverterBeanTest.COMBO_QTY, comboResult.getQty());
    assertEquals(PromoMerchantResponseConverterBeanTest.COMBO_DISC_PERCENTAGE,
        comboResult.getDiscPercentage(), 0);
    assertNotNull(comboResult.getInfoItem());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_SKU,
        comboResult.getInfoItem().getItemSku());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_CODE,
        comboResult.getInfoItem().getItemCode());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_NAME,
        comboResult.getInfoItem().getItemName());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_IMAGE_LOCATION_PATH,
        comboResult.getInfoItem().getItemImageLocationPath());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_PRICE,
        comboResult.getInfoItem().getItemPrice(), 0);
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_SALE_PRICE,
        comboResult.getInfoItem().getItemSalePrice(), 0);
    assertNotNull(comboResult.getInfoInventory());
    assertEquals(PromoMerchantResponseConverterBeanTest.INVENTORY_STOCK,
        comboResult.getInfoInventory().getStock());
    assertEquals(PromoMerchantResponseConverterBeanTest.INVENTORY_STOCK_RESERVED,
        comboResult.getInfoInventory().getStockReserved());
    assertEquals(PromoMerchantResponseConverterBeanTest.INVENTORY_SYNC_STOCK,
        comboResult.getInfoInventory().isSyncStock());
    assertNotNull(result.getPromoRuleWholesaleSet());
    PromoMerchantRuleWholesaleResponse wholesaleResult =
        result.getPromoRuleWholesaleSet().iterator().next();
    assertEquals(PromoMerchantResponseConverterBeanTest.WHOLESALE_MIN_QTY,
        wholesaleResult.getMinQty(), 0);
    assertEquals(PromoMerchantResponseConverterBeanTest.WHOLESALE_MAX_QTY,
        wholesaleResult.getMaxQty(), 0);
    assertEquals(PromoMerchantResponseConverterBeanTest.WHOLESALE_DISC_PERCENTAGE,
        wholesaleResult.getDiscPercentage(), 0);
  }

  @Test
  public void testConvertToPromoMerchantDetailResponse_promoMerchant_comboAndWholesaleNull() {
    this.promoMerchant.setPromoRuleComboSet(null);
    this.promoMerchant.setPromoRuleWholesaleSet(null);
    PromoMerchantDetailResponse result = this.promoMerchantResponseConverterBean
        .convertToPromoMerchantDetailResponse(this.promoMerchant);
    assertNotNull(result);
    assertEquals(PromoMerchantResponseConverterBeanTest.BUSINESS_PARTNER_CODE,
        result.getBusinessPartnerCode());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_SKU, result.getItemSku());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_ID, result.getPromoId());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_TYPE, result.getPromoType());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_NAME, result.getPromoName());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_STATUS, result.getPromoStatus());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_START, result.getPromoStart());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_END, result.getPromoEnd());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_QUOTA, result.getPromoQuota());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_SOLD, result.getPromoSold());
    assertEquals(PromoMerchantResponseConverterBeanTest.CREATED_DATE, result.getCreatedDate());
    assertNotNull(result.getInfoItem());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_SKU,
        result.getInfoItem().getItemSku());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_CODE,
        result.getInfoItem().getItemCode());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_NAME,
        result.getInfoItem().getItemName());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_IMAGE_LOCATION_PATH,
        result.getInfoItem().getItemImageLocationPath());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_PRICE,
        result.getInfoItem().getItemPrice(), 0);
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_SALE_PRICE,
        result.getInfoItem().getItemSalePrice(), 0);
    assertNotNull(result.getInfoInventory());
    assertEquals(PromoMerchantResponseConverterBeanTest.INVENTORY_STOCK,
        result.getInfoInventory().getStock());
    assertEquals(PromoMerchantResponseConverterBeanTest.INVENTORY_STOCK_RESERVED,
        result.getInfoInventory().getStockReserved());
    assertEquals(PromoMerchantResponseConverterBeanTest.INVENTORY_SYNC_STOCK,
        result.getInfoInventory().isSyncStock());
    assertNull(result.getPromoRuleComboSet());
    assertNull(result.getPromoRuleWholesaleSet());
  }

  @Test
  public void testConvertToPromoMerchantHistoryResponse_promoMerchantHistory() {
    PromoMerchantHistoryResponse result = this.promoMerchantResponseConverterBean
        .convertToPromoMerchantHistoryResponse(this.promoMerchantHistory);
    assertNotNull(result);
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_ID, result.getPromoId());
    assertEquals(PromoMerchantResponseConverterBeanTest.HISTORY_ACTIVITY, result.getActivity());
    assertEquals(PromoMerchantResponseConverterBeanTest.HISTORY_NEW_VALUE, result.getNewValue());
    assertEquals(PromoMerchantResponseConverterBeanTest.HISTORY_OLD_VALUE, result.getOldValue());
    assertEquals(PromoMerchantResponseConverterBeanTest.HISTORY_UPDATED_DATE,
        result.getUpdatedDate());
    assertEquals(PromoMerchantResponseConverterBeanTest.HISTORY_UPDATED_BY, result.getUpdatedBy());
  }

  @Test
  public void testConvertToPromoMerchantSummaryResponse_promoMerchant() {
    PromoMerchantSummaryResponse result = this.promoMerchantResponseConverterBean
        .convertToPromoMerchantSummaryResponse(this.promoMerchant);
    assertNotNull(result);
    assertEquals(PromoMerchantResponseConverterBeanTest.BUSINESS_PARTNER_CODE,
        result.getBusinessPartnerCode());
    assertEquals(PromoMerchantResponseConverterBeanTest.ITEM_SKU, result.getItemSku());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_ID, result.getPromoId());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_TYPE, result.getPromoType());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_NAME, result.getPromoName());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_STATUS, result.getPromoStatus());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_START, result.getPromoStart());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_END, result.getPromoEnd());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_QUOTA, result.getPromoQuota());
    assertEquals(PromoMerchantResponseConverterBeanTest.PROMO_SOLD, result.getPromoSold());
    assertEquals(PromoMerchantResponseConverterBeanTest.CREATED_DATE, result.getCreatedDate());
    assertNotNull(result.getInfoInventory());
    assertEquals(PromoMerchantResponseConverterBeanTest.INVENTORY_STOCK,
        result.getInfoInventory().getStock());
    assertEquals(PromoMerchantResponseConverterBeanTest.INVENTORY_STOCK_RESERVED,
        result.getInfoInventory().getStockReserved());
    assertEquals(PromoMerchantResponseConverterBeanTest.INVENTORY_SYNC_STOCK,
        result.getInfoInventory().isSyncStock());
  }
}
