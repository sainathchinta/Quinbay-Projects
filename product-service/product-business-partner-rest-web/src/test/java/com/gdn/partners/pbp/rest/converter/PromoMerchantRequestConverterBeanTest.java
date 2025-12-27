package com.gdn.partners.pbp.rest.converter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import com.gdn.partners.pbp.dto.promo.request.PromoMerchantCreateRequest;
import com.gdn.partners.pbp.dto.promo.request.PromoMerchantRuleComboRequest;
import com.gdn.partners.pbp.dto.promo.request.PromoMerchantRuleWholesaleRequest;
import com.gdn.partners.pbp.dto.promo.request.PromoMerchantSummaryRequest;
import com.gdn.partners.pbp.dto.promo.request.PromoMerchantUpdateRequest;
import com.gdn.partners.pbp.model.promo.PromoMerchant;
import com.gdn.partners.pbp.model.promo.PromoMerchantRuleCombo;
import com.gdn.partners.pbp.model.promo.PromoMerchantRuleWholesale;
import com.gdn.partners.pbp.model.promo.PromoMerchantSummaryFilter;

public class PromoMerchantRequestConverterBeanTest {

  private static final String ITEM_SKU = "itemSku";
  private static final String PROMO_ID = "promoId";
  private static final String PROMO_TYPE = "promoType";
  private static final String PROMO_NAME = "promoName";
  private static final String PROMO_STATUS = "promoStatus";
  private static final Date PROMO_START = new Date();
  private static final Date PROMO_END = new Date();
  private static final int PROMO_QUOTA = 1;
  private static final boolean COMBO_MAIN_SKU = true;
  private static final int COMBO_QTY = 2;
  private static final double COMBO_DISC_PERCENTAGE = 3;
  private static final int WHOLESALE_MIN_QTY = 4;
  private static final int WHOLESALE_MAX_QTY = 5;
  private static final double WHOLESALE_DISC_PERCENTAGE = 6;

  @InjectMocks
  private PromoMerchantRequestConverterBean promoMerchantRequestConverterBean;

  private PromoMerchantCreateRequest promoMerchantCreateRequest;
  private PromoMerchantUpdateRequest promoMerchantUpdateRequest;
  private PromoMerchantRuleComboRequest promoMerchantRuleComboRequest;
  private Set<PromoMerchantRuleComboRequest> promoMerchantRuleComboRequestSet;
  private PromoMerchantRuleWholesaleRequest promoMerchantRuleWholesaleRequest;
  private Set<PromoMerchantRuleWholesaleRequest> promoMerchantRuleWholesaleRequestSet;
  private Set<String> promoStatusSet;
  private PromoMerchantSummaryRequest promoMerchantSummaryRequest;

  @BeforeEach
  public void __initialize() {
    initMocks(this);

    this.promoMerchantRuleComboRequest = new PromoMerchantRuleComboRequest();
    this.promoMerchantRuleComboRequest
        .setMainSku(PromoMerchantRequestConverterBeanTest.COMBO_MAIN_SKU);
    this.promoMerchantRuleComboRequest.setItemSku(PromoMerchantRequestConverterBeanTest.ITEM_SKU);
    this.promoMerchantRuleComboRequest.setQty(PromoMerchantRequestConverterBeanTest.COMBO_QTY);
    this.promoMerchantRuleComboRequest
        .setDiscPercentage(PromoMerchantRequestConverterBeanTest.COMBO_DISC_PERCENTAGE);
    this.promoMerchantRuleComboRequestSet = new HashSet<>();
    this.promoMerchantRuleComboRequestSet.add(this.promoMerchantRuleComboRequest);
    this.promoMerchantRuleWholesaleRequest = new PromoMerchantRuleWholesaleRequest();
    this.promoMerchantRuleWholesaleRequest
        .setMinQty(PromoMerchantRequestConverterBeanTest.WHOLESALE_MIN_QTY);
    this.promoMerchantRuleWholesaleRequest
        .setMaxQty(PromoMerchantRequestConverterBeanTest.WHOLESALE_MAX_QTY);
    this.promoMerchantRuleWholesaleRequest
        .setDiscPercentage(PromoMerchantRequestConverterBeanTest.WHOLESALE_DISC_PERCENTAGE);
    this.promoMerchantRuleWholesaleRequestSet = new HashSet<>();
    this.promoMerchantRuleWholesaleRequestSet.add(this.promoMerchantRuleWholesaleRequest);
    this.promoMerchantCreateRequest = new PromoMerchantCreateRequest();
    this.promoMerchantCreateRequest.setItemSku(PromoMerchantRequestConverterBeanTest.ITEM_SKU);
    this.promoMerchantCreateRequest.setPromoType(PromoMerchantRequestConverterBeanTest.PROMO_TYPE);
    this.promoMerchantCreateRequest.setPromoName(PromoMerchantRequestConverterBeanTest.PROMO_NAME);
    this.promoMerchantCreateRequest
        .setPromoStart(PromoMerchantRequestConverterBeanTest.PROMO_START);
    this.promoMerchantCreateRequest.setPromoEnd(PromoMerchantRequestConverterBeanTest.PROMO_END);
    this.promoMerchantCreateRequest
        .setPromoQuota(PromoMerchantRequestConverterBeanTest.PROMO_QUOTA);
    this.promoMerchantCreateRequest.setPromoRuleComboSet(this.promoMerchantRuleComboRequestSet);
    this.promoMerchantCreateRequest
        .setPromoRuleWholesaleSet(this.promoMerchantRuleWholesaleRequestSet);
    this.promoMerchantUpdateRequest = new PromoMerchantUpdateRequest();
    this.promoMerchantUpdateRequest.setItemSku(PromoMerchantRequestConverterBeanTest.ITEM_SKU);
    this.promoMerchantUpdateRequest.setPromoType(PromoMerchantRequestConverterBeanTest.PROMO_TYPE);
    this.promoMerchantUpdateRequest.setPromoId(PromoMerchantRequestConverterBeanTest.PROMO_ID);
    this.promoMerchantUpdateRequest.setPromoName(PromoMerchantRequestConverterBeanTest.PROMO_NAME);
    this.promoMerchantUpdateRequest
        .setPromoStart(PromoMerchantRequestConverterBeanTest.PROMO_START);
    this.promoMerchantUpdateRequest.setPromoEnd(PromoMerchantRequestConverterBeanTest.PROMO_END);
    this.promoMerchantUpdateRequest
        .setPromoQuota(PromoMerchantRequestConverterBeanTest.PROMO_QUOTA);
    this.promoMerchantUpdateRequest.setPromoRuleComboSet(this.promoMerchantRuleComboRequestSet);
    this.promoMerchantUpdateRequest
        .setPromoRuleWholesaleSet(this.promoMerchantRuleWholesaleRequestSet);

    this.promoStatusSet = new HashSet<>();
    this.promoStatusSet.add(PromoMerchantRequestConverterBeanTest.PROMO_STATUS);
    this.promoMerchantSummaryRequest = new PromoMerchantSummaryRequest();
    this.promoMerchantSummaryRequest.setItemSku(PromoMerchantRequestConverterBeanTest.ITEM_SKU);
    this.promoMerchantSummaryRequest.setPromoId(PromoMerchantRequestConverterBeanTest.PROMO_ID);
    this.promoMerchantSummaryRequest.setPromoType(PromoMerchantRequestConverterBeanTest.PROMO_TYPE);
    this.promoMerchantSummaryRequest.setPromoName(PromoMerchantRequestConverterBeanTest.PROMO_NAME);
    this.promoMerchantSummaryRequest.setPromoStatusSet(this.promoStatusSet);
  }

  @AfterEach
  public void _finalize() {}

  @Test
  public void testConvertToPromoMerchant_promoMerchantCreateRequest() {
    PromoMerchant result = this.promoMerchantRequestConverterBean
        .convertToPromoMerchant(this.promoMerchantCreateRequest);
    assertNotNull(result);
    assertEquals(PromoMerchantRequestConverterBeanTest.ITEM_SKU, result.getItemSku());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_TYPE, result.getPromoType());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_NAME, result.getPromoName());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_START, result.getPromoStart());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_END, result.getPromoEnd());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_QUOTA, result.getPromoQuota());
    assertNotNull(result.getPromoRuleComboSet());
    PromoMerchantRuleCombo comboRuleResult = result.getPromoRuleComboSet().iterator().next();
    assertEquals(PromoMerchantRequestConverterBeanTest.COMBO_MAIN_SKU, comboRuleResult.isMainSku());
    assertEquals(PromoMerchantRequestConverterBeanTest.ITEM_SKU, comboRuleResult.getItemSku());
    assertEquals(PromoMerchantRequestConverterBeanTest.COMBO_QTY, comboRuleResult.getQty());
    assertEquals(PromoMerchantRequestConverterBeanTest.COMBO_DISC_PERCENTAGE,
        comboRuleResult.getDiscPercentage(), 0);
    assertNotNull(result.getPromoRuleWholesaleSet());
    PromoMerchantRuleWholesale wholesaleRuleResult =
        result.getPromoRuleWholesaleSet().iterator().next();
    assertEquals(PromoMerchantRequestConverterBeanTest.WHOLESALE_MIN_QTY,
        wholesaleRuleResult.getMinQty());
    assertEquals(PromoMerchantRequestConverterBeanTest.WHOLESALE_MAX_QTY,
        wholesaleRuleResult.getMaxQty());
    assertEquals(PromoMerchantRequestConverterBeanTest.WHOLESALE_DISC_PERCENTAGE,
        wholesaleRuleResult.getDiscPercentage(), 0);
  }

  @Test
  public void testConvertToPromoMerchant_promoMerchantCreateRequest_comboAndWholesaleNull() {
    this.promoMerchantCreateRequest.setPromoRuleComboSet(null);
    this.promoMerchantCreateRequest.setPromoRuleWholesaleSet(null);
    PromoMerchant result = this.promoMerchantRequestConverterBean
        .convertToPromoMerchant(this.promoMerchantCreateRequest);
    assertNotNull(result);
    assertEquals(PromoMerchantRequestConverterBeanTest.ITEM_SKU, result.getItemSku());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_TYPE, result.getPromoType());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_NAME, result.getPromoName());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_START, result.getPromoStart());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_END, result.getPromoEnd());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_QUOTA, result.getPromoQuota());
    assertNull(result.getPromoRuleComboSet());
    assertNull(result.getPromoRuleWholesaleSet());
  }

  @Test
  public void testConvertToPromoMerchant_promoMerchantUpdateRequest() {
    PromoMerchant result = this.promoMerchantRequestConverterBean
        .convertToPromoMerchant(this.promoMerchantUpdateRequest);
    assertNotNull(result);
    assertEquals(PromoMerchantRequestConverterBeanTest.ITEM_SKU, result.getItemSku());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_ID, result.getPromoId());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_TYPE, result.getPromoType());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_NAME, result.getPromoName());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_START, result.getPromoStart());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_END, result.getPromoEnd());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_QUOTA, result.getPromoQuota());
    assertNotNull(result.getPromoRuleComboSet());
    PromoMerchantRuleCombo comboRuleResult = result.getPromoRuleComboSet().iterator().next();
    assertEquals(PromoMerchantRequestConverterBeanTest.COMBO_MAIN_SKU, comboRuleResult.isMainSku());
    assertEquals(PromoMerchantRequestConverterBeanTest.ITEM_SKU, comboRuleResult.getItemSku());
    assertEquals(PromoMerchantRequestConverterBeanTest.COMBO_QTY, comboRuleResult.getQty());
    assertEquals(PromoMerchantRequestConverterBeanTest.COMBO_DISC_PERCENTAGE,
        comboRuleResult.getDiscPercentage(), 0);
    assertNotNull(result.getPromoRuleWholesaleSet());
    PromoMerchantRuleWholesale wholesaleRuleResult =
        result.getPromoRuleWholesaleSet().iterator().next();
    assertEquals(PromoMerchantRequestConverterBeanTest.WHOLESALE_MIN_QTY,
        wholesaleRuleResult.getMinQty());
    assertEquals(PromoMerchantRequestConverterBeanTest.WHOLESALE_MAX_QTY,
        wholesaleRuleResult.getMaxQty());
    assertEquals(PromoMerchantRequestConverterBeanTest.WHOLESALE_DISC_PERCENTAGE,
        wholesaleRuleResult.getDiscPercentage(), 0);
  }

  @Test
  public void testConvertToPromoMerchantSummaryFilter_promoMerchantSummaryRequest() {
    PromoMerchantSummaryFilter result = this.promoMerchantRequestConverterBean
        .convertToPromoMerchantSummaryFilter(this.promoMerchantSummaryRequest);
    assertNotNull(result);
    assertEquals(PromoMerchantRequestConverterBeanTest.ITEM_SKU, result.getItemSku());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_ID, result.getPromoId());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_TYPE, result.getPromoType());
    assertEquals(PromoMerchantRequestConverterBeanTest.PROMO_NAME, result.getPromoName());
    assertEquals(this.promoStatusSet, result.getPromoStatusSet());
  }
}
