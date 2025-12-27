package com.gdn.partners.pbp.rest.converter;

import java.util.List;
import java.util.stream.Collectors;

import com.gdn.mta.product.util.BeanUtils;
import org.springframework.stereotype.Component;

import com.gdn.partners.pbp.dto.promo.response.PromoMerchantCountResponse;
import com.gdn.partners.pbp.dto.promo.response.PromoMerchantDetailResponse;
import com.gdn.partners.pbp.dto.promo.response.PromoMerchantHistoryResponse;
import com.gdn.partners.pbp.dto.promo.response.PromoMerchantInfoInventoryResponse;
import com.gdn.partners.pbp.dto.promo.response.PromoMerchantInfoItemResponse;
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

@Component
public class PromoMerchantResponseConverterBean implements PromoMerchantResponseConverter {

  @Override
  public PromoMerchantCountResponse convertToPromoMerchantCountResponse(PromoMerchantCount source) {
    PromoMerchantCountResponse target = new PromoMerchantCountResponse();
    BeanUtils.copyProperties(source, target);
    return target;
  }

  @Override
  public PromoMerchantDetailResponse convertToPromoMerchantDetailResponse(PromoMerchant source) {
    List<PromoMerchantRuleComboResponse> promoRuleComboList = null;
    if (source.getPromoRuleComboSet() != null) {
      promoRuleComboList = source.getPromoRuleComboSet().stream()
          .map(combo -> this.convertToPromoMerchantRuleComboResponse(combo))
          .collect(Collectors.toList());
    }
    List<PromoMerchantRuleWholesaleResponse> promoRuleWholesaleList = null;
    if (source.getPromoRuleWholesaleSet() != null) {
      promoRuleWholesaleList = source.getPromoRuleWholesaleSet().stream()
          .map(wholesale -> this.convertToPromoMerchantRuleWholesaleResponse(wholesale))
          .collect(Collectors.toList());
    }
    PromoMerchantDetailResponse target = new PromoMerchantDetailResponse();
    BeanUtils.copyProperties(source, target, "promoRuleComboSet", "promoRuleWholesaleSet",
        "infoItem", "infoInventory");
    target.setPromoRuleComboSet(promoRuleComboList);
    target.setPromoRuleWholesaleSet(promoRuleWholesaleList);
    target.setInfoItem(this.convertToPromoMerchantInfoItemResponse(source.getInfoItem()));
    target.setInfoInventory(
        this.convertToPromoMerchantInfoInventoryResponse(source.getInfoInventory()));
    return target;
  }

  @Override
  public PromoMerchantHistoryResponse convertToPromoMerchantHistoryResponse(
      PromoMerchantHistory source) {
    PromoMerchantHistoryResponse target = new PromoMerchantHistoryResponse();
    BeanUtils.copyProperties(source, target);
    return target;
  }

  private PromoMerchantInfoInventoryResponse convertToPromoMerchantInfoInventoryResponse(
      PromoMerchantInfoInventory source) {
    PromoMerchantInfoInventoryResponse target = new PromoMerchantInfoInventoryResponse();
    BeanUtils.copyProperties(source, target);
    return target;
  }

  private PromoMerchantInfoItemResponse convertToPromoMerchantInfoItemResponse(
      PromoMerchantInfoItem source) {
    PromoMerchantInfoItemResponse target = new PromoMerchantInfoItemResponse();
    BeanUtils.copyProperties(source, target);
    return target;
  }

  private PromoMerchantRuleComboResponse convertToPromoMerchantRuleComboResponse(
      PromoMerchantRuleCombo source) {
    PromoMerchantRuleComboResponse target = new PromoMerchantRuleComboResponse();
    BeanUtils.copyProperties(source, target, "infoItem", "infoInventory");
    target.setInfoItem(this.convertToPromoMerchantInfoItemResponse(source.getInfoItem()));
    target.setInfoInventory(
        this.convertToPromoMerchantInfoInventoryResponse(source.getInfoInventory()));
    return target;
  }

  private PromoMerchantRuleWholesaleResponse convertToPromoMerchantRuleWholesaleResponse(
      PromoMerchantRuleWholesale source) {
    PromoMerchantRuleWholesaleResponse target = new PromoMerchantRuleWholesaleResponse();
    BeanUtils.copyProperties(source, target);
    return target;
  }

  @Override
  public PromoMerchantSummaryResponse convertToPromoMerchantSummaryResponse(PromoMerchant source) {
    PromoMerchantSummaryResponse target = new PromoMerchantSummaryResponse();
    BeanUtils.copyProperties(source, target, "infoInventory");
    target.setInfoInventory(
        this.convertToPromoMerchantInfoInventoryResponse(source.getInfoInventory()));
    return target;
  }
}
