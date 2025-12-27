package com.gdn.partners.pbp.rest.converter;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.mta.product.util.BeanUtils;
import org.springframework.stereotype.Component;

import com.gdn.partners.pbp.dto.promo.request.PromoMerchantCreateRequest;
import com.gdn.partners.pbp.dto.promo.request.PromoMerchantRuleComboRequest;
import com.gdn.partners.pbp.dto.promo.request.PromoMerchantRuleWholesaleRequest;
import com.gdn.partners.pbp.dto.promo.request.PromoMerchantSummaryRequest;
import com.gdn.partners.pbp.dto.promo.request.PromoMerchantUpdateRequest;
import com.gdn.partners.pbp.model.promo.PromoMerchant;
import com.gdn.partners.pbp.model.promo.PromoMerchantRuleCombo;
import com.gdn.partners.pbp.model.promo.PromoMerchantRuleWholesale;
import com.gdn.partners.pbp.model.promo.PromoMerchantSummaryFilter;

@Component
public class PromoMerchantRequestConverterBean implements PromoMerchantRequestConverter {

  /**
   * Convert source to a new object
   *
   * @param source should be instanceof {@link PromoMerchantCreateRequest} or
   *        {@link PromoMerchantUpdateRequest}
   * @param comboRequestSet a set of {@link PromoMerchantRuleComboRequest}
   * @param wholesaleRequestSet a set of {@link PromoMerchantRuleWholesaleRequest}
   * @return a new object with class PromoMerchant
   */
  private PromoMerchant convertToPromoMerchant(Object source,
      Set<PromoMerchantRuleComboRequest> comboRequestSet,
      Set<PromoMerchantRuleWholesaleRequest> wholesaleRequestSet) {
    List<PromoMerchantRuleCombo> comboList = null;
    if (comboRequestSet != null) {
      comboList = comboRequestSet.stream().map(combo -> this.convertToPromoMerchantRuleCombo(combo))
          .collect(Collectors.toList());
    }
    List<PromoMerchantRuleWholesale> wholesaleList = null;
    if (wholesaleRequestSet != null) {
      wholesaleList = wholesaleRequestSet.stream()
          .map(wholesale -> this.convertToPromoMerchantRuleWholesale(wholesale))
          .collect(Collectors.toList());
    }
    PromoMerchant target = PromoMerchant.builder().promoRuleComboSet(comboList)
        .promoRuleWholesaleSet(wholesaleList).build();
    BeanUtils.copyProperties(source, target, "promoRuleComboSet", "promoRuleWholesaleSet");
    return target;
  }

  @Override
  public PromoMerchant convertToPromoMerchant(PromoMerchantCreateRequest source) {
    return this.convertToPromoMerchant(source, source.getPromoRuleComboSet(),
        source.getPromoRuleWholesaleSet());
  }

  @Override
  public PromoMerchant convertToPromoMerchant(PromoMerchantUpdateRequest source) {
    return this.convertToPromoMerchant(source, source.getPromoRuleComboSet(),
        source.getPromoRuleWholesaleSet());
  }

  private PromoMerchantRuleCombo convertToPromoMerchantRuleCombo(
      PromoMerchantRuleComboRequest source) {
    PromoMerchantRuleCombo target = new PromoMerchantRuleCombo();
    BeanUtils.copyProperties(source, target);
    return target;
  }

  private PromoMerchantRuleWholesale convertToPromoMerchantRuleWholesale(
      PromoMerchantRuleWholesaleRequest source) {
    PromoMerchantRuleWholesale target = new PromoMerchantRuleWholesale();
    BeanUtils.copyProperties(source, target);
    return target;
  }

  @Override
  public PromoMerchantSummaryFilter convertToPromoMerchantSummaryFilter(
      PromoMerchantSummaryRequest source) {
    PromoMerchantSummaryFilter target = new PromoMerchantSummaryFilter();
    BeanUtils.copyProperties(source, target);
    return target;
  }
}
