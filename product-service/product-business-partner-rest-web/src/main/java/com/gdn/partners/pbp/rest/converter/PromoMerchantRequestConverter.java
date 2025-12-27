package com.gdn.partners.pbp.rest.converter;

import com.gdn.partners.pbp.dto.promo.request.PromoMerchantCreateRequest;
import com.gdn.partners.pbp.dto.promo.request.PromoMerchantSummaryRequest;
import com.gdn.partners.pbp.dto.promo.request.PromoMerchantUpdateRequest;
import com.gdn.partners.pbp.model.promo.PromoMerchant;
import com.gdn.partners.pbp.model.promo.PromoMerchantSummaryFilter;

public interface PromoMerchantRequestConverter {

  PromoMerchant convertToPromoMerchant(PromoMerchantCreateRequest source);

  PromoMerchant convertToPromoMerchant(PromoMerchantUpdateRequest source);

  PromoMerchantSummaryFilter convertToPromoMerchantSummaryFilter(
      PromoMerchantSummaryRequest source);
}
