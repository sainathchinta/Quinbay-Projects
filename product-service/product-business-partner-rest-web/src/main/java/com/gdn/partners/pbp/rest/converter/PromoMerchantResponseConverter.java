package com.gdn.partners.pbp.rest.converter;

import com.gdn.partners.pbp.dto.promo.response.PromoMerchantCountResponse;
import com.gdn.partners.pbp.dto.promo.response.PromoMerchantDetailResponse;
import com.gdn.partners.pbp.dto.promo.response.PromoMerchantHistoryResponse;
import com.gdn.partners.pbp.dto.promo.response.PromoMerchantSummaryResponse;
import com.gdn.partners.pbp.model.promo.PromoMerchant;
import com.gdn.partners.pbp.model.promo.PromoMerchantCount;
import com.gdn.partners.pbp.model.promo.PromoMerchantHistory;

public interface PromoMerchantResponseConverter {

  PromoMerchantCountResponse convertToPromoMerchantCountResponse(PromoMerchantCount source);

  PromoMerchantDetailResponse convertToPromoMerchantDetailResponse(PromoMerchant source);

  PromoMerchantHistoryResponse convertToPromoMerchantHistoryResponse(PromoMerchantHistory source);

  PromoMerchantSummaryResponse convertToPromoMerchantSummaryResponse(PromoMerchant source);
}
