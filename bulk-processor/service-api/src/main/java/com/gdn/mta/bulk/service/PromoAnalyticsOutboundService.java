package com.gdn.mta.bulk.service;

import com.gdn.partners.promo.analytics.web.model.request.LowestPriceRecommendationRequest;
import com.gdn.partners.promo.analytics.web.model.response.LowestPriceRecommendationResponse;

public interface PromoAnalyticsOutboundService {

  LowestPriceRecommendationResponse getLowestPriceRecommendation(LowestPriceRecommendationRequest request);

  LowestPriceRecommendationResponse getLowestPriceRecommendationV2(LowestPriceRecommendationRequest request);
}
