package com.gdn.mta.bulk.service;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.PromoAnalyticsFeign;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.promo.analytics.web.model.request.LowestPriceRecommendationRequest;
import com.gdn.partners.promo.analytics.web.model.response.LowestPriceRecommendationL5Response;
import com.gdn.partners.promo.analytics.web.model.response.LowestPriceRecommendationResponse;
import com.gdn.partners.promo.analytics.web.model.response.LowestPriceResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PromoAnalyticsOutboundServiceImpl implements PromoAnalyticsOutboundService {

  @Autowired
  PromoAnalyticsFeign promoAnalyticsFeign;

  @Override
  public LowestPriceRecommendationResponse getLowestPriceRecommendation(LowestPriceRecommendationRequest request) {
    GdnRestSingleResponse<LowestPriceRecommendationResponse> response = promoAnalyticsFeign
        .getLowestPriceRecommendation(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, request);
    if (!response.isSuccess()) {
      log.error("Exception while getting lowest price recommendation for request : {}", request);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return response.getValue();
  }

  @Override
  public LowestPriceRecommendationResponse getLowestPriceRecommendationV2(LowestPriceRecommendationRequest request) {
    GdnRestListResponse<LowestPriceRecommendationL5Response> response = promoAnalyticsFeign
        .getLowestPriceRecommendationV2(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, request);
    LowestPriceRecommendationResponse lowestPriceRecommendationResponse = new LowestPriceRecommendationResponse();
    Map<String, LowestPriceResponse> L5CodeLowestPriceMap = new HashMap<>();
    if (!response.isSuccess()) {
      log.error("Exception while getting lowest price recommendation for request : {}", request);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    if (CollectionUtils.isNotEmpty(response.getContent())) {
      for (LowestPriceRecommendationL5Response l5Response : response.getContent()) {
        LowestPriceResponse lowestPriceResponse = new LowestPriceResponse();
        lowestPriceResponse.setLowestPrice(Optional.ofNullable(l5Response.getLowestPrice()).orElse(Double.MAX_VALUE));
        lowestPriceResponse.setFsRecommended(l5Response.isRecommended());
        L5CodeLowestPriceMap.put(l5Response.getItemSku() + Constant.HYPHEN + l5Response.getPickUpPointCode(), lowestPriceResponse);
      }
    }
    lowestPriceRecommendationResponse.setItemSkuLowestPriceMap(L5CodeLowestPriceMap);
    return lowestPriceRecommendationResponse;
  }

}
