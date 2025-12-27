package com.gdn.x.mta.distributiontask.model;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

@Component
public class BoostConfig {
    public final Map<String, Float> boostValueMap = new HashMap<>();
    public final Map<String, Float> boostMultiplierMap = new HashMap<>();

    public BoostConfig(
            @Value("${boost.postLive.false:10000}") float postLiveFalseBoost,
            @Value("${boost.ProductReviewType.edited:1000}") float productReviewTypeEdited,
            @Value("${boost.appealProduct:100}") float appealProductTrueBoost,
            @Value("${boost.sellerBadge:10}") float sellerBadgeBoost,
            @Value("${boost.multiplier.postLive.true:0.5}") float postLiveTrueMultiplier,
            @Value("${boost.multiplier.ProductReviewType.newlyAdded:0.25}") float productReviewTypeNewlyAddedMultiplier,
            @Value("${boost.multiplier.ProductReviewType.revised:0.5}") float productReviewTypeRevisedMultiplier,
            @Value("${boost.multiplier.sellerBadge.0:0.1}") float sellerBadge5Multiplier,
            @Value("${boost.multiplier.sellerBadge.4:0.75}") float sellerBadge4Multiplier,
            @Value("${boost.multiplier.sellerBadge.3:0.5}") float sellerBadge3Multiplier,
            @Value("${boost.multiplier.sellerBadge.2:0.25}") float sellerBadge2Multiplier,
            @Value("${boost.multiplier.sellerBadge.1:0.2}") float sellerBadge1Multiplier,
            @Value("${boost.multiplier.appealedProduct.false:0.5}") float appealedProductMultiplier
    ) {
        boostValueMap.put(Constants.POST_LIVE_FALSE_BOOST, postLiveFalseBoost);
        boostValueMap.put(Constants.SELLER_BADGE_BOOST, sellerBadgeBoost);
        boostValueMap.put(Constants.PRODUCT_REVIEW_TYPE_BOOST, productReviewTypeEdited);
        boostValueMap.put(Constants.APPEALED_PRODUCT_TRUE_BOOST, appealProductTrueBoost);
        boostMultiplierMap.put(Constants.POST_LIVE_TRUE, postLiveTrueMultiplier);
        boostMultiplierMap.put(Constants.PRODUCT_REVIEW_TYPE_0, productReviewTypeNewlyAddedMultiplier);
        boostMultiplierMap.put(Constants.PRODUCT_REVIEW_TYPE_2, productReviewTypeRevisedMultiplier);
        boostMultiplierMap.put(Constants.SELLER_BADGE_0, sellerBadge5Multiplier);
        boostMultiplierMap.put(Constants.SELLER_BADGE_4, sellerBadge4Multiplier);
        boostMultiplierMap.put(Constants.SELLER_BADGE_3, sellerBadge3Multiplier);
        boostMultiplierMap.put(Constants.SELLER_BADGE_2, sellerBadge2Multiplier);
        boostMultiplierMap.put(Constants.SELLER_BADGE_1, sellerBadge1Multiplier);
        boostMultiplierMap.put(Constants.APPEALED_PRODUCT_FALSE_MULTIPLIER, appealedProductMultiplier);
    }

    public Map<String, Float> getBoostValueMap() {
        return boostValueMap;
    }

    public Map<String, Float> getBoostMultiplierMap() {
        return boostMultiplierMap;
    }
}