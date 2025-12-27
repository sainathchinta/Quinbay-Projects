package com.gdn.x.mta.distributiontask.dao.util;

import static com.gdn.x.mta.distributiontask.model.Constants.APPEALED_PRODUCT_FALSE_MULTIPLIER;
import static com.gdn.x.mta.distributiontask.model.Constants.APPEALED_PRODUCT_TRUE_BOOST;
import static com.gdn.x.mta.distributiontask.model.Constants.POST_LIVE_FALSE_BOOST;
import static com.gdn.x.mta.distributiontask.model.Constants.POST_LIVE_TRUE;
import static com.gdn.x.mta.distributiontask.model.Constants.PRODUCT_REVIEW_TYPE_0;
import static com.gdn.x.mta.distributiontask.model.Constants.PRODUCT_REVIEW_TYPE_2;
import static com.gdn.x.mta.distributiontask.model.Constants.PRODUCT_REVIEW_TYPE_BOOST;
import static com.gdn.x.mta.distributiontask.model.Constants.SELLER_BADGE_0;
import static com.gdn.x.mta.distributiontask.model.Constants.SELLER_BADGE_1;
import static com.gdn.x.mta.distributiontask.model.Constants.SELLER_BADGE_2;
import static com.gdn.x.mta.distributiontask.model.Constants.SELLER_BADGE_3;
import static com.gdn.x.mta.distributiontask.model.Constants.SELLER_BADGE_4;
import static com.gdn.x.mta.distributiontask.model.Constants.SELLER_BADGE_BOOST;
import com.gdn.x.mta.distributiontask.model.BoostConfig;
import com.gdn.x.mta.distributiontask.model.enums.VendorProductSolrFieldNames;
import com.gdn.x.mta.distributiontask.model.solr.SolrConstants;
import com.gdn.x.mta.distributiontask.model.type.ProductReviewType;
import org.apache.solr.client.solrj.SolrQuery;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CustomBoostQueryBuilder {
  private final BoostConfig boostConfig;

  @Autowired
  public CustomBoostQueryBuilder(BoostConfig boostConfig) {
    this.boostConfig = boostConfig;
  }

  public SolrQuery getBoostQueryForAutoAssignment(boolean appealProductEnabled) {
    SolrQuery solrQuery = new SolrQuery();
    buildBoostQuery(solrQuery, appealProductEnabled);
    return solrQuery;
  }

  private void buildBoostQuery(SolrQuery solrQuery, boolean appealProductEnabled) {
    solrQuery.set(SolrConstants.DEF_TYPE, SolrConstants.EDISMAX);
    solrQuery.add(SolrConstants.QUERY,
      VendorProductSolrFieldNames.ASSIGNED + SolrConstants.COLON + Boolean.FALSE);
    addPostLiveBoost(solrQuery);
    addReviewTypeBoost(solrQuery);
    //Add appeal product boost only if switch enabled
    if (appealProductEnabled) {
      addAppealedProductBoost(solrQuery);
    }
    addSellerBadgeBoost(solrQuery);

  }

  private void addPostLiveBoost(SolrQuery solrQuery) {
    float postLiveFalseBoost = boostConfig.getBoostValueMap().get(POST_LIVE_FALSE_BOOST);
    float postLiveTrueMultiplier = boostConfig.getBoostMultiplierMap().get(POST_LIVE_TRUE);

    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.POST_LIVE + SolrConstants.COLON + Boolean.FALSE
        + SolrConstants.CARET + postLiveFalseBoost);

    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.POST_LIVE + SolrConstants.COLON + Boolean.TRUE
        + SolrConstants.CARET + postLiveFalseBoost * postLiveTrueMultiplier);
  }

  private void addReviewTypeBoost(SolrQuery solrQuery) {
    float productReviewTypeBoost = boostConfig.getBoostValueMap().get(PRODUCT_REVIEW_TYPE_BOOST);
    float productReview0Multiplier = boostConfig.getBoostMultiplierMap().get(PRODUCT_REVIEW_TYPE_0);
    float productReview2Multiplier = boostConfig.getBoostMultiplierMap().get(PRODUCT_REVIEW_TYPE_2);

    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON
        + ProductReviewType.EDITED.getValue() + SolrConstants.CARET + productReviewTypeBoost);

    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON
        + ProductReviewType.REVISED.getValue() + SolrConstants.CARET
        + productReviewTypeBoost * productReview2Multiplier);

    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON
        + ProductReviewType.NEWLY_ADDED.getValue() + SolrConstants.CARET
        + productReviewTypeBoost * productReview0Multiplier);
  }

  private void addSellerBadgeBoost(SolrQuery solrQuery) {
    float sellerBadgeBoost = boostConfig.getBoostValueMap().get(SELLER_BADGE_BOOST);
    float sellerBadge0Multiplier = boostConfig.getBoostMultiplierMap().get(SELLER_BADGE_0);
    float sellerBadge4Multiplier = boostConfig.getBoostMultiplierMap().get(SELLER_BADGE_4);
    float sellerBadge3Multiplier = boostConfig.getBoostMultiplierMap().get(SELLER_BADGE_3);
    float sellerBadge2Multiplier = boostConfig.getBoostMultiplierMap().get(SELLER_BADGE_2);
    float sellerBadge1Multiplier = boostConfig.getBoostMultiplierMap().get(SELLER_BADGE_1);

    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.SELLER_BADGE + SolrConstants.COLON + "5" + SolrConstants.CARET
        + sellerBadgeBoost);
    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.SELLER_BADGE + SolrConstants.COLON + "0" + SolrConstants.CARET
        + sellerBadgeBoost * sellerBadge0Multiplier);
    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.SELLER_BADGE + SolrConstants.COLON + "4" + SolrConstants.CARET
        + sellerBadgeBoost * sellerBadge4Multiplier);
    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.SELLER_BADGE + SolrConstants.COLON + "3" + SolrConstants.CARET
        + sellerBadgeBoost * sellerBadge3Multiplier);
    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.SELLER_BADGE + SolrConstants.COLON + "2" + SolrConstants.CARET
        + sellerBadgeBoost * sellerBadge2Multiplier);
    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.SELLER_BADGE + SolrConstants.COLON + "1" + SolrConstants.CARET
        + sellerBadgeBoost * sellerBadge1Multiplier);
  }

  private void addAppealedProductBoost(SolrQuery solrQuery) {
    float appealedProductTrueBoost =
      boostConfig.getBoostValueMap().get(APPEALED_PRODUCT_TRUE_BOOST);
    float appealedProductMultiplier =
      boostConfig.getBoostMultiplierMap().get(APPEALED_PRODUCT_FALSE_MULTIPLIER);

    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.APPEALED_PRODUCT + SolrConstants.COLON + Boolean.TRUE
        + SolrConstants.CARET + appealedProductTrueBoost);

    solrQuery.add(SolrConstants.BOOSTED_QUERY,
      VendorProductSolrFieldNames.APPEALED_PRODUCT + SolrConstants.COLON + Boolean.FALSE
        + SolrConstants.CARET + appealedProductTrueBoost * appealedProductMultiplier);
  }
}
