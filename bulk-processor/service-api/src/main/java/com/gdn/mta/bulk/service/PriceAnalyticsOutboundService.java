package com.gdn.mta.bulk.service;

import java.util.List;
import java.util.Set;

import com.gdn.mta.bulk.dto.TaggedProductFilterRequest;
import com.gdn.mta.bulk.dto.TaggedProductFilterResponse;
import com.gdn.mta.bulk.models.SkuRebateUpdateRequest;
import com.gdn.mta.bulk.models.download.BulkPriceRecommendationDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkRebateUpdateRequest;
import com.gdn.mta.bulk.models.download.UpdateRemoveProductTaggingRequest;
import com.gdn.mta.bulk.models.download.responsedata.DownloadSkuResponse;
import com.gdn.mta.bulk.models.download.responsedata.FailedReasonResponse;

public interface PriceAnalyticsOutboundService {

  /**
   * get data for price recommendation using brEmail Address
   *
   * @param brEmailAddress
   * @param bulkPriceRecommendationDownloadRequest
   * @return
   */
  List<DownloadSkuResponse> getDownloadSkus(String brEmailAddress,
      BulkPriceRecommendationDownloadRequest bulkPriceRecommendationDownloadRequest);

  /**
   * get tagged products
   *
   * @param taggedProductFilterRequest
   * @return
   */
  List<TaggedProductFilterResponse> getDownloadTaggedProducts(TaggedProductFilterRequest taggedProductFilterRequest);


  /**
   *
   * @param bulkRebateUpdateRequest
   * @param officerEmailAddress
   * @return
   */
  String updatePriceRebate(BulkRebateUpdateRequest bulkRebateUpdateRequest, String officerEmailAddress);

  /**
   * update Tagging at L5
   *
   * @param officerEmailAddress not null
   * @param updateRemoveProductTaggingRequest list of L5 to be updated
   * @return List<FailedReasonResponse>
   */
  List<FailedReasonResponse> updateTagging(String officerEmailAddress,
    List<UpdateRemoveProductTaggingRequest> updateRemoveProductTaggingRequest);

  /**
   * remove Tagging at L5
   *
   * @param officerEmailAddress not null
   * @param updateRemoveProductTaggingRequest list of L5 to be removed
   * @return List<FailedReasonResponse>
   */
  List<FailedReasonResponse> removeTagging(String officerEmailAddress,
    List<UpdateRemoveProductTaggingRequest> updateRemoveProductTaggingRequest);

  /**
   * Update sku level rebate
   *
   * @param skuRebateUpdateRequest
   * @return
   */
  String updateSkuRebate(SkuRebateUpdateRequest skuRebateUpdateRequest);

  /**
   * Get skus tagged to officer username
   *
   * @param offlineItemIds
   * @return
   */
  Set<String> getOfficerTaggedSkus(Set<String> offlineItemIds);
}
