package com.gdn.mta.bulk.repository;

import com.gdn.mta.bulk.models.download.ClusterReviewFeedbackRequest;
import com.gdn.mta.bulk.models.download.DownloadItemsRequest;
import com.gdn.mta.bulk.models.download.responsedata.ClusterActionResponse;
import com.gdn.mta.bulk.models.download.responsedata.ItemsDownloadResponse;

public interface MasterSkuItemsRepository {

  /**
   * to fetch items for master  sku review
   *
   * @param request DownloadItemsRequest
   * @return ItemsDownloadResponse
   * @throws Exception Exception
   */
  ItemsDownloadResponse getItemsForMasterSkuReview(DownloadItemsRequest request) throws Exception;

  /**
   * perform bulk master sku review
   *
   * @param masterSku                    masterSku
   * @param clusterReviewFeedbackRequest clusterReviewFeedbackRequest
   * @param username uploaded username
   * @return ClusterActionResponse
   * @throws Exception Exception
   */
  ClusterActionResponse performClusterAction(String masterSku,
      ClusterReviewFeedbackRequest clusterReviewFeedbackRequest, String username) throws Exception;
}
