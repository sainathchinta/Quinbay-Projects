package com.gdn.mta.bulk.service;

import java.util.List;

import com.gdn.mta.bulk.dto.BulkBasicInfoRequest;
import com.gdn.mta.bulk.dto.product.BulkBasicInfoVideoDownloadResponseModel;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;

public interface BulkBasicInfoUpdateService {

/**
 * Pre-processes a bulk basic information update request by creating a bulk process and publishing an event.
 *
 * @param storeId String
 * @param requestId String
 * @param bulkBasicInfoRequest The request containing bulk basic information to be processed,
 *                            including business partner code, file name and trusted seller status **/
  void preProcessBulkBasicInfoUpdate(String storeId, String requestId, BulkBasicInfoRequest bulkBasicInfoRequest);

  /**
   * process bulk basic info update
   * @param bulkUpdateEventModel bulk update event model containing the details of the bulk update
   * @throws Exception error during processing
   */
  void processBulkBasicInfoUpdate(BulkUpdateEventModel bulkUpdateEventModel) throws Exception;

  /**
   * Processes a bulk basic information update request
   *
   * @param bulkBasicInfoRequest The request containing bulk basic information to be processed,
   *                            including business partner code, file name and trusted seller status
  **/
  void processBulkUpdate(BulkBasicInfoRequest bulkBasicInfoRequest);

  /**
   * Sets the final status of a bulk video process after receiving the download response from Kafka.
   *
   * @param storeId The unique identifier of the store
   * @param bulkBasicInfoVideoDownloadResponseModel The response model containing video download results
   *        including video ID, source URL, error details, and additional metadata
   */
  void processBulkProcessVideoUpdate(String storeId,
      BulkBasicInfoVideoDownloadResponseModel bulkBasicInfoVideoDownloadResponseModel);

  /**
   * Processes downloading images
   *
   * @param bulkProcessCode String
   * @param imageDownloadList List<String>
   *
   **/
  void downloadImages(String bulkProcessCode, List<String> imageDownloadList);
}
