package com.gdn.mta.bulk.service.download;

import com.gdn.mta.bulk.dto.TaggedProductFilterRequest;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;

/**
 * Created by keshashah on 24/10/16.
 */
public interface BulkProcessDownloadService {
  /**
   * API to download All records
   *
   * @param request
   */
  void downloadAll(BulkDownloadRequest request) throws Exception;

  /**
   * To create a excel file if not exist
   *
   * @param request
   * @param value
   * @throws Exception
   */
  void downloadExcelFile(BulkDownloadRequest request, String value) throws Exception;

  /**
   * @param request
   * @param processType
   */
  String internalProcessFailedDownloadExcelFile(BulkDownloadRequest request, String processType) throws Exception;

  /**
   * Create excel if it doesn't exists , if exists override the file
   *
   * @param request
   * @throws Exception
   */
  String downloadAndOverwriteExcelFile(BulkDownloadRequest request) throws Exception;

  /**
   * API to process download in batches
   * @param storeId
   */
  void processDownload(String storeId) throws Exception;

  /**
   * process download tagged products
   *
   * @param taggedProductFilterRequest
   * @throws Exception
   */
  void processDownloadTaggedProducts(TaggedProductFilterRequest taggedProductFilterRequest) throws Exception;
}
