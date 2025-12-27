package com.gdn.mta.bulk.service;

import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.mta.bulk.dto.BulkDownloadFileContentDTO;
import com.gdn.mta.bulk.dto.BulkDownloadMailRecipient;
import com.gdn.mta.bulk.dto.BulkDownloadProductDTO;
import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.CampaignBulkDownloadRequest;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadDTO;
import com.gdn.mta.bulk.entity.BulkDownloadQueue;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by virajjasani on 30/08/16.
 */
public interface BulkDownloadService {

  /**
   * Pre processing of bulk download product request before sending data to queue
   *
   * @param requestId
   * @param privilegedMap
   * @param businessPartnerCode
   * @param productSize
   * @param request
   * @param username
   * @param bulkDownloadMailRecipient
   * @throws Exception
   */
  void preProcess(String requestId, Map<String, Boolean> privilegedMap, String businessPartnerCode,
      Integer productSize, ProductLevel3SummaryRequest request, String username, BulkDownloadMailRecipient bulkDownloadMailRecipient)
      throws Exception;

  /**
   * Post processing of bulk download product request after data retrieval from queue
   *
   * @param bulkDownloadQueue
   * @throws Exception
   */
  void postProcess(BulkDownloadQueue bulkDownloadQueue) throws Exception;

  /**
   * get BulkDownloadEntity info by requestId
   *
   * @param requestId
   * @return
   * @throws Exception
   */
  BulkDownloadProductDTO getBulkDownloadProductByRequestId(String requestId) throws Exception;

  BulkDownloadProductDTO getBulkDownloadProduct(String requestId, String status)
      throws Exception;

  /**
   * Service method to generate file contents for bulk download product file
   *
   * @param fileId
   * @return
   */
  BulkDownloadFileContentDTO getFileContents(String fileId);

  /**
   * API to get file contents from request
   *
   * @param bulkDownloadProductDTO
   * @return
   */
  BulkDownloadFileContentDTO getFileContents(BulkDownloadProductDTO bulkDownloadProductDTO);

  /**
   * Service method to generate file contents of bulk Process(update/upload) product file
   *
   * @param storeId
   * @param bulkProcessCode
   * @return BulkDownloadFileContentDTO
   */

  BulkDownloadFileContentDTO getBulkProcessProductFile(String storeId, String bulkProcessCode) throws Exception;

  /**
   * Service method to download unified bulk template
   *
   * @param storeId
   * @param requestId
   * @param businessPartnerCode
   * @param pickupPointCodes
   * @return
   */
  UnifiedBulkDownloadDTO downloadProductUnifiedTemplate(String storeId, String requestId, String businessPartnerCode,
      Set<String> pickupPointCodes) throws  Exception;


  /**
   * @param bulkProcessCode bulk process code
   * @param productContentsForXLFile content
   * @return
   */
  String generateWholeSaleErrorWorkbookBulkDownload(String bulkProcessCode,
    List<List<String>> productContentsForXLFile)
      throws Exception;

  /**
   * @param bulkProcessCode bulk process code
   * @param productContentsForXLFile content
   * @return
   */
  String generateEanErrorWorkBookBulkDownload(String bulkProcessCode,
      List<List<String>> productContentsForXLFile)
      throws Exception;

  /**
   *
   * @param storeId
   * @param businessPartnerCode
   * @param request
   * @return
   */
  long countNumberOfCampaignDownloads(String storeId, String businessPartnerCode, CampaignBulkDownloadRequest request)
      throws Exception;

  /**
   * Get counts based on bp code , username and download type
   *
   * @param businessPartnerCode
   * @param username
   * @param downloadType
   * @return
   */
  BulkInternalPendingRequestResponse countPendingRequestsByUsernameAndDownloadType(String businessPartnerCode, String username, String downloadType)
      throws Exception;

  /**
   * Clear in progress downloads
   *  @param storeId
   * @param entityType
   * @param status
   */
  void clearInProgressDownloads(String storeId, String entityType, String status);
}
