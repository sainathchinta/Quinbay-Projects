package com.gdn.partners.pcu.external.service;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.gdn.mta.bulk.dto.BulkProcessResponse;
import com.gdn.partners.pcu.external.web.model.request.BulkBasicInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.response.BulkProcessStatusListingWebResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.mta.bulk.dto.SystemParameterConfigResponse;
import com.gdn.partners.pcu.external.web.model.response.BulkPendingRequestsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.PromoUpdateProductResponse;
import com.gdn.partners.pcu.external.web.model.response.UnifiedBulkDownloadWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesaleCountWebResponse;

public interface BulkProcessService {


  /**
   * Method to upload Bulk-update file
   *
   * @param businessPartnerCode
   * @param username
   * @param isOnlyExternalUser
   * @param request
   * @throws Exception
   */
  void uploadBulkUpdate(String businessPartnerCode, String username, boolean isOnlyExternalUser, MultipartFile request)
      throws IOException;

  /**
   * Method to upload Bulk-update-Ean file
   *
   * @param businessPartnerCode
   * @param username
   * @param request
   * @throws Exception
   */
  void uploadBulkUpdateEAN(String businessPartnerCode, String username, MultipartFile request)
      throws IOException;

  /**
   * Method to upload Bulk-update file
   *
   * @param businessPartnerCode
   * @param username
   * @param request
   * @throws Exception
   */
  void uploadBulkUpdateMasterInfo(String businessPartnerCode, String username, BulkBasicInfoWebRequest request)
      throws IOException;

  /**
   * Method to upload files
   *
   * @param businessPartnerCode
   * @param username
   * @param filenames
   * @throws Exception
   */
  String upload(String businessPartnerCode, String username, List<String> filenames) throws
      Exception;

  /**
   * Method to upload files V2
   *
   * @param businessPartnerCode
   * @param username
   * @param filenames
   * @param processType
   * @throws Exception
   */
  String uploadV2(String businessPartnerCode, String username, List<String> filenames,
    String processType)
      throws Exception;

  /**
   * Upload bulk Archive File
   * @param username
   * @param businessPartnerCode
   * @param multipartFile
   * @throws Exception
   */
  void uploadBulkUpdateForBulkArchive(String username, String businessPartnerCode, MultipartFile multipartFile) throws Exception;

  /**
   * Bulk upload InStore update
   *
   * @param username
   * @param businessPartnerCode
   * @param multipartFile
   * @throws Exception
   */
  void uploadBulkUpdateForInStoreUpdate(String username, String businessPartnerCode, MultipartFile multipartFile)
      throws Exception;

  /**
   * Fetch promo related bulk products
   *
   * @param bulkProcessCode
   * @return
   */
  List<PromoUpdateProductResponse> fetchPromoUpdatedProductNotes(String bulkProcessCode);


  WholesaleCountWebResponse fetchWholeSaleConfigCount(String bulkProcessCode);


  /**
   * Check if merchant has pending bulk upload requests
   *
   * @param type
   * @param businessPartnerCode
   * @param bulkProcessType
   * @return
   */
  BulkPendingRequestsWebResponse checkPendingBulkProcess(String type, String businessPartnerCode,
      String bulkProcessType);
  /**
   * Download unified product template
   *
   *
   * @param businessPartnerCode
   * @return
   */
  UnifiedBulkDownloadWebResponse downloadProductUnifiedTemplate(String businessPartnerCode) throws Exception;

  /**
   * get bulk system parameter configuration by variable name
   *
   *
   * @param variableName
   * @return
   */
  SystemParameterConfigResponse getBulkSystemParameterConfig(String variableName);

  /**
   * Bulk archive product sku
   * @param username
   * @param businessPartnerCode
   * @param multipartFile
   * @throws Exception
   */
  void uploadBulkUpdateForBulkArchiveProductSkus(String username, String businessPartnerCode,
      MultipartFile multipartFile) throws Exception;

  /**
   *
   * @param username
   * @param type
   * @param businessPartnerCode
   * @param multipartFile
   */
  void uploadBulkForWorkOrderCreation(String username, String type, String businessPartnerCode,
      MultipartFile multipartFile) throws Exception;

  /**
   * Upload bulk subject to vat skus
   *
   *  @param businessPartnerCode
   * @param fileName
   * @param bulkProcessCode
   */
  void uploadBulkSubjectToVatSkus(String businessPartnerCode, String fileName, String bulkProcessCode);

  /**
   * Bulk delete offline items
   *
   * @param businessPartnerCode
   * @param username
   * @param clientId
   * @param file
   * @throws Exception
   */
  void uploadBulkDeleteOfflineItems(String requestId, String businessPartnerCode, String username,
    String clientId, MultipartFile file) throws Exception;

  /**
   * Bulk upsert offline items
   *
   * @param businessPartnerCode
   * @param username
   * @param multipartFile
   * @param profileResponse must not be null
   * @throws Exception
   */
  void uploadForBulkUpsertOfflineItems(String username, String businessPartnerCode,
    ProfileResponse profileResponse, MultipartFile multipartFile)
      throws Exception;

  /**
   * Fetch Bulk Process listing Web response
   *
   * @param requestId           user
   * @param businessPartnerCode business Partner coded
   * @param bulkProcessType     creation, update and upsert
   * @param bulkProcessCodes    Optional Param bulk Process codes
   * @param estimationsNeeded   true if ETA calculations are to be performed
   * @param page                Int
   * @param size                Int
   */
  Page<BulkProcessStatusListingWebResponse> fetchBulkProcessListingWebResponse(String storeId,
    String requestId, String businessPartnerCode, String bulkProcessType,
    Optional<List<String>> bulkProcessCodes, boolean estimationsNeeded, Integer page,
    Integer size) throws Exception;

  /**
   * GET bulk process by process code
   * @param bulkProcessCode
   * @return
   */
  BulkProcessResponse getBulkProcessResponseByProcessCode(String bulkProcessCode);

  /**
   * Method to upload files
   *
   * @param businessPartnerCode
   * @param username
   * @param fileNames
   * @param processType
   * @throws Exception
   */
  void uploadExcelFile(String businessPartnerCode, String username, List<String> fileNames,
    String processType) throws Exception;

  /**
   * Method to upload files
   *
   * @param businessPartnerCode
   * @param username
   * @param files
   * @param pickupPointCode
   * @param bulkProcessCode
   * @throws Exception
   */
  void uploadExternalFiles(String businessPartnerCode, String username, String zipFileName,
      Map<String, String> files, String pickupPointCode, String bulkProcessCode) throws Exception;
}
