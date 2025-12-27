package com.gdn.partners.pcu.internal.service;

import org.springframework.web.multipart.MultipartFile;

public interface FileHelper {

  /**
   * Upload based on fileType
   *
   * @param multipartFile
   * @param processType
   * @param bipRequestCode
   */
  void uploadFileBasedOnProcessType(MultipartFile multipartFile, String processType, String bipRequestCode)
      throws Exception;

  /**
   * to upload bulk file
   * @param file MultipartFile
   * @param processType String
   * @param requestId String
   * @param storeId String
   * @param username String
   * @throws Exception while uploading file to gcs
   */
  void uploadBulkFile(MultipartFile file, String processType, String requestId, String storeId,
      String username) throws Exception;
}