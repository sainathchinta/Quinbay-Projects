package com.gdn.partners.pcu.internal.service;

import com.gdn.partners.pcu.internal.web.model.response.PendingDownloadProcessWebResponse;

public interface StoreCopyService {

  /**
   * Download all products by seller code
   *
   * @param username
   * @param sellerCode
   */
  void downloadAllProductsBySellerCode(String username, String sellerCode);

  /**
   * Download upload template
   *
   *
   * @param sellerCode
   */
  String downloadUploadTemplate(String sellerCode);

  /**
   * Fetching pending download process
   *
   * @param sellerCode
   * @param userName
   * @param processType
   * @return
   */
  PendingDownloadProcessWebResponse getPendingProcesses(String sellerCode, String userName, String processType);
}
