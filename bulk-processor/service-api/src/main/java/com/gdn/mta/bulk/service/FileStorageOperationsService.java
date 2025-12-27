package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkProcess;

import java.io.IOException;

public interface FileStorageOperationsService {

  /**
   * get filePrefix according to gcs switch
   *
   * @param bulkProcessType
   * @return
   */
  String getFilePrefix(String bulkProcessType);

  /**
   * get email prefix based on gcs switch
   *
   * @return
   */
  String getEmailPrefix();

  /**
   * download base template for price recommendation
   *
   * @return
   */
  byte[] downloadBaseTemplateForPriceRecommendation();

  /**
   *
   * @param bulkProcess
   * @param excelFileType
   * @return
   */
  byte[] downloadFile(BulkProcess bulkProcess, String excelFileType) throws IOException;

  /**
   *
   * @param fileName String
   * @return byte[]
   */
  byte[] downloadBaseTemplateForBulkBasicInfoUpdate(String fileName);
}
