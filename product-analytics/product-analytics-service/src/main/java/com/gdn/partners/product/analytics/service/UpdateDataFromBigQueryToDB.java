package com.gdn.partners.product.analytics.service;

import java.io.IOException;
import java.util.List;

import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;

public interface UpdateDataFromBigQueryToDB {

  /**
   * Write json data Data to DB
   *
   * @param filePath
   * @param batchSize
   * @return
   */
  List<SellerFieldsChangeResponse> writeJsonDataFromFileToDB(String filePath, int batchSize) throws IOException;

  /**
   * Get result file name local
   *
   * @return
   */
  String getResultFileNameLocal();

  /**
   * Get result file name prefix
   *
   * @return
   */
  String getResultFileNamePrefix();

  /**
   * Get Result Data set
   *
   * @return
   */
  String getResultDataSet();

  /**
   * Get result table prefix
   *
   * @return
   */
  String getResultTablePrefix();

}
