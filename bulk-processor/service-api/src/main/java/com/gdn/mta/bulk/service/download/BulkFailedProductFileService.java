package com.gdn.mta.bulk.service.download;

import java.io.IOException;
import java.util.List;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;

public interface BulkFailedProductFileService {

  /**
   * Create failed product file
   *
   * @param rawExcelHeaders      must not be null
   * @param bulkProcess          must not be null
   * @param sheetName
   * @param mergeEmptyValueCells
   * @return MTA link to download this file
   * @throws ApplicationException
   */
  String createFile(List<List<Object>> rawExcelHeaders, BulkProcess bulkProcess,
      List<List<Object>> invalidRows, String sheetName, boolean mergeEmptyValueCells)
    throws ApplicationException, IOException;

  /**
   * Delete file that has modified date last week
   *
   * @return number of files deleted
   * @throws ApplicationException
   */
  int deleteFileWithModifiedDateLastWeek() throws IOException;

  String getDownloadLinkHtml(String url);

  void createFileAndSendNotification(BulkProcess bulkProcess, List<List<Object>> userInputRows,
      MerchantStatusType merchantStatusType, String merchantType, boolean instoreSeller) throws Exception;

  /**
   * Create error file for converted upload process using the unified template file
   * and populate failed BulkProcessData records with error messages
   *
   * @param internalActivationPeriod internalActivationPeriod
   * @param bulkProcess              Bulk Process Entity
   * @param failedRowData            Failed Rows
   * @param merchantType             Merchant Type
   */
  void createErrorFileForConvertedUploadProcess(int internalActivationPeriod,
    BulkProcess bulkProcess, List<BulkProcessData> failedRowData, String merchantType);

}
