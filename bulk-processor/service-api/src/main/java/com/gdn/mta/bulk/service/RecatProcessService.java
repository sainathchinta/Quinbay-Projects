package com.gdn.mta.bulk.service;

import java.text.ParseException;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.entity.RecatProcess;
import org.springframework.data.domain.Page;

public interface RecatProcessService {

  /**
   * Get all recat process which are status has NEW
   *
   * @param storeId
   * @param status
   * @return
   */
  List<RecatProcess> getAllEligibleNewRecatProcess(String storeId, String status) throws ParseException;

  /**
   * Get recat process by status
   *
   * @param storeId
   * @param status
   * @return
   */
  List<RecatProcess> getRecatProcessByStoreIdAndStatus(String storeId, String status);

  /**
   * Save recat process
   *
   * @param recatProcess
   */
  void saveRecatProcess(RecatProcess recatProcess);

  /**
   * get Recat process by recat request code
   *
   * @param storeId
   * @param recatRequestCode
   * @return
   */

  RecatProcess getRecatProcessByRecatRequestCode(String storeId, String recatRequestCode);

  /**
   * Fetch RecatProcess entity by request
   *
   * @param storeId
   * @param recatProcessSummaryRequest
   * @param page
   * @param size
   * @return
   */
  Page<RecatProcess> getRecatProcessSummary(String storeId,
      RecatProcessSummaryRequest recatProcessSummaryRequest, int page, int size);

  /**
   * Update final recat status
   *
   * @param recatProcess
   * @param statusCountMap
   * @return
   */
  RecatProcess updateFinalStatus(RecatProcess recatProcess, Map<String, Integer> statusCountMap);

  /**
   * Send mail for finished recat process
   *
   * @param recatProcessList
   */
  void sendMailForFinishedRecatProcess(List<RecatProcess> recatProcessList) throws Exception;

  /**
   * Send mail for recat failed products
   *
   * @param recatProcess
   * @param mailTo
   */
  void sendMailForRecatFailedProducts(RecatProcess recatProcess, String mailTo);

  /**
   * Get recat request by request code and storeId
   *
   * @param storeId
   * @param recatRequestCode
   * @return
   */
  RecatProcess findRecatProcessByRecatRequestCode(String storeId, String recatRequestCode);

  /**
   * Send mail for new recat process
   *
   * @param recatProcessService
   */
  void sendMailForNewRecatProcess(RecatProcess recatProcessService);

  /**
   * Delete recat file
   *
   * @param fileName
   */
  void deleteRecatFile(String fileName);
}
