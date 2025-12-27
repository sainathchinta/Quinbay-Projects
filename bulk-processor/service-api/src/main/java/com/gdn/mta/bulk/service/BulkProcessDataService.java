package com.gdn.mta.bulk.service;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.gdn.mta.bulk.dto.BulkProcessDataDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.bulk.dto.RowNumberParentCodeDTO;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;

public interface BulkProcessDataService {

  /**
   * Find by storeId and bulk process
   *
   * @param storeId
   * @param bulkProcess
   * @return
   */
  List<BulkProcessData> findByStoreIdAndBulkProcess(String storeId, BulkProcess bulkProcess);

  /**
   * save bulk process data
   *
   * @param bulkProcessData
   * @return
   */
  BulkProcessData saveOperation(BulkProcessData bulkProcessData);

  /**
   * fetch details based on bulk process and parent product
   *
   * @param storeId
   * @param bulkProcess
   * @param parentProduct
   * @param status
   * @return
   */
  List<BulkProcessData> findByBulkProcessCodeAndParentProductAndStatus(String storeId, BulkProcess bulkProcess,
      String parentProduct, String status) throws Exception;


  /**
   * fetch details based on bulk process and parent product
   *
   * @param storeId
   * @param bulkProcessId
   * @param parentProduct
   * @param status
   * @return
   */
  List<BulkProcessData> findByBulkProcessIdAndParentProductAndStatus(String storeId, String bulkProcessId,
      String parentProduct, String status) throws Exception;
  /**
   * fetch details for processed blp
   *
   * @param storeId
   * @param bulkProcessCode
   * @return
   */
  List<BulkProcessData> getFailedDataForProcessedFile(String storeId, String bulkProcessCode) throws Exception;

  /**
   * fetch details for processed blp
   *
   * @param storeId
   * @param bulkProcessCode
   * @parm statusList
   * @return
   */
  List<BulkProcessData> getFailedDataForProcessedFileInStatusIn(String storeId,
      String bulkProcessCode,
      List<String> statusList) throws Exception;

  /**
   * fetch distinct parent of a bulk process
   *
   * @param storeId
   * @param bulkProcessCode
   * @return
   */
  List<String> getDistinctParentProduct(String storeId, String bulkProcessCode) throws Exception;

  /**
   * @param storeId
   * @param bulkProcessCode
   * @param status
   * @return
   */
  List<Integer> findRowNumberByStoreIdAndBulkProcessCodeAndStatus(String storeId, String bulkProcessCode,
      String status);

  /**
   * Get row number and parentProduct by storeId and bulkProcessCode and status
   *
   * @param storeId
   * @param bulkProcessCode
   * @param status
   * @return
   */
  List<RowNumberParentCodeDTO> getRowNumberAndDataByStoreIdAndBulkProcessCodeAndStatus(String storeId, String bulkProcessCode,
      String status);

  /**
   * save bulk process data
   *
   * @param bulkProcessDataList
   * @return
   */
  void saveBulkProcessData(List<BulkProcessData> bulkProcessDataList);

  /**
   * save bulk process data requires new
   *
   * @param bulkProcessDataList
   * @return
   */
  void saveOperationBulkProcessData(List<BulkProcessData> bulkProcessDataList);

  /**
   * save bulk process data
   *
   * @param bulkProcessDataList
   * @return
   */
  List<BulkProcessData> saveAndReturnBulkProcessData(List<BulkProcessData> bulkProcessDataList);

  /**
   * delete data by updated date
   *
   * @param bulkProcessCode
   * @throws Exception
   */
  void deleteDataByBulkProcessCode(String storeId, String bulkProcessCode);


  /**
   * Find by storeId and bulk process code
   *
   * @param storeId
   * @param bulkProcessCodes
   * @return
   */
  void updatePendingProcesses(String storeId, List<String> bulkProcessCodes, Date updatedDate);

  /**
   * fetch details based on bulk process and rows numbers
   *
   * @param storeId
   * @param bulkProcessCode
   * @param rowNumbers
   * @param status
   * @return
   */
  List<BulkProcessData> findByStoreIdAndBulkProcessCodeAndRowNumberInAndStatus(String storeId, String bulkProcessCode,
      List<Integer> rowNumbers, String status) throws Exception;

  /**
   * fetch details based on bulk process and rows numbers
   *
   * @param storeId must not be empty
   * @param bulkProcessCode must not be empty
   * @param status must not be empty
   * @param pageable must not be null
   * @return
   */
  Page<BulkProcessData> findByStoreIdAndBulkProcessCodeAndStatus(String storeId, String bulkProcessCode, String status,
      Pageable pageable) throws Exception;

  /**
   * fetch pending processes
   *
   * @param storeId
   * @param bulkProcessCodes
   * @return
   */
  List<String> getPendingBulkProcessCodes(String storeId, List<String> bulkProcessCodes) throws Exception;

  void abortPendingBulkProcessDataBeforeOrById(String storeId, String id);

   void saveRequestInBulkProcessData(DownloadQRCodeRequest downloadQRCodeRequest,
      BulkProcess bulkProcess) throws Exception;

  /**
   * fetch bulk process data dto with bulk process code
   *
   * @param storeId         must not be empty
   * @param bulkProcessCode must not be empty
   * @param status          must not be empty
   * @return
   */
  List<BulkProcessDataDTO> getBulkProcessDataDTOByStoreIdAndBulkProcessCodeAndStatus(String storeId,
    String bulkProcessCode, String status);

  /**
   * fetch BulkProcessData by id
   *
   * @param id must not be null
   * @return Optional BulkProcessData
   */
  Optional<BulkProcessData> findBulkProcessDataById(String id);

  void updateStatusToFailByBulkProcessCodeAndStatusIn(String bulkProcessCode,
    List<String> statuses);

  Map<String, List<BulkProcessData>> findByStoreIdAndBulkProcessCodeAndStatusAndIdentifier(String storeId,
    String bulkProcessCode, String status);
}
