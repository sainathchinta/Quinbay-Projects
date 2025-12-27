package com.gdn.mta.bulk.service;

import java.util.List;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.dto.product.DormantSellerProductUpdateRequest;
import com.gdn.mta.bulk.entity.DormantSellerEvent;
import com.gdn.mta.bulk.entity.DormantSellerProduct;
import com.gdn.mta.bulk.models.DormantSellerItemDetail;
import org.springframework.data.domain.Page;
import org.springframework.scheduling.annotation.Async;

public interface DormantSellerService {

  /**
   *
   * @param businessPartnerCode
   */
  void processSellerDeactivate(String businessPartnerCode, String sellerProcessType) throws ApplicationException;

  /**
   * Process pending events stored in db
   *
   * @param storeId
   * @param requestId
   * @param username
   */
  void processPendingDormantSellerEvent(String storeId, String requestId, String username);

  /**
   *  @param storeId
   * @param requestId
   * @param username
   * @param processType
   */
  void updateViewConfigForItemsOfDormantSeller(String storeId, String requestId, String username,
    String processType);

  /**
   *
   * @param dormantSellerItemDetail
   */
  void updateProductItemViewConfig(DormantSellerItemDetail dormantSellerItemDetail) throws Exception;

  /**
   *
   * @param storeId
   * @param requestId
   * @param username
   */
  void updateDormantSellerStatus(String storeId, String requestId, String username);

  /**
   * @param storeId
   * @param requestId
   * @param username
   * @param pageSize
   * @param batchSize
   * @param days
   */
  void deleteDormantSellerProduct(String storeId, String requestId, String username, int pageSize, int batchSize,
      int days);

  /**
   * Manually update product status for dormant seller
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param dormantSellerProductUpdateRequestList
   */
  void overrideDormantSellerProductStatus(String storeId, String requestId, String username,
    List<DormantSellerProductUpdateRequest> dormantSellerProductUpdateRequestList);


  /**
   * @param storeId
   * @param requestId
   * @param username
   * @param status
   * @param businessPartnerCodes
   */
  void updateDormantSellerEvent(String storeId, String requestId, String username, String status,
      List<String> businessPartnerCodes, boolean deleteDataEntries);

  /**
   * Process event for seller resign
   *
   * @param storeId
   * @param businessPartnerCode
   */
  void processResignSellerEvent(String storeId, String businessPartnerCode)
    throws ApplicationException;

  /**
   * Fetch Seller events by batch size
   *
   * @param storeId
   * @param status
   * @param processType
   * @param fetchBatchSize
   * @return
   */
  Page<DormantSellerEvent> findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(String storeId,
    String status, String processType, Integer fetchBatchSize);

  /**
   * Save list of DormantSellerProduct
   *
   * @param dormantSellerProductList
   */
  void saveCollectionInput(List<DormantSellerProduct> dormantSellerProductList);

  /**
   * Update dormant seller event
   *
   * @param dormantSellerEvent
   * @return
   */
  DormantSellerEvent upsertDormantSellerEvent(DormantSellerEvent dormantSellerEvent);

  /**
   *
   * @param businessPartnerCode
   * @param processType
   * @return
   */
  DormantSellerEvent findByBusinessPartnerCodeAndProcessType(String businessPartnerCode, String processType);

  /**
   * Retry dormant seller process
   *
   * @param storeId
   */
  void retryDormantSeller(String storeId);

  /**
   * Notify stuck dormant process
   *
   * @param storeId
   */
  void notifyStuckDormantProcess(String storeId);

  List<DormantSellerEvent>  findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(String storeId, String businessPartnerCode, String processType);

  List<DormantSellerEvent> saveDormantSellerEvents(List<DormantSellerEvent> dormantSellerEvents);
}
