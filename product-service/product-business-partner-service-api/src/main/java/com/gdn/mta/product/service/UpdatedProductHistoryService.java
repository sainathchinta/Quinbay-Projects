package com.gdn.mta.product.service;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gda.mta.product.dto.response.SimpleBooleanResponse;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.valueobject.UpdateProductItemLevel3Model;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;


public interface UpdatedProductHistoryService {

  /**
   * creates audits trail logs for product
   *
   * @param updatedProductHistoryList list of product
   * @param historySolrUpdateNewEvent
   * @return
   */
  List<UpdatedProductHistory> createAudit(List<UpdatedProductHistory> updatedProductHistoryList, boolean historySolrUpdateNewEvent);

  /**
   * Publish history collection events
   *
   * @param logList
   */
  void saveUpdatedProductHistoryToSolr(List<UpdatedProductHistory> logList);

  /**
   * Get audit trail logs for product
   * 
   * @param pageable page information
   * @param gdnSku gdn sku
   * @return page of gdn sku audit trail
   */
  Page<UpdatedProductHistory> getAuditLogsForProduct(Pageable pageable, String gdnSku);

  void saveUpdateProductLevel3Audit(String businessPartnerCode, String gdnSku,
      UpdateProductItemLevel3Model savedProductValue, UpdateProductItemLevel3Model updatedProductValue,
      String accessChannel, String productSku, String name, boolean needCorrection, String username) throws Exception;

  /**
   *
   * @param savedProductValue
   * @param updatedProductValue
   * @return
   * @throws Exception
   */
  List<String> getMerchantModifiedFields(UpdateProductItemLevel3Model savedProductValue,
      UpdateProductItemLevel3Model updatedProductValue) throws Exception;

  /**
   * Update audit for all item sku's during sync and unsync process
   *
   * @param businessPartnerCode
   * @param gdnSku
   * @param ActualItemSku
   * @param savedProductValue
   * @param updatedProductValue
   * @param accessChannel
   * @throws Exception
   */
  void saveUpdateProductLevel3AuditWithDescription(String businessPartnerCode, String gdnSku, String ActualItemSku,
      UpdateProductItemLevel3Model savedProductValue, UpdateProductItemLevel3Model updatedProductValue,
      String accessChannel) throws Exception;

  void saveUpdateProductLevel3AuditForWholeSale(String businessPartnerCode, String gdnSku, String activity,
      String oldValue, String newValue, String productSku, String name) throws Exception;

  /**
   * Create audit for all unsync item during migration
   *
   * @param businessPartnerCode
   * @param gdnSku
   * @throws Exception
   */
  void saveUpdateProductLevel3AuditForMigration(String businessPartnerCode, String gdnSku)
      throws JsonProcessingException;

  void saveCreateProductLevel3Audit(String businessPartnerCode,
      ProductAndItemActivationRequest productLevel3Request, ProductBusinessPartner productBusinessPartner) throws Exception;

  /**
   * Create audit for all item sku's during sync and unsync process
   *
   * @param businessPartnerCode
   * @param skus
   * @param sku
   * @param savedProductData
   * @param updatedProductData
   * @param accessChannel
   * @throws Exception
   */
  void createAuditTrailForAllItemSkus(String businessPartnerCode, List<String> skus, String sku, ProductAndItemsResponse
          savedProductData, ProductAndItemsResponse updatedProductData, String accessChannel)
          throws Exception;

  /**
   *
   * @param sku
   * @param fromDate
   * @return
   */
  SimpleBooleanResponse isPriceChangedForSku(String sku, Date fromDate);

  /**
   * get variant history from db using product sku and keyword
   * @param storeId
   * @param productSku
   * @param keyword
   * @param page
   * @param size
   * @return
   */
  Page<HistoryResponse> getProductEditHistory(String storeId, String productSku, String keyword, int page, int size);

  /**
   *
   * @param auditTrailId
   * @param page
   * @param size
   * @param total
   * @return
   */
  Page<HistoryResponse> getProductEditHistoryByAuditTrailId(List<String> auditTrailId, int page, int size, long total);

  /**
   *
   * @param storeId
   */
  void deleteFromSolr(String storeId);

  /**
   *
   * @param storeId
   */
  void deleteFromDb(String storeId);

  /**
   *
   * @param auditTrailIds
   */
  void deleteUpdatedProductHistoryByAuditTrialIds(List<String> auditTrailIds);

  /**
   * Create audit log for logistic update
   * @param businessPartnerCode
   * @param gdnSku
   * @param productSku
   * @param name
   * @param activity
   * @param oldValue
   * @param newValue
   * @param needRevision
   * @param pickupPointCode
   * @throws Exception
   */
  void createProductL3AuditLog(String businessPartnerCode, String gdnSku, String productSku, String name,
      String activity, String oldValue, String newValue, boolean needRevision, String pickupPointCode) throws Exception;

  /**
   *
   * Add Audit logs for product history update
   *
   * @param auditLogs
   * @param auditTrailRequest
   *
   */
  void addAuditLogsForProductHistoryUpdate(AuditTrailDto auditTrailRequest, List<UpdatedProductHistory> auditLogs, String accessChannel) throws Exception;

  /**
   * find variant history based on productSku and keyword
   *
   * @param historyRequest
   * @param page
   * @param size
   * @return
   */
  Page<HistoryResponse> findProductHistoryByProductSkuAndKeyword(HistoryRequest historyRequest, int page, int size);

  /**
   * Add history for vat update.
   *
   * @param productItemBusinessPartnerList
   * @param itemName
   * @param oldValue
   * @param newValue
   */
  void addAuditLogsForVatUpdate(List<ProductItemBusinessPartner> productItemBusinessPartnerList,
      String itemName, String oldValue, String newValue) throws JsonProcessingException;

  /**
   *
   * get offline history by itemSku and PickupPointCode
   *
   * @param storeId
   * @param itemSku
   * @param pickupPointCode
   * @param pageable
   * @return
   */
  Page<UpdatedProductHistory> getOfflineProductHistoryByItemSkuAndPickupPointCode(String storeId,
      String itemSku, String pickupPointCode, Pageable pageable);

  /**
   * Set gdnSku and return HistoryUpdateResponse page
   *
   * @param auditTrailIds
   * @param page
   * @param size
   * @param totalElements
   * @return
   */
  Page<HistoryUpdateResponse> getProductUpdateHistoryByAuditTrailId(List<String> auditTrailIds,
    int page, int size, long totalElements);

  Page<HistoryUpdateResponse> findProductUpdateHistoryByProductSkuAndKeyword(
          HistoryUpdateRequest historyUpdateRequest, int page, int size);

  /**
   *
   * @param auditTrailUpdatedProductList
   */
  void updateProductHistoryDeltailList(List<UpdatedProductHistory> auditTrailUpdatedProductList)
      throws JsonProcessingException;

  /**
   * Get is price changed or not
   *
   * @param sku
   * @param fromDate
   * @param ppCode
   * @return
   */
  SimpleBooleanResponse isPriceChangedForSkuAndPPCode(String sku, Date fromDate, String ppCode);

  /**
   * Add to updated product history table
   *
   * @param gdnSku
   * @param activity
   * @param businessPartnerCode
   * @param requestId
   * @param username
   * @param oldValues
   * @param newValues
   * @param productSku
   * @param itemName
   * @param pickupPointcode
   * @param onlineStatus
   * @return
   */
  UpdatedProductHistory addToUpdatedProductHistory(String gdnSku, String activity, String businessPartnerCode,
      String requestId, String username, String oldValues, String newValues, String productSku, String itemName,
      String pickupPointcode, boolean onlineStatus);

  /**
   * delete entries from update product history repo by storeId and productId
   * @param storeId
   * @param productId
   */
  void deleteUpdateProductHistoryByStoreIdAndProductId(String storeId, String productId);
}
