package com.gdn.x.mta.distributiontask.service.api;

import java.util.List;

import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.x.mta.distributiontask.model.ProductActionRetry;
import com.gdn.x.mta.distributiontask.model.enums.ActionRetryStatus;

public interface ProductActionRetryService {

  /**
   *
   * @param storeId
   * @param action
   * @param actionRetryStatus
   * @param maximumAllowedProductForActionRetry
   * @return
   */
  List<ProductActionRetry> findProductsToRetryActionOrderByCreatedDateAsc(String storeId,
      String action, ActionRetryStatus actionRetryStatus, int maximumAllowedProductForActionRetry);

  /**
   *
   * @param storeId
   * @param action
   * @param actionRetryStatus
   * @param maximumAllowedProductForActionRetry
   * @return
   */
  List<ProductActionRetry> findProductsToRetryActionOrderByCreatedDateDesc(String storeId,
      String action, ActionRetryStatus actionRetryStatus, int maximumAllowedProductForActionRetry);

  /**
   *
   * @param storeId
   * @param action
   * @param actionRetryStatus
   * @param maximumAllowedProductForActionRetry
   * @return
   */
  List<ProductActionRetry> findProductsToRetryActionOrderByUpdatedDateAsc(String storeId,
      String action, ActionRetryStatus actionRetryStatus, int maximumAllowedProductForActionRetry);

  /**
   *
   * @param storeId
   * @param action
   * @param actionRetryStatus
   * @param maximumAllowedProductForActionRetry
   * @return
   */
  List<ProductActionRetry> findProductsToRetryActionOrderByUpdatedDateDesc(String storeId,
      String action, ActionRetryStatus actionRetryStatus, int maximumAllowedProductForActionRetry);

  /**
   * @param productActionRetry
   */
  void updateProductActionRetryDetails(ProductActionRetry productActionRetry);

  /**
   * Find product by storeId, productCode and action
   *
   * @param storeId
   * @param productCode
   * @param action
   * @return
   */
  ProductActionRetry getProductActionRetryByProductCodeAndAction(String storeId, String productCode, String action);

  /**
   * Upsert entry into product action retry event
   *
   * @param productActionRetryEvent
   */
  void upsertProductActionRetry(ProductActionRetryEvent productActionRetryEvent);

  /**
   * save list of entries into product action retry table
   *
   * @param productActionRetries
   */
  void saveProductActionRetryList(List<ProductActionRetry> productActionRetries);
}
