package com.gdn.x.mta.distributiontask.dao.api;

import java.util.List;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.mta.distributiontask.model.ProductActionRetry;
import com.gdn.x.mta.distributiontask.model.enums.ActionRetryStatus;

public interface ProductActionRetryRepository  extends JpaRepository<ProductActionRetry, String> {

  ProductActionRetry findByStoreIdAndProductCodeAndAction(String storeId, String productCodes, String action);

  List<ProductActionRetry> findByStoreIdAndActionAndStatusOrderByCreatedDateAsc(String storeId,
      String action, ActionRetryStatus status, Pageable pageable);

  List<ProductActionRetry> findByStoreIdAndActionAndStatusOrderByCreatedDateDesc(String storeId,
      String action, ActionRetryStatus status, Pageable pageable);

  List<ProductActionRetry> findByStoreIdAndActionAndStatusOrderByUpdatedDateAsc(String storeId,
      String action, ActionRetryStatus status, Pageable pageable);

  List<ProductActionRetry> findByStoreIdAndActionAndStatusOrderByUpdatedDateDesc(String storeId,
      String action, ActionRetryStatus status, Pageable pageable);

  void deleteByStoreIdAndProductCode(String storeId, String productCode);
}
