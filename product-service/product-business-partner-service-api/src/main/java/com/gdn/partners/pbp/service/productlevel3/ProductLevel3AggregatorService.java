package com.gdn.partners.pbp.service.productlevel3;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.valueobject.ProductLevel3SummaryCount;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator.ProductLevel3AggregatorInventoryCriteria;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator.ProductLevel3AggregatorState;
import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;

public interface ProductLevel3AggregatorService {

  ProductLevel3Aggregator findByGdnSku(String gdnSku) throws Exception;

  Page<ProductLevel3Aggregator> findByBusinessPartnerCodeAndInventoryFilterAndState(String businessPartnerCode,
      ProductLevel3AggregatorInventoryCriteria inventoryCriteria, ProductLevel3AggregatorState state, Pageable pageable) throws Exception;

  void updateMinimumStock(Level2InventoryMinimumStockAlertEvent messageEvent, boolean value, int retry) throws Exception;

  void updateOOS(Level2InventoryOosEvent messageEvent, int retry) throws Exception;
  
  void updateNonOOS(Level2InventoryNonOosEvent messageEvent, int retry) throws Exception;
  
  void update(String gdnSku, String businessPartnerCode) throws Exception;

  void create(String gdnSku, String businessPartnerCode) throws Exception;
  
  void updateState(String gdnSku, String businessPartnerCode, ProductLevel3AggregatorState state) throws Exception;

  ProductLevel3SummaryCount countSummary(String businessPartnerCode) throws Exception;

  void create(ProductLevel3Aggregator productLevel3Aggregator) throws Exception;

  void delete(String storeId, List<String> gdnSkus) throws Exception;
}
