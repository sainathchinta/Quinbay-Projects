package com.gdn.partners.pbp.repository.workflow;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.partners.pbp.entity.workflow.product.ProductWfHistory;

public interface ProductWfHistoryRepository extends JpaRepository<ProductWfHistory, String> {

  ProductWfHistory findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(String storeId,
      String productCode, String state);

  List<ProductWfHistory> findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalseOrderByUpdatedDateAsc(
      String storeId, String productCode, String state);

  long deleteByStoreIdAndProductCode(String storeId, String productCode);
}
