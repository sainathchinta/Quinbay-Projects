package com.gdn.x.mta.distributiontask.dao.api;

import java.util.List;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.mta.distributiontask.model.ProductAutoApproval;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;

public interface ProductAutoApprovalRepository extends JpaRepository<ProductAutoApproval, String> {

  List<ProductAutoApproval> findByStoreIdAndAutoApprovalStatusOrderByCreatedDateAsc(
      String storeId, AutoApprovalStatus autoApprovalStatus,Pageable pageable);

  List<ProductAutoApproval> findByStoreIdAndAutoApprovalStatusOrderByCreatedDateDesc(
      String storeId, AutoApprovalStatus autoApprovalStatus,Pageable pageable);

  List<ProductAutoApproval> findByStoreIdAndAutoApprovalStatusOrderByUpdatedDateAsc(
      String storeId, AutoApprovalStatus autoApprovalStatus,Pageable pageable);

  List<ProductAutoApproval> findByStoreIdAndAutoApprovalStatusOrderByUpdatedDateDesc(
      String storeId, AutoApprovalStatus autoApprovalStatus,Pageable pageable);

  List<ProductAutoApproval> findByStoreIdAndProductCodeIn(String storeId, List<String> productCodes);

  ProductAutoApproval findByStoreIdAndProductCode(String storeId, String productCodes);

  void deleteByProductCode(String productCode);
}
