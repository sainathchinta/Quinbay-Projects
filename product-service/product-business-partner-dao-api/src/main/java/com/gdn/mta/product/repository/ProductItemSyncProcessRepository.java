package com.gdn.mta.product.repository;


import java.util.List;

import com.gdn.mta.product.entity.ProductItemSyncProcess;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ProductItemSyncProcessRepository extends JpaRepository<ProductItemSyncProcess, String> {

  List<ProductItemSyncProcess> findByStoreIdAndIsUserNotifiedFalseAndMarkForDeleteFalse(String storeId);

}
