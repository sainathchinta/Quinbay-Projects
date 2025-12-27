package com.gdn.x.productcategorybase.repository;

import com.gdn.x.productcategorybase.dto.OmniChannelSkuBasicDetailDTO;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface ProductItemUomInfoRepository extends JpaRepository<ProductItemUomInfo, String> {

  Page<ProductItemUomInfo> findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId,
      String productCode, Pageable pageable);

  List<ProductItemUomInfo> findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId,
      String productCode);

  List<ProductItemUomInfo> findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(String storeId,
      List<String> skuCode);
}
