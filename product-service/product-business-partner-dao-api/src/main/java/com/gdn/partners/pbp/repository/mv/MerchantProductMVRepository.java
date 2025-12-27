package com.gdn.partners.pbp.repository.mv;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.partners.pbp.entity.mv.MerchantProductMV;

public interface MerchantProductMVRepository extends JpaRepository<MerchantProductMV, String>,
    MerchantProductMVRepositoryCustom {

  MerchantProductMV findByStoreIdAndItemSku(String storeId, String itemSKu);

}
