package com.gdn.partners.pbp.repository.eventstore;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.partners.pbp.entity.mv.MerchantProductMVIndexingFailed;

public interface MerchantProductMVIndexingFailedRepository extends
    JpaRepository<MerchantProductMVIndexingFailed, String> {

  MerchantProductMVIndexingFailed findByItemSku(String itemSku);
}
