package com.gdn.mta.product.repository;

import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.entity.ProductItemBusinessPartner;

public interface ProductItemBusinessPartnerCustomRepository {

  Page<ProductItemBusinessPartner> findByStoreIdAndProductBusinessPartnerIdAndGdnProductItemSkuAndPickupPointIdIn(
    String storeId, String productBusinessPartnerId, String itemSku, Set<String> pickupPointCodes,
    Pageable pageable, boolean isFbbSortRequired);

}
