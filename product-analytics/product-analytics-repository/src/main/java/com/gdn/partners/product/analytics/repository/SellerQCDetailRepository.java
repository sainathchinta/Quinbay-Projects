package com.gdn.partners.product.analytics.repository;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.partners.product.analytics.entity.SellerQCDetail;

public interface SellerQCDetailRepository
    extends MongoRepository<SellerQCDetail, String>, SellerQcDetailRepositoryCustom {

  SellerQCDetail findByBusinessPartnerCode(String businessPartnerCode);

}