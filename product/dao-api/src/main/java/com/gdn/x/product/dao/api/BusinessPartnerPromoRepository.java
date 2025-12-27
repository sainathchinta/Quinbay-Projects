package com.gdn.x.product.dao.api;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.BusinessPartnerPromo;

import java.util.List;

public interface BusinessPartnerPromoRepository extends MongoRepository<BusinessPartnerPromo, String> {

  BusinessPartnerPromo findByBusinessPartnerCode(String businessPartnerCode);

  List<BusinessPartnerPromo> findByStoreIdAndBusinessPartnerCodeIn(String storeId,
    List<String> businessPartnerCodes);
}
