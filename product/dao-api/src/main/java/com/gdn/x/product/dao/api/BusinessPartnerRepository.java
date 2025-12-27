package com.gdn.x.product.dao.api;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.BusinessPartner;

public interface BusinessPartnerRepository extends MongoRepository<BusinessPartner, String> {

  BusinessPartner findByStoreIdAndBusinessPartnerCode(String storeId, String businessPartnerCode);

}
