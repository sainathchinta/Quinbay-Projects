package com.gdn.x.product.dao.api;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;

import java.util.List;

public interface BusinessPartnerPickupPointRepository
    extends MongoRepository<BusinessPartnerPickupPoint, String>, BusinessPartnerPickupPointCustomRepository {

  BusinessPartnerPickupPoint findByBusinessPartnerCodeAndCode(String businessPartnerCode, String code);

  BusinessPartnerPickupPoint findByCode(String code);

  List<BusinessPartnerPickupPoint> findByCodeIn(List<String> code);
}