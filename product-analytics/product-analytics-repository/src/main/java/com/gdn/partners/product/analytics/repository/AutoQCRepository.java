package com.gdn.partners.product.analytics.repository;

import java.util.List;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.partners.product.analytics.entity.AutoQCDetail;

public interface AutoQCRepository extends MongoRepository<AutoQCDetail, String>, AutoQCRepositoryCustom {

  AutoQCDetail findByBusinessPartnerCodeAndC1Code(String businessPartnerCode, String c1Code);

  List<AutoQCDetail> findByBusinessPartnerCode(String businessPartnerCode);
}
