package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.SystemParameter;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface SystemParameterRepository extends MongoRepository<SystemParameter, String> {

  SystemParameter findByStoreIdAndVariable(String storeId, String variable);
}
