package com.gdn.x.product.dao.api;

import java.util.List;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.SystemParameterHistory;

public interface SystemParameterHistoryRepository extends
MongoRepository<SystemParameterHistory, String> {

  /**
   *
   * @param variable
   * @param storeId
   * @return empty list if not found
   */
  List<SystemParameterHistory> findByStoreIdAndVariable(String storeId, String variable);


}
