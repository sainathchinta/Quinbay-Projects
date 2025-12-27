package com.gdn.x.product.dao.api;

import java.util.List;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.SystemParameter;

public interface SystemParameterRepository extends MongoRepository<SystemParameter, String>,
    SystemParameterRepositoryCustom {

  /**
   * @param storeId
   * @param variable
   * @return empty list if object not found
   */
  List<SystemParameter> deleteByStoreIdAndVariable(String storeId, String variable);

  /**
   * @param storeId
   * @return
   */
  List<SystemParameter> findAllByStoreId(String storeId);


  /**
   * @param variable
   * @param storeId
   * @return null if not found
   */
  SystemParameter findByStoreIdAndVariable(String storeId, String variable);

}
