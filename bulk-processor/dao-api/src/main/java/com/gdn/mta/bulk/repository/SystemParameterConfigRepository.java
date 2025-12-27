package com.gdn.mta.bulk.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.bulk.entity.SystemParameterConfig;

import java.util.List;

public interface SystemParameterConfigRepository extends JpaRepository<SystemParameterConfig, String> {

  /**
   *
   * @param storeId
   * @param variable
   * @return
   */
  SystemParameterConfig deleteByStoreIdAndVariable(String storeId, String variable);


  /**
   *
   * @param storeId
   * @param variable
   * @return
   */
  SystemParameterConfig findByStoreIdAndVariable(String storeId, String variable);

  /**
   * Repository call to fetch list of SystemParameterConfig by variable list
   *
   * @param storeId String must not be empty
   * @param variableList List must not be empty
   * @return
   */
  List<SystemParameterConfig> findByStoreIdAndVariableIn(String storeId, List<String> variableList);
}
