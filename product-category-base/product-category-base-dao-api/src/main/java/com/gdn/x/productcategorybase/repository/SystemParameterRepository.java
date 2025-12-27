package com.gdn.x.productcategorybase.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.gdn.x.productcategorybase.entity.SystemParameter;

public interface SystemParameterRepository extends JpaRepository<SystemParameter, String> {

  SystemParameter findByStoreIdAndVariable(String storeId, String variable);
}
