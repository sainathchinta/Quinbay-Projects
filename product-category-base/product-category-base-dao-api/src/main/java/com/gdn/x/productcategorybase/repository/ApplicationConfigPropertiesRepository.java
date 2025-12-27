package com.gdn.x.productcategorybase.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.productcategorybase.entity.ApplicationConfigProperties;

public interface ApplicationConfigPropertiesRepository extends JpaRepository<ApplicationConfigProperties, String> {

  ApplicationConfigProperties findByStoreIdAndPropertyNameAndMarkForDeleteFalse(String storeId, String propertyName);

}
