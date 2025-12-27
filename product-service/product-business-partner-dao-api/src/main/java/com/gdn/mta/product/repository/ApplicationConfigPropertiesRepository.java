package com.gdn.mta.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.product.entity.ApplicationConfigProperties;

public interface ApplicationConfigPropertiesRepository extends JpaRepository<ApplicationConfigProperties, String> {

  ApplicationConfigProperties findByStoreIdAndPropertyNameAndMarkForDeleteFalse(String storeId, String propertyName);

}
