package com.gdn.x.mta.distributiontask.dao.api;

import com.gdn.x.mta.distributiontask.model.ConfigProperties;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ConfigPropertiesRepository extends JpaRepository<ConfigProperties, String> {

  ConfigProperties findByStoreIdAndPropertyNameAndMarkForDeleteFalse(String storeId,
      String propertyName);
}
