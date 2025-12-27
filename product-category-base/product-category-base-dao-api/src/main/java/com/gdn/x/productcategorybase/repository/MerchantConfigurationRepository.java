package com.gdn.x.productcategorybase.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import com.gdn.x.productcategorybase.entity.MerchantConfiguration;

public interface MerchantConfigurationRepository
    extends JpaRepository<MerchantConfiguration, String>, JpaSpecificationExecutor<MerchantConfiguration>,
    MerchantConfigurationCustomRepository {

  List<MerchantConfiguration> findByMerchantCodeInAndMarkForDeleteFalse(List<String> merchantCodes);

  List<MerchantConfiguration> findByStoreIdAndMerchantCodeIn(String storeId, List<String> merchantCodes);

  MerchantConfiguration findByStoreIdAndMerchantCode(String storeId, String merchantCode);

  MerchantConfiguration findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(String storeId, String merchantCode);

  Page<MerchantConfiguration> findByStoreIdAndUpdatedDateGreaterThan(String storeId, Date date, Pageable pageable);

  Long countByStoreIdAndMarkForDeleteFalse(String storeId);
}
