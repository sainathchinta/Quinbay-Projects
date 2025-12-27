package com.gdn.partners.pdt.repository.configuration.distribution;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import com.gdn.partners.pdt.entity.configuration.distribution.AutoDistributionConfiguration;

public interface AutoDistributionConfigurationRepository extends JpaRepository<AutoDistributionConfiguration, String> {

  String QUERY_FIND_VENDOR_CODE_BY_AUTO_CONFIGURATION =
      "SELECT v.vendor_code FROM pdt_vendor AS v LEFT JOIN pdt_auto_distribution_configuration AS adc ON v.vendor_code = adc.vendor_code "
          + "WHERE v.store_id = ?1 AND (adc.store_id = ?1 OR adc.store_id IS NULL) AND v.mark_for_delete IS FALSE AND "
          + "(adc.mark_for_delete IS FALSE OR adc.mark_for_delete IS NULL) AND (adc.priority_value IN (?2) OR adc.priority_value IS NULL) AND "
          + "((v.start_holiday_date > CURRENT_TIMESTAMP OR v.start_holiday_date IS NULL) OR (v.end_holiday_date < CURRENT_TIMESTAMP OR v.end_holiday_date IS NULL)) AND "
          + "(v.quota - (SELECT COUNT(*) FROM pdt_product WHERE current_vendor = v.id AND mark_for_delete IS FALSE)) > 0 ORDER BY adc.priority ASC, "
          + "(v.quota - (SELECT COUNT(*) FROM pdt_product WHERE current_vendor = v.id AND mark_for_delete IS FALSE)) DESC LIMIT 1";

  @Query(value = AutoDistributionConfigurationRepository.QUERY_FIND_VENDOR_CODE_BY_AUTO_CONFIGURATION,
      nativeQuery = true)
  String findVendorCodeByStoreIdAndPriorityValuesAndMarkForDeleteFalse(String storeId, List<String> priorityValues)
      throws Exception;

  List<AutoDistributionConfiguration> findByStoreIdAndVendorCodeAndMarkForDeleteIsFalse(String storeId,
      String vendorCode) throws Exception;

}
