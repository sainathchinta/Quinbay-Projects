package com.gdn.x.productcategorybase.repository;

import com.gdn.x.productcategorybase.entity.SizeChart;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface SizeChartRepository
    extends JpaRepository<SizeChart, String>, SizeChartRepositoryCustom {

  /**
   * fetch size chart details
   *
   * @param storeId       String
   * @param sizeChartCode String
   * @param markForDelete boolean
   * @return SizeChart
   */
  SizeChart findByStoreIdAndSizeChartCodeAndMarkForDelete(String storeId, String sizeChartCode,
    boolean markForDelete);

  /**
   * Get size charts based on storeId and sizeChartCodes
   * @param storeId
   * @param sizeChartCodes
   * @return
   */
  List<SizeChart> findByStoreIdAndSizeChartCodeInAndMarkForDeleteFalse(String storeId,
      List<String> sizeChartCodes);

  /**
   * find size chart based on size-chart code and business partner code
   *
   * @param storeId
   * @param sizeChartCode
   * @param businessPartnerCode
   * @param markForDelete
   * @return
   */
  SizeChart findByStoreIdAndSizeChartCodeAndBusinessPartnerCodeAndMarkForDelete(String storeId,
      String sizeChartCode, String businessPartnerCode, boolean markForDelete);

  /**
   * get sequence for size chart code
   *
   * @param key String
   * @return long
   */
  @Query(value = "select get_sequence(?1) as sequence", nativeQuery = true)
  long getSequenceForSizeChart(String key);

  /**
   * find size chart based on size-chart name and business partner code
   *
   * @param storeId
   * @param sizeChartName
   * @param businessPartnerCode
   * @param markForDelete
   * @return
   */
  SizeChart findByStoreIdAndNameAndBusinessPartnerCodeAndMarkForDelete(String storeId,
      String sizeChartName, String businessPartnerCode, boolean markForDelete);
}
