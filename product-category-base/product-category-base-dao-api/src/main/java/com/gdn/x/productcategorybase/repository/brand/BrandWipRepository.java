package com.gdn.x.productcategorybase.repository.brand;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;

public interface BrandWipRepository extends JpaRepository<BrandWip, String>  {

  /**
   * Get inprogress brands by state and store id.
   *
   * @param storeId
   * @param state
   * @param pageable
   * @return
   */
  Page<BrandWip> findByStoreIdAndStateAndMarkForDeleteFalse(String storeId, BrandWipState state, Pageable pageable);

  /**
   * Get updated brand wips
   *
   * @param storeId
   * @param startDate
   * @param endDate
   * @return
   */
  Page<BrandWip> findByStoreIdAndUpdatedDateBetween(String storeId, Date startDate, Date endDate, Pageable pageable);

  /**
   * Fetches brand wip detail using brand request code
   *
   * @param storeId
   * @param brandRequestCode
   * @return
   */
  BrandWip findByStoreIdAndBrandRequestCode(String storeId, String brandRequestCode);

  /**
   * Fetches brand wip detail using brand request code and state
   *
   * @param storeId
   * @param brandRequestCode
   * @return
   */
  BrandWip findByStoreIdAndBrandRequestCodeAndState(String storeId, String brandRequestCode, BrandWipState state);

  /**
   * Searches for brand wips with given brand name and state having mark for delete flag as false
   * @param brandName
   * @param state
   * @param pageable
   * @return
   */
  Page<BrandWip> findByBrandNameAndStateAndMarkForDeleteFalse(String brandName, BrandWipState state, Pageable pageable);

  /**
   * Searches for brand wips of any name with given state and mark for delete as false
   * @param state
   * @param pageable
   * @return
   */
  Page<BrandWip> findByStateAndMarkForDeleteFalse(BrandWipState state, Pageable pageable);

  @Query(value = "SELECT brand_code, brand_name, 'APPROVED', sequence FROM pcc_brand where mark_for_delete = false and "
      + "brand_name ilike :value" + "UNION (SELECT brand_code, brand_name, state, sequence FROM pcc_brand_wip where "
      + "business_partner_code = :businessPartnerCode"
      + "AND state = :state AND mark_for_delete = false AND brand_name ilike :value "
      + ") ORDER BY brand_name ASC LIMIT :size OFFSET :offset", nativeQuery = true)
  List<Object[]> getBrandSuggestions(@Param("value") String value,
      @Param("businessPartnerCode") String businessPartnerCode, @Param("state") String state,
      @Param("offset") int offset, @Param("size") int size);

  @Query(value = "SELECT count(*) FROM pcc_brand where mark_for_delete = false and"
      + "brand_name ilike :value UNION (SELECT brand_code, brand_name, state, sequence FROM "
      + "pcc_brand_wip where business_partner_code = :businessPartnerCode"
      + "AND state = :state AND mark_for_delete = false AND brand_name ilike :value)", nativeQuery = true)
  long getTotalCountsOfInProgressAndActiveBrands(@Param("value") String value,
      @Param("businessPartnerCode") String businessPartnerCode, @Param("state") String state);

  /**
   * Get brandwip by brandCode
   * @param storeId
   * @param brandCode
   * @return
   */
  BrandWip findByStoreIdAndBrandCodeAndMarkForDeleteFalse(String storeId, String brandCode);


  /**
   * Get Brand by BrandRequestCode
   *
   * @param storeId
   * @param brandRequestCode
   * @param state
   * @return
   */
  BrandWip findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(String storeId, String brandRequestCode,
      BrandWipState state);

  /**
   * Get BrandWip by name business partner code
   *
   * @param brandName
   * @param businessPartnerCode
   * @return
   */
  BrandWip findByBrandNameAndBusinessPartnerCodeAndStateAndMarkForDeleteFalse(String brandName, String businessPartnerCode,
      BrandWipState state);

  /**
   * Get BrandWip by name business partner code
   *
   *
   * @param businessPartnerCode
   * @return
   */
  Page<BrandWip> findByBusinessPartnerCodeAndStateAndMarkForDeleteFalseOrderByBrandNameAsc(String businessPartnerCode, BrandWipState state,
      Pageable pageable);

  /**
   * Returns brand wip by brand request code
   *
   * @param brandRequestCode
   * @return
   */
  BrandWip findByBrandRequestCodeAndMarkForDeleteFalse(String brandRequestCode);

  /**
   * Get brandWip by brand name and state
   *
   * @param name
   * @param state
   * @return
   */
  BrandWip findByBrandNameIgnoreCaseAndStateAndMarkForDeleteFalse(String name, BrandWipState state);


  /**
   * Check the brand's status
   *
   * @param brandRequestCode
   * @param state
   * @return
   */
  BrandWip findByBrandRequestCodeAndStateAndMarkForDeleteFalse(String brandRequestCode, BrandWipState state);

  /**
   * Get first matching brand by name
   * @param storeId
   * @param brandName
   * @return
   */
  BrandWip findTop1ByStoreIdAndBrandNameIgnoreCaseOrderByCreatedDateAsc(String storeId, String brandName);

  @Modifying(clearAutomatically = true)
  @Query(value = "UPDATE pcc_brand_wip SET updated_date=CURRENT_TIMESTAMP, updated_by=?1, valid_brand = ?2 WHERE brand_code = ?3", nativeQuery = true)
  void updateValidBrandFlagByBrandCode(String updatedBy, boolean validBrand, String brandCode);

  /**
   * Get list of all in review brands
   *
   * @param storeId
   * @param state
   * @return
   */
  List<BrandWip> findByStoreIdAndStateAndMarkForDeleteFalse(String storeId, BrandWipState state);
}
