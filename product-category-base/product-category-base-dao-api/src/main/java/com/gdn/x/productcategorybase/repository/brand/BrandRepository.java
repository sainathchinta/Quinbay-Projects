package com.gdn.x.productcategorybase.repository.brand;

import com.gdn.x.productcategorybase.entity.brand.Brand;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Date;
import java.util.List;

public interface BrandRepository extends JpaRepository<Brand, String> {

  String QUERY_FIND_BRAND_SUMMARY_BY_BRAND_CODES =
      "SELECT b FROM Brand b WHERE b.markForDelete = false AND b.storeId = :storeId AND b"
          + ".brandCode in (:brandCodes)";

  String QUERY_FIND_BRAND_SUMMARY_BY_NAME_AND_MARK_FOR_DELETE =
	      "SELECT b FROM Brand b WHERE b.storeId = ?1 AND LOWER(b.brandName) LIKE '%' || LOWER(?2) || '%' AND b.markForDelete = ?3 ORDER BY b.updatedDate DESC";

  String QUERY_FIND_BRAND_SUMMARY_BY_NAME_AND_MARK_FOR_DELETE_FALSE_ORDER_BY_NAME =
      "SELECT b FROM Brand b WHERE b.storeId = ?1 AND LOWER(b.brandName) LIKE LOWER(?2) || '%' AND b.markForDelete = false ORDER BY b.brandName";
  
  String QUERY_DELETE_BRAND_BY_BRAND_CODE =
      "UPDATE pcc_brand SET mark_for_delete = TRUE, updated_by = ?2,updated_date = CURRENT_TIMESTAMP WHERE store_id = ?1 AND brand_code = ?3";

  String QUERY_FIND_DEFAULT_BRANDS =
      "SELECT b FROM Brand b WHERE b.storeId = :storeId AND b.brandName = 'no brand' OR b.brandName = 'OEM' ORDER BY b.brandName ASC";

  String QUERY_FIND_NO_BRAND =
      "SELECT b FROM Brand b WHERE b.storeId = :storeId AND b.brandName = 'no brand' ";
  
  Brand findByStoreIdAndBrandCodeAndMarkForDeleteFalse(String storeId, String brandCode) throws Exception;

  Brand findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(String storeId, String brandName, boolean markForDelete)
      throws Exception;

  Brand findFirstByStoreIdAndBrandCodeAndMarkForDeleteTrue(String storeId, String brandCode) throws Exception;

  Page<Brand> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  Page<Brand> findByStoreIdAndUpdatedDateBetweenAndMarkForDeleteFalse(String storeId, Date startDate, Date endDate,
      Pageable pageable);

  Page<String> findIdByStoreIdAndUpdatedDateBetweenAndMarkForDeleteTrue(String storeId, Date startDate, Date endDate,
      Pageable pageable);


  @Query(value = BrandRepository.QUERY_FIND_BRAND_SUMMARY_BY_BRAND_CODES)
  List<Brand> findByStoreIdAndBrandCodesAndMarkForDeleteFalse(@Param("storeId") String storeId,
      @Param("brandCodes")List<String> brandCodes);

  @Query(value = BrandRepository.QUERY_FIND_BRAND_SUMMARY_BY_NAME_AND_MARK_FOR_DELETE)
  Page<Brand> findByStoreIdAndBrandNameAndMarkForDelete(String storeId, String brandName, boolean markForDelete, Pageable pageable)
      throws Exception;
  
  @Query(value = BrandRepository.QUERY_FIND_BRAND_SUMMARY_BY_NAME_AND_MARK_FOR_DELETE_FALSE_ORDER_BY_NAME)
  Page<Brand> findByStoreIdAndBrandNameAndMarkForDeleteFalseOrderByBrandName(String storeId, String brandName, Pageable pageable)
      throws Exception;
  
  @Modifying(clearAutomatically = false)
  @Query(value = BrandRepository.QUERY_DELETE_BRAND_BY_BRAND_CODE, nativeQuery = true)
  void deleteByStoreIdAndBrandCode(String storeId, String username, String brandCode);

  List<Brand> findByBrandWipIdIn(List<String> brandWipIds);

  @Query(value = BrandRepository.QUERY_FIND_DEFAULT_BRANDS)
  List<Brand> findByStoreIdAndDefaultBrands(@Param("storeId") String storeId);

  @Query(value = BrandRepository.QUERY_FIND_NO_BRAND)
  List<Brand> findNoBrandByStoreId(@Param("storeId") String storeId);

  Page<Brand> findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDateDescIdAsc(String storeId, Pageable pageable);

  List<Brand> findByStoreIdAndProtectedBrandTrueAndMarkForDeleteFalse(String storeId);
}
