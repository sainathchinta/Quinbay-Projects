package com.gdn.x.productcategorybase.repository.brand;

import java.util.Date;
import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;

public interface BrandAuthorisationRepository
    extends JpaRepository<BrandAuthorisation, String>, BrandAuthorisationRepositoryCustom {

  @Modifying
  @Query("update BrandAuthorisation ba set ba.brandName = ?1 where ba.brandCode = ?2")
  void updateBrandNameByBrandCode(String brandName, String brandCode);

  String QUERY_DELETE_BRAND_AUTHORISATION_MAPPING_BY_SELLER_CODE_AND_BRAND_CODE =
    "UPDATE pcc_brand_authorisation SET mark_for_delete = TRUE, updated_by = ?2,updated_date = CURRENT_TIMESTAMP WHERE store_id = ?1 AND brand_code = ?3 AND seller_code = ?4";

  List<BrandAuthorisation> findByStoreIdAndBrandCodeAndMarkForDeleteFalse(String storeId, String brandCode);

  @Modifying(clearAutomatically = false)
  @Query(value = BrandAuthorisationRepository.QUERY_DELETE_BRAND_AUTHORISATION_MAPPING_BY_SELLER_CODE_AND_BRAND_CODE, nativeQuery = true)
  void deleteByStoreIdAndBrandCodeAndSellerCode(String storeId, String username, String brandCode,
    String sellerCode);

  List<BrandAuthorisation> findByStoreIdAndIdInAndMarkForDeleteFalse(String storeId, Set<String> ids);

  BrandAuthorisation findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(String storeId,
    String brandCode, String sellerCode);

  BrandAuthorisation findFirstByStoreIdAndBrandCodeAndSellerCode(String storeId,
      String brandCode, String sellerCode);

}
