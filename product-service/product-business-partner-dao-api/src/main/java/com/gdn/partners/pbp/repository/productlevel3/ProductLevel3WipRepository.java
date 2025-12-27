package com.gdn.partners.pbp.repository.productlevel3;

import java.util.Date;
import java.util.List;

import com.gdn.mta.product.entity.StateCountDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;

public interface ProductLevel3WipRepository extends JpaRepository<ProductLevel3Wip, String>, JpaSpecificationExecutor<ProductLevel3Wip>,
    ProductLevel3CustomRepository {
  
  String QUERY_COUNT_PRODUCT_LEVEL3_WIP_SUMMARY_BY_ACTIVE_AND_STATE =
      "SELECT pl3w.activated, pl3w.state, COUNT(1) FROM prd_product_business_partner AS pl3w WHERE pl3w.store_id = ?1 AND pl3w.business_partner_id = ?2 AND "
          + "pl3w.mark_for_delete IS FALSE GROUP BY pl3w.activated, pl3w.state";

  String QUERY_COUNT_PRODUCT_LEVEL3_WIP_SUMMARY_BY_ACTIVATED_AND_STATE =
      "SELECT pl3w.state, COUNT(1) FROM prd_product_business_partner AS pl3w WHERE pl3w.store_id = ?1 AND pl3w.business_partner_id = ?2 AND "
          + "pl3w.mark_for_delete IS FALSE and pl3w.activated = FALSE GROUP BY pl3w.activated, pl3w.state";
  String QUERY_COUNT_PRODUCT_LEVEL3_WIP_SUMMARY_BY_STATE =
    "SELECT new com.gdn.mta.product.entity.StateCountDTO(pl3w.state, COUNT(pl3w)) "
      + "FROM ProductBusinessPartner pl3w " + "WHERE pl3w.storeId = :storeId "
      + "AND pl3w.businessPartnerId = :businessPartnerId " + "AND pl3w.markForDelete = false "
      + "AND pl3w.activated = false " + "GROUP BY pl3w.state";

  String QUERY_COUNT_PRODUCT_LEVEL3_WIP_SUMMARY_WITH_ACTIVATION_EXPECTED_DATE_EXCEED =
      "SELECT COUNT(*) FROM ProductLevel3Wip pl3w WHERE pl3w.storeId = ?1 AND pl3w"
          + ".businessPartnerCode = ?2 AND pl3w.markForDelete IS FALSE AND pl3w.active IS FALSE "
          + "AND (pl3w.state = 'DRAFT' OR pl3w.state = 'IN_PROGRESS' OR pl3w.state IS NULL) AND "
          + "(pl3w.expectedActivationDate IS NOT NULL AND pl3w.expectedActivationDate < ?3)";

  String QUERY_PRODUCT_LEVEL3_WIP_SUMMARY_WITH_ACTIVATION_EXPECTED_DATE_EXCEED =
      "SELECT pl3w FROM ProductLevel3Wip pl3w WHERE pl3w.storeId = ?1 AND pl3w"
          + ".businessPartnerCode = ?2 AND pl3w.markForDelete IS FALSE AND pl3w.active IS FALSE "
          + "AND (pl3w.state = 'DRAFT' OR pl3w.state = 'IN_PROGRESS' OR pl3w.state IS NULL) AND "
          + "(pl3w.expectedActivationDate IS NOT NULL AND pl3w.expectedActivationDate < ?3)";

  
  @Query(value = ProductLevel3WipRepository.QUERY_COUNT_PRODUCT_LEVEL3_WIP_SUMMARY_BY_ACTIVE_AND_STATE, nativeQuery = true)
  List<Object[]> countByStoreIdAndBusinessPartnerCodeAndActiveAndStateAndMarkForDeleteFalse(String storeId,
      String businessPartnerCode);

  @Query(value = ProductLevel3WipRepository.QUERY_COUNT_PRODUCT_LEVEL3_WIP_SUMMARY_BY_ACTIVATED_AND_STATE, nativeQuery = true)
  List<Object[]> countByStoreIdAndBusinessPartnerCodeAndActivatedAndMarkForDeleteFalse(String storeId,
      String businessPartnerCode);

  @Query(value = ProductLevel3WipRepository.QUERY_COUNT_PRODUCT_LEVEL3_WIP_SUMMARY_BY_STATE)
  List<StateCountDTO> countByStoreIdAndBusinessPartnerCodeAndActivatedFalseAndMarkForDeleteFalse(
    String storeId, String businessPartnerId);

  List<ProductLevel3Wip> findByStoreIdAndProductLevel1IdAndMarkForDeleteFalse(String storeId,
      String productLevel1Id);

  List<ProductLevel3Wip> findByStoreIdAndProductLevel1Id(String storeId,
      String productLevel1Id);


  /**
   * count for all in progress products who all not yet active before or on expected activation date
   *
   * @param storeId             must not blank
   * @param businessPartnerCode must not blank
   * @param currentDate         current date must not null
   * @return productWip row count
   */
  @Query(value = QUERY_COUNT_PRODUCT_LEVEL3_WIP_SUMMARY_WITH_ACTIVATION_EXPECTED_DATE_EXCEED)
  Integer findProductWipCountByExpectationActivationDateGreater(String storeId,
      String businessPartnerCode, Date currentDate);


  /**
   * list of productLevel3Wip for all in progress products who all not yet active before or on
   * expected activation date
   *
   * @param storeId             must not blank
   * @param businessPartnerCode must not blank
   * @param currentDate         current date must not null
   * @return list of productLevel3Wip
   */
  @Query(value = QUERY_PRODUCT_LEVEL3_WIP_SUMMARY_WITH_ACTIVATION_EXPECTED_DATE_EXCEED)
  List<ProductLevel3Wip> findProductWipByExpectationActivationDateGreater(String storeId,
      String businessPartnerCode, Date currentDate);

  ProductLevel3Wip findByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku) throws Exception;

  ProductLevel3Wip findByStoreIdAndProductSkuAndMarkForDeleteTrue(String storeId, String productSku) throws Exception;

  ProductLevel3Wip findByStoreIdAndProductSku(String storeId, String productSku);

}
