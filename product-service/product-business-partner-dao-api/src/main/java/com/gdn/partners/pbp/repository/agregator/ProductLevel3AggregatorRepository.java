package com.gdn.partners.pbp.repository.agregator;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator.ProductLevel3AggregatorState;

public interface ProductLevel3AggregatorRepository extends JpaRepository<ProductLevel3Aggregator, String> {

  String QUERY_COUNT_SUMMARY_BY_STORE_ID_AND_BUSINESS_PARTNER_CODE = "select e.oos, e.minimum_stock, count(1) from pbp_product_level3_aggregator e"
      + " where e.store_id = :storeId and e.business_partner_code = :businessPartnerCode and e.mark_for_delete = false and e.state = 'ACTIVE'"
      + " group by e.oos, e.minimum_stock";

  String QUERY_COUNT_OOS_STOCK = "select oos, count(oos) from pbp_product_level3_aggregator "
      + " where store_id = :storeId and business_partner_code = :businessPartnerCode and mark_for_delete = false and state = 'ACTIVE'"
      + " group by oos";

  String QUERY_COUNT_MINIMUM_STOCK = "select count(minimum_stock) from pbp_product_level3_aggregator "
      + " where store_id = :storeId and business_partner_code = :businessPartnerCode and mark_for_delete = false and state = 'ACTIVE'"
      + " and minimum_stock = true";
  
  ProductLevel3Aggregator findByStoreIdAndGdnSkuAndMarkForDeleteFalse(String storeId, String gdnSku) throws Exception;

  @Query(value = QUERY_COUNT_SUMMARY_BY_STORE_ID_AND_BUSINESS_PARTNER_CODE, nativeQuery = true)
  List<Object[]> countSummaryByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(@Param(value = "storeId") String storeId,
      @Param(value = "businessPartnerCode") String businessPartnerCode) throws Exception;

  @Query(value = QUERY_COUNT_OOS_STOCK, nativeQuery = true)
  List<Object[]> countSummaryOosByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(
      @Param(value = "storeId") String storeId, @Param(value = "businessPartnerCode") String businessPartnerCode);

  @Query(value = QUERY_COUNT_MINIMUM_STOCK, nativeQuery = true)
  long countSummaryMinimumStockByStoreIdAndBusinessPartnerCodeAndMarkForDeleteFalse(
      @Param(value = "storeId") String storeId, @Param(value = "businessPartnerCode") String businessPartnerCode);

  Page<ProductLevel3Aggregator> findByStoreIdAndBusinessPartnerCodeAndStateAndOosAndMarkForDeleteFalse(String storeId,
      String businessPartnerCode, ProductLevel3AggregatorState state, boolean oos, Pageable pageable) throws Exception;

  Page<ProductLevel3Aggregator> findByStoreIdAndBusinessPartnerCodeAndStateAndOosAndMarkForDeleteFalseOrderByUpdatedDateDesc(
      String storeId, String businessPartnerCode, ProductLevel3AggregatorState state, boolean oos, Pageable pageable)
      throws Exception;

  Page<ProductLevel3Aggregator> findByStoreIdAndBusinessPartnerCodeAndStateAndMinimumStockAndOosAndMarkForDeleteFalse(
      String storeId, String businessPartnerCode, ProductLevel3AggregatorState state, boolean minimumStock,
      boolean oos, Pageable pageable) throws Exception;

  List<ProductLevel3Aggregator> findByStoreIdAndGdnSkuInAndMarkForDeleteFalse(String storeId, List<String> gdnSkus)
      throws Exception;
}
