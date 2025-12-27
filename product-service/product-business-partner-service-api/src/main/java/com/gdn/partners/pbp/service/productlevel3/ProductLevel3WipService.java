package com.gdn.partners.pbp.service.productlevel3;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.gdn.mta.product.entity.StateCountDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.model.vo.CacheKeys;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.mta.product.valueobject.ProductLevel3WipDTO;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
import com.gdn.partners.pbp.entity.productlevel3.CountProductLevel3Wip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3Wip;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

public interface ProductLevel3WipService {

  Page<ProductLevel3WipDTO> findSummaryByFilterWithState(ProductLevel3WipSummaryRequest request, Pageable pageable) throws Exception;

  CountProductLevel3Wip countSummaryWithState(String businessPartnerCode) throws Exception;

  /**
   * Get product counts by filter type
   *
   * @param businessPartnerCode
   * @param storeId
   * @param type
   * @return
   * @throws Exception
   */
  ProductLevel3CountResponse countSummaryByFilterType(String businessPartnerCode, String storeId, String type) throws Exception;

  void update(String productCode) throws Exception;

  void delete(String productCode) throws Exception;

  /**
   * Return for correction
   * @param productCode
   * @param notes
   * @throws Exception
   */
  void returnDraftForCorrection(String productCode, String notes) throws Exception;

  void resubmit(ProductRequest productRequest, UpdateProductLevel3Wip updateProductLevel3Wip,
      Date submitDate) throws Exception;

  /**
   * find product level 3 data by product sku
   * @param productSku
   * @param isActive
   * @return
   * @throws Exception
   */
  ProductLevel3Wip findByProductSku(String productSku, boolean isActive) throws Exception;

  /**
   * count for all in progress products who all not yet active before or on expected activation date
   *
   * @param storeId             must not blank
   * @param businessPartnerCode must not blank
   * @param curr                current date must not null
   * @return productWip row count
   */
  Integer findCountByExceedingActivationDate(String storeId, String businessPartnerCode, Date curr);


  /**
   * find all productWip in progress products who all not yet active before or on expected
   * activation date
   *
   * @param storeId             must not blank
   * @param businessPartnerCode must not blank
   * @param curr                current date must not null
   * @return list of ProductLevel3WipDTO
   */
  List<ProductLevel3WipDTO> findProductWipByExpectationActivationDateGreater(String storeId,
      String businessPartnerCode, Date curr);

  /**
   * find l3 product using l1 product code
   * @param storeId
   * @param productCode
   * @return
   * @throws Exception
   */
  List<ProductLevel3Wip> findProductL3WipByStoreIdAndProductCode(String storeId , String productCode)
      throws Exception;

  /**
   * send mail for exceeded activation
   * @param businessPartnerCode
   * @param username
   * @return
   * @throws Exception
   */
  void sendMailForEmailExceededActivation(String businessPartnerCode, String username) throws Exception;

  /**
   *
   * @param storeId
   * @param productSku
   * @return
   */
  ProductLevel3Wip getProductLevel3WipByProductSkuWithItemsInitialised(String storeId, String productSku);
  /**
   * Get product counts by state
   *
   * @param businessPartnerCode String
   * @return totalItemsByCriteria , totalItems
   * @throws Exception
   */
  CountProductLevel3Wip countSummaryWithStateCached(String businessPartnerCode)
    throws Exception;

  /**
   * Get product counts by filter type
   *
   * @param businessPartnerCode String
   * @param storeId string
   * @param type primary secondary
   * @return totalItemsByCriteria , totalItems
   * @throws Exception
   */
  ProductLevel3CountResponse countSummaryByFilterTypeCached(String storeId,
    String businessPartnerCode, String type) throws Exception;
}
