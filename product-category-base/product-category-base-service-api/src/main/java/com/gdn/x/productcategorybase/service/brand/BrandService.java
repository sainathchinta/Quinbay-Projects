package com.gdn.x.productcategorybase.service.brand;

import java.util.List;

import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.dto.BrandDTO;
import com.gdn.x.productcategorybase.dto.BrandSummaryFilterDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandPredefinedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandlogoPath;
import com.gdn.x.productcategorybase.dto.response.BrandSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.gdn.x.productcategorybase.entity.brand.Brand;

public interface BrandService {

  String PREFIX_BRAND_CODE = "BRD";

  String create(Brand brand) throws Exception;

  /**
   * @param brand
   * @param brandRequestCode
   * @param brandLogo
   * @param profileBanner
   * @param username
   * @param skuCreationAllowedForAllSellers
   * @throws Exception
   */
  UpdateBrandlogoPath update(Brand brand, String brandRequestCode, String brandLogo, String profileBanner,
      String username, Boolean skuCreationAllowedForAllSellers) throws Exception;

  /**
   * Delete brand
   * @param brandCode
   * @param brandDeletedReason
   * @return
   * @throws Exception
   */
  String delete(String brandCode, String brandDeletedReason) throws Exception;
  
  Brand undelete(Brand brand) throws Exception;

  Brand findByBrandCode(String brandCode) throws Exception;

  Brand findByBrandName(String brandName, boolean markForDelete) throws Exception;

  Page<Brand> findSummaryByFilter(BrandSummaryFilterDTO filter, String storeId) throws Exception;
  
  Page<Brand> findSummaryByName(String brandName, Pageable pageable) 
      throws Exception;

  /**
   * API to fetch brand names by list of brandCodes
   * @param brandCodes
   * @return
   * @throws Exception
   */

  List<BrandDTO> findBrandNamesByBrandCodes(List<String> brandCodes) throws Exception;

  /**
   * Get brand suggestions from solr
   *
   * @param storeId
   * @param value
   * @param businessPartnerCode
   * @param pageable
   * @param isExternal
   * @return
   */
  Page<PredefinedAllowedAttributeValueResponse> getBrandSuggestions(String storeId, String value,
      String businessPartnerCode, Pageable pageable, boolean isSearch, boolean isExternal) throws Exception;


  /**
   * Get brand detail by name and state
   *
   * @param storeId
   * @param brandCode
   * @param status
   * @return
   */
  BrandPredefinedAttributeValueResponse getBrandPredefinedValueByCodeAndState(String storeId,
      String brandCode, String status) throws Exception;

  /**
   * Get default brands
   * @return
   */
  List<PredefinedAllowedAttributeValueResponse> getDefaultBrands(String storeId) throws Exception;

  /**
   * For getting brand summary in paginated form
   * @param storeId
   * @param page
   * @param size
   * @return
   * @throws Exception
   */
  Page<BrandSummaryResponse> getBrandSummaryResponseForValidBrands(String storeId, int page, int size)
      throws Exception;

  /**
   * Get brand detail from brand cache
   *
   * @param storeId
   * @param brandCode
   * @return
   * @throws Exception
   */
  BrandResponse findByBrandCodeCached(String storeId, String brandCode) throws Exception;

  /**
   * Get list of protected brands
   * @param storeId
   * @return
   */
  List<ProtectedBrandResponse> getProtectedBrandList(String storeId);

  /**
   * Get brand Response By brand Name cached
   *
   * @param storeId 10001
   * @param brandName String non null
   * @param markForDelete boolean
   * @param activeBrandsOnly boolean
   * @return BrandResponse
   * @throws Exception
   */
  BrandResponse getBrandResponseByBrandName(String storeId, String brandName, boolean markForDelete,
    boolean activeBrandsOnly) throws Exception;

  /**
   * Get brand Response By brand Code cached
   *
   * @param storeId 10001
   * @param brandCode String non null
   * @return BrandResponse
   * @throws Exception
   */
  BrandResponse getBrandResponseByBrandCode(String storeId, String brandCode) throws Exception;
  /**
   * delete All Brand Cache by brand code and brand name for both active and inactive brands
   *
   * @param storeId 10001
   * @param brandCode String
   * @param brandName String
   */
  void deleteAllBrandCache(String storeId, String brandName, String brandCode);

  /**
   * Get brand-code of a product with product-code
   *
   * @param storeId
   * @param productCode
   * @return
   * @throws Exception
   */
  String getBrandCodeByProductCode(String storeId, String productCode) throws Exception;

  /**
   * Update only brand-name for a particular brand-code
   *
   * @param storeId
   * @param brandName
   * @param brandCode
   * @return
   * @throws Exception
   */
  Triple<BrandHistoryEventModel, BrandWip, Brand> updateOnlyBrandName(String storeId,
      String brandName, String brandCode) throws Exception;
}
