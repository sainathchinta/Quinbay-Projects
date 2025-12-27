package com.gdn.partners.pcu.external.service;

import java.util.List;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.BrandInReviewResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.partners.pcu.external.web.model.response.PredefinedAttributeValueWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandSummaryRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

public interface BrandService {

  /**
   * Get brand suggestions by value/keyword
   *
   * @param value
   * @param mandatoryParameterHelper
   * @param isSearch
   * @param page
   * @param size
   * @return
   */
  Page<PredefinedAttributeValueWebResponse> getBrandSuggestions(String value, MandatoryParameterHelper mandatoryParameterHelper, boolean isSearch, Integer page, Integer size);

  /**
   * Get brand suggestions by value/keyword
   *
   * @param value
   * @param businessPartnerCode
   * @param isSearch
   * @param isExternal
   * @return
   */
  List<PredefinedAllowedAttributeValueResponse> getBrandSuggestions(String value, String businessPartnerCode, boolean isSearch, boolean isExternal);

  /**
   * Get all active brands
   * @param request
   * @param pageable
   * @return
   */
  Page<BrandResponse> findSummaryByFilter(BrandSummaryRequest request, Pageable pageable);

  /**
   * Checks whether a brand already exists with given brandName
   * @param brandName
   * @return
   */
  BrandResponse findByBrandName(String brandName);


  /**
   * API to get list of active brands for a category
   * @param categoryId
   * @return
   */
  List<PredefinedAllowedAttributeValueResponse> activeBrandsByCategoryId(String categoryId) throws Exception;

  /**
   * Get default brands
   * @return
   */
  List<PredefinedAttributeValueWebResponse> getDefaultBrands(MandatoryParameterHelper mandatoryParameterHelper);

  /**
   * Validate authorised brand
   *
   * @param brandCode
   * @param businessPartnerCode
   * @return
   */
  boolean validateAuthorisedBrand(String brandCode, String businessPartnerCode);

  /**
   * Get list of all in review brands
   * @param storeId
   * @param requestId
   * @return
   */
  List<BrandInReviewResponse> getAllInReviewBrand(String storeId, String requestId);
}
