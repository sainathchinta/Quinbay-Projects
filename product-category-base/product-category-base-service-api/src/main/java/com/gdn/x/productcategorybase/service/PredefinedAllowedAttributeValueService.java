package com.gdn.x.productcategorybase.service;

import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.base.service.GdnBaseService;
import com.gdn.common.exception.ApplicationException;
import com.gdn.x.productcategorybase.AttributeSortType;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;

public interface PredefinedAllowedAttributeValueService extends GdnBaseService<PredefinedAllowedAttributeValue> {
  void deactivated(String predefinedAllowedAttributeValueId, String storeId) throws Exception;

  /**
   * Find predefinedAllowedAttributeValue by storeId, attribute and value
   * @param storeId
   * @param attribute
   * @param value
   * @return
   */
  List<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeAndValue(String storeId, Attribute attribute,
      String value);

  PredefinedAllowedAttributeValue findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(String storeId,
      Attribute attribute, String value);

  PredefinedAllowedAttributeValue findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(String storeId,
      Attribute attribute, String value);

  Page<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(
      String storeId, String value, String attributeId, Pageable pageable);

  /**
   * Fetch predefined allowed attribute value based on value and attribute Id
   *
   * @param storeId
   * @param value
   * @param attributeId
   * @return
   */
  List<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(String storeId,
      String value, String attributeId);

  PredefinedAllowedAttributeValue findByStoreIdAndId(String storeId, String id);

  String getSequence(String attributeCode);

  void markForDeletePredefinedAllowedAttributeValue(String storeId,
      List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues) throws Exception;

  void markForDeletePredefinedAllowedAttributeValue(String storeId, String id) throws Exception;

  void saveWithGeneratedCode(String storeId, String attributeId,
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue) throws Exception;
  
  PredefinedAllowedAttributeValue findByStoreIdAndPredefinedAllowedAttributeCode(String storeId,
      String predefinedAllowedAttributeCode) throws Exception;
  
  PredefinedAllowedAttributeValue findTopByStoreIdAndAttributeCodeAndValueOrPredefinedAllowedAttributeCode(
      String storeId, String attributeCode, String value, boolean fetchByPredefinedAttributeCode);

  /**
   * Get brand suggestions
   *
   * @param storeId
   * @param value
   * @param businessPartnerCode
   * @param pageable
   * @param isSearch
   * @param isExternal
   * @return
   * @throws Exception
   */
  Page<PredefinedAllowedAttributeValueResponse> getBrandSuggestions(String storeId, String value, String businessPartnerCode,
      Pageable pageable, boolean isSearch, boolean isExternal) throws Exception;

  /**
   * Get predefined allowed attribute values based on attribute Id
   *
   * @param storeId
   * @param attributeId
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeId(String storeId, String attributeId,
      Pageable pageable, AttributeSortType sortType) throws Exception;
  /**
   * fetch list of Predefined Allowed Attribute Values by id
   *
   * @param storeId store id default value "10001"
   * @param ids list of allowed attribute id
   * @return
   */
  List<PredefinedAllowedAttributeValue> findByStoreIdAndIds(String storeId, Set<String> ids);

  /**
   * fetch list of Predefined Allowed Attribute Values by id
   *
   * @param storeId store id default value "10001"
   * @param predefinedAllowedAttributeCodes list of predefined attribute Codes
   * @return
   */
  List<PredefinedAllowedAttributeValue> findByStoreIdAndPredefinedAllowedAttributeCodes(String storeId,
      Set<String> predefinedAllowedAttributeCodes);

  /**
   * Saves a PredefinedAllowedAttributeValue to the table
   * @param predefinedAllowedAttributeValue
   */
  void add(PredefinedAllowedAttributeValue predefinedAllowedAttributeValue);

  /**
   * Generates PredefinedAllowedAttributeValue which is to be stored to the table
   *
   * @param brandWip
   * @param attribute
   * @return
   */
  PredefinedAllowedAttributeValue generatePredefinedAllowedAttributeValue(BrandWip brandWip, Attribute attribute);

  /**
   * Update brandRequestCode to brandCode for approvedBrand
   * @param storeId
   * @param PredefinedAllowedAttributeCode
   * @param brand
   */
  void updatePredefinedAllowedAttributeCodeForApprovedBrand(String storeId, String PredefinedAllowedAttributeCode,
      Brand brand) throws ApplicationException;

  void updatePredefinedAllowedAttributeCodeForRejectedBrand(String storeId, String brandRequestCode)
      throws ApplicationException;

  /**
   *
   * @param storeId
   * @param attributeId
   * @return
   */
  List<PredefinedAllowedAttributeValue> getPredefinedAllowedAttributeValuesByStoreIdAndAttributeId(
      String storeId, String attributeId);

  /**
   *
   * @param storeId
   * @param ids
   * @return
   */
  List<PredefinedAllowedAttributeValue> getPredefinedAllowedAttributeValuesByStoreIdAndIds(
      String storeId, Set<String> ids);

  /**
   * get get predefined allowed attribute values based on attribute id and value and sort it based on mark for delete
   *
   * @param storeId
   * @param attribute
   * @param value
   * @return
   */
  List<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeAndValueOrderByMarkForDelete(String storeId,
      Attribute attribute, String value);

  /**
   * get predefined allowed attribute by id and value
   * @param storeId
   * @param id
   * @param value
   * @return
   */
  List<PredefinedAllowedAttributeValue> getPredefinedAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(
      String storeId, String id, String value);

  /**
   * Add predefined allowed attribute value
   *
   * @param storeId   String
   * @param attribute Attribute
   * @param updatedBy String
   * @param value     String
   * @param sequence  Integer
   * @param valueEn
   * @return PredefinedAllowedAttributeValue
   */
  PredefinedAllowedAttributeValue addPredefinedAllowedAttributeValue(String storeId, Attribute attribute,
      String updatedBy, String value, Integer sequence, String valueEn);

  /**
   * Fetch predefined values by ignore case
   * @param storeId storeId String
   * @param attributeId attributeId String
   * @param value value to query with lower case
   * @return Return the value
   */
  List<PredefinedAllowedAttributeValue> getAttributeIdAndValueLikeIgnoreCase(
    String storeId, String attributeId, String value);
}
