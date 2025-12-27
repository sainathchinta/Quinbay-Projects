package com.gdn.x.productcategorybase.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.productcategorybase.dto.request.AttributeAndValueByTypeRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.base.service.GdnBaseService;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.AttributeBasicDetailDTO;
import com.gdn.x.productcategorybase.dto.AttributeSummaryDTO;
import com.gdn.x.productcategorybase.dto.CategoryAttributeDetailDTO;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.entity.Attribute;

public interface AttributeService extends GdnBaseService<Attribute> {

  void deleteAllowedAttributeValue(String storeId, String allowedAttributeValueId) throws Exception;

  void deletePredefinedAllowedAttributeValue(String storeId, String predefinedAllowedAttributeValueId) throws Exception;

  List<Attribute> findByAttributeCode(String storeId, String attributeCode);

  Page<Attribute> findByAttributeCode(String storeId, String attributeCode, Pageable pageable);

  List<Attribute> findByAttributeType(String storeId, AttributeType attributeType);

  Page<Attribute> findByAttributeType(String storeId, AttributeType attributeType, Pageable pageable);

  Attribute findById(String storeId, String id);

  /**
   * To get list of attributes given attribute Id list
   *
   * @param storeId
   * @param attributeIdList
   * @return
   */
  List<Attribute> findByAttributeIds(String storeId, List<String> attributeIdList);
  
  Page<Attribute> findByNameLikeIgnoreCase(String storeId, String name, Pageable pageable);

  Page<Attribute> findByNameStartingWith(String storeId, String name, Pageable pageable);

  List<Attribute> findBySearchAbleFalse(String storeId);

  Page<Attribute> findBySearchAbleFalse(String storeId, Pageable pageable);

  List<Attribute> findBySearchAbleTrue(String storeId);

  Page<Attribute> findBySearchAbleTrue(String storeId, Pageable pageable);

  /**
   * return attribute detail
   *
   * @param storeId must not null
   * @param id must not null
   * @param needCategoryAttributes
   * @return
   */
  Attribute findDetailByStoreIdAndId(String storeId, String id, boolean needCategoryAttributes);

  /**
   * return attribute detail
   *
   * @param storeId must not null
   * @param attributeCode must not null
   * @return
   */
  Attribute findDetailByStoreIdAndAttributeCode(String storeId, String attributeCode);


  /**
   * return attribute list
   *
   * @param attributeCode must not be empty
   * @param fetchOnlyBasicAttributeDetails boolean
   * @return
   */
  List<Attribute> findDetailByStoreIdAndAttributeCodes(List<String> attributeCode,
      boolean fetchOnlyBasicAttributeDetails);

  /**
   * get attribute by list of attribute codes
   * @param storeId
   * @param attributeCodes
   * @return
   */
  List<Attribute> findDetailByStoreIdAndAttributeCodeList(String storeId, List<String> attributeCodes);

  String getSequence(String attributeCode);

  CategoryAttributeDetailDTO getAttributeDetailByCategoryCode(String categoryCode,
      boolean concatenateValueWithValueType) throws Exception;
  
  void regenerateAllowedAttributeValue(String storeId, Attribute oldAttribute, Attribute newAttribute) throws Exception;

  /**
   * return all attribute detail by attribute codes
   * @param storeId String
   * @param attributeCodes List
   * @return List
   */
  List<Attribute> findAttributesByStoreIdAndAttributeCodeList(String storeId, List<String> attributeCodes);

  /**
   * return all attribute detail -except the options- by category code
   * @param storeId
   * @param categoryCode
   * @return
   * @throws Exception
   */
  List<AttributeSummaryDTO> getAttributeDetailByCategoryCodeWithoutOptions(
      String storeId, String categoryCode) throws Exception;

  /**
   * Returns attribute searched by name of attribute
   * @param storeId
   * @param name
   * @return
   * @throws Exception
   */
  List<Attribute> findByName(String storeId, String name) throws Exception;

  /**
   * Get attribute details by categoryCode
   *
   * @param storeId
   * @param categoryCode
   * @return
   */
  List<AttributeBasicDetailDTO> getAttributeBasicDetailByCategoryCode(String storeId, String categoryCode);

  /**
   * evict attribute cache by attribute ID
   *
   * @param storeId
   * @param attributeId
   */
  void evictAttributeCache(String storeId, String attributeId, String attributeCode);

  /**
   *
   * @param storeId
   * @param attributeId
   * @return
   */
  Attribute getAttributeByStoreIdAndAttributeIdAndMarkForDeleteFalse(String storeId, String attributeId);

  /**
   * get attribute detail by id and value
   *
   * @param storeId
   * @param id
   * @param value
   * @param ignoreCaseFetch
   * @return
   */
  AttributeResponse findDetailByStoreIdAndIdAndValue(String storeId, String id,String value,
    boolean ignoreCaseFetch);

  /**
   *
   * @param storeId
   * @param attributeIds
   * @return
   */
  List<Attribute> getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalse(String storeId, List<String> attributeIds);

  /**
   *
   * @param storeId
   * @param attributeCode
   * @return
   */
  Attribute getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(String storeId, String attributeCode);

  /**
   *
   * @param storeId
   * @param attributeCode
   * @param name
   * @param value
   * @param attributeType
   * @return
   */
  Attribute findAttributeByCodeOrNameAndValue(String storeId, String attributeCode, String name, String value,
      String attributeType);

  /**
   *
   * @param storeId
   * @param name
   * @param attributeType
   * @return
   */
  List<Attribute> findByStoreIdAndNameAndAttributeTypeAndMarkForDeleteFalse(String storeId, String name, AttributeType attributeType);

  /**
   * get attribute values by product code and attribute code
   *
   * @param storeId
   * @param productCode
   * @param attributeCode
   * @return
   */
  String findProductAttributeValuesByProductCodeAndAttributeCode(String storeId, String productCode, String attributeCode);

  /**
   * Get categoryCodes mapped to attributeCode
   * @param storeId
   * @param attributeCode
   * @return
   */
  List<String> findCategoryCodesByAttributeCode(String storeId, String attributeCode);

  /**
   * filters Seller Hidden Attributes
   * @param attributeAndValueMap attributes and its allowed/predefined values map
   * @param attributeAndValueMap Set of allowed attribute value IDs.
   * @param productSuitabilityFeatureEnabled product suitability feature
   * * @return
   */
  void filterSellerHiddenAttributes(
    Map<AttributeAndValueByTypeRequest, Object> attributeAndValueMap,
    boolean productSuitabilityFeatureEnabled, String productCode);

  /**
   * Fetches attributes and its allowed/predefined values into a map.
   *
   * @param storeId                               Store ID.
   * @param allowedAttributeValuesIds             Set of allowed attribute value IDs.
   * @param predefinedAllowedAttributeValuesIds   Set of predefined allowed attribute value IDs.
   * @param predefinedAllowedAttributeValuesCodes Set of predefined allowed attribute value codes.
   * @param attributesIds                         List of attribute IDs.
   * @param productSuitabilityFeatureEnabled      product suitability feature
   * @param productCode                           product code
   */
  Map<AttributeAndValueByTypeRequest, Object> getAttributeAndValueMap(String storeId,
    Set<String> allowedAttributeValuesIds, Set<String> predefinedAllowedAttributeValuesIds,
    Set<String> predefinedAllowedAttributeValuesCodes, List<String> attributesIds,
    boolean productSuitabilityFeatureEnabled, String productCode);
}
