package com.gdn.x.productcategorybase.service;

import java.util.List;
import java.util.Set;

import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.base.service.GdnBaseService;
import com.gdn.x.productcategorybase.AttributeSortType;
import com.gdn.x.productcategorybase.dto.AttributeOptionDTO;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;

public interface AllowedAttributeValueService extends GdnBaseService<AllowedAttributeValue> {

  /**
   * Find allowedAttributeValue by storeId, attribute and value
   * @param storeId
   * @param attribute
   * @param value
   * @return
   */
  List<AllowedAttributeValue> findByStoreIdAndAttributeAndValue(String storeId, Attribute attribute,
      String value);

  AllowedAttributeValue findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(String storeId, Attribute attribute,
      String value);

  AllowedAttributeValue findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(String storeId, Attribute attribute,
      String value);

  AllowedAttributeValue findByStoreIdAndId(String storeId, String id);

  /**
   * fetch list of Allowed Attribute Values by id
   *
   * @param storeId store id default value "10001"
   * @param ids list of allowed attribute id (Defining Attribute Ids)
   * @return
   */
  List<AllowedAttributeValue> findByStoreIdAndIds(String storeId, Set<String> ids);

  /**
   * get allowed attribute values based on attribute id and sort it based on sort type
   *
   * @param storeId
   * @param attributeId
   * @param pageable
   * @return
   */
  Page<AllowedAttributeValue> findByStoreIdAndAttributeId(String storeId, String attributeId, Pageable pageable,
      AttributeSortType sortType);

  String getSequence(String attributeCode);

  void markForDeleteAllowedAttributeValue(String storeId, String id) throws Exception;
  
  /**
   * Return allowed attribute value for both predefined and defining attribute. User input is list of attribute codes and values.
   *
   * @param request
   * @return
   * @throws Exception
   */
  List<AllowedAttributeValueDtoResponse> findAllowedPredefiningAndDefiningAttributeValue(List<AllowedAttributeValueDtoRequest> request) throws Exception;
  
  
  /**
   * Return attribute's options by attributeCode and the search keyword
   * @param attributeCode
   * @param keyword
   * @return
   * @throws Exception
   */
  Page<AttributeOptionDTO> getAttributeOptionsByAttributeCodeAndKeyword(String attributeCode,
      String keyword, Pageable pageable) throws Exception;

  /**
   *
   * @param storeId
   * @param attributeId
   * @return
   */
  List<AllowedAttributeValue> getAllowedAttributeValuesByStoreIdAndAttributeId(String storeId, String attributeId);

  /**
   * get allowed attribute value by id and value
   * @param storeId
   * @param attributeId
   * @param value
   * @return
   */
  AllowedAttributeValue getAllowedAttributeValuesByStoreIdAndAttributeIdAndValue(String storeId, String attributeId,
      String value);

  /**
   *
   * @param storeId
   * @param ids
   * @return
   */
  List<AllowedAttributeValue> getAllowedAttributeValuesByStoreIdAndIds(String storeId, Set<String> ids);

  /**
   * Get allowed attribute value based on allowed attribute code
   *
   * @param storeId
   * @param allowedAttributeCode
   * @return
   */
  AllowedAttributeValue findByStoreIdAndAllowedAttributeCode(String storeId, String allowedAttributeCode);

  /**
   * Get allowed attribute value based on allowed attribute code
   *
   * @param storeId
   * @param attribute
   * @param value
   * @return
   */
  List<AllowedAttributeValue> findByStoreIdAndAttributeAndValueOrderByMarkForDelete(String storeId, Attribute attribute,
      String value);
}
