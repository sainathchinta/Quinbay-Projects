package com.gdn.partners.pcu.master.service;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.master.model.attribute.AttributeValue;
import com.gdn.partners.pcu.master.model.request.AttributeValueAddServiceRequest;
import com.gdn.partners.pcu.master.model.request.AttributeValuesUpdateServiceRequest;
import com.gdn.partners.pcu.master.web.model.request.MasterAttributeDTO;
import com.gdn.partners.pcu.master.web.model.response.AttributeDetailWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeValueWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeValuesWebResponse;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeFilterRequest;
import com.gdn.partners.pcu.master.client.model.MasterAttributeRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;


/**
 * @author Pradeep Reddy
 */
public interface AttributeService {

  /**
   * Service method to add master attribtue
   *
   * @param request
   * @return
   */
  GdnBaseRestResponse addAttribute(MasterAttributeRequest request);

  /**
   * api to get detail of a attribute
   *
   * @param attributeCode
   * @return MasterAttributeResponse
   */
  MasterAttributeResponse getAttributeDetail(String attributeCode);

  /**
   * Get attribute values based on attribute code
   *
   * @param attributeCode
   * @param pageable
   * @param getAllValues
   * @param concatenateValueWithValueType
   * @return
   */
  Page<AttributeValue> getAttributeValues(String attributeCode, Pageable pageable, Boolean getAllValues,
      boolean concatenateValueWithValueType);

  /**
   * Method to update master attribute
   *
   * @param request
   * @return
   */
  GdnBaseRestResponse updateAttribute(MasterAttributeDTO request);

  /**
   *
   * @param masterAttributeFilterRequest
   * @param pageable
   * @return
   */
  Page<MasterAttributeResponse> findByFilter(
      MasterAttributeFilterRequest masterAttributeFilterRequest, Pageable pageable);

  /**
   * @param attributeCode
   * @param attributeValuesUpdateServiceRequest
   * @return
   */
  GdnBaseRestResponse updateAttributeValues(String attributeCode,
      AttributeValuesUpdateServiceRequest attributeValuesUpdateServiceRequest);

  /**
   * Get attribute values by attribute codes
   *
   * @param attributeCodes
   * @return
   */
  List<AttributeValuesWebResponse> getAttributeValuesByAttributeCodes(List<String> attributeCodes);

  /**
   * Method to return predefined allowed attributes by attribute ID and value
   *
   * @param attributeId
   * @param value
   * @return
   */
  List<AttributeValueWebResponse> getPredefinedAllowedAttributesByAttributeIdAndValue(String attributeId, String value);

  /**
   * Method to return a specific predefined allowed attribute by attribute ID and value
   *
   * @param attributeId
   * @param value
   * @return
   */
  List<AttributeValueWebResponse> getSpecificPredefinedAllowedAttributesByAttributeIdAndValue(String attributeId,
      String value);

  /**
   * Method to add allowed attribute value for the given attribute code.
   * @param attributeCode
   * @param attributeValueAddServiceRequest
   * @return
   */
  AttributeValueResponse addAttributeValue(String attributeCode,
      AttributeValueAddServiceRequest attributeValueAddServiceRequest);

  /**
   * Method to get the attribute details using the attributeCode
   *
   * @param attributeCode
   * @return
   */
  AttributeDetailWebResponse getAttributeDetails(String attributeCode);

}
