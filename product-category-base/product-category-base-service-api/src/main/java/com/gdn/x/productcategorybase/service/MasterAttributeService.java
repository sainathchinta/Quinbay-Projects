package com.gdn.x.productcategorybase.service;

import java.util.Date;
import java.util.List;

import com.gdn.x.productcategorybase.dto.request.DimensionMappingRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.dto.AttributeValueDTO;
import com.gdn.x.productcategorybase.dto.AttributeValueUpdateDTO;
import com.gdn.x.productcategorybase.dto.MasterAttributeUpdateDTO;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.entity.Attribute;

import com.gdn.x.productcategorybase.AttributeFilter;

public interface MasterAttributeService {

  /**
   * Get sorted attribute values based on attribute code and sort type
   *
   * @param storeId
   * @param attributeCode
   * @param pageable
   * @param concatenateValueWithValueType
   * @return
   * @throws Exception
   * @Param getAllValues
   */
  Page<AttributeValueDTO> getAttributeValuesByAttributeCode(String storeId, String attributeCode, Pageable pageable,
      Boolean getAllValues, boolean concatenateValueWithValueType) throws Exception;

  /**
   * return attribute detail
   *
   * @param attributeCode
   * @return attribute
   */
  Attribute findDetailByAttributeCode(String attributeCode);

  /**
   * Service method to insert {@link Attribute}
   *
   * @param attribute               - attribtue
   * @param dimensionMappingRequest
   * @param valueTypeList
   * @return uuid
   * @throws Exception if not valid attribute
   */
  String insertMasterAttribute(Attribute attribute, List<DimensionMappingRequest> dimensionMappingRequest,
      List<String> valueTypeList) throws Exception;

  /**
   * Update attribute values
   * @param storeId
   * @param attributeCode
   * @param masterAttributeUpdateDTO
   * @throws Exception
   */
  void updateAttributeValues(String storeId, String attributeCode,
      MasterAttributeUpdateDTO masterAttributeUpdateDTO) throws Exception;

  /**
   * Add attribute value for an existing attribute.
   *
   * @param storeId
   * @param attributeCode
   * @param attributeValueUpdateDTO
   * @param createdBy
   * @param createdDate
   * @return
   * @throws Exception
   */
  AttributeValueResponse addAttributeValue(String storeId, String attributeCode,
      AttributeValueUpdateDTO attributeValueUpdateDTO, String createdBy, Date createdDate) throws Exception;

  /**
   * Update master attribute information
   *
   * @param attribute
   * @param valueTypes
   * @return
   * @throws Exception
   */
  Attribute updateMasterAttribute(Attribute attribute, List<String> valueTypes) throws Exception;

  /**
   *  Get attribute list by AttributeFilter
   * @param storeId
   * @param attributeFilter
   * @param pageable
   * @return
   */
  Page<Attribute> getAttributeByAttributeFilter(String storeId, AttributeFilter attributeFilter,
      Pageable pageable);
}
