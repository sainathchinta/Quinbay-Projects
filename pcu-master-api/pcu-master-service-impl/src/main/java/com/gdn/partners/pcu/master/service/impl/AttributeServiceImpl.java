package com.gdn.partners.pcu.master.service.impl;

import java.util.ArrayList;
import java.util.List;

import com.gdn.partners.pcu.master.client.model.AttributeResponse;
import com.gdn.partners.pcu.master.service.impl.helper.BeanUtils;
import com.gdn.partners.pcu.master.client.model.MasterAttributeRequest;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.model.attribute.AttributeValue;
import com.gdn.partners.pcu.master.model.request.AttributeValueAddServiceRequest;
import com.gdn.partners.pcu.master.model.request.AttributeValuesUpdateServiceRequest;
import com.gdn.partners.pcu.master.service.AttributeService;
import com.gdn.partners.pcu.master.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.master.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.master.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.master.web.model.request.MasterAttributeDTO;
import com.gdn.partners.pcu.master.web.model.response.AttributeDetailWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeValueWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeValuesWebResponse;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeSortTypeRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeFilterRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

/**
 * @author Pradeep Reddy
 */
@Service
public class AttributeServiceImpl implements AttributeService {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public GdnBaseRestResponse addAttribute(MasterAttributeRequest request) {
    return pcbFeign.saveMasterAttribute(request);
  }

  @Override
  public MasterAttributeResponse getAttributeDetail(String attributeCode) {
    GdnRestSingleResponse<MasterAttributeResponse> response = pcbFeign.getAttributeInfo(attributeCode);
    MasterAttributeResponse masterAttributeResponse = new MasterAttributeResponse();
    ResponseHelper.validateResponse(response);
    BeanUtils.copyProperties(response.getValue(), masterAttributeResponse, "sortType");
    masterAttributeResponse.setSortType((AttributeSortTypeRequest) response.getValue().getSortType());
    return masterAttributeResponse;
  }

  @Override
  public Page<AttributeValue> getAttributeValues(String attributeCode, Pageable pageable, Boolean getAllValues,
      boolean concatenateValueWithValueType) {
    GdnRestListResponse<AttributeValueResponse> response =
        pcbFeign.getAttributeValues(attributeCode, pageable.getPageNumber(), pageable.getPageSize(), getAllValues,
            concatenateValueWithValueType);
    ResponseHelper.validateResponse(response);
    List<AttributeValueResponse> attributeValueResponses = response.getContent();
    List<AttributeValue> attributeValues = new ArrayList<>();
    for (AttributeValueResponse attributeValueResponse : attributeValueResponses) {
      AttributeValue attributeValue = new AttributeValue();
      BeanUtils.copyProperties(attributeValueResponse, attributeValue);
      attributeValue.setAttributeCode(attributeCode);
      attributeValues.add(attributeValue);
    }
    return new PageImpl<>(attributeValues, pageable, response.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<MasterAttributeResponse> findByFilter(
      MasterAttributeFilterRequest masterAttributeFilterRequest, Pageable pageable) {
    GdnRestListResponse<MasterAttributeResponse> response = pcbFeign
        .getAttributesByAttributeFilter(masterAttributeFilterRequest, pageable.getPageNumber(),
            pageable.getPageSize());
    ResponseHelper.validateResponse(response);
    return new PageImpl<MasterAttributeResponse>(response.getContent(), pageable,
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public GdnBaseRestResponse updateAttribute(MasterAttributeDTO request) {
    GdnBaseRestResponse response =
        pcbFeign.updateMasterAttribute(RequestHelper.convertToMasterAttributeRequest(request));
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public GdnBaseRestResponse updateAttributeValues(String attributeCode,
      AttributeValuesUpdateServiceRequest attributeValuesUpdateServiceRequest) {
    GdnBaseRestResponse response = pcbFeign.updateMasterAttributeValues(attributeCode,
        RequestHelper.toMasterAttributeUpdateRequest(attributeValuesUpdateServiceRequest));
    ResponseHelper.validateResponse(response);
    return response;
  }

  @Override
  public AttributeValueResponse addAttributeValue(String attributeCode,
      AttributeValueAddServiceRequest attributeValueAddServiceRequest) {
    GdnRestSingleResponse<AttributeValueResponse> response = pcbFeign.addMasterAttributeValue(attributeCode,
        RequestHelper.toMasterAttributeAddRequest(attributeValueAddServiceRequest));
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public AttributeDetailWebResponse getAttributeDetails(String attributeCode) {
    GdnRestSingleResponse<AttributeResponse> gdnSingleResponse =
        pcbFeign.getAttributeDetailAndValuesByAttributeCode(attributeCode);
    ResponseHelper.validateResponse(gdnSingleResponse);
    return ResponseHelper.toAttributeDetailWebResponse(gdnSingleResponse.getValue());
  }

  @Override
  public List<AttributeValuesWebResponse> getAttributeValuesByAttributeCodes(List<String> attributeCodes) {
    AttributeCodesRequest request = new AttributeCodesRequest();
    request.setAttributeCodes(attributeCodes);
    GdnRestListResponse<AttributeResponse> response = pcbFeign.getAttributeValuesByAttributeCodes(request);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toAttributeValuesWebResponseList(response.getContent());
  }

  @Override
  public List<AttributeValueWebResponse> getPredefinedAllowedAttributesByAttributeIdAndValue(String attributeId,
      String value) {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response =
        pcbFeign.getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(attributeId, value, 0,
            Integer.MAX_VALUE);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toAttributeValueWebResponseList(response.getContent());
  }

  @Override
  public List<AttributeValueWebResponse> getSpecificPredefinedAllowedAttributesByAttributeIdAndValue(String attributeId,
      String value) {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response =
        pcbFeign.getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue(attributeId, value);
    ResponseHelper.validateResponse(response);
    if (CollectionUtils.isEmpty(response.getContent())) {
      throw new InvalidStateException(ErrorMessages.BRAND_REJECTED_ERR_MESSAGE);
    }
    return ResponseHelper.toAttributeValueWebResponseList(response.getContent());
  }
}
