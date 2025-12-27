package com.gdn.partners.pcu.master.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.client.model.DimensionFilterRequest;
import com.gdn.partners.pcu.master.client.model.DimensionRequest;
import com.gdn.partners.pcu.master.client.model.DimensionMappingResponse;
import com.gdn.partners.pcu.master.client.model.DimensionResponse;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.service.DimensionService;
import com.gdn.partners.pcu.master.service.impl.helper.BeanUtils;
import com.gdn.partners.pcu.master.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.master.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.master.web.model.request.DimensionWebRequest;
import com.gdn.partners.pcu.master.web.model.request.ModifyDimensionMappingWebRequest;
import com.gdn.partners.pcu.master.web.model.request.EditDimensionWebRequest;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class DimensionServiceImpl implements DimensionService {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public GdnRestListResponse<DimensionResponse> fetchDimensionListing(
    DimensionFilterRequest dimensionFilterRequest, int page, int size) {
    GdnRestListResponse<DimensionResponse> response =
      pcbFeign.fetchDimensionListing(page, size, dimensionFilterRequest);
    ResponseHelper.validateResponseByErrorCode(response);
    return response;
  }

  @Override
  public DimensionResponse fetchDimensionDetail(String dimensionCode) {
    GdnRestSingleResponse<DimensionResponse> response =
      pcbFeign.fetchDimensionDetail(dimensionCode);
    ResponseHelper.validateResponseByErrorCode(response);
    return response.getValue();
  }

  @Override
  public GdnBaseRestResponse save(DimensionWebRequest dimensionWebRequest) {
    RequestHelper.checkArgument(StringUtils.isNotBlank(dimensionWebRequest.getName()),
        ErrorMessages.DIMENSION_NAME_SHOULD_NOT_BE_BLANK);
    RequestHelper.checkArgumentsOfDimension(dimensionWebRequest.getNameEnglish(),
        dimensionWebRequest.getDescription(), dimensionWebRequest.getDescriptionEnglish(),
        dimensionWebRequest.getExample());
    DimensionRequest dimensionRequest = new DimensionRequest();
    BeanUtils.copyProperties(dimensionWebRequest, dimensionRequest);
    GdnBaseRestResponse response = pcbFeign.createDimension(dimensionRequest);
    ResponseHelper.validateResponseByErrorCode(response);
    return response;
  }

  @Override
  public GdnRestListResponse<DimensionMappingResponse> fetchDimensionMapping(String attributeCode,
      int page, int size) {
    GdnRestListResponse<DimensionMappingResponse> response =
        pcbFeign.fetchDimensionMapping(attributeCode, page, size);
    ResponseHelper.validateResponseByErrorCode(response);
    return response;
  }

  @Override
  public GdnBaseRestResponse modifyDimensionMapping(String attributeCode,
      ModifyDimensionMappingWebRequest modifyDimensionMappingWebRequest) {
    GdnBaseRestResponse response = pcbFeign.modifyDimensionMapping(attributeCode,
        RequestHelper.convertToModifyDimensionMappingRequest(modifyDimensionMappingWebRequest));
    ResponseHelper.validateResponseByErrorCode(response);
    return response;
  }

  @Override
  public GdnBaseRestResponse edit(String dimensionCode,
      EditDimensionWebRequest editDimensionWebRequest) {
    RequestHelper.checkArgumentsOfDimension(editDimensionWebRequest.getNameEnglish(),
        editDimensionWebRequest.getDescription(), editDimensionWebRequest.getDescriptionEnglish(),
        editDimensionWebRequest.getExample());
    DimensionRequest dimensionRequest = new DimensionRequest();
    BeanUtils.copyProperties(editDimensionWebRequest, dimensionRequest);
    dimensionRequest.setDimensionCode(dimensionCode);
    GdnBaseRestResponse response = pcbFeign.editDimension(dimensionRequest);
    ResponseHelper.validateResponseByErrorCode(response);
    return response;
  }

  @Override
  public GdnRestSingleResponse<DimensionResponse> validate(String dimensionName) {
    GdnRestSingleResponse<DimensionResponse> response = pcbFeign.findByName(dimensionName);
    ResponseHelper.validateResponseByErrorCodeExcludeValue(response);
    return response;
  }
}
