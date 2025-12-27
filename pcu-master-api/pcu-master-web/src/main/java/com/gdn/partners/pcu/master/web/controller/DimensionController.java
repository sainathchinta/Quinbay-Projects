package com.gdn.partners.pcu.master.web.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.client.model.DimensionFilterRequest;
import com.gdn.partners.pcu.master.client.model.DimensionMappingResponse;
import com.gdn.partners.pcu.master.client.model.DimensionResponse;
import com.gdn.partners.pcu.master.model.DimensionApiPath;
import com.gdn.partners.pcu.master.service.DimensionService;
import com.gdn.partners.pcu.master.web.model.request.DimensionWebRequest;
import com.gdn.partners.pcu.master.web.model.request.EditDimensionWebRequest;
import com.gdn.partners.pcu.master.web.model.request.ModifyDimensionMappingWebRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;


import java.util.Objects;


@Slf4j
@Tag(name = "Dimension API")
@RestController
@RequestMapping(value = DimensionApiPath.BASE_PATH)
@Validated
public class DimensionController {

  @Autowired
  private DimensionService dimensionService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "To get the dimension details using dimension code")
  @GetMapping(value = DimensionApiPath.FETCH_DIMENSION_DETAIL_BY_DIMENSION_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<DimensionResponse> fetchDimensionDetail(
    @PathVariable("dimensionCode") String dimensionCode) {
    return new GdnRestSingleResponse<>(dimensionService.fetchDimensionDetail(dimensionCode),
      clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Get dimensions list by filter request")
  @PostMapping(value = DimensionApiPath.FILTER, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<DimensionResponse> getDimensionListByFilter(
    @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "20") int size,
    @RequestBody DimensionFilterRequest dimensionFilterRequest) {
    GdnRestListResponse<DimensionResponse> response =
      dimensionService.fetchDimensionListing(dimensionFilterRequest, page, size);
    response.setRequestId(clientParameterHelper.getRequestId());
    return response;
  }

  @Operation(summary = "Create new dimension")
  @PostMapping(value = DimensionApiPath.CREATE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse saveNewDimension(@Valid @RequestBody DimensionWebRequest request){
    GdnBaseRestResponse response = dimensionService.save(request);
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(), response.isSuccess(),
        this.clientParameterHelper.getRequestId());
  }

  @Operation(summary = "To get the dimension mapping using attribute code")
  @GetMapping(value = DimensionApiPath.DIMENSION_MAPPING_LIST, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<DimensionMappingResponse> fetchDimensionMapping(
      @PathVariable("attributeCode") String attributeCode,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size) {
    GdnRestListResponse<DimensionMappingResponse> response =
        dimensionService.fetchDimensionMapping(attributeCode, page, size);
    response.setRequestId(clientParameterHelper.getRequestId());
    return response;
  }

  @Operation(summary = "To update add or delete the dimension mapping using attribute code")
  @PostMapping(value = DimensionApiPath.UPDATE_DIMENSION_MAPPING_LIST, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse modifyDimensionMapping(
      @PathVariable("attributeCode") String attributeCode,
      @RequestBody ModifyDimensionMappingWebRequest modifyDimensionMappingWebRequest) throws Exception {
    GdnBaseRestResponse response =
        dimensionService.modifyDimensionMapping(attributeCode, modifyDimensionMappingWebRequest);
    response.setRequestId(clientParameterHelper.getRequestId());
    return response;
  }

  @Operation(summary = "Edit an existing dimension")
  @PostMapping(value = DimensionApiPath.EDIT, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse editDimension(@PathVariable("dimensionCode") String dimensionCode,
      @Valid @RequestBody EditDimensionWebRequest request) {
    GdnBaseRestResponse response = dimensionService.edit(dimensionCode, request);
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(), response.isSuccess(),
        this.clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Validate duplicate dimension name")
  @GetMapping(value =  DimensionApiPath.VALIDATE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse validateDimensionName(@RequestParam("dimensionName") String dimensionName) {
    GdnRestSingleResponse<DimensionResponse> response = dimensionService.validate(dimensionName);
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(),
        Objects.isNull(response.getValue()), this.clientParameterHelper.getRequestId());
  }
}
