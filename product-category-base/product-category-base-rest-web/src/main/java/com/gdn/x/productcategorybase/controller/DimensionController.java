package com.gdn.x.productcategorybase.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.DimensionApiPath;
import com.gdn.x.productcategorybase.dto.request.DimensionFilterRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionRequest;
import com.gdn.x.productcategorybase.dto.response.DimensionMappingResponse;
import com.gdn.x.productcategorybase.dto.response.DimensionResponse;
import com.gdn.x.productcategorybase.service.DimensionMappingService;
import com.gdn.x.productcategorybase.service.DimensionMappingWrapperService;
import com.gdn.x.productcategorybase.service.DimensionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = DimensionApiPath.BASE_PATH)
@Tag(name = "DimensionController", description = "Dimension apis")
public class DimensionController {

  @Autowired
  private DimensionService dimensionService;

  @Autowired
  private DimensionMappingService dimensionMappingService;

  @Autowired
  private DimensionMappingWrapperService dimensionMappingWrapperService;

  @GetMapping(value = DimensionApiPath.DETAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "fetch dimension details by dimension code", description = "fetch dimension details by dimension code")
  
  public GdnRestSingleResponse<DimensionResponse> fetchDimensionDetails(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @PathVariable("dimensionCode") String dimensionCode) {
    return new GdnRestSingleResponse<>(
        dimensionService.fetchDimensionDetails(storeId, dimensionCode), requestId);
  }

  @PostMapping(value = DimensionApiPath.SAVE, produces = MediaType.APPLICATION_JSON_VALUE,
               consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse createDimension(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody DimensionRequest request) {
    dimensionService.save(storeId, request);
    return new GdnBaseRestResponse(true);
  }

  @PostMapping(value = DimensionApiPath.FILTER, produces = MediaType.APPLICATION_JSON_VALUE,
               consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<DimensionResponse> fetchDimensionListing(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size,
      @RequestBody DimensionFilterRequest request) {
    Pageable pageable = PageRequest.of(page, size);
    Page<DimensionResponse> dimensionResponsePage =
        dimensionService.filter(storeId, request, pageable);
    return new GdnRestListResponse<>(null, null, true, dimensionResponsePage.getContent(),
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            dimensionResponsePage.getTotalElements()), requestId);
  }

  @GetMapping(value = DimensionApiPath.DIMENSION_MAPPING_LIST, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get dimension mapping to attribute by attribute code", description = "get"
      + " dimension mapping by store id and attribute code and pageable")
  public GdnRestListResponse<DimensionMappingResponse> getAttributeByAttributeCode(
      @PathVariable("attributeCode") String attributeCode, @RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "50") Integer size) {
    Pageable pageable = PageRequest.of(page, size);
      log.info("Getting Dimension mapping based on attribute code : {}", attributeCode);
    Page<DimensionMappingResponse> dimensionMappingResponsePage =
        dimensionMappingService.fetchDimensionMapping(attributeCode, storeId, pageable);
    return new GdnRestListResponse<>(null, null, true, dimensionMappingResponsePage.getContent(),
        new PageMetaData(size, page, dimensionMappingResponsePage.getTotalElements()), requestId);
  }

  @PostMapping(value = DimensionApiPath.EDIT, produces = MediaType.APPLICATION_JSON_VALUE,
               consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "api to edit dimension for size chart", description = "edit dimension for "
      + "size chart")
  public GdnBaseRestResponse editDimension(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody DimensionRequest request) {
    log.info("Edit dimension with dimension code:{}, with request:{}", request.getDimensionCode(),
        request);
    dimensionService.edit(storeId, request);
    return new GdnBaseRestResponse(true);
  }

  @GetMapping(value = DimensionApiPath.FIND_BY_NAME, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "api to fetch dimension based on dimension name", description = "fetch "
      + "dimension based on dimension name")
  public GdnRestSingleResponse<DimensionResponse> findByName(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String dimensionName) {
    return new GdnRestSingleResponse<>(dimensionService.findByName(storeId, dimensionName),
        requestId);
  }

  @PostMapping(value = DimensionApiPath.UPDATE_DIMENSION_MAPPING, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "api to update dimension mapping for a size attribute", description = "api to update dimension mapping for a size attribute")
  public GdnBaseRestResponse updateDimensionMapping(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username,
    @PathVariable("attributeCode") String attributeCode,
    @RequestBody DimensionMappingUpdateRequest request) {
    dimensionMappingWrapperService.updateDimensionMapping(storeId, attributeCode, request);
    return new GdnBaseRestResponse(true);
  }
}
