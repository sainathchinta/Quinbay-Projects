package com.gdn.x.productcategorybase.controller;


import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.productcategorybase.SizeChartApiPath;
import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.request.SizeChartFilterRequest;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailMapResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.dto.response.SizeChartDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SizeChartResponse;
import com.gdn.x.productcategorybase.dto.request.SizeChartRequest;
import com.gdn.x.productcategorybase.entity.SizeChart;
import com.gdn.x.productcategorybase.exception.ValidationException;
import com.gdn.x.productcategorybase.service.SizeChartService;
import io.swagger.v3.oas.annotations.Operation;
import com.gdn.x.productcategorybase.service.SizeChartServiceWrapper;
import com.gdn.x.productcategorybase.util.CommonUtil;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = SizeChartApiPath.BASE_PATH)
@Tag(name = "SizeChartController", description = "Size chart apis")
public class SizeChartController {

  @Autowired
  private SizeChartService sizeChartService;

  @Autowired
  private SizeChartServiceWrapper sizeChartServiceWrapper;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @PostMapping(value = SizeChartApiPath.UPSERT, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse upsertSizeChart(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @RequestBody SizeChartRequest request) throws Exception {
    sizeChartServiceWrapper.upsertSizeChart(storeId, request);
    return new GdnBaseRestResponse(true);
  }


  @PostMapping(value = SizeChartApiPath.FILTER, consumes = MediaType.APPLICATION_JSON_VALUE,
               produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "fetch size-charts based on filter applied", description = "fetch "
      + "size-charts based on filter applied")
  public GdnRestListResponse<SizeChartResponse> filter(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size, @RequestBody SizeChartFilterRequest request)
      throws Exception {
    log.info("Fetching size chart list with request:{}", request);
    Pageable pageable = PageRequest.of(page, size);
    Page<SizeChartResponse> sizeCharts = sizeChartService.filter(storeId, request, pageable);
    return new GdnRestListResponse<>(null, null, true, sizeCharts.getContent(),
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            sizeCharts.getTotalElements()), requestId);
  }

  @GetMapping(value = SizeChartApiPath.DETAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch size chart details by size chart code", description = "Fetch size chart details by size chart code")
  public GdnRestSingleResponse<SizeChartDetailResponse> fetchSizeChartDetails(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestParam(required = false) boolean preview,
    @PathVariable("sizeChartCode") String sizeChartCode) throws Exception {
    return new GdnRestSingleResponse<>(
      sizeChartServiceWrapper.fetchSizeChartDetails(storeId, preview, sizeChartCode), requestId);
  }

  @PostMapping(value = SizeChartApiPath.UPDATE_SIZE_CHART_STATUS, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update size chart status by size chart code", description =
      "Update size chart's mfd and waiting deletion flags" + "by size chart code")
  public GdnBaseRestResponse updateSizeChartStatus(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("sizeChartCode") String sizeChartCode,
      @RequestParam String businessPartnerCode,
      @RequestParam(required = false) Boolean waitingDeletion,
      @RequestParam(required = false) Boolean markForDelete) {
    log.info("Update size chart status request, sizeChartCode:{}, businessPartnerCode:{}, "
            + "waitingDeletion:{}, markForDelete:{}", sizeChartCode, businessPartnerCode,
        waitingDeletion, markForDelete);
    sizeChartService.updateSizeChartStatusBySizeChartCode(storeId, sizeChartCode,
        businessPartnerCode, waitingDeletion, markForDelete);
    return new GdnBaseRestResponse(true);
  }

  @GetMapping(value = SizeChartApiPath.FIND_BY_NAME_AND_BUSINESS_PARTNER_CODE, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Find size chart by size-chart name and business partner code",
             description = "Find size chart by size-chart name and business partner code")
  public GdnRestSingleResponse<SizeChartResponse> findByNameAndBPCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String sizeChartName,
      @RequestParam String businessPartnerCode) {
    return new GdnRestSingleResponse<>(
        sizeChartService.findByNameAndBusinessPartnerCode(storeId, sizeChartName,
            businessPartnerCode), requestId);
  }

  @GetMapping(value = SizeChartApiPath.VALIDATE_SIZE_CHART_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Check whether sizeChartCode is valid or Not", description = "Check whether sizeChartCode is valid or Not")
  public GdnBaseRestResponse validateSizeChartCode(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("sizeChartCode") String sizeChartCode) {
    try {
      SizeChart sizeChart =
          sizeChartService.findBySizeChartCodeAndMarkForDeleteFalse(mandatoryParameterHelper.getStoreId(),
              sizeChartCode);
      return new GdnBaseRestResponse(Objects.nonNull(sizeChart));
    } catch (ValidationException exception) {
      return new GdnBaseRestResponse(exception.getErrorMessage(), null, false, mandatoryParameterHelper.getRequestId());
    }
  }

  @GetMapping(value = SizeChartApiPath.VALIDATE_CATEGORY_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Check whether category is Cn", description = "Check whether category is Cn")
  public GdnRestSingleResponse<SimpleBooleanResponse> validateCategoryCode(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String categoryCode, @RequestParam String sizeChartCode) {
    try {
      boolean isCategoryValid =
          sizeChartService.validateCategoryAttributesForSizeChart(mandatoryParameterHelper.getStoreId(), sizeChartCode,
              categoryCode);
      return new GdnRestSingleResponse<>(null, null, true, new SimpleBooleanResponse(isCategoryValid), requestId);
    } catch (Exception e) {
      log.error("Exception caught while validating category for sizeChartCode : {} and categoryCode : {} ",
          sizeChartCode, categoryCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), e.getMessage(), false, null, requestId);
    }
  }

  @PostMapping(value = SizeChartApiPath.GET_SIZE_CHART_CODE, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get  sizeChartName with sizeChartCode", description = "Get  sizeChartName"
      + " with sizeChartCode")
  public GdnRestSingleResponse<BasicSizeChartDetailMapResponse> getSizeChartNameBySizeChartCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody List<String> sizeChartCodes) {
    List<SizeChart> sizeCharts =
        sizeChartService.findSizeChartsBySizeChartCode(mandatoryParameterHelper.getStoreId(),
            sizeChartCodes.stream().distinct().collect(Collectors.toList()));
    return new GdnRestSingleResponse<>(null, null, true,
        CommonUtil.generateBasicSizeChartDetailResponse(sizeCharts), requestId);
  }
}
