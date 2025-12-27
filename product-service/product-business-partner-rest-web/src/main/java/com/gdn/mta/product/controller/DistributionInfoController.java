package com.gdn.mta.product.controller;

import java.util.List;

import com.gda.mta.product.dto.DistributionInfoRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.service.DistributionInfoService;
import com.gdn.mta.product.service.exception.ApiDataNotFoundException;
import com.gdn.mta.product.web.model.DistributionInfoApiPath;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = DistributionInfoApiPath.BASE_PATH)
@Tag(name = "DistributionInfoController", description = "Distribution Info Service API")
@RequiredArgsConstructor
@Slf4j
public class DistributionInfoController {

  private final DistributionInfoService distributionInfoService;

  @PostMapping(value = DistributionInfoApiPath.DISTRIBUTION_INFO
      , produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update distribution info for product items", description = "Update "
      + "distribution info for product items based on the provided request")
  public GdnBaseRestResponse updateDistributionInfo(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam(required = false) String username, @PathVariable String productCode,
      @RequestBody DistributionInfoRequest distributionInfoRequest) {
    try {
      distributionInfoService.validateAndUpdateDistributionInfo(productCode,
          distributionInfoRequest, username);
      return new GdnBaseRestResponse(true);
    } catch (ApiDataNotFoundException e) {
      log.error("Failed to update Distribution Info , errorCode : {}, errorMessage : {}, requestId : "
              + "{},distributionInfoRequest : {}, e-", e.getErrorCode().getCode(), e.getErrorMsg(), requestId,
          distributionInfoRequest, e);
      return new GdnBaseRestResponse(e.getErrorMsg(), e.getErrorCode().getCode(), Boolean.FALSE, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error(
          "Failed to update Distribution Info , errorCode : {}, errorMessage : {}, requestId : "
              + "{},distributionInfoRequest : {}, e-", e.getErrorCodes().toString(),
          e.getErrorMessage(), requestId, distributionInfoRequest, e);
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().toString(),
          Boolean.FALSE, requestId);
    } catch (Exception e) {
      log.error("Failed to update Distribution Info with requestId:{}, request:{}, error - ",
          requestId, distributionInfoRequest, e);
      return new GdnBaseRestResponse(e.getMessage(), e.getMessage(), Boolean.FALSE, requestId);
    }
  }

  @GetMapping(value = DistributionInfoApiPath.DISTRIBUTION_INFO, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get distribution info for product code", description = "Get distribution info for product code")
  public GdnRestListResponse<DistributionInfoPerSkuResponse> fetchDistributionInfo(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam(required = false) String username, @PathVariable String productCode,
      @RequestParam boolean needDistributionInfoResponse, @RequestParam int page,
      @RequestParam(defaultValue = "20") int size) {
    try {
      GdnRestListResponse<DistributionInfoPerSkuResponse> distributionInfoByProductCode =
          distributionInfoService.fetchDistributionInfoByProductCode(storeId, productCode, page, size);
      return new GdnRestListResponse<>(null, null, true, distributionInfoByProductCode.getContent(),
          new PageMetaData(size, page, distributionInfoByProductCode.getPageMetaData().getTotalRecords()), requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Failed to update Distribution Info , errorCode : {}, errorMessage : {}, requestId : "
          + "{}, productCode : {}, e- ", e.getErrorCodes().toString(), e.getErrorMessage(), requestId, productCode, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().toString(), Boolean.FALSE, requestId);
    } catch (Exception e) {
      log.error("Failed to update Distribution Info with requestId:{}, request:{}, error - ", requestId, productCode,
          e);
      return new GdnRestListResponse<>(e.getMessage(), e.getMessage(), Boolean.FALSE, requestId);
    }
  }
}