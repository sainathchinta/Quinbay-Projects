package com.gdn.x.productcategorybase.controller;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.DistributionInfoApiPath;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.service.DistributionInfoWrapperService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collections;

@Slf4j
@RestController
@RequestMapping(value = DistributionInfoApiPath.BASE_PATH)
@RequiredArgsConstructor
@Tag(name = "DistributionInfoController", description = "Distribution Info API")
public class DistributionInfoController {

  private static final String SKU_CODE = "skuCode";
  private final DistributionInfoWrapperService distributionInfoWrapperService;

  @PostMapping(value = DistributionInfoApiPath.DISTRIBUTION_INFO, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update distribution info for product items", description = "Update "
      + "distribution info for product items based on the provided request")
  public GdnBaseRestResponse updateDistributionInfo(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam(required = false) String username, @PathVariable String productCode,
      @RequestBody DistributionInfoUpdateRequest distributionInfoUpdateRequest) {
    try {
      log.info("Updating distribution info for product: {} with request: {}", productCode,
          distributionInfoUpdateRequest);
      distributionInfoWrapperService.updateDistributionInfoAndPublishProduct(storeId, productCode,
          distributionInfoUpdateRequest);
      return new GdnBaseRestResponse(true);
    } catch (Exception e) {
      log.error("Error updating distribution info for product: {} ", productCode, e);
      return new GdnBaseRestResponse(false);
    }
  }

  @GetMapping(value = DistributionInfoApiPath.DISTRIBUTION_INFO, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<DistributionInfoPerSkuResponse> getDistributionInfo(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(required = false) String username,
      @PathVariable String productCode,
      @RequestParam(defaultValue = "false") boolean needDistributionInfoResponse,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size) {
    try {
      Pageable pageable = PageRequest.of(page, size, Sort.by(SKU_CODE).ascending());
      Page<DistributionInfoPerSkuResponse> pageData =
          distributionInfoWrapperService.getDistributionInfo(storeId, productCode, pageable);
      return new GdnRestListResponse<>(null, null, true, pageData.getContent(),
          new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
              pageData.getTotalElements()), requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while getting getting distribution info. storeId : {}, requestId : {}, "
          + "productCode : {} ", storeId, requestId, productCode, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getMessage(), false,
          null, null, requestId);
    } catch (Exception e) {
      log.error("Error while getting getting distribution info. storeId : {}, requestId : {}, "
          + "productCode : {} ", storeId, requestId, productCode, e);
      return new GdnRestListResponse<>(ErrorMessage.SYSTEM_ERROR.getMessage(),
          ErrorMessage.SYSTEM_ERROR.getMessage(), false, Collections.emptyList(),
          new PageMetaData(size, page, 0), requestId);
    }
  }
}