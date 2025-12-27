package com.gdn.x.product.rest.web.controller.api;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.exception.ApiErrorCodes;
import com.gdn.x.product.rest.web.model.SizeChartApiPath;
import com.gdn.x.product.rest.web.model.request.ProductSizeChartUpdateRequest;
import com.gdn.x.product.rest.web.model.response.SizeChartResponse;
import com.gdn.x.product.service.api.SizeChartService;
import com.gdn.x.product.service.util.ModelConverter;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(value = SizeChartApiPath.BASE_PATH)
@Tag(name = "SizeChartController", description = "Size chart Service API")
public class SizeChartController {

  @Autowired
  private SizeChartService sizeChartService;

  @Autowired
  private ModelConverter modelConverter;

  @GetMapping(value = SizeChartApiPath.GET_SIZE_CHART_DETAILS, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get size chart detail", description = "get size chart detail from pcb")
  public GdnRestSingleResponse<SizeChartResponse> fetchSizeChart(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("sizeChartCode") String sizeChartCode) {
    try {
      log.info("Getting size chart details for sizeChartCode : {} ", sizeChartCode);
      SizeChartResponse sizeChartResponse = sizeChartService.fetchSizeChartDetails(storeId, sizeChartCode);
      return new GdnRestSingleResponse<>(null, null, true, modelConverter.updateSizeChartResponse(sizeChartResponse),
          requestId);
    } catch (ApplicationRuntimeException exception) {
      log.error("Error while getting size chart details for sizeChartCode : {} ", sizeChartCode, exception);
      return new GdnRestSingleResponse<>(exception.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null,
          requestId);
    } catch (Exception e) {
      log.error("Error while getting size chart details for sizeChartCode : {} ", sizeChartCode, e);
      return new GdnRestSingleResponse<>(ApiErrorCodes.SYSTEM_ERROR.getErrorMessage(),
          ApiErrorCodes.SYSTEM_ERROR.getErrorCode(), false, null, requestId);
    }
  }

  @PostMapping(value = SizeChartApiPath.UPDATE_PRODUCT_SIZE_CHART, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product size chart", description = "update product size chart")
  public GdnBaseRestResponse updateProductSizeChart(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("sizeChartCode") String sizeChartCode,
      @RequestBody ProductSizeChartUpdateRequest productSizeChartUpdateRequest) {
    log.info("Update products for size chart sizeChartCode : {} , productSizeChartUpdateRequest: {} ", sizeChartCode,
        productSizeChartUpdateRequest);
    try {
      sizeChartService.updateProductSizeChartCode(storeId, sizeChartCode, productSizeChartUpdateRequest);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      log.info("Error while products for size chart sizeChartCode : {} , productSizeChartUpdateRequest: {} ",
          sizeChartCode, productSizeChartUpdateRequest);
      return new GdnBaseRestResponse(e.getMessage(), e.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      log.info("Error while products for size chart sizeChartCode : {} , productSizeChartUpdateRequest: {} ",
          sizeChartCode, productSizeChartUpdateRequest);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }
}
