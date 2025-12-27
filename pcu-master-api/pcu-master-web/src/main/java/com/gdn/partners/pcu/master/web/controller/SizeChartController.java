package com.gdn.partners.pcu.master.web.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.client.model.BooleanResponse;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterResponse;
import com.gdn.partners.pcu.master.client.model.SizeChartRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartResponse;
import com.gdn.partners.pcu.master.model.SizeChartApiPath;
import com.gdn.partners.pcu.master.service.SizeChartService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;


@Slf4j
@Tag(name = "SizeChart API")
@RestController
@RequestMapping(value = SizeChartApiPath.BASE_PATH)
public class SizeChartController {

  @Autowired
  private SizeChartService sizeChartService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "To create size chart")
  @PostMapping(value = SizeChartApiPath.UPSERT, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse upsertSizeChart(@RequestBody SizeChartRequest sizeChartRequest) {
    return sizeChartService.upsertSizeChart(clientParameterHelper.getStoreId(), sizeChartRequest);
  }

  @Operation(summary = "Fetching Size Chart")
  @GetMapping(value = SizeChartApiPath.FETCH_SIZE_CHART, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<SizeChartResponse> fetchSizeChartDetails(
    @RequestParam(required = false) boolean preview,
    @PathVariable("sizeChartCode") String sizeChartCode) {
    return new GdnRestSingleResponse<>(sizeChartService.fetchSizeChart(sizeChartCode, preview),
      clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Deleting Size Chart")
  @DeleteMapping(value = SizeChartApiPath.DELETE, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse deleteSizeChart(
      @PathVariable("sizeChartCode") String sizeChartCode) {
    sizeChartService.deleteSizeChart(sizeChartCode);
    return new GdnBaseRestResponse(true);
  }

  @Operation(summary = "Fetch size chart based on filter")
  @PostMapping(value = SizeChartApiPath.FILTER, produces = MediaType.APPLICATION_JSON_VALUE,
               consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<SizeChartFilterResponse> fetchSizeChartListByFilter(
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "20") int size,
      @RequestBody SizeChartFilterRequest sizeChartRequest) {
    GdnRestListResponse<SizeChartFilterResponse> response =
        sizeChartService.filter(page, size, sizeChartRequest);
    response.setRequestId(clientParameterHelper.getRequestId());
    return response;
  }

  @Operation(summary = "Validate duplicate size-chart")
  @GetMapping(value = SizeChartApiPath.VALIDATE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse validateSizeChartName(@RequestParam String sizeChartName,
      @RequestParam String businessPartnerCode) {
    GdnRestSingleResponse<SizeChartResponse> response =
        sizeChartService.validate(sizeChartName, businessPartnerCode);
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(),
        Objects.isNull(response.getValue()), this.clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Check if category is valid for size chart")
  @GetMapping(value = SizeChartApiPath.VALID_CATEGORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<BooleanResponse> validCategory(@RequestParam String categoryCode,
      @RequestParam String sizeChartCode) {
    BooleanResponse booleanResponse = sizeChartService.validateCategory(categoryCode, sizeChartCode);
    return new SingleBaseResponse<>(null, null, true, this.clientParameterHelper.getRequestId(),
        booleanResponse);
  }
}
