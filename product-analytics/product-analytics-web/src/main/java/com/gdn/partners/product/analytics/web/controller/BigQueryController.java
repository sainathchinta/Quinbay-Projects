package com.gdn.partners.product.analytics.web.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.product.analytics.model.BigQueryApiPath;
import com.gdn.partners.product.analytics.service.bigQuery.DownloadSellerInfoBigQueryService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "Big Query Controller", description = "Big Query Controller")
@RestController
@RequestMapping(value = BigQueryApiPath.BASE_PATH)
@Validated
public class BigQueryController {

  @Autowired
  private DownloadSellerInfoBigQueryService downloadSellerInfoBigQueryService;

  @Operation(summary = "update seller QC info")
  @PutMapping(value = BigQueryApiPath.BIG_QUERY_SELLER_INFO_SUBMIT_API, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public BaseResponse updateSellerQCData(@RequestParam String storeId,
      @RequestParam(required = false, defaultValue = "SELLER_INFO_BQ_JOB") String jobProcessType,
      @RequestParam(required = false, defaultValue = "0") int fetchHourThreshold,
      @RequestParam(required = false, defaultValue = "0") int suggestedDateFetch,
      @RequestParam(required = false, defaultValue = "") String attributeName) {
    downloadSellerInfoBigQueryService.execute(storeId, jobProcessType, fetchHourThreshold,
        suggestedDateFetch, attributeName);
    return new BaseResponse(null, null, true, StringUtils.EMPTY);
  }
}
