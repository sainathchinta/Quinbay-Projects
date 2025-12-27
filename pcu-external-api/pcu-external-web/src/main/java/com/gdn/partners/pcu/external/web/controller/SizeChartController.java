package com.gdn.partners.pcu.external.web.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.SizeChartApiPath;
import com.gdn.partners.pcu.external.service.SizeChartService;
import com.gdn.partners.pcu.external.web.model.request.ProductSizeChartUpdateWebRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(value = SizeChartApiPath.BASE_PATH)
@Tag(name ="Size Chart API")
public class SizeChartController {

  @Autowired
  private SizeChartService sizeChartService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Operation(summary = "API to size chart code in product")
  @PostMapping(value = SizeChartApiPath.UPDATE_PRODUCT_SIZE_CHART, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateProductSizeChartCode(@PathVariable("sizeChartCode") String sizeChartCode,
      @RequestBody ProductSizeChartUpdateWebRequest productSizeChartUpdateWebRequest) {
    log.info("Update product sizeChartCode : {} , productSizeChartUpdateWebRequest : {} , requestId : {}  ",
        sizeChartCode, productSizeChartUpdateWebRequest, mandatoryParameterHelper.getRequestId());
    sizeChartService.updateProductSizeChart(sizeChartCode, productSizeChartUpdateWebRequest);
    return new BaseResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

}
