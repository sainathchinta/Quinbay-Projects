package com.gdn.partners.pcu.external.web.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.GeneratorApiPath;
import com.gdn.partners.pcu.external.service.GeneratorService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name ="Generator API")
@RestController
@RequestMapping(value = GeneratorApiPath.BASE_PATH)
public class GeneratorController {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private GeneratorService generatorService;

  @Operation(summary ="Generate Product Code")
  @GetMapping(value = GeneratorApiPath.GENERATE_PRODUCT_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public SingleBaseResponse<String> generateProductCode() {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Api to generate product code");
    String response = generatorService.generateProductCode();
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @Operation(summary ="Generate BarCode")
  @GetMapping(value = GeneratorApiPath.GENERATE_BAR_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public SingleBaseResponse<String> generateBarCode() {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Api to generate barCode");
    String response = generatorService.generateBarCode();
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @Operation(summary ="Generate shipping weight")
  @GetMapping(value = GeneratorApiPath.GENERATE_SHIPPING_WEIGHT, produces = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public SingleBaseResponse<Double> generateShippingWeight(@PathVariable String categoryCode,
      @RequestParam double length, @RequestParam double width, @RequestParam double height,
      @RequestParam double weight) {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Api to generate shipping weight for categoryCode {}", categoryCode);
    Double response = generatorService.generateShippingWeight(categoryCode, length, width, height, weight);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }
}
