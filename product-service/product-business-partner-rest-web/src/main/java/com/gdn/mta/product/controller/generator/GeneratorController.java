package com.gdn.mta.product.controller.generator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.service.generator.GeneratorService;
import com.gdn.mta.product.web.model.GeneratorControllerErrorMessage;
import com.gdn.mta.product.web.model.GeneratorControllerPath;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping(value = GeneratorControllerPath.BASE_PATH)
@Tag(name = "Generator", description = "Generator Service API")
public class GeneratorController {

  @Autowired
  private GeneratorService generatorService;

  @RequestMapping(value = GeneratorControllerPath.GENERATE_SHIPPING_WEIGHT, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "generate shipping weight", description = "generate shipping weight")
  @ResponseBody
  public GdnRestSingleResponse<GenerateShippingWeightResponse> generateShippingWeight(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody GenerateShippingWeightRequest request) throws Exception {
    GdnPreconditions.checkArgument(request.getLength() != null,
        GeneratorControllerErrorMessage.LENGTH_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(request.getWidth() != null, GeneratorControllerErrorMessage.WIDTH_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(request.getHeight() != null,
        GeneratorControllerErrorMessage.HEIGHT_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(request.getWeight() != null,
        GeneratorControllerErrorMessage.WEIGHT_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getCategoryCode()),
        GeneratorControllerErrorMessage.CATEGORY_CODE_MUST_NOT_BE_BLANK);
    Double shippingWeight =
        this.generatorService.generateShippingWeight(request.getLength(), request.getWidth(), request.getHeight(),
            request.getWeight(), request.getCategoryCode());
    GenerateShippingWeightResponse generateShippingWeightResponse =
        this.generateGenerateShippingWeightResponse(shippingWeight);
    return new GdnRestSingleResponse<GenerateShippingWeightResponse>(generateShippingWeightResponse, requestId);
  }

  private GenerateShippingWeightResponse generateGenerateShippingWeightResponse(Double shippingWeight) throws Exception {
    GenerateShippingWeightResponse generateShippingWeightResponse = new GenerateShippingWeightResponse();
    generateShippingWeightResponse.setShippingWeight(shippingWeight);
    return generateShippingWeightResponse;
  }

}
