package com.gdn.x.product.rest.web.controller.api;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.rest.web.model.RulesControllerApiPath;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;
import com.gdn.x.product.service.api.CategoryService;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = RulesControllerApiPath.BASE_PATH)
@Tag(name = "Rules Controller", description = "Rule Service Api")
public class RulesController {

  private static final Logger LOGGER = LoggerFactory.getLogger(RulesController.class);

  @Autowired
  private CategoryService categoryService;

  @RequestMapping(value = RulesControllerApiPath.PRODUCT_SCORE_RULE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get product score rules from db", description = "get product score rules from db")
  @ResponseBody
  public GdnRestSingleResponse<ProductScoreRuleResponse> getProductScoreRule(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(required = false) String categoryCode) throws Exception {
    try {
      ProductScoreRuleResponse productScoreRuleResponse = categoryService.getProductScoreRuleForCategory(categoryCode);
      return new GdnRestSingleResponse<>(null, null, true, productScoreRuleResponse, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while score", e);
      return new GdnRestSingleResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

}
