package com.gdn.mta.product.controller;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.ProductSystemParameterRequest;
import com.gda.mta.product.dto.ProductSystemParameterResponse;
import com.gda.mta.product.dto.response.ProductSystemParameterSwitchResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.web.model.ProductSystemParameterControllerPath;

import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

@RestController
@RequestMapping(value = ProductSystemParameterControllerPath.BASE_PATH)
@Tag(name = "ProductSystemParameterController", description = "Controller for system parameter")
@Slf4j
public class ProductSystemParameterController {

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @RequestMapping(value = ProductSystemParameterControllerPath.SYSTEM_PARAMETER_INSERT, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnBaseRestResponse insertSystemParameter(@RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestBody ProductSystemParameterRequest productSystemParameterRequest) throws Exception {
    ProductSystemParameter productSystemParameter = new ProductSystemParameter();
    BeanUtils.copyProperties(productSystemParameterRequest, productSystemParameter);
    productSystemParameter.setStoreId(storeId);
    this.productSystemParameterService.insert(productSystemParameter);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductSystemParameterControllerPath.SYSTEM_PARAMETER_UPDATE, method = RequestMethod.PUT,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnBaseRestResponse updateSystemParameter(@RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestBody ProductSystemParameterRequest productSystemParameterRequest) throws Exception {
    ProductSystemParameter productSystemParameter = new ProductSystemParameter();
    BeanUtils.copyProperties(productSystemParameterRequest, productSystemParameter);
    productSystemParameter.setStoreId(storeId);
    this.productSystemParameterService.update(productSystemParameter);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductSystemParameterControllerPath.SYSTEM_PARAMETER_DELETE, method = RequestMethod.DELETE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnBaseRestResponse deleteSystemParameter(@RequestParam String storeId, @RequestParam String requestId,
      @RequestParam String channelId, @RequestParam String clientId, @PathVariable String variable) throws Exception {
    this.productSystemParameterService.delete(storeId, variable);
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductSystemParameterControllerPath.SYSTEM_PARAMETER_FIND, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestSingleResponse<ProductSystemParameterResponse> findSystemParameter(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String variable) {
    ProductSystemParameterResponse productSystemParameterResponse = new ProductSystemParameterResponse();
    ProductSystemParameter response =
        this.productSystemParameterService.findByStoreIdAndVariable(storeId, variable);
    BeanUtils.copyProperties(response, productSystemParameterResponse);
    return new GdnRestSingleResponse<>(productSystemParameterResponse, requestId);
  }

  @RequestMapping(value = ProductSystemParameterControllerPath.SYSTEM_PARAMETER_SWITCH_FETCH, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestSingleResponse<ProductSystemParameterSwitchResponse> findSystemParameterSwitchValues(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId) {
    log.info("Fetching flag values for various switches from system parameter database");
    return new GdnRestSingleResponse<>(
        new ProductSystemParameterSwitchResponse(this.productSystemParameterService.findSwitchValuesWithCanaryAndNonCanary(storeId)),
            requestId);
  }

  @RequestMapping(value =
                      ProductSystemParameterControllerPath.SYSTEM_PARAMETER_FETCH_WITH_SHOW_ON_UI
      , method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestListResponse<ProductSystemParameterResponse> findSystemParameterSwitchValuesWithShowOnUI(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
      @RequestParam String clientId) {
    log.info("Fetching system parameter values with Show On UI true");
    List<ProductSystemParameter> productSystemParameter =
        this.productSystemParameterService.findByStoreIdAndShowOnUITrue(storeId);
    List<ProductSystemParameterResponse> productSystemParameterResponses = new ArrayList<>();
    for (ProductSystemParameter parameter : productSystemParameter) {
      productSystemParameterResponses.add(
          new ProductSystemParameterResponse(parameter.getVariable(), parameter.getValue(),
              parameter.getDescription(), parameter.isShowOnUI()));
    }
    return new GdnRestListResponse<>(productSystemParameterResponses,
        new PageMetaData(productSystemParameterResponses.size(), 0,
            productSystemParameterResponses.size()), requestId);
  }

}
