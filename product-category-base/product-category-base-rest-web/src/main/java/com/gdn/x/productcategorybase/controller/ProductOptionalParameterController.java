package com.gdn.x.productcategorybase.controller;

import java.util.ArrayList;
import java.util.List;

import com.gdn.x.productcategorybase.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.ProductOptionalParameterRequest;
import com.gdn.x.productcategorybase.dto.response.ProductOptionalParameterResponse;
import com.gdn.x.productcategorybase.entity.ProductOptionalParameter;
import com.gdn.x.productcategorybase.service.ProductOptionalParameterService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = ProductOptionalParameterController.BASE_PATH)
@Tag(name = "ProductOptionalParameterController", description = "Product Optional Parameter Service API")
public class ProductOptionalParameterController {
  public static final String BASE_PATH = "/api/productOptionalParameter";
  public static final String SAVE = "/save";
  public static final String UPDATE = "/update";
  public static final String DELETE = "/delete";
  public static final String DETAIL = "/{id}";
  public static final String FILTER_NAME = "/filter/name";
  public static final String FILTER_PRODUCT_CODE = "/filter/productCode";
  private static final Logger LOG = LoggerFactory.getLogger(ProductOptionalParameterController.class);

  @Autowired
  private ProductOptionalParameterService productOptionalParameterService;

  @RequestMapping(value = ProductOptionalParameterController.DELETE, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "Mark for delete true product optional parameter by id ", description =
      "Mark for delete true product optional parameter by id ")
  public GdnBaseRestResponse deleteProductOptionalParameter(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody SimpleRequestHolder request)
      throws Exception {
    this.productOptionalParameterService.markForDeleteProductOptionalParameter(request.getId());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductOptionalParameterController.FILTER_NAME, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get list of product optional parameter by name", description = "get list "
      + "of product optional parameter by name")
  public GdnRestListResponse<ProductOptionalParameterResponse> getProductOptionalParameterByName(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String name, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    ProductOptionalParameterController.LOG.debug("get list of product optional parameter by name : {}", name);
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, pageable,
        this.productOptionalParameterService.findByNameLike(storeId, name, pageable));
  }

  @RequestMapping(value = ProductOptionalParameterController.FILTER_PRODUCT_CODE, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get list of product optional parameter by product code", description =
      "get list of product optional parameter by product code")
  public GdnRestListResponse<ProductOptionalParameterResponse> getProductOptionalParameterByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productCode, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    ProductOptionalParameterController.LOG.debug("get list of product optional parameter by productCode : {}",
        productCode);
    Pageable pageable = PageRequest.of(page, size);

    return this.populateListResponse(storeId, channelId, clientId, requestId, pageable,
        this.productOptionalParameterService.findByProductCodeLike(storeId, productCode, pageable));
  }

  @RequestMapping(value = ProductOptionalParameterController.DETAIL, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get detail of product optional parameter", description = "get detail of "
      + "product optional parameter")
  public GdnRestSingleResponse<ProductOptionalParameterResponse> getProductOptionalParameterDetail(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("id") String id) throws Exception {
    ProductOptionalParameterResponse productOptionalParameterResponse = new ProductOptionalParameterResponse();
    ProductOptionalParameter productOptionalParameter = this.productOptionalParameterService.findById(id);
    BeanUtils.copyProperties(productOptionalParameter, productOptionalParameterResponse);
    return new GdnRestSingleResponse<ProductOptionalParameterResponse>("", "", true, productOptionalParameterResponse,
        requestId);
  }


  public ProductOptionalParameterService getProductOptionalParameterService() {
    return this.productOptionalParameterService;
  }

  @RequestMapping(method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get summary of product optional parameter", description = "get list of "
      + "product optional parameter by store id and pageable parameter")
  public GdnRestListResponse<ProductOptionalParameterResponse> getProductOptionalParameterSummary(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    ProductOptionalParameterController.LOG.debug("get summary of product optional parameter");
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, pageable,
        this.productOptionalParameterService.findByStoreId(storeId, pageable));
  }

  private GdnRestListResponse<ProductOptionalParameterResponse> populateListResponse(String storeId, String channelId,
      String clientId, String requestId, Pageable pageable, Page<ProductOptionalParameter> productPageOptionalParameter)
          throws Exception {
    GdnRestListResponse<ProductOptionalParameterResponse> wrapper = null;
    List<ProductOptionalParameterResponse> productOptionalParameterResponses =
        new ArrayList<ProductOptionalParameterResponse>();
    for (ProductOptionalParameter productOptionalParameter : productPageOptionalParameter.getContent()) {
      ProductOptionalParameterResponse response = new ProductOptionalParameterResponse();
      BeanUtils.copyProperties(productOptionalParameter, response);
      productOptionalParameterResponses.add(response);
    }
    wrapper = new GdnRestListResponse<ProductOptionalParameterResponse>(null, null, true,
        productOptionalParameterResponses, new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productPageOptionalParameter.getTotalElements()),
        requestId);
    return wrapper;
  }

  @RequestMapping(value = ProductOptionalParameterController.SAVE, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "Save new product optional parameter", description = "Save new product "
      + "optional parameter")
  public GdnBaseRestResponse saveProductOptionalParameter(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestBody ProductOptionalParameterRequest request) throws Exception {
    this.setStoreIdIfNull(storeId, request);
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getCreatedBy()) || StringUtils.isEmpty(request.getCreatedDate())),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage());
    ProductOptionalParameter productOptionalParameter = new ProductOptionalParameter();
    BeanUtils.copyProperties(request, productOptionalParameter, Constants.ID);
    this.productOptionalParameterService.save(productOptionalParameter);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  public void setProductOptionalParameterService(ProductOptionalParameterService productOptionalParameterService) {
    this.productOptionalParameterService = productOptionalParameterService;
  }

  private void setStoreIdIfNull(String storeId, ProductOptionalParameterRequest request) {
    if (StringUtils.isEmpty(request.getStoreId())) {
      request.setStoreId(storeId);
    }
  }

  @RequestMapping(value = ProductOptionalParameterController.UPDATE, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update existing product optional parameter", description = "update "
      + "existing product optional parameter")
  public GdnBaseRestResponse updateProductOptionalParameter(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestBody ProductOptionalParameterRequest request) throws Exception {
    this.setStoreIdIfNull(storeId, request);
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy()) || StringUtils.isEmpty(request.getUpdatedDate())),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
    ProductOptionalParameter productOptionalParameter = this.productOptionalParameterService.findById(request.getId());
    BeanUtils.copyProperties(request, productOptionalParameter, "version", "createdBy", "createdDate");
    this.productOptionalParameterService.update(productOptionalParameter);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
}
