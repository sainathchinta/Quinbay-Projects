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
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.CategoryShippingApiPath;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.controller.util.MapperUtil;
import com.gdn.x.productcategorybase.dto.request.CategoryShippingRequest;
import com.gdn.x.productcategorybase.dto.request.ShippingRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingWeightResponse;
import com.gdn.x.productcategorybase.entity.CategoryShipping;
import com.gdn.x.productcategorybase.service.CategoryShippingService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = CategoryShippingApiPath.BASE_PATH)
@Tag(name = "CategoryShippingCodeController", description = "Master Category - Shipping Code Service API")
public class CategoryShippingController {

  private static final Logger LOG = LoggerFactory.getLogger(CategoryShippingController.class);

  @Autowired
  private CategoryShippingService categoryShippingService;

  @Autowired
  private MapperUtil mapperUtil;

  @RequestMapping(value = CategoryShippingApiPath.DELETE_VALUE, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "delete category - shipping entry", description = "delete category - shipping entry")
  
  public GdnBaseRestResponse deleteCategoryShipping(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleRequestHolder request) throws Exception {
    this.categoryShippingService.markForDeleteCategoryShipping(storeId, request.getId());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = CategoryShippingApiPath.FILTER_CATEGORY_CODE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get category - shipping by category name",
      description = "get entries by store id and category code and pageable")
  
  public GdnRestListResponse<CategoryShippingResponse> getCategoryShippingByCategoryCodeAndPageable(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @RequestParam String categoryName) throws Exception {
    CategoryShippingController.LOG.debug(categoryName.toString());
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, page, size, pageable,
        this.categoryShippingService.findByCategoryCode(storeId, categoryName, pageable));
  }

  @RequestMapping(value = CategoryShippingApiPath.FILTER_SHIPPING_CODE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get category - shipping by shipping code",
      description = "get entries by store id and shipping code and pageable")
  
  public GdnRestListResponse<CategoryShippingResponse> getCategoryShippingByShippingCodeAndPageable(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @RequestParam boolean deliveredByMerchant, @RequestParam boolean specialHandling,
      @RequestParam boolean directFlight) throws Exception {
    ShippingRequest shippingCodeReq = new ShippingRequest(deliveredByMerchant, specialHandling, directFlight);
    String shippingCode = this.mapperUtil.mapRequestToString(shippingCodeReq);
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, page, size, pageable,
        this.categoryShippingService.findByShippingCode(storeId, shippingCode, pageable));
  }

  @RequestMapping(method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get summary of category - shipping code",
      description = "get list of category - shipping code entries by store id and pageable")
  
  public GdnRestListResponse<CategoryShippingResponse> getCategoryShippingSummary(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, page, size, pageable,
        this.categoryShippingService.findByStoreId(storeId, pageable));
  }

  private GdnRestListResponse<CategoryShippingResponse> populateListResponse(String storeId, String channelId,
      String clientId, String requestId, Integer page, Integer size, Pageable pageable,
      Page<CategoryShipping> catShipCodePage) throws Exception {
    GdnRestListResponse<CategoryShippingResponse> wrapper = null;
    List<CategoryShippingResponse> catShipCodeResponses = new ArrayList<CategoryShippingResponse>();
    for (CategoryShipping catShipCode : catShipCodePage.getContent()) {
      CategoryShippingResponse response = new CategoryShippingResponse();
      BeanUtils.copyProperties(catShipCode, response);
      response.setShippingCode(this.mapperUtil.mapStringToShippingCodeResponse(catShipCode.getShippingCode()));
      catShipCodeResponses.add(response);
    }
    wrapper = new GdnRestListResponse<CategoryShippingResponse>(null, null, true, catShipCodeResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), catShipCodePage.getTotalElements()),
        requestId);
    return wrapper;
  }

  @RequestMapping(value = CategoryShippingApiPath.SAVE, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "save category - shipping", description = "save category - shipping")
  
  public GdnBaseRestResponse saveCategoryShipping(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CategoryShippingRequest request) throws Exception {
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getCreatedBy()) || StringUtils.isEmpty(request.getCreatedDate())),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage());

    CategoryShipping catShipCode = new CategoryShipping();
    BeanUtils.copyProperties(request, catShipCode, Constants.ID);
    catShipCode.setShippingCode(this.mapperUtil.mapRequestToString(request.getShippingCode()));

    if (StringUtils.isEmpty(catShipCode.getStoreId())) {
      catShipCode.setStoreId(storeId);
    }
    this.categoryShippingService.save(catShipCode);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = CategoryShippingApiPath.UPDATE, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update category - shipping", description = "update category - shipping")
  
  public GdnBaseRestResponse updateCategoryShipping(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CategoryShippingRequest request) throws Exception {
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy()) || StringUtils.isEmpty(request.getUpdatedDate())),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
    CategoryShipping catShipCode = this.categoryShippingService.findByStoreIdAndId(storeId, request.getId());
    GdnPreconditions.checkArgument(catShipCode != null,
        ErrorMessage.ENTITY_NOT_FOUND_FOR_UPDATE_MESSAGE.getMessage() + request);
    BeanUtils.copyProperties(request, catShipCode, "version", "createdBy", "createdDate",
        Constants.ID);
    catShipCode.setShippingCode(this.mapperUtil.mapRequestToString(request.getShippingCode()));
    if (StringUtils.isEmpty(catShipCode.getStoreId())) {
      catShipCode.setStoreId(storeId);
    }
    this.categoryShippingService.update(catShipCode);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = CategoryShippingApiPath.GENERATE_SHIPPING_WEIGHT, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "generate shipping weight", description = "generate shipping weight")
  
  public GdnRestSingleResponse<CategoryShippingWeightResponse> generateShippingWeight(@PathVariable String categoryCode,
      @RequestParam String storeId, @RequestParam double length, @RequestParam double width,
      @RequestParam double height, @RequestParam double weight) {
    LOG.info("Api to generate shipping weight for category code {}", categoryCode);
    double response = 0;
    try {
      response =
          this.categoryShippingService.generateShippingWeight(storeId, categoryCode, length, height, weight, width);
    } catch (ApplicationException e) {
      LOG.error("Error in generating shipping weight for category code {}", categoryCode);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, new CategoryShippingWeightResponse(), null);
    } catch (Exception e) {
      LOG.error("Error in generating shipping weight for category code {}", categoryCode);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, new CategoryShippingWeightResponse(), null);
    }
    return new GdnRestSingleResponse<>(null, null, true, new CategoryShippingWeightResponse(response), null);
  }

}
