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
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.PredefinedAllowedAttributeValueApiPath;
import com.gdn.x.productcategorybase.SolrConstants;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = PredefinedAllowedAttributeValueApiPath.BASE_PATH)
@Tag(name = "PredefinedAllowedAttributeValueController",
description = "Master Predefined Allowed Attribute Value Service API")
public class PredefinedAllowedAttributeValueController {

  private static final Logger LOG = LoggerFactory.getLogger(PredefinedAllowedAttributeValueController.class);

  @Autowired
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @RequestMapping(value = PredefinedAllowedAttributeValueApiPath.DEACTIVATED, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "deactivated PredefinedAllowedAttributeValue",
  description = "deactivated PredefinedAllowedAttributeValue")
  
  public GdnBaseRestResponse deactivated(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String id) throws Exception {
    this.predefinedAllowedAttributeValueService.deactivated(id, storeId);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
  
  @RequestMapping(value = PredefinedAllowedAttributeValueApiPath.GET_BY_ATTRIBUTE_CODE_AND_VALUE,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(
      summary = "get PredefinedAllowedAttributeValue by match attributeCode, storeID and value",
      description = "get PredefinedAllowedAttributeValue by match attributeCode, storeID and value")
  
  public GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String attributeCode, @RequestParam String value,
      @RequestParam(required = false, defaultValue = "false") boolean fetchByPredefinedAttributeCode) throws Exception {
    PredefinedAllowedAttributeValueResponse response =
        new PredefinedAllowedAttributeValueResponse();
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
        this.predefinedAllowedAttributeValueService
            .findTopByStoreIdAndAttributeCodeAndValueOrPredefinedAllowedAttributeCode(
                storeId, attributeCode, value, fetchByPredefinedAttributeCode);
    if(predefinedAllowedAttributeValue!=null){
      BeanUtils.copyProperties(predefinedAllowedAttributeValue, response, "attribute");
    }else{
      LOG.error("attribute {} is not found!", value);
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "attribute "+value+" is not found!");
    }
    return new GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse>(response, requestId);
  }

  @RequestMapping(value = PredefinedAllowedAttributeValueApiPath.FILTER_ATTRIBUTE_ID_AND_VALUE,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get PredefinedAllowedAttributeValue by attribute id",
      description = "get PredefinedAllowedAttributeValue by store id and attribute id and pageable")
  
  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @RequestParam String attributeId, @RequestParam(required = false) String value) throws Exception {
    PredefinedAllowedAttributeValueController.LOG.debug(attributeId);
    PageRequest pageRequest = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "sequence"));
    return this.populateListResponse(storeId, channelId, clientId, requestId, page, size, pageRequest,
        this.predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(storeId,
            StringUtils.isEmpty(value) ? "%" : "%" + value + "%", attributeId, pageRequest));
  }

  @RequestMapping(value = PredefinedAllowedAttributeValueApiPath.GET_ATTRIBUTE_BY_ID_AND_VALUE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get specific PredefinedAllowedAttributeValue by attribute id", description = "get specific PredefinedAllowedAttributeValue by store id and attribute id and pageable")
  
  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username, @RequestParam String attributeId,
      @RequestParam String value) throws Exception {
    PredefinedAllowedAttributeValueController.LOG.debug(attributeId);
    return this.populateListResponse(requestId, this.predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(storeId, value, attributeId));
  }

  @RequestMapping(value = PredefinedAllowedAttributeValueApiPath.FILTER_ATTRIBUTE_ID_AND_VALUE_FOR_SUGGESTIONS,
                  method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get PredefinedAllowedAttributeValue for brand suggestions", description = "get "
      + "PredefinedAllowedAttributeValue by store id and search parameter(value) and pageable")
  
  public GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getBrandSuggestions(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(required = false, defaultValue = SolrConstants.NA) String businessPartnerCode,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @RequestParam(required = false) String value, @RequestParam(defaultValue = "false") boolean isSearch,
      @RequestParam(defaultValue = "false") boolean isExternal) throws Exception {
    PageRequest pageRequest = PageRequest.of(page, size);
    Page<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValues = null;
    try {
       predefinedAllowedAttributeValues =
          this.predefinedAllowedAttributeValueService
              .getBrandSuggestions(storeId, value, businessPartnerCode, pageRequest, isSearch, isExternal);
      return new GdnRestListResponse<>(null, null, true, predefinedAllowedAttributeValues.getContent(),
          new PageMetaData(size, page, predefinedAllowedAttributeValues.getTotalElements()), requestId);
    } catch (Exception e) {
      LOG.error(
          "Exception caught in predefined allowed attribute value controller while getting brand suggestions Request "
              + "Id :{}", requestId, e);
      return new GdnRestListResponse<>(null, null, false, null, null, requestId);
    }
  }

  @SuppressWarnings("unused")
  private List<PredefinedAllowedAttributeValue> populateListFromRequest(String storeId, String channelId,
      String clientId, String requestId,
      List<PredefinedAllowedAttributeValueRequest> predefinedAllowedAttributeValueRequests) throws Exception {
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues =
        new ArrayList<PredefinedAllowedAttributeValue>();

    for (PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest : predefinedAllowedAttributeValueRequests) {
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
      BeanUtils.copyProperties(predefinedAllowedAttributeValueRequest, predefinedAllowedAttributeValue);
      predefinedAllowedAttributeValues.add(predefinedAllowedAttributeValue);
    }
    return predefinedAllowedAttributeValues;
  }

  private GdnRestListResponse<PredefinedAllowedAttributeValueResponse> populateListResponse(String storeId,
      String channelId, String clientId, String requestId, Integer page, Integer size, Pageable pageable,
      Page<PredefinedAllowedAttributeValue> attributePage) throws Exception {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> wrapper = null;
    List<PredefinedAllowedAttributeValueResponse> attributeResponses =
        new ArrayList<PredefinedAllowedAttributeValueResponse>();
    for (PredefinedAllowedAttributeValue attribute : attributePage.getContent()) {
      PredefinedAllowedAttributeValueResponse response = new PredefinedAllowedAttributeValueResponse();
      BeanUtils.copyProperties(attribute, response, "brandApprovalStatus");
      attributeResponses.add(response);
    }
    wrapper = new GdnRestListResponse<>(null, null, true, attributeResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), attributePage.getTotalElements()),
        requestId);
    return wrapper;
  }

  private GdnRestListResponse<PredefinedAllowedAttributeValueResponse> populateListResponse(String requestId,
      List<PredefinedAllowedAttributeValue> attributeValueList) throws Exception {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> wrapper = null;
    List<PredefinedAllowedAttributeValueResponse> attributeResponses =
        new ArrayList<PredefinedAllowedAttributeValueResponse>();
    for (PredefinedAllowedAttributeValue attribute : attributeValueList) {
      PredefinedAllowedAttributeValueResponse response = new PredefinedAllowedAttributeValueResponse();
      BeanUtils.copyProperties(attribute, response, "brandApprovalStatus");
      attributeResponses.add(response);
    }
    wrapper = new GdnRestListResponse<>(null, null, true, attributeResponses, new PageMetaData(), requestId);
    return wrapper;
  }

  @RequestMapping(value = PredefinedAllowedAttributeValueApiPath.SAVE, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "save PredefinedAllowedAttributeValue", description = "save PredefinedAllowedAttributeValue")
  
  public GdnBaseRestResponse savePredefinedAllowedAttribute(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String attributeId,
      @RequestBody PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest) throws Exception {
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    trimPredefinedValue(predefinedAllowedAttributeValueRequest);
    BeanUtils.copyProperties(predefinedAllowedAttributeValueRequest,
        predefinedAllowedAttributeValue, Constants.ID);
    this.predefinedAllowedAttributeValueService.saveWithGeneratedCode(storeId, attributeId,
        predefinedAllowedAttributeValue);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
  
  private void trimPredefinedValue(PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest){
    predefinedAllowedAttributeValueRequest.setValue(predefinedAllowedAttributeValueRequest.getValue().trim());
  }

}
