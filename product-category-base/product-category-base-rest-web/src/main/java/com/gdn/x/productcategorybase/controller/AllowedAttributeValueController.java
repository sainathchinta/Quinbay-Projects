package com.gdn.x.productcategorybase.controller;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.AllowedAttributeValueApiPath;
import com.gdn.x.productcategorybase.dto.AttributeOptionDTO;
import com.gdn.x.productcategorybase.dto.ListHolderRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoResponse;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeOptionResponse;
import com.gdn.x.productcategorybase.helper.AllowedAttributeValueConverter;
import com.gdn.x.productcategorybase.service.AllowedAttributeValueService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RestController;

/**
 * Provide API to fetch allowed attribute value for both PREDEFINED and DEFINING attribute
 * 
 * @author agie.falah
 *
 */
@RestController
@RequestMapping(value = AllowedAttributeValueApiPath.BASE_PATH)
@Tag(name = "AllowedAttributeController", description = "Allowed attribute value service API")
public class AllowedAttributeValueController {
  private static final Logger LOG = LoggerFactory.getLogger(AllowedAttributeValueController.class);
  
  @Autowired 
  private AllowedAttributeValueService allowedAttributeValueService;
  
  @RequestMapping(value = AllowedAttributeValueApiPath.FIND_ALLOWED_VALUES, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get allowed value for predefined and defining attribute", description = "get allowed value for predefined and defining attribute")
  
  public GdnRestListResponse<AllowedAttributeValueResponse> getPredefinedAndDefiningAllowedAttributeValue(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ListHolderRequest<AllowedAttributeValueRequest> requestList) {
    try{
      LOG.info("Finding allowed values for request : {} ", requestList);
      List<AllowedAttributeValueDtoResponse> response =
          allowedAttributeValueService.findAllowedPredefiningAndDefiningAttributeValue(
              AllowedAttributeValueConverter.convertAllowedValueRestRequestToDtoRequest(requestList.getLists()));
      return new GdnRestListResponse<>(
          AllowedAttributeValueConverter.convertAllowedValueDtoResponseToRestResponse(response), null, requestId);
    } catch(Exception e){
      LOG.error("Error while fetching attribute values - request : {} ", requestList, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }
  
  @RequestMapping(value = AllowedAttributeValueApiPath.GET_ATTRIBUTE_OPTIONS,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get attribute options by attributeCode and keyword")
  
  public GdnRestListResponse<AttributeOptionResponse> getAttributeOptions(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String attributeCode, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @RequestParam(required=false) String keyword) throws Exception {
    try {
      Pageable pageable = PageRequest.of(page, size);
      Page<AttributeOptionDTO> options = this.allowedAttributeValueService
          .getAttributeOptionsByAttributeCodeAndKeyword(attributeCode, keyword, pageable);
      LOG.info("Calling API to get attribute options by attribute code = {} and keyword = {}", attributeCode, keyword);
      return new GdnRestListResponse<>(
          AllowedAttributeValueConverter.convertIntoAttributeOptionResponse(options.getContent()), 
          new PageMetaData(options.getSize(), options.getNumber(), options.getTotalElements()), requestId);
    }catch(Exception e) {
      LOG.error("error when get list of attribute options by attribute codes = {} and keyword = {}, error = {}",
          attributeCode, keyword, e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }
  
}
