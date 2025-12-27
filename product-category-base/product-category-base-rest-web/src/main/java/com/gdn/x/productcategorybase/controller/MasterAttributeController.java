package com.gdn.x.productcategorybase.controller;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

import com.gdn.x.productcategorybase.exception.ValidationException;
import org.apache.commons.lang3.StringUtils;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.AttributeApiPath;
import com.gdn.x.productcategorybase.AttributeFilter;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.controller.util.ConverterUtil;
import com.gdn.x.productcategorybase.dto.AttributeValueDTO;
import com.gdn.x.productcategorybase.dto.AttributeValueUpdateDTO;
import com.gdn.x.productcategorybase.dto.MasterAttributeUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeAddRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeFilterRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.service.MasterAttributeService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;


/**
 * @author BhagwatiMalav - created on 30/10/18
 */
@RestController
@RequestMapping(value = AttributeApiPath.MASTER_ATTRIBUTE_BASE_PATH)
@Slf4j
@Tag(name = "MasterAttributeController", description = "Master Attribute Service API")
public class MasterAttributeController {

  private static final String ATTRIBUTE_VALUES_ERROR_MESSAGE = "ERROR IN FETCHING ATTRIBUTE VALUES";
  private static final String ATTRIBUTE_INFO_ERROR_MESSAGE = "ERROR IN FETCHING ATTRIBUTE INFO";
  private static final String ATTRIBUTE_INFO_ADD_ERROR_MESSAGE = "ERROR IN ADDING ATTRIBUTE";
  private static final String ATTRIBUTE_EDIT_ERROR_MESSAGE = "ERROR WHILE EDITING ATTRIBUTE INFO";
  private static final String ATTRIBUTE_FILTER_ERROR_MESSAGE = "ERROR WHILE FETCHING ATTRIBUTE LIST";
  private static final String ATTRIBUTE_VALUE_ADDITION_ERROR_MESSAGE = "ERROR WHILE ADDING ATTRIBUTE VALUE";


  @Autowired
  private MasterAttributeService masterAttributeService;


  @RequestMapping(method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "save master attribute", description = "save master attribute")
  
  public GdnBaseRestResponse saveMasterAttribute(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody MasterAttributeRequest request)
      throws Exception {
    GdnBaseRestResponse gdnBaseRestResponse = null;
    try {
      log.info("Started inserting master attribute : {}", request);
      Attribute attribute = ConverterUtil.convertMasterAttributeRequestToAttribute(request,
          storeId);
      this.masterAttributeService.insertMasterAttribute(attribute, request.getDimensionMapping(),
          request.getValueTypes());
      gdnBaseRestResponse = new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationException applicationException) {
      log.error("Exception occurred while adding master attribute : {}", request, applicationException);
      gdnBaseRestResponse = new GdnBaseRestResponse(applicationException.getErrorMessage(),
          applicationException.getErrorCodes().getCode().toString(), Boolean.FALSE, requestId);
    } catch (ValidationException ex) {
      log.error("Exception occurred while creating attribute : {}", request, ex);
      gdnBaseRestResponse =
          new GdnBaseRestResponse(ex.getErrorMessage(), ex.getErrorCode(), Boolean.FALSE, requestId);
    } catch (Exception e) {
      log.error("Error Occurred in saving master attribtue : {}", request, e);
      gdnBaseRestResponse =
          new GdnBaseRestResponse(ATTRIBUTE_INFO_ADD_ERROR_MESSAGE, ErrorCategory.UNSPECIFIED.getCode(), Boolean.FALSE, requestId);
    }
    return gdnBaseRestResponse;
  }

  @RequestMapping(value = AttributeApiPath.FILTER_ATTRIBUTE_VALUES_BY_ATTRIBUTE_CODE, method = RequestMethod.GET,
                  produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get summary of Attribute values", description = "get list of Attribute values by attribute code "
      + "and pageable")
  
  public GdnRestListResponse<AttributeValueResponse> getAttributeValuesBasedOnAttributeCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "50") Integer size,
      @PathVariable("attributeCode") String attributeCode, @RequestParam(defaultValue = "false") Boolean getAllValues,
      @RequestParam(defaultValue = "true") boolean concatenateValueWithValueType) {
    Pageable pageable = PageRequest.of(page, size);
    Page<AttributeValueDTO> attributeValueDTOPage = null;
    try {
      log.info("Getting Attribute values based on attribute code : {}", attributeCode);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(attributeCode),
          ErrorMessage.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK.getMessage());
      attributeValueDTOPage =
          this.masterAttributeService.getAttributeValuesByAttributeCode(storeId, attributeCode, pageable, getAllValues,
              concatenateValueWithValueType);
      return ConverterUtil.convertAttributeValueDTOToAttributeValueResponse(attributeValueDTOPage, pageable, requestId);
    } catch (ApplicationException applicationException) {
      log.error("Error in fetching attribute values , attribute code :{}", attributeCode, applicationException);
      return new GdnRestListResponse<AttributeValueResponse>(applicationException.getErrorMessage(), ATTRIBUTE_VALUES_ERROR_MESSAGE,
          false, null,
          new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), 0), requestId);
    } catch (Exception e) {
      log.error("Error in fetching attribute values , attribute code :{}", attributeCode, e);
      return new GdnRestListResponse<AttributeValueResponse>(ATTRIBUTE_VALUES_ERROR_MESSAGE, ATTRIBUTE_VALUES_ERROR_MESSAGE, false, null,
          new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), 0), requestId);
    }
  }

  @RequestMapping(value = AttributeApiPath.INFO, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get attribute by attribute code", description = "get attribute detail by store id"
      + " and attribute code ")
  
  public GdnRestSingleResponse<MasterAttributeResponse> getAttributeDetailByAttributeCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("attributeCode") String attributeCode)
      throws Exception {
    try {
      log.info("Getting attribute detail info for the attribute code : {}", attributeCode);
      attributeCode = URLDecoder.decode(attributeCode, StandardCharsets.UTF_8.name());
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(attributeCode),
          ErrorMessage.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK.getMessage());
      Attribute result = this.masterAttributeService.findDetailByAttributeCode(attributeCode);
      MasterAttributeResponse response = ConverterUtil.convertAttributeRequestToMasterAttributeResponse(result);
      return new GdnRestSingleResponse<MasterAttributeResponse>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException ex) {
      log.error(
          "Error while fetching attribute detail for attribute code: {}, Attribute Data not found for this attribute code",
          attributeCode, ex);
      return new GdnRestSingleResponse<MasterAttributeResponse>(ex.getErrorMessage(), null, false, null, requestId);
    } catch (Exception e) {
      log.error(
          "Error while fetching attribute detail for attribute code: {}, Attribute Data not found for this attribute code"
              + attributeCode, e);
      return new GdnRestSingleResponse<MasterAttributeResponse>(ATTRIBUTE_INFO_ERROR_MESSAGE, null, false, null, requestId);
    }
  }

  @RequestMapping(value = AttributeApiPath.UPDATE_VALUES, method = RequestMethod.PUT, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Update master attribute values", description = "Update master attribute values")
  
  public GdnBaseRestResponse updateMasterAttributeValues(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable String attributeCode,
      @RequestBody MasterAttributeUpdateRequest request) {
    GdnBaseRestResponse gdnBaseRestResponse;
    try {
      log.info("Started update master attribute values : {}", request);
      GdnPreconditions.checkArgument((StringUtils.isNoneBlank(request.getUpdatedBy())
              && Objects.nonNull(request.getUpdatedDate())),
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
      MasterAttributeUpdateDTO masterAttributeUpdateDTO =
          ConverterUtil.convertMasterAttributeUpdateRequestToMasterAttributeUpdateDTO(request);
      masterAttributeService.updateAttributeValues(storeId, attributeCode, masterAttributeUpdateDTO);
      gdnBaseRestResponse = new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException applicationRuntimeException) {
      log.error("Exception occurred while updating master attribute values : {}", request,
          applicationRuntimeException);
      gdnBaseRestResponse = new GdnBaseRestResponse(applicationRuntimeException.getErrorMessage(),
          applicationRuntimeException.getErrorCodes().getCode(), Boolean.FALSE, requestId);
  } catch (ApplicationException applicationException) {
      log.error("Exception occurred while updating master attribute values : {}", request,
          applicationException);
      gdnBaseRestResponse = new GdnBaseRestResponse(applicationException.getErrorMessage(),
          applicationException.getErrorCodes().getCode(), Boolean.FALSE, requestId);
    } catch (Exception e) {
      log.error("Error occurred while updating master attribute values : {}", request, e);
      gdnBaseRestResponse =
          new GdnBaseRestResponse(ATTRIBUTE_EDIT_ERROR_MESSAGE, ErrorCategory.UNSPECIFIED.getCode(), Boolean.FALSE, requestId);
    }
    return gdnBaseRestResponse;
  }

  @RequestMapping(value = AttributeApiPath.ADD_VALUE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Add master attribute value", description = "Add master attribute value")
  
  public GdnRestSingleResponse<AttributeValueResponse> addMasterAttributeValue(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable String attributeCode,
      @RequestBody MasterAttributeAddRequest request) {
    try {
      log.info("Started add master attribute value : {}", request.getValue());
      AttributeValueUpdateDTO attributeValueUpdateDTO =
          ConverterUtil.convertMasterAttributeAddRequestToAttributeValueUpdateDTO(request);
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getValue()),
          ErrorMessage.ATTRIBUTE_VALUE_MUST_NOT_BE_EMPTY.getMessage());
      AttributeValueResponse attributeValueResponse = masterAttributeService
          .addAttributeValue(storeId, attributeCode, attributeValueUpdateDTO, request.getCreatedBy(),
              request.getCreatedDate());
      return new GdnRestSingleResponse<AttributeValueResponse>(null, null, true, attributeValueResponse, requestId);
    } catch (ApplicationRuntimeException applicationRuntimeException) {
      log.error("Exception occurred while adding master attribute values : {}", request, applicationRuntimeException);
      return new GdnRestSingleResponse<AttributeValueResponse>(applicationRuntimeException.getErrorMessage(),
          applicationRuntimeException.getErrorCodes().getCode(), Boolean.FALSE, null, requestId);
    } catch (ApplicationException applicationException) {
      log.error("Exception occurred while adding master attribute value : {}", request, applicationException);
      return new GdnRestSingleResponse<AttributeValueResponse>(applicationException.getErrorMessage(),
          applicationException.getErrorCodes().getCode(), Boolean.FALSE, null, requestId);
    } catch (Exception e) {
      log.error("Error occurred while adding master attribute value : {}", request, e);
      return new GdnRestSingleResponse<AttributeValueResponse>(ATTRIBUTE_VALUE_ADDITION_ERROR_MESSAGE,
          ErrorCategory.UNSPECIFIED.getCode(), Boolean.FALSE, null, requestId);
    }
  }

  @RequestMapping(value = AttributeApiPath.UPDATE_MASTER_ATTRIBUTE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update master attribute", description = "update master attribute")
  
  public GdnBaseRestResponse updateMasterAttribute(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody MasterAttributeRequest request)
      throws Exception {
    GdnBaseRestResponse gdnBaseRestResponse = null;
    try {
      log.info("Started updating master attribute : {}", request);
      Attribute attribute = ConverterUtil.convertMasterAttributeRequestToAttributeExcludingAttributeValues(request,
          storeId);
      this.masterAttributeService.updateMasterAttribute(attribute, request.getValueTypes());
      gdnBaseRestResponse = new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException applicationRuntimeException) {
      log.error("Exception occurred while updating master attribute : {}", request, applicationRuntimeException);
      gdnBaseRestResponse = new GdnBaseRestResponse(applicationRuntimeException.getErrorMessage(),
          applicationRuntimeException.getErrorCodes().getCode(), Boolean.FALSE, requestId);
    } catch (ApplicationException applicationException) {
      log.error("Exception occurred while updating master attribute : {}", request, applicationException);
      gdnBaseRestResponse = new GdnBaseRestResponse(applicationException.getErrorMessage(),
          applicationException.getErrorCodes().getCode(), Boolean.FALSE, requestId);
    } catch (ValidationException validationException) {
      log.error("Exception occurred while updating master attribute : {}", request,
          validationException);
      gdnBaseRestResponse = new GdnBaseRestResponse(validationException.getErrorMessage(),
          validationException.getErrorCode(), Boolean.FALSE, requestId);
    } catch (Exception e) {
      log.error("Error Occurred in updating master attribute : {}", request, e);
      gdnBaseRestResponse =
          new GdnBaseRestResponse(ATTRIBUTE_EDIT_ERROR_MESSAGE, ErrorCategory.UNSPECIFIED.getCode(), Boolean.FALSE, requestId);
    }
    return gdnBaseRestResponse;
  }


  @RequestMapping(value = AttributeApiPath.FILTER_LIST, method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get attribute list by attribute filter", description = "get attribute list by "
      + "attribute filter and pageable")
   public GdnRestListResponse<MasterAttributeResponse> getAttributeByAttributeFilter(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestBody
      MasterAttributeFilterRequest request) {
    log.info("Getting attribute list by filter :{}", request);
    AttributeFilter filter = AttributeFilter.builder().build();
    try {
      BeanUtils.copyProperties(request, filter);
      Pageable pageable = PageRequest.of(page, size);
      Page<Attribute> attributePage =
          this.masterAttributeService.getAttributeByAttributeFilter(storeId, filter, pageable);
      return new GdnRestListResponse<MasterAttributeResponse>(null, null, true,
          ConverterUtil.toMasterAttributeResponsesFromAttributes(attributePage.getContent()),
          new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
              attributePage.getTotalElements()), requestId);
    } catch (ApplicationRuntimeException ex) {
      log.error("Error while Getting attribute list by filter :{}", request, ex);
      return new GdnRestListResponse<MasterAttributeResponse>(ex.getErrorMessage(),
          ex.getErrorCodes().getCode(), Boolean.FALSE, null, null, requestId);
    } catch (Exception ex) {
      log.error("Error while Getting attribute list by filter :{}", request, ex);
      return new GdnRestListResponse<MasterAttributeResponse>(ATTRIBUTE_FILTER_ERROR_MESSAGE, ErrorCategory.UNSPECIFIED.getCode(),
          Boolean.FALSE, null, null, requestId);
    }
  }

  @RequestMapping(value = AttributeApiPath.DETAIL_BY_ATTRIBUTE_CODE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get attribute by attribute code", description = "get attribute detail by store id"
      + " and attribute code ")
  
  public GdnRestSingleResponse<MasterAttributeResponse> getAttributeByAttributeCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String attributeCode) {
    try {
      log.info("Getting attribute info for the attribute code : {}", attributeCode);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(attributeCode),
          ErrorMessage.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK.getMessage());
      Attribute result = this.masterAttributeService.findDetailByAttributeCode(attributeCode);
      MasterAttributeResponse response = ConverterUtil.convertAttributeRequestToMasterAttributeResponse(result);
      return new GdnRestSingleResponse<MasterAttributeResponse>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException ex) {
      log.error(
          "Error while fetching attribute for attribute code: {}, Attribute Data not found for this attribute code",
          attributeCode, ex);
      return new GdnRestSingleResponse<>(ex.getErrorMessage(), null, false, null, requestId);
    } catch (Exception e) {
      log.error(
          "Error while fetching attribute for attribute code: {}, Attribute Data not found for this attribute code"
              + attributeCode, e);
      return new GdnRestSingleResponse<>(ATTRIBUTE_INFO_ERROR_MESSAGE, null, false, null, requestId);
    }
  }
}
