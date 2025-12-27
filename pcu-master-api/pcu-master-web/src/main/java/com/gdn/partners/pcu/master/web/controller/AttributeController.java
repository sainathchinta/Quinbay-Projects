package com.gdn.partners.pcu.master.web.controller;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.UUID;


import com.gdn.partners.pcu.master.service.impl.helper.BeanUtils;
import jakarta.servlet.http.HttpSession;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.EnumUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.security.model.Session;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.AttributeApiPath;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.model.attribute.AttributeFilterRequest;
import com.gdn.partners.pcu.master.model.attribute.AttributeResponse;
import com.gdn.partners.pcu.master.model.attribute.AttributeValue;
import com.gdn.partners.pcu.master.service.AttributeService;
import com.gdn.partners.pcu.master.web.controller.util.ConverterUtil;
import com.gdn.partners.pcu.master.web.model.request.AttributeTypeDTO;
import com.gdn.partners.pcu.master.web.model.request.AttributeValueAddWebRequest;
import com.gdn.partners.pcu.master.web.model.request.AttributeValuesUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.MasterAttributeDTO;
import com.gdn.partners.pcu.master.web.model.response.AttributeDetailWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeMasterResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeValueWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeValuesWebResponse;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeFilterRequest;
import com.gdn.partners.pcu.master.client.model.MasterAttributeRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Pradeep Reddy
 */
@Slf4j
@Tag(name = "Attribute API")
@RestController
@RequestMapping(value = AttributeApiPath.BASE_PATH)
public class AttributeController {

  @Autowired
  private AttributeService attributeService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "Get attribute values based on attribute code")
  @GetMapping(value = AttributeApiPath.GET_ATTRIBUTE_VALUES_BY_ATTRIBUTE_CODE, produces = MediaType
      .APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public ListBaseResponse<AttributeValueWebResponse> getAtrributeValues(HttpSession httpSession,
      @PathVariable String attributeCode, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "20") int size) {
    String requestId = String.valueOf(UUID.randomUUID());
    Pageable pageable =  PageRequest.of(page, size);
    Page<AttributeValue> attributeValues =
        this.attributeService.getAttributeValues(attributeCode, pageable, false, false);
    List<AttributeValueWebResponse> attributeValueWebResponseList = null;
    if (Objects.nonNull(attributeValues)) {
      attributeValueWebResponseList = ConverterUtil.toAttributeValueWebResponses(attributeValues.getContent());
    }
    return new ListBaseResponse<>(null, null, true, requestId, attributeValueWebResponseList,
        new Metadata(page, size, attributeValues.getTotalElements()));
  }

  @Operation(summary = "Get Attribute List")
  @PostMapping(value = AttributeApiPath.FILTER, produces = MediaType
      .APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public ListBaseResponse<AttributeResponse> filterAttributes(HttpSession httpSession,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @RequestBody AttributeFilterRequest attributeFilterRequest) throws Exception {
    Session session = (Session) httpSession.getAttribute("SESSION");
    String requestId = String.valueOf(UUID.randomUUID());
    MasterAttributeFilterRequest masterAttributeFilterRequest =
        MasterAttributeFilterRequest.builder().build();
    BeanUtils.copyProperties(attributeFilterRequest, masterAttributeFilterRequest);
    Pageable pageable = PageRequest.of(page, size);
    Page<MasterAttributeResponse> attributes =
        this.attributeService.findByFilter(masterAttributeFilterRequest, pageable);
    List<AttributeResponse> attributeResponses = ConverterUtil.toMasterAttributeResponses(attributes.getContent());
    return new ListBaseResponse<AttributeResponse>(null, null, true, requestId, attributeResponses,
        new Metadata(page, size, attributes.getTotalElements()));
  }

  @Operation(summary = "Add Master Attribute")
  @PostMapping(produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType
      .APPLICATION_JSON_VALUE)
  public BaseResponse addMasterAttribute(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody MasterAttributeDTO request) {
    BaseResponse baseResponse = new BaseResponse();
    MasterAttributeRequest masterAttributeRequest =
        ConverterUtil.convertMasterAttributeRequestToAttribute(request, storeId);
    GdnBaseRestResponse gdnBaseRestResponse = attributeService.addAttribute(masterAttributeRequest);
    BeanUtils.copyProperties(gdnBaseRestResponse, baseResponse);
    return baseResponse;
  }

  @Operation(summary = "Get attribute Details")
  @GetMapping(value = AttributeApiPath.GET_ATTRIBUTE_INFO, produces = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public SingleBaseResponse<AttributeMasterResponse> getAttributeDetail(HttpSession httpSession,
      @PathVariable String attributeCode) throws Exception {
    attributeCode = URLDecoder.decode(attributeCode, StandardCharsets.UTF_8.name());
    String requestId = String.valueOf(UUID.randomUUID());
    MasterAttributeResponse masterAttributeResponse = attributeService.getAttributeDetail(attributeCode);
    AttributeMasterResponse attributeMasterResponse =
        ConverterUtil.convertMasterAttributeResponseToAttributeResponse(masterAttributeResponse);
    return new SingleBaseResponse<>(null, null, true, requestId, attributeMasterResponse);
  }

  @Operation(summary = "update Master Attribute")
  @PutMapping(value = AttributeApiPath.UPDATE_ATTRIBUTE_DETAIL, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateMasterAttribute(@PathVariable String attributeCode,
      @RequestBody MasterAttributeDTO request) {
    BaseResponse baseResponse = new BaseResponse();
    GdnBaseRestResponse gdnBaseRestResponse = attributeService.updateAttribute(request);
    BeanUtils.copyProperties(gdnBaseRestResponse, baseResponse);
    return baseResponse;
  }

  @Operation(summary = "Update attribute values")
  @PutMapping(value = AttributeApiPath.UPDATE_ATTRIBUTE_VALUES,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public BaseResponse updateAttributeValues(@PathVariable String attributeCode,
      @RequestBody AttributeValuesUpdateWebRequest attributeValuesUpdateWebRequest) {
    GdnBaseRestResponse response = attributeService.updateAttributeValues(attributeCode,
        ConverterUtil.toAttributeValuesUpdateServiceRequest(attributeValuesUpdateWebRequest));
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(), response.isSuccess(),
       this.clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Get Attribute List by attribute type")
  @GetMapping(value = AttributeApiPath.ATTRIBUTES_LIST_BY_ATTRIBUTE_TYPE, produces = MediaType
      .APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public ListBaseResponse<AttributeResponse> getAttributeListByAttributeType(
      @PathVariable("attributeType") String attributeType) throws Exception {
    String requestId = this.clientParameterHelper.getRequestId();
    if (EnumUtils.isValidEnum(AttributeTypeDTO.class, attributeType)) {
      MasterAttributeFilterRequest masterAttributeFilterRequest =
          MasterAttributeFilterRequest.builder().attributeType(attributeType).build();
      Pageable pageable =  PageRequest.of(0, Integer.MAX_VALUE);
      Page<MasterAttributeResponse> attributes =
          this.attributeService.findByFilter(masterAttributeFilterRequest, pageable);
      List<AttributeResponse> attributeResponses = ConverterUtil.toMasterAttributeResponses(attributes.getContent());
      return new ListBaseResponse<AttributeResponse>(null, null, true, requestId, attributeResponses,
          new Metadata(0, Integer.MAX_VALUE, attributes.getTotalElements()));
    } else {
      return new ListBaseResponse<>(ErrorMessages.ERR_INVALID_ATTRIBUTE_TYPE, ErrorCategory.VALIDATION.getMessage(),
          false, requestId, null, new Metadata());
    }
  }

  @Operation(summary = "Get all the attribute values based on attribute code")
  @GetMapping(value = AttributeApiPath.GET_ALL_ATTRIBUTE_VALUES_BY_ATTRIBUTE_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public ListBaseResponse<AttributeValueWebResponse> getAllAttributeValues(@PathVariable String attributeCode,
      @RequestParam(defaultValue = "true") boolean concatenateValueWithValueType) {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Method : getAllAttributeValues for attributeCode : {}", attributeCode);
    if (StringUtils.isNotBlank(attributeCode)) {
      Page<AttributeValue> attributeValues =
          this.attributeService.getAttributeValues(attributeCode, PageRequest.of(0, Integer.MAX_VALUE), true,
              concatenateValueWithValueType);
      if (Objects.nonNull(attributeValues)) {
        List<AttributeValueWebResponse> attributeValueWebResponseList =
            ConverterUtil.toAttributeValueWebResponses(attributeValues.getContent());
        return new ListBaseResponse<>(null, null, true, requestId, attributeValueWebResponseList,
            new Metadata(0, attributeValueWebResponseList.size(), attributeValues.getTotalElements()));
      } else {
        return new ListBaseResponse<AttributeValueWebResponse>(ErrorMessages.ERR_EMPTY_ATTRIBUTE_VALUE,
            ErrorCategory.INVALID_FORMAT.getMessage(), false, requestId, Collections.emptyList(), new Metadata());
      }
    } else {
      return new ListBaseResponse<AttributeValueWebResponse>(ErrorMessages.ERR_EMPTY_ATTRIBUTE_CODE,
          ErrorCategory.INVALID_FORMAT.getMessage(), false, requestId, Collections.emptyList(), new Metadata());
    }
  }

  @Operation(summary = "Get all the attribute values based on attribute codes")
  @PostMapping(value = AttributeApiPath.GET_ALL_ATTRIBUTE_VALUES_BY_ATTRIBUTE_CODES, produces = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public ListBaseResponse<AttributeValuesWebResponse> getAllAttributeValuesByAttributeCodes(
      @RequestBody List<String> attributeCodes) {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Method : getAllAttributeValues for attributeCodes : {}", attributeCodes);
    if (CollectionUtils.isNotEmpty(attributeCodes)) {
      List<AttributeValuesWebResponse> response =
          this.attributeService.getAttributeValuesByAttributeCodes(attributeCodes);
      return new ListBaseResponse<>(null, null, true, requestId, response, new Metadata());
    } else {
      return new ListBaseResponse<>(ErrorMessages.ERR_EMPTY_ATTRIBUTE_CODE,
          ErrorCategory.INVALID_FORMAT.getMessage(), false, requestId, Collections.emptyList(), new Metadata());
    }
  }

  @Operation(summary = "Get predefined allowed attribute value based on attributeId and value")
  @GetMapping(value = AttributeApiPath.GET_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_BY_ATTRIBUTE_ID_AND_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<AttributeValueWebResponse> getPredefinedAllowedAttributesByAttributeIdAndValue(
      @PathVariable String attributeId, @PathVariable String value) {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Method : getPredefinedAllowedAttributes for attribute Id : {}", attributeId, " and value : {}", value);
    List<AttributeValueWebResponse> response =
        this.attributeService.getPredefinedAllowedAttributesByAttributeIdAndValue(attributeId, value);
    return new ListBaseResponse<>(null, null, true, requestId, response, new Metadata());
  }

  @Operation(summary = "Get specific predefined allowed attribute value based on attributeId and value")
  @GetMapping(value = AttributeApiPath.GET_SPECIFIC_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_BY_ATTRIBUTE_ID_AND_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<AttributeValueWebResponse> getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue(
      @RequestParam String attributeId, @RequestParam String value) throws Exception {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Method : getSpecificPredefinedAllowedAttributes for attribute Id : {} and value : {}", attributeId,
        value);
    List<AttributeValueWebResponse> response =
        this.attributeService.getSpecificPredefinedAllowedAttributesByAttributeIdAndValue(attributeId, value);
    return new ListBaseResponse<>(null, null, true, requestId, response, new Metadata());
  }

  @Operation(summary = "Add attribute value to an existing attribute")
  @PostMapping(value = AttributeApiPath.ADD_ATTRIBUTE_VALUE_BY_ATTRIBUTE_CODE, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<AttributeValueWebResponse> addAttributeValue(@PathVariable String attributeCode,
      @RequestBody AttributeValueAddWebRequest attributeValueAddWebRequest) {
    String requestId = this.clientParameterHelper.getRequestId();
    AttributeValueResponse response = attributeService.addAttributeValue(attributeCode,
        ConverterUtil.toAttributeValueAddServiceRequest(attributeValueAddWebRequest, clientParameterHelper));
    AttributeValueWebResponse attributeValueWebResponse = ConverterUtil.toAttributeValueWebResponse(response);
    return new SingleBaseResponse<>(null, null, true, requestId, attributeValueWebResponse);
  }

  @Operation(summary = "To get the attribute details using attribute code")
  @GetMapping(value = AttributeApiPath.GET_DETAIL_BY_ATTRIBUTE_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<AttributeDetailWebResponse> getAttributes(
      @PathVariable("attributeCode") String attributeCode) {
    String requestId = this.clientParameterHelper.getRequestId();
    AttributeDetailWebResponse attributeDetailWebResponse = attributeService.getAttributeDetails(attributeCode);
    return new SingleBaseResponse<AttributeDetailWebResponse>(null, null, true, requestId, attributeDetailWebResponse);
  }
}
