package com.gdn.x.productcategorybase.controller;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.response.SimpleListStringResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.supercsv.io.CsvBeanReader;
import org.supercsv.io.CsvBeanWriter;
import org.supercsv.io.ICsvBeanReader;
import org.supercsv.io.ICsvBeanWriter;
import org.supercsv.prefs.CsvPreference;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.AttributeApiPath;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.controller.util.AttributeControllerUtil;
import com.gdn.x.productcategorybase.controller.util.ConverterUtil;
import com.gdn.x.productcategorybase.csv.AttributeCsv;
import com.gdn.x.productcategorybase.dto.AttributeSummaryDTO;
import com.gdn.x.productcategorybase.dto.CategoryAttributeDetailDTO;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.service.AttributeService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@RestController
@RequestMapping(value = AttributeApiPath.BASE_PATH)
@Tag(name = "AttributeController", description = "Master Attribute Service API")
public class AttributeController {

  private static final String ATTRIBUTE_EXPORT_CSV_FILENAME = "attribute-export.csv";
  private static final Logger LOG = LoggerFactory.getLogger(AttributeController.class);

  private static final String ALLOWED_ATTRIBUTE_VALUES = "allowedAttributeValues";
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_VALUES = "predefinedAllowedAttributeValues";
  private static final String VERSION = "version";
  private static final String CREATED_DATE = "createdDate";
  private static final String CREATED_BY = "createdBy";

  @Autowired
  private AttributeService attributeService;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @RequestMapping(value = AttributeApiPath.DELETE_VALUE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "delete allowed attribute value", description = "delete allowed attribute value")
  
  public GdnBaseRestResponse deleteAllowedAttributeValue(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody SimpleRequestHolder request)
      throws Exception {
    this.attributeService.deleteAllowedAttributeValue(storeId, request.getId());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = AttributeApiPath.FILTER_ATTRIBUTE_CODE, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get attribute by attribute code", description = "get attribute by store id and"
      + " attribute code and pageable")
  
  public GdnRestListResponse<AttributeResponse> getAttributeByAttributeCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String attributeCode)
      throws Exception {
    AttributeController.LOG.debug(attributeCode.toString());
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, page, size, pageable,
        this.attributeService.findByAttributeCode(storeId, attributeCode, pageable));
  }

  @RequestMapping(value = AttributeApiPath.FILTER_ATTRIBUTE_CODES, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get list of attribute detail by attribute codes", description = "get list of "
      + "attribute detail by attribute codes")
  
  public GdnRestListResponse<AttributeResponse> getAttributeDetailByAttributeCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "false") boolean fetchOnlyBasicAttributeDetails,
      @RequestBody AttributeCodesRequest attributeCodeRequest) throws Exception {
    try {
      GdnPreconditions.checkArgument(attributeCodeRequest != null && !CollectionUtils
              .isEmpty(attributeCodeRequest.getAttributeCodes()),
          ErrorMessage.ATTRIBUTE_CODES_MUST_NOT_BE_BLANK.getMessage());
      List<AttributeResponse> responses = ConverterUtil.convertToAttributeResponses(
          this.attributeService.findDetailByStoreIdAndAttributeCodes(attributeCodeRequest.getAttributeCodes(),
              fetchOnlyBasicAttributeDetails), fetchOnlyBasicAttributeDetails);
      return new GdnRestListResponse<AttributeResponse>(responses,
          new PageMetaData(responses.size(), 1, responses.size()), requestId);
    } catch (Exception e) {
      LOG.error("error when get list of attribute details by attribute codes, error = {}",
          e.getMessage(), e);
      return new GdnRestListResponse<AttributeResponse>(e.getMessage(),
          ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = AttributeApiPath.FILTER_ATTRIBUTE_DETAIL_BY_CATEGORY_CODE, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get attribute by attribute code", description = "get attribute by store id and"
      + " attribute code and pageable")
  
  public GdnRestSingleResponse<CategoryAttributeSummaryResponse> getAttributeDetailByCategoryCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String categoryCode, @RequestParam(defaultValue = "true") boolean concatenateValueWithValueType) {

    try {
      CategoryAttributeDetailDTO result =
          this.attributeService.getAttributeDetailByCategoryCode(categoryCode, concatenateValueWithValueType);
      CategoryAttributeSummaryResponse response = new CategoryAttributeSummaryResponse();
      BeanUtils.copyProperties(result, response);

      return new GdnRestSingleResponse<CategoryAttributeSummaryResponse>(null, null, true, response,
          requestId);
    } catch (Exception e) {
      LOG.error("getAttributeDetailByCategoryCode: " + e.getMessage(), e);
      return new GdnRestSingleResponse<CategoryAttributeSummaryResponse>(e.getMessage(), null,
          false, null, requestId);
    }
  }
  
  @RequestMapping(value = AttributeApiPath.FILTER_ATTRIBUTE_DETAIL_BY_CATEGORY_CODE_WITHOUT_OPTIONS,
      method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get attribute detail by category code without attribute's option",
      description = "get attribute by store id and without attribute options")
  
  public GdnRestListResponse<AttributeDetailResponse> getAttributeDetailByCategoryCodeWithoutOptions(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String categoryCode) {
    try {
      List<AttributeSummaryDTO> result =
          this.attributeService.getAttributeDetailByCategoryCodeWithoutOptions(storeId, categoryCode);
      List<AttributeDetailResponse> response =
          AttributeControllerUtil.convertIntoAttributeSummaryResponse(result);
      return new GdnRestListResponse<AttributeDetailResponse>(response,
          null, requestId);
    } catch (Exception e) {
      LOG.error("getAttributeDetailByCategoryCode: " + e.getMessage(), e);
      return new GdnRestListResponse<AttributeDetailResponse>(e.getMessage(),
          ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = AttributeApiPath.FILTER_TYPE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get attribute by attribute type", description = "get attribute by store id and"
      + " attribute type and pageable")
  
  public GdnRestListResponse<AttributeResponse> getAttributeByAttributeTypeAndPageable(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String attributeType)
      throws Exception {
    AttributeController.LOG.debug(attributeType.toString());
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, page, size, pageable,
        this.attributeService
            .findByAttributeType(storeId, AttributeType.valueOf(attributeType), pageable));
  }

  @RequestMapping(value = AttributeApiPath.FILTER_NAME_LIKE, method = RequestMethod.GET, produces
      = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get attribute by name like ignore case with", description = "get attribute by "
      + "store id and name like ignore case with and pageable")
  
  public GdnRestListResponse<AttributeResponse> getAttributeByNameLikeAndPageable(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @RequestParam(defaultValue = "%") String name) throws Exception {
    AttributeController.LOG.debug(name.toString());
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, page, size, pageable,
        this.attributeService.findByNameLikeIgnoreCase(storeId, name, pageable));
  }

  @RequestMapping(value = AttributeApiPath.FILTER_NAME, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get attribute by name starting with", description = "get attribute by store id"
      + " and name starting with and pageable")
  
  public GdnRestListResponse<AttributeResponse> getAttributeByNameStartingWithAndPageable(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String name) throws Exception {
    AttributeController.LOG.debug(name.toString());
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, page, size, pageable,
        this.attributeService.findByNameStartingWith(storeId, name, pageable));
  }

  @RequestMapping(value = AttributeApiPath.FILTER_SEARCHABLE_FALSE, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get attribute by searchable false", description = "get attribute by store id "
      + "and searchable false and pageable")
  
  public GdnRestListResponse<AttributeResponse> getAttributeBySearchAbleFalseAndPageable(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    AttributeController.LOG.debug(page.toString());
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, page, size, pageable,
        this.attributeService.findBySearchAbleFalse(storeId, pageable));
  }

  @RequestMapping(value = AttributeApiPath.FILTER_SEARCHABLE_TRUE, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get attribute by searchable true", description = "get attribute by store id "
      + "and searchable true and pageable")
  
  public GdnRestListResponse<AttributeResponse> getAttributeBySearchAbleTrueAndPageable(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    AttributeController.LOG.debug(page.toString());
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, page, size, pageable,
        this.attributeService.findBySearchAbleTrue(storeId, pageable));
  }

  @RequestMapping(value = AttributeApiPath.
      DETAIL, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get attribute detail", description = "get attribute detail")
  
  public GdnRestSingleResponse<AttributeResponse> getAttributeDetail(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("id") String attributeId,
      @RequestParam(required = false) boolean allowedAttributeValuesTrim,
      @RequestParam(defaultValue = "true") boolean concatenateValueWithValueType,
      @RequestParam(defaultValue = "false") boolean sortValues) {
    AttributeResponse response = new AttributeResponse();
    try {
      Attribute attribute = this.attributeService.findDetailByStoreIdAndId(storeId, attributeId, false);
      ConverterUtil.setAttributeDetailResponse(allowedAttributeValuesTrim, attribute, response,
          valueTypeAdditionForDefiningAttributes, concatenateValueWithValueType, sizeChartValueTypeDelimiter, sortValues);
      return new GdnRestSingleResponse<AttributeResponse>("", "", true, response, requestId);
    } catch (Exception e) {
      LOG.error("Error in Attribute Controller To get detail for id : {}", attributeId, e);
      return new GdnRestSingleResponse<>(e.getMessage(), e.getMessage(), false, response, requestId);
    }
  }

  @RequestMapping(value = AttributeApiPath.FIND_ATTRIBUTE_DETAIL_BY_ID_AND_VALUE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get attribute detail by id and value", description = "get attribute detai by id and value")

  public GdnRestSingleResponse<AttributeResponse> getAttributeDetailById(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required= false) String username, @PathVariable("id") String attributeId,
    @PathVariable("value") String value,
    @RequestParam(required = false, defaultValue = "false") boolean ignoreCaseFetch) {
    try {
      LOG.info("get attribute detail by id : {} and value {}", attributeId, value);
      AttributeResponse attributeResponse =
          this.attributeService.findDetailByStoreIdAndIdAndValue(storeId, attributeId, value, ignoreCaseFetch);
      return new GdnRestSingleResponse<>(null, null, true, attributeResponse, requestId);
    } catch (Exception e) {
      LOG.error("Error in Attribute Controller To get detail for id : {} and value {} ", attributeId, value, e);
      return new GdnRestSingleResponse<>(e.getMessage(), e.getMessage(), false, null, requestId);
    }
  }

  @RequestMapping(value = AttributeApiPath.GET_DETAIL_BY_ATTRIBUTE_CODE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get attribute detail by attribute code", description = "get attribute detail by attribute code")
  
  public GdnRestSingleResponse<AttributeResponse> getAttributeDetailAndValuesByAttributeCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("attributeCode") String attributeCode,
      @RequestParam(required = false) boolean allowedAttributeValuesTrim) {
    Attribute attribute = this.attributeService.findDetailByStoreIdAndAttributeCode(storeId, attributeCode);
    return new GdnRestSingleResponse<AttributeResponse>(null, null, true, ConverterUtil.toAttributeResponse(attribute),
        requestId);
  }


  @RequestMapping(method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get summary of Attribute", description = "get list of Attribute by store id "
      + "and pageable")
  
  public GdnRestListResponse<AttributeResponse> getAttributeSummary(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(storeId, channelId, clientId, requestId, page, size, pageable,
        this.attributeService.findByStoreId(storeId, pageable));
  }
 
  private GdnRestListResponse<AttributeResponse> populateListResponse(String storeId,
      String channelId, String clientId, String requestId, Integer page, Integer size,
      Pageable pageable, Page<Attribute> attributePage) throws Exception {
    GdnRestListResponse<AttributeResponse> wrapper = null;
    List<AttributeResponse> attributeResponses = new ArrayList<AttributeResponse>();
    for (Attribute attribute : attributePage.getContent()) {
      AttributeResponse response = new AttributeResponse();
      BeanUtils.copyProperties(attribute, response, ALLOWED_ATTRIBUTE_VALUES,
          PREDEFINED_ALLOWED_ATTRIBUTE_VALUES);
      response.setAttributeType(attribute.getAttributeType().toString());
      attributeResponses.add(response);
    }
    wrapper = new GdnRestListResponse<AttributeResponse>(null, null, true, attributeResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            attributePage.getTotalElements()), requestId);
    return wrapper;
  }

  @RequestMapping(value = AttributeApiPath.SAVE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "save attribute", description = "save attribute")
  
  public GdnBaseRestResponse saveAttribute(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody AttributeRequest request)
      throws Exception {
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getCreatedBy()) || StringUtils
            .isEmpty(request.getCreatedDate())),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage());
    Attribute attribute = new Attribute();
    AttributeType attributeType = AttributeType.valueOf(request.getAttributeType().toString());
    if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attributeType)
        || AttributeType.DEFINING_ATTRIBUTE.equals(attributeType)) {
      if (!request.isBasicView())
        return new GdnBaseRestResponse("PREDEFINED/DEFINING Attribute cannot be in Basic View",
            ErrorCategory.VALIDATION.toString(), false, requestId);
    }
    BeanUtils.copyProperties(request, attribute, ALLOWED_ATTRIBUTE_VALUES,
        PREDEFINED_ALLOWED_ATTRIBUTE_VALUES, "attributeType", Constants.ID);
    attribute.setAttributeType(AttributeType.valueOf(request.getAttributeType().toString()));

    for (AllowedAttributeValueRequest allowedAttributeValueRequest : request
        .getAllowedAttributeValues()) {
      AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
      BeanUtils.copyProperties(allowedAttributeValueRequest, allowedAttributeValue, "attribute",
          Constants.ID);
      allowedAttributeValue.setAttribute(attribute);
      if (StringUtils.isEmpty(allowedAttributeValue.getStoreId())) {
        allowedAttributeValue.setStoreId(storeId);
      }
      attribute.getAllowedAttributeValues().add(allowedAttributeValue);
    }

    for (PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest : request
        .getPredefinedAllowedAttributeValues()) {
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
          new PredefinedAllowedAttributeValue();
      BeanUtils.copyProperties(predefinedAllowedAttributeValueRequest,
          predefinedAllowedAttributeValue, "attribute", Constants.ID);
      predefinedAllowedAttributeValue.setAttribute(attribute);
      if (StringUtils.isEmpty(predefinedAllowedAttributeValue.getStoreId())) {
        predefinedAllowedAttributeValue.setStoreId(storeId);
      }
      attribute.getPredefinedAllowedAttributeValues().add(predefinedAllowedAttributeValue);
    }
    if (StringUtils.isEmpty(attribute.getStoreId())) {
      attribute.setStoreId(storeId);
    }
    this.attributeService.save(attribute);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = AttributeApiPath.UPDATE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update attribute", description = "update attribute")
  
  public GdnBaseRestResponse updateAttribute(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody AttributeRequest request)
      throws Exception {
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getUpdatedBy()) || StringUtils
            .isEmpty(request.getUpdatedDate())),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());

    Attribute attribute = this.attributeService.findDetailByStoreIdAndId(storeId, request.getId(), true);

    Attribute newAttribute = new Attribute();
    BeanUtils.copyProperties(request, attribute, ALLOWED_ATTRIBUTE_VALUES,
        PREDEFINED_ALLOWED_ATTRIBUTE_VALUES, "attributeType", "categoryAttributes", VERSION,
        CREATED_BY, CREATED_DATE, "attributeCode", Constants.ID);
    attribute.setAttributeType(AttributeType.valueOf(request.getAttributeType().toString()));
    
    for (AllowedAttributeValueRequest allowedAttributeValueRequest : request
        .getAllowedAttributeValues()) {
      AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
      BeanUtils.copyProperties(allowedAttributeValueRequest, allowedAttributeValue, "attribute",
          VERSION, CREATED_BY, CREATED_DATE, Constants.ID);
      allowedAttributeValue.setAttribute(attribute);
      if (StringUtils.isEmpty(allowedAttributeValue.getStoreId())) {
        allowedAttributeValue.setStoreId(storeId);
      }
      newAttribute.getAllowedAttributeValues().add(allowedAttributeValue);
    }

    for (PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest : request
        .getPredefinedAllowedAttributeValues()) {
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
          new PredefinedAllowedAttributeValue();
      BeanUtils
          .copyProperties(predefinedAllowedAttributeValueRequest, predefinedAllowedAttributeValue,
              "attribute", VERSION, CREATED_BY, CREATED_DATE);
      predefinedAllowedAttributeValue.setAttribute(attribute);
      if (StringUtils.isEmpty(predefinedAllowedAttributeValue.getStoreId())) {
        predefinedAllowedAttributeValue.setStoreId(storeId);
      }
      newAttribute.getPredefinedAllowedAttributeValues().add(predefinedAllowedAttributeValue);
    }

    if (StringUtils.isEmpty(attribute.getStoreId())) {
      attribute.setStoreId(storeId);
    }

    this.attributeService.regenerateAllowedAttributeValue(storeId, attribute, newAttribute);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }


  @RequestMapping(value = AttributeApiPath.UPLOAD, method = RequestMethod.POST, produces = {
      "text/csv"}, consumes = {MediaType.MULTIPART_FORM_DATA_VALUE})
  
  @Operation(summary = "upload attribute", description = "upload attribute")
  public byte[] uploadAttribute(HttpServletRequest request, HttpServletResponse response,
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestPart("attributeCsvFile") MultipartFile attributeCsvFile) throws Exception {
    List<String> attributeIds = new ArrayList<String>();
    ICsvBeanReader beanReader = null;
    try {
      beanReader =
          new CsvBeanReader(new InputStreamReader(attributeCsvFile.getInputStream(), "UTF-8"),
              CsvPreference.STANDARD_PREFERENCE);
      beanReader.getHeader(true);
      AttributeCsv attributeCsv = null;
      AttributeCsv currentAttributeCsv = null;
      Attribute currentAttribute = null;

      while ((attributeCsv = beanReader
          .read(AttributeCsv.class, AttributeCsv.INPUT_HEADER, AttributeCsv.INPUT_PROCESSORS))
          != null) {
        storeId = attributeCsv.getStoreId();
        if (!attributeCsv.equals(currentAttributeCsv)) {
          if (currentAttribute != null) {
            attributeIds.add(this.attributeService.save(currentAttribute));
          }
          currentAttributeCsv = attributeCsv;
          currentAttribute = new Attribute();
          BeanUtils.copyProperties(attributeCsv, currentAttribute);
        }
        if (currentAttribute.getAttributeType().toString()
            .equals(AttributeType.DEFINING_ATTRIBUTE.toString())) {
          currentAttribute.getAllowedAttributeValues().add(
              new AllowedAttributeValue(currentAttribute, attributeCsv.getAllowedAttributeValue(),
                  attributeCsv.getStoreId(), attributeCsv.getAllowedAttributeSequence()));
        } else if (currentAttribute.getAttributeType().toString()
            .equals(AttributeType.PREDEFINED_ATTRIBUTE.toString())) {
          currentAttribute.getPredefinedAllowedAttributeValues().add(
              new PredefinedAllowedAttributeValue(currentAttribute,
                  attributeCsv.getAllowedAttributeValue(), attributeCsv.getStoreId(),
                  attributeCsv.getAllowedAttributeSequence()));
        }
      }
      attributeIds.add(this.attributeService.save(currentAttribute));
    } finally {
      if (beanReader != null) {
        beanReader.close();
      }
    }

    ICsvBeanWriter beanWriter = null;
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    PrintWriter writer = new PrintWriter(new BufferedOutputStream(out, 1024));
    try {
      beanWriter = new CsvBeanWriter(writer, CsvPreference.STANDARD_PREFERENCE);
      beanWriter.writeHeader(AttributeCsv.ACTUAL_HEADER);

      for (String attributeId : attributeIds) {
        Attribute attribute = this.attributeService.findDetailByStoreIdAndId(storeId, attributeId, false);
        AttributeCsv attributeCsv = new AttributeCsv();
        BeanUtils.copyProperties(attribute, attributeCsv);
        if (attribute.getAttributeType().toString()
            .equals(AttributeType.DEFINING_ATTRIBUTE.toString())) {
          for (AllowedAttributeValue allowedAttributeValue : attribute
              .getAllowedAttributeValues()) {
            attributeCsv.setAllowedAttributeSequence(allowedAttributeValue.getSequence());
            attributeCsv.setAllowedAttributeValue(allowedAttributeValue.getValue());
            attributeCsv.setAllowedAttributeValueId(allowedAttributeValue.getId());
            beanWriter
                .write(attributeCsv, AttributeCsv.OUTPUT_HEADER, AttributeCsv.OUTPUT_PROCESSORS);
          }
        } else if (attribute.getAttributeType().toString()
            .equals(AttributeType.PREDEFINED_ATTRIBUTE.toString())) {
          for (PredefinedAllowedAttributeValue predefinedAllowedAttributeValue : attribute
              .getPredefinedAllowedAttributeValues()) {
            attributeCsv.setAllowedAttributeSequence(predefinedAllowedAttributeValue.getSequence());
            attributeCsv.setAllowedAttributeValue(predefinedAllowedAttributeValue.getValue());
            attributeCsv.setAllowedAttributeValueId(predefinedAllowedAttributeValue.getId());
            beanWriter
                .write(attributeCsv, AttributeCsv.OUTPUT_HEADER, AttributeCsv.OUTPUT_PROCESSORS);
          }
        } else {
          beanWriter
              .write(attributeCsv, AttributeCsv.OUTPUT_HEADER, AttributeCsv.OUTPUT_PROCESSORS);
        }
      }
    } finally {
      if (beanWriter != null) {
        beanWriter.close();
      }
    }
    out.close();
    byte[] result = out.toByteArray();
    response.setHeader("Content-Disposition",
        "attachment; filename=\"" + AttributeController.ATTRIBUTE_EXPORT_CSV_FILENAME + "\"");
    return result;
  }

  @RequestMapping(value = AttributeApiPath.GET_ATTRIBUTE_VALUES, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get attribute values by product code and attribute code", description = "get attribute values by product code and attribute code")
  
  public GdnRestSingleResponse<SingleObjectResponse> getAttributeValuesByProductCodeAndAttributeCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = true) String productCode, @RequestParam(required = true) String attributeCode) {
    try {
      LOG.info("get attribute values by productCode : {} and attributeCode : {} ", productCode, attributeCode);
      String response =
          this.attributeService.findProductAttributeValuesByProductCodeAndAttributeCode(storeId, productCode,
              attributeCode);
      return new GdnRestSingleResponse<SingleObjectResponse>(null, null, true, new SingleObjectResponse<>(response),
          requestId);
    } catch (Exception e) {
      LOG.info("Error while fetching attribute values for given product code : {} and attribute code : {} ",
          productCode, attributeCode);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @GetMapping(value = AttributeApiPath.CATEGORIES_BY_ATTRIBUTE_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  
  @Operation(summary = "get category codes by attribute code", description = "get category codes "
      + "by attribute code")
  public GdnRestSingleResponse<SimpleListStringResponse> getCategoryCodesByAttributeCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String attributeCode) {
    GdnPreconditions.checkArgument(org.apache.commons.lang.StringUtils.isNotBlank(attributeCode),
        ErrorMessage.REQUEST_LIST_MUST_NOT_BE_EMPTY.getMessage());
    return new GdnRestSingleResponse<>(null, null, true,
        new SimpleListStringResponse(attributeService.findCategoryCodesByAttributeCode(storeId, attributeCode)),
        requestId);
  }
}
