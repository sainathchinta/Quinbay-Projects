package com.gdn.partners.pcu.internal.service.impl;

import com.gda.mta.product.dto.ProductCollectionResponse;
import com.gda.mta.product.dto.ProductHistoryRequest;
import com.gda.mta.product.dto.ProductHistoryResponse;
import com.gda.mta.product.dto.ProductRevisionInfoResponse;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.FilterCountResponse;
import com.gda.mta.product.dto.response.HalalProductHistoryResponse;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gda.mta.product.dto.response.SequenceResponse;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.dto.product.BulkProductSuspensionRequest;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductCollectionCountRestResponse;
import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;
import com.gdn.partners.pcu.internal.client.feign.BPJPHFeign;
import com.gdn.partners.pcu.internal.client.feign.MarginFeign;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.client.feign.XProductFeign;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.client.model.response.BPJPHListResponse;
import com.gdn.partners.pcu.internal.client.model.response.HalalCertificationDetailResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductDetailCompleteResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductL3BasicResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.properties.ExtCatalogProperties;
import com.gdn.partners.pcu.internal.properties.KafkaTopicProperties;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.BPService;
import com.gdn.partners.pcu.internal.service.CacheProductService;
import com.gdn.partners.pcu.internal.service.CategoryService;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.ProductService;
import com.gdn.partners.pcu.internal.service.ProductSuggestionService;
import com.gdn.partners.pcu.internal.service.ProductWorkflowService;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.event.model.ProductAttributeFeedbackEventModel;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.service.impl.util.BeanUtils;
import com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.internal.streaming.model.bulk.DownloadType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.MasterProductDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.SelectedMasterProductDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.enums.Source;
import com.gdn.partners.pcu.internal.web.model.request.CountWebRequest;
import com.gdn.partners.pcu.internal.client.model.request.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pcu.internal.web.model.request.HalalProductsFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuggestionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ScreeningProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SelectedMasterProductDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SuspensionProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.TimeFilterWebType;
import com.gdn.partners.pcu.internal.web.model.response.CategoryChangeCheckResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryChangeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.FilterCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalCertificationWebDetailsResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalDashboardProductsWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MapWebResponse;
import com.gdn.partners.pcu.internal.client.model.response.OrderItemMarginsResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCollectionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductReviewerWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductRevisionHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuggestionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ReviewProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.TemplateDownloadFilePathWebResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTNeedRevisionEventModel;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.HalalDashboardProductsResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.text.SimpleDateFormat;
import java.util.stream.Collectors;

/**
 * Created by govind on 11/01/2019 AD.
 */

@Service
@Slf4j
public class ProductServiceImpl implements ProductService{

  private static final SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
  public static final String SPECIAL_MARGIN = "SPECIAL-MARGIN";
  public static final String BASE = "BASE";
  public static final String BASE_ADDON = "BASE-ADDON";
  public static final String BASE_MARGIN = "BASE-MARGIN";
  public static final String INTERNAL = "INTERNAL";

  @Autowired
  private ProductWorkflowService productWorkflowService;

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private PDTFeign pdtFeign;

  @Autowired
  private ProductSuggestionService productSuggestionService;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private CacheProductService cacheProductService;

  @Autowired
  private ExtCatalogProperties extCatalogProperties;

  @Autowired
  private SystemParameterProperties systemParameterProperties;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private BPService bpService;

  @Autowired
  private XProductFeign xProductFeign;

  @Autowired
  private MarginFeign marginFeign;

  @Autowired
  private BPJPHFeign bpjphFeign;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Value("${client.parameter.client-id}")
  private String clientHost;

  @Value("${attribute.code.for.certificate.number}")
  private String attributeCode;

  @Value("${halal.product.link.prefix}")
  private String halalProductLinkPrefix;

  @Value("${halal.third.party.api.switch}")
  private boolean halalThirdPartyApiSwitch;

  @Value("${halal.validation.api.key}")
  private String halalApiKey;

  @Value("${margin.new.changes.enabled}")
  private boolean marginNewChangesEnabled;

  @Value("${set.default.orderType.margin}")
  private boolean setDefaultOrderTypeForMargin;

  @Value("${default.orderType.margin}")
  private String defaultOrderTypeForMargin;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${check.product.type.switch}")
  private boolean checkProductTypeSwitch;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${product.detail.page.url.prefix}")
  private String productDetailPageUrlPrefix;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Override
  public void returnForCorrection(
      ProductReturnForCorrectionRequest productReturnForCorrectionRequest) {
    this.productWorkflowService.returnForCorrection(productReturnForCorrectionRequest);
  }

  @Override
  public void mergeProduct(String masterProductId, String duplicateProductId, boolean isForceMerge){
    GdnBaseRestResponse response =
        this.pbpFeign.mergeProducts(masterProductId, duplicateProductId, isForceMerge);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void updateProductAndPublishToPDT(ProductRequest productRequest){
    GdnBaseRestResponse response = this.pbpFeign.updateAndPublishProductToPDT(productRequest);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public ProductDetailResponse findProduct(String productId){
    GdnRestSingleResponse<ProductDetailResponse> response = this.pbpFeign.getProduct(productId);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public String generateBarcode(){
    GdnRestSimpleResponse<String> response = this.pbpFeign.generateBarcode();
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public void updateProduct(ProductRequest productRequest, boolean isActive) throws Exception {
    String businessPartnerCode = clientParameterHelper.getBusinessPartnerCode();
    GdnRestSingleResponse<ProductDetailCompleteResponse> productDetail =
        this.pbpFeign.getProductDetail(productRequest.getProductCode(), false, businessPartnerCode);
    ResponseHelper.validateResponse(productDetail);
    if (!isActive) {
      ResponseHelper.validateProductState(productDetail);
    }
    productRequest.getProductAttributes().stream().filter(
        productAttribute -> !productAttribute.getAttribute().isSkuValue() && productAttribute.getAttribute()
            .getAttributeType().equals(AttributeType.DESCRIPTIVE_ATTRIBUTE) && StringUtils
            .isEmpty(productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValue())).forEach(
        productAttribute -> productAttribute.getProductAttributeValues().get(0).setDescriptiveAttributeValue("-"));
    productRequest.setSpecificationDetail(RequestHelper.generateSpecificationDetail(productRequest));
    GdnBaseRestResponse response = this.pbpFeign.updateProduct(productRequest);
    ResponseHelper.validateResponse(response);
    validateUpdateOnDSExtractedAttributesAndPublishFeedback(productRequest,
        productDetail.getValue());
  }

  private void validateUpdateOnDSExtractedAttributesAndPublishFeedback(
      ProductRequest productRequest, ProductDetailCompleteResponse productDetail) {
    List<ProductAttributeRequest> productAttributeRequests = productRequest.getProductAttributes();
    List<ProductAttributeResponse> productAttributeResponses =
        productDetail.getProductAttributeResponses();
    Map<String, ProductAttributeRequest> productAttributeRequestMap = new HashMap<>();
    Map<String, ProductAttributeResponse> productAttributeResponseMap = new HashMap<>();
    Set<String> attributeCodes = new HashSet<>();
    for (ProductAttributeRequest productAttributeRequest : productAttributeRequests) {
      if (productAttributeRequest.getAttribute().isDsExtraction()) {
        attributeCodes.add(productAttributeRequest.getAttribute().getAttributeCode());
        productAttributeRequestMap.put(productAttributeRequest.getAttribute().getAttributeCode(),
            productAttributeRequest);
      }
    }
    for (ProductAttributeResponse productAttributeResponse : productAttributeResponses) {
      if (productAttributeResponse.getAttribute().isDsExtraction()) {
        attributeCodes.add(productAttributeResponse.getAttribute().getAttributeCode());
        productAttributeResponseMap.put(productAttributeResponse.getAttribute().getAttributeCode(),
            productAttributeResponse);
      }
    }
    for (String attributeCode : attributeCodes) {
      validateUpdateOnDSExtractedProductAttributeValue(productAttributeRequestMap,
          productAttributeResponseMap, attributeCode, productDetail.getProductCode());
    }
  }

  private void validateUpdateOnDSExtractedProductAttributeValue(
      Map<String, ProductAttributeRequest> productAttributeRequestMap,
      Map<String, ProductAttributeResponse> productAttributeResponseMap, String attributeCode,
      String productCode) {
    ProductAttributeRequest productAttributeRequest = productAttributeRequestMap.get(attributeCode);
    ProductAttributeResponse productAttributeResponse =
        productAttributeResponseMap.get(attributeCode);
    if (Objects.isNull(productAttributeRequest) || Objects.isNull(productAttributeResponse)) {
      //Dont publish history for category update.
      return;
    }
    if (productAttributeRequest.getAttribute().getAttributeType()
        .equals(AttributeType.PREDEFINED_ATTRIBUTE)) {
      validateUpdateOnDSExtractedPredefinedProductAttributeValue(productAttributeRequest,
          productAttributeResponse, productCode);
    } else if (productAttributeRequest.getAttribute().getAttributeType()
        .equals(AttributeType.DESCRIPTIVE_ATTRIBUTE)) {
      validateUpdateOnDSExtractedDescriptiveProductAttributeValue(productAttributeRequest,
          productAttributeResponse, productCode);
    } else if (productAttributeRequest.getAttribute().getAttributeType()
        .equals(AttributeType.PREDEFINED_MULTIVALUE)) {
      validateUpdateOnDSExtractedPredefinedMultiValueProductAttributeValue(productAttributeRequest,
          productAttributeResponse, productCode);
    } else if (AttributeType.DESCRIPTIVE_MULTIVALUE.equals(
        productAttributeRequest.getAttribute().getAttributeType())) {
      validateUpdateOnDSExtractedDescriptiveMultiValueProductAttributeValue(productAttributeRequest,
          productAttributeResponse, productCode);
    }
  }

  private void validateUpdateOnDSExtractedPredefinedProductAttributeValue(
      ProductAttributeRequest productAttributeRequest,
      ProductAttributeResponse productAttributeResponse, String productCode) {
    ProductAttributeValueRequest productAttributeValueRequest =
        productAttributeRequest.getProductAttributeValues().get(0);
    ProductAttributeValueResponse productAttributeValueResponse =
        productAttributeResponse.getProductAttributeValues().get(0);
    if (!Optional.ofNullable(productAttributeValueRequest.getPredefinedAllowedAttributeValue())
        .map(PredefinedAllowedAttributeValueRequest::getValue).orElse(StringUtils.EMPTY).equals(
            Optional.ofNullable(productAttributeValueResponse.getPredefinedAllowedAttributeValue())
                .map(PredefinedAllowedAttributeValueResponse::getValue)
                .orElse(StringUtils.EMPTY))) {
      publishFeedbackForUpdateOnDSExtractedPredefinedProductAttributeValue(
          productAttributeRequest.getProductAttributeValues(),
          productAttributeResponse.getProductAttributeValues(),
          productAttributeRequest.getAttribute().getAttributeCode(), productCode);
    }
  }

  private void validateUpdateOnDSExtractedDescriptiveProductAttributeValue(
      ProductAttributeRequest productAttributeRequest,
      ProductAttributeResponse productAttributeResponse, String productCode) {
    ProductAttributeValueRequest productAttributeValueRequest =
        productAttributeRequest.getProductAttributeValues().get(0);
    ProductAttributeValueResponse productAttributeValueResponse =
        productAttributeResponse.getProductAttributeValues().get(0);
    if (!Optional.ofNullable(productAttributeValueRequest.getDescriptiveAttributeValue())
        .orElse(StringUtils.EMPTY).equals(
            Optional.ofNullable(productAttributeValueResponse.getDescriptiveAttributeValue())
                .orElse(StringUtils.EMPTY))) {
      publishFeedbackForUpdateOnDSExtractedDescriptiveProductAttributeValue(
          productAttributeRequest.getProductAttributeValues(),
          productAttributeResponse.getProductAttributeValues(),
          productAttributeRequest.getAttribute().getAttributeCode(), productCode);
    }
  }

  private void validateUpdateOnDSExtractedPredefinedMultiValueProductAttributeValue(
      ProductAttributeRequest productAttributeRequest,
      ProductAttributeResponse productAttributeResponse, String productCode) {

    List<ProductAttributeValueRequest> productAttributeValueRequests =
        productAttributeRequest.getProductAttributeValues();
    List<ProductAttributeValueResponse> productAttributeValueResponses =
        productAttributeResponse.getProductAttributeValues();
    if (productAttributeValueRequests.size() != productAttributeValueResponses.size()) {
      publishFeedbackForUpdateOnDSExtractedPredefinedProductAttributeValue(
          productAttributeValueRequests, productAttributeValueResponses,
          productAttributeRequest.getAttribute().getAttributeCode(), productCode);
      return;
    }

    productAttributeValueRequests.sort(Comparator.comparing(
        productAttributeValueRequest -> productAttributeValueRequest.getPredefinedAllowedAttributeValue()
            .getId()));
    productAttributeValueResponses.sort(Comparator.comparing(
        productAttributeValueResponse -> productAttributeValueResponse.getPredefinedAllowedAttributeValue()
            .getId()));
    for (int i = 0; i < productAttributeValueRequests.size(); i++) {
      ProductAttributeValueRequest productAttributeValueRequest =
          productAttributeValueRequests.get(i);
      ProductAttributeValueResponse productAttributeValueResponse =
          productAttributeValueResponses.get(i);
      if (!Optional.ofNullable(productAttributeValueRequest.getPredefinedAllowedAttributeValue())
          .map(PredefinedAllowedAttributeValueRequest::getValue).orElse(StringUtils.EMPTY).equals(
              Optional.ofNullable(
                      productAttributeValueResponse.getPredefinedAllowedAttributeValue())
                  .map(PredefinedAllowedAttributeValueResponse::getValue)
                  .orElse(StringUtils.EMPTY))) {
        publishFeedbackForUpdateOnDSExtractedPredefinedProductAttributeValue(
            productAttributeValueRequests, productAttributeValueResponses,
            productAttributeRequest.getAttribute().getAttributeCode(), productCode);
        return;
      }
    }
  }

  private void validateUpdateOnDSExtractedDescriptiveMultiValueProductAttributeValue(
      ProductAttributeRequest productAttributeRequest,
      ProductAttributeResponse productAttributeResponse, String productCode) {
    List<ProductAttributeValueRequest> productAttributeValueRequests =
        productAttributeRequest.getProductAttributeValues();
    List<ProductAttributeValueResponse> productAttributeValueResponses =
        productAttributeResponse.getProductAttributeValues();
    if (productAttributeValueRequests.size() != productAttributeValueResponses.size()) {
      publishFeedbackForUpdateOnDSExtractedDescriptiveProductAttributeValue(
          productAttributeValueRequests, productAttributeValueResponses,
          productAttributeRequest.getAttribute().getAttributeCode(), productCode);
      return;
    }
    productAttributeValueRequests.sort(
        Comparator.comparing(ProductAttributeValueRequest::getDescriptiveAttributeValue));
    productAttributeValueResponses.sort(
        Comparator.comparing(ProductAttributeValueResponse::getDescriptiveAttributeValue));
    for (int i = 0; i < productAttributeValueRequests.size(); i++) {
      ProductAttributeValueRequest productAttributeValueRequest =
          productAttributeValueRequests.get(i);
      ProductAttributeValueResponse productAttributeValueResponse =
          productAttributeValueResponses.get(i);
      if (!Optional.ofNullable(productAttributeValueRequest.getDescriptiveAttributeValue())
          .orElse(StringUtils.EMPTY).equals(
              Optional.ofNullable(productAttributeValueResponse.getDescriptiveAttributeValue())
                  .orElse(StringUtils.EMPTY))) {
        publishFeedbackForUpdateOnDSExtractedDescriptiveProductAttributeValue(
            productAttributeValueRequests, productAttributeValueResponses,
            productAttributeRequest.getAttribute().getAttributeCode(), productCode);
        return;
      }
    }
  }

  private void publishFeedbackForUpdateOnDSExtractedPredefinedProductAttributeValue(
      List<ProductAttributeValueRequest> current, List<ProductAttributeValueResponse> previous,
      String attributeCode, String productCode) {
    ProductAttributeFeedbackEventModel productAttributeFeedbackEventModel =
        new ProductAttributeFeedbackEventModel();
    productAttributeFeedbackEventModel.setProductCode(productCode);
    productAttributeFeedbackEventModel.setAttributeCode(attributeCode);
    productAttributeFeedbackEventModel.setPreviousValue(previous.stream().map(
            productAttributeValueResponse -> Optional.ofNullable(
                    productAttributeValueResponse.getPredefinedAllowedAttributeValue())
                .map(PredefinedAllowedAttributeValueResponse::getValue).orElse(StringUtils.EMPTY))
        .collect(Collectors.toList()));
    productAttributeFeedbackEventModel.setCurrentValue(current.stream().map(
            productAttributeValueRequest -> Optional.ofNullable(
                    productAttributeValueRequest.getPredefinedAllowedAttributeValue())
                .map(PredefinedAllowedAttributeValueRequest::getValue).orElse(StringUtils.EMPTY))
        .collect(Collectors.toList()));
    kafkaPublisher.send(kafkaTopicProperties.getProductAttributeFeedbackEventName(), productCode,
        productAttributeFeedbackEventModel);
  }

  private void publishFeedbackForUpdateOnDSExtractedDescriptiveProductAttributeValue(
      List<ProductAttributeValueRequest> current, List<ProductAttributeValueResponse> previous,
      String attributeCode, String productCode) {
    ProductAttributeFeedbackEventModel productAttributeFeedbackEventModel =
        new ProductAttributeFeedbackEventModel();
    productAttributeFeedbackEventModel.setProductCode(productCode);
    productAttributeFeedbackEventModel.setAttributeCode(attributeCode);
    productAttributeFeedbackEventModel.setPreviousValue(
        previous.stream().map(ProductAttributeValueResponse::getDescriptiveAttributeValue)
            .collect(Collectors.toList()));
    productAttributeFeedbackEventModel.setCurrentValue(
        current.stream().map(ProductAttributeValueRequest::getDescriptiveAttributeValue)
            .collect(Collectors.toList()));
    kafkaPublisher.send(kafkaTopicProperties.getProductAttributeFeedbackEventName(), productCode,
        productAttributeFeedbackEventModel);
  }

  @Override
  public void updateProductAssignmentStatus(String productCode, String assignedTo, String assignedBy) {
    GdnBaseRestResponse response = pbpFeign.productAssignment(productCode, assignedTo, assignedBy);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void approveDraft(ProductRequest productRequest) throws Exception {
    updateProduct(productRequest, false);
    GdnBaseRestResponse response = pbpFeign.approveDraft(productRequest.getProductCode());
    ResponseHelper.validateResponse(response);
  }

  @Override
  public ProductDetailWebResponse getProductDetail(String productCode, boolean inAllProducts, String clientId,
      boolean concatenateValueWithValueType) {
    String businessPartnerCode = clientParameterHelper.getBusinessPartnerCode();
    log.info("Fetching product detail for {} & bp code {}", productCode, businessPartnerCode);
    GdnRestSingleResponse<ProductDetailCompleteResponse> response =
        pbpFeign.getProductDetail(productCode, inAllProducts, businessPartnerCode);
    ResponseHelper.validateResponse(response);
    validateForNeedRevision(response, clientId);
    ProfileResponse profileResponse =
        bpService.getProfileResponseByBusinessPartnerCode(response.getValue().getBusinessPartnerCode());
    ProductDetailWebResponse productDetailWebResponse;
    if (concatenateValueWithValueType && valueTypeAdditionForDefiningAttributes) {
      productDetailWebResponse =
          ResponseHelper.toProductDetailWebResponseFromProductDetailCompleteResponse(response.getValue(),
              profileResponse);
      productDetailWebResponse = ResponseHelper.setValueConcatenatedWithValueType(productDetailWebResponse, sizeChartValueTypeDelimiter);
    } else {
      productDetailWebResponse = ResponseHelper.toProductDetailWebResponseFromProductDetailCompleteResponse(response.getValue(),
          profileResponse);
    }

    if (instoreNewFlowEnabled && Optional.ofNullable(profileResponse)
      .map(ProfileResponse::getCompany).map(CompanyDTO::isOfflineToOnlineFlag).orElse(false)) {
      ProductL3BasicResponse productL3BasicResponse = getProductL3BasicDetails(productCode);
      if (Objects.isNull(productL3BasicResponse)) {
        productDetailWebResponse.setPureInstore(false);
      } else {
        productDetailWebResponse.setPureInstore(productL3BasicResponse.isPureInStoreProduct());
      }
    }
    productDetailWebResponse.setProductDetailPageLink(
        ResponseHelper.toProductDetailPage(productDetailWebResponse.getProductCode(), productDetailPageUrlPrefix));
    ResponseHelper.populateAiGeneratedFields(productDetailWebResponse, response);
    return productDetailWebResponse;
  }

  private void validateForNeedRevision(GdnRestSingleResponse<ProductDetailCompleteResponse> response, String clientId) {
    log.info("validateForNeedRevision isPostLive: {}, isEdited : {}, isRevised : {}, clientId : {}",
        response.getValue().isPostLive(), response.getValue().isEdited(), response.getValue().isRevised(), clientId);
    if (StringUtils.equalsIgnoreCase(clientId, Constants.MTA_APP) && response.getValue().isRevised() && (
        response.getValue().isPostLive() || response.getValue().isEdited())) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
          ErrorMessages.PRODUCT_NEED_REVISION_MOBILE_ERROR);
    }
  }

  @Override
  public FilterCountWebResponse getFilterCounts(boolean activated, boolean viewable) {
    GdnRestSingleResponse<FilterCountResponse> response = pbpFeign.getFilterCounts(activated, viewable);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toFilterCountWebResponse(response.getValue());
  }

  @Override
  public Page<ReviewProductWebResponse> getReviewProductsByFilterRequest(ReviewProductsFilterRequest request,
      boolean activated, boolean viewable, int page, int size) {
    SummaryFilterRequest filterRequest = new SummaryFilterRequest();
    BeanUtils.copyProperties(request, filterRequest);
    GdnRestListResponse<ReviewProductResponse> response = pbpFeign.getReviewProducts(filterRequest, activated, viewable,
        page, size);
    ResponseHelper.validateResponse(response);
    Map<String, ProfileResponse> profileResponseMap;
    List<String> businessPartnerCodes = response.getContent().stream()
        .filter(reviewProductResponse -> Objects.nonNull(reviewProductResponse.getBusinessPartnerCode()))
        .map(ReviewProductResponse::getBusinessPartnerCode).distinct().collect(Collectors.toList());
    profileResponseMap = bpService.getProfileResponseMap(businessPartnerCodes);
    List<ReviewProductWebResponse> responseList =
        ResponseHelper.toReviewProductWebResponseList(response.getContent(), profileResponseMap);
    return new PageImpl<>(responseList, PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void doSuspensionAction(SuspensionProductBulkActionsWebRequest request) {
    GdnBaseRestResponse response = pbpFeign.doSuspensionAction(RequestHelper.toSuspensionProductRequest(request));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public Page<ProductHistoryWebResponse> getProductHistory(String productId, int page, int size) {
    GdnRestListResponse<ProductHistoryResponse> response = pbpFeign.getProductHistory(productId, page, size);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(ResponseHelper.toProductHistoryWebResponseList(response.getContent()),
        PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void doScreeningProductsBulkActions(String actionType,
      ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest) {
    ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest =
        RequestHelper.toScreeningProductBulkActionsRequest(screeningProductBulkActionsWebRequest);
    screeningProductBulkActionsRequest.setScreeningAction(true);
    GdnBaseRestResponse response =
        pbpFeign.doScreeningProductsBulkActions(actionType, screeningProductBulkActionsRequest);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public List<ProductSuggestionWebResponse> getScreeningSuggestion(String productCode, String productName,
      String upcCode, String categoryId, List<ProductSuggestionWebRequest> productSuggestionWebRequests,
      Pageable pageable) {
    String finalCategoryId;
    List<ProductCodeResponse> response = getResponseFromPristine(categoryId, productCode, pageable);
    if (CollectionUtils.isEmpty(response)) {
      Map<String, String> categoryToFinalCategoryMap = categoryService.getCategoryToFinalParentMap();
      if (MapUtils.isNotEmpty(categoryToFinalCategoryMap) && categoryToFinalCategoryMap.containsKey(categoryId)) {
        finalCategoryId = categoryToFinalCategoryMap.get(categoryId);
      } else {
        finalCategoryId = categoryService.getFinalParentCategoryAndUpdateMap(categoryId);
      }
      response = findByNameOrUpcCode(productName, upcCode, finalCategoryId, productSuggestionWebRequests, pageable);
    }
    return ResponseHelper.toProductSuggestionWebResponseList(response);
  }

  /**
   * Get product suggestion from pristine
   *  @param categoryId
   * @param productCode
   * @param pageable
   */
  private List<ProductCodeResponse> getResponseFromPristine(String categoryId, String productCode, Pageable pageable) {
    List<ProductCodeResponse> responseList = new ArrayList<>();
    if (Boolean.valueOf(extCatalogProperties.getNeedPristineSuggestion())) {
      Map<String, Set<String>> supportedCategoryMap = productSuggestionService.getSupportedBlibliCategoriesByPristine();
      String supportedCategory =
          ResponseHelper.validateAndGetPristineSupportedCategory(categoryId, supportedCategoryMap);
      if (StringUtils.isNotEmpty(supportedCategory)) {
        log.info("invoking ext-catalog to get product suggestions for productCode: {}, category: {}", productCode,
            categoryId);
        responseList = productSuggestionService.getPCBProductCodes(productCode, supportedCategory, pageable);
      }
    }
    return responseList;
  }

  /**
   *
   * Find productSuggestion based on name or upcCode
   *
   * @param productName
   * @param upcCode
   * @param finalCategoryId
   * @param productSuggestionWebRequests
   * @param pageable
   * @return
   */
  private List<ProductCodeResponse> findByNameOrUpcCode(String productName, String upcCode, String finalCategoryId,
      List<ProductSuggestionWebRequest> productSuggestionWebRequests, Pageable pageable) {
    List<AttributeReqModel> attributeReqModelList = RequestHelper.toAttributeReqModelList(productSuggestionWebRequests);
    GdnRestListResponse<ProductCodeResponse> response = pbpFeign
        .findByNameOrUpcCode(productName, upcCode, finalCategoryId, attributeReqModelList, pageable.getPageNumber(),
            pageable.getPageSize());
    ResponseHelper.validateResponse(response);
    return response.getContent();
  }

  @Override
  public List<ProductSuggestionWebResponse> filterProductsBySearchKeyword(String keyword, int page, int size) {
    GdnRestListResponse<ProductCodeResponse> response = pbpFeign.filterProductsBySearchKeyword(keyword, page, size);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toProductSuggestionWebResponseList(response.getContent());
  }

  @Override
  public ProductReviewerWebResponse getProductReviewerList(String productCodeRedisKey, String userName) {
    ProductReviewerWebResponse productReviewerWebResponse =
        new ProductReviewerWebResponse(cacheProductService.getReviewerList(productCodeRedisKey));
    cacheProductService.addUserToProductReviewList(productCodeRedisKey, userName);
    if (!CollectionUtils.isEmpty(productReviewerWebResponse.getUsername())) {
      if (productReviewerWebResponse.getUsername().contains(userName)) {
        List<String> userList = new ArrayList<>(productReviewerWebResponse.getUsername());
        userList.remove(userName);
        productReviewerWebResponse.setUsername(userList);
      }
    }
    return productReviewerWebResponse;
  }

  @Override
  public void deleteProductReviewer(String productCodeRedisKey, String userName) {
    cacheProductService.removeCurrentUserFromProductView(productCodeRedisKey, userName);
  }

  @Override
  public List<ProductRevisionHistoryWebResponse> getProductRevisionHistory(String productCode) {
    GdnRestListResponse<ProductRevisionInfoResponse> response = pbpFeign.getProductRevisionHistory(productCode);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toProductRevisionHistoryWebResponseList(response.getContent());
  }

  @Override
  public CategoryChangeCheckResponse checkCategoryChange(String presentCategory, String targetCategory, String productCode,
      boolean isActive) {
    String error = null;
    GdnRestSingleResponse<CategoryDetailResponse> targetCategoryDetail = pcbFeign.getCategoryDetail(targetCategory);
    ResponseHelper.validateResponse(targetCategoryDetail);
    GdnRestSingleResponse<CategoryDetailResponse> presentCategoryDetail = pcbFeign.getCategoryDetail(presentCategory);
    ResponseHelper.validateResponse(presentCategoryDetail);
    error = compareDefiningAttributes(productCode, targetCategoryDetail.getValue(), presentCategoryDetail.getValue());
    if (StringUtils.isNotEmpty(error)) {
      log.error("Defining attribute mismatch for categories with id : {} and {}, for product : {} ", presentCategory,
          targetCategory, productCode);
      return new CategoryChangeCheckResponse(false, error);
    }
    if (!isActive) {
      if (targetCategoryDetail.getValue().isWholesalePriceConfigEnabled()) {
        error = this.pbpFeign
            .compareProductAndCategoryWholesale(productCode, targetCategoryDetail.getValue().getCategoryCode())
            .isSuccess() ? StringUtils.EMPTY : ErrorMessages.WHOLESALE_CONFIGURATIONS_MISMATCH;
        if (StringUtils.isNotEmpty(error)) {
          log.error("Wholesale rules not match for category change : {} and {}, for product : {} ", presentCategory,
              targetCategory, productCode);
          return new CategoryChangeCheckResponse(false, error);
        }
      }
      return new CategoryChangeCheckResponse(true, StringUtils.EMPTY);
    } else {
      if (targetCategoryDetail.getValue().isWholesalePriceConfigEnabled()) {
        error = compareCategoryWholesaleConfig(presentCategory, targetCategory);
        if (StringUtils.isNotEmpty(error)) {
          log.error("Wholesale configurations mismatch for categories with id {} and {}", presentCategory,
              targetCategory);
          return new CategoryChangeCheckResponse(true, error);
        }
      }
      return new CategoryChangeCheckResponse(true, StringUtils.EMPTY);
    }
  }

  private String compareDefiningAttributes(String productCode,
      CategoryDetailResponse targetCategoryDetail, CategoryDetailResponse presentCategoryDetail) {
    GdnRestSingleResponse<ProductDetailResponse> productDetailResponse =
        pcbFeign.filterProductDetailByProductCode(productCode);
    ResponseHelper.validateResponse(productDetailResponse);
    return ResponseHelper.categoryChangeCheck(presentCategoryDetail.getCategoryAttributes(),
        targetCategoryDetail.getCategoryAttributes(),
        productDetailResponse.getValue().getProductAttributeResponses());
  }

  private String compareCategoryWholesaleConfig(String presentCategory, String targetCategory) {
    GdnRestSingleResponse<WholesaleMappingResponse> presentWholesaleMappingResponse =
        pcbFeign.getWholesaleConfigToCategory(presentCategory);
    ResponseHelper.validateResponse(presentWholesaleMappingResponse);
    GdnRestSingleResponse<WholesaleMappingResponse> targetWholesaleMappingResponse =
        pcbFeign.getWholesaleConfigToCategory(targetCategory);
    ResponseHelper.validateResponse(targetWholesaleMappingResponse);
    return ResponseHelper.categoryWholesaleConfigCheck(presentWholesaleMappingResponse.getValue(),
        targetWholesaleMappingResponse.getValue()) ?
        StringUtils.EMPTY :
        ErrorMessages.WHOLESALE_CONFIGURATIONS_MISMATCH;
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  public void saveHistoryIfCategoryChanged(String productCode, String presentCategory, String requestCategory) {
    if (!presentCategory.equals(requestCategory)) {
      ProductHistoryRequest productHistoryRequest = new ProductHistoryRequest();
      productHistoryRequest.setProductCode(productCode);
      productHistoryRequest.setDescription(Constants.CATEGORY_CHANGED);
      productHistoryRequest.setNotes("Category changed from " + presentCategory + " to " + requestCategory);
      productHistoryRequest.setState(5);
      pbpFeign.submitHistory(productHistoryRequest);
    }
  }

  @Override
  public ProductDetailResponse findDetailByProductCodeAndReplaceCategoryInfo(String productCode, String categoryCode) throws Exception {
    GdnRestSingleResponse<ProductDetailResponse> response =
        this.pcbFeign.filterProductDetailByProductCodeWithOriginalImages(productCode, categoryCode, true);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public MasterAttributeResponse getAttributeInfoByAttributeCode(String attributeCode) throws Exception {
    GdnRestSingleResponse<MasterAttributeResponse> response = this.pcbFeign.getAttributeByAttributeCode(attributeCode);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public Page<ProductSuspensionWebResponse> getAllProducts(
      ProductSuspensionFilterRequest productSuspensionFilterRequest, Pageable pageable) throws Exception {
    GdnRestListResponse<SuspensionProductResponse> response = pbpFeign
        .getAllProducts(RequestHelper.toSummaryFilterRequest(productSuspensionFilterRequest), pageable.getPageNumber(),
            pageable.getPageSize());
    ResponseHelper.validateResponse(response);
    List<String> businessPartnerCodes = response.getContent().stream()
        .filter(suspensionProductResponse -> Objects.nonNull(suspensionProductResponse.getBusinessPartnerCode()))
        .map(SuspensionProductResponse::getBusinessPartnerCode).distinct().collect(Collectors.toList());
    Map<String, ProfileResponse> profileResponseMap = bpService.getProfileResponseMap(businessPartnerCodes);
    List<ProductSuspensionWebResponse> productSuspensionWebResponseList =
        ResponseHelper.toProductSuspensionWebResponseList(response.getContent(), profileResponseMap);
    if (CollectionUtils.isEmpty(productSuspensionWebResponseList)) {
      return new PageImpl<>(productSuspensionWebResponseList, pageable, response.getPageMetaData().getTotalRecords());
    }
    return new PageImpl<>(getBusinessPartnerAndCategoryDetails(productSuspensionWebResponseList, profileResponseMap), pageable,
        response.getPageMetaData().getTotalRecords());
  }

  private List<ProductSuspensionWebResponse> getBusinessPartnerAndCategoryDetails(
      List<ProductSuspensionWebResponse> productSuspensionWebResponseList, Map<String, ProfileResponse> profileResponseMap) {
    Map<String, String> categoryMap = new HashMap<>();
    Set<String> categoryCodes = productSuspensionWebResponseList.stream()
        .map(productSuspensionWebResponse -> productSuspensionWebResponse.getCategoryCode())
        .filter(data -> StringUtils.isNotBlank(data)).collect(Collectors.toSet());
    if (CollectionUtils.isNotEmpty(categoryCodes)) {
      CategoryMultipleIdRequest categoryMultipleIdRequest = new CategoryMultipleIdRequest();
      categoryMultipleIdRequest.setCategoryCode(new ArrayList<>(categoryCodes));
      GdnRestSingleResponse<CategoryNamesResponse> categoryNamesResponseMap =
          pcbFeign.getCategoryNames(categoryMultipleIdRequest, 0, categoryCodes.size());
      ResponseHelper.validateResponse(categoryNamesResponseMap);
      categoryMap = categoryNamesResponseMap.getValue().getCategoryMap();
    }
    List<ProductSuspensionWebResponse> result = new ArrayList<>();
    for (ProductSuspensionWebResponse productSuspensionWebResponse : productSuspensionWebResponseList) {
      if (profileResponseMap.containsKey(productSuspensionWebResponse.getBusinessPartnerCode())) {
        productSuspensionWebResponse.setBusinessPartnerName(
            profileResponseMap.get(productSuspensionWebResponse.getBusinessPartnerCode()).getCompany()
                .getBusinessPartnerName());
      }
      productSuspensionWebResponse.setCategoryName(categoryMap.get(productSuspensionWebResponse.getCategoryCode()));
      result.add(productSuspensionWebResponse);
    }
    return result;
  }

  @Override
  public Page<ProductSuspensionHistoryWebResponse> getSuspensionHistory(String productSku, int page, int size) {
    GdnRestListResponse<ProductSuspensionHistoryResponse> response =
        pbpFeign.getSuspensionHistory(productSku, page, size);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(ResponseHelper.toProductSuspensionHistoryWebResponseList(response.getContent()),
        PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void saveProductSuspensionFile(MultipartFile multipartFile, String type, String requestId, String storeId, String userName)
      throws Exception {
    String baseDirPath = fileStorageService.uploadFilePath(multipartFile, requestId,
        BulkInternalProcessType.BULK_INTERNAL_SUSPENSION_TYPE.getValue());
    BulkProductSuspensionRequest bulkProductSuspensionRequest = RequestHelper
        .toBulkProductSuspensionRequest(storeId,
            new StringBuilder(baseDirPath).append(multipartFile.getOriginalFilename()).toString(), type,
            requestId, userName);
    kafkaPublisher.send(DomainEventName.BULK_PRODUCT_SUSPENSION, userName,bulkProductSuspensionRequest);
  }

  @Override
  public boolean updateProductCategory(String productCode, String categoryCode) {
    GdnBaseRestResponse response = pbpFeign.updateProductCategory(productCode, categoryCode);
    ResponseHelper.validateResponse(response);
    return response.isSuccess();
  }

  @Override
  public Page<ProductCollectionWebResponse> findProductCollectionSummaryByKeyword(
      SummaryFilterWebRequest summaryFilterWebRequest, Pageable pageable) throws Exception {
    GdnRestListResponse<ProductCollectionResponse> response = null;
    if (StringUtils.isNotBlank(summaryFilterWebRequest.getTimeFilterType())) {
      String timeFilterType = summaryFilterWebRequest.getTimeFilterType();
      if (TimeFilterWebType.TODAY.getTimeFilterType().equalsIgnoreCase(timeFilterType) || TimeFilterWebType.YESTERDAY
          .getTimeFilterType().equalsIgnoreCase(timeFilterType) || TimeFilterWebType.TWO_DAYS_AGO.getTimeFilterType()
          .equalsIgnoreCase(timeFilterType) || TimeFilterWebType.THREE_TO_FIVE_DAYS_AGO.getTimeFilterType()
          .equalsIgnoreCase(timeFilterType)) {
        response = this.pbpFeign
            .filterProductCollectionSummaryByKeywordAndAgeBetween(pageable.getPageNumber(), pageable.getPageSize(),
                summaryFilterWebRequest.getBusinessPartnerCode(), summaryFilterWebRequest.getCategoryCode(),
                summaryFilterWebRequest.getKeyword(), null, null, timeFilterType, summaryFilterWebRequest.isActivated(),
                summaryFilterWebRequest.isViewable());
      } else if (TimeFilterWebType.FIVE_DAYS_AGO.getTimeFilterType().equalsIgnoreCase(timeFilterType)) {
        response = this.pbpFeign
            .filterProductCollectionSummaryByKeywordAndAgeLessThan(pageable.getPageNumber(), pageable.getPageSize(),
                summaryFilterWebRequest.getBusinessPartnerCode(), summaryFilterWebRequest.getCategoryCode(),
                summaryFilterWebRequest.getKeyword(), null, timeFilterType, summaryFilterWebRequest.isActivated(),
                summaryFilterWebRequest.isViewable());
      } else {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_TIME_FILTER_TYPE);
      }
    } else {
      response = this.pbpFeign.filterProductCollectionSummaryByKeyword(summaryFilterWebRequest.getBusinessPartnerCode(),
          summaryFilterWebRequest.getCategoryCode(), summaryFilterWebRequest.getKeyword(),
          summaryFilterWebRequest.getReviewPending(), summaryFilterWebRequest.isActivated(),
          summaryFilterWebRequest.isViewable(), summaryFilterWebRequest.getSortBy(), pageable.getPageNumber(),
          pageable.getPageSize());
    }
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(ResponseHelper.toProductCollectionWebResponseList(response.getContent()), pageable,
        response.getPageMetaData().getTotalRecords());
  }

  public void bulkUpdateMasterProductData(MultipartFile multipartFile, String requestId, String storeId,
      String userName) throws Exception {
    String bulkUploadCode = UUID.randomUUID().toString();
    GdnRestSingleResponse<SequenceResponse> sequenceResponse =
        this.pbpFeign.findCounterByKey(Constants.BULK_INTERNAL_PROCESS_CODE_KEY);
    ResponseHelper.validateResponse(sequenceResponse);
    String bipRequestCode =
        new StringBuilder().append(Constants.BULK_INTERNAL_PROCESS_CODE_KEY).append(Constants.HYPHEN)
            .append(StringUtils
                .leftPad(String.valueOf(sequenceResponse.getValue().getCounter()), Constants.PADDING_COUNT, Constants.PADDING_CONSTANT))
            .toString();
    String baseDirPath = fileStorageService.uploadFilePath(multipartFile, requestId,
        BulkInternalProcessType.BULK_INTERNAL_PROCESS_TYPE.getValue());
    MasterDataBulkUpdateRequest masterDataBulkUpdateRequest = RequestHelper
        .toMasterDataBulkUpdateRequest(requestId, storeId,
            new StringBuilder(baseDirPath).append(multipartFile.getOriginalFilename()).toString(), bulkUploadCode,
            userName, clientHost, bipRequestCode);
    kafkaPublisher.send(DomainEventName.INTERNAL_USER_BULK_UPLOAD_EVENT, userName,masterDataBulkUpdateRequest);
  }

  @Override
  public MapWebResponse getFilterCountsBySource(CountWebRequest countWebRequest) throws Exception {
    if (Source.IN_PROGRESS.name().equals(countWebRequest.getSource())) {
      GdnRestSingleResponse<ProductCollectionCountRestResponse> response = pbpFeign
          .countProductCollectionBySpecifiedDateRange(countWebRequest.getBusinessPartnerCode(),
              countWebRequest.getCategoryCode(), countWebRequest.getKeyword(), true, false);
      ResponseHelper.validateResponse(response);
      return ResponseHelper.toMapWebResponse(response.getValue());
    } else if (Source.FINAL_QC.name().equals(countWebRequest.getSource())) {
      DistributionTaskMultipleFilterRequest distributionTaskMultipleFilterRequest =
          RequestHelper.toDistributionTaskMultipleFilterRequest(countWebRequest);
      GdnRestSingleResponse<MapResponse> response =
          pdtFeign.countDistributionSummaryByFilter(false, false, distributionTaskMultipleFilterRequest);
      ResponseHelper.validateResponse(response);
      return ResponseHelper.toMapWebResponse(response.getValue());
    } else if (Source.DISTRIBUTION_LIST.name().equals(countWebRequest.getSource())) {
      GdnRestSingleResponse<MapResponse> response =
          pdtFeign.countDistributionSummaryByFilter(true, false, new DistributionTaskMultipleFilterRequest());
      ResponseHelper.validateResponse(response);
      return ResponseHelper.toMapWebResponse(response.getValue());
    } else {
      throw new ApplicationException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_SOURCE);
    }
  }

  @Override
  public void downloadBulkSelectedMasterProductsForInternal(String username,
      SelectedMasterProductDownloadWebRequest request) {
    String requestId = UUID.randomUUID().toString();
    log.info("invoking of selected master products download. requestId: {},", requestId);
    String fileName =
        new StringBuilder().append(requestId).append(Constants.DOT).append(FileType.XLSX.name().toLowerCase())
            .toString();
    SelectedMasterProductDownloadRequest selectedMasterProductDownloadRequest =
        SelectedMasterProductDownloadRequest.SelectedMasterProductDownloadRequestBuilder()
            .productCodes(request.getProductCodes()).downloadType(DownloadType.ALL).fileType(FileType.XLSX)
            .bulkProcessEntity(BulkProcessEntity.SELECTED_MASTER_PRODUCTS).directDownload(false).filename(fileName)
            .emailTo(username).username(username).language(Constants.LANGUAGE).requestId(requestId).build();
    this.kafkaPublisher.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,selectedMasterProductDownloadRequest);
  }

  @Override
  public void downloadBulKMasterProducts(String username, SummaryFilterWebRequest summaryFilterWebRequest) {
    String requestId = UUID.randomUUID().toString();
    log.info("Invoking all product download at controller. requestId: {}", requestId);
    String fileName =
        new StringBuilder().append(requestId).append(Constants.DOT).append(FileType.XLSX.name().toLowerCase())
            .toString();
    MasterProductDownloadRequest masterProductDownloadRequest =
        MasterProductDownloadRequest.MasterProductDownloadBuilder().filterName(summaryFilterWebRequest.getKeyword())
            .categoryCode(summaryFilterWebRequest.getCategoryCode())
            .reviewPending(summaryFilterWebRequest.getReviewPending()).sortBy(summaryFilterWebRequest.getSortBy())
            .downloadType(DownloadType.ALL).fileType(FileType.XLSX).bulkProcessEntity(BulkProcessEntity.MASTER_PRODUCT)
            .directDownload(false).filename(fileName).emailTo(username).emailCc(username).username(username)
            .language(Constants.LANGUAGE).requestId(requestId).build();
    this.kafkaPublisher.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,masterProductDownloadRequest);
  }

  @Override
  public Page<HistoryWebResponse> findProductHistory(String source, String productCode, String productId, int page,
      int size) {
    if (Source.ACTIVE.name().equals(source) || Source.IN_PROGRESS.name().equals(source)) {
      GdnRestListResponse<ProductHistoryResponse> response = pbpFeign.getProductHistorySummary(page, size, productId);
      ResponseHelper.validateResponse(response);
      List<HistoryWebResponse> responseList =
          ResponseHelper.toHistoryWebResponseListFromProductHistoryResponse(response.getContent());
      return new PageImpl<>(responseList, PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());

    } else if (Source.FINAL_QC.name().equals(source) || Source.DISTRIBUTION_LIST.name().equals(source)) {
      GdnRestListResponse<TaskHistoryResponse> response =
          this.pdtFeign.getProductHistories(page, size, productCode, true);
      ResponseHelper.validateResponse(response);
      List<HistoryWebResponse> responseList =
          ResponseHelper.toHistoryWebResponseListFromTaskHistoryResponse(response.getContent());
      return new PageImpl<>(responseList, PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_SOURCE);
    }
  }

  @Override
  public boolean retryProductPublishToPDT(String productCode) {
    com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse<Boolean> response = pdtFeign.isProductExists(productCode);
    ResponseHelper.validateResponse(response);
    if (response.getValue()) {
      return false;
    }
    GdnBaseRestResponse baseRestResponse = pbpFeign.publishProductToPDT(productCode);
    ResponseHelper.validateResponse(baseRestResponse);
    return true;
  }

  @Override
  public void reindexByProductSku(String storeId, String productSku) {
    SimpleListStringRequest request = new SimpleListStringRequest();
    request.setValue(Arrays.asList(productSku));
    GdnBaseRestResponse response =
        xProductFeign.reindexByProductSkus(storeId, Constants.REQUEST_ID, request);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public boolean retryProductNeedRevisionToPBP(String storeId, String productCode) {
    kafkaPublisher.send(com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME,
        productCode, new PDTNeedRevisionEventModel(storeId, productCode));
    return true;
  }

  @Override
  public CategoryChangeWebResponse getProductCategoryChangeCheckResponse(String productCode, String presentCategoryId,
      String newCategoryId, boolean isActive, String productType) throws Exception {
    CategoryChangeWebResponse categoryChangeWebResponse = new CategoryChangeWebResponse();
    GdnRestSingleResponse<CategoryDetailResponse> presentCategoryDetail =
        pcbFeign.getCategoryDetail(presentCategoryId);
    ResponseHelper.validateResponse(presentCategoryDetail);
    GdnRestSingleResponse<CategoryDetailResponse> newCategoryDetail =
        pcbFeign.getCategoryDetail(newCategoryId);
    ResponseHelper.validateResponse(newCategoryDetail);
    categoryChangeWebResponse.setDefiningAttributesMismatch(StringUtils.isNotEmpty(
        compareDefiningAttributes(productCode, newCategoryDetail.getValue(),
            presentCategoryDetail.getValue())));
    if (isActive) {
      categoryChangeWebResponse.setWholesaleRulesMismatch(
          StringUtils.isNotEmpty(compareCategoryWholesaleConfig(presentCategoryId, newCategoryId)));
    } else {
      if (newCategoryDetail.getValue().isWholesalePriceConfigEnabled()) {
        GdnBaseRestResponse gdnBaseRestResponse = this.pbpFeign
            .compareProductAndCategoryWholesale(productCode, newCategoryDetail.getValue().getCategoryCode());
        if (Objects.nonNull(gdnBaseRestResponse)) {
          if (!gdnBaseRestResponse.isSuccess()) {
            categoryChangeWebResponse.setWholesaleRulesMismatch(true);
          }
        }
      } else {
        categoryChangeWebResponse.setWholesaleRulesMismatch(false);
      }
    }
    categoryChangeWebResponse.setCategoryMarginMismatch(
        compareCategoryMargin(presentCategoryDetail.getValue(), newCategoryDetail.getValue()));
    boolean isProductTypeBopis = String.valueOf(ProductType.BOPIS.getCode()).equals(productType);
    if (checkProductTypeSwitch) {
      if (presentCategoryDetail.getValue().isBopisEligible() && !newCategoryDetail.getValue()
          .isBopisEligible() && isProductTypeBopis) {
        categoryChangeWebResponse.setBopisEligibilityChange(true);
      }
    } else {
      if (presentCategoryDetail.getValue().isBopisEligible() && !newCategoryDetail.getValue()
          .isBopisEligible()) {
        categoryChangeWebResponse.setBopisEligibilityChange(true);
      }
    }
    return categoryChangeWebResponse;
  }

  private boolean compareCategoryMargin(CategoryDetailResponse presentCategory,
      CategoryDetailResponse newCategory) throws Exception {
    if (presentCategory.isB2bExclusive() || newCategory.isB2bExclusive()) {
      return !presentCategory.isB2bExclusive() || !newCategory.isB2bExclusive();
    } else {
      return compareCategoryMargin(presentCategory.getCategoryCode(), newCategory.getCategoryCode());
    }
  }

  private boolean compareCategoryMargin(String presentCategoryCode, String newCategoryCode) throws Exception {
    if(!marginNewChangesEnabled) {
      GdnRestSingleResponse<MarginCategoryResponse> presentCategoryMarginResponse =
          marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(presentCategoryCode, simpleDateFormat.format(new Date()));
      ResponseHelper.validateResponse(presentCategoryMarginResponse);
      GdnRestSingleResponse<MarginCategoryResponse> newCategoryMarginResponse =
          marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(newCategoryCode, simpleDateFormat.format(new Date()));
      ResponseHelper.validateResponse(newCategoryMarginResponse);
      return presentCategoryMarginResponse.getValue().getValue() < newCategoryMarginResponse.getValue().getValue();
    } else {
      String businessPartnerCode = INTERNAL;
      FilterMarginsByOrderItemsRequest filterMarginsByOrderItemsRequestForExisting =
          RequestHelper.getMarginsByOrderItemRequest(businessPartnerCode, presentCategoryCode,
              setDefaultOrderTypeForMargin, defaultOrderTypeForMargin);
      FilterMarginsByOrderItemsRequest filterMarginsByOrderItemsRequestForNew =
          RequestHelper.getMarginsByOrderItemRequest(businessPartnerCode, newCategoryCode, setDefaultOrderTypeForMargin,
              defaultOrderTypeForMargin);
      Optional<OrderItemMarginsResponse> orderItemMarginsResponseForNew =
          getOrderItemMarginsResponse(filterMarginsByOrderItemsRequestForNew);
      Optional<OrderItemMarginsResponse> orderItemMarginsResponseForExisting =
          getOrderItemMarginsResponse(filterMarginsByOrderItemsRequestForExisting);

      return ResponseHelper.checkMismatchForCategory(orderItemMarginsResponseForNew, orderItemMarginsResponseForExisting);
    }
  }

  private Optional<OrderItemMarginsResponse> getOrderItemMarginsResponse(
      FilterMarginsByOrderItemsRequest filterMarginsByOrderItemsRequest) throws Exception {
    ListBaseResponse<OrderItemMarginsResponse> orderItemMarginsResponseListBaseResponse =
        marginFeign.filterMargin(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            filterMarginsByOrderItemsRequest);
    ResponseHelper.validateResponse(orderItemMarginsResponseListBaseResponse);
    return orderItemMarginsResponseListBaseResponse.getContent().stream().filter(
            orderItemMarginsResponse1 -> Optional.ofNullable(orderItemMarginsResponse1.getMargins())
                .orElse(new ArrayList<>()).stream().anyMatch(margin -> Objects.nonNull(margin) && (
                    (SPECIAL_MARGIN.equals(margin.getMarginType()) && (BASE.equals(margin.getReplacementType())
                        || BASE_ADDON.equals(margin.getReplacementType()))) || BASE_MARGIN.equals(margin.getMarginType()))))
        .findFirst();
  }

  @Override
  public void reindexByProductCode(String productCode) {
    GdnBaseRestResponse response = pbpFeign.reindexByProductCode(productCode);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public TemplateDownloadFilePathWebResponse getInternalDownloadTemplateFilePaths() {
    TemplateDownloadFilePathWebResponse templateDownloadFilePathWebResponse = new TemplateDownloadFilePathWebResponse();
    templateDownloadFilePathWebResponse.setInternalTemplateDownloadFilePathMap(
        fileStorageService.getInternalDownloadTemplateFilePaths());
    return templateDownloadFilePathWebResponse;
  }

  @Override
  public HalalProductWebResponse getHalalProductDetailsByProductSku(String productSku) {
    GdnRestListResponse<com.gdn.x.product.rest.web.model.response.HalalProductResponse> halalProductResponseList =
        xProductFeign.getProductDetailsByProductSkuList(Collections.singletonList(productSku));
    ResponseHelper.validateResponse(halalProductResponseList);
    HalalProductWebResponse halalProductWebResponse = new HalalProductWebResponse();
    ResponseHelper.toSetHalalResponse(halalProductResponseList.getContent(), halalProductWebResponse, productSku,
        halalProductLinkPrefix);
    if (StringUtils.isNotEmpty(halalProductWebResponse.getProductCode())) {
      GdnRestSingleResponse<SingleObjectResponse> certificateNumberResponse =
          pcbFeign.getAttributeValuesByProductCodeAndAttributeCode(halalProductWebResponse.getProductCode(),
              attributeCode);
      try {
        ResponseHelper.validateResponse(certificateNumberResponse);
        halalProductWebResponse.setCertificateNumber(certificateNumberResponse.getValue().getValue().toString());
      } catch (Exception e) {
        log.error("Error while invoking pcb feign call for product code: {} Exception : ",
            halalProductWebResponse.getProductCode(), e);
        halalProductWebResponse.setCertificateNumber(StringUtils.EMPTY);
      }
    }
    return halalProductWebResponse;
  }

  @Override
  public Page<HalalCertificationWebDetailsResponse> getHalalCertificationDetails(String certificationNumber, int page,
      int size) {
    BPJPHListResponse<HalalCertificationDetailResponse> response = new BPJPHListResponse<>();
    if (halalThirdPartyApiSwitch) {
      response = bpjphFeign.getHalalCertificationDetails(halalApiKey ,page, size, certificationNumber);
    } else {
      return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
    }
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(ResponseHelper.toHalalCertificationDetailResponse(response.getData()),
        PageRequest.of(page, size), response.getData().getTotal_items());
  }

  @Override
  public Page<HalalProductHistoryWebResponse> getHalaProductHistory(String productSku, int page, int size) {
    GdnRestListResponse<HalalProductHistoryResponse> responseGdnRestListResponse =
        pbpFeign.getHalalProductHistory(productSku, page, size);
    ResponseHelper.validateResponse(responseGdnRestListResponse);
    List<HalalProductHistoryWebResponse> halalProductHistoryWebResponses =
        responseGdnRestListResponse.getContent().stream().map(ResponseHelper::getHalalProductHistoryWebResponse)
            .collect(Collectors.toList());
    return new PageImpl<>(halalProductHistoryWebResponses, PageRequest.of(page, size),
        responseGdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<HalalDashboardProductsWebResponse> getHalalDashboardProductsResponses(int page, int size,
      HalalProductsFilterWebRequest halalProductsFilterWebRequest) {
    GdnRestListResponse<HalalDashboardProductsResponse> halalDashboardProductsResponse =
        xProductFeign.getHalalDashboardProducts(page, size,
            RequestHelper.toHalalProductsFilterRequest(halalProductsFilterWebRequest));
    ResponseHelper.validateResponse(halalDashboardProductsResponse);
    List<HalalDashboardProductsWebResponse> halalDashboardProductsWebResponses =
        halalDashboardProductsResponse.getContent().stream()
            .map(response -> ResponseHelper.getHalaDashboardProductsWebResponse(response, halalProductLinkPrefix))
            .collect(Collectors.toList());
    return new PageImpl<>(halalDashboardProductsWebResponses, PageRequest.of(page, size),
        halalDashboardProductsResponse.getPageMetaData().getTotalRecords());
  }

  @Override
  public void updateHalalConfigOfProduct(String storeId, String requestId, String username, String productSku,
      String curationStatus) {
    GdnBaseRestResponse response =
        xProductFeign.updateHalalConfigOfProduct(storeId, requestId, username, productSku, curationStatus);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public List<ProductBasicResponse> getProductBasicDetails(List<String> productSkus) {
    SimpleListStringRequest request = new SimpleListStringRequest();
    request.setValue(productSkus);
    GdnRestListResponse<ProductBasicResponse> response = xProductFeign.getProductBasicDetails(request);
    ResponseHelper.validateResponse(response);
    return response.getContent();
  }

  @Override
  public ProductL3BasicResponse getProductL3BasicDetails(String productCode) {
    GdnRestSingleResponse<ProductL3BasicResponse> l3BasicResponse =
        pbpFeign.getProductLevel3BasicDetails(productCode);
    if (Objects.isNull(l3BasicResponse) || !l3BasicResponse.isSuccess() || Objects.isNull(
        l3BasicResponse.getValue())) {
      return null;
    }
    return l3BasicResponse.getValue();
  }
}
