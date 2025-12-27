package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.apache.commons.lang3.tuple.Pair;

import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;

import com.gdn.mta.product.service.solr.SolrActiveProductCollectionService;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ItemNeedRevisionNotes;
import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.partners.pbp.distributiontask.PredefinedAttributeAllowedValueService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Service;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTItemNotesDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductItemDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductNotesDomainEventModel;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.productcategorybase.domain.event.model.PredefinedAllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@Transactional
public class NeedCorrectionServiceBean implements NeedCorrectionService {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private PredefinedAttributeAllowedValueService predefinedAttributeAllowedValueService;

  @Autowired
  private SolrActiveProductCollectionService solrActiveProductCollectionService;

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Autowired
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Autowired
  private ProductLevel3WipService productLevel3WipService;

  @Autowired
  private ProductLevel1WipService productLevel1WipService;

  @Autowired
  private ProductMailEventService productMailEventService;

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductAppealService productAppealService;

  @Override
  public void sendForNeedCorrectionByProductCode(String productCode, boolean autoNeedRevision,
      AutoNeedRevisionRequest autoNeedRevisionRequest, boolean overrideDataFromPDT)
      throws Exception {
    PDTProductDomainEventModel pdtDomainModelResponseByCode = this.productDistributionTaskRepository
        .getPDTDomainModelResponseByCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, productCode);
    String username = (autoNeedRevision)
        ? Constants.SYSTEM + Constants.HYPHEN + Constants.AUTO_NEED_REVISION
        : (StringUtils.isNotBlank(pdtDomainModelResponseByCode.getUpdatedBy())
        ? pdtDomainModelResponseByCode.getUpdatedBy()
        : Constants.DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, username + UUID.randomUUID());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    ProductCollection productCollection = this.productLevel1CollectionService
        .findByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(), productCode);
    List<ProductBusinessPartner> productBusinessPartnerList = this.productBusinessPartnerService
        .findByStoreIdAndProductId(GdnMandatoryRequestParameterUtil.getStoreId(), productCollection.getProductId());
    ProductDetailResponse productDetailResponse =
        productOutbound.getProductDetailByProductCode(productCode, false, false);
    Map<String, String> itemCodeIdMap = new HashMap<>();
    if (ProductCreationType.MIGRATION.getProductCreationType().equals(productCollection.getProductCreationType())) {
      itemCodeIdMap.putAll(productDetailResponse.getProductItemResponses().stream()
          .collect(Collectors.toMap(ProductItemResponse::getSkuCode, ProductItemResponse::getId)));
    }
    if (pdtDomainModelResponseByCode.isPostLive() || pdtDomainModelResponseByCode.isEdited()) {
      takeDownNeedForCorrectionProduct(productCode, itemCodeIdMap, productDetailResponse);
    }
    setVendorAutoNeedRevisionNotes(pdtDomainModelResponseByCode, autoNeedRevision, autoNeedRevisionRequest);
    updateStateAndFlagsForL3Data(pdtDomainModelResponseByCode, productBusinessPartnerList, overrideDataFromPDT);
    saveVendorHistory(pdtDomainModelResponseByCode, username);
    if (overrideDataFromPDT) {
      updatePCBData(pdtDomainModelResponseByCode, productDetailResponse);
    }
    updatePBPData(pdtDomainModelResponseByCode, productCollection, productDetailResponse, productBusinessPartnerList,
        autoNeedRevision, overrideDataFromPDT);
  }

  @Override
  public void takeDownNeedForCorrectionProduct(String productCode, Map<String, String> itemCodeIdMap,
      ProductDetailResponse productDetailResponse)
      throws Exception {
    productLevel3Service.takeDownNeedForCorrectionProduct(productCode, itemCodeIdMap, productDetailResponse);
  }

  private void setVendorAutoNeedRevisionNotes(PDTProductDomainEventModel pdtProductDomainEventModel,
      boolean autoNeedRevision, AutoNeedRevisionRequest autoNeedRevisionRequest) {
    if (autoNeedRevision) {
      PDTProductNotesDomainEventModel pdtProductNotesDomainEventModel = new PDTProductNotesDomainEventModel();
      pdtProductNotesDomainEventModel.setAllVariants(true);
      pdtProductNotesDomainEventModel.setImagesAdditionalNotes(autoNeedRevisionRequest.getReason());
      pdtProductNotesDomainEventModel.setContentAdditionalNotes(autoNeedRevisionRequest.getContentReason());
      if(Objects.nonNull(autoNeedRevisionRequest.getImageReason())){
        pdtProductNotesDomainEventModel.setImageReason(Collections.singletonList(autoNeedRevisionRequest.getImageReason()));
      }
      pdtProductNotesDomainEventModel.setVendorErrorFields(autoNeedRevisionRequest.getVendorErrorFields());
      pdtProductDomainEventModel.setProductNotes(pdtProductNotesDomainEventModel);
    }
  }

  private void updateStateAndFlagsForL3Data(PDTProductDomainEventModel pdtDomainModelResponseByCode,
      List<ProductBusinessPartner> productBusinessPartnerList, boolean overrideDataFromPDT) throws Exception {
    for (ProductBusinessPartner productBusinessPartner : productBusinessPartnerList) {
      ConverterUtil
          .refreshProductAndItemDetails(pdtDomainModelResponseByCode, productBusinessPartner, overrideDataFromPDT);
      log.info("Updating PBP db with L3 data for : {}", productBusinessPartner);
      productBusinessPartnerService.update(productBusinessPartner);
    }
  }

  private void updatePCBData(PDTProductDomainEventModel pdtDomainModelResponseByCode,
      ProductDetailResponse productDetailResponse) throws Exception {
    ProductRequest productRequest = convertPDTDomainModelToProductRequest(pdtDomainModelResponseByCode,
        productDetailResponse);
    productRequest.setSpecificationDetail(CommonUtils.generateSpecificationDetail(productRequest));
    this.productOutbound.updateAndMarkProductForNeedCorrection(productRequest);
  }

  private void updatePBPData(PDTProductDomainEventModel pdtDomainModelResponseByCode,
      ProductCollection productCollection, ProductDetailResponse productDetailResponse,
      List<ProductBusinessPartner> productBusinessPartner, boolean autoNeedRevision, boolean overrideDataFromPDT) throws Exception {
    ConverterUtil
        .setProductCollectionDetailsForNeedRevision(pdtDomainModelResponseByCode, productCollection, autoNeedRevision,
            overrideDataFromPDT);
    List<PDTItemNotesDomainEventModel> pdtItemNotesDomainEventModelList =
      pdtDomainModelResponseByCode.getProductItems().stream()
        .filter(pdtProductItemDomainEventModel -> Objects.nonNull(pdtProductItemDomainEventModel.getItemNotes()))
        .map(PDTProductItemDomainEventModel::getItemNotes).collect(Collectors.toList());
    productCollection.setNeedCorrectionNotes(
      setVendorNotes(pdtDomainModelResponseByCode.getProductNotes(), pdtItemNotesDomainEventModelList, productDetailResponse,
        productBusinessPartner));
    for (ProductBusinessPartner businessPartner : productBusinessPartner) {
      resetAppealedFlagAndDecreaseCount(productCollection.getStoreId(),
          productCollection.getBusinessPartnerCode(), businessPartner,
          productBusinessPartnerService, productAppealService);
    }
    productLevel1CollectionService.updateAndSendForCorrection(productCollection);
    solrActiveProductCollectionService.deleteSolrProductCollectionDocument(productCollection.getId());
    solrReviewProductCollectionService.deleteProductFromReviewProductCollection(productCollection.getId());
  }

  public static void resetAppealedFlagAndDecreaseCount(String storeId, String businessPartnerCode,
      ProductBusinessPartner businessPartner,
      ProductBusinessPartnerService productBusinessPartnerService,
      ProductAppealService productAppealService) throws Exception {
    if (businessPartner.isAppealedProduct()) {
      businessPartner.setAppealedProduct(false);
      productBusinessPartnerService.saveProductBusinessPartner(businessPartner);
      Pair<Integer, ProductBusinessPartnerCounter> pair =
          productAppealService.fetchThresholdAndCounterForAppealProduct(
              storeId, businessPartnerCode);
      ProductBusinessPartnerCounter businessPartnerCounter = pair.getRight();
      productAppealService.decrementCounterForProductAppeal(GdnMandatoryRequestParameterUtil.getStoreId(),
          businessPartnerCounter.getBusinessPartnerCode());
    }
  }


  private String setVendorNotes(PDTProductNotesDomainEventModel pdtProductNotesDomainEventModel,
      List<PDTItemNotesDomainEventModel> pdtItemNotesDomainEventModelList,
      ProductDetailResponse productDetailResponse, List<ProductBusinessPartner> productBusinessPartner) throws Exception {
    NeedRevisionNotes needRevisionNotes = ConverterUtil.getNeedRevisionNotes(pdtProductNotesDomainEventModel,
      pdtItemNotesDomainEventModelList, productDetailResponse, productBusinessPartner);
    return objectMapper.writeValueAsString(needRevisionNotes);
  }

  public ProductRequest convertPDTDomainModelToProductRequest(PDTProductDomainEventModel pdtProductDomainEventModel,
      ProductDetailResponse productDetailResponse) throws Exception {
    ProductRequest productRequest = ConverterUtil.setProductBasicDetails(pdtProductDomainEventModel, productDetailResponse);

    if (pdtProductDomainEventModel.getProductCategories().get(0).getCategory().getCategoryCode().equals(
        productDetailResponse.getProductCategoryResponses().stream()
            .filter(productCategoryResponse -> !productCategoryResponse.isMarkForDelete()).findFirst().get()
            .getCategory().getCategoryCode())) {
      ConverterUtil.setProductRequestCategories(productDetailResponse, productRequest);
      setProductRequestAttributes(pdtProductDomainEventModel, productDetailResponse, productRequest);
    } else {
      Map<String, CategoryDetailResponse> categoryCodeToCategoryDetailMap =
        pdtProductDomainEventModel.getProductCategories().stream().map(
            productCategoryDomainEventModel -> productOutbound.getCategoryDetailByCategoryCode(
              productCategoryDomainEventModel.getCategory().getCategoryCode())).collect(Collectors.toList()).stream()
          .collect(Collectors.toMap(CategoryResponse::getCategoryCode, Function.identity()));
      ConverterUtil.setProductRequestCategoriesOnCategoryChange(productRequest, pdtProductDomainEventModel,
        categoryCodeToCategoryDetailMap);
      setProductRequestAttributesOnCategoryChange(pdtProductDomainEventModel, productDetailResponse, productRequest);
    }
    ConverterUtil.setProductRequestImages(pdtProductDomainEventModel, productRequest);
    setProductRequestItems(pdtProductDomainEventModel, productDetailResponse, productRequest);
    return productRequest;
  }

  private void setProductRequestItems(PDTProductDomainEventModel pdtProductDomainEventModel,
      ProductDetailResponse productDetailResponse, ProductRequest productRequest) {
    List<ProductItemRequest> productItemRequestList = new ArrayList<>();
    for (ProductItemResponse productItemResponse : productDetailResponse
        .getProductItemResponses()) {
      ProductItemRequest productItemRequest = ConverterUtil.setProductItemBasicDetails(pdtProductDomainEventModel,
        productItemResponse);
      prepareProductItemAttributeValueRequestList(productItemResponse, productItemRequest, pdtProductDomainEventModel);
      ConverterUtil.setProductRequestItemImageList(pdtProductDomainEventModel, productItemResponse,
          productItemRequest);
      productItemRequestList.add(productItemRequest);
    }
    productRequest.setProductItems(productItemRequestList);
  }

  private void prepareProductItemAttributeValueRequestList(ProductItemResponse productItemResponse,
    ProductItemRequest productItemRequest, PDTProductDomainEventModel productDomainEventModel) {
    PDTProductItemDomainEventModel pdtProductItemDomainEventModel1 =
      productDomainEventModel.getProductItems().stream().filter(
        pdtProductItemDomainEventModel -> pdtProductItemDomainEventModel.getSkuCode()
          .equals(productItemResponse.getSkuCode())).findFirst().get();
    Map<String, ProductItemAttributeValueDomainEventModel>
      productItemAttributeValueDomainEventModelMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(
      pdtProductItemDomainEventModel1.getProductItemAttributeValues())) {
      productItemAttributeValueDomainEventModelMap =
        pdtProductItemDomainEventModel1.getProductItemAttributeValues().stream().collect(
          Collectors.toMap(
            productItemAttributeValueResponse -> productItemAttributeValueResponse.getAttribute()
              .getAttributeCode(),
            productItemAttributeValueResponse -> productItemAttributeValueResponse));
    }
    Map<String, ProductItemAttributeValueResponse> productItemAttributeValueResponseMap =
      productItemResponse.getProductItemAttributeValueResponses().stream().collect(Collectors.toMap(
        productItemAttributeValueResponse -> productItemAttributeValueResponse.getAttributeResponse().getAttributeCode(),
        productItemAttributeValueResponse -> productItemAttributeValueResponse));
    List<ProductItemAttributeValueRequest> productItemAttributeValueRequestList = new ArrayList<>();
    for (ProductItemAttributeValueResponse productItemAttributeValueResponse : productItemResponse.getProductItemAttributeValueResponses()) {
      if (productItemAttributeValueDomainEventModelMap.containsKey(
        productItemAttributeValueResponse.getAttributeResponse().getAttributeCode())) {
        ProductItemAttributeValueRequest productItemAttributeValueRequest =
          new ProductItemAttributeValueRequest();
        productItemAttributeValueRequest.setValue(productItemAttributeValueDomainEventModelMap.get(
          productItemAttributeValueResponse.getAttributeResponse().getAttributeCode()).getValue());
        AttributeRequest attributeRequest = new AttributeRequest();
        AttributeResponse attributeResponse =
          productItemAttributeValueResponse.getAttributeResponse();
        BeanUtils.copyProperties(attributeResponse, attributeRequest, "attributeType",
          "allowedAttributeValues", "predefinedAllowedAttributeValues");
        attributeRequest.setAttributeType(
          Enum.valueOf(AttributeType.class, attributeResponse.getAttributeType()));
        setProductRequestAllowedAttributeValues(attributeRequest, attributeResponse);
        setProductRequestPredefinedAttributeValues(attributeRequest, attributeResponse);
        productItemAttributeValueRequest.setAttribute(attributeRequest);
        productItemAttributeValueRequestList.add(productItemAttributeValueRequest);
      }
    }
    if (CollectionUtils.isNotEmpty(
      pdtProductItemDomainEventModel1.getProductItemAttributeValues())) {
      for (ProductItemAttributeValueDomainEventModel productItemAttributeValueDomainEventModel :
        pdtProductItemDomainEventModel1.getProductItemAttributeValues()) {
        if (!productItemAttributeValueResponseMap.containsKey(
          productItemAttributeValueDomainEventModel.getAttribute().getAttributeCode())) {
          ProductItemAttributeValueRequest productItemAttributeValueRequest =
            new ProductItemAttributeValueRequest();
          productItemAttributeValueRequest.setValue(
            productItemAttributeValueDomainEventModel.getValue());
          AttributeResponse attributeResponse = productOutbound.getAttributeDetailByAttributeCode(
            productItemAttributeValueDomainEventModel.getAttribute().getAttributeCode());
          AttributeRequest attributeRequest = new AttributeRequest();
          BeanUtils.copyProperties(attributeResponse, attributeRequest, "attributeType",
            "allowedAttributeValues", "predefinedAllowedAttributeValues");
          attributeRequest.setAttributeType(Enum.valueOf(AttributeType.class,
            productItemAttributeValueDomainEventModel.getAttribute().getAttributeType()));
          productItemAttributeValueRequest.setAttribute(attributeRequest);
          productItemAttributeValueRequestList.add(productItemAttributeValueRequest);
        }
      }
    }
    productItemRequest.setProductItemAttributeValues(productItemAttributeValueRequestList);
  }

  private void setProductRequestPredefinedAttributeValues(AttributeRequest attributeRequest,
      AttributeResponse attributeResponse) {
    if (CollectionUtils.isNotEmpty(attributeResponse.getPredefinedAllowedAttributeValues())) {
      List<PredefinedAllowedAttributeValueRequest> predefinedAllowedAttributeValueRequests =
          new ArrayList<>();
      for (PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse :
          attributeResponse.getPredefinedAllowedAttributeValues()) {
        PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
            new PredefinedAllowedAttributeValueRequest();
        BeanUtils.copyProperties(predefinedAllowedAttributeValueResponse,
            predefinedAllowedAttributeValueRequest);
        predefinedAllowedAttributeValueRequests.add(predefinedAllowedAttributeValueRequest);
      }
      attributeRequest
          .setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueRequests);
    }
  }

  private void setProductRequestAllowedAttributeValues(AttributeRequest attributeRequest,
      AttributeResponse attributeResponse) {
    if (Objects.nonNull(attributeResponse.getAllowedAttributeValues())) {
      List<AllowedAttributeValueRequest> allowedAttributeValueRequests = new ArrayList<>();
      for (AllowedAttributeValueResponse allowedAttributeValueResponse : attributeResponse
          .getAllowedAttributeValues()) {
        AllowedAttributeValueRequest allowedAttributeValueRequest =
            new AllowedAttributeValueRequest();
        BeanUtils.copyProperties(allowedAttributeValueResponse, allowedAttributeValueRequest);
        allowedAttributeValueRequests.add(allowedAttributeValueRequest);
      }
      attributeRequest.setAllowedAttributeValues(allowedAttributeValueRequests);
    }
  }

  private void setProductRequestAttributesOnCategoryChange(PDTProductDomainEventModel pdtProductDomainEventModel,
      ProductDetailResponse productDetailResponse, ProductRequest productRequest) throws Exception {
    Map<String, ProductAttributeResponse> existingAttributeCodesMap =
        productDetailResponse.getProductAttributeResponses().stream()
            .filter(productAttributeResponse -> !productAttributeResponse.isMarkForDelete()).collect(Collectors
            .toMap(productAttributeResponse -> productAttributeResponse.getAttribute().getAttributeCode(),
                productAttributeResponse -> productAttributeResponse));

    productRequest.setProductAttributes(new ArrayList<>());
    for (ProductAttributeDomainEventModel productAttributeDomainEventModel : pdtProductDomainEventModel
        .getProductAttributes()) {
      if (existingAttributeCodesMap.containsKey(productAttributeDomainEventModel.getAttribute().getAttributeCode())) {
        setProductAttributeResponsesForExistingAttributes(existingAttributeCodesMap.get(
            productAttributeDomainEventModel.getAttribute().getAttributeCode()),
          productAttributeDomainEventModel, productRequest);
      } else {
        setProductAttributeResponsesForNewAttributes(productAttributeDomainEventModel, productRequest);
      }
    }
  }

  private void setProductAttributeResponsesForExistingAttributes(
    ProductAttributeResponse productAttributeResponse,
    ProductAttributeDomainEventModel productAttributeDomainEventModel,
    ProductRequest productRequest) throws Exception {
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    BeanUtils.copyProperties(productAttributeResponse, productAttributeRequest, "attribute",
        "productAttributeValues");
    AttributeRequest attributeRequest = new AttributeRequest();
    AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
    BeanUtils.copyProperties(attributeResponse, attributeRequest, "attributeType", "allowedAttributeValues",
        "predefinedAllowedAttributeValues");
    productAttributeRequest.setAttribute(attributeRequest);
    if (productAttributeResponse.getAttribute().isSkuValue()) {
      return;
    }
    if (AttributeType.DEFINING_ATTRIBUTE.name().equals(productAttributeDomainEventModel.getAttribute().getAttributeType())) {
      setProductRequestDefiningAttributeValues(productAttributeResponse, productAttributeRequest,
          productAttributeDomainEventModel, attributeRequest);
    } else if (AttributeType.PREDEFINED_ATTRIBUTE.name()
      .equals(productAttributeDomainEventModel.getAttribute().getAttributeType())) {
      setProductRequestPredefinedAttributeValues(productAttributeResponse, productAttributeRequest,
          productAttributeDomainEventModel, attributeRequest);
    } else {
      ConverterUtil.setProductRequestDescriptiveAttributeValues(productAttributeResponse, productAttributeRequest,
          productAttributeDomainEventModel, attributeRequest);
    }
    productRequest.getProductAttributes().add(productAttributeRequest);
  }

  private void setProductAttributeResponsesForNewAttributes(
      ProductAttributeDomainEventModel productAttributeDomainEventModel,
      ProductRequest productRequest) throws Exception {
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    AttributeResponse attributeResponse = productOutbound
        .getAttributeDetailByAttributeCode(productAttributeDomainEventModel.getAttribute().getAttributeCode());
    if (attributeResponse.isSkuValue()) {
      return;
    }
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(attributeResponse);
    AttributeRequest attributeRequest = new AttributeRequest();
    BeanUtils.copyProperties(attributeResponse, attributeRequest, "attributeType", "allowedAttributeValues",
        "predefinedAllowedAttributeValues");
    if (AttributeType.PREDEFINED_ATTRIBUTE.name()
      .equals(productAttributeDomainEventModel.getAttribute().getAttributeType())) {
      setProductRequestPredefinedAttributeValues(productAttributeResponse, productAttributeRequest,
          productAttributeDomainEventModel, attributeRequest);
    } else if (AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
      .equals(productAttributeDomainEventModel.getAttribute().getAttributeType())) {
      ConverterUtil.setProductRequestDescriptiveAttributeValues(productAttributeResponse, productAttributeRequest,
          productAttributeDomainEventModel, attributeRequest);
    }
    productRequest.getProductAttributes().add(productAttributeRequest);
  }

  private void setProductRequestAttributes(PDTProductDomainEventModel pdtProductDomainEventModel,
      ProductDetailResponse productDetailResponse, ProductRequest productRequest) throws Exception {
    List<ProductAttributeRequest> productAttributeRequestList = new ArrayList<>();
    for (ProductAttributeResponse productAttributeResponse : productDetailResponse
        .getProductAttributeResponses()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      BeanUtils.copyProperties(productAttributeResponse, productAttributeRequest, "attribute",
          "productAttributeValues");
      for (ProductAttributeDomainEventModel productAttributeDomainEventModel :
          pdtProductDomainEventModel.getProductAttributes()) {
        if (Objects.nonNull(productAttributeDomainEventModel) && productAttributeDomainEventModel
            .getAttribute().getAttributeCode()
            .equals(productAttributeResponse.getAttribute().getAttributeCode())) {
          AttributeRequest attributeRequest = new AttributeRequest();
          AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
          BeanUtils.copyProperties(attributeResponse, attributeRequest, "attributeType",
              "allowedAttributeValues", "predefinedAllowedAttributeValues");
          if (AttributeType.DEFINING_ATTRIBUTE.name()
            .equals(productAttributeDomainEventModel.getAttribute().getAttributeType())) {
            setProductRequestDefiningAttributeValues(productAttributeResponse,
                productAttributeRequest, productAttributeDomainEventModel, attributeRequest);
          } else if (AttributeType.PREDEFINED_ATTRIBUTE.name()
            .equals(productAttributeDomainEventModel.getAttribute().getAttributeType())) {
            setProductRequestPredefinedAttributeValues(productAttributeResponse,
                productAttributeRequest, productAttributeDomainEventModel, attributeRequest);
          } else {
            ConverterUtil.setProductRequestDescriptiveAttributeValues(productAttributeResponse,
                productAttributeRequest, productAttributeDomainEventModel, attributeRequest);
          }
          productAttributeRequestList.add(productAttributeRequest);
        }
      }
    }
    productRequest.setProductAttributes(productAttributeRequestList);
  }

  private void setProductRequestPredefinedAttributeValues(ProductAttributeResponse productAttributeResponse,
      ProductAttributeRequest productAttributeRequest,
      ProductAttributeDomainEventModel productAttributeDomainEventModel, AttributeRequest attributeRequest)
      throws Exception {
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    productAttributeRequest.setAttribute(attributeRequest);
    String attributeValue =
        productAttributeDomainEventModel.getProductAttributeValues().stream().findFirst()
            .map(ProductAttributeValueDomainEventModel::getPredefinedAllowedAttributeValue)
            .map(PredefinedAllowedAttributeValueDomainEventModel::getValue).orElse(StringUtils.EMPTY);
    List<ProductAttributeValueRequest> productAttributeValueRequestList = new ArrayList<>();
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    ProductAttributeValueResponse productAttributeValueResponse =
      new ProductAttributeValueResponse();
    if (CollectionUtils.isNotEmpty(productAttributeResponse.getProductAttributeValues())) {
      productAttributeValueResponse =
          productAttributeResponse.getProductAttributeValues().get(0);
    }
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse =
        getPredefinedAllowedAttributeValueResponse(productAttributeDomainEventModel, attributeValue,
            predefinedAllowedAttributeValueResponse);
    BeanUtils.copyProperties(productAttributeValueResponse, productAttributeValueRequest,
      "allowedAttributeValue", "descriptiveAttributeValueType", "predefinedAllowedAttributeValue");
    productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    BeanUtils.copyProperties(predefinedAllowedAttributeValueResponse, predefinedAllowedAttributeValueRequest);
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productAttributeValueRequestList.add(productAttributeValueRequest);
    productAttributeRequest.setProductAttributeValues(productAttributeValueRequestList);
  }

  private PredefinedAllowedAttributeValueResponse getPredefinedAllowedAttributeValueResponse(
      ProductAttributeDomainEventModel productAttributeDomainEventModel, String attributeValue,
      PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse) {
    try {
      if (!StringUtils.isEmpty(attributeValue)) {
        predefinedAllowedAttributeValueResponse =
            this.predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(
                Constants.DEFAULT_REQUEST_ID, productAttributeDomainEventModel.getAttribute().getAttributeCode(),
                attributeValue);
      }
    } catch (Exception e) {
      log.error("Failed to fetch attribute response by attributeCode = {} , value = {} ",
          productAttributeDomainEventModel.getAttribute().getAttributeCode(), attributeValue);
      try {
        predefinedAllowedAttributeValueResponse =
            this.predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(
                Constants.DEFAULT_REQUEST_ID, productAttributeDomainEventModel.getAttribute().getAttributeCode(),
                Constants.HYPHEN);
      } catch (Exception ex) {
        log.error("Failed to fetch attribute response by attributeCode = {} , value = {} ",
            productAttributeDomainEventModel.getAttribute().getAttributeCode(), attributeValue);
      }
    }
    return predefinedAllowedAttributeValueResponse;
  }

  private void setProductRequestDefiningAttributeValues(ProductAttributeResponse productAttributeResponse,
      ProductAttributeRequest productAttributeRequest,
      ProductAttributeDomainEventModel productAttributeDomainEventModel, AttributeRequest attributeRequest) {
    attributeRequest.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    productAttributeRequest.setAttribute(attributeRequest);
    String attributeValue =
      productAttributeDomainEventModel.getProductAttributeValues().get(0).getAllowedAttributeValue().getValue();
    List<ProductAttributeValueRequest> productAttributeValueRequestList = new ArrayList<>();
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    ProductAttributeValueResponse productAttributeValueResponse = productAttributeResponse.getProductAttributeValues().get(0);
    if (Objects.nonNull(productAttributeValueResponse)) {
      BeanUtils.copyProperties(productAttributeValueResponse, productAttributeValueRequest, "allowedAttributeValue",
        "descriptiveAttributeValueType", "predefinedAllowedAttributeValue");
      productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
      AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
      BeanUtils.copyProperties(productAttributeValueResponse.getAllowedAttributeValue(), allowedAttributeValueRequest,
        "value");
      allowedAttributeValueRequest.setValue(attributeValue);
      productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest);
    }
    productAttributeValueRequestList.add(productAttributeValueRequest);
    productAttributeRequest.setProductAttributeValues(productAttributeValueRequestList);
  }

  private void saveVendorHistory(PDTProductDomainEventModel response, String username) throws Exception {
    log.info("set saveVendorHistory : {}", response.getProductNotes());
    if (Objects.nonNull(response.getProductNotes())) {
      NeedRevisionNotes revisionNotes = new NeedRevisionNotes();
      BeanUtils.copyProperties(response.getProductNotes(), revisionNotes);
      List<ItemNeedRevisionNotes> itemNotes = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(response.getProductItems())) {
        for (PDTProductItemDomainEventModel itemResponse : response.getProductItems()) {
          if (Objects.nonNull(itemResponse.getItemNotes())) {
            ItemNeedRevisionNotes itemNeedRevisionNotes = new ItemNeedRevisionNotes();
            BeanUtils.copyProperties(itemResponse.getItemNotes(), itemNeedRevisionNotes);
            itemNotes.add(itemNeedRevisionNotes);
          }
        }
      }
      revisionNotes.setItemNotes(itemNotes);
      this.productLevel1HistoryService
          .createForNeedRevision(response.getProductCode(), WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue(),
              generateNeedForCorrectionNotes(revisionNotes), revisionNotes, username);
    }
  }

  private String generateNeedForCorrectionNotes(NeedRevisionNotes revisionNotes) {
    StringBuilder stringBuilder = new StringBuilder();
    Set<String> errorReasons = new HashSet<>();
    Set<String> errorNotes = new HashSet<>();
    if (CollectionUtils.isNotEmpty(revisionNotes.getVendorNotes())) {
      errorReasons.addAll(revisionNotes.getVendorNotes());
      errorNotes.add(revisionNotes.getContentAdditionalNotes());
    }
    if (CollectionUtils.isNotEmpty(revisionNotes.getImageReason())) {
      errorReasons.addAll(revisionNotes.getImageReason());
    } else {
      if (CollectionUtils.isNotEmpty(revisionNotes.getItemNotes())) {
        for (ItemNeedRevisionNotes itemNotes : revisionNotes.getItemNotes()) {
          if (CollectionUtils.isNotEmpty(itemNotes.getVendorNotes())) {
            errorReasons.addAll(itemNotes.getVendorNotes());
          }
        }
      }
    }
    if (CollectionUtils.isNotEmpty(revisionNotes.getCommonImageReason())) {
      for (String reason : revisionNotes.getCommonImageReason()) {
          errorReasons.add(reason);
      }
    }
      if (StringUtils.isNotEmpty(revisionNotes.getImagesAdditionalNotes())) {
        errorNotes.add(revisionNotes.getImagesAdditionalNotes());
      }

    if (CollectionUtils.isNotEmpty(errorReasons)) {
      stringBuilder.append(StringUtils.join(errorReasons, Constants.COMMA)).append(Constants.DASH_DELIMITER)
          .append(StringUtils.join(errorNotes, Constants.COMMA));
    } else {
      stringBuilder = new StringBuilder(StringUtils.join(errorNotes, Constants.COMMA));
    }
    return stringBuilder.toString();
  }

  @Override
  public void updateStateInPBPAndSendProductToNeedRevision(String productCode, String notes,
      NeedRevisionNotes revisionNotes, boolean autoNeedRevision, boolean screeningAction, boolean validateDraftState,
      ProductCollection productCollection) throws Exception {
    String username = Constants.SYSTEM+Constants.HYPHEN+Constants.AUTO_NEED_REVISION;
    this.productLevel1WipService
        .returnDraftForCorrection(productCode, revisionNotes, autoNeedRevision, screeningAction, validateDraftState);
    this.productLevel3WipService.returnDraftForCorrection(productCode, notes);
    this.productLevel1HistoryService.createForNeedRevision(productCode,
        WorkflowProcessCode.RETURN_FOR_CORRECTION.getValue(), notes, revisionNotes,
        (autoNeedRevision) ? username : StringUtils.EMPTY);
    this.productMailEventService.sendDomainEventForSentForCorrection(productCode, notes);
    this.productService.publishProductStatusEventByProductCode(productCode, ProductStatus.NEED_CORRECTION, notes);
    this.productLevel1CollectionService.updateProductWorkFlowStateForNeedRevision(productCode);
    solrActiveProductCollectionService.deleteSolrProductCollectionDocument(productCollection.getProductId());
    List<ProductBusinessPartner> productBusinessPartnerList =
        this.productBusinessPartnerService.findByStoreIdAndProductId(
            GdnMandatoryRequestParameterUtil.getStoreId(), productCollection.getProductId());
    for (ProductBusinessPartner businessPartner : productBusinessPartnerList) {
      resetAppealedFlagAndDecreaseCount(productCollection.getStoreId(),
          productCollection.getBusinessPartnerCode(), businessPartner,
          productBusinessPartnerService, productAppealService);
    }
    solrReviewProductCollectionService.deleteProductFromReviewProductCollection(productCollection.getProductId());
  }
}
