package com.gdn.partners.pbp.distributiontask;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.mta.product.enums.ProductType;
import com.gdn.mta.product.service.NeedCorrectionServiceBean;

import com.gdn.mta.product.util.CommonUtils;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import org.apache.commons.lang3.tuple.Pair;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.service.ProductAppealService;
import com.gdn.mta.product.service.util.ApproveProductUtils;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.ApproveProductService;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductDistributionService;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.partners.pbp.distributiontask.PredefinedAttributeAllowedValueService;
import com.gdn.mta.product.service.partners.pbp.distributiontask.ProductDistributionTaskQCService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.entity.workflow.product.ProductWorkflowStatus;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTImageDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductItemDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTRevisedProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.PredefinedAllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by akshay.bhatt on 25/04/18
 */

@Service("productDistributionTaskQCService")
@Slf4j
public class ProductDistributionTaskQCServiceBean implements ProductDistributionTaskQCService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductDistributionTaskQCServiceBean.class);
  private static final String CONTENT_APPROVED = "CONTENT_APPROVED";
  private static final String IMAGES_APPROVED = "IMAGES_APPROVED";
  private static final String STATE_ACTIVATED = "ACTIVE";
  private static final List<String> ELIGLIBLE_STATUS_TO_APPROVE_CONTENT = Collections
      .unmodifiableList(Arrays.asList("IN_VENDOR", "CONTENT_APPROVAL"));
  private static final List<String> ELIGLIBLE_STATUS_TO_APPROVE_IMAGES = Collections
      .unmodifiableList(Arrays.asList("IN_VENDOR", "IMAGE_APPROVAL"));
  private static final List<String> ELIGLIBLE_STATUS_TO_PROCESS_IMAGES = Collections
      .unmodifiableList(Arrays.asList("IN_VENDOR", "PROCESS_IMAGE"));
  public static final String CONTENT = "CONTENT";
  public static final String IMAGE = "IMAGE";
  public static final String CONTENT_AND_IMAGE = "CONTENT_AND_IMAGE";
  private static final String SYSTEM_REVIEW = "System-Review";
  private static final String RESIZE = "resize";

  @Autowired
  private ProductWfService productWfService;

  @Autowired
  private PredefinedAttributeAllowedValueService predefinedAttributeAllowedValueService;

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductAppealService productAppealService;

  @Autowired
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Autowired
  private ApproveProductService approveProductService;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ProductDistributionService productDistributionService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Value("${special.attribute.value.check.switch}")
  private boolean specialAttributeValueCheckSwitch;

  @Value("${ignore.pre.defined.attribute.value.check.switch}")
  private boolean ignorePreDefinedAttributeValueCheckSwitch;

  @Value("${fetch.brand.value.by.predefined.attribute.value.code}")
  private boolean fetchBrandValueByPredefinedAttributeValueCode;

  @Value("${avoid.duplicate.rescaling.enabled}")
  private boolean avoidDuplicateRescalingEnabled;

  @Value("${override.post.live.flag}")
  private boolean overridePostLiveFlag;

  @Value("${check.eligibility.edited.vendor.approval}")
  private boolean checkEligibilityForEditedVendorApproval;

  @Value("${revised.product.score.api}")
  private boolean revisedProductScoreApi;

  @Value("${bopis.category.action.on.category.change.switch}")
  private boolean bopisCategoryActionOnCategoryChangeSwitch;

  private int inventoryApiBatchSize;

  public Pair<ProductRequest, CategoryResponse> convertPDTDomainModelToProductRequest(PDTProductDomainEventModel pdtProductDomainEventModel,
      ProductDetailResponse productDetailResponse, boolean needActiveFlag) throws Exception {
    ProductRequest productRequest = new ProductRequest();
    CategoryResponse categoryResponse = null;
    productRequest.setBrandCode(pdtProductDomainEventModel.getBrandCode());
    productRequest.setBrandApprovalStatus(pdtProductDomainEventModel.getBrandApprovalStatus());
    productRequest.setBrand(pdtProductDomainEventModel.getBrand());
    BeanUtils.copyProperties(pdtProductDomainEventModel, productRequest, "markForDelete", "productItems",
        "productCategories", "productAttributes", "images");
    setProductRequestParams(productDetailResponse, productRequest);
    if (pdtProductDomainEventModel.getProductCategories().get(0).getCategory().getCategoryCode().equals(
        productDetailResponse.getProductCategoryResponses().stream()
            .filter(productCategoryResponse -> !productCategoryResponse.isMarkForDelete()).findFirst().get()
            .getCategory().getCategoryCode())) {
      setProductRequestCategories(productDetailResponse, productRequest);
      setProductRequestAttributes(pdtProductDomainEventModel, productDetailResponse, productRequest);
    } else {
      categoryResponse =
        setProductRequestCategoriesOnCategoryChange(productRequest, pdtProductDomainEventModel);
      setProductRequestAttributesOnCategoryChange(pdtProductDomainEventModel, productDetailResponse, productRequest);
    }
    setProductRequestImages(pdtProductDomainEventModel, productRequest, needActiveFlag);
    setProductRequestItems(pdtProductDomainEventModel, productDetailResponse, productRequest, needActiveFlag);
    return Pair.of(productRequest, categoryResponse);
  }

  private CategoryResponse setProductRequestCategoriesOnCategoryChange(ProductRequest productRequest,
      PDTProductDomainEventModel productDomainEventModel) {
    CategoryResponse categoryResponse = null;
    productRequest.setProductCategories(new ArrayList<>());
    for (ProductCategoryDomainEventModel productCategoryDomainEventModel : productDomainEventModel
        .getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      categoryResponse = productOutbound
          .getCategoryDetailByCategoryCode(productCategoryDomainEventModel.getCategory().getCategoryCode());
      CategoryRequest category = new CategoryRequest();
      BeanUtils.copyProperties(categoryResponse, category, "catalog");
      if (Objects.nonNull(categoryResponse.getCatalog())) {
        CatalogRequest catalog = new CatalogRequest();
        BeanUtils.copyProperties(categoryResponse.getCatalog(), catalog);
        category.setCatalog(catalog);
      }
      productCategoryRequest.setCategory(category);
      productRequest.getProductCategories().add(productCategoryRequest);
    }
    return categoryResponse;
  }

  private void setProductRequestCategories(ProductDetailResponse productDetailResponse,
      ProductRequest productRequest) {
    List<ProductCategoryRequest> productCategoryRequestList = new ArrayList<>();
    for (ProductCategoryResponse productCategoryResponse : productDetailResponse
        .getProductCategoryResponses()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      BeanUtils.copyProperties(productCategoryResponse, productCategoryRequest, "category");
      CategoryRequest categoryRequest = new CategoryRequest();
      CategoryResponse categoryResponse = productCategoryResponse.getCategory();
      if (categoryResponse != null) {
        BeanUtils.copyProperties(categoryResponse, categoryRequest, "catalog");
        CatalogRequest catalogRequest = new CatalogRequest();
        if (categoryResponse.getCatalog() != null) {
          BeanUtils.copyProperties(categoryResponse.getCatalog(), catalogRequest);
          categoryRequest.setCatalog(catalogRequest);
        }
        productCategoryRequest.setCategory(categoryRequest);
      }
      productCategoryRequestList.add(productCategoryRequest);
    }
    productRequest.setProductCategories(productCategoryRequestList);
  }

  private void setProductRequestItems(PDTProductDomainEventModel pdtProductDomainEventModel,
      ProductDetailResponse productDetailResponse, ProductRequest productRequest, boolean needActiveFlag) {
    List<ProductItemRequest> productItemRequestList = new ArrayList<>();
    for (ProductItemResponse productItemResponse : productDetailResponse
        .getProductItemResponses()) {
      boolean internalUpdate = productItemResponse.isInternalUpdate();
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItemResponse, productItemRequest, "images",
          "productItemAttributeValueResponses");
      productItemRequest.setActivated(productItemResponse.isActivated());
      productItemRequest.setViewable(productItemResponse.isViewable());
      for (ProductItemDomainEventModel productItemDomainEventModel : pdtProductDomainEventModel
          .getProductItems()) {
        if (productItemDomainEventModel.getSkuCode().equals(productItemResponse.getSkuCode())) {
          productItemRequest
              .setGeneratedItemName(productItemDomainEventModel.getGeneratedItemName());
          if (pdtProductDomainEventModel.isPostLive() && !internalUpdate && Objects
              .nonNull(productItemResponse.getDangerousGoodsLevel()) && !productItemDomainEventModel
              .getDangerousGoodsLevel().equals(productItemResponse.getDangerousGoodsLevel())) {
            productItemRequest.setDangerousGoodsLevel(productItemDomainEventModel.getDangerousGoodsLevel());
          } else if(!pdtProductDomainEventModel.isPostLive()) {
            productItemRequest.setDangerousGoodsLevel(productItemDomainEventModel.getDangerousGoodsLevel());
          }
          productItemRequest.setUpcCode(productItemDomainEventModel.getUpcCode());
          break;
        }
      }
      prepareProductItemAttributeValueRequestList(productItemResponse, productItemRequest, pdtProductDomainEventModel);
      setProductRequestItemImageList(pdtProductDomainEventModel, productItemResponse,
          productItemRequest, needActiveFlag);
      productItemRequestList.add(productItemRequest);
    }
    productRequest.setProductItems(productItemRequestList);
  }

  private void setProductRequestItemImageList(PDTProductDomainEventModel pdtProductDomainEventModel,
      ProductItemResponse productItemResponse, ProductItemRequest productItemRequest, boolean needActiveFlag) {
    for (PDTProductItemDomainEventModel productItemDomainEventModel : pdtProductDomainEventModel.getProductItems()) {
      if (productItemDomainEventModel.getSkuCode().equals(productItemResponse.getSkuCode())) {
        List<Image> itemImageList = new ArrayList<>();
        if (!needActiveFlag) {
          for (ImageDomainEventModel imageDomainEventModel : productItemDomainEventModel.getImages()) {
            Image image = new Image();
            image.setLocationPath(imageDomainEventModel.getLocationPath());
            image.setMainImages(imageDomainEventModel.isMainImage());
            image.setSequence(imageDomainEventModel.getSequence());
            image.setOriginalImage(Boolean.FALSE);
            image.setStoreId(productItemRequest.getStoreId());
            image.setCommonImage(imageDomainEventModel.isCommonImage());
            itemImageList.add(image);
          }
        } else {
          for (PDTImageDomainEventModel imageDomainEventModel : productItemDomainEventModel
              .getPdtImageDomainEventModels()) {
            Image image = new Image();
            image.setLocationPath(imageDomainEventModel.getLocationPath());
            image.setMainImages(imageDomainEventModel.isMainImage());
            image.setSequence(imageDomainEventModel.getSequence());
            image.setOriginalImage(Boolean.FALSE);
            image.setStoreId(productItemRequest.getStoreId());
            image.setActive(imageDomainEventModel.isActive());
            image.setCommonImage(imageDomainEventModel.isCommonImage());
            itemImageList.add(image);
          }
        }
        productItemRequest.setImages(itemImageList);
        break;
      }
    }
  }

  private void prepareProductItemAttributeValueRequestList(ProductItemResponse productItemResponse,
      ProductItemRequest productItemRequest, PDTProductDomainEventModel productDomainEventModel) {
    PDTProductItemDomainEventModel pdtProductItemDomainEventModel1 = productDomainEventModel.getProductItems().stream()
        .filter(pdtProductItemDomainEventModel -> pdtProductItemDomainEventModel.getSkuCode()
            .equals(productItemResponse.getSkuCode())).findFirst().get();
    Map<String, ProductItemAttributeValueDomainEventModel> productItemAttributeValueDomainEventModelMap = new HashMap<>();
    if (!CollectionUtils.isEmpty(pdtProductItemDomainEventModel1.getProductItemAttributeValues())) {
       productItemAttributeValueDomainEventModelMap =
          pdtProductItemDomainEventModel1.getProductItemAttributeValues().stream().collect(Collectors.toMap(
              productItemAttributeValueResponse -> productItemAttributeValueResponse.getAttribute().getAttributeCode(),
              productItemAttributeValueResponse -> productItemAttributeValueResponse));
    }
    Map<String, ProductItemAttributeValueResponse> productItemAttributeValueResponseMap =
        productItemResponse.getProductItemAttributeValueResponses().stream().collect(Collectors.toMap(
            productItemAttributeValueResponse -> productItemAttributeValueResponse.getAttributeResponse()
                .getAttributeCode(), productItemAttributeValueResponse -> productItemAttributeValueResponse,
            (a, b) -> a));
    List<ProductItemAttributeValueRequest> productItemAttributeValueRequestList =
        new ArrayList<>();
    for (ProductItemAttributeValueResponse productItemAttributeValueResponse : productItemResponse
        .getProductItemAttributeValueResponses()) {
      if (productItemAttributeValueDomainEventModelMap
          .containsKey(productItemAttributeValueResponse.getAttributeResponse().getAttributeCode())) {
        ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
        productItemAttributeValueRequest.setValue(productItemAttributeValueDomainEventModelMap
            .get(productItemAttributeValueResponse.getAttributeResponse().getAttributeCode()).getValue());
        AttributeRequest attributeRequest = new AttributeRequest();
        AttributeResponse attributeResponse = productItemAttributeValueResponse.getAttributeResponse();
        BeanUtils.copyProperties(attributeResponse, attributeRequest, "attributeType", "allowedAttributeValues",
            "predefinedAllowedAttributeValues");
        attributeRequest.setAttributeType(Enum.valueOf(AttributeType.class, attributeResponse.getAttributeType()));
        setProductRequestAllowedAttributeValues(attributeRequest, attributeResponse);
        setProductRequestPredefinedAttributeValues(attributeRequest, attributeResponse);
        productItemAttributeValueRequest.setAttribute(attributeRequest);
        productItemAttributeValueRequestList.add(productItemAttributeValueRequest);
      }
    }
    if (!CollectionUtils.isEmpty(pdtProductItemDomainEventModel1.getProductItemAttributeValues())) {
      for (ProductItemAttributeValueDomainEventModel productItemAttributeValueDomainEventModel :
          pdtProductItemDomainEventModel1.getProductItemAttributeValues()) {
        if (!productItemAttributeValueResponseMap
            .containsKey(productItemAttributeValueDomainEventModel.getAttribute().getAttributeCode())) {
          ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
          productItemAttributeValueRequest.setValue(productItemAttributeValueDomainEventModel.getValue());
          AttributeResponse attributeResponse = productOutbound.getAttributeDetailByAttributeCode(
              productItemAttributeValueDomainEventModel.getAttribute().getAttributeCode());
          AttributeRequest attributeRequest = new AttributeRequest();
          BeanUtils.copyProperties(attributeResponse, attributeRequest, "attributeType", "allowedAttributeValues",
              "predefinedAllowedAttributeValues");
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
    if (attributeResponse.getPredefinedAllowedAttributeValues() != null) {
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
    if (attributeResponse.getAllowedAttributeValues() != null) {
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
        ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
        setProductAttributeResponsesForExistingAttributes(
            existingAttributeCodesMap.get(productAttributeDomainEventModel.getAttribute().getAttributeCode()),
            productAttributeDomainEventModel, productAttributeRequest, pdtProductDomainEventModel);
        productRequest.getProductAttributes().add(productAttributeRequest);
      } else {
        ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
        setProductAttributeResponsesForNewAttributes(productAttributeDomainEventModel, productAttributeRequest,
            pdtProductDomainEventModel);
        productRequest.getProductAttributes().add(productAttributeRequest);
      }
    }
  }

  private void setProductAttributeResponsesForExistingAttributes(
     ProductAttributeResponse productAttributeResponse,
      ProductAttributeDomainEventModel productAttributeDomainEventModel,
      ProductAttributeRequest productAttributeRequest, PDTProductDomainEventModel pdtProductDomainEventModel) throws Exception {
    BeanUtils.copyProperties(productAttributeResponse, productAttributeRequest, "attribute",
        "productAttributeValues");
    AttributeRequest attributeRequest = new AttributeRequest();
    AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
    BeanUtils.copyProperties(attributeResponse, attributeRequest, "attributeType", "allowedAttributeValues",
        "predefinedAllowedAttributeValues");
    productAttributeRequest.setAttribute(attributeRequest);
    if (productAttributeDomainEventModel.getAttribute().getAttributeType()
        .equals(AttributeType.DEFINING_ATTRIBUTE.toString())) {
      setProductRequestDefiningAttributeValues(productAttributeResponse, productAttributeRequest,
          productAttributeDomainEventModel, attributeRequest);
    } else if (productAttributeDomainEventModel.getAttribute().getAttributeType()
        .equals(AttributeType.PREDEFINED_ATTRIBUTE.toString())) {
      setProductRequestPredefinedAttributeValues(productAttributeResponse, productAttributeRequest,
          productAttributeDomainEventModel, attributeRequest, pdtProductDomainEventModel);
    } else {
      setProductRequestDescriptiveAttributeValues(productAttributeResponse, productAttributeRequest,
          productAttributeDomainEventModel, attributeRequest);
    }
  }

  private void setProductAttributeResponsesForNewAttributes(
      ProductAttributeDomainEventModel productAttributeDomainEventModel,
      ProductAttributeRequest productAttributeRequest, PDTProductDomainEventModel pdtProductDomainEventModel) throws Exception {
    AttributeResponse attributeResponse = productOutbound
        .getAttributeDetailByAttributeCode(productAttributeDomainEventModel.getAttribute().getAttributeCode());
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(attributeResponse);
    AttributeRequest attributeRequest = new AttributeRequest();
    BeanUtils.copyProperties(attributeResponse, attributeRequest, "attributeType", "allowedAttributeValues",
        "predefinedAllowedAttributeValues");
    if (productAttributeDomainEventModel.getAttribute().getAttributeType()
        .equals(AttributeType.PREDEFINED_ATTRIBUTE.toString())) {
      setProductRequestPredefinedAttributeValues(productAttributeResponse, productAttributeRequest,
          productAttributeDomainEventModel, attributeRequest, pdtProductDomainEventModel);
    } else if (productAttributeDomainEventModel.getAttribute().getAttributeType()
        .equals(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString())) {
      setProductRequestDescriptiveAttributeValues(productAttributeResponse, productAttributeRequest,
          productAttributeDomainEventModel, attributeRequest);
    }
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
        if (productAttributeDomainEventModel != null && productAttributeDomainEventModel
            .getAttribute().getAttributeCode()
            .equals(productAttributeResponse.getAttribute().getAttributeCode())) {
          AttributeRequest attributeRequest = new AttributeRequest();
          AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
          BeanUtils.copyProperties(attributeResponse, attributeRequest, "attributeType",
              "allowedAttributeValues", "predefinedAllowedAttributeValues");
          if (productAttributeDomainEventModel.getAttribute().getAttributeType().equals(
              AttributeType.DEFINING_ATTRIBUTE.toString())) {
            setProductRequestDefiningAttributeValues(productAttributeResponse,
                productAttributeRequest, productAttributeDomainEventModel, attributeRequest);
          } else if (productAttributeDomainEventModel.getAttribute().getAttributeType()
              .equals(AttributeType.PREDEFINED_ATTRIBUTE.toString())) {
            setProductRequestPredefinedAttributeValues(productAttributeResponse,
                productAttributeRequest, productAttributeDomainEventModel, attributeRequest, pdtProductDomainEventModel);
          } else {
            setProductRequestDescriptiveAttributeValues(productAttributeResponse,
                productAttributeRequest, productAttributeDomainEventModel, attributeRequest);
          }
          productAttributeRequestList.add(productAttributeRequest);
          break;
        }
      }
    }
    productRequest.setProductAttributes(productAttributeRequestList);
  }

  private void setProductRequestDescriptiveAttributeValues(
      ProductAttributeResponse productAttributeResponse,
      ProductAttributeRequest productAttributeRequest,
      ProductAttributeDomainEventModel productAttributeDomainEventModel,
      AttributeRequest attributeRequest) {
    attributeRequest.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    productAttributeRequest.setAttribute(attributeRequest);
    String attributeValue = productAttributeDomainEventModel.getProductAttributeValues().get(0)
        .getDescriptiveAttributeValue();
    List<ProductAttributeValueRequest> productAttributeValueRequestList = new ArrayList<>();
    ProductAttributeValueRequest productAttributeValueRequest =
        new ProductAttributeValueRequest();
    ProductAttributeValueResponse productAttributeValueResponse = null;
    if (!CollectionUtils.isEmpty(productAttributeResponse.getProductAttributeValues())) {
      productAttributeValueResponse = productAttributeResponse.getProductAttributeValues().get(0);
    }
    if (productAttributeValueResponse != null) {
      BeanUtils.copyProperties(productAttributeValueResponse, productAttributeValueRequest,
          "allowedAttributeValue", "descriptiveAttributeValueType",
          "predefinedAllowedAttributeValue");
      productAttributeValueRequest.setDescriptiveAttributeValueType(
          productAttributeValueResponse.getDescriptiveAttributeValueType());
      productAttributeValueRequest.setDescriptiveAttributeValue(attributeValue);
    } else {
      productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
      productAttributeValueRequest.setDescriptiveAttributeValue(attributeValue);
    }
    productAttributeValueRequestList.add(productAttributeValueRequest);
    productAttributeRequest.setProductAttributeValues(productAttributeValueRequestList);
  }

  private void setProductRequestPredefinedAttributeValues(ProductAttributeResponse productAttributeResponse,
      ProductAttributeRequest productAttributeRequest,
      ProductAttributeDomainEventModel productAttributeDomainEventModel, AttributeRequest attributeRequest,
      PDTProductDomainEventModel pdtProductDomainEventModel)
      throws Exception {
    attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    productAttributeRequest.setAttribute(attributeRequest);
    String attributeValue =
            productAttributeDomainEventModel.getProductAttributeValues().stream().findFirst()
                    .map(ProductAttributeValueDomainEventModel::getPredefinedAllowedAttributeValue)
                    .map(PredefinedAllowedAttributeValueDomainEventModel::getValue).orElse(StringUtils.EMPTY);
    List<ProductAttributeValueRequest> productAttributeValueRequestList = new ArrayList<>();
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    ProductAttributeValueResponse productAttributeValueResponse = null;
    if (!CollectionUtils.isEmpty(productAttributeResponse.getProductAttributeValues())) {
      productAttributeValueResponse =
          productAttributeResponse.getProductAttributeValues().get(0);
    }
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    if (!StringUtils.isEmpty(attributeValue)) {
      try {
        if (fetchBrandValueByPredefinedAttributeValueCode && StringUtils.isNotEmpty(
            pdtProductDomainEventModel.getBrandCode()) && Constants.BRAND.equalsIgnoreCase(
            productAttributeDomainEventModel.getAttribute().getName())) {
          try {
            predefinedAllowedAttributeValueResponse = fetchBrandValueBasedOnPredefinedAttributeCode(productAttributeDomainEventModel,
                pdtProductDomainEventModel);
          } catch (Exception e) {
            log.error("Exception while getting brand value for productCode {} and brandCode : {} ",
                pdtProductDomainEventModel.getProductCode(), pdtProductDomainEventModel.getBrandCode());
            predefinedAllowedAttributeValueResponse =
                getPredefinedAllowedAttributeValueResponse(productAttributeDomainEventModel, attributeValue);
          }
        } else {
          predefinedAllowedAttributeValueResponse =
              getPredefinedAllowedAttributeValueResponse(productAttributeDomainEventModel, attributeValue);
        }
      } catch (Exception e) {
        if (productAttributeResponse.getAttribute().isSkuValue() && specialAttributeValueCheckSwitch) {
          predefinedAllowedAttributeValueResponse =
              getPredefinedAllowedAttributeValueResponse(productAttributeDomainEventModel, Constants.HYPHEN);
          log.error(
              "Predefined special attribute value doesnt exist in PCB for attributeCode = {}, attributeValue = {} ",
              productAttributeResponse.getAttribute().getAttributeCode(), attributeValue);
        } else if (!Constants.BRAND.equalsIgnoreCase(productAttributeDomainEventModel.getAttribute().getName()) &&
            ignorePreDefinedAttributeValueCheckSwitch) {
          log.error("Predefined attribute value doesnt exist in PCB for attributeCode = {}, attributeValue = {} ",
              productAttributeResponse.getAttribute().getAttributeCode(), attributeValue);
        } else {
          log.error(
              "Predefined special attribute value doesnt exist in PCB for attributeCode = {}, attributeValue = {} ",
              productAttributeResponse.getAttribute().getAttributeCode(), attributeValue);
          throw e;
        }
      }
    }
    if (productAttributeValueResponse != null) {
      BeanUtils.copyProperties(productAttributeValueResponse, productAttributeValueRequest, "allowedAttributeValue",
          "descriptiveAttributeValueType", "predefinedAllowedAttributeValue");
      productAttributeValueRequest
          .setDescriptiveAttributeValueType(productAttributeValueResponse.getDescriptiveAttributeValueType());
      BeanUtils.copyProperties(predefinedAllowedAttributeValueResponse, predefinedAllowedAttributeValueRequest);
      productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    } else {
      productAttributeValueRequest
          .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
      BeanUtils.copyProperties(predefinedAllowedAttributeValueResponse, predefinedAllowedAttributeValueRequest);
      productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    }
    productAttributeValueRequestList.add(productAttributeValueRequest);
    productAttributeRequest.setProductAttributeValues(productAttributeValueRequestList);
  }

  private PredefinedAllowedAttributeValueResponse getPredefinedAllowedAttributeValueResponse(
      ProductAttributeDomainEventModel productAttributeDomainEventModel, String attributeValue)
      throws Exception {
    return this.predefinedAttributeAllowedValueService.findByStoreIdAndMatchAttributeCodeAndValue(
        GdnMandatoryRequestParameterUtil.getRequestId(),
        productAttributeDomainEventModel.getAttribute().getAttributeCode(), attributeValue);
  }

  private PredefinedAllowedAttributeValueResponse fetchBrandValueBasedOnPredefinedAttributeCode(
      ProductAttributeDomainEventModel productAttributeDomainEventModel,
      PDTProductDomainEventModel pdtProductDomainEventModel) {
    return productOutbound.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
        productAttributeDomainEventModel.getAttribute().getAttributeCode(), pdtProductDomainEventModel.getBrandCode(),
        true);
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
    ProductAttributeValueResponse productAttributeValueResponse =
        productAttributeResponse.getProductAttributeValues().get(0);
    if (productAttributeValueResponse != null) {
      BeanUtils.copyProperties(productAttributeValueResponse, productAttributeValueRequest, "allowedAttributeValue",
          "descriptiveAttributeValueType", "predefinedAllowedAttributeValue");
      productAttributeValueRequest
          .setDescriptiveAttributeValueType(productAttributeValueResponse.getDescriptiveAttributeValueType());
      AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
      BeanUtils.copyProperties(productAttributeValueResponse.getAllowedAttributeValue(), allowedAttributeValueRequest,
          "value");
      allowedAttributeValueRequest.setValue(attributeValue);
      productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest);
    }
    productAttributeValueRequestList.add(productAttributeValueRequest);
    productAttributeRequest.setProductAttributeValues(productAttributeValueRequestList);
  }

  private void setProductRequestImages(PDTProductDomainEventModel pdtProductDomainEventModel,
      ProductRequest productRequest, boolean needActiveFlag) {
    List<Image> imageList = new ArrayList<>();
    if (!needActiveFlag) {
      for (ImageDomainEventModel imageDomainEventModel : pdtProductDomainEventModel.getImages()) {
        Image image = new Image();
        image.setLocationPath(imageDomainEventModel.getLocationPath());
        image.setMainImages(imageDomainEventModel.isMainImage());
        image.setSequence(imageDomainEventModel.getSequence());
        image.setOriginalImage(Boolean.FALSE);
        image.setCommonImage(imageDomainEventModel.isCommonImage());
        image.setStoreId(productRequest.getStoreId());
        imageList.add(image);
      }
    } else {
      for (PDTImageDomainEventModel imageDomainEventModel : pdtProductDomainEventModel.getPdtImageDomainEventModels()) {
        Image image = new Image();
        image.setLocationPath(imageDomainEventModel.getLocationPath());
        image.setMainImages(imageDomainEventModel.isMainImage());
        image.setSequence(imageDomainEventModel.getSequence());
        image.setOriginalImage(Boolean.FALSE);
        image.setStoreId(productRequest.getStoreId());
        image.setCommonImage(imageDomainEventModel.isCommonImage());
        image.setActive(imageDomainEventModel.isActive());
        imageList.add(image);
      }
    }
    productRequest.setImages(imageList);
  }

  private void setProductRequestParams(ProductDetailResponse productDetailResponse,
      ProductRequest productRequest) {
    productRequest.setId(productDetailResponse.getId());
    productRequest.setActivated(productDetailResponse.isActivated());
    productRequest.setViewable(productDetailResponse.isViewable());
    productRequest.setSpecificationDetail(productDetailResponse.getSpecificationDetail());
    productRequest.setLongDescription(productDetailResponse.getLongDescription());
    productRequest.setStoreId(productDetailResponse.getStoreId());
    productRequest.setVersion(productDetailResponse.getVersion());
    productRequest.setCreatedBy(productDetailResponse.getCreatedBy());
    productRequest.setCreatedDate(productDetailResponse.getCreatedDate());
    productRequest.setUpdatedBy(productRequest.getUpdatedBy());
    productRequest.setUpdatedDate(Calendar.getInstance().getTime());
  }

  private boolean isEligibleToProcess(PDTProductDomainEventModel pdtProductDomainEventModel) throws Exception {
    if ((pdtProductDomainEventModel.isPostLive() && !pdtProductDomainEventModel.isReviewPending())
      || pdtProductDomainEventModel.isEdited()) {
      return false;
    }
    ProductWorkflowStatus productWorkflowStatus =
        this.productWfService.status(pdtProductDomainEventModel.getProductCode());
    LOGGER.debug("IsEligible to process :  States {}" , productWorkflowStatus );
    return productWorkflowStatus.getStates().stream().noneMatch(state -> state.equalsIgnoreCase(STATE_ACTIVATED));
  }

  private boolean isEligibleToApproveContent(String productCode) throws Exception {
    ProductWorkflowStatus productWorkflowStatus = this.productWfService.status(productCode);
    LOGGER.debug("IsEligible to Contetnt :  States {}" , productWorkflowStatus );
    return productWorkflowStatus.getStates().stream().anyMatch(ELIGLIBLE_STATUS_TO_APPROVE_CONTENT::contains);
  }

  private boolean isEligibleToApproveImages(String productCode) throws Exception {
    ProductWorkflowStatus productWorkflowStatus = this.productWfService.status(productCode);
    LOGGER.debug("IsEligible to Approve Image :  States {}" , productWorkflowStatus );
    return productWorkflowStatus.getStates().stream().anyMatch(ELIGLIBLE_STATUS_TO_APPROVE_IMAGES::contains);
  }

  private boolean isEligibleToProcessImages(String productCode) throws Exception {
    ProductWorkflowStatus productWorkflowStatus = this.productWfService.status(productCode);
    LOGGER.debug("IsEligible to process image :  States {}" , productWorkflowStatus);
    return productWorkflowStatus.getStates().stream().anyMatch(ELIGLIBLE_STATUS_TO_PROCESS_IMAGES::contains);
  }

  private boolean isEligibleForUpdate(PDTProductDomainEventModel pdtProductDomainEventModel) {
    return pdtProductDomainEventModel.isPostLive() && !pdtProductDomainEventModel.isReviewPending()
        && !pdtProductDomainEventModel.isEdited();
  }

  @Override
  public void processProductDistributionTaskQCKafkaConsumer(PDTProductDomainEventModel pdtProductDomainEventModel, int prioritySeller)
      throws Exception {
    GdnPreconditions.checkArgument(Objects.nonNull(pdtProductDomainEventModel),
        "pdtProductDomainEventModel should not be null");
    boolean isCategoryChanged = false;
    try {
      if (overridePostLiveFlag) {
        ProductCollection productCollection =
          productCollectionRepository.findByStoreIdAndProductCode(
            GdnMandatoryRequestParameterUtil.getStoreId(),
            pdtProductDomainEventModel.getProductCode());
        if (Objects.nonNull(productCollection)) {
          log.info("Overriding post live flag for {} from {} to {}",
            pdtProductDomainEventModel.getProductCode(), pdtProductDomainEventModel.isPostLive(),
            productCollection.isPostLive());
          pdtProductDomainEventModel.setPostLive(productCollection.isPostLive());
          resetAndDecreaseCounter(productCollection);
        }
      }
      if (this.isEligibleToProcess(pdtProductDomainEventModel)) {
        ProductDetailResponse productData = getUpdatedProductData(pdtProductDomainEventModel);
        isCategoryChanged = isCategoryChanged(pdtProductDomainEventModel, productData);
        Pair<ProductRequest, CategoryResponse> productRequestCategoryResponsePair =
          convertPDTDomainModelToProductRequest(pdtProductDomainEventModel, productData, false);
        ProductRequest productRequest = productRequestCategoryResponsePair.getLeft();
        try {
          this.approveProductService.approveQc(pdtProductDomainEventModel.getProductCode());
          productRequest.setVersion(productRequest.getVersion() + 1);
        } catch (Exception e) {
          LOGGER.error("methodName={} action={} service=MTA status=ERROR ref=product-creation productCode={} "
              + "desc=error when approve QC message={}",
            "processProductDistributionTaskQCKafkaConsumer", "approveQc",
            pdtProductDomainEventModel.getProductCode(), e.getMessage(), e);
        }
        if (this.isEligibleToApproveContent(pdtProductDomainEventModel.getProductCode())) {
          this.approveProductService.approveContent(productRequest);
          productRequest.setVersion(productRequest.getVersion() + 1);
        }
        if (this.isEligibleToApproveImages(pdtProductDomainEventModel.getProductCode())) {
          Map<String,String> oldHashCodeXLocationPathsMap = updateProductImageRequest(productData,
            productRequest);
          log.info("Calling with updated request2 : {} for code : {} ", productRequest,
            pdtProductDomainEventModel.getProductCode());
          this.approveProductService.approveImage(productRequest, prioritySeller);
          // before processing images for scaling after approval set the non eligible images as
          // active false to avoid re-scaling
          revertImagePath(productRequest, oldHashCodeXLocationPathsMap);
          // process images for scaling for eligible images (newly added or edited images)
          processAllImagesWithEligibility(prioritySeller, productRequest, productData);
        } else if (this.isEligibleToProcessImages(pdtProductDomainEventModel.getProductCode())) {
          Map<String,String> oldHashCodeXLocationPathsMap = updateProductImageRequest(productData, productRequest);
          log.info("Calling with updated request2 : {} for code : {} ", productRequest,
            pdtProductDomainEventModel.getProductCode());
          if (avoidDuplicateRescalingEnabled) {
            approveProductService.updateImageAndAvoidHashCodeRegeneration(productRequest);
          } else {
            approveProductService.updateImage(productRequest);
          }
          // before processing images for scaling after approval set the non eligible images as
          // active false to avoid re-scaling
          revertImagePath(productRequest, oldHashCodeXLocationPathsMap);
          // process images for scaling for eligible images (newly added or edited images)
          processAllImagesWithEligibility(prioritySeller, productRequest, productData);
        }
        takeActionsOnCategoryChangeFromVendor(pdtProductDomainEventModel, isCategoryChanged, productData,
          productRequestCategoryResponsePair.getRight());
      } else if (isEligibleForUpdate(pdtProductDomainEventModel)) {
        ProductDetailResponse productData = getUpdatedProductData(pdtProductDomainEventModel);
        isCategoryChanged = isCategoryChanged(pdtProductDomainEventModel, productData);
        Pair<ProductRequest, CategoryResponse> productRequestCategoryResponsePair =
          convertPDTDomainModelToProductRequest(pdtProductDomainEventModel, productData, false);
        ProductRequest productRequest = productRequestCategoryResponsePair.getLeft();
        productRequest.setVersion(productRequest.getVersion() + 1);
        productService.checkIfProductWasTakeDown(productData, productData.getId());
        productRequest.setUpdateFromVendor(true);
        productService.updateProductContent(productRequest);
        productRequest.setVersion(productRequest.getVersion() + 1);
        log.info("Calling with request3 : {} for code : {} ", productRequest,
          pdtProductDomainEventModel.getProductCode());
        Map<String,String> revertImagePath = updateProductImageRequest(productData, productRequest);
        log.info("Calling with updated request3 : {} for code : {} ", productRequest,
          pdtProductDomainEventModel.getProductCode());
        if (avoidDuplicateRescalingEnabled) {
          approveProductService.updateImageAndAvoidHashCodeRegeneration(productRequest);
        } else {
          approveProductService.updateImage(productRequest);
        }
        revertImagePath(productRequest, revertImagePath);
        processAllImagesWithEligibility(prioritySeller, productRequest, productData);
        takeActionsOnCategoryChangeFromVendor(pdtProductDomainEventModel, isCategoryChanged, productData,
          productRequestCategoryResponsePair.getRight());
      }
      xProductOutbound.generateProductScoreByProductSkuOrProductCode(null, pdtProductDomainEventModel.getProductCode(), isCategoryChanged);
    } catch (Exception e) {
      this.productDistributionTaskRepository
          .moveFailedProductToQC(UUID.randomUUID().toString(), Constants.DEFAULT_USERNAME,
              pdtProductDomainEventModel.getProductCode());
      this.productService.saveProductHistory(Constants.DEFAULT_STORE_ID, pdtProductDomainEventModel.getProductCode(),
          Constants.DEFAULT_USERNAME, SaveHistoryConstants.CONTENT_AND_IMAGE_APPROVAL_FAILED, null);
      productService.setReviewPendingFlagToTrue(Constants.DEFAULT_STORE_ID, pdtProductDomainEventModel.getProductCode());
      LOGGER.error("methodName=processProductDistributionTaskQCKafkaConsumer action=approve Product  status=ERROR "
              + "ref=product-creation productCode={} desc=error when activating product ",
          pdtProductDomainEventModel.getProductCode(), e);
    }
  }

  public void processAllImagesWithEligibility(int prioritySeller, ProductRequest productRequest,
    ProductDetailResponse productData) throws Exception {
    Map<String, Image> productImageHashCodeMapDB = getProductImagesMapDB(productData.getImages());
    Map<String, Image> itemImageHashCodeMapDB =
      getItemImagesMapDB(productData.getProductItemResponses());
    if (avoidDuplicateRescalingEnabled) {
      approveProductService.processImageWithScalingEligibility(productRequest, prioritySeller,
        productImageHashCodeMapDB, itemImageHashCodeMapDB);
    } else {
      approveProductService.processImage(productRequest, prioritySeller);
    }
  }

  public Map<String, String> updateProductImageRequest(ProductDetailResponse productDetailResponse,
    ProductRequest productRequest)
      throws Exception {
    Map<String, String> oldHashCodeAndLocationPath = new HashMap<>();

    if (avoidDuplicateRescalingEnabled) {
      ApproveProductUtils.generateImageHashcode(productRequest);

      Map<String, String> activeProductAndItemImageMap =
        getActiveProductAndItemImageMap(productDetailResponse);

      updateImageStateAndLocationAndGenerateOldHashCodesAndLocationMap(productRequest,
        oldHashCodeAndLocationPath, activeProductAndItemImageMap);
    }

    return oldHashCodeAndLocationPath;
  }

  private static void updateImageStateAndLocationAndGenerateOldHashCodesAndLocationMap(
    ProductRequest productRequest, Map<String, String> oldHashCodeAndLocationPath,
    Map<String, String> activeProductAndItemImageMap) {
    Optional.ofNullable(productRequest.getImages()).orElse(new ArrayList<>()).stream()
      .filter(Predicate.not(Image::isMarkForDelete))
      .filter(image -> activeProductAndItemImageMap.containsKey(image.getHashCode()))
      .forEach(image -> {
        oldHashCodeAndLocationPath.put(image.getHashCode(), image.getLocationPath());
        image.setLocationPath(activeProductAndItemImageMap.get(image.getHashCode()));
        image.setActive(true);
      });

    Optional.ofNullable(productRequest.getProductItems()).orElse(new ArrayList<>()).stream()
      .map(ProductItemRequest::getImages).filter(CollectionUtils::isNotEmpty).flatMap(List::stream)
      .filter(Predicate.not(Image::isMarkForDelete))
      .filter(image -> activeProductAndItemImageMap.containsKey(image.getHashCode()))
      .forEach(image -> {
        oldHashCodeAndLocationPath.put(image.getHashCode(), image.getLocationPath());
        image.setLocationPath(activeProductAndItemImageMap.get(image.getHashCode()));
        image.setActive(true);
      });
  }

  private static Map<String, String> getActiveProductAndItemImageMap(
    ProductDetailResponse productDetailResponse) {
    return Stream.concat(
        Optional.ofNullable(productDetailResponse.getImages()).orElse(new ArrayList<>()).stream()
          .filter(Predicate.not(Image::isMarkForDelete)).filter(
            image -> Objects.isNull(image.getOriginalImage()) || Boolean.FALSE.equals(
              image.getOriginalImage())).filter(image -> !image.getLocationPath().contains(RESIZE))
          .map(image -> Pair.of(image.getHashCode(), image.getLocationPath())),

        Optional.ofNullable(productDetailResponse.getProductItemResponses()).orElse(new HashSet<>())
          .stream().filter(Predicate.not(ProductItemResponse::isMarkForDelete))
          .map(ProductItemResponse::getImages).filter(CollectionUtils::isNotEmpty)
          .flatMap(List::stream).filter(Predicate.not(Image::isMarkForDelete)).filter(
            image -> Objects.isNull(image.getOriginalImage()) || Boolean.FALSE.equals(
              image.getOriginalImage())).filter(image -> !image.getLocationPath().contains(RESIZE))
          .map(image -> Pair.of(image.getHashCode(), image.getLocationPath())))
      .collect(Collectors.toMap(Pair::getLeft, Pair::getRight, (v1, v2) -> v2));
  }

  private void revertImagePath(ProductRequest productRequest, Map<String, String> oldHashCodeAndLocationPath) {
    if (avoidDuplicateRescalingEnabled) {
      Optional.ofNullable(productRequest.getImages()).orElse(new ArrayList<>()).stream()
        .filter(Predicate.not(Image::isMarkForDelete))
        .filter(image -> oldHashCodeAndLocationPath.containsKey(image.getHashCode()))
        .forEach(image -> {
          image.setLocationPath(oldHashCodeAndLocationPath.get(image.getHashCode()));
          image.setActive(false);
        });

      Optional.ofNullable(productRequest.getProductItems()).orElse(new ArrayList<>()).stream()
        .map(ProductItemRequest::getImages).filter(CollectionUtils::isNotEmpty)
        .flatMap(List::stream).filter(Predicate.not(Image::isMarkForDelete))
        .filter(image -> oldHashCodeAndLocationPath.containsKey(image.getHashCode()))
        .forEach(image -> {
          image.setLocationPath(oldHashCodeAndLocationPath.get(image.getHashCode()));
          image.setActive(false);
        });
    }
  }

  private Map<String, Image> getProductImagesMapDB(List<Image> images) {
    return images.stream().filter(this::isImageValidForScaling).distinct()
      .collect(Collectors.toMap(Image::getHashCode, Function.identity()));
  }

  private Map<String, Image> getItemImagesMapDB(Set<ProductItemResponse> productItemResponses) {
    Map<String, Image> itemImageHashCodeMapDB = new HashMap<>();
    for (ProductItemResponse productItemResponse : productItemResponses) {
      productItemResponse.getImages().stream().filter(this::isImageValidForScaling)
        .forEach(image -> itemImageHashCodeMapDB.putIfAbsent(image.getHashCode(), image));
    }
    return itemImageHashCodeMapDB;
  }

  private boolean isImageValidForScaling(Image image) {
    return !image.isMarkForDelete() &&
      (Objects.isNull(image.getOriginalImage()) || Boolean.FALSE.equals(image.getOriginalImage())) &&
      (image.isActive() || !image.getLocationPath().contains(RESIZE));
  }



  @Override
  public void processVendorApprovalEventForEditedProducts(PDTProductDomainEventModel pdtProductDomainEventModel,
      String approvalType) throws Exception {
    if (!pdtProductDomainEventModel.isReviewPending()) {
      ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(GdnMandatoryRequestParameterUtil.getStoreId(),
          pdtProductDomainEventModel.getProductCode());
      if (overridePostLiveFlag) {
        if (Objects.nonNull(productCollection)) {
          log.info("Overriding post live flag for {} from {} to {}",
            pdtProductDomainEventModel.getProductCode(), pdtProductDomainEventModel.isPostLive(),
            productCollection.isPostLive());
          pdtProductDomainEventModel.setPostLive(productCollection.isPostLive());
          resetAndDecreaseCounter(productCollection);
        }
      }
      if (isEligibleForProcessing(productCollection)) {
        ProductDetailResponse productData = getUpdatedProductData(pdtProductDomainEventModel);
        Pair<ProductRequest, CategoryResponse> productRequestCategoryResponsePair =
          convertPDTDomainModelToProductRequest(pdtProductDomainEventModel, productData, true);
        ProductRequest productRequest = productRequestCategoryResponsePair.getLeft();
        productRequest.setReviewPending(false);
        ProfileResponse profileResponse =
          businessPartnerRepository.filterDetailByBusinessPartnerCode(pdtProductDomainEventModel.getMerchantCode());
        if (pdtProductDomainEventModel.isRevised() && !pdtProductDomainEventModel.isPostLive()) {
          updateAndActivateRevisedProduct(pdtProductDomainEventModel, productRequest, productData.getId(),
            productData.getStoreId(), profileResponse, productCollection, productData.getProductCategoryResponses());
        }
        boolean isCategoryChanged = false;
        switch (approvalType) {
          case CONTENT: {
            isCategoryChanged = isCategoryChanged(pdtProductDomainEventModel, productData);
            if (!(pdtProductDomainEventModel.isRevised() && !pdtProductDomainEventModel.isPostLive())) {
              productRequest.setVersion(productRequest.getVersion() + 1);
              productRequest.setUpdateFromVendor(true);
              productService.updateProductContent(productRequest);
              productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE,
                Arrays.asList(pdtProductDomainEventModel.getProductCode()));
              String productSku = productService.isProductActivationNeeded(productData.getStoreId(),
                productData.getId());
              if (StringUtils.isNotBlank(productSku)) {
                if (pdtProductDomainEventModel.isRevised()) {
                  productLevel3Service.activateProductOnNeedCorrection(productData.getStoreId(),
                    productSku, profileResponse, productData.getProductCategoryResponses());
                } else {
                  productLevel3Service.takeDownOrReactivateProduct(productData.getStoreId(),
                    productSku, false, null, null);
                }
                LOGGER.info(
                  "Updating the content change for product sku at time of reactivation : {}",
                  productSku);
                xProductOutbound.updateContentChange(productSku, true, true);
              }
            }
            productCollection = productService.updateReviewType(productData.getStoreId(),
              pdtProductDomainEventModel.getProductCode(), null);
            publishReviewProductCollectionDeleteEvent(productCollection);
            productService.updateSolrProductCollection(productCollection);
            takeActionsOnCategoryChangeFromVendor(pdtProductDomainEventModel, isCategoryChanged,
              productData, productRequestCategoryResponsePair.getRight());
            this.productDistributionService.removeProductFromPDT(UUID.randomUUID().toString(),
              GdnMandatoryRequestParameterUtil.getUsername(), new RemoveProductRequest(pdtProductDomainEventModel.getProductCode()));
            break;
          }
          case IMAGE: {
            productRequest.setVersion(productRequest.getVersion() + 1);
            approveProductService.updateImage(productRequest);
            approveProductService.processEditedImage(productRequest);
            break;
          }
          case CONTENT_AND_IMAGE: {
            isCategoryChanged = isCategoryChanged(pdtProductDomainEventModel, productData);
            productRequest.setVersion(productRequest.getVersion() + 1);
            productRequest.setUpdateFromVendor(true);
            productService.updateProductContent(productRequest);
            productRequest.setVersion(productRequest.getVersion() + 1);
            approveProductService.updateImage(productRequest);
            approveProductService.processEditedImage(productRequest);
            takeActionsOnCategoryChangeFromVendor(pdtProductDomainEventModel, isCategoryChanged,
              productData, productRequestCategoryResponsePair.getRight());
            break;
          }
        }
        List<String> productSkuByProductCode =
          productBusinessPartnerService.getProductSkusByProductCode(pdtProductDomainEventModel.getProductCode());
        if (CollectionUtils.isNotEmpty(productSkuByProductCode) && CONTENT.equals(approvalType)) {
          LOGGER.info("Updating master data field for edited/revised product. Product Sku : {}",
            productSkuByProductCode.get(0));
          xProductOutbound.generateProductScoreByProductSkuOrProductCode(productSkuByProductCode.get(0), null, isCategoryChanged);
        }
      }
      }
  }

  private boolean isEligibleForProcessing(ProductCollection productCollection) {
    return !checkEligibilityForEditedVendorApproval ||
      (Objects.nonNull(productCollection) && productCollection.isReviewPending());
  }


  private void publishReviewProductCollectionDeleteEvent(ProductCollection productCollection) {
    SolrReviewProductCollectionDeleteEvent solrReviewProductCollectionDeleteEvent =
        new SolrReviewProductCollectionDeleteEvent();
    solrReviewProductCollectionDeleteEvent.setIds(Collections.singletonList(productCollection.getId()));
    kafkaProducer.send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
        solrReviewProductCollectionDeleteEvent);
  }

  private void updateAndActivateRevisedProduct(PDTProductDomainEventModel pdtProductDomainEventModel,
      ProductRequest productRequest, String productId, String storeId,
    ProfileResponse profileResponse, ProductCollection productCollection,
      List<ProductCategoryResponse> categoryResponseList) throws Exception {
    productRequest.setActivated(true);
    productRequest.setViewable(true);
    productRequest.setVersion(productRequest.getVersion() + 1);
    productRequest.setUpdateFromVendor(true);
    productService.updateProductContent(productRequest);
    productOutbound
        .republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(pdtProductDomainEventModel.getProductCode()));
    String productSku = productService.isProductActivationNeeded(storeId, productId);
    if (StringUtils.isNotBlank(productSku)) {
      productLevel3Service.activateProductOnNeedCorrection(storeId, productSku, profileResponse, categoryResponseList);
      xProductOutbound.updateContentChange(productSku, true, true);
    }
    productCollection.setState("ACTIVE");
    productCollectionRepository.save(productCollection);
    productService.updateSolrProductCollection(productCollection);
  }

  @Override
  public void processVendorApprovalEventForRevisedProducts(PDTProductDomainEventModel pdtProductDomainEventModel,
      PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel) throws Exception {
    boolean reviewPending = pdtRevisedProductVendorApprovedEventModel.isReviewPending();
    boolean postlive = pdtRevisedProductVendorApprovedEventModel.isPostLive();
    if (overridePostLiveFlag) {
      ProductCollection productCollection = productCollectionRepository.findByStoreIdAndProductCode(
        GdnMandatoryRequestParameterUtil.getStoreId(), pdtProductDomainEventModel.getProductCode());
      if (Objects.nonNull(productCollection)) {
        log.info("Overriding post live flag for {} from {} to {}",
          pdtProductDomainEventModel.getProductCode(), pdtProductDomainEventModel.isPostLive(),
          productCollection.isPostLive());
        pdtProductDomainEventModel.setPostLive(productCollection.isPostLive());
        if (!reviewPending) {
          resetAndDecreaseCounter(productCollection);
        }
      }
    }
    if (StringUtils.isNotBlank(pdtRevisedProductVendorApprovedEventModel.getApprovalType())) {
      pdtProductDomainEventModel.setRevised(true);
      pdtProductDomainEventModel.setPostLive(postlive);
      pdtProductDomainEventModel.setReviewPending(reviewPending);
      processVendorApprovalEventForEditedProducts(pdtProductDomainEventModel,
          pdtRevisedProductVendorApprovedEventModel.getApprovalType());
    } else {
      if (postlive || !reviewPending) {
        ProductDetailResponse productData = getUpdatedProductData(pdtProductDomainEventModel);
        boolean isCategoryChanged = isCategoryChanged(pdtProductDomainEventModel, productData);
        Pair<ProductRequest, CategoryResponse> productRequestCategoryResponsePair =
          convertPDTDomainModelToProductRequest(pdtProductDomainEventModel, productData, true);
        ProductRequest productRequest = productRequestCategoryResponsePair.getLeft();
        productRequest.setReviewPending(reviewPending);
        productRequest.setActivated(true);
        productRequest.setViewable(true);
        productRequest.setVersion(productRequest.getVersion() + 1);
        productRequest.setPublishProductEvent(false);
        productRequest.setUpdateFromVendor(true);
        productService.updateProductContent(productRequest);
        if (!reviewPending && !CollectionUtils.isEmpty(productRequest.getImages())) {
          productRequest.setVersion(productRequest.getVersion() + 1);
          approveProductService.updateImage(productRequest);
        }
        ProductDetailResponse productDetailResponse =
            productOutbound.getImagesForScalingByProductCode(productData.getProductCode());
        if (CollectionUtils.isEmpty(productDetailResponse.getImages())) {
          LOGGER.info("No images found for scaling for revised product : {}", productData.getProductCode());
          productWfService.approveImageForRevisedProduct(productData.getStoreId(), productData.getProductCode(), null, isCategoryChanged,
              productData.getProductCategoryResponses());
        } else {
          LOGGER.info("Sending images for scaling for revised product : {}", productData.getProductCode());
          approveProductService.processImageForRevisedProduct(productDetailResponse);
        }
        takeActionsOnCategoryChangeFromVendor(pdtProductDomainEventModel, isCategoryChanged,
          productData, productRequestCategoryResponsePair.getRight());
        checkAutoApprovalCriteriaForRevisedProduct(reviewPending, postlive, productData,
            CollectionUtils.isEmpty(productDetailResponse.getImages()));
        if (!reviewPending) {
          this.productDistributionService
              .removeProductFromPDT(UUID.randomUUID().toString(), GdnMandatoryRequestParameterUtil.getUsername(),
                  new RemoveProductRequest(pdtProductDomainEventModel.getProductCode()));
        }
        productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE,
          Arrays.asList(productData.getProductCode()));
        if (revisedProductScoreApi && isCategoryChanged) {
          xProductOutbound.generateProductScoreByProductSkuOrProductCode(null,
            pdtProductDomainEventModel.getProductCode(), true);
        }
      }
    }
  }

  public void resetAndDecreaseCounter(ProductCollection productCollection) throws Exception {
    ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerService.findFirstByStoreIdAndProductId(productCollection.getStoreId(),
            productCollection.getProductId());
    if (Objects.nonNull(productBusinessPartner) && !Constants.INTERNAL.equalsIgnoreCase(
      productCollection.getBusinessPartnerCode())) {
    NeedCorrectionServiceBean.resetAppealedFlagAndDecreaseCount(productCollection.getStoreId(),
        productCollection.getBusinessPartnerCode(), productBusinessPartner,
        productBusinessPartnerService, productAppealService);
    }
  }


  private void checkAutoApprovalCriteriaForRevisedProduct(boolean reviewPending, boolean postlive,
      ProductDetailResponse productDetailResponse, boolean emptyScalingImages) {
    try {
      if (reviewPending && postlive && emptyScalingImages) {
        ProductCollection productCollection = productCollectionRepository
            .findByStoreIdAndProductCode(productDetailResponse.getStoreId(), productDetailResponse.getProductCode());
        AutoApprovalTypeRequest autoApprovalTypeRequest = new AutoApprovalTypeRequest();
        autoApprovalTypeRequest.setCategoryCode(
            productDetailResponse.getCategoryCodes().get(productDetailResponse.getCategoryCodes().size() - 1));
        autoApprovalTypeRequest.setEdited(productCollection.isEdited());
        autoApprovalTypeRequest
            .setReviewType(productCollection.isEdited() ? productCollection.getReviewType() : CONTENT_AND_IMAGE);
        autoApprovalTypeRequest.setRevised(true);
        autoApprovalTypeRequest.setProductCode(productCollection.getProductCode());
        autoApprovalTypeRequest.setStoreId(productCollection.getStoreId());
        productStatusPublisherService.publishVendorCombinedEvent(autoApprovalTypeRequest);
      }
    } catch (Exception e) {
      LOGGER.error("Exception when trying to auto approve revised product : {}, error : ",
          productDetailResponse.getProductCode(), e);
    }
  }

  private void updateFlagsAndSendEmailOnCategoryChange(PDTProductDomainEventModel pdtProductDomainEventModel,
      ProductDetailResponse productData, boolean isCategoryChanged) throws Exception {
    if (pdtProductDomainEventModel.isMarginExceeded() && isCategoryChanged) {
      String existingCategoryCode = productData.getProductCategoryResponses().stream()
          .filter(productCategoryResponse -> !productCategoryResponse.isMarkForDelete()).findFirst().get()
          .getCategory().getCategoryCode();
      String existingCategoryName = productData.getProductCategoryResponses().stream()
          .filter(productCategoryResponse -> !productCategoryResponse.isMarkForDelete()).findFirst().get()
          .getCategory().getName();
      productService.updateBuyableAndDiscoverableFlagAndSendMailOnMarginChange(productData.getStoreId(),
          existingCategoryCode, existingCategoryName, productData.getProductCode(),
          pdtProductDomainEventModel.isPostLive(), SYSTEM_REVIEW);
    }
  }

  private boolean isCategoryChanged(PDTProductDomainEventModel pdtProductDomainEventModel,
      ProductDetailResponse productData) {
    boolean isCategoryChanged;
    if (pdtProductDomainEventModel.getProductCategories().get(0).getCategory().getCategoryCode().equals(
        productData.getProductCategoryResponses().stream()
            .filter(productCategoryResponse -> !productCategoryResponse.isMarkForDelete()).findFirst().get()
            .getCategory().getCategoryCode())) {
      isCategoryChanged = false;
    } else {
      isCategoryChanged = true;
    }
    return isCategoryChanged;
  }

  private ProductDetailResponse getUpdatedProductData(PDTProductDomainEventModel pdtProductDomainEventModel) throws Exception {
    ProductDetailResponse productData;
    productData = this.productService.findProductDetailByProductCode(pdtProductDomainEventModel.getProductCode(), false);
    if (productData == null) {
      LOGGER.error("cannot find product data for code = {}", pdtProductDomainEventModel.getProductCode());
      throw new IllegalArgumentException("null value for findProductDetailByProductCode");
    }
    ProductResponse productBasicDetailsFromDB =
        productService.findProductBasicDetailByProductCode(pdtProductDomainEventModel.getProductCode());
    if (productBasicDetailsFromDB == null) {
      LOGGER.error("cannot find product basic details for code = {}", productData.getProductCode());
      throw new IllegalArgumentException("null value for findProductBasicDetailByProductCode");
    }
    if (productBasicDetailsFromDB.getVersion() > productData.getVersion()){
      this.productService
          .clearMasterProductCacheSync(productBasicDetailsFromDB.getId(), productBasicDetailsFromDB.getProductCode());
      ProductDetailResponse updatedProductData  = this.productService
          .findProductDetailByProductCode(pdtProductDomainEventModel.getProductCode(), false);
      LOGGER.info("Cache evict failed for product {}, outdated Product Data Response = {}, updated data response = {}",
          productData.getProductCode(), productData, updatedProductData);
      productData = updatedProductData;
    }
    return productData;
  }

  private void takeActionsOnCategoryChangeFromVendor(PDTProductDomainEventModel pdtProductDomainEventModel,
    boolean isCategoryChanged, ProductDetailResponse productData, CategoryResponse categoryResponse) throws Exception {
    if (isCategoryChanged) {
      String existingCategoryCode = productData.getProductCategoryResponses().stream()
        .filter(productCategoryResponse -> !productCategoryResponse.isMarkForDelete()).findFirst()
        .map(ProductCategoryResponse::getCategory).map(CategoryResponse::getCategoryCode)
        .orElse(StringUtils.EMPTY);
      String existingCategoryName = productData.getProductCategoryResponses().stream()
        .filter(productCategoryResponse -> !productCategoryResponse.isMarkForDelete()).findFirst()
        .map(ProductCategoryResponse::getCategory).map(CategoryResponse::getName)
        .orElse(StringUtils.EMPTY);
      if(bopisCategoryActionOnCategoryChangeSwitch) {
        processCategoryUpdateNewFlow(pdtProductDomainEventModel, productData, existingCategoryCode,
          existingCategoryName, categoryResponse);
      }
      else if(pdtProductDomainEventModel.isMarginExceeded()) {
        productService.updateBuyableAndDiscoverableFlagAndSendMailOnMarginChange(productData.getStoreId(),
          existingCategoryCode, existingCategoryName, productData.getProductCode(),
          pdtProductDomainEventModel.isPostLive(),
          SYSTEM_REVIEW);
      }
    }
  }

  private void processCategoryUpdateNewFlow(PDTProductDomainEventModel pdtProductDomainEventModel,
    ProductDetailResponse productData, String existingCategoryCode, String existingCategoryName,
    CategoryResponse categoryResponse) throws Exception {
    Boolean bopisEligible = null;
    if (ProductType.BOPIS.getProductType().equals(pdtProductDomainEventModel.getProductType())
      && CommonUtils.isDimensionLess(pdtProductDomainEventModel.getHeight(),
      pdtProductDomainEventModel.getWeight(), pdtProductDomainEventModel.getWeight(),
      pdtProductDomainEventModel.getShippingWeight())) {
      // If product is not physical and dimension not added, check if new category is eligible
      String updatedCategoryCode =
        pdtProductDomainEventModel.getProductCategories().stream().findFirst()
          .map(ProductCategoryDomainEventModel::getCategory)
          .map(CategoryDomainEventModel::getCategoryCode).orElse(StringUtils.EMPTY);
      bopisEligible = Optional.ofNullable(categoryResponse).stream()
        .filter(response -> updatedCategoryCode.equals(response.getCategoryCode()))
        .map(CategoryResponse::isBopisEligible).findFirst().orElseGet(
          () -> productOutbound.getCategoryDetailByCategoryCode(updatedCategoryCode)
            .isBopisEligible());
    }
    boolean eligibleForShippingMigration = Boolean.FALSE.equals(bopisEligible);
    productService.takeActionsOnCategoryChangeFromVendor(productData.getStoreId(),
      existingCategoryCode, existingCategoryName, productData.getProductCode(),
      eligibleForShippingMigration, pdtProductDomainEventModel.isMarginExceeded());
  }

}
