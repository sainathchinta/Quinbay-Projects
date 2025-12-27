package com.gdn.partners.pbp.helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.x.product.rest.web.model.request.VideoAddEditRequest;
import com.gdn.x.productcategorybase.dto.VideoDTO;
import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemsImageUpdateRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.ProductDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.request.B2bFieldsRequest;
import com.gdn.x.product.rest.web.model.request.ItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
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

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ProductItemCreationRequestHelper {

  private static ObjectMapper objectMapper = new ObjectMapper();

  private static final String FAMILY_COLOR_ATTRIBUTE = "Family Colour";

  private static final List<String> ALLOWED_ATTRIBUTE_NAME = new ArrayList<>(Arrays.asList(FAMILY_COLOR_ATTRIBUTE));

  private static final List<Integer> UPC_VALID_LENGTH = new ArrayList<>(Arrays.asList(5, 8, 12, 13));

  private static final List<String> IGNORE_FIELDS_FOR_COPY = new ArrayList<>(Arrays.asList("id", "version"));

  public static ProductCreationRequest createProductItemRequestPayload(ProductDetailResponse productData,
    ProfileResponse businessPartnerProfile, Map<String, ProductItemBusinessPartner> productItemsBusinessPartnerMap,
    ProductCreationType productCreationType, boolean imagesUpdated, String productCode, String pickupPoint,
    String categoryName, String partnerCode) {
    ProductCreationRequest request = new ProductCreationRequest();
    BeanUtils.copyProperties(productData, request, getIgnoreProperties());
    request.setCreatedBy(Constants.DEFAULT_USERNAME);
    request.setUpdatedBy(Constants.DEFAULT_USERNAME);
    request.setCreatedDate(new Date());
    request.setUpdatedDate(new Date());
    request.setActivated(false);
    request.setViewable(false);
    request.setImages(Collections.emptyList());
    request.setProductCode(productCode);
    request.setBusinessPartnerCode(partnerCode);
    request.setBusinessPartnerId(businessPartnerProfile.getId());
    request.setBusinessPartnerName(businessPartnerProfile.getCompany().getBusinessPartnerName());
    request.setCategoryName(getCategoryName(productData));
    request.setGdnProductSku(null);
    request.setImagesUpdated(imagesUpdated);
    request.setProductCreationType(productCreationType);
    List<ProductAttributeRequest> productAttributeRequests = generateProductAttributeRequests(productData);
    log.info("product attribute request {} for productCode {}", productAttributeRequests, productCode);
    request.setProductAttributes(productAttributeRequests);
    List<ProductCategoryRequest> productCategoryRequests = generateProductCategoryRequests(productData);
    log.info("product category request {} for productCode {}", productCategoryRequests, productCode);
    request.setProductCategories(productCategoryRequests);
    List<ProductBusinessPartnerAttributeRequest> businessPartnerAttributeRequests
      = generateProductBusinessPartnerAttributes(productData);
    log.info("product businessPartner request {} for productCode {}", businessPartnerAttributeRequests, productCode);
    request.setProductBusinessPartnerAttributes(businessPartnerAttributeRequests);
    request
      .setProductItemRequests(generateProductItemRequest(productData, productItemsBusinessPartnerMap, pickupPoint));
    return request;
  }

  private static String getCategoryName(ProductDetailResponse productData) {
    if (Objects.isNull(productData.getProductCategoryResponses())) {
      return null;
    }
    return productData.getProductCategoryResponses()
      .stream()
      .findFirst()
      .map(ProductCategoryResponse::getCategory)
      .map(CategoryResponse::getName)
      .orElse(null);
  }

  private static String[] getIgnoreProperties() {
    String[] ignoreProperties = new String[IGNORE_FIELDS_FOR_COPY.size()];
    return IGNORE_FIELDS_FOR_COPY.toArray(ignoreProperties);
  }

  private static List<ProductAttributeRequest> generateProductAttributeRequests(ProductDetailResponse productData) {
    if (Objects.isNull(productData.getProductAttributeResponses())) {
      return Collections.emptyList();
    }
    return productData.getProductAttributeResponses().stream()
      .map(productAttributeResponses -> processProductAttributeRequests(productAttributeResponses,
        productData.getProductCode()))
      .collect(Collectors.toList());
  }

  private static ProductAttributeRequest processProductAttributeRequests(
    ProductAttributeResponse productAttributeResponses, String productCode) {
    log.info("product attribute response {} for productCode {}", productAttributeResponses, productCode);
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    BeanUtils.copyProperties(productAttributeResponses, productAttributeRequest, getIgnoreProperties());
    productAttributeRequest.setCreatedBy(Constants.DEFAULT_USERNAME);
    productAttributeRequest.setUpdatedBy(Constants.DEFAULT_USERNAME);
    productAttributeRequest.setCreatedDate(new Date());
    productAttributeRequest.setUpdatedDate(new Date());
    AttributeRequest attributeRequest = new AttributeRequest();
    Optional.ofNullable(productAttributeResponses.getAttribute())
      .ifPresent(attributeResponse -> generateAttributeRequest(attributeResponse, attributeRequest));
    productAttributeRequest.setAttribute(attributeRequest);

    Optional.ofNullable(productAttributeResponses.getProductAttributeValues())
      .ifPresent(productAttributeValues -> generateProductAttributeRequest(productAttributeValues, productAttributeRequest));
    return productAttributeRequest;
  }

  private static List<ProductCategoryRequest> generateProductCategoryRequests(ProductDetailResponse productData) {
    log.info("product Category response {} for productCode {}", productData.getProductCategoryResponses(),
      productData.getProductCode());
    if (Objects.isNull(productData.getProductCategoryResponses())) {
      return Collections.emptyList();
    }
    return productData.getProductCategoryResponses()
      .stream()
      .map(ProductItemCreationRequestHelper::generateProductCategoryRequest)
      .map(ProductCategoryRequest.class::cast)
      .collect(Collectors.toList());
  }

  private static List<ProductBusinessPartnerAttributeRequest> generateProductBusinessPartnerAttributes(
    ProductDetailResponse productData) {
    log.info("product attribute response {} for productCode {}", productData.getProductAttributeResponses(),
      productData.getProductCode());
    if (Objects.isNull(productData.getProductAttributeResponses())) {
      return Collections.emptyList();
    }
    return productData.getProductAttributeResponses().stream()
      .filter(productAttributeResponses -> productAttributeResponses.getAttribute().isSearchAble())
      .filter(productAttributeResponses -> productAttributeResponses.getAttribute().isSkuValue())
      .map(ProductItemCreationRequestHelper::generateProductBusinessPartnerAttributeRequest)
      .collect(Collectors.toList());
  }

  private static List<ProductItemCreationRequest> generateProductItemRequest(ProductDetailResponse productData,
    Map<String, ProductItemBusinessPartner> productItemBusinessPartnerMap,
    String pickupPoint) {
    List<ProductItemCreationRequest> productItemCreationRequests = new ArrayList<>();
    for (ProductItemResponse productItemResponse : productData.getProductItemResponses()) {
      if (productItemBusinessPartnerMap.containsKey(productItemResponse.getId())) {
        ProductItemCreationRequest request = new ProductItemCreationRequest();
        request.setItemGeneratedName(productItemResponse.getGeneratedItemName());

        if (!CollectionUtils.isEmpty(productItemResponse.getProductItemAttributeValueResponses())) {
          TreeMap<String, String> attributesMap = productItemResponse.getProductItemAttributeValueResponses()
            .stream()
            .filter(ProductItemCreationRequestHelper::isValidAttribute)
            .collect(Collectors.toMap(keyMapper -> keyMapper.getAttributeResponse().getAttributeCode(),
              ProductItemAttributeValueResponse::getValue,
              (value1, value2) -> {
                throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
                  "Duplicate key for values " + value1 + " and " + value2);
              },
              TreeMap::new));
          request.setAttributesMap(attributesMap);
        }
        request.setGdnProductItemSku(null);
        request.setMarkDefaultAddress(true);
        request.setSourceItemCode(productItemResponse.getSkuCode());
        request.setContentChanged(false);
        request.setUpcCode(generateValidUpcCode(productItemResponse.getUpcCode()));
        request.setMerchantSku(null);
        request.setStock(0);
        request.setPickupPointId(pickupPoint);
        request.setProductItemHashCode(Arrays.toString(productItemResponse.getHash()));
        request.setProductItemId(null);
        request.setMinimumStock(0);
        request.setImages(generateImagesOnProductItemLevel(productItemResponse.getImages()));
        request.setProductItemAttributeValueRequests(generateProductItemAttributeValueRequests(productItemResponse));
        ProductItemBusinessPartner itemBusinessPartner = productItemBusinessPartnerMap.get(productItemResponse.getId());
        request.setProductType(itemBusinessPartner.getProductType());
        request.setDisplay(itemBusinessPartner.isDisplay());
        request.setBuyable(itemBusinessPartner.isBuyable());
        request.setPrice(itemBusinessPartner.getPrice());
        request.setSalePrice(itemBusinessPartner.getSalePrice());
        productItemCreationRequests.add(request);
      }
    }
    log.info("product item createion request {}", productItemCreationRequests);
    return productItemCreationRequests;
  }

  private static boolean isValidAttribute(ProductItemAttributeValueResponse productItemAttributeValueResponse) {
    return productItemAttributeValueResponse.getAttributeResponse().isVariantCreation()
      || productItemAttributeValueResponse.getAttributeResponse()
      .getAttributeType().equals(AttributeType.DEFINING_ATTRIBUTE.toString());
  }

  private static List<Image> generateImagesOnProductItemLevel(List<Image> images) {
    return images
      .stream()
      .map(imageResponse -> copyProperties(imageResponse, new Image(), getIgnoreProperties()))
      .map(Image.class::cast)
      .map(ProductItemCreationRequestHelper::setActiveFlagTrue)
      .collect(Collectors.toList());
  }

  private static Image setActiveFlagTrue(Image image) {
    image.setActive(true);
    return image;
  }

  private static String generateValidUpcCode(String upcCode) {
    return isValidUPCCode(upcCode) ? upcCode : StringUtils.EMPTY;
  }

  public static boolean isValidUPCCode(String upcCode) {
    try {
      Long.parseLong(upcCode);
    } catch(NumberFormatException e) {
      return false;
    }
    return UPC_VALID_LENGTH.contains(upcCode.length());
  }

  private static List<ProductItemAttributeValueRequest> generateProductItemAttributeValueRequests(
    ProductItemResponse itemResponse) {
    if (Objects.isNull(itemResponse.getProductItemAttributeValueResponses())) {
      return Collections.emptyList();
    }
    return itemResponse.getProductItemAttributeValueResponses().stream()
      .filter(ProductItemCreationRequestHelper::isAllowedAttributeCode)
      .map(itemAttributeValueResponses -> {
        ProductItemAttributeValueRequest itemAttributeValueRequest = new ProductItemAttributeValueRequest();
        BeanUtils.copyProperties(itemAttributeValueResponses, itemAttributeValueRequest, getIgnoreProperties());
        itemAttributeValueRequest.setValue(itemAttributeValueResponses.getValue());
        AttributeRequest attributeRequest = new AttributeRequest();
        Optional.ofNullable(itemAttributeValueResponses.getAttributeResponse())
          .ifPresent(
            itemAttributeResponse -> processProductItemAttributeValueRequests(itemAttributeResponse, attributeRequest));
        itemAttributeValueRequest.setAttribute(attributeRequest);
        return itemAttributeValueRequest;
      })
      .collect(Collectors.toList());
  }

  private static boolean isAllowedAttributeCode(ProductItemAttributeValueResponse itemAttributeValueResponse) {
    return ALLOWED_ATTRIBUTE_NAME.contains(itemAttributeValueResponse.getAttributeResponse().getName());
  }

  private static ProductBusinessPartnerAttributeRequest generateProductBusinessPartnerAttributeRequest(
    ProductAttributeResponse productAttributeResponses) {
    ProductBusinessPartnerAttributeRequest businessPartnerAttributeRequest = new ProductBusinessPartnerAttributeRequest();
    BeanUtils.copyProperties(productAttributeResponses, businessPartnerAttributeRequest, getIgnoreProperties());
    businessPartnerAttributeRequest.setAttributeId(productAttributeResponses.getAttribute().getId());
    businessPartnerAttributeRequest.setMandatory(productAttributeResponses.getAttribute().isMandatory());
    String value = Optional.ofNullable(productAttributeResponses.getProductAttributeValues())
      .map(ProductItemCreationRequestHelper::getBusinessPartnerAttributeValue)
      .orElse(StringUtils.EMPTY);
    businessPartnerAttributeRequest.setValue(value);
    return businessPartnerAttributeRequest;
  }

  private static String getBusinessPartnerAttributeValue(List<ProductAttributeValueResponse> productAttributeValues) {
    return productAttributeValues.stream()
      .findFirst()
      .filter(productAttributeValue -> Objects.nonNull(productAttributeValue.getPredefinedAllowedAttributeValue()))
      .filter(productAttributeValue -> Objects.nonNull(productAttributeValue.getPredefinedAllowedAttributeValue().getValue()))
      .map(productAttributeValue -> productAttributeValue.getPredefinedAllowedAttributeValue().getValue())
      .orElse(StringUtils.EMPTY);
  }

  private static void generateProductAttributeRequest(List<ProductAttributeValueResponse> productAttributeValues,
    ProductAttributeRequest productAttributeRequest) {
    List<ProductAttributeValueRequest> productAttributeRequestValues = productAttributeValues.stream()
      .map(ProductItemCreationRequestHelper::generateProductAttributeValueRequest)
      .collect(Collectors.toList());
    productAttributeRequest.setProductAttributeValues(productAttributeRequestValues);
  }

  private static ProductAttributeValueRequest generateProductAttributeValueRequest(
    ProductAttributeValueResponse productAttributeValueResponse) {
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    BeanUtils.copyProperties(productAttributeValueResponse, productAttributeValueRequest, getIgnoreProperties());
    AllowedAttributeValueRequest allowedAttributeValue = new AllowedAttributeValueRequest();
    Optional.ofNullable(productAttributeValueResponse.getAllowedAttributeValue())
      .ifPresent(allowedAttributeValueResponse -> BeanUtils
        .copyProperties(allowedAttributeValueResponse, allowedAttributeValue));
    productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValue);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValue
      = new PredefinedAllowedAttributeValueRequest();
    Optional.ofNullable(productAttributeValueResponse.getPredefinedAllowedAttributeValue())
      .ifPresent(predefinedAllowedAttributeValueResponse -> BeanUtils
        .copyProperties(predefinedAllowedAttributeValueResponse, predefinedAllowedAttributeValue));
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    return productAttributeValueRequest;
  }

  private static void generatePredefinedAllowedAttributeValuesRequest(
    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValues, AttributeRequest attributeRequest) {
    List<PredefinedAllowedAttributeValueRequest> predefinedAllowedAttributeValuesRequest =
      predefinedAllowedAttributeValues
        .stream()
        .map(predefinedAllowedAttributeValueResponse -> copyProperties(predefinedAllowedAttributeValueResponse,
          new PredefinedAllowedAttributeValueRequest()))
        .map(PredefinedAllowedAttributeValueRequest.class::cast)
        .collect(Collectors.toList());
    attributeRequest.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValuesRequest);
  }

  private static ProductCategoryRequest generateProductCategoryRequest(
    ProductCategoryResponse productCategoryResponse) {
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    BeanUtils.copyProperties(productCategoryResponse, productCategoryRequest, getIgnoreProperties());
    productCategoryRequest.setCreatedBy(Constants.DEFAULT_USERNAME);
    productCategoryRequest.setUpdatedBy(Constants.DEFAULT_USERNAME);
    productCategoryRequest.setCreatedDate(new Date());
    productCategoryRequest.setUpdatedDate(new Date());
    Optional.ofNullable(productCategoryResponse.getCategory())
      .ifPresent(categoryResponse -> generateCategoryRequest(categoryResponse, productCategoryRequest));
    return productCategoryRequest;
  }

  private static void generateCategoryRequest(CategoryResponse productCategoryResponse,
    ProductCategoryRequest productCategoryRequest) {
    CategoryRequest categoryRequest = new CategoryRequest();
    BeanUtils.copyProperties(productCategoryResponse, categoryRequest);
    if (StringUtils.isNotEmpty(productCategoryResponse.getParentCategoryId())) {
      CategoryRequest parentCategory = new CategoryRequest();
      parentCategory.setId(productCategoryResponse.getParentCategoryId());
      categoryRequest.setParentCategory(parentCategory);
    }
    Optional.ofNullable(productCategoryResponse.getCatalog())
      .ifPresent(catalogResponse -> {
        CatalogRequest catalogRequest = new CatalogRequest();
        BeanUtils.copyProperties(catalogResponse, catalogRequest);
        categoryRequest.setCatalog(catalogRequest);
      });
    productCategoryRequest.setCategory(categoryRequest);
  }

  private static void generateAllowedAttributeValuesRequest(List<AllowedAttributeValueResponse> allowedAttributeValues,
    AttributeRequest attributeRequest) {
    List<AllowedAttributeValueRequest> allowedAttributeValuesRequest =
      allowedAttributeValues.stream()
        .map(allowedAttributeValueResponse -> copyProperties(allowedAttributeValueResponse,
          new AllowedAttributeValueRequest()))
        .map(AllowedAttributeValueRequest.class::cast)
        .collect(Collectors.toList());
    attributeRequest.setAllowedAttributeValues(allowedAttributeValuesRequest);
  }

  private static Object copyProperties(Object source, Object destination, String... ignoreProperties) {
    BeanUtils.copyProperties(source, destination, ignoreProperties);
    return destination;
  }

  private static void generateAttributeRequest(AttributeResponse attributeResponse, AttributeRequest attributeRequest) {
    BeanUtils.copyProperties(attributeResponse, attributeRequest);
    Optional.ofNullable(attributeResponse.getAttributeType())
      .ifPresent(attributeType -> attributeRequest.setAttributeType(AttributeType.valueOf(attributeType)));
    Optional.ofNullable(attributeResponse.getAllowedAttributeValues())
      .ifPresent(
        allowedAttributeValues -> generateAllowedAttributeValuesRequest(allowedAttributeValues, attributeRequest));
    Optional.ofNullable(attributeResponse.getPredefinedAllowedAttributeValues())
      .ifPresent(predefinedAllowedAttributeValues -> generatePredefinedAllowedAttributeValuesRequest(
        predefinedAllowedAttributeValues, attributeRequest));
  }

  private static void processProductItemAttributeValueRequests(AttributeResponse itemAttributeResponse,
    AttributeRequest attributeRequest) {
    BeanUtils.copyProperties(itemAttributeResponse, attributeRequest);
    attributeRequest.setAttributeType(AttributeType.valueOf(itemAttributeResponse.getAttributeType()));
    if (!CollectionUtils.isEmpty(itemAttributeResponse.getAllowedAttributeValues())) {
      List<AllowedAttributeValueRequest> allowedAttributeValues = itemAttributeResponse
        .getAllowedAttributeValues()
        .stream()
        .map(allowedAttributeValueResponse -> copyProperties(allowedAttributeValueResponse, new AllowedAttributeValueRequest()))
        .map(AllowedAttributeValueRequest.class::cast)
        .collect(Collectors.toList());
      attributeRequest.setAllowedAttributeValues(allowedAttributeValues);
    }
    if (!CollectionUtils.isEmpty(itemAttributeResponse.getPredefinedAllowedAttributeValues())) {
      List<PredefinedAllowedAttributeValueRequest> predefinedAllowedAttributeValues = itemAttributeResponse
        .getPredefinedAllowedAttributeValues()
        .stream()
        .map(predefinedAllowedAttributeValueResponse -> copyProperties(predefinedAllowedAttributeValueResponse,
          new PredefinedAllowedAttributeValueRequest()))
        .map(PredefinedAllowedAttributeValueRequest.class::cast)
        .collect(Collectors.toList());
      attributeRequest.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValues);
    }
  }

  public static ProductAndItemActivationRequest generateProductLevel3Request(ProfileResponse businessPartner,
      ProductDetailResponse productData, ProductBusinessPartner productBusinessPartner,
      Map<String, AttributeResponse> attributes, Map<String, ProductItemWholesalePrice> itemSkuToWholesalePriceMap,
      boolean forceReview) {
    ProductAndItemActivationRequest productLevel3Request = new ProductAndItemActivationRequest();
    Map<String, ProductItemResponse> productItems = productData.getProductItemResponses().stream()
        .collect(Collectors.toMap(ProductItemResponse::getId, Function.identity(), (v1, v2) -> v2));
    productLevel3Request.setProduct(
        getProductDto(businessPartner, productBusinessPartner, attributes, productData, forceReview));
    productLevel3Request.setItems(
        getListOfItemActivationRequest(productBusinessPartner, productItems, itemSkuToWholesalePriceMap, forceReview
        ));
    return productLevel3Request;
  }

  private static ProductDTO getProductDto(ProfileResponse businessPartner,
      ProductBusinessPartner productBusinessPartner, Map<String, AttributeResponse> attributes, ProductDetailResponse productDetailResponse,
      boolean forceReview) {
    ProductDTO productDTO = new ProductDTO();
    Integer productType = productBusinessPartner.getProductItemBusinessPartners().get(0).getProductType();
    Boolean installationRequired = productBusinessPartner.getProductItemBusinessPartners().get(0).isInstallation();
    PreOrderDTO preOrderDTO = PreOrderDTO.builder().isPreOrder(productBusinessPartner.isPreOrder())
        .preOrderType(productBusinessPartner.getPreOrderType()).preOrderValue(productBusinessPartner.getPreOrderValue())
        .preOrderDate(productBusinessPartner.getPreOrderDate()).build();

    productDTO.setForceReview(forceReview);
    productDTO.setMarkForDelete(forceReview);
    productDTO.setProductCode(productDetailResponse.getProductCode());
    productDTO.setProductType(GdnBaseLookup.PRODUCT_TYPE_REGULAR == productType ?
        ProductType.REGULAR :
        GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT == productType ? ProductType.BIG_PRODUCT : ProductType.BOPIS);
    productDTO.setInstallationRequired(installationRequired);
    productDTO.setMerchantCode(businessPartner.getBusinessPartnerCode());
    productDTO.setFreeSample(productBusinessPartner.isFreeSample());
    productDTO.setPreOrder(preOrderDTO);
    productDTO.setProductSku(productBusinessPartner.getGdnProductSku());
    productDTO.setTradingProduct(isTradingProduct(businessPartner));
    productDTO.setProductSpecialAttributes(getProductSpecialAttribute(productBusinessPartner, attributes));
    productDTO.setOff2OnChannelActive(productBusinessPartner.isOff2OnChannelActive());
    productDTO.setOnline(productBusinessPartner.isOnline());
    productDTO.setFbbActivated(productBusinessPartner.isFbbActivated());
    productDTO.setB2bActivated(productBusinessPartner.isB2bActivated());
    productDTO.setB2cActivated(productBusinessPartner.isB2cActivated());
    productDTO.setBundleProduct(productBusinessPartner.isBundleProduct());
    productDTO.setSizeChartCode(productBusinessPartner.getSizeChartCode());
    productDTO.setVideoUpdated(true);
    Optional.ofNullable(productDetailResponse.getVideoDTO()).ifPresent(videoDTO -> {
      com.gdn.x.product.rest.web.model.request.VideoAddEditRequest videoAddEditRequestForXprod =
        new VideoAddEditRequest();
      BeanUtils.copyProperties(videoDTO, videoAddEditRequestForXprod);
      videoAddEditRequestForXprod.setVideoUrl(videoDTO.getSourceUrl());
      productDTO.setVideoAddEditRequest(videoAddEditRequestForXprod);
    });
    return productDTO;
  }

  public static List<ItemActivationRequest> getListOfItemActivationRequest(
      ProductBusinessPartner productBusinessPartner, Map<String, ProductItemResponse> productItems,
      Map<String, ProductItemWholesalePrice> itemSkuToWholesalePriceMap, boolean forceReview) {
    List<ItemActivationRequest> itemActivationRequestList = new ArrayList<>();
    Map<String, List<ProductItemBusinessPartner>> productItemBusinessPartnerGroup =
        productBusinessPartner.getProductItemBusinessPartners().stream()
            .collect(Collectors.groupingBy(ProductItemBusinessPartner::getGdnProductItemSku));
    for (List<ProductItemBusinessPartner> productItemBusinessPartners : productItemBusinessPartnerGroup.values()) {
      ProductItemBusinessPartner firstProductItemBusinessPartner = productItemBusinessPartners.get(0);
      ProductItemResponse productItem = productItems.get(firstProductItemBusinessPartner.getProductItemId());
      ItemActivationRequest itemActivationRequest =
          getItemActivationRequest(productBusinessPartner, firstProductItemBusinessPartner, productItem, forceReview);
      itemActivationRequest.setItemPickupPoints(Optional.of(getItemPickupPointActivationRequest(
              productItemBusinessPartners.stream()
                  .filter(productItemBusinessPartner -> !productItemBusinessPartner.isMarkForDelete())
                  .collect(Collectors.toList()), itemSkuToWholesalePriceMap, forceReview))
          .orElse(new ArrayList<>()));
      itemActivationRequestList.add(itemActivationRequest);
    }
    return itemActivationRequestList;
  }

  private static ItemActivationRequest getItemActivationRequest(ProductBusinessPartner productBusinessPartner,
      ProductItemBusinessPartner productItemBusinessPartner, ProductItemResponse productItem, boolean forceReview) {
    ItemActivationRequest itemRequest = new ItemActivationRequest();
    itemRequest.setForceReview(forceReview);
    itemRequest.setMerchantSku(productItemBusinessPartner.getMerchantSku());
    itemRequest.setItemSku(productItemBusinessPartner.getGdnProductItemSku());
    itemRequest.setFreeSample(productBusinessPartner.isFreeSample());
    itemRequest.setIsLateFulfillment(
        !GdnBaseLookup.PRODUCT_TYPE_REGULAR.equals(productItemBusinessPartner.getProductType()));
    if (Objects.nonNull(productItem)) {
      itemRequest.setItemCode(productItem.getSkuCode());
    }
    if (StringUtils.isNotBlank(productItemBusinessPartner.getBundleRecipe())) {
      try {
        Set<BundleRecipeVo> bundleRecipe = objectMapper.readValue(productItemBusinessPartner.getBundleRecipe(),
            new TypeReference<Set<BundleRecipeVo>>() {
            });
        itemRequest.setBundleRecipe(bundleRecipe);
      } catch (JsonProcessingException e) {
        log.error("Error while deserializing bundle recipe : {} ", productItemBusinessPartner.getBundleRecipe());
      }
    }
    return itemRequest;
  }

  private static List<ItemPickupPointActivationRequest> getItemPickupPointActivationRequest(
      List<ProductItemBusinessPartner> productItemBusinessPartners,
      Map<String, ProductItemWholesalePrice> itemSkuToWholesalePriceMap, boolean forceReview) {
    List<ItemPickupPointActivationRequest> itemPickupPointActivationRequestList = new ArrayList<>();
    for (ProductItemBusinessPartner productItemBusinessPartner : productItemBusinessPartners) {
      ItemPickupPointActivationRequest itemPickupPointActivationRequest = new ItemPickupPointActivationRequest();
      itemPickupPointActivationRequest.setCncActive(productItemBusinessPartner.isCncActivated());
      itemPickupPointActivationRequest.setFbbActivated(productItemBusinessPartner.isFbbActive());
      itemPickupPointActivationRequest.setPickupPointCode(productItemBusinessPartner.getPickupPointId());
      itemPickupPointActivationRequest.setPrice(new HashSet<>());
      itemPickupPointActivationRequest.setItemViewConfigs(new HashSet<>());
      itemPickupPointActivationRequest.setDistribution(productItemBusinessPartner.isDistribution());
      itemPickupPointActivationRequest.getPrice().add(getPriceRequest(productItemBusinessPartner));
      itemPickupPointActivationRequest.setB2bFields(new B2bFieldsRequest(productItemBusinessPartner.getB2bPrice(),
          productItemBusinessPartner.isB2bManaged()));
      itemPickupPointActivationRequest.getItemViewConfigs()
          .addAll(new HashSet<>(getItemViewConfigRequest(productItemBusinessPartner, forceReview)));
      boolean wholePricePriceExists =
          isWholePricePriceExists(itemSkuToWholesalePriceMap, productItemBusinessPartner);
      if (wholePricePriceExists) {
        itemPickupPointActivationRequest.setWholesalePriceExists(true);
        ProductItemWholesalePrice productItemWholesalePrice =
            itemSkuToWholesalePriceMap.get(
                productItemBusinessPartner.getGdnProductItemSku() + Constants.HYPHEN + productItemBusinessPartner
                    .getPickupPointId());
        itemPickupPointActivationRequest
            .setWholesalePriceActivated(productItemWholesalePrice.isWholesalePriceActivated());
      }
      itemPickupPointActivationRequestList.add(itemPickupPointActivationRequest);
    }
    return itemPickupPointActivationRequestList;
  }

  private static boolean isWholePricePriceExists(Map<String, ProductItemWholesalePrice> itemSkuToWholesalePriceMap,
    ProductItemBusinessPartner productItemBusinessPartner) {
    // If MPP is on key for map will be itemSku + pick up point code else it will be just itemSku
    return itemSkuToWholesalePriceMap.containsKey(productItemBusinessPartner.getGdnProductItemSku() + Constants.HYPHEN + productItemBusinessPartner
    .getPickupPointId());
  }

  private static PriceRequest getPriceRequest(ProductItemBusinessPartner productItemBusinessPartner) {
    PriceRequest priceRequest = new PriceRequest();
    priceRequest.setOfferPrice(productItemBusinessPartner.getSalePrice());
    priceRequest.setListPrice(productItemBusinessPartner.getPrice());
    priceRequest.setChannel(ChannelName.DEFAULT.name());
    return priceRequest;
  }

  private static List<ItemViewConfigRequest> getItemViewConfigRequest(ProductItemBusinessPartner productItemBusinessPartner,
      boolean forceReview) {
    ItemViewConfigRequest viewConfigRequest = new ItemViewConfigRequest();
    ItemViewConfigRequest b2bViewConfigRequest = new ItemViewConfigRequest();
    ItemViewConfigRequest cncViewConfigRequest = new ItemViewConfigRequest();
    viewConfigRequest.setChannel(ChannelName.DEFAULT.name());
    b2bViewConfigRequest.setChannel(Constants.B2B_CHANNEL);
    cncViewConfigRequest.setChannel(Constants.CNC_CHANNEL);
    if (forceReview) {
      viewConfigRequest.setBuyable(false);
      viewConfigRequest.setDiscoverable(false);
      b2bViewConfigRequest.setBuyable(false);
      b2bViewConfigRequest.setDiscoverable(false);
      cncViewConfigRequest.setBuyable(false);
      cncViewConfigRequest.setDiscoverable(false);
    } else {
      viewConfigRequest.setBuyable(productItemBusinessPartner.isBuyable());
      viewConfigRequest.setDiscoverable(productItemBusinessPartner.isDisplay());
      b2bViewConfigRequest.setBuyable(productItemBusinessPartner.isB2bBuyable());
      b2bViewConfigRequest.setDiscoverable(productItemBusinessPartner.isB2bDiscoverable());
      cncViewConfigRequest.setBuyable(productItemBusinessPartner.isCncBuyable());
      cncViewConfigRequest.setDiscoverable(productItemBusinessPartner.isCncDiscoverable());
    }
    return Arrays.asList(viewConfigRequest, b2bViewConfigRequest, cncViewConfigRequest);
  }

  private static List<ProductSpecialAttributeDTO> getProductSpecialAttribute(
      ProductBusinessPartner productBusinessPartner, Map<String, AttributeResponse> attributes) {
    List<ProductSpecialAttributeDTO> productSpecialAttributeDTOList = new ArrayList<>();
    for (ProductBusinessPartnerAttribute productBusinessPartnerAttribute : productBusinessPartner.getProductBusinessPartnerAttributes()) {
      AttributeResponse attribute = attributes.get(productBusinessPartnerAttribute.getAttributeId());
      if (Objects.nonNull(attribute)) {
        ProductSpecialAttributeDTO attributeRequest = new ProductSpecialAttributeDTO();
        attributeRequest.setAttributeCode(attribute.getAttributeCode());
        attributeRequest.setAttributeValue(productBusinessPartnerAttribute.getValue());
        productSpecialAttributeDTOList.add(attributeRequest);
      }
    }
    return productSpecialAttributeDTOList;
  }

  private static boolean isTradingProduct(ProfileResponse businessPartner) {
    if (GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI.equalsIgnoreCase(
        businessPartner.getCompany().getInventoryFulfillment())
        && GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER.equalsIgnoreCase(
        businessPartner.getCompany().getPurchaseTerm())) {
      return true;
    }
    return false;
  }

  public static ProductItemsImageUpdateRequest createImageUpdateRequest(
    ProductL3UpdateRequest productL3UpdateRequest, ProductVariantUpdateRequest productVariantUpdateRequest) {

    List<ProductPriceStockAndImagesRequest> productPriceStockAndImagesRequestList =
      new ArrayList<>();
    List<ProductVariantPriceStockAndImagesRequest> priceStockAndImagesRequests =
      productVariantUpdateRequest.getProductItems();
    if (org.apache.commons.collections.CollectionUtils.isNotEmpty(priceStockAndImagesRequests)) {
      for (ProductVariantPriceStockAndImagesRequest variantsStockImageRequest : priceStockAndImagesRequests) {
        ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
        productPriceStockAndImagesRequest.setItemSku(variantsStockImageRequest.getItemSku());
        productPriceStockAndImagesRequest.setItemName(variantsStockImageRequest.getItemName());
        productPriceStockAndImagesRequest.setSkuCode(variantsStockImageRequest.getSkuCode());
        productPriceStockAndImagesRequest.setProductSku(variantsStockImageRequest.getProductSku());
        productPriceStockAndImagesRequest.setImages(variantsStockImageRequest.getImages());
        productPriceStockAndImagesRequestList.add(productPriceStockAndImagesRequest);
      }
    }
    List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImagesList = new ArrayList<>();
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest =
      new ProductLevel3SummaryDetailsImageRequest();

    if (org.apache.commons.collections.CollectionUtils.isNotEmpty(productL3UpdateRequest.getCommonImages())) {
      for (ProductLevel3SummaryDetailsImageRequest commonImageRequest : productL3UpdateRequest.getCommonImages()) {
        productLevel3SummaryDetailsImageRequest
          .setLocationPath(commonImageRequest.getLocationPath());
        productLevel3SummaryDetailsImageRequest.setMainImage(commonImageRequest.getMainImage());
        productLevel3SummaryDetailsImageRequest
          .setMarkForDelete(commonImageRequest.getMarkForDelete());
        productLevel3SummaryDetailsImageRequest.setReviewType(commonImageRequest.getReviewType());
        productLevel3SummaryDetailsImageRequest.setSequence(commonImageRequest.getSequence());
        copyToAllVariantImagesList.add(productLevel3SummaryDetailsImageRequest);
      }
    }
    copyToAllVariantImagesList
      .addAll(productVariantUpdateRequest.getCopyToAllVariantImages());
    return ProductItemsImageUpdateRequest.builder()
      .businessPartnerCode(productL3UpdateRequest.getBusinessPartnerCode())
      .productCode(productL3UpdateRequest.getProductCode())
      .needCorrection(productL3UpdateRequest.isNeedCorrection())
      .variantList(productPriceStockAndImagesRequestList)
      .copyToAllVariantImages(copyToAllVariantImagesList).build();
  }

  public static ProductVariantUpdateRequest validateProductItems(
    ProductVariantUpdateRequest productVariantUpdateRequest) {
    String productSku = productVariantUpdateRequest.getProductSku();
    String productCode = productVariantUpdateRequest.getProductCode();
    if (CollectionUtils.isEmpty(productVariantUpdateRequest.getProductItems())) {
      List<ProductVariantPriceStockAndImagesRequest> productItemsList = new ArrayList<>();
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
      productVariantPriceStockAndImagesRequest.setProductSku(productSku);
      productVariantPriceStockAndImagesRequest.setProductCode(productCode);
      productItemsList.add(productVariantPriceStockAndImagesRequest);
      productVariantUpdateRequest.setProductItems(productItemsList);
    } else {
      for (ProductVariantPriceStockAndImagesRequest stockAndImagesRequest : productVariantUpdateRequest
        .getProductItems()) {
        if (StringUtils.isEmpty(stockAndImagesRequest.getProductSku())) {
          stockAndImagesRequest.setProductSku(productSku);
        }
        if (StringUtils.isEmpty(stockAndImagesRequest.getProductCode())) {
          stockAndImagesRequest.setProductCode(productCode);
        }
      }
    }
    return productVariantUpdateRequest;
  }
}
